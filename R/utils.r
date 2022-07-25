################################ FUNCOES AUXILIARES PARA OS SCRIPTS ################################

#' Identifica Quadrante Da Usina
#' 
#' Identifica entre quais vertices de \code{coords} esta \code{usina}
#' 
#' \code{usinas} deve corresponder a uma ou mais linhas do dado na tabela \code{usinas} do banco ou 
#' arquivo local de mesma estrutura. No minimo, code{usinas} deve ter duas colunas indicando 
#' longitude e latitude, com estes nomes. O restante das colunas nao tem uso nessa funcao.
#' 
#' \code{coords} deve ter tres colunas nomeadas: \code{longitude} e \code{latitude} contendo as 
#' coordenadas do vertice e \code{id} contendo o indice do vertice, um valor inteiro nao repetido.
#' 
#' @param usinas \code{data.table} contendo informacoes das usinas. Ver Detalhes
#' @param coords \code{data.table} contendo informacoes dos vertices da grade. Ver Detalhes
#' 
#' @return Lista contendo, para cada linha de \code{usinas}, o vetor de indices dos vertices do 
#'     quadrilatero circulante, em sentido anti-horario a partir do vertice superior direito.
#'     Se alguma usina nao estiver dentro da grade ou entre trechos disjuntos, retorna vetor de NAs

quadrante_usina <- function(usinas, coords) {

    usinas <- as.data.table(usinas)

    lons <- sort(unique(coords$longitude))
    lats <- sort(unique(coords$latitude))

    resol_lon <- diff(lons[1:2])
    resol_lat <- diff(lats[1:2])

    out <- lapply(seq(nrow(usinas)), function(i) {
        tryCatch({
        lonusi <- usinas$longitude[i]
        latusi <- usinas$latitude[i]

        lon_p <- head(lons[lons >= lonusi], 1)
        lon_m <- tail(lons[lons < lonusi], 1)
        lat_p <- head(lats[lats >= latusi], 1)
        lat_m <- tail(lats[lats < latusi], 1)

        if(((lon_p - lon_m) != resol_lon) || ((lat_p - lat_m) != resol_lat)) stop("fora da grade")

        verts <- vector("double", 4L)
        verts[1] <- coords[(longitude == lon_p) & (latitude == lat_p), id]
        verts[2] <- coords[(longitude == lon_m) & (latitude == lat_p), id]
        verts[3] <- coords[(longitude == lon_m) & (latitude == lat_m), id]
        verts[4] <- coords[(longitude == lon_p) & (latitude == lat_m), id]

        return(verts)
        }, error = function(e) rep(NA_real_, 4))
    })

    return(out)
}

#' Interpolacao Bilinear Na Grade
#' 
#' Interpola a serie de vento de reanalise numa coordenada qualquer dentro da grade
#' 
#' Considerando o tamanho do dado de reanalise, e mais eficiente que \code{interp_usina} receba a 
#' conexao ao banco do que exigir que o usuario mantenha todo o \code{data.table} em memoria e o 
#' passe para a funcao (correndo ainda o risco de realizar copias disso). As usinas a serem 
#' interpoladas sao reordenadas de modo a minimizar o numero de queries.
#' 
#' \code{usinas} deve corresponder a uma ou mais linhas do dado na tabela \code{usinas} do banco ou 
#' arquivo "Dados das Usinas.txt", com colunas renomeadas para o mesmo padrao do dado no banco. No
#' minimo, code{usinas} deve ter duas colinas indicando longitude e latitude, com estes nomes, e uma
#' coluna chamada codigo contendo o codigo de seis caracters da usina. O restante das colunas nao
#' tem uso nessa funcao.
#' 
#' \code{coords} deve ter tres colunas nomeadas: \code{longitude} e \code{latitude} contendo as 
#' coordenadas do vertice e \code{ind} contendo o indice do vertice, um valor inteiro nao repetido.
#' 
#' @param usinas \code{data.table} contendo informacoes das usinas. Ver Detalhes
#' @param coords \code{data.table} contendo informacoes dos vertices da grade. Ver Detalhes
#' @param conn conexao ao banco contendo o dado de reanalise
#' @param anos string no padrao do \code{xts} indicando quais datas deve ser extraidos 
#' @param agr string indicando algum tipo de agregacao a ser feito no dado, um entre "dia", "mes",
#'     "ano"; "none" nao faz nenhuma agregacao
#' 
#' @return \code{data.table} de tres colunas: \code{data_hora}, \code{vento_reanalise} e 
#'     \code{codigo}. O dado nao estara ordenado conforme as ordem em \code{usinas} e possivelmente
#'     nao contera todas as usinas, caso alguma esteja fora da grade de reanalise

interp_usina <- function(usinas, coords, conn, anos = "2017/", agr = "none") {

    usinas <- as.data.table(usinas)

    anos <- dbrenovaveis:::parsedatas(anos, "", FALSE)
    anos <- c(year(anos[[1]][1]), year(anos[[2]][2]))

    agr_fun <- switch(agr,
        "none" = function(d) d,
        "dia" = function(d) d[, mean(V2), by = .(V1 = as.Date(V1))],
        "mes" = function(d) d[, mean(V2), by = .(V1 = format(V1, "%Y-%m"))],
        "ano" = function(d) d[, mean(V2), by = .(V1 = format(V1, "%Y"))]
    )
    date_fun <- switch(agr,
        "none" = function(v) v,
        "dia" = function(v) v,
        "mes" = function(v) as.Date(paste0(v, "-01")),
        "ano" = function(v) as.Date(paste0(v, "-01-01"))
    )

    datas <- dbGetQuery(conn, paste0("SELECT id_ano,id_mes,id_dia,id_hora FROM FT_MERRA2",
        " WHERE id_lon = ", coords$longitude[1], " AND id_lat = ", coords$latitude[1],
        " AND id_ano >= ", anos[1], " AND id_ano <= ", anos[2]))
    datas <- as.data.table(datas)
    datas[, c("id_mes", "id_dia", "id_hora") := lapply(.SD, formatC, width = 2, flag = "0"),
        .SDcols = c("id_mes", "id_dia", "id_hora")]
    datas <- datas[, paste0(paste(id_ano, id_mes, id_dia, sep = "-"), " ", id_hora, ":00:00")]
    datas <- as.POSIXct(datas, "GMT")

    quads <- quadrante_usina(usinas, coords)
    quads <- as.data.table(do.call(rbind, quads))
    quads <- quads[, c(3, 2, 4, 1)] # reorndena para uma estrutura adaptada pra interp bilin
    colnames(quads) <- paste0("vert", seq_len(4))

    usinas_vert <- cbind(usinas, quads)
    setorder(usinas_vert, vert1, na.last = TRUE)

    quad11 <- 0

    interps <- lapply(seq(nrow(usinas_vert)), function(i) {

        verts <- usinas_vert[i, c(vert1, vert2, vert3, vert4)]
        if(is.na(verts[1])) return(NULL)

        coords_quad <- coords[verts, ]

        if(verts[1] != quad11) {
            quad11 <- verts[1]

            deltax <- diff(sort(unique(coords_quad$longitude)))
            deltay <- diff(sort(unique(coords_quad$latitude)))

            queries <- paste0("SELECT vr_velocidade FROM FT_MERRA2",
                " WHERE id_lon = ", coords_quad$longitude, " AND id_lat = ", coords_quad$latitude,
                " AND id_ano >= ", anos[1], " AND id_ano <= ", anos[2]
                )
            series <- lapply(seq(queries), function(i) unlist(dbGetQuery(conn, queries[i])))
        }

        vec <- interp_bilin(usinas_vert[i], series, coords_quad)
        out <- data.table(V1 = datas, V2 = vec)
        out <- agr_fun(out)
        out[, V1 := date_fun(V1)]
        colnames(out) <- c("data_hora", "vento_reanalise")

        out[, codigo := rep(usinas_vert[i, codigo], .N)]

        out
    })
    interps <- interps[!sapply(interps, is.null)]

    interps <- rbindlist(interps)

    return(interps)
}

interp_bilin <- function(x, vals, verts) {

    deltax <- diff(sort(unique(verts$longitude)))
    deltay <- diff(sort(unique(verts$latitude)))

    deltax1 <- x$longitude - min(verts$longitude)
    deltax2 <- max(verts$longitude) - x$longitude
    deltay1 <- x$latitude - min(verts$latitude)
    deltay2 <- max(verts$latitude) - x$latitude

    pesos <- 1 / (deltax * deltay) * c(deltax2 * deltay2, deltax1 * deltay2,
        deltax2 * deltay1, deltax1 * deltay1)

    vec <- rowSums(mapply("*", vals, pesos))

    return(vec)
}
