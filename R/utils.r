################################ FUNCOES AUXILIARES PARA OS SCRIPTS ################################

# INTERPOLACAO DE REANALISE ------------------------------------------------------------------------

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
#' @param datahoras string no padrao do \code{xts} indicando quais datas deve ser extraidos 
#' @param agr string indicando algum tipo de agregacao a ser feito no dado, um entre "dia", "mes",
#'     "ano"; "none" nao faz nenhuma agregacao
#' 
#' @return \code{data.table} de tres colunas: \code{data_hora}, \code{vento_reanalise} e 
#'     \code{codigo}. O dado nao estara ordenado conforme as ordem em \code{usinas} e possivelmente
#'     nao contera todas as usinas, caso alguma esteja fora da grade de reanalise

interp_usina <- function(usinas, coords, conn, datahoras = "2017/", agr = "none") {

    usinas <- as.data.table(usinas)

    datahoras <- dbrenovaveis:::parsedatas(datahoras, "", FALSE)
    datahoras <- c(year(datahoras[[1]][1]), year(datahoras[[2]][2]))

    agr_fun <- switch(agr,
        "none" = function(d) d,
        "dia" = function(d) d[, mean(vento), by = .(data_hora = as.Date(data_hora))],
        "mes" = function(d) d[, mean(vento), by = .(data_hora = format(data_hora, "%Y-%m"))],
        "ano" = function(d) d[, mean(vento), by = .(data_hora = format(data_hora, "%Y"))]
    )
    date_fun <- switch(agr,
        "none" = function(v) v,
        "dia" = function(v) v,
        "mes" = function(v) as.Date(paste0(v, "-01")),
        "ano" = function(v) as.Date(paste0(v, "-01-01"))
    )

    datas <- getreanalise(conn, longitudes = coords$lon[1], latitudes = coords$lat[1])$data_hora

    quads <- quadrante_usina(usinas, coords)
    quads <- as.data.table(do.call(rbind, quads))
    colnames(quads) <- paste0("vert", seq_len(4))

    usinas_vert <- cbind(usinas, quads)
    setorder(usinas_vert, vert1, na.last = TRUE)

    quad11 <- 0
    env_base <- environment()

    interps <- vector("list", nrow(usinas_vert))
    for(i in seq(nrow(usinas_vert))) {

        verts <- usinas_vert[i, c(vert1, vert2, vert3, vert4)]
        if(is.na(verts[1])) next

        coords_quad <- coords[verts, ]

        if(verts[1] != quad11) {
            cat("indice ", i, " -- lendo dado\n")
            assign("quad11", verts[1], envir = env_base)

            series <- getreanalise(conn, longitudes = coords_quad$lon, latitudes = coords_quad$lat)
            series[, id := factor(id_vertice, levels = verts, ordered = TRUE)]
            series <- lapply(split(series, series$id), "[[", "vento")
        }

        vec <- interp_bilin(usinas_vert[i], series, coords_quad)
        out <- data.table(data_hora = datas, vento = vec)
        out <- agr_fun(out)
        out[, data_hora := date_fun(data_hora)]

        out[, id_usina := rep(usinas_vert[i, id], .N)]

        interps[[i]] <- out
    }
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

    pesos <- 1 / (deltax * deltay) * c(deltax1 * deltay1, deltax2 * deltay1,
        deltax2 * deltay2, deltax1 * deltay2)

    vec <- rowSums(mapply("*", vals, pesos))

    return(vec)
}

# AUXILIARES DA ESTIMACAO DE FTMS ------------------------------------------------------------------

#' Determina Capacidade Instalada Evolutiva
#' 
#' Calcula a serie de capacidades instaladas evolutivas mes a mes por cluster dentro de uma janela
#' 
#' @param usinas um dado de usinas contendo adiconalmente a coluna \code{Cluster}
#' @param janela uma lista de tamanho dois indicando data incial e final da serie de potencia 
#'     evolutiva a ser retornada
#' 
#' @return data.table de tres colunas: data_hora, capacidade_instalada, Cluster

determina_pot_evol <- function(usinas, janela) {

    out <- lapply(split(usinas, usinas$cluster), function(dat) {
        setorder(dat, data_inicio_operacao)
        datas_ini <- dat$data_inicio_operacao
        pot_evol  <- cumsum(dat$capacidade_instalada)

        datas <- seq(janela[[1]], janela[[2]], by = "month")
        datas <- datas[datas >= min(datas_ini)]
        pot_evol_meses <- sapply(datas, function(dt) max(pot_evol[datas_ini <= dt]))

        data.table(data_hora = datas, capacidade_instalada = pot_evol_meses, cluster = dat$cluster[1])
    })
    out <- rbindlist(out)

    return(out)
}

#' Combina Dados Para Estimacao
#' 
#' Merges e preparacao de dados para estimacao das FTMs
#' 
#' @param usinas um dado de usinas contendo adiconalmente a coluna \code{Cluster}
#' @param geracao dado de geracao media mensal por usina, no formato como retornado por 
#' @param reanalise dado de vento medio mensal por usina
#' @param pot_evol dado de potencia instalada evolutiva no tempo por cluster
#' 
#' @return data.table com informacoes necessarias para estimacao das FTMs

monta_regdata <- function(usinas, geracao, reanalise, pot_evol) {
    geracao[, data_hora := as.Date(data_hora)]
    reanalise[, data_hora := as.Date(data_hora)]

    regdata <- merge(geracao, reanalise, by = c("id_usina", "data_hora"))
    regdata <- merge(regdata, usinas[, .(id, cluster)], by.x = "id_usina", by.y = "id")
    regdata[is.na(geracao), vento := NA]
    regdata[, id_usina := NULL]
    regdata <- regdata[, .(geracao = sum(geracao, na.rm = TRUE), vento = mean(vento, na.rm = TRUE),
        count = mean(count, na.rm = TRUE)), by = .(cluster, data_hora)]
    regdata <- merge(regdata, pot_evol, by = c("cluster", "data_hora"))
    regdata[, fator_capacidade := geracao / capacidade_instalada]

    return(regdata)
}