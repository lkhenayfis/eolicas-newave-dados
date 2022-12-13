library(data.table)

# --------------------------------------------------------------------------------------------------

valida_dup_ceg <- function(dat) {
    dat <- dat[!duplicated(dat)]

    if(nrow(dat) == 1) return(dat)

    vv <- complete.cases(dat)
    if(sum(vv) <= 1) return(dat[vv])

    antigo <- grepl("^D[[:digit:]]+", dat$codigo)
    if(sum(!antigo) == 1) return(dat[!antigo]) else stop("Impossivel escolher")
}

# --------------------------------------------------------------------------------------------------

shape <- readRDS("data/shape.rds")
setDT(shape)
shape[, group := as.numeric(as.character(group))]

estados <- c(
    MA = 7.1, PI = 8.1, CE = 9.1, RN = 10.1, PB = 11.1, PE = 12.1,
    AL = 13.1, SE = 14.1, BA = 15.1, RJ = 18.1, PR = 20.1, SC = 21.1, RS = 22.1
)

pois <- lapply(estados, function(num) {
    dd <- shape[group == num, .SD, .SDcols = c("long", "lat")]
    do.call(cbind, polylabelr::poi(dd$long, dd$lat, precision = .01))
})
pois <- as.data.table(do.call(rbind, pois))
names(pois) <- c("longitude", "latitude", "prec")
pois[, estado := names(estados)]

# leitura das colunas relevantes
confeol <- fread("data-raw/CONF_EOL_FULL.csv", fill = TRUE, na.strings = "NULL")
confeol <- confeol[, .(USI_ID, ID_SUBSISTEMA, NOME, CEG_ANEEL, LATITUDE, LONGITUDE, UF, DT_ENTRADA_OPER)]
colnames(confeol) <- c("codigo", "subsistema", "nome", "ceg", "latitude", "longitude", "estado", "data_inicio_operacao")

# filtra CEG vazio
confeol <- confeol[ceg != ""]

# seleciona dos CEGs duplicados apenas os registros com todas as informacoes necessarias
# isso assume que a duplicacao ocorre de registros antigos (preliminares) que continuam no banco
# erroneamente, mas existe um mais novo sem buracos nas informacoes
confeol <- split(confeol, confeol$ceg)
confeol <- lapply(confeol, valida_dup_ceg)
confeol <- rbindlist(confeol)
confeol[, data_inicio_operacao := NULL] # isso vem da planilha da CCEE depois com os modifs de cap_inst

confeol[latitude == 0, latitude := NA_real_]
confeol[longitude == 0, longitude := NA_real_]
confeol[, coordenadas_aproximadas := ifelse(is.na(latitude) | is.na(longitude), TRUE, FALSE)]

# junta com os centroides
confeol <- merge(confeol, pois, by = "estado", suffixes = c("", ".y"))
confeol[is.na(latitude), latitude := latitude.y]
confeol[is.na(longitude), longitude := longitude.y]

confeol[, longitude.y := NULL]
confeol[, latitude.y := NULL]
confeol[, prec := NULL]
confeol[, estado := NULL]

confeol[, id := seq(.N)]

#dados_data <- fread("data-raw/Dados_Usinas_EOL.csv", na.strings = "NULL")
#dados_data <- dados_data[, .(CEG_ANEEL, DT_ENTRADA_OPER)]
#dados_data <- dados_data[complete.cases(dados_data)]
#colnames(dados_data) <- c("ceg", "data_inicio_operacao")
#
#confeol <- merge(confeol, dados_data)
#confeol[, data_inicio_operacao := as.Date(as.character(data_inicio_operacao), format = "%d/%m/%Y")]
#
#setcolorder(confeol, c("id", "codigo", "nome", "ceg", "subsistema", "latitude", "longitude",
#    "data_inicio_operacao", "coordenadas_aproximadas"))
#setorder(confeol, id)

setcolorder(confeol, c("id", "codigo", "nome", "ceg", "subsistema", "latitude", "longitude",
    "coordenadas_aproximadas"))
setorder(confeol, id)

fwrite(confeol, "data/usinas.csv")
