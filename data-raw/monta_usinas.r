library(data.table)

#usinas <- fread("data-raw/Dados das Usinas.txt")
#usinas[] <- lapply(usinas, trimws)
#usinas[, codigo := sub("_.*", "", V1)]
#usinas[, nome := sub(".{6}_", "", V1)]
#usinas[, c(2:6, 8, 10)] <- lapply(usinas[, c(2:6, 8, 10)], as.numeric)
#usinas[, V1 := NULL]
#names(usinas) <- c("capacidade_instalada", "latitude", "longitude", "data_inicio_operacao",
#    "simulada", "subsistema_eletrico", "data_inicio_simulacao", "subsistema_geografico", "B",
#    "codigo", "nome")
#usinas[, id := seq(.N)]
#setcolorder(usinas,
#    c("id", "codigo", "nome", "capacidade_instalada", "latitude", "longitude", "data_inicio_operacao",
#    "data_inicio_simulacao", "simulada", "subsistema_geografico", "subsistema_eletrico", "B")
#)
#
#usinas[, data_inicio_operacao := as.Date(as.character(data_inicio_operacao), format = "%Y%m%d")]
#usinas[, data_inicio_simulacao := as.Date(as.character(data_inicio_simulacao), format = "%Y%m%d")]
#usinas[, B := NULL]
#usinas[, nome := gsub("_", " ", nome)]

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

confeol <- fread("data-raw/CONF_EOL.csv", fill = TRUE)
confeol[, FONTE := NULL]
confeol[] <- lapply(confeol, trimws)
confeol[, SUBSISTEMA := factor(toupper(SUBSISTEMA),
    levels = c("NORTE", "NORDESTE", "SUL", "SUDESTE"),
    labels = c("N", "NE", "S", "SE"))]

colnames(confeol) <- c("codigo", "nome", "subsistema", "estado", "ceg", "latitude", "longitude")

confeol <- confeol[!duplicated(codigo)]
confeol <- confeol[!duplicated(ceg)]

confeol[latitude == 0, latitude := NA_real_]
confeol[longitude == 0, longitude := NA_real_]
confeol[, coordenadas_aproximadas := ifelse(is.na(latitude) | is.na(longitude), TRUE, FALSE)]

confeol <- merge(confeol, pois, by = "estado", suffixes = c("", ".y"))
confeol[is.na(latitude), latitude := latitude.y]
confeol[is.na(longitude), longitude := longitude.y]

confeol[, longitude.y := NULL]
confeol[, latitude.y := NULL]
confeol[, prec := NULL]
confeol[, estado := NULL]

confeol[, id := seq(.N)]

dados_data <- fread("data-raw/Dados_Usinas_EOL.csv", na.strings = "NULL")
dados_data <- dados_data[, .(CEG_ANEEL, DT_ENTRADA_OPER)]
dados_data <- dados_data[complete.cases(dados_data)]
colnames(dados_data) <- c("ceg", "data_inicio_operacao")

confeol <- merge(confeol, dados_data)
confeol[, data_inicio_operacao := as.Date(as.character(data_inicio_operacao), format = "%d/%m/%Y")]

setcolorder(confeol, c("id", "codigo", "nome", "ceg", "subsistema", "latitude", "longitude",
    "data_inicio_operacao", "coordenadas_aproximadas"))
setorder(confeol, id)

fwrite(confeol, "data/usinas.csv")
