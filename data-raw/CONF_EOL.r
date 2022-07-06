library(data.table)
library(ggplot2)

shape <- readRDS("data/shape.rds")
setDT(shape)
shape[, group := as.numeric(as.character(group))]

#ggplot(shape[group == 22.1], aes(long, lat)) +
#    geom_polygon(aes(group = group), fill = NA, color = 1)

estados <- c(
    MA = 7.1,
    PI = 8.1,
    CE = 9.1,
    RN = 10.1,
    PB = 11.1,
    PE = 12.1,
    AL = 13.1,
    SE = 14.1,
    BA = 15.1,
    RJ = 18.1,
    PR = 20.1,
    SC = 21.1,
    RS = 22.1
)

centroides <- lapply(estados, function(num) {
    dd <- shape[group == num, lapply(.SD, mean), .SDcols = c("long", "lat")]
    dd[, estado := names(estados[estados == num])]
    dd
})

centroides <- rbindlist(centroides)

ggplot(shape[(group > 7) & (group < 23)], aes(long, lat)) +
    geom_polygon(aes(group = group), fill = NA, color = 1) +
    geom_point(data = centroides, color = "red")

CONF_EOL <- fread("data-raw/CONF_EOL.csv", fill = TRUE)
CONF_EOL <- CONF_EOL[, lapply(.SD, trimws)]
CONF_EOL[, c("P.INST", "LATITUDE", "LONGITUDE") := lapply(.SD, as.numeric),
    .SDcols = c("P.INST", "LATITUDE", "LONGITUDE")]

CONF_EOL <- merge(CONF_EOL, centroides, by.x = "PEE", by.y = "estado")

CONF_EOL[LATITUDE == 0, LATITUDE := lat]
CONF_EOL[LONGITUDE == 0, LONGITUDE := long]

colnames(CONF_EOL) <- c("estado", "codigo", "nome", "capinst", "existente", "iniop", "latitude",
    "longitude", "modalidade", "A", "B", "C")
CONF_EOL[, c("A", "B", "C") := NULL]
CONF_EOL[, iniop := as.Date(as.character(iniop), format = "%Y%m%d")]

# tem alguns registros duplicados, pode ser expansao ou decomissionamento de usinas, deve ser
# avaliado com mais calma
CONF_EOL <- CONF_EOL[!duplicated(CONF_EOL$codigo)]

saveRDS(CONF_EOL, "data/conf_eol.rds")
