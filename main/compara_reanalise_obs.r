library(dbrenovaveis)
library(data.table)
library(odbc)
library(ggplot2)

source("R/utils.r")

if(!dir.exists("out/compara_reanalise_obs")) dir.create("out/compara_reanalise_obs")

conn_banco1 <- conectabanco("lucas", "banco_meta")
usinas      <- as.data.table(getusinas(conn_banco1))

conn_bancoR <- dbConnect(odbc(),
    driver = "SQL Server",
    server = "prd-sql-09\\eolica",
    database = "MERRA2",
    uid = "LKSouza", pwd = "LKSouza"
)

# INTERPOLACAO DOS DADOS DE REANALISE --------------------------------------------------------------

coords <- dbGetQuery(conn_bancoR, "SELECT DISTINCT id_lon,id_lat FROM FT_MERRA2")
setDT(coords)
setorder(coords, id_lon, id_lat)
names(coords) <- c("longitude", "latitude")
coords[, ind := seq(.N)]

reanalise <- interp_usina(usinas, coords, conn_bancoR)
colnames(reanalise)[2] <- "rean"
setDT(reanalise)

# VERIFICADOS --------------------------------------------------------------------------------------

verificados <- getverificado(conn_banco1, datahoras = "2017/2021-01", campos = c("vento", "id_usina"))
setDT(verificados)
colnames(verificados)[2] <- "obs"
verificados <- merge(verificados, usinas[, .(id, codigo)], by.x = "id_usina", by.y = "id")
verificados[, id_usina := NULL]

# COMPARACOES DE DADOS -----------------------------------------------------------------------------

dcomp     <- merge(reanalise, verificados, by = c("data_hora", "codigo"))
dcomp_dia <- dcomp[, .(rean = mean(rean), obs = mean(obs)),
    by = .(data_hora = format(data_hora, format = "%Y/%m/%d"), codigo)]
dcomp_mes <- dcomp[, .(rean = mean(rean), obs = mean(obs)),
    by = .(data_hora = format(data_hora, format = "%Y/%m"), codigo)]

dcomp[, resol := "Horario"]
dcomp_dia[, resol := "Diario"]
dcomp_mes[, resol := "Mensal"]

dplot <- rbind(dcomp[, -"data_hora"],
    dcomp_dia[, -"data_hora"],
    dcomp_mes[, -"data_hora"]
)
dplot[, resol := factor(resol, levels = c("Horario", "Diario", "Mensal"))]

for(usi in unique(dcomp$codigo)) {
    rangemax <- c(0, max(max(dplot[codigo == usi, rean]), max(dplot[codigo == usi, obs])))
    gg <- ggplot(dplot[codigo == usi], aes(rean, obs)) + geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "blue") +
        scale_x_continuous(limits = rangemax) +
        scale_y_continuous(limits = rangemax) +
        facet_wrap(~ resol, ncol = 1) +
        labs(x = "Vento de Reanalise", y = "Vento Observado", title = toupper(usi)) +
        theme_bw()
    ggsave(file.path("out/compara_reanalise_obs/", paste0(usi, ".png")), gg, width = 6, height = 8)
}

usinas_mantidas <- usinas[codigo %in% unique(dcomp$codigo)]
gg <- ggplot(shape) +
    geom_polygon(aes(long, lat, group = group), fill = NA, color = "grey50") +
    geom_point(data = usinas_mantidas, aes(longitude, latitude), color = "blue") +
    theme_bw()
ggsave("out/compara_reanalise_obs/usinas_teste.png", gg, width = 7, height = 6)
