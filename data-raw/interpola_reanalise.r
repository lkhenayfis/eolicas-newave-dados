library(odbc)
library(data.table)

source("R/utils.r")

dat_usinas <- readRDS("data/usinas.rds")
usi_comhist <- sub(".rds", "", list.files("data/mhg"))
dat_usinas <- dat_usinas[codigo %in% usi_comhist]

# EXTRACAO DOS DADOS DE REANALISE ------------------------------------------------------------------

# Coordenadas da grade de reanalise do MERRA2 (passado pela EPE)
conn <- dbConnect(odbc(),
    driver = "SQL Server", server = "prd-sql-09\\eolica", database = "MERRA2", uid = "LKSouza", pwd = "LKSouza")

grade_merra2 <- as.data.table(dbGetQuery(conn, "SELECT DISTINCT id_lon,id_lat FROM FT_MERRA2"))
names(grade_merra2) <- c("longitude", "latitude")
setorder(grade_merra2, longitude, latitude)
grade_merra2[, ind := seq(.N)]

for(subsistema in c("NE", "S")) {
    dat_clust <- interp_usina(dat_usinas[sub == subsistema], grade_merra2, conn, "1900/", agr = "mes")
    saveRDS(dat_clust, file.path("data", paste0("reanalise_", subsistema, ".rds")))
}