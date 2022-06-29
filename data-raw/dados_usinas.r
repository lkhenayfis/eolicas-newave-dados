library(data.table)

usinas <- fread("data-raw/Dados das Usinas.txt")
usinas[, V1 := sub("_.*", "", V1)]
usinas[] <- lapply(usinas, trimws)
usinas[, 2:5] <- lapply(usinas[, 2:5], as.numeric)
names(usinas) <- c("codigo", "capinst", "latitude", "longitude", "iniop", "A", "sub", "iniop2", "sub2", "B")
usinas[, iniop  := as.Date(as.character(iniop),  format = "%Y%m%d")]
usinas[, iniop2 := as.Date(as.character(iniop2), format = "%Y%m%d")]

saveRDS(usinas, "data/usinas.rds")
