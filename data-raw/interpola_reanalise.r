library(dbrenovaveis)
library(data.table)

source("R/utils.r")

conn <- conectalocal("data/")

usinas <- getusinas(conn)
usinas <- usinas[!duplicated(id)]

# EXTRACAO DOS DADOS DE REANALISE ------------------------------------------------------------------

vertices <- getvertices(conn)

rean_interp <- interp_usina(usinas, vertices, conn, "1900/")
setorder(rean_interp, id_usina, data_hora)
setcolorder(rean_interp, c("id_usina", "data_hora", "vento"))

fwrite(rean_interp, "data/reanalise_interpolado.csv")
