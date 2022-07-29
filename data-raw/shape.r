library(rgdal)

shape <- readOGR(dsn = "data-raw/shape", layer = "UFEBRASIL", verbose = FALSE)
shape <- ggplot2::fortify(shape)

saveRDS(shape, "data/shape.rds")
