library(ggplot2)
library(data.table)
library(ggplot2)

shape <- readRDS("data/shape.rds")
usinas <- readRDS("data/usinas.rds")
usinas_total <- readRDS("data/conf_eol.rds")

# INICIALIZACAO ------------------------------------------------------------------------------------

arq_conf <- commandArgs(trailingOnly = TRUE)
arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
if(length(arq_conf) == 0) arq_conf <- "conf/default/adiciona_usinas_default.jsonc"

CONF <- jsonlite::read_json(arq_conf, TRUE)

outdir <- file.path("out/adiciona_usinas", CONF$tag)
dir.create(outdir, recursive = TRUE)

# EXECUCAO PRINCIPAL -------------------------------------------------------------------------------

usi_cluster <- lapply(CONF$clusters, fread)
usi_cluster <- rbindlist(usi_cluster)
usi_cluster <- usi_cluster[!duplicated(usi_cluster, fromLast = TRUE)]
centroide_cluster <- merge(usinas, usi_cluster)
centroide_cluster <- centroide_cluster[, lapply(.SD, mean), by = Cluster,
    .SDcols = c("longitude", "latitude")]

usi_sem_cluster <- usinas_total[!(codigo %in% usi_cluster$codigo)]
mais_proximo <- sapply(seq(nrow(centroide_cluster)), function(i) {
    dists <- mapply("-", centroide_cluster[i, -1], usi_sem_cluster[, .(longitude, latitude)])
    sqrt(dists[, 1]^2 + dists[, 2]^2)
})
mais_proximo <- centroide_cluster[apply(mais_proximo, 1, which.min), Cluster]
usi_sem_cluster[, Cluster := mais_proximo]

usinas_total <- merge(usinas_total, usi_cluster, by = "codigo", all = TRUE)
usinas_total <- rbind(usinas_total[!is.na(Cluster)], usi_sem_cluster)

gg <- ggplot(shape, aes(long, lat)) + geom_polygon(aes(group = group), fill = NA, color = "grey60") +
    geom_point(data = usinas_total, aes(longitude, latitude, color = Cluster)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(text = element_text(size = 14))
ggsave(file.path(outdir, "clusters_finais.png"), gg, width = 10, height = 8)

out <- usinas_total[, .(Cluster, iniop, capinst)]
setorder(out, Cluster, iniop)
out[, capinst_acum := cumsum(capinst), by = Cluster]
out[, capinst := NULL]
colnames(out) <- c("Cluster", "Data", "CapInst_acum")

fwrite(out, file.path(outdir, "capinst_acum_cluster.csv"))