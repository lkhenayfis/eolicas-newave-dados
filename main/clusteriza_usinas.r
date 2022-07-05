library(data.table)
library(clustcens)
library(mclust) # Nao e necessario chamar diretamente, esta aqui so pro renv ver
library(ggplot2)

source("R/utils.r")

# INICIALIZACAO ------------------------------------------------------------------------------------

arq_conf <- commandArgs(trailingOnly = TRUE)[1]
if(is.na(arq_conf)) arq_conf <- "conf/default/clusteriza_usinas_default.jsonc"

CONF <- jsonlite::read_json(arq_conf, TRUE)
CONF$mod_cluster <- lapply(CONF$mod_cluster, function(l) {
    l[[1]] <- paste0("clust", l[[1]])
    l[[1]] <- str2lang(l[[1]])
    as.call(l)
})
CONF$mod_compact <- lapply(CONF$mod_compact, function(l) {
    l[[1]] <- paste0(l[[1]], "cens")
    l[[1]] <- str2lang(l[[1]])
    as.call(l)
})

shape <- readRDS("data/shape.rds")

dat_usinas <- readRDS("data/usinas.rds")
usi_comhist <- sub(".rds", "", list.files("data/mhg"))
dat_usinas <- dat_usinas[codigo %in% usi_comhist]

outdir <- file.path("out/clusteriza_usinas", CONF$tag)
dir.create(outdir, recursive = TRUE)
if(CONF$limpadir) file.remove(list.files(outdir, full.names = TRUE))

# EXECUCAO PRINCIPAL -------------------------------------------------------------------------------

index_loop <- expand.grid(clst = names(CONF$mod_cluster), compac = names(CONF$mod_compact),
    subsist = CONF$Subsistemas, stringsAsFactors = FALSE)

track_s <- ""
track_c <- ""

for(i in seq(nrow(index_loop))) {

    print(index_loop[i, , drop = FALSE])

    subsist <- index_loop$subsist[i]
    compac  <- index_loop$compac[i]
    clst    <- index_loop$clst[i]

    if(track_s != subsist) {
        rean_mensal <- readRDS(file.path("data", paste0("reanalise_", subsist, ".rds")))
        rean_mensal[, grupo := subsist]
        colnames(rean_mensal)[1:3] <- c("indice", "valor", "cenario")
        rean_mensal <- clustcens:::new_cenarios(rean_mensal)
        track_s <- subsist
    }
    if(track_c != compac) {
        rean_compac <- CONF$mod_compac[[index_loop$compac[i]]]
        rean_compac$cenarios <- quote(rean_mensal)
        rean_compac <- eval(rean_compac)
        rean_compac$compact[, valor := scale(valor), by = .(ind)]
        track_c <- compac
    }

    clusters <- CONF$mod_cluster[[index_loop$clst[i]]]
    clusters$compact <- quote(rean_compac)
    clusters <- eval(clusters)

    classe <- getclustclass(clusters)

    usiplot <- merge(dat_usinas,
        data.table(codigo = unique(rean_mensal$cenarios$cenario),
            cluster = paste0("cluster_", subsist, "_", classe)))

    gg <- ggplot() +
        geom_polygon(data = shape, aes(long, lat, group = group), fill = NA, color = "grey60") +
        geom_point(data = usiplot, aes(longitude, latitude, color = cluster)) +
        coord_cartesian(xlim = range(usiplot$longitude), ylim = range(usiplot$latitude)) +
        labs(title = index_loop$clst[i]) +
        theme_bw()
    outarq <- file.path(outdir, paste0(subsist, "_", compac, "_", index_loop$clst[i], ".png"))
    ggsave(outarq, gg, width = 8, height = 8)

    outarq <- file.path(outdir, paste0(subsist, "_", compac, "_", index_loop$clst[i], ".csv"))
    fwrite(usiplot[, .(codigo, cluster)], outarq)
}
