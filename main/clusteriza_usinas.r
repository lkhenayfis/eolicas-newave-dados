suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(clustcens))
suppressPackageStartupMessages(library(mclust)) # Nao e necessario chamar diretamente, esta aqui so pro renv ver
suppressPackageStartupMessages(library(logr))

main <- function(arq_conf) {

    if(is.null(this.path::this.dir2())) {
        root <- getwd()
    } else {
        root <- this.path::this.dir2()
        root <- sub("/main", "", root)
    }

    source(file.path(root, "R", "utils.r"))
    source(file.path(root, "R", "parseconfs.r"))
    source(file.path(root, "R", "altlogs.r"))

    # INICIALIZACAO --------------------------------------------------------------------------------

    if(missing("arq_conf")) {
        arq_conf <- commandArgs(trailingOnly = TRUE)
        arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
    }
    if(length(arq_conf) == 0) arq_conf <- file.path(root, "conf", "default", "clusteriza_usinas_default.jsonc")
    CONF <- parseconf_clustusi(arq_conf)

    logopen  <- func_logopen(CONF$log_info$dolog)
    logprint <- func_logprint(CONF$log_info$dolog)
    logclose <- func_logclose(CONF$log_info$dolog)

    timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    timestamp <- file.path(CONF$log_info$logdir, "log", paste0("clusteriza_usinas_", timestamp))
    logopen(timestamp, FALSE)

    logprint(paste0("Arquivo de configuracao: ", arq_conf))

    logprint(paste0("\n", yaml::as.yaml(CONF), "\n"), console = FALSE)
    cat(paste0("\n", yaml::as.yaml(CONF), "\n"))

    outdir <- file.path(CONF$outdir, "clusteriza_usinas", CONF$tag)
    if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    if(CONF$limpadir) inv <- file.remove(list.files(outdir, full.names = TRUE))

    # LEITURA DOS DADOS NECESSARIOS ----------------------------------------------------------------

    dat_usinas <- readRDS("data/usinas.rds")
    usi_comhist <- sub(".rds", "", list.files("data/mhg"))
    dat_usinas <- dat_usinas[codigo %in% usi_comhist]

    # EXECUCAO PRINCIPAL ---------------------------------------------------------------------------

    index_loop <- lapply(CONF$subs, function(ss) {
        expand.grid(sub = ss, compact = names(CONF$mod_compact[[ss]]), cluster = names(CONF$mod_cluster[[ss]]))
    })
    index_loop <- rbindlist(index_loop)

    for(i in seq(nrow(index_loop))) {

        logprint(unname(unlist(index_loop[i, ])))

        subsist <- index_loop$subsist[i]
        compac  <- index_loop$compac[i]
        clst    <- index_loop$clst[i]

        if(track_s != subsist) {
            rean_mensal <- readRDS(file.path("data", paste0("reanalise_", subsist, ".rds")))
            rean_mensal[, grupo := subsist]
            colnames(rean_mensal)[1:3] <- c("indice", "valor", "cenario")
            rean_mensal <- clustcens:::new_cenarios(rean_mensal)
        }
        if((track_s != subsist) || (track_c != compac)) {
            rean_compac <- CONF$mod_compac[[index_loop$compac[i]]]
            rean_compac$cenarios <- quote(rean_mensal)
            rean_compac <- eval(rean_compac)
            rean_compac$compact[, valor := scale(valor), by = .(ind)]
        }
        track_s <- subsist
        track_c <- compac

        clusters <- CONF$mod_cluster[[index_loop$clst[i]]]
        clusters$compact <- quote(rean_compac)
        clusters <- eval(clusters)

        classe <- getclustclass(clusters)

        outarq <- file.path(outdir, paste0(subsist, "_", compac, "_", index_loop$clst[i], ".csv"))
        fwrite(usiplot[, .(codigo, Cluster)], outarq)
    }

    on.exit(logclose())
}
