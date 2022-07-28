
main <- function(arq_conf, activate = FALSE) {

    root <- Sys.getenv("INSTALLDIR", getwd())

    if(activate) renv::activate(root)

    # as chamadas de bibliotecas precisam estar por dentro de main para que haja certeza do ambiente
    # ter sido corretamente carregado
    suppressWarnings({
        suppressPackageStartupMessages(library(data.table))
        suppressPackageStartupMessages(library(dbrenovaveis))
        suppressPackageStartupMessages(library(clustcens))
        suppressPackageStartupMessages(library(logr))
    })
    source(file.path(root, "R", "utils.r"))
    source(file.path(root, "R", "parseconfs.r"))
    source(file.path(root, "R", "altlogs.r"))

    # INICIALIZACAO --------------------------------------------------------------------------------

    if(missing("arq_conf")) {
        arq_conf <- commandArgs(trailingOnly = TRUE)
        arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
    }
    if(length(arq_conf) == 0) arq_conf <- file.path(root, "conf", "default", "clusteriza_usinas_default.jsonc")
    CONF <- jsonlite::read_json(arq_conf, TRUE)

    logopen  <- func_logopen(CONF$log_info$dolog)
    logprint <- func_logprint(CONF$log_info$dolog)
    logclose <- func_logclose(CONF$log_info$dolog)

    timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    timestamp <- file.path(CONF$log_info$logdir, "log", paste0("clusteriza_usinas_", timestamp))
    logopen(timestamp, FALSE)

    if(CONF$log_info$trace > 0) {
        logprint("======= CLUSTERIZACAO DE USINAS =======")
        cat("\n")
    }

    if(CONF$log_info$trace >= 2) logprint(paste0("Arquivo de configuracao: ", arq_conf))

    if(CONF$log_info$trace == 3) {
        logprint(paste0("\n", yaml::as.yaml(CONF), "\n"), console = FALSE)
        cat(paste0("\n", yaml::as.yaml(CONF), "\n"))
    }

    CONF <- parseconf_clustusi(CONF)

    outdir <- file.path(CONF$outdir, "clusteriza_usinas", CONF$tag)
    if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    if(CONF$limpadir) inv <- file.remove(list.files(outdir, full.names = TRUE))

    if(CONF$datasource$tipo == "csv") {
        conn <- conectalocal(CONF$datasource$diretorio)
    } else {
        stop("Tipo de 'datasource' nao reconhecido")
    }

    # LEITURA DOS DADOS NECESSARIOS ----------------------------------------------------------------

    if(CONF$log_info$trace > 0)  logprint("LEITURA DOS DADOS")

    usinas <- getusinas(conn)
    usinas <- usinas[data_inicio_operacao <= CONF$data_ref]

    # EXECUCAO PRINCIPAL ---------------------------------------------------------------------------

    if(CONF$log_info$trace > 0)  logprint("CLUSTERIZACAO DAS USINAS")

    index_loop <- lapply(CONF$subs, function(ss) {
        expand.grid(subsistema = ss,
                    compact = names(CONF$mod_compact[[ss]]),
                    cluster = names(CONF$mod_cluster[[ss]]),
                    stringsAsFactors = FALSE)
    })
    index_loop <- rbindlist(index_loop)

    track_s <- ""
    track_c <- ""

    for(i in seq(nrow(index_loop))) {

        subsist <- index_loop$subsistema[i]
        compac  <- index_loop$compact[i]
        clst    <- index_loop$cluster[i]

        if(CONF$log_info$trace > 0) {
            logprint(paste0("*    ", "Subsistema: ", subsist, " -- Compactacao: ",
               compac, " -- Clusterizacao: ", clst))
        }

        if(track_s != subsist) {
            rean_mensal <- getreanalise(conn, usinas = usinas[subsistema == subsist, codigo],
                modo = "interp")
            rean_mensal <- merge(rean_mensal, usinas[, .(id, codigo)], by.x = "id_usina", by.y = "id")
            rean_mensal[, id_usina := NULL]
            rean_mensal[, grupo := subsist]
            colnames(rean_mensal)[1:3] <- c("indice", "valor", "cenario")
            rean_mensal <- clustcens:::new_cenarios(rean_mensal)
        }
        if((track_s != subsist) || (track_c != compac)) {
            rean_compac <- CONF$mod_compac[[subsist]][[index_loop$compact[i]]]
            rean_compac$cenarios <- quote(rean_mensal)
            rean_compac <- eval(rean_compac)
            rean_compac$compact[, valor := scale(valor), by = .(ind)]
        }
        track_s <- subsist
        track_c <- compac

        clusters <- CONF$mod_cluster[[subsist]][[index_loop$cluster[i]]]
        clusters$compact <- quote(rean_compac)
        clusters <- eval(clusters)

        classe <- getclustclass(clusters)

        out <- data.table(codigo = unique(rean_compac$compact$cenario), cluster = factor(classe))
        out <- merge(out, usinas, by = "codigo")

        outarq <- file.path(outdir, paste0(subsist, "_", compac, "_", index_loop$cluster[i], ".csv"))
        fwrite(out[, .(codigo, cluster)], outarq)

        outarq <- file.path(outdir, paste0(subsist, "_", compac, "_", index_loop$cluster[i],
            "_cmptclst.rds"))
        saveRDS(list(rean_compac, clusters), outarq)
    }

    if(CONF$log_info$trace > 0) {
        logprint("CLUSTERIZACAO CONCLUIDA")
        cat("\n")
    }
    on.exit(logclose())
}

ca <- commandArgs()
ca <- ca[grep("--file", ca)]
ca <- sub("--file=.*(/|\\\\)", "", ca)
if(length(ca) == 0) ca <- "FAULT"
thisarq <- this.path::this.path()

if(grepl(ca, thisarq)) main(activate = TRUE)
