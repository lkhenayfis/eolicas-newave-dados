
main <- function(arq_conf, activate = FALSE) {

    if(is.null(this.path::this.dir2())) {
        root <- getwd()
    } else {
        root <- this.path::this.dir2()
        root <- sub("/main", "", root)
    }

    if(activate) renv::activate(root)

    # as chamadas de bibliotecas precisam estar por dentro de main para que haja certeza do ambiente
    # ter sido corretamente carregado
    suppressPackageStartupMessages(library(data.table))
    suppressPackageStartupMessages(library(dbrenovaveis))
    suppressPackageStartupMessages(library(logr))

    source(file.path(root, "R", "utils.r"))
    source(file.path(root, "R", "parseconfs.r"))
    source(file.path(root, "R", "altlogs.r"))

    # INICIALIZACAO ---------------------------------------------------------------------------------

    if(missing("arq_conf")) {
        arq_conf <- commandArgs(trailingOnly = TRUE)
        arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
    }
    if(length(arq_conf) == 0) arq_conf <- file.path(root, "conf", "default", "estima_ftm_default.jsonc")
    CONF <- jsonlite::read_json(arq_conf, TRUE)

    logopen  <- func_logopen(CONF$log_info$dolog)
    logprint <- func_logprint(CONF$log_info$dolog)
    logclose <- func_logclose(CONF$log_info$dolog)

    timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    timestamp <- paste0("estima_ftm_", timestamp)
    logopen(timestamp)

    logprint(paste0("Arquivo de configuracao: ", arq_conf))

    logprint(paste0("\n", yaml::as.yaml(CONF), "\n"), console = FALSE)
    cat(paste0("\n", yaml::as.yaml(CONF), "\n"))

    CONF$janela <- dbrenovaveis:::parsedatas(CONF$janela, "", FALSE)
    CONF$janela <- lapply(seq(2), function(i) as.Date(CONF$janela[[i]][i]))

    if(CONF$datasource$tipo == "csv") {
        conn <- conectalocal(CONF$datasource$diretorio)
    } else {
        stop("Tipo de 'datasource' nao reconhecido")
    }

    outdir <- file.path("out/estima_ftm", CONF$tag)
    dir.create(outdir, recursive = TRUE)

    # LEITURA DOS DADOS ----------------------------------------------------------------------------

    logprint("Leitura dos dados de entrada")

    clusters <- lapply(CONF$clusters, fread)
    clusters <- rbindlist(clusters)
    clusters <- clusters[!duplicated(clusters, fromLast = TRUE)]
    usinas <- getusinas(conn)
    usinas <- merge(usinas, clusters)

    pot_evol <- determina_pot_evol(usinas, CONF$janela)

    # EXECUCAO PRINCIPAL ---------------------------------------------------------------------------

    logprint("Estimacao das FTMs")

    geracao <- getverificado(conn, campos = "*")
    reanalise <- getreanalise(conn, modo = "interpolado")

    regdata <- monta_regdata(usinas, geracao, reanalise, pot_evol)
    ftms    <- lapply(split(regdata, regdata$cluster), lm, formula = fator_capacidade ~ vento)

    outmod <- lapply(ftms, function(mod) coef(mod))
    outmod <- lapply(names(outmod), function(nome) {
        data.table(Cluster = nome, b0 = outmod[[nome]][1], b1 = outmod[[nome]][2])
    })
    outmod <- rbindlist(outmod)

    outarq <- file.path(outdir, "vento_medio.csv")
    fwrite(regdata[, .(cluster, vento)], outarq)

    outarq <- file.path(outdir, "ftm.csv")
    fwrite(outmod, outarq)

    logprint("======== CONCLUIDO ========")

    on.exit(logclose())
}

main()