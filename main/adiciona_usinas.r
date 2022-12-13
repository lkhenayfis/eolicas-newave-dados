
main <- function(arq_conf, activate = TRUE) {

    # A ativacao do ambiente virtual nao pode ser feita por fora, e rodadas a partir de outros
    # diretorios que nao o root nao necessariamente terao os pacotes renv e this.path disponiveis
    # Dessa forma, so e possivel ativa-lo caso o programa esteja sendo rodado de dentro da raiz
    # ou pelo executavel eolica-newave-dados, que conhece o diretorio de instalacao do programa
    root <- Sys.getenv("INSTALLDIR", getwd())

    if(activate) {
        wd0 <- getwd()
        setwd(root)
        arq <- list.files("renv", "activate", full.names = TRUE)
        source(arq)
        setwd(wd0)
    }

    suppressWarnings({
        suppressPackageStartupMessages(library(data.table))
        suppressPackageStartupMessages(library(dbrenovaveis))
        suppressPackageStartupMessages(library(clustcens))
    })
    source(file.path(root, "R", "utils.r"))
    source(file.path(root, "R", "parseconfs.r"))
    source(file.path(root, "R", "altlogs.r"))

    # INICIALIZACAO --------------------------------------------------------------------------------

    if(missing("arq_conf")) {
        arq_conf <- commandArgs(trailingOnly = TRUE)
        arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
    }
    if(length(arq_conf) == 0) arq_conf <- file.path(root, "conf", "default", "adiciona_usinas_default.jsonc")

    CONF <- jsonlite::read_json(arq_conf, TRUE)

    logopen  <- func_logopen(CONF$log_info$dolog)
    logprint <- func_logprint(CONF$log_info$dolog)
    logclose <- func_logclose(CONF$log_info$dolog)

    timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    timestamp <- paste0("estima_ftm_", timestamp)
    logopen(timestamp)

    if(CONF$log_info$trace > 0) {
        logprint("======== ADICAO DE NOVAS USINAS =======")
        cat("\n")
    }

    if(CONF$log_info$trace >= 2) logprint(paste0("Arquivo de configuracao: ", arq_conf))

    if(CONF$log_info$trace == 3) {
        logprint(paste0("\n", yaml::as.yaml(CONF), "\n"), console = FALSE)
        cat(paste0("\n", yaml::as.yaml(CONF), "\n"))
    }

    if(CONF$datasource$tipo == "csv") {
        conn <- conectalocal(CONF$datasource$diretorio)
    } else {
        stop("Tipo de 'datasource' nao reconhecido")
    }

    outdir <- file.path(CONF$outdir, CONF$tag, "adiciona_usinas")
    if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

    # LEITURA DOS DADOS ----------------------------------------------------------------------------

    if(CONF$log_info$trace > 0)  logprint("LEITURA DOS DADOS")

    clusters <- lapply(CONF$clusters, fread)
    clusters <- rbindlist(clusters)
    clusters <- clusters[!duplicated(clusters, fromLast = TRUE)]
    usinas <- getusinas(conn)
    usinas <- usinas[!duplicated(ceg)]
    usinas <- merge(usinas, clusters, all = TRUE)
    usinas[, subsistema := sub("_.*", "", sub("cluster_", "", cluster))]
    usinas <- usinas[complete.cases(usinas)]
    usinas[, cluster := sub("cluster_(NE|S)_", "", cluster)]

    max_data <- round_month(usinas[!is.na(cluster), max(data_inicio_operacao)])

    cmpt_clst <- lapply(CONF$clusters, function(s) sub(".csv", "_cmptclst.rds", s))
    cmpt_clst <- lapply(cmpt_clst, readRDS)

    # EXECUCAO PRINCIPAL ---------------------------------------------------------------------------

    if(CONF$log_info$trace > 0)  logprint("ADICAO DE USINAS")
    for(subsist in unique(usinas$subsistema)) {

        if(CONF$log_info$trace > 0)  logprint(paste0("*    Subsistema: ", subsist))

        cmpt_clst_sub <- cmpt_clst[[subsist]]

        usi_sem_cluster <- usinas[(subsistema == subsist) & (is.na(cluster))]
        if(!CONF$coord_aprox_by_cluster) usi_sem_cluster <- usi_sem_cluster[coordenadas_aproximadas == FALSE]

        rean_mensal <- getreanalise(conn, modo = "interpolado", usinas = usi_sem_cluster$codigo,
            datahoras = paste0("/", max_data))
        rean_mensal <- merge(rean_mensal, usinas[, .(id, codigo)], by.x = "id_usina", by.y = "id")
        rean_mensal[, id_usina := NULL]
        rean_mensal[, grupo := subsist]
        colnames(rean_mensal)[1:3] <- c("indice", "valor", "cenario")
        rean_mensal <- clustcens:::new_cenarios(rean_mensal)

        # Primeira parte da adicao de novas usinas tentando usar os modelos estimados da
        # clusterizacao original
        usinas <- add_by_cluster(usinas, cmpt_clst_sub, rean_mensal)
    }

    # Para as que ainda continuam sem cluster, adiciona por proximidade geografica
    usinas <- add_by_geo(usinas)

    usinas[, cluster := paste("cluster", subsistema, cluster, sep = "_")]
    pot_evol_cluster <- determina_pot_evol(usinas)

    fwrite(pot_evol_cluster, file.path(outdir, "capinst_acum_cluster.csv"))

    if(CONF$log_info$trace > 0) {
        logprint("ADICAO CONCLUIDA")
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
