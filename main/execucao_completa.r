
main <- function(arq_conf, activate = TRUE) {

    # A ativacao do ambiente virtual nao pode ser feita por fora, e rodadas a partir de outros
    # diretorios que nao o root nao necessariamente terao os pacotes renv e this.path disponiveis
    # Dessa forma, so e possivel ativa-lo caso o programa esteja sendo rodado de dentro da raiz
    # ou pelo executavel eolica-newave-dados, que conhece o diretorio de instalacao do programa
    root <- Sys.getenv("INSTALLDIR", getwd())

    wd0 <- getwd()
    setwd(root)
    arq <- list.files("renv", "activate", full.names = TRUE)
    source(arq)
    setwd(wd0)

    if(missing("arq_conf")) {
        arq_conf <- commandArgs(trailingOnly = TRUE)
        arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
    }
    if(length(arq_conf) == 0) arq_conf <- file.path(root, "conf", "default", "execucao_completa_default.jsonc")
    CONF <- jsonlite::read_json(arq_conf)

    subsistemas   <- structure(CONF$subsistemas, names = CONF$subsistemas)
    CONF$clusters <- lapply(subsistemas, function(subsist) {
        arq <- paste(subsist, names(CONF$mod_compact[[subsist]]), names(CONF$mod_clust[[subsist]]), sep = "_")
        arq <- paste0(arq, ".csv")
        file.path(CONF$outdir, "clusteriza_usinas", CONF$tag, arq)
    })

    tmpdir  <- tempdir()
    auxconf <- file.path(tmpdir, "auxconf.jsonc")
    jsonlite::write_json(CONF, auxconf)

    parts <- file.path(root, "main",
        c("clusteriza_usinas.r", "estima_ftm.r", "adiciona_usinas.r"))

    for(part in parts) {
        local({
            source(part)
            main(auxconf, activate = FALSE)
        })
    }
}

main()