suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(logr))

# INICIALIZACAO ------------------------------------------------------------------------------------

timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
timestamp <- paste0("estima_ftm_", timestamp)
log_open(timestamp)

arq_conf <- commandArgs(trailingOnly = TRUE)
arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
if(length(arq_conf) == 0) arq_conf <- "conf/default/estima_ftm_default.jsonc"

log_print(paste0("Arquivo de configuracao: ", arq_conf))

CONF <- jsonlite::read_json(arq_conf, TRUE)
log_print(paste0("\n", yaml::as.yaml(CONF), "\n"), console = FALSE)
cat(paste0("\n", yaml::as.yaml(CONF), "\n"))

CONF$janela <- dbrenovaveis:::parsedatas(CONF$janela, "", FALSE)
CONF$janela <- lapply(seq(2), function(i) as.Date(CONF$janela[[i]][i]))

dat_usinas <- readRDS("data/usinas.rds")

outdir <- file.path("out/estima_ftm", CONF$tag)
dir.create(outdir)

# LEITURA DOS CLUSTERS -----------------------------------------------------------------------------

usi_cluster <- lapply(CONF$clusters, fread)
usi_cluster <- rbindlist(usi_cluster)
usi_cluster <- usi_cluster[!duplicated(usi_cluster, fromLast = TRUE)]

dat_usinas <- merge(dat_usinas, usi_cluster)

pot_evol <- lapply(split(dat_usinas, dat_usinas$cluster), function(dat) {
    setorder(dat, iniop)
    datas_ini <- dat$iniop
    pot_evol  <- cumsum(dat$capinst)

    datas <- seq(CONF$janela[[1]], CONF$janela[[2]], by = "month")
    pot_evol_meses <- sapply(datas, function(dt) max(pot_evol[datas_ini <= dt]))

    data.table(data_hora = datas, capinst = pot_evol_meses)
})

pot_evol <- lapply(names(pot_evol), function(n) cbind(pot_evol[[n]], cluster = n))
pot_evol <- rbindlist(pot_evol)

gg <- ggplot(pot_evol, aes(data_hora, capinst, color = cluster)) + geom_line() + geom_point() +
    scale_color_discrete(name = "Cluster") +
    facet_wrap(~ cluster, scales = "free_y") +
    labs(x = "Data", y = "Capacidade instalada") +
    theme_bw() +
    theme(text = element_text(size = 14))
outarq <- file.path(outdir, "pot_evol_cluster.png")
ggsave(outarq, gg, width = 9, height = 6)

# EXECUCAO PRINCIPAL -------------------------------------------------------------------------------

reanalise <- list.files("data", pattern = "reanalise", full.names = TRUE)
reanalise <- lapply(reanalise, readRDS)
reanalise <- rbindlist(reanalise)

reanalise <- merge(reanalise, dat_usinas[, .(codigo, cluster)], by = "codigo")
reanalise <- reanalise[, .(vento_medio = mean(vento_reanalise)), by = .(cluster, data_hora)]

vento_obs <- lapply(split(dat_usinas, dat_usinas$cluster), function(dat) {
    arqs <- paste0("data/mhg/", dat$codigo, ".rds")
    out <- rbindlist(lapply(arqs, readRDS))
    out <- out[, .(geracao = sum(geracao, na.rm = TRUE), count = mean(count)), by = .(data_hora)]
    out[, cluster := rep(dat$cluster[1], .N)]
    setorder(out, data_hora)
    out
})
vento_obs <- rbindlist(vento_obs)

regdata <- Reduce(merge, list(vento_obs, reanalise, pot_evol))
regdata[, fator_cap := geracao / max(capinst), by = cluster]
regdata[, peso := (capinst / max(capinst))^3 * count, by = cluster]
setorder(regdata, cluster, data_hora)

mods <- lapply(split(regdata, regdata$cluster), function(dat) {
    lm(fator_cap ~ vento_medio, dat, weights = dat$peso)
})
prevs <- lapply(names(mods), function(n) {
    xx <- data.frame(vento_medio = seq(0, 10, by = .1))
    pred <- predict(mods[[n]], newdata = xx)
    data.table(vento_medio = xx[[1]], fator_cap = pred, cluster = n)
})
prevs <- rbindlist(prevs)

gg1 <- ggplot(regdata, aes(vento_medio, fator_cap, color = peso)) + geom_point() +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Vento m\u00e9dio [m/s]", y = "Fator de Capacidade [%]") +
    scale_color_viridis_c(name = "Peso") +
    facet_wrap(~cluster) +
    theme_bw()
outarq <- file.path(outdir, "scatter_clust.png")
ggsave(outarq, gg1, width = 9, height = 6)

gg2 <- ggplot() +
    geom_point(data = regdata, aes(vento_medio, fator_cap, color = peso)) +
    geom_line(data = prevs, aes(vento_medio, fator_cap)) +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Vento m\u00e9dio [m/s]", y = "Fator de Capacidade [%]") +
    scale_color_viridis_c(name = "Peso") +
    facet_wrap(~cluster) +
    theme_bw()
outarq <- file.path(outdir, "scatter_clust_ftm.png")
ggsave(outarq, gg2, width = 9, height = 6)
