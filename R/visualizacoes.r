###################################### FUNCOES DE VISUALIZACAO #####################################

library(ggplot2)

plota_ftms <- function(regdata, ftms) {

    gg_potevol <- ggplot(regdata, aes(data_hora, capacidade_instalada)) +
        geom_line() + geom_point() +
        labs(x = "Data", y = "Capacidade Instalada") +
        facet_wrap(~ cluster, scales = "free_y") +
        theme_bw()

    XX <- data.table(vento = seq(0, 15, by = .1))
    dd_ftms <- lapply(ftms, function(mod) {
        fc <- predict(mod, newdata = XX)
        data.table(vento = XX[[1]], fator_capacidade = fc)
    })
    dd_ftms <- rbindlist(lapply(names(dd_ftms), function(s) cbind(dd_ftms[[s]], cluster = s)))

    colnames(regdata)[2] <- "Data"
    gg_ftms <- ggplot(mapping = aes(vento, fator_capacidade)) +
        geom_point(data = regdata, aes(color = Data)) +
        geom_line(data = dd_ftms) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_color_viridis_c() +
        labs(x = "Velocidade do vento", y = "Fator de Capacidade") +
        facet_wrap(~ cluster) +
        theme_bw()

    return(list(gg_potevol, gg_ftms))
}

plota_clusters <- function(usinas, shape) {

    gg_clust <- lapply(unique(usinas$subsistema), function(subsist) {
        dat_usi <- usinas[subsistema == subsist]

        ggplot() +
            geom_polygon(data = shape, aes(long, lat, group = group), fill = NA, color = 1) +
            geom_point(data = dat_usi, aes(longitude, latitude, color = cluster)) +
            labs(x = "Longitude", y = "Latitude") +
            coord_cartesian(xlim = dat_usi[, range(longitude)], ylim = dat_usi[, range(latitude)]) +
            theme_bw()
    })
    names(gg_clust) <- unique(usinas$subsistema)

    return(gg_clust)
}