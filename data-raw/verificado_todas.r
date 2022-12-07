library(dbrenovaveis)
library(data.table)

# FUNCOES AUX --------------------------------------------------------------------------------------

convertehora <- function(s, diff = 2) {
    s    <- as.numeric(sub("V", "", s)) - diff
    hora <- s %/% 2
    min  <- (s %% 2) * 30

    out <- paste0(formatC(hora, width = 2, flag = "0"), ":", formatC(min, width = 2, flag = "0"))
    return(out)
}

processa_arquivo <- function(arq) {
    usi <- sub(".*/", "", sub("_.*", "", arq))
    dat <- fread(arq, na.strings = "999")
    dat <- melt(dat, id.vars = "V1", value.name = "geracao")
    dat[, V1 := as.Date(as.character(V1), format = "%Y%m%d")]
    dat[, variable := convertehora(variable)]
    dat[, data_hora := paste(V1, variable)]
    dat[, data_hora := as.POSIXct(data_hora, format = "%Y-%m-%d %H:%M", tz = "GMT")]
    dat[, V1 := NULL]
    dat[, variable := NULL]
    dat <- dat[, .(geracao = mean(geracao, na.rm = TRUE), count = mean(!is.na(geracao))),
        by = .(data_hora = format(data_hora, "%Y-%m"))]
    dat[, data_hora := as.Date(paste0(data_hora, "-01"))]
    dat[, usina := usi]

    # Garantia de que valores mensais possivelmente espurios sejam retirados da conta
    # qualquer coisa com media mensal tao baixa certamente era dado errado
    dat[geracao < 0.1, geracao := NA_real_]

    return(dat)
}

# INIT ---------------------------------------------------------------------------------------------

ndias <- structure(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), names = 1:12)

conn <- conectalocal("data")
usinas <- getusinas(conn)

p_backup <- "C:/Users/lucask/Downloads/Geração Verificada"
p_mhg <- file.path("//rj-vd-weol-11/d$/ModeloPrevEolico/ModEolGeral", 
    "VersaoAutomatica/Arquivos de Saída/MHG Usinas/Geração Verificada")

l_verifpath <- lapply(usinas$codigo, function(usi) {
    c(
        list.files(p_backup, paste0(usi, ".*Ger_Verif_Consis.txt$"), full.names = TRUE),
        list.files(p_mhg, paste0(usi, ".*Ger_Verif_Consis.txt$"), full.names = TRUE)
    )
})
names(l_verifpath) <- usinas$codigo

hists <- lapply(names(l_verifpath), function(usi) {
    print(usi)
    ld <- lapply(l_verifpath[[usi]], processa_arquivo)
    out <- rbindlist(ld)
    out <- out[!duplicated(out$data_hora, fromLast = TRUE)]
    out
})

hists <- rbindlist(hists)
colnames(hists)[4] <- "codigo"

hists <- merge(hists, usinas[, .(codigo, id)])
hists[, codigo := NULL]
setcolorder(hists, c("id", "data_hora", "geracao", "count"))
colnames(hists)[1] <- "id_usina"

hists[, tira := all(is.na(geracao)), by = id_usina]
hists <- hists[tira == FALSE]
hists <- hists[, tira := NULL]

fwrite(hists, "data/verificados.csv")

# ------------------------------------------------------------------------------

usinas <- fread("data/usinas.csv")
usinas <- usinas[id %in% hists$id_usina]

fwrite(usinas, "data/usinas.csv")
