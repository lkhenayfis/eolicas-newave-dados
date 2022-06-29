library(data.table)

convertehora <- function(s, diff = 2) {
    s    <- as.numeric(sub("V", "", s)) - diff
    hora <- s %/% 2
    min  <- (s %% 2) * 30

    out <- paste0(formatC(hora, width = 2, flag = "0"), ":", formatC(min, width = 2, flag = "0"))
    return(out)
}

ndias <- structure(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), names = 1:12)

# MONTA O DADO A PARTIR DO BACKUP ------------------------------------------------------------------

p_verif <- "C:/Users/lucas/Downloads/Geração Verificada"
arqs    <- list.files(p_verif, pattern = "_Ger_Verif_Consis.txt$", full.names = TRUE)

for(arq in arqs) {
    usi <- sub(".*/", "", sub("_.*", "", arq))
    dat <- fread(arq)
    dat <- melt(dat, id.vars = "V1", value.name = "geracao")
    dat[, V1 := as.Date(as.character(V1), format = "%Y%m%d")]
    dat[, variable := convertehora(variable)]
    dat[, data_hora := paste(V1, variable)]
    dat[, data_hora := as.POSIXct(data_hora, format = "%Y-%m-%d %H:%M", tz = "GMT")]
    dat[, V1 := NULL]
    dat[, variable := NULL]
    dat <- dat[, .(geracao = mean(geracao, na.rm = TRUE), count = mean(!is.na(geracao))),
        by = .(anomes = format(data_hora, "%Y-%m"))]
    dat[, usina := usi]
    
    saveRDS(dat, file.path("data/mhg", paste0(usi, ".rds")))
}

# COMPLETA COM O MELHOR HISTORICO OPERACIONAL ------------------------------------------------------

usinas <- readRDS("data/usinas.rds")

usi_backup <- sub(".rds", "", list.files("data/mhg"))
usi_faltante <- usinas[!(codigo %in% usi_backup) & (A == 1), codigo]

p_verif <- "//rj-vd-weol-11/d$/ModeloPrevEolico/ModEolGeral/VersaoAutomatica/Arquivos de Saída/MHG Usinas/Geração Verificada"
pat  <- paste0(usi_faltante, collapse = "|")
pat  <- paste0("(", pat, ").*Ger_Verif_Consis.txt$")
arqs <- list.files(p_verif, pattern = pat, full.names = TRUE)

for(arq in arqs) {
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
        by = .(anomes = format(data_hora, "%Y-%m"))]
    dat[, usina := usi]
    
    saveRDS(dat, file.path("data/mhg", paste0(usi, ".rds")))
}
