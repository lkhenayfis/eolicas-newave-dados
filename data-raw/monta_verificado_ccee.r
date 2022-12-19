library(data.table)
library(readxl)

MESES_EXT <- structure(formatC(seq_len(12), width = 2, flag = "0"),
    names = c("JANEIRO", "FEVEREIRO", "MARÇO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO",
    "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO"))

# --------------------------------------------------------------------------------------------------

le_aba <- function(arq, aba = 1, nome = "geracao") {
    dat <- read_xlsx(arq, aba, skip = 6, col_names = FALSE)
    setDT(dat)

    col_corte <- sapply(dat, function(col) all(is.na(col)))
    col_corte <- seq_len(which(col_corte) - 1)
    dat <- dat[, ..col_corte]

    linhas_na <- sapply(seq(nrow(dat)), function(i) all(is.na(dat[i])))
    dat <- dat[!linhas_na]

    nomes1 <- unlist(dat[1, ])
    nomes1 <- ifelse(is.na(nomes1), "", paste0(nomes1, "-"))

    nomes2 <- unlist(dat[2, ])
    nomes2[-c(1:6)] <- paste0(MESES_EXT[match(nomes2[-c(1:6)], names(MESES_EXT))], "-01")
    nomes <- paste0(nomes1, nomes2)

    colnames(dat) <- nomes
    dat <- dat[-seq(2)]

    dat <- melt(dat, id.vars = 1:6)
    dat <- dat[, .(CEG, variable, value)]
    dat[, value := as.numeric(value)]
    dat[, variable := as.Date(as.character(variable))]
    colnames(dat) <- c("ceg", "data", nome)
    setorder(dat, ceg, data)

    return(dat)
}

le_planilha <- function(arq) {
    list(
        le_aba(arq),
        le_aba(arq, 9, "cap_inst")
    )
}

processa_cap_inst <- function(dat) {
    dat <- dat[complete.cases(dat)]
    dat <- dat[!duplicated(dat[, .(ceg, cap_inst)])]
    dat <- split(dat, dat$ceg)

    dat <- lapply(dat, function(d) {
        if(nrow(d) > 1) {
            aux <- copy(d[1:(.N - 1)])
            aux[, cap_inst := -cap_inst]
            datas <- aux$data
            aux[, data := d[-1]$data]
            d <- rbind(d, aux)
        }
        return(d)
    })
    dat <- rbindlist(dat)
    setorder(dat, ceg, data, cap_inst)

    return(dat)
}

# --------------------------------------------------------------------------------------------------

dir <- "C:/Users/lucask/Downloads/EOL"
arqs <- list.files(dir)

ger_capinst <- lapply(file.path(dir, arqs), le_planilha)

ger <- lapply(ger_capinst, "[[", 1)
ger <- rbindlist(ger)
ger[, count := 1]
colnames(ger)[2] <- "data_hora"

cap_inst <- lapply(ger_capinst, "[[", 2)
cap_inst <- lapply(cap_inst, processa_cap_inst)
cap_inst <- rbindlist(cap_inst)
colnames(cap_inst)[-1] <- c("data_inicio_operacao", "capacidade_instalada")

# add datas de entrada e cap_insts ao dado de usinas
usinas <- fread("data/usinas.csv", encoding = "UTF-8")
usinas <- merge(cap_inst, usinas, by = "ceg")
fwrite(usinas, "data/usinas.csv")

# troca ceg por id em ger
ger <- merge(ger, usinas[!duplicated(id), .(id, ceg)], by = "ceg")
ger <- ger[, .(id, data_hora, geracao, count)]
colnames(ger)[1] <- "id_usina"

fwrite(ger, "data/verificados.csv")
