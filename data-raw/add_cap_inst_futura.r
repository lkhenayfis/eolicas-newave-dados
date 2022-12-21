library(data.table)
library(readxl)

# --------------------------------------------------------------------------------------------------

shape <- readRDS("data/shape.rds")
setDT(shape)
shape[, group := as.numeric(as.character(group))]

estados <- c(
    MA = 7.1, PI = 8.1, CE = 9.1, RN = 10.1, PB = 11.1, PE = 12.1,
    AL = 13.1, SE = 14.1, BA = 15.1, RJ = 18.1, PR = 20.1, SC = 21.1, RS = 22.1
)

pois <- lapply(estados, function(num) {
    dd <- shape[group == num, .SD, .SDcols = c("long", "lat")]
    do.call(cbind, polylabelr::poi(dd$long, dd$lat, precision = .01))
})
pois <- as.data.table(do.call(rbind, pois))
names(pois) <- c("longitude", "latitude", "prec")
pois[, estado := names(estados)]

# --------------------------------------------------------------------------------------------------

usinas <- fread("data/usinas.csv", encoding = "UTF-8")
usinas[, data_inicio_operacao := as.Date(data_inicio_operacao)]
total  <- usinas[, sum(capacidade_instalada), by = subsistema]

# --------------------------------------------------------------------------------------------------

final_do_mes <- function(data) {
    out <- sapply(seq_along(data), function(i) {
        out <- seq.Date(data[i], length.out = 2, by = "month")
        out <- out[2] - 1
        out
    })
    out
}

# --------------------------------------------------------------------------------------------------

arq <- "C:/Users/lucask/Downloads/Evo_cap_inst_EOL_PMO_DEZ2022.xlsx"

plans_existentes <- excel_sheets(arq)
plans_existentes <- plans_existentes[plans_existentes %in% c("NE", "N", "SE", "S")]

estado_aux <- c("NE" = "BA", "S" = "RS")

cap_evol <- lapply(plans_existentes, function(subsist) {
    plan <- read_xlsx(arq, sheet = subsist)
    plan <- as.data.table(plan)
    colnames(plan) <- c("mes", "ano", "capacidade_instalada")
    plan[, capacidade_instalada := diff(c(total[subsistema == subsist]$V1, capacidade_instalada))]
    plan <- plan[capacidade_instalada != 0]
    plan[, mes := tolower(mes)]

    plan[, data_inicio_operacao := as.Date(paste0(ano, "-", mes, "-01"), format = "%Y-%b-%d")]
    #plan[, data_inicio_operacao := final_do_mes(data_inicio_operacao)]
    plan[, data_inicio_operacao := as.Date(data_inicio_operacao, origin = "1970-01-01")]

    plan[, c("ano", "mes") := .(NULL, NULL)]

    # add coisas que faltam pra bater com usinas
    plan[, subsistema := subsist]
    plan[, codigo := formatC(seq(.N), width = 6, flag = "0")]
    plan[, codigo := gsub("0", "X", codigo)]
    plan[, nome := paste0("dummy_", formatC(seq(.N), width = 3, flag = "0"))]
    plan[, ceg := paste0("EOL.CV.", subsist, ".999999-9.", formatC(seq(.N), width = 2, flag = "0"))]
    plan[, estado := estado_aux[subsist]]
    plan <- merge(plan, pois[, .(latitude, longitude, estado)])
    plan[, coordenadas_aproximadas := TRUE]
    plan[, estado := NULL]

    return(plan)
})
cap_evol <- rbindlist(cap_evol)

cap_evol[, id := seq(.N) + max(usinas$id)]
setcolorder(cap_evol, colnames(usinas))

usinas <- rbind(usinas, cap_evol)
fwrite(usinas, "data/usinas.csv")