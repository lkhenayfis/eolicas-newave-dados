library(data.table)
library(ncdf4)

# NE -----------------------------------------------------------------------------------------------

arqNE <- "C:/Users/lucas/Downloads/monthly_mean_wind_speed_ne (1).nc"
datNE <- nc_open(arqNE)

lonNE <- ncvar_get(datNE, "lon")
latNE <- ncvar_get(datNE, "lat")
timeNE <- ncvar_get(datNE, "time")
timeNE <- as.POSIXct("1900-01-01 00:00:00", "GMT") + timeNE * 3600
windNE <- ncvar_get(datNE, "wind_speed")

id_verticeNE <- seq(length(lonNE) * length(latNE))
longitudeNE  <- rep(lonNE, length(latNE))
latitudeNE   <- rep(latNE, each = length(lonNE))
vertNE <- data.table(
    id = id_verticeNE,
    longitude = longitudeNE,
    latitude = latitudeNE
)

data_horaNE <- rep(timeNE, each = length(lonNE) * length(latNE))
verticeNE   <- rep(seq(length(lonNE) * length(latNE)), length(timeNE))
ventoNE     <- c(windNE)
reanNE <- data.table(
    data_hora = data_horaNE,
    id_vertice = verticeNE,
    vento = ventoNE
)
setorder(reanNE, id_vertice, data_hora)

# S ------------------------------------------------------------------------------------------------

arqS <- "C:/Users/lucas/Downloads/monthly_mean_wind_speed_sul.nc"
datS <- nc_open(arqS)

lonS <- ncvar_get(datS, "lon")
latS <- ncvar_get(datS, "lat")
timeS <- ncvar_get(datS, "time")
timeS <- as.POSIXct("1900-01-01 00:00:00", "GMT") + timeS * 3600
windS <- ncvar_get(datS, "wind_speed")

id_verticeS <- seq(length(lonS) * length(latS))
longitudeS  <- rep(lonS, length(latS))
latitudeS   <- rep(latS, each = length(lonS))
vertS <- data.table(
    id = id_verticeS + max(id_verticeNE),
    longitude = longitudeS,
    latitude = latitudeS
)

data_horaS <- rep(timeS, each = length(lonS) * length(latS))
verticeS   <- rep(seq(length(lonS) * length(latS)), length(timeS))
ventoS     <- c(windS)
reanS <- data.table(
    data_hora = data_horaS,
    id_vertice = verticeS + max(verticeNE),
    vento = ventoS
)
setorder(reanS, id_vertice, data_hora)

# --------------------------------------------------------------------------------------------------

rean <- rbind(reanNE, reanS)
vert <- rbind(vertNE, vertS)

fwrite(rean, "data/reanalise_grade.csv")
fwrite(vert, "data/vertices.csv")
