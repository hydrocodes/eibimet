### Extracción de la TSM del HadISST, corte para un periodo de tiempo y correlación con una serie de escorrentía (2026)
### https://github.com/hydrocodes
# Instalar paquetes
library(terra)
library(RColorBrewer)
library(lattice)

# Ingreso de datos
setwd(".../climatologie") #carpeta de trabajo
sst_file <- "HadISST_sst.nc"
runoff_file <- "runoff.txt"
coast_file <- "new3_ne_110m_coastline.shp"
startYear <- 1970
endYear   <- 2011
# Coordenada para la extraccion de datos
lon0 <- -76.5
lat0 <- -18.5

### Procesamiento ###
costa <- vect(coast_file)
sst <- rast(sst_file)
sst[sst == -1000] <- NA
dates <- time(sst)

if (is.null(dates)) {
  dates <- as.Date(gsub("\\.", "-", substr(names(sst), 2, 11)))
}
years <- as.numeric(format(dates, "%Y"))
idx <- which(years >= startYear & years <= endYear)
sst <- sst[[idx]]
dates <- dates[idx]
years <- years[idx]
yrs <- sort(unique(years))

# TSM anual
annual_sst <- tapp(sst, index = years, fun = mean, na.rm = TRUE)
names(annual_sst) <- yrs

# Mapa TSM anual promedio espacial
mean_sst <- mean(annual_sst, na.rm = TRUE)
mean_sst <- terra::rotate(mean_sst) #plot con el O.Pacifico centrado
colors <- rev(brewer.pal(10,"RdYlBu"))
pal <- colorRampPalette(colors)
plot(mean_sst, col = pal(100), main = "Mean Annual SST")
lines(costa)

# Anomalia con el promedio global
global_mean <- global(annual_sst, "mean", na.rm = TRUE)[,1]
annual_anom <- annual_sst
for(i in seq_len(nlyr(annual_anom))){
  annual_anom[[i]] <- annual_anom[[i]] - global_mean[i]
}

# Serie de tiempo de anomalia TSM
sst_ts <- extract(annual_anom, cbind(lon0, lat0))
sst_ts <- as.numeric(sst_ts[1,])
plot(yrs, sst_ts, type = "l", lwd = 2, xlab = "Year", ylab = "SST Anomaly (°C)",
  main = paste0("SST Anomaly\nLon=", lon0, " Lat=", lat0))

# Leer la serie de escorrentía
runoff <- scan(runoff_file, quiet = TRUE)
if(length(runoff) != nlyr(annual_anom)){
  stop(paste("Runoff series length =", length(runoff), "but SST years =", nlyr(annual_anom)))
}

# Correlacion TSM y escorrentia
sst_mat <- values(annual_anom, mat = TRUE)
corr_fun <- function(x){
  if(all(is.na(x))) return(NA)
  if(sd(x, na.rm = TRUE) == 0) return(NA)
  cor(x, runoff, use = "pairwise.complete.obs")
}
corr_values <- apply(sst_mat, 1, corr_fun)
corr_rast <- rast(annual_anom, nlyrs = 1)
values(corr_rast) <- corr_values
corr_rast <- terra::rotate(corr_rast)

# Mapa de correlacion espacial
plot(corr_rast, col = pal(100), range = c(-1,1), main = paste0("SST vs Runoff Correlation\n",
    startYear, "-", endYear), xlim=c(130,360), ylim=c(-40,30))
lines(costa)
