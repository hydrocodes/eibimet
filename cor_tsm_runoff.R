### Extraccion de la TSM del HadISST (Rayner et al, 2003), corte para un periodo de tiempo y correlacion con una serie de escorrentia
### https://github.com/hydrocodes
# Instalar librerias
library(sp)
library(raster) 
library(ncdf4)
library(rgdal)
library(RColorBrewer)
library(lattice)
library(latticeExtra)
library(reshape2)
library(maps)
# Configurar la ruta del directorio de trabajo
setwd("D:/2_Courses/R_Hidrologia/Tutorial_files/climatologie")
# Leer el shapefile de limites costeros ubicado en la carpeta "data"
costa <- readOGR(dsn = 'data', layer= 'ne_110m_coastline' )
# Leer el TSM global y corte para un periodo de tiempo
nc0 <- brick('HadISST_sst.nc')
startYear <- 1970   # ingresar el año inicial
endYear <- 2011     # ingresar el año final
Date <- substr(names(nc0),2,11) 
Date <- gsub('\\.', '\\-', Date)
Date <- as.Date(Date)
dstart1 <- paste(startYear,'01','16',sep='-')
dstart <- grep(dstart1, Date)
dend1 <- paste(endYear,'12','16',sep='-')
dend <- grep(dend1, Date)
nc1 <- subset(nc0, dstart:dend)
# Guardando el archivo nc cortado
writeRaster(nc1,"output.nc", overwrite = TRUE, format="CDF",
            varname="sst", varunit="degC", 
            longname="SST -- raster stack to netCDF, monthly average", 
            xname="Longitude",   yname="Latitude", zname='Time', zunit='months')
# Leer el archivo nc cortado
nc <- nc_open('output.nc', write = TRUE)
lat <- ncvar_get(nc,"Latitude") 
lon <- ncvar_get(nc,"Longitude") 
sst <- ncvar_get(nc,"sst")
# Llenado de valores faltantes
fillvalue <- ncatt_get(nc,"sst","_FillValue")
sst[sst==fillvalue$value] <- NA
missvalue <- ncatt_get(nc,"sst","missing_value")
sst[sst==missvalue$value] <- NA
sst[sst==-1000] <- NA
# Configurando fechas
date_nc <- seq(as.Date(dstart1), as.Date(dend1), by = "months")
year <- format(as.Date(date_nc, format="%Y-%m-%d"),"%Y")
# Calculando el promedio mensual TSM
gmean <- colMeans(sst, na.rm = TRUE, dims=2)
annmean <- aggregate(gmean,by=list(year),FUN=mean,na.rm=TRUE)
avsst <- rowMeans(sst,na.rm=FALSE,dims=2)
grid <- expand.grid(x=lon, y=lat)
grid$avsst <- as.vector(avsst)
# Calculando y ploteando el promedio anual TSM
yrs <- annmean$Group.1 
nyr <- length(yrs)
asst <- array(NA,c(dim(lon),dim(lat),nyr)) 
for (k in 1:nyr) {
  asst[,,k] <- rowMeans(sst[,,year==yrs[k]],na.rm=FALSE,dims=2)
}
grid$an_avsst <- as.vector(rowMeans(asst,na.rm=FALSE,dims=2))
colors <- rev(brewer.pal(10, "RdYlBu"))
pal <- colorRampPalette(colors)
levelplot(an_avsst~x*y, data=grid,col.regions = pal(100),xlab='Longitud',ylab='Latitud',main='TSM promedio anual') + layer(sp.lines(costa))

# Removiendo las medias globales
gmean <- colMeans(asst, na.rm = TRUE, dims=2)
for (k in 1:nyr){
  asst[,,k] <- asst[,,k]-matrix(gmean[k],length(lon),length(lat))
}
# Extrayendo los datos de TSM para una coordenada indicada
lon0 <- -76.5 
lat0 <- -18.5
sst_ts<-asst[which(lon==lon0),which(lat==lat0),]
# Ploteando la serie de tiempo 
plot(yrs,sst_ts,type='l',xlab='Años',ylab='Anomalia TSM',main=paste0('Anomalia TSM Long=', lon0, ',Lat=', lat0))

# lectura de escorrentia y correlacion con la TSM
runoff <- read.table("runoff.txt")
cmatrix <- matrix(NA,dim(lon),dim(lat))
for (i in 1:dim(lon)) {
  for (j in 1:dim(lat)) {
    cmatrix[i,j] <- cor(asst[i,j,], runoff)
  }
}
grid$corr <- as.vector(cmatrix)
# indicar el rango de longitud y de latitud de visualizacion
levelplot(corr~x*y, data=grid , xlim=c(-180,0),ylim=c(-30,20),
          col.regions = pal(100),xlab='Longitud',ylab='Latitud',main='Correlacion TSM vs escorrentia anual') + 
  layer(sp.lines(costa)) 
