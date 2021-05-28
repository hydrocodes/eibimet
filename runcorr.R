### Correlacion corrida entre un indice climatico y varias estaciones
### https://github.com/hydrocodes
# Instalar libreria hydRopclim, solo una vez
library(devtools)
devtools::install_github ("hydrocodes/hydRopclim")
# Llamar librerias
library(hydRopclim)
library(reshape2)
library(ggplot2)
library(wesanderson)
library(cowplot)
data <- read.csv("D:/2_Courses/R_Hidrologia/Tutorial_files/climatologie/database.csv")
index <- data$Index
variables <- data[ ,3:ncol(data)]
#ingresar el tamaño de la ventana en años para la correlacion corrida
rwin <- 11 
# Generar un indice con el promedio desde diciembre (12) y (3) meses siguientes: Dic-Ene-Feb
p1 <- seasavg(p=index, start=12, win=3)
# Generar un indice por cada estacion con la suma desde septiembre (9) y (8) meses siguientes: Sep-Oct-Nov-Did-Ene-Feb-Mar-Abr
p2 <- seassum(p=variables, start=9, win=8)
# Creacion de archivos
output1 <- "D:/2_Courses/R_Hidrologia/Tutorial_files/climatologie/indexes.csv"
output2 <- "D:/2_Courses/R_Hidrologia/Tutorial_files/climatologie/runcorr.csv"
output3 <- "D:/2_Courses/R_Hidrologia/Tutorial_files/climatologie/runcorr_format.csv"
# Visualizar la correlacion corrida y tendencias
indexcorrl(index.seas=p1, variable.seas=p2, rwin=rwin)
