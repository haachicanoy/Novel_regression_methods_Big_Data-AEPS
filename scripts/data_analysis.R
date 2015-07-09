# Novel regression methods for Big Data-AEPS
# H. Achicanoy
# CIAT, 2015

# Download toy sets from Big Data-AEPS repository
# download.file(url='ftp://ftp.ciat.cgiar.org/DAPA/projects/BIGDATA_AEPS/TOY_SETS/estacionClimatica.csv',
#               destfile='C:/Users/haachicanoy/Documents/GitHub/Novel_regression_methods_Big_Data-AEPS/data/estacionClimatica.csv', method='internal')
# download.file(url='ftp://ftp.ciat.cgiar.org/DAPA/projects/BIGDATA_AEPS/TOY_SETS/eventosDeCosecha.csv',
#               destfile='C:/Users/haachicanoy/Documents/GitHub/Novel_regression_methods_Big_Data-AEPS/data/eventosDeCosecha.csv',method='internal')

g <- gc()
rm(list = ls())
setwd('C:/Users/haachicanoy/Documents/GitHub/Novel_regression_methods_Big_Data-AEPS/data')

management <- read.csv(file='eventosDeCosecha.csv',row.names=1)
climate <- read.csv(file='estacionClimatica.csv')

# Vincular clima a los datos de manejo
vinculClima <- function(baseCosecha,climBase,fsiem="Sowing_Date",fcos="Harvest_Date")
{
  nEven <- nrow(baseCosecha)
  
  ##Por evento extrae el vector de fechas entre siembra y cosecha
  fechaEvent  <- lapply(1:nEven,function(x){fecha <- as.Date(baseCosecha[fsiem][,1][x]:baseCosecha[fcos][,1][x],format='%m/%d/%Y',origin = "01/01/1970");return(as.data.frame(fecha))})
  climBase$DATE <- as.Date(climBase$DATE, format='%m/%d/%Y', origin="01/01/1970")
  
  ##Por vector de fechas de cada evento asigna variables de clima
  eventosCosech <- lapply(1:nEven,function(x){merge(fechaEvent[[x]],climBase,by.x="fecha",by.y="DATE",all.x=T,all.y=F)})
  
  #Calculando algunos indicadores para todos los eventos
  Indicadores <- lapply(1:nEven, function(x){Indicadores<-with(eventosCosech[[x]],c( mean(TMAX),mean(TMIN),mean((TMAX-TMIN)/2),mean(TMAX-TMIN),sum(ESOL),sum(TMAX>34),sum(RAIN),sum(RAIN>=10)) );return(as.data.frame(Indicadores))})
  
  nomrows=c("Temp_Max_Avg","Temp_Min_Avg","Temp_Avg",  "Diurnal_Range_Avg",	"Sol_Ener_Accu"	,"Temp_Max_34_Freq","Rain_Accu","Rain_10_Freq")
  
  sa=as.data.frame(Indicadores)
  row.names(sa)=nomrows
  climEvent=as.data.frame(t(sa))
  
  # Union de indicadores clima y eventos
  joint=as.data.frame(cbind(baseCosecha,climEvent))
  return(joint)
}

datos <- vinculClima(baseCosecha=management,climBase=climate,fsiem="Sowing_Date",fcos="Harvest_Date")
datos$Sowing_Date <- as.Date(datos$Sowing_Date, format='%m/%d/%Y', origin="01/01/1970")
datos$Harvest_Date <- as.Date(datos$Harvest_Date, format='%m/%d/%Y', origin="01/01/1970")
rm(vinculClima, management, climate)

datos <- datos[,c('File_Code','Sowing_Date','Sowing_Month','Harvest_Date','Harvest_Month','Harvest_Year',
                  'Department','Municipality','Area','Cropping_System_Simple','Variety',
                  'Temp_Max_Avg','Temp_Min_Avg','Temp_Avg','Diurnal_Range_Avg','Sol_Ener_Accu','Temp_Max_34_Freq','Rain_Accu','Rain_10_Freq',
                  'Yield')]

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# PLS-Path modeling
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(plspm)

# Define groups of variables
# Management group
management <- c(3,5,6,8,9,10,11)

# Climate group
climate <- 12:(ncol(datos)-1)

# Yield
yield <- ncol(datos)

pls_yield <- plspm(Data=datos, path_matrix=, blocks=)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# PLS-Regression 1
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #



