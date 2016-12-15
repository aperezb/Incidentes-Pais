

PIBs <- function(ruta="C:/Users/Angel/Dropbox/Master Ciberseg/03-Data Driven/RStudio/Practica",
                 fichPIBs="./API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv",
                 fichTipos="./Metadata_Country_API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv"){
  # Propósito de la función: Obtener un Dataframe con PIBs de 2013 a 2016 de todos los países
  # Entrada:
  #   ruta--> Path donde se encuentran los archivos .CSV que precisa
  #   fichPIBs --> fichero .CSV que contiene la relación de PIBs por país
  #   fichTipos --> fichero .CSV que contiene datos de clasificación adicionales de países (Región y TipoIngresos del País)
  # Salida, data.frame con:
  #   Country.Name --> nombre largo del país (p.e: Spain)
  #   iso3c --> Código 3 dígitos identificativo del pais (p.ej: ESP)
  #   PIB2013...PIB2016 --> Cifra de PIB del país para de cada ejercicio
  #   iso2c --> Código 2 dígitos identificativos del país (p.ej: ES)
  # Los ficheros .CSV se obtienen de: http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
  
  # ASPECTOS PENDIENTES DE RESOLVER
  # Sustituir los NAs del data.frame origen por 0 para que no casque cuando lo crucen
  # Añadir al DataFrame la región del país y el tipo de economía que tiene
  # Quitar el install.packages y el library de dentro de la función
  # Cargar el CSV directamente del ZIP que está en la URL: http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.PP.CD?downloadformat=csv
  # Montarlo en GIT
  # Control de errores por si no se puede cargar el dataframe
  
  # Obtener relación de PIB 2015 por país
  setwd(ruta)
  PIBPaisFull <- read.csv(file = fichPIBs, skip = 4, header = TRUE, na.strings = 0)
  PIBPais <- dplyr::select(PIBPaisFull,dplyr::one_of(c('Country.Name','Country.Code', 'X2013', 'X2014', 'X2015')))
  names(PIBPais)[2]<-"iso3c"
  names(PIBPais)[3]<-"PIB2013"
  names(PIBPais)[4]<-"PIB2014"
  
  names(PIBPais)[5]<-"PIB2015"
  #Lo siguiente es para que exista una variable con el PIB2016 (no viene informada)
  PIBPais$PIB2016 <- PIBPais$PIB2015
  
  #En el CSV del PIB tengo el Country Code en formato ISO3C, obtener el código en ISO2C (ISO3166)
  #En el dataframe de incidentes tengo los country codes en ISO2C
  install.packages("countrycode")
  library(countrycode)
  
  iso3c <- PIBPais$iso3c
  iso2c <- countrycode(iso3c,"iso3c","iso2c")
  PIBPais <- dplyr::mutate(PIBPais,countrycode(iso3c,"iso3c","iso2c"))
  names(PIBPais)[7]<-"iso2c"
  
  #Obtener del segundo CSV las características de país (tipo economía="IncomeGroup" y región="Region")
  TipoPais <- read.csv(file=fichTipos,header=TRUE)
  names(TipoPais)[1] <- "iso3c"
  
  #Reemplazar todos los NAs por 0
  PIBPais[is.na(PIBPais)] <- 0
  PIBPais
}
