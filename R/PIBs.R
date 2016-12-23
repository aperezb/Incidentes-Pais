#ruta="C:/Users/Angel/Dropbox/Master Ciberseg/03-Data Driven/RStudio/Practica"
PIBs <- function(ruta="./Dades",
                 fichPIBs="./Dades/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv",
                 fichTipos="./Dades/Metadata_Country_API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv"){
  # Propósito de la función: Obtener un Dataframe con PIBs de 2013 a 2016 de todos los países
  # Entrada:
  #   ruta--> Path donde se encuentran los archivos .CSV que precisa
  #   fichPIBs --> fichero .CSV que contiene la relación de PIBs por país
  #   fichTipos --> fichero .CSV que contiene datos de clasificación adicionales de países (Región y TipoIngresos del País)
  # Salida, data.frame con:
  #   Country.Name --> nombre largo del país (p.e: Spain)
  #   iso3c --> Código 3 dígitos identificativo del pais (p.ej: ESP)
  #   PIB --> El PIB que se utilizará para las combinaciones
  #   Country --> Identificación País (ISO2)
  #   Region --> Zona geográfica del País
  #   IncomeGroup --> Tipo de economía del País
  # Los ficheros .CSV se obtienen de: http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD

  # ASPECTOS PENDIENTES DE RESOLVER
  # OK: Sustituir los NAs del data.frame origen por 0 para que no casque cuando lo crucen
  # OK: Añadir al DataFrame la región del país y el tipo de economía que tiene
  # Quitar el install.packages y el library de dentro de la función
  # Cargar el CSV directamente del ZIP que está en la URL: http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.PP.CD?downloadformat=csv
  # OK: Montarlo en GIT
  # Eliminar el warning porque PIBPais tiene más observaciones que TipoPais (cosas del Wordlbank)
  # Control de errores por si no se puede cargar el dataframe

  # Obtener relación de PIBs por país
  #setwd(ruta)
  PIBPaisFull <- read.csv(file = fichPIBs, skip = 4, header = TRUE, na.strings = 0)
  PIBPais <- dplyr::select(PIBPaisFull,dplyr::one_of(c('Country.Name','Country.Code', 'X2015')))
  names(PIBPais)[2]<-"iso3c"
  names(PIBPais)[3]<-"PIB"

  #En el CSV del PIB tengo el Country Code en formato ISO3C, obtener el código en ISO2C (ISO3166)
  #En el dataframe de incidentes tengo los country codes en ISO2C
  #install.packages("countrycode")
  #library(countrycode)

  iso3c <- PIBPais$iso3c
  iso2c <- countrycode(iso3c,"iso3c","iso2c")
  PIBPais <- dplyr::mutate(PIBPais,countrycode(iso3c,"iso3c","iso2c"))
  names(PIBPais)[4]<-"Country"

  #Obtener del segundo CSV las características de país (tipo economía="IncomeGroup" y región="Region")
  TipoP <- read.csv(file=fichTipos,header=TRUE)
  names(TipoP)[1] <- "iso3c"
  
  TipoPais <- dplyr::select(TipoP,dplyr::one_of(c('iso3c','Region', 'IncomeGroup')))
  

  #Reemplazar todos los NAs por 0
  PIBPais[is.na(PIBPais)] <- 0

  #Añadir el tipo de economía y la localización del país
  a <- dplyr::inner_join(PIBPais,TipoPais,by="iso3c",copy=TRUE)

  #Eliminar del dataframe aquellos países que no tienen Region (de los datos origen hay países ficticios que son agregaciones)
  b <- LimpiaPIBs(a)

  b
  
}

LimpiaPIBs <- function(df){
  #Propósito de la función: Eliminar del dataframe aquellos países que no tienen informado Region o IncomeGroup
  #Entrada: dataframe de PIBs por países con las columnas "Region" e "IncomeGroup"
  #Salida: dataframe sin las observaciones que tengan blanco en Region o en IncomeGroup

  lvls <- levels(df$Region)
  lvls <- lvls[lvls != ""]
  #dfFiltered <- filter(df,df$Region %in% lvls)
  dfFiltered <- subset(df,Region %in% lvls)
}

PintaRegion <- function(df,coloret="red"){
  #Función que muestra en BoxPlot el PIB por región
#  lvls <- levels(df$Region)
#  lvls <- lvls[lvls != ""]
#  dfFiltered <- filter(df,df$Region %in% lvls)
  boxplot(PIB ~ droplevels(Region), data = df, col = coloret)
}

PintaIncomeGroup <- function(df,coloret="red"){
  #Función que muestra en BoxPlot el PIB por región
  boxplot(PIB ~ droplevels(IncomeGroup), data = df, col = coloret)
}

PIBxIncomeGroup <- function(df){
  #Pinta el PIB por Grupo de Economía
  ll <- aggregate(PIB~IncomeGroup,df,mean)
  a <- ll[order(ll$PIB),]
  barplot(a$PIB,main="PIB por Tipo de Economia",names.arg=a$IncomeGroup,col = "red")
}


Atacs_x_Pais <- function(totalTable,PIBs){
  #Agregar por num ataques País
  a <- data.frame(table(totalTable$Country))

  names(a)[1] <- "Country"

  #Afegir PIBs i agregacions pais al dataframe d'atacs
  df <- dplyr::left_join(a,PIBs,by="Country",copy=TRUE)
}



aa<-PIBs()



