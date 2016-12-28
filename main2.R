#Código principal que carga los dataframes que se precisan

# Paquetes necesarios
pkg <- c("countrycode", "dplyr", "rworldmap" )
# Instalar paquetes que no esten instalados
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg))
{
  install.packages(new.pkg)
}


#install.packages("countrycode")
library(countrycode)

#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(MASS)

#install.packages("rworldmap")
library("rworldmap")

source("R/parseAttacks.R")
source("R/parseHolidays.R")
source("R/PIBs.R")
source("R/parseURLs.R")
source("R/modifyFrame.R")

# Recuprem tots els atacs que s'han fet i els guardem en un dataframe
attacks <- parseURL()

#Modifico el ID de pais de UK por GB para United Kingdom, así lo tratan los dataframes de holidays y PIBs
attacks$Country[attacks$Country=="UK"] <- "GB"

# Recuperar els festius de tots els països que han rebut un atac. Només es recuperen des de l'any més petit fins a l'any més gran.
holidays <- getHolidays(attacks$Date, attacks$Country)

# Incorproem una columna al dataframe attacks per indicar si la data de l'atac era festiva o no al país atacat.
attacks <- setHolidays(attacks, holidays)

# Dataframe amb un breu resum del num d'atacs per país, dies festius, atacs en festius...
resumAttacks <- summarizeHolidaysAttacks(attacks, holidays)

# Creació d'una gràfica de l'estil quesito per respresentar el total d'atacs en dies festius i no festius.
# Mirem quants atacs en dies festius hi ha i fem es percentatge respecte el total d'atacs
percentFestiu <- (length(attacks[attacks$Holiday == TRUE,]$Date)/length(attacks$Date))*100
percentNoFestiu <- 100 - percentFestiu # Els dies no festius són el complementaris dels festius
percentFestiu <- format(round(percentFestiu, 2), nsmall = 2) # Només volem 2 decimals
# Construcció de la gràfica (títols dels eixos)
percentNoFestiu <- format(round(percentNoFestiu, 2), nsmall = 2)
titleNoFestiu <- paste("No festius:",percentNoFestiu,"%",sep = " ")
titleFestiu <- paste("Festius:",percentFestiu,"%",sep = " ")
pie(table(attacks$Holiday), labels = c(titleNoFestiu,titleFestiu)) # Quesito style
title(main = "Dies dels atacs")


#Obtener la relación de PIB por País
df.PIBs <- PIBs()

#Pintar PIB por regiones en BoxPlot --> Descartado
#PintaRegion(df.PIBs)

#Pintar PIB por tipos de economía en BoxPlot --> Descartado
#PintaIncomeGroup(df.PIBs)

#Pintar PIB por tipos de economía más ordenado en Barras
#Aquí hay un aggregate
PIBxIncomeGroup(df.PIBs)

#Juntar en un solo dataframe los ataques por países y su PIB
df.AtacsxPais <- Atacs_x_Pais(attacks,df.PIBs)
#Eliminar los países que no tienen Region ni IncomeGroup
#(son muestras pequeñas sin datos de PIB del WorldBank)
df.AtacsxPais <- subset(df.AtacsxPais, !is.na(Region) & !is.na(IncomeGroup))

#Pinta gráfico de ataques por países (problema: Usa  demasiados ataques vs los otros)
Region <- df.AtacsxPais$Region
qplot(df.AtacsxPais$Freq,df.AtacsxPais$PIB,main="Ataques por Pais",xlab="Num.Ataques",ylab="PIB",colour=Region)

#Quitar USA y UK para tener una visualización del resto de países (es más representativo)
df2.AtacsxPais <- subset(df.AtacsxPais, Country != "US" & Country != "GB")
Region <- df2.AtacsxPais$Region
qplot(df2.AtacsxPais$Freq,df2.AtacsxPais$PIB,colour=Region,main="Ataques por Pais, sin US ni GB",xlab="Num.Ataques",ylab="PIB")

#ídem pero por tipo de economía
Tipo_Economia <- df2.AtacsxPais$IncomeGroup
qplot(df2.AtacsxPais$Freq,df2.AtacsxPais$PIB,colour=Tipo_Economia,main="Ataques por Pais, sin US ni GB",xlab="Num.Ataques",ylab="PIB")+xlim(0,50)

#--[Ahora pintamos valores logarítmicos]---------------
#Por región
Region <- df.AtacsxPais$Region
qplot(log10(df.AtacsxPais$Freq),log10(df.AtacsxPais$PIB),main="Ataques por Pais y Region (log10)",xlab="Ataques",ylab="PIB",colour=Region)
#Por tipo de economía
Tipo_Economia <- df.AtacsxPais$IncomeGroup
qplot(log10(df.AtacsxPais$Freq),log10(df.AtacsxPais$PIB),colour=Tipo_Economia,main="Ataques por Pais y Tipo Economia (log10)",xlab="Ataques",ylab="PIB")


