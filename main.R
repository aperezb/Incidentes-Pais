#Código principal que carga los dataframes que se precisan

install.packages("countrycode")
library(countrycode)

install.packages("dplyr")
library(dplyr)
library(ggplot2)

source("R/parseAttacks.R")
source("R/PIBs.R")
source("R/parseURLs.R")
source("R/modifyFrame.R")


df <- PIBs()

#Pintar PIB por regiones en BoxPlot
PintaRegion(df)

#Pintar PIB por tipos de economía en BoxPlot
PintaIncomeGroup(df)

#Pintar PIB por tipos de economía más ordenado en Barras
#Aquí hay un aggregate
PIBxIncomeGroup(df)


#Juntar en un solo dataframe los ataques por países y su PIB
df.AtacsxPais <- Atacs_x_Pais(totalTable,df)

#Pinta gráfico de ataques por países (problema: Usa  demasiados ataques vs los otros)
Region <- df.AtacsxPais$Region
qplot(df.AtacsxPais$Freq,df.AtacsxPais$PIB,main="Ataques por Pais",xlab="Num.Ataques",ylab="PIB",colour=Region)

#Quitar USA para tener una visualización de ése subconjunto
df2 <- subset(df.AtacsxPais, Country != "US")
Region <- df2$Region
qplot(df2$Freq,df2$PIB,colour=Region,main="Ataques por Pais, sin USA",xlab="Num.Ataques",ylab="PIB")+xlim(0,50)

#ídem pero por tipo de economía
df2 <- subset(df.AtacsxPais, Country != "US")
Tipo_Economia <- df2$IncomeGroup
qplot(df2$Freq,df2$PIB,colour=Tipo_Economia,main="Ataques por Pais, sin USA",xlab="Num.Ataques",ylab="PIB")+xlim(0,50)

