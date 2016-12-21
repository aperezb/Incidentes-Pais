#Código principal que carga los dataframes que se precisan

install.packages("countrycode")
library(countrycode)

install.packages("dplyr")
library(dplyr)


source("R/parseAttacks.R")
source("R/PIBs.R")
source("R/parseURLs.R")
source("R/modifyFrame.R")

df <- PIBs()
