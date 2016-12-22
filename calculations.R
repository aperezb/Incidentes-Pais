library(XML)
library(httr)
library(countrycode)

source("./R/parseHolidays.R")
source("./R/PIBs.R")

holidays <- getHolidays(totalTable)
attacks <- summarizeHolidaysAttacks(totalTable, holidays)
countryPIBs <- PIBs()

a <- dplyr::left_join(attacks,dplyr::select(aa,PIB,iso2c),by = c("country" = "iso2c") )
