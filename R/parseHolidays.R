library("XML")
library("httr")

# http://www.officeholidays.com/countries/{country}/{year}.php

fileURL <- 'http://www.officeholidays.com/countries/france/2016.php'
generalHoliday <- '//tr[@class="holiday" or @class="regional"]/td/span[@class="mobile_ad"]'

page <- htmlTreeParse(fileURL, useInternalNodes = T)
x <- xpathSApply(page, generalHoliday, xmlValue)
data.frame(date = x)


parseCountryHolidays <- function(countries, years) {
  holidays <- data.frame()
  for (c in countries) {
    holidays <- dplyr::bind_cols(holidays, parseCountry(c, years))
  }
}
# cbind.fill (plyr)

parseCountry <- function(country, years) {
  holidays <- data.frame(character(), stringsAsFactors = FALSE)
  names(holidays) <- country

  for (y in years) {
    holidays <- dplyr::bind_rows(holidays, parseYear(y, country))
  }
  holidays
}

parseYear <- function(year, country) {
  fileURL <- paste('http://www.officeholidays.com/countries/', country, '/', year,'.php',
                   sep = "")
  page <- htmlTreeParse(fileURL, useInternalNodes = T)

  generalHoliday <- '//tr[@class="holiday" or @class="regional"]/td/span[@class="mobile_ad"]'
  x <- xpathSApply(page, generalHoliday, xmlValue)

  d <- data.frame(x)
  names(d) <- country
  d
}
