library(XML)
library(httr)
library(countrycode)

isCountryHoliday <- function(dates,country,holidays=FALSE) {
  hd <- ifelse(holidays && !is.null(holidays[[country]]),
               holidays[[country]],
               getCountryHolidays(country, unique(format(as.Date(dates),'%Y'))))[[1]]
  as.Date(dates) %in% hd
}

getCountriesHolidays <- function(countries, years) {
  holidays <- c()
  countriesLong <- tolower(countrycode(toupper(countries), "iso2c", "country.name"))
  for (c in countries) {
    holidays <- c(holidays, getCountryHolidays(c, years))
  }
  holidays
}

getCountryHolidays <- function(country, years) {
  holidays <- data.frame(stringsAsFactors = FALSE)
  
  countryISO2 <- countrycode(toupper(country), "iso2c", "country.name")
  countryISO2 <- sub(pattern = '\\s', replacement = "_", x = countryISO2)
  for (y in years) {
    holidays <- dplyr::bind_rows(holidays, parseYear(y, countryISO2))
  }
  
  names(holidays) <- toupper(country)
  holidays
}

dateToISO <- function(table,year=2015) {
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  as.Date(sapply(lapply(table, FUN = function(elem) {
    if (grepl(pattern = "\\w{3}\\s\\d{1,2}", x = elem)) {
      replace <- paste("\\2\\1", year, sep = "")
      strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = replace, x = elem)
      as.character(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
    } else {
      as.character(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = elem))
    }
  }), function(x) as.character(x)))
}

parseYear <- function(year, country) {
  # http://www.officeholidays.com/countries/{country}/{year}.php
  fileURL <- paste('http://www.officeholidays.com/countries/', country, '/', year,'.php',
                   sep = "")
  page <- htmlTreeParse(fileURL, useInternalNodes = T)

  generalHoliday <- '//tr[@class="holiday" or @class="regional"]/td/span[@class="mobile_ad"]'
  x <- xpathSApply(page, generalHoliday, xmlValue)
  
  d <- data.frame(dateToISO(x, year = year))
  names(d) <- country
  d
}
