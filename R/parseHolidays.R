library(XML)
library(httr)
library(countrycode)

isCountryHoliday <- function(dates,country,holidays) {
  # print(holidays)
  # print(holidays[[country]])
  # hd <- ifelse(length(holidays)!=0 && !is.null(holidays[[country]]),
  #              holidays[[country]],
  #              getCountryHolidays(country, unique(format(as.Date(dates),'%Y'))))[[1]]
  as.Date(dates) %in% holidays[[country]]
}

getCountriesHolidays <- function(countries, years) {
  holidays <- c()
  countriesLong <- tolower(countrycode(toupper(countries), "iso2c", "country.name"))
  for (c in countries) {
    if (!is.na(c)) {
      holidays <- c(holidays, getCountryHolidays(c, years))
    }
  }
  holidays
}

getCountryHolidays <- function(country, years) {
  countryISO2 <- countrycode(toupper(country), "iso2c", "country.name")
  print(paste("Parsing",countryISO2,"holidays...",sep = " "))
  countryISO2 <- gsub(pattern = '\\s|\\n', replacement = "_", x = countryISO2)
  countryISO2 <- ifelse(toupper(country)=="US","usa",countryISO2)
  if (is.na(countryISO2)) {
    holidays <- data.frame(c(NA), stringsAsFactors = FALSE)
  } else {
    holidays <- data.frame(stringsAsFactors = FALSE)
    for (y in years) {
      holidays <- dplyr::bind_rows(holidays, parseYear(y, countryISO2))
    }
  }
  
  names(holidays) <- toupper(country)
  holidays
}

dateToISO <- function(table,year=2015) {
  if (length(table) == 0) { 
    return(c(NA))
  }
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  as.Date(sapply(lapply(table, FUN = function(elem) {
    if (grepl(pattern = "\\w{3}\\s\\d{1,2}", x = elem)) {
      replace <- paste("\\2\\1", year, sep = "")
      strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = replace, x = elem)
      as.character(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
    } else if(grepl(pattern = "(\\d{2})/(\\d{2})/(\\d{4})", x = elem)) {
      as.character(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = elem))
    } else {
      c(NA)
    }
  }), function(x) as.character(x)))
}

parseYear <- function(year, country) {
  if (is.na(country)) {
    return(c(NA))
  }
  # http://www.officeholidays.com/countries/{country}/{year}.php
  fileURL <- paste('http://www.officeholidays.com/countries/', country, '/', year,'.php',
                   sep = "")
  
  r <- HEAD(fileURL)
  if ( status_code(r) == "200" ) {
    page <- htmlTreeParse(fileURL, useInternalNodes = T)
  
    generalHoliday <- '//tr[@class="holiday" or @class="regional"]/td/span[@class="mobile_ad"]'
    x <- xpathSApply(page, generalHoliday, xmlValue)
    
    d <- data.frame(dateToISO(x, year = year), stringsAsFactors = FALSE)
    names(d) <- country
    d
  } else {
    d <- data.frame(c(NA), stringsAsFactors = FALSE)
    names(d) <- country
    d
  }
}
