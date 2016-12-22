source("./R/parseHolidays.R")

addHolidaysToFrame <- function(totalTable) {
  holidays <- getHolidays(totalTable)
  setHolidays(totalTable,holidays)
}

getHolidays <- function(dates,countries) {
  countries <- unique(countries)
  years <- unique(format(as.Date(dates),'%Y'))
  getCountriesHolidays(countries[!is.na(countries)], years[!is.na(years)])
}

setHolidays <- function(totalTable,holidays) {
  totalTable <- attackInCountryHoliday(totalTable, holidays)
  totalTable$TotalCountriesHoliday <- sapply(totalTable$Date,totalCountriesHoliday, holidays = holidays )
  totalTable$HolidayIn <- sapply(totalTable$Date,holidayIn, holidays = holidays )
  totalTable
}

attackInCountryHoliday <- function(totalTable,holidays) {
  totalTable$Holiday <- NA
  for (c in unique(totalTable$Country)) {
    attackDates <- totalTable[totalTable$Country == c,]$Date
    totalTable[totalTable$Country == c,]$Holiday <- isCountryHoliday(attackDates,c,holidays)
  }
  totalTable
}

totalCountriesHoliday <- function(dates,holidays) {
  isHoliday <- sapply(holidays, FUN = function(x) any(as.Date(dates) %in% x))
  holidayCountries <- names(holidays[isHoliday])
  length(holidayCountries)
}

holidayIn <- function(dates,holidays) {
  isHoliday <- sapply(holidays, FUN = function(x) any(as.Date(dates) %in% x))
  # if (length(names(holidays[isHoliday])) <= 2) {
    countries <- names(holidays[isHoliday])
  # } else {
  #   countries <- c("> 2")
  # }
  paste(countries,collapse = " ")
}

summarizeHolidaysAttacks <- function(totalTable, holidays) {
  rel <- data.frame(country = character(), totalAttacks = integer(), holidayAttacks = double(), hoildays = integer(), "hoilday/year" = double(), "holidayAttacks/attacks" = double(), stringsAsFactors = FALSE)
  for (co in unique(totalTable$Country)) {
    hy <- length(holidays[[co]])
    attacks <- length(totalTable[totalTable$Country == co,]$Date)
    holidayAttacks <- length(totalTable[totalTable$Country == co & totalTable$Holiday == TRUE,]$Date)
    country_rel <- data.frame(country = co, totalAttacks = attacks, holidayAttacks = holidayAttacks, hoildays = hy, "hoilday/year" = hy/365, "holidayAttacks/attacks" = holidayAttacks/attacks, stringsAsFactors = FALSE)
    rel <- dplyr::bind_rows(rel, country_rel)
  }
  rel
}