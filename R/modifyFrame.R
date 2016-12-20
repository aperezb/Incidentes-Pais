source("./R/parseHolidays.R")

getHolidays <- function(totalTable) {
  countries <- unique(totalTable$Country)
  years <- unique(format(as.Date(totalTable$Date),'%Y'))
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
  for (c in countries) {
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
  if (length(names(holidays[isHoliday])) <= 2) {
    countries <- names(holidays[isHoliday])
  } else {
    countries <- c("> 2")
  }
  paste(countries,collapse = " ")
}