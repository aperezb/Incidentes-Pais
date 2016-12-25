# Fitxer de funcions per aplicar transformacions sobre el dataframe original en funció de les vacances dels països d'aquest. Utilitza les funcions de parseHoliday
source("./R/parseHolidays.R")

# A partir del dataframe del Carles calcula tots els dies festius de tots els països i incorporar una nova columna en funció de si la data de l'atac era festiva o no.
addHolidaysToFrame <- function(totalTable) {
  holidays <- getHolidays(totalTable) # Recupera els dies festius de tots els països.
  setHolidays(totalTable,holidays) # Incorporar columna de festiu sí/no
}

# Recupera els dies festius del països. Li pasem una columna de països i les dates dels atacs. Poden estar repetits. Extreu les holidays dels països i només dels anys presents en els atacs.
getHolidays <- function(dates,countries) {
  countries <- unique(countries) # Països no repetits.
  years <- unique(format(as.Date(dates),'%Y')) # Els anys dels atacs, no repetis, que interessen 
  getCountriesHolidays(countries[!is.na(countries)], years[!is.na(years)])
}

# Incorpora dues columnes al dataframe indicant a quants països el dia de l'atac era un dia festiu i en cas que siguin 1 o 2 n'especifica el nom en una altra columna. 
setHolidays <- function(totalTable,holidays) {
  totalTable <- attackInCountryHoliday(totalTable, holidays)
  totalTable$TotalCountriesHoliday <- sapply(totalTable$Date,totalCountriesHoliday, holidays = holidays )
  totalTable$HolidayIn <- sapply(totalTable$Date,holidayIn, holidays = holidays )
  totalTable
}

# Incorpora una nova columna al df que indica si el la data de l'atac era un dia festiu al país atacat o no.
attackInCountryHoliday <- function(totalTable,holidays) {
  totalTable$Holiday <- NA
  for (c in unique(totalTable$Country)) {
    attackDates <- totalTable[totalTable$Country == c,]$Date
    totalTable[totalTable$Country == c,]$Holiday <- isCountryHoliday(attackDates,c,holidays)
  }
  totalTable
}

# Calcula a quants països la data en qüestió és festiva.
totalCountriesHoliday <- function(dates,holidays) {
  isHoliday <- sapply(holidays, FUN = function(x) any(as.Date(dates) %in% x))
  holidayCountries <- names(holidays[isHoliday])
  length(holidayCountries)
}

# Mira a quants països era festiu per a la data de l'atac (en cas que la data fos festiva a més de dos països ho indica com > 2)
holidayIn <- function(dates,holidays) {
  isHoliday <- sapply(holidays, FUN = function(x) any(as.Date(dates) %in% x))
  if (length(names(holidays[isHoliday])) <= 2) {
    countries <- names(holidays[isHoliday])
  } else {
    countries <- c("> 2")
  }
  paste(countries,collapse = " ")
}

# A partir del dataframe original en crea un de resum on hi ha el total d'atacs per país, el total de dies festius, el % d'atacs en festius i el % de festius anuals.
summarizeHolidaysAttacks <- function(totalTable, holidays) {
  # Preparem el nou data frame amb les columnes que ens interessen.
  rel <- data.frame(country = character(), totalAttacks = integer(), holidayAttacks = double(), hoildays = integer(), "hoilday/year" = double(), "holidayAttacks/attacks" = double(), stringsAsFactors = FALSE)
  years <- length(unique(format(as.Date(totalTable$Date),'%Y'))) # Els anys dels atacs, no repetis, que interessen 
  totalDays = years*365 # Total de dies en tots els anys dels atacs. Per fer % correctament.
  # Per cada país del df original extreiem les seves estadistiques
  for (co in unique(totalTable$Country)) {
    hy <- length(holidays[[co]]) # Total de dies festius del païs
    attacks <- length(totalTable[totalTable$Country == co,]$Date) # Total datacs al país
    holidayAttacks <- length(totalTable[totalTable$Country == co & totalTable$Holiday == TRUE,]$Date) # Total d'atacs al país en dies festius
    
    # Assignem els valors a una nova fila del dataframe
    country_rel <- data.frame(country = co, totalAttacks = attacks, holidayAttacks = holidayAttacks, hoildays = hy, "hoilday/year" = hy/totalDays, "holidayAttacks/attacks" = holidayAttacks/attacks, stringsAsFactors = FALSE)
    rel <- dplyr::bind_rows(rel, country_rel)
  }
  rel
}