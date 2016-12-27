# Dependències del fitxer
library(XML)
library(httr)
library(countrycode)

# Funció per saber si una data (o vector de dates) és dia festiu en el país o no. 
isCountryHoliday <- function(dates,country,holidays) {
  as.Date(dates) %in% holidays[[country]]
}

# Recuepra tots els festius dels anys de la llista year de tots els països de la llista
getCountriesHolidays <- function(countries, years) {
  holidays <- c()
  for (c in countries) {
    # Per cada país recuperem els seus festius...
    holidays <- c(holidays, getCountryHolidays(c, years))
  }
  holidays
}

# Recuper tots els dies festius dels anys de la llista years pel país especificat
getCountryHolidays <- function(country, years) {
  # Passem el nom del país a format llarg
  countryISO2 <- countrycode(toupper(country), "iso2c", "country.name")
 # print(paste("Parsing",countryISO2,"holidays...",sep = " ")) # Indiquem el procés...
  # En cas de països amb nom de més d'una paraula les saparem per "_" que és com ho demana el web
  countryISO2 <- gsub(pattern = '\\s|\\n', replacement = "_", x = countryISO2)
  # En el cas d'Estatus Units el web de holidays està clasificat com a "usa"
  countryISO2 <- ifelse(toupper(country) == "US","usa",countryISO2)
  if (is.na(countryISO2)) { 
    # Si la conversió de iso2 a nom complert no existeix posem NA com a holiday
    holidays <- data.frame(c(NA), stringsAsFactors = FALSE)
  } else {
    holidays <- data.frame(stringsAsFactors = FALSE)
    for (y in years) {
      # Per cada any parsejem els dies festius
      holidays <- dplyr::bind_rows(holidays, parseYear(y, countryISO2))
    }
  }
  # Assignem el nom del país a la columna
  names(holidays) <- toupper(country)
  holidays
}

# Funció que permet convertir un strings de data a un objecte Date. Converteix els formats May 23 i 23/05/2016 a 2016/05/23
dateToISO <- function(table,year=2015) {
  if (length(table) == 0) { 
    # Si no hi ha res a convertir retornem NA
    return(c(NA))
  }
  # Preparem el sistema per parsejar correctament la data
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  
  as.Date(sapply(lapply(table, FUN = function(elem) {
    if (grepl(pattern = "\\w{3}\\s\\d{1,2}", x = elem)) { # Format May 23
      replace <- paste("\\2\\1", year, sep = "")
      strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = replace, x = elem)
      as.character(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
    } else if(grepl(pattern = "(\\d{2})/(\\d{2})/(\\d{4})", x = elem)) { # Format 23/05/2016
      as.character(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = elem))
    } else { # Un altre format no suportat. Retornem NA
      c(NA)
    }
  }), function(x) as.character(x)))
}

# Funció que parseja els dies fastius d'un any en concret per a un país.
parseYear <- function(year, country) {
  if (is.na(country)) { # Si el país no existeix no cal buscar dates -> NA
    return(c(NA))
  }
  # Les dates s'extreuen de la següent URL on country és el nom llarg del país i year és la representació de 4 digits de l'any.
  # http://www.officeholidays.com/countries/{country}/{year}.php
  fileURL <- paste('http://www.officeholidays.com/countries/', country, '/', year,'.php',
                   sep = "")
  
  # Comprovem que la pàgina existeixi i no retorni un codi 404 perquè sinó el parser falla...
  r <- HEAD(fileURL)
  if ( status_code(r) == "200" ) { # Tot OK
    # Parse de la pàgina
    page <- htmlTreeParse(fileURL, useInternalNodes = T)
  
    # Recuprem els celes on està la informació del festiu utilitzant xPath.
    generalHoliday <- '//tr[@class="holiday" or @class="regional"]/td/span[@class="mobile_ad"]'
    x <- xpathSApply(page, generalHoliday, xmlValue) # Extraiem el valor del festiu
    
    # La data està en format May 23 per tan la convertim a tipo Date.
    d <- data.frame(dateToISO(x, year = year), stringsAsFactors = FALSE)
    names(d) <- country # Especifiquem el nom del país del que hem recuperat els fetsius
    d
  } else { # No existeix la pàgina al web (o no tenen les dades o el nom del país és diferent)
    d <- data.frame(c(NA), stringsAsFactors = FALSE) # NA a festius del país i punto!
    names(d) <- country
    d
  }
}
