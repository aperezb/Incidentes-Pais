# Dependències del fitxer
library("XML")
library("httr")
library(stringi)

# Funció per extreure totes les url a les pàgines que contenten una taula resum dels atacs de cada mes. Permet expecificar a prtir de quin any volem recuperar la informacio i si ens interessa que ens digui el que està fent en cada moment.
getTimelinesLinks <- function(verbose = FALSE, greater = 2015) {
  # La pàgina on estan llistats tots els links a les taules.
  baseURL <- "http://www.hackmageddon.com/category/security/cyber-attacks-timeline/page/"
  links <- data.frame(values = character(), stringsAsFactors = FALSE)
  i <- 1
  repeat { # Ho repetim per cada pàgina
    fileURL <- paste(baseURL, i, sep = "")
    page <- getHTMLPage(fileURL)
    # Recuprem la pàgina i si ens retorna un codi 404 vol dir que no hi ha més pàgines ja.
    if ( is.null(page) ) { break }
    else {
      i <- i + 1
    }
    # Per cada pàgina busquem tots els links a les taules d'atacs.
    temporal_links = parseLinks(page,greater)
    if (dim(temporal_links)[1] == 0) { break }
    if ( verbose ) { print( paste(fileURL, "->", dim(temporal_links)[1], "links found")) }
    # Els incorporem al total de links que ja hem trobat anteriorment.
    links <- dplyr::bind_rows(links, temporal_links)
  }
  links
}

# Mira si la pàgina existeix abans de parsejar-la ja que si el servidor retorna el codi 404 el parser dona un error.
getHTMLPage <- function(fileUrl) {
  r <- HEAD(fileUrl)
  if ( status_code(r) == "200" ) {
    # Tot OK, parse de la pàgina!
    htmlTreeParse(fileUrl, useInternalNodes = T)
  } else {
    # Malament, per tant el resultat del parser és null.
    NULL
  }
}

# Busca tots els enllaços que portin a taules d'atacs utilitzant la situació de l'enllaç amb xPath i el format de l'enllaç: [...]2016-cyber-attacks-timeline[...]
parseLinks <- function(page,greater=2015) {
  val <- xpathSApply(page, '//h2[@class="entry-title"]/a', XML::xmlAttrs, name = "href")
  valFrame <- data.frame(values = val[1,], stringsAsFactors = FALSE )
  t <- (stri_extract_last(valFrame$values, regex = "\\d{4}") >= greater & grepl(pattern = "\\d{4}-cyber-attacks-timeline", x = valFrame$values))
  data.frame(values = valFrame$values[t], stringsAsFactors = FALSE)
}
