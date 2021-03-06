#Create the data frame from a file containing the urls to parse
#source("./R/parseAttacks.R")

# Modificat per Angel el 29/12, el parseURL es talla en varies funcions per poder anar-ho cridant progressivament desde el rmarkdown
# carga_atacs() --> Carrega el dataframe principal d'atacs
# carga_Categories_atacs() --> dataframe amb  resum de tipus d'atacs per pais
# pintaMapaAtaques() --> pinta mapa del món amb atacs
# pintaMapaAtaques_noUSAGB() --> idem sense USA ni GB que distorsionen
# pintaResumAtacs() --> Agregat en barres

# Tot el codi del Carles dins d'una funció... En teoria parseja tots els atacs llistats a la pàgina web i crec wue també li ha fotut tot el codi per generar un mapa sexi... Per què separar el codi i fer-lo llegible? Nah, és to mainstream. El codi ordenat són els pares igual que el tió, els reis, el pare Noel... FUCK CARLES
parseURL <- function() {
  #links <- getTimel
  data <- read.table("./Dades/URLs_To_Parse.txt")
  data$urls <- as.character(data$V1)
  # Know the length of the file that contains the urls to parse
  totalLen <- nrow(data)
  #Define the trim function
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  # Get the info from all urls and bind the information together
  library(XML)
  library(dplyr)
  totalTable <- data.frame()

  #for(i in 1:totalLen) {
  #  rowInfo <- as.character(data[i,1])
  #  table <- readHTMLTable(row)
  #  totalTable <- dplyr::bind_rows(totalTable, table)
  #}

  for(url in data$urls) {
    table <- readHTMLTable(url)
    totalTable <- dplyr::bind_rows(totalTable,table)
  }
  names(totalTable)[1] <- "ID_1"
  names(totalTable)[2] <- "Date_1"
  names(totalTable)[3] <- "Author_1"
  names(totalTable)[4] <- "Target_1"
  names(totalTable)[5] <- "Description_1"
  names(totalTable)[6] <- "Attack_1"
  names(totalTable)[7] <- "Target_Class_1"
  names(totalTable)[8] <- "Category_1"
  names(totalTable)[9] <- "Country_1"
  names(totalTable)[10] <- "Link_1"
  names(totalTable)[11] <- "Category_2"
  names(totalTable)[12] <- "Tags"
  names(totalTable)[13] <- "ID_2"
  names(totalTable)[14] <- "Description_2"
  names(totalTable)[15] <- "ID_3"
  names(totalTable)[16] <- "Date_2"
  names(totalTable)[17] <- "Author_2"
  names(totalTable)[18] <- "Target_2"
  names(totalTable)[19] <- "Description_3"
  names(totalTable)[20] <- "Attack_2"
  names(totalTable)[21] <- "Target_Class_2"
  names(totalTable)[22] <- "Category_3"
  names(totalTable)[23] <- "Country_2"
  names(totalTable)[24] <- "ID_4"
  names(totalTable)[25] <- "Target_Class_3"
  names(totalTable)[26] <- "Category_4"

  #Unify all the IDs
  #totalTable <- dplyr::mutate(totalTable, ID = ifelse(test=is.na(ID_1), yes = ifelse(test = is.na(ID_2), yes = ifelse(test = is.na(ID_3), yes = ID_4, no = ID_3), no = ID_2),no = ID_1))
  #Unify all Dates
  totalTable <- dplyr::mutate(totalTable, Date = ifelse(test = is.na(Date_1), yes = Date_2, no = Date_1))
  #Unify all Authors
  #totalTable <- dplyr::mutate(totalTable, Author = ifelse(test = is.na(Author_1), yes = Author_2, no = Author_1))
  #Unify all Targets
  #totalTable <- dplyr::mutate(totalTable, Target = ifelse(test = is.na(Target_1), yes = Target_2, no = Target_1))
  #Unify all Descriptions
  #totalTable <- dplyr::mutate(totalTable, Description = ifelse(test = is.na(Description_1), yes = ifelse(test = is.na(Description_2), yes = Description_3, no = Description_2), no = Description_1))
  #Unify all Attacks
  totalTable <- dplyr::mutate(totalTable, Attack = ifelse(test = is.na(Attack_1), yes = Attack_2, no = Attack_1))
  #Unify all Target Classes
  #totalTable <- dplyr::mutate(totalTable, Target_Class = ifelse(test = is.na(Target_Class_1), yes = ifelse(test = is.na(Target_Class_2), yes = Target_Class_3, no = Target_Class_2), no = Target_Class_1))
  #Unify all Categories
  totalTable <- dplyr::mutate(totalTable, Category = ifelse(test = is.na(Category_1), yes = ifelse(test = is.na(Category_2), yes = ifelse(test = is.na(Category_3), yes = Category_4, no = Category_3), no = Category_2),no = as.character(Category_1)))
  #Unify Countries
  totalTable <- dplyr::mutate(totalTable, Country = ifelse(test = is.na(Country_1), yes = Country_2, no = Country_1))
  #We don't care about information link
  #We delete all the columns we dont want
  totalTable$ID_1 <- NULL
  totalTable$Date_1 <- NULL
  totalTable$Author_1 <- NULL
  totalTable$Target_1 <- NULL
  totalTable$Description_1 <- NULL
  totalTable$Attack_1 <- NULL
  totalTable$Target_Class_1 <- NULL
  totalTable$Category_1 <- NULL
  totalTable$Country_1 <- NULL
  totalTable$Link_1 <- NULL
  totalTable$Category_2 <- NULL
  totalTable$Tags <- NULL
  totalTable$ID_2 <- NULL
  totalTable$Description_2 <- NULL
  totalTable$ID_3 <- NULL
  totalTable$Date_2 <- NULL
  totalTable$Author_2 <- NULL
  totalTable$Target_2 <- NULL
  totalTable$Description_3 <- NULL
  totalTable$Attack_2 <- NULL
  totalTable$Target_Class_2 <- NULL
  totalTable$Category_3 <- NULL
  totalTable$Country_2 <- NULL
  totalTable$ID_4 <- NULL
  totalTable$Target_Class_3 <- NULL
  totalTable$Category_4 <- NULL

  #solve the dates

  # transformDates <- function(x){
  #   aux <- unlist(strsplit("May 16"," ", fixed=TRUE))
  #   strTime <- paste(aux[2],aux[1],"2015", sep="")
  #   x<-format(strptime(strTime, "%d%b%Y"), "%d/%m/%Y")
  # }
  # transformDates("May 16")

  totalTable$Date <- as.Date(sapply(lapply(totalTable$Date, FUN = function(elem) {
    if (!grepl(pattern = ".*/", elem)) {
      strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = "\\2\\12015", x = elem)
      as.character(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
    } else {
      as.character(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = elem))
    }
  }), function(x) as.character(x)))


  #totalTable$Date <- as.Date(sapply(kk, function(x) as.character(x)))
  #Delete the rows we don't need
  totalTable <- dplyr::filter(totalTable, grepl(pattern = "^[a-zA-Z].*" ,x = Country))
  totalTable <- dplyr::filter(totalTable, grepl(pattern = "[^Country]", x = Country))

  countries <- dplyr::filter(totalTable, grepl(pattern = "^.{5,50}$", x = Country))

  #auxTable <- data.frame("Date", "Attack", "Category", "Country")
  auxTable <- data.frame()
  #For every cell with more than one country, how many countries are there?
  for (i in 1:nrow(countries)) {
    nCountries <- unlist(strsplit(countries$Country[[i]], split = "\\s|\\n"))
    #For every country , create a new row with only 1 country each time
    for (z in 1:length(nCountries)) {
      auxTable <- rbind(auxTable,data.frame(countries$Date[i],countries$Attack[i],countries$Category[i],nCountries[z]))
    }
  }

  #Put the same names as totalTable in order to be able to do rbind
  names(auxTable) <- names(totalTable)
  #Bind auxTable and TotalTable
  totalTable <- rbind(totalTable,auxTable)

  for(i in 1:nrow(totalTable)){
    if(grepl(pattern = "^UK$", totalTable$Country[i])){
      totalTable$Country[i] <- "GB"
    }
  }

  #Filter all the values to only get the countries that have 2 characters
  totalTable <- dplyr::filter(totalTable, grepl(pattern = "^.{2}$", x = Country))
  #Sort table by country, Date
  totalTable <- dplyr::arrange(totalTable, Country, Date)
  #Delete repeated rows
  totalTable <- unique(totalTable)
  #Construct a table with only the values we need, that are country and attack category
  category_Country <- as.data.frame.matrix(table(totalTable$Country,totalTable$Category))
  category_Country$`Industry: Telco`<-NULL
  category_Country$`N/A`<- NULL
  names(category_Country$`CW?`)<-"CW"
  category_Country$`CW?`<-NULL
  category_Country <- cbind(category_Country, Total = rowSums(category_Country))
  category_Country <- cbind(category_Country, unique(totalTable$Country))
  names(category_Country)[6]<- "Country"
  category_Country <- dplyr::arrange(category_Country, desc(Total))

  #completeCode <- countrycode(category_Country$Country,"iso2c","country.name")
  #category_Country <- cbind(category_Country,completeCode)

  #printColoredMap(category_Country)

  #printColoredMap <- function(Country_Category_dataFrame) {
  n <- joinCountryData2Map(category_Country, joinCode="ISO2", nameJoinColumn="Country")

  totalColoredMap <- mapCountryData(n, nameColumnToPlot="Total", mapTitle="Total attacks",missingCountryCol="black",oceanCol="lightblue",catMethod = c(0,10,20,30,40,50,546), addLegend = TRUE)
  # we see that attacks on USA and GB difficults the analysis of the plot, so we make up another table without them and repeat the plotting.
  category_Country_Smooth <- category_Country[3:nrow(category_Country),]
  #Plot again
  n <- joinCountryData2Map(category_Country_Smooth, joinCode="ISO2", nameJoinColumn="Country")
  # dev.new()
  ColoredMapNoUSA_UK <-mapCountryData(n, nameColumnToPlot="Total", mapTitle="World attacks (except USA UK)",missingCountryCol="black",oceanCol="lightblue",catMethod = c(0,10,20,30,40,50), addLegend = TRUE)
  #}

  #Create a vector with the sum of every kind of attack
  totalAttacks <- c("CC" = sum(category_Country$CC), "CE" = sum(category_Country$CE),"CW" = sum(category_Country$CW),"H" = sum(category_Country$H))
  #Most atacks are Ciber Crime and then Hijacking
  # dev.new()
  plotTotalAttacks <- barplot(totalAttacks, main = "Total registered attacks")

  totalTable
}


Carga_Atacs1 <- function() {
  #links <- getTimel
  data <- read.table("./Dades/URLs_To_Parse.txt")
  data$urls <- as.character(data$V1)
  # Know the length of the file that contains the urls to parse
  totalLen <- nrow(data)
  #Define the trim function
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  # Get the info from all urls and bind the information together
  library(XML)
  library(dplyr)
  totalTable <- data.frame()
  
  #for(i in 1:totalLen) {
  #  rowInfo <- as.character(data[i,1])
  #  table <- readHTMLTable(row)
  #  totalTable <- dplyr::bind_rows(totalTable, table)
  #}
  
  for(url in data$urls) {
    table <- readHTMLTable(url)
    totalTable <- dplyr::bind_rows(totalTable,table)
  }
  names(totalTable)[1] <- "ID_1"
  names(totalTable)[2] <- "Date_1"
  names(totalTable)[3] <- "Author_1"
  names(totalTable)[4] <- "Target_1"
  names(totalTable)[5] <- "Description_1"
  names(totalTable)[6] <- "Attack_1"
  names(totalTable)[7] <- "Target_Class_1"
  names(totalTable)[8] <- "Category_1"
  names(totalTable)[9] <- "Country_1"
  names(totalTable)[10] <- "Link_1"
  names(totalTable)[11] <- "Category_2"
  names(totalTable)[12] <- "Tags"
  names(totalTable)[13] <- "ID_2"
  names(totalTable)[14] <- "Description_2"
  names(totalTable)[15] <- "ID_3"
  names(totalTable)[16] <- "Date_2"
  names(totalTable)[17] <- "Author_2"
  names(totalTable)[18] <- "Target_2"
  names(totalTable)[19] <- "Description_3"
  names(totalTable)[20] <- "Attack_2"
  names(totalTable)[21] <- "Target_Class_2"
  names(totalTable)[22] <- "Category_3"
  names(totalTable)[23] <- "Country_2"
  names(totalTable)[24] <- "ID_4"
  names(totalTable)[25] <- "Target_Class_3"
  names(totalTable)[26] <- "Category_4"
  totalTable
}  

Carga_Atacs2 <- function(totalTable) {
  #Unify all the IDs
  #totalTable <- dplyr::mutate(totalTable, ID = ifelse(test=is.na(ID_1), yes = ifelse(test = is.na(ID_2), yes = ifelse(test = is.na(ID_3), yes = ID_4, no = ID_3), no = ID_2),no = ID_1))
  #Unify all Dates
  totalTable <- dplyr::mutate(totalTable, Date = ifelse(test = is.na(Date_1), yes = Date_2, no = Date_1))
  #Unify all Authors
  #totalTable <- dplyr::mutate(totalTable, Author = ifelse(test = is.na(Author_1), yes = Author_2, no = Author_1))
  #Unify all Targets
  #totalTable <- dplyr::mutate(totalTable, Target = ifelse(test = is.na(Target_1), yes = Target_2, no = Target_1))
  #Unify all Descriptions
  #totalTable <- dplyr::mutate(totalTable, Description = ifelse(test = is.na(Description_1), yes = ifelse(test = is.na(Description_2), yes = Description_3, no = Description_2), no = Description_1))
  #Unify all Attacks
  totalTable <- dplyr::mutate(totalTable, Attack = ifelse(test = is.na(Attack_1), yes = Attack_2, no = Attack_1))
  #Unify all Target Classes
  #totalTable <- dplyr::mutate(totalTable, Target_Class = ifelse(test = is.na(Target_Class_1), yes = ifelse(test = is.na(Target_Class_2), yes = Target_Class_3, no = Target_Class_2), no = Target_Class_1))
  #Unify all Categories
  totalTable <- dplyr::mutate(totalTable, Category = ifelse(test = is.na(Category_1), yes = ifelse(test = is.na(Category_2), yes = ifelse(test = is.na(Category_3), yes = Category_4, no = Category_3), no = Category_2),no = as.character(Category_1)))
  #Unify Countries
  totalTable <- dplyr::mutate(totalTable, Country = ifelse(test = is.na(Country_1), yes = Country_2, no = Country_1))
  #We don't care about information link
  #We delete all the columns we dont want
  totalTable$ID_1 <- NULL
  totalTable$Date_1 <- NULL
  totalTable$Author_1 <- NULL
  totalTable$Target_1 <- NULL
  totalTable$Description_1 <- NULL
  totalTable$Attack_1 <- NULL
  totalTable$Target_Class_1 <- NULL
  totalTable$Category_1 <- NULL
  totalTable$Country_1 <- NULL
  totalTable$Link_1 <- NULL
  totalTable$Category_2 <- NULL
  totalTable$Tags <- NULL
  totalTable$ID_2 <- NULL
  totalTable$Description_2 <- NULL
  totalTable$ID_3 <- NULL
  totalTable$Date_2 <- NULL
  totalTable$Author_2 <- NULL
  totalTable$Target_2 <- NULL
  totalTable$Description_3 <- NULL
  totalTable$Attack_2 <- NULL
  totalTable$Target_Class_2 <- NULL
  totalTable$Category_3 <- NULL
  totalTable$Country_2 <- NULL
  totalTable$ID_4 <- NULL
  totalTable$Target_Class_3 <- NULL
  totalTable$Category_4 <- NULL
  
  #solve the dates
  
  # transformDates <- function(x){
  #   aux <- unlist(strsplit("May 16"," ", fixed=TRUE))
  #   strTime <- paste(aux[2],aux[1],"2015", sep="")
  #   x<-format(strptime(strTime, "%d%b%Y"), "%d/%m/%Y")
  # }
  # transformDates("May 16")
  
  totalTable$Date <- as.Date(sapply(lapply(totalTable$Date, FUN = function(elem) {
    if (!grepl(pattern = ".*/", elem)) {
      strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = "\\2\\12015", x = elem)
      as.character(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
    } else {
      as.character(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = elem))
    }
  }), function(x) as.character(x)))

  #totalTable$Date <- as.Date(sapply(kk, function(x) as.character(x)))
  #Delete the rows we don't need
  totalTable <- dplyr::filter(totalTable, grepl(pattern = "^[a-zA-Z].*" ,x = Country))
  totalTable <- dplyr::filter(totalTable, grepl(pattern = "[^Country]", x = Country))
  
  countries <- dplyr::filter(totalTable, grepl(pattern = "^.{5,50}$", x = Country))
  

  #auxTable <- data.frame("Date", "Attack", "Category", "Country")
  auxTable <- data.frame()
  #For every cell with more than one country, how many countries are there?
  for (i in 1:nrow(countries)) {
    nCountries <- unlist(strsplit(countries$Country[[i]], split = "\\s|\\n"))
    #For every country , create a new row with only 1 country each time
    for (z in 1:length(nCountries)) {
      auxTable <- rbind(auxTable,data.frame(countries$Date[i],countries$Attack[i],countries$Category[i],nCountries[z]))
    }
  }
  
  
  #Put the same names as totalTable in order to be able to do rbind
  names(auxTable) <- names(totalTable)
  #Bind auxTable and TotalTable
  totalTable <- rbind(totalTable,auxTable)
  
  for(i in 1:nrow(totalTable)){
    if(grepl(pattern = "^UK$", totalTable$Country[i])){
      totalTable$Country[i] <- "GB"
    }
  }
  
  #Filter all the values to only get the countries that have 2 characters
  totalTable <- dplyr::filter(totalTable, grepl(pattern = "^.{2}$", x = Country))
  #Sort table by country, Date
  totalTable <- dplyr::arrange(totalTable, Country, Date)
  #Delete repeated rows
  totalTable <- unique(totalTable)
  totalTable
}

Carga_Categories_atacs_Pais <- function(totalTable){
  #Afegida Angel 29/12/16 per poder tenir el dataframe de categories pais per separat
  #Construct a table with only the values we need, that are country and attack category
  category_Country <- as.data.frame.matrix(table(totalTable$Country,totalTable$Category))
  category_Country$`Industry: Telco`<-NULL
  category_Country$`N/A`<- NULL
  names(category_Country$`CW?`)<-"CW"
  category_Country$`CW?`<-NULL
  category_Country <- cbind(category_Country, Total = rowSums(category_Country))
  category_Country <- cbind(category_Country, unique(totalTable$Country))
  names(category_Country)[6]<- "Country"
  category_Country <- dplyr::arrange(category_Country, desc(Total))

  category_Country
}

pintaMapaAtaques <- function(totalTable,category_Country){
  library(rworldmap)
  n <- joinCountryData2Map(category_Country, joinCode="ISO2", nameJoinColumn="Country")
  
  totalColoredMap <- mapCountryData(n, nameColumnToPlot="Total", mapTitle="Total attacks",missingCountryCol="black",oceanCol="lightblue",catMethod = c(0,10,20,30,40,50,546), addLegend = TRUE)
}

pintaMapaAtaques_noUSAGB <- function(totalTable,category_Country){
  # we see that attacks on USA and GB difficults the analysis of the plot, so we make up another table without them and repeat the plotting.
  category_Country_Smooth <- category_Country[3:nrow(category_Country),]
  #Plot again
  n <- joinCountryData2Map(category_Country_Smooth, joinCode="ISO2", nameJoinColumn="Country")
  # dev.new()
  ColoredMapNoUSA_UK <-mapCountryData(n, nameColumnToPlot="Total", mapTitle="World attacks (except USA UK)",missingCountryCol="black",oceanCol="lightblue",catMethod = c(0,10,20,30,40,50), addLegend = TRUE)
  
}

pintaResumAtacs <- function(category_Country){
  #Create a vector with the sum of every kind of attack
  totalAttacks <- c("CC" = sum(category_Country$CC), "CE" = sum(category_Country$CE),"CW" = sum(category_Country$CW),"H" = sum(category_Country$H))
  #Most atacks are Ciber Crime and then Hijacking
  # dev.new()
  plotTotalAttacks <- barplot(totalAttacks, main = "Total registered attacks", col = "red")
}