#Código principal que carga los dataframes que se precisan

install.packages("countrycode")
library(countrycode)

install.packages("dplyr")
library(dplyr)


source("R/parseAttacks.R")
source("R/parseHolidays.R")
source("R/PIBs.R")
source("R/parseURLs.R")
source("R/modifyFrame.R")

attacks <- parseURL()
holidays <- getHolidays(attacks$Date, attacks$Country)
attacks <- setHolidays(attacks, holidays)
resumAttacks <- summarizeHolidaysAttacks(attacks, holidays)
df <- PIBs()


c <- attacks[attacks$Country=="US",]$HolidayIn
auxTable2 <- data.frame()
for (i in 1:length(c)) {
  nCountries <- unlist(strsplit(c[[i]], split = "\\s|\\n"))
  #For every country , create a new row with only 1 country each time
  for (z in 1:length(nCountries)) {
    auxTable2 <- rbind(auxTable2,data.frame(nCountries[z]))
  }
}

library(MASS)
percentFestiu <- (length(attacks[attacks$Holiday == TRUE,]$Date)/length(attacks$Date))*100
percentNoFestiu <- 100 - percentFestiu
percentFestiu <- format(round(percentFestiu, 2), nsmall = 2)
percentNoFestiu <- format(round(percentNoFestiu, 2), nsmall = 2)
titleNoFestiu <- paste("No festius:",percentNoFestiu,"%",sep = " ")
titleFestiu <- paste("Festius:",percentFestiu,"%",sep = " ")
pie(table(attacks$Holiday), labels = c(titleNoFestiu,titleFestiu))
title(main = "Dies dels atacs")

resumAttacks$holidayAttacks.attacks

h <- dplyr::select(resumAttacks,holidayAttacks.attacks,hoilday.year,country)
h$holidayAttacks.attacks <- h$holidayAttacks.attacks * 100
h$hoilday.year <- h$hoilday.year * 100
names(h) <- c("AttacksInHoliday","HolidaysYear","Country")

library(ggplot2)
g <- ggplot(h, aes(HolidaysYear,AttacksInHoliday))
g + stat_density2d(mapping=aes(x = HolidaysYear, y = AttacksInHoliday),data = h, h=10000)
 # g + stat_density2d()

us <- attacks[attacks$Country == "US",]
qplot(data = us, x = holidayIn, fill = Category )

g <- ggplot(resumAttacks, aes(totalAttacks,holidayAttacks) )
# g + stat_density2d(aes(fill = ..level..), geom="polygon", h=1)
g + geom_count()
