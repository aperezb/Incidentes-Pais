library("XML")
library("httr")
library(stringi)

# # May 23  ---> 2015/05/23
# lct <- Sys.getlocale("LC_TIME"); t <- Sys.setlocale("LC_TIME", "C")
# strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = "\\2\\12015", x = "May 25")
# as.Date(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
#
# # 16/03/2016  ---> 2016/03/16
# as.Date(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = "12/02/2016"))

getTimelinesLinks <- function(verbose = FALSE, greater = 2015) {
  baseURL <- "http://www.hackmageddon.com/category/security/cyber-attacks-timeline/page/"
  links <- data.frame(values = character(), stringsAsFactors = FALSE)
  i <- 1
  repeat {
    fileURL <- paste(baseURL, i, sep = "")
    page <- getHTMLPage(fileURL)
    if ( is.null(page) ) { break }
    else {
      i <- i + 1
    }
    temporal_links = parseLinks(page,greater)
    if (dim(temporal_links)[1] == 0) { break }
    if ( verbose ) { print( paste(fileURL, "->", dim(temporal_links)[1], "links found")) }
    links <- dplyr::bind_rows(links, temporal_links)
  }
  links
}

getHTMLPage <- function(fileUrl) {
  r <- HEAD(fileUrl)
  if ( status_code(r) == "200" ) {
    htmlTreeParse(fileUrl, useInternalNodes = T)
  } else {
    NULL
  }
}

parseLinks <- function(page,greater=2015) {
  val <- xpathSApply(page, '//h2[@class="entry-title"]/a', XML::xmlAttrs, name = "href")
  valFrame <- data.frame(values = val[1,], stringsAsFactors = FALSE )
  t <- (stri_extract_last(valFrame$values, regex = "\\d{4}") >= greater & grepl(pattern = "\\d{4}-cyber-attacks-timeline", x = valFrame$values))
  data.frame(values = valFrame$values[t], stringsAsFactors = FALSE)
}
