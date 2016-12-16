library("XML")
library("httr")

# # May 23  ---> 2015/05/23
# lct <- Sys.getlocale("LC_TIME"); t <- Sys.setlocale("LC_TIME", "C")
# strTime <- sub(pattern = '(\\w{3})\\s(\\d{1,2})', replacement = "\\2\\12015", x = "May 25")
# as.Date(format(strptime(strTime, "%d%b%Y"),"%Y/%m/%d/%m"))
#
# # 16/03/2016  ---> 2016/03/16
# as.Date(sub(pattern = '(\\d{2})/(\\d{2})/(\\d{4})', replacement = "\\3/\\2/\\1", x = "12/02/2016"))

getTimelinesLinks <- function(verbose=FALSE) {
  baseURL <- "http://www.hackmageddon.com/category/security/cyber-attacks-timeline/page/"
  links <- data.frame(values = character())
  i <- 1
  repeat {
    fileURL <- paste(baseURL, i, sep = "")
    page <- getHTMLPage(fileURL)
    if ( is.null(page) ) { break }
    else {
      i <- i + 1
    }
    temporal_links = parseLinks(page)
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

parseLinks <- function(page) {
  val <- xpathSApply(page, '//h2[@class="entry-title"]/a', XML::xmlAttrs, name = "href")
  data.frame(values = val[1,] )
}
