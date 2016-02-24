enar_base <- function() "http://www.ebi.ac.uk/ena/data"

ct <- function(l) Filter(Negate(is.null), l)

enar_GET <- function(path, args = NULL, ...){
  temp <- GET(file.path(enar_base(), path), query = args, ...)
  stop_for_status(temp)
  stopifnot(temp$headers$`content-type` == 'text/plain;charset=UTF-8')
  content(temp, as = 'text', encoding = "UTF-8")
}

enar_parse <- function(x) {
  xml2::read_xml(x)
}
