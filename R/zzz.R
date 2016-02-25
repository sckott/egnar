enar_base <- function() "http://www.ebi.ac.uk/ena/data"

ct <- function(l) Filter(Negate(is.null), l)

enar_GET <- function(path, args = NULL, file = NULL, ...){
  if (grepl("download", path)) {
    if (is.null(file)) file <- gsub("&|=|/|:", "_", path)
    temp <- GET(file.path(enar_base(), path), query = args,
                write_disk(path = file, overwrite = FALSE), ...)
  } else {
    temp <- GET(file.path(enar_base(), path), query = args, ...)
  }
  stop_for_status(temp)
  #stopifnot(temp$headers$`content-type` == 'text/plain;charset=UTF-8')
  temp
}

cont <- function(x) {
  content(x, as = 'text', encoding = "UTF-8")
}

eparse <- function(x) {
  xml2::read_xml(x)
}
