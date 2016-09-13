enar_base <- function() "http://www.ebi.ac.uk/ena/data"

ct <- function(l) Filter(Negate(is.null), l)

enar_GET <- function(path, args = NULL, ...){
  temp <- httr::GET(file.path(enar_base(), path), query = args, ...)
  httr::stop_for_status(temp)
  temp
}

enar_GET_down <- function(path, overwrite = TRUE, ...){
  fpath <- file.path(cache_path(), make_f(path))
  if (file.exists(fpath)) {
    message("File in cache")
    return(fpath)
  } else {
    temp_path <- tempfile()
    res <- httr::GET(
      file.path(enar_base(), path),
      httr::write_disk(path = temp_path, overwrite = overwrite)
      )

    #if download has failed, it will stop here
    handle_errors(res, fpath)

    # create directory if it doesn't exist yet
    dir.create(cache_path(), showWarnings = FALSE, recursive = TRUE)
    file.rename(temp_path, fpath)

    # return file path
    return(fpath)
  }
}

make_f <- function(x) {
  paste0(digest::digest(x), ".gzip")
}

cont <- function(x) {
  httr::content(x, as = 'text', encoding = "UTF-8")
}

foo <- function(x) {
  xml2::read_xml(gzcon(rawConnection(content(temp2))))
}

eparse <- function(x, format) {
  switch(
    format,
    xml = xml2::read_xml(x),
    fasta = x,
    fastq = x,
    text = x
  )
}

en_data_helper <- function(id = NULL, result = NULL, format = "xml",
                           download = NULL, limit = NULL, offset = NULL,
                           max = NULL) {

  args <- ct(list(
    display = format, result = result, download = download,
    length = limit, offset = offset, max = max
  ))
  if (length(args) > 0) args <- paste("&", paste(names(args), args, sep = "="), sep = "", collapse = "")
  if (is.null(id)) {
    path <- 'view'
  } else {
    id <- curl::curl_escape(paste(id, collapse = ","))
    path <- file.path('view', id)
  }
  paste0(path, args)
  #enar_GET(path, file = file, ...)
}

handle_errors <- function(x, path) {
  if (x$status_code > 201) {
    httr::stop_for_status(x)
    unlink(path, recursive = TRUE, force = TRUE)
  }
}
