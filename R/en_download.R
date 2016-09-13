#' Download data
#'
#' @export
#' @param id (character) Ids of various sorts, combine in a vector for > 1
#' @param result (character) result option, one of: sequence_release,
#' assembly, ...
#' @param format (character) data format, one of xml (default), fasta,
#' text, fastq
#' @param limit (integer) Number of results to return
#' @param offset (integer) Result number to start at
#' @param max (integer) Max results
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return file path as character string, or parsed data
#' @details caching using \pkg{rappdirs}
#' @references \url{http://www.ebi.ac.uk/ena/browse/data-retrieval-rest}
#' @examples \dontrun{
#' en_download(id = 'Taxon:4235', limit = 10)
#' en_download(id = 'Taxon:4235', limit = 10, read = FALSE)
#' en_download(id = 'A00145', format = "fasta")
#' en_download(id = 'A00145', format = "fastq")
#' en_download(id = 'A00145', format = "text")
#' en_download(id = c('Taxon:4235', 'Taxon:6543'))
#' en_download(id = c('Taxon:4235', 'Taxon:6543'), read = FALSE)
#' }
en_download <- function(id = NULL, result = NULL, format = "xml", limit = NULL,
                        offset = NULL, max = NULL, read = TRUE, ...) {
  res <- en_data_helper(
    id, result, format, download = "gzip", limit, offset, max
  )
  pth <- enar_GET_down(res)
  if (read) {
    switch(
      format,
      xml = xml2::read_xml(pth),
      fasta = paste0(readLines(pth), collapse = ""),
      fastq = paste0(readLines(pth), collapse = ""),
      text = paste0(readLines(pth), collapse = "")
    )
  } else {
    return(pth)
  }
}

cache_path <- function() rappdirs::user_cache_dir("egnar-cache")
