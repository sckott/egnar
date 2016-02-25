#' Fetch data
#'
#' @export
#' @param id Ids of various sorts, combine in a vector for > 1
#' @param result result option, one of: sequence_release, assembly, ...
#' @param format data format, one of xml (default), fasta, text, fastq
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return data.frame
#' @examples \dontrun{
#' en_download(id = 'Taxon:4235')
#' en_download(id = c('Taxon:4235', 'Taxon:6543'))
#' en_download(id = c('Taxon:Human', 'Taxon:Cat', 'Taxon:Mouse', 'Taxon:Zebrafish'))
#' }
en_download <- function(id = NULL, result = NULL, format = "xml", file = NULL, ...) {
  res <- en_data_helper(id, result, format, download = "gzip", file = file, ...)
  res$request$output$path
}
