#' Fetch data
#'
#' @export
#' @param id (character) Ids of various sorts, combine in a vector for > 1
#' @param result (character) result option, one of: sequence_release, assembly, ...
#' @param format (character) data format, one of xml (default), fasta, text, fastq
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return object of class \code{xml_document}
#' @references \url{http://www.ebi.ac.uk/ena/browse/data-retrieval-rest}
#' @examples \dontrun{
#' en_data(id = 'Taxon:4235')
#' en_data(id = c('Taxon:4235', 'Taxon:6543'))
#' en_data(id = 'A00145', format = "fasta")
#' en_data(id = 'A00145', format = "fastq")
#' en_data(id = 'A00145', format = "text")
#' ## not working right now
#' # taxa <- c('Taxon:Human', 'Taxon:Cat', 'Taxon:Mouse', 'Taxon:Zebrafish')
#' # en_data(id = taxa)
#' }
en_data <- function(id = NULL, result = NULL, format = "xml", ...) {
  eparse(
    cont(
      enar_GET(en_data_helper(id, result, format), ...)
    ), format)
}
