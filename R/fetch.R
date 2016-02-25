#' Fetch data
#'
#' @export
#' @param id Ids of various sorts, combine in a vector for > 1
#' @param result result option, one of: sequence_release, assembly, ...
#' @param format data format, one of xml (default), fasta, text, fastq
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return data.frame
#' @examples \dontrun{
#' en_data(id = 'Taxon:4235')
#' en_data(id = c('Taxon:4235', 'Taxon:6543'))
#' en_data(id = c('Taxon:Human', 'Taxon:Cat', 'Taxon:Mouse', 'Taxon:Zebrafish'))
#' }
en_data <- function(id = NULL, result = NULL, format = "xml", ...) {
  eparse(cont((en_data_helper(id, result, ...))))
}

# helper ---------
en_data_helper <- function(id = NULL, result = NULL, format = "xml",
                           download = NULL, file = NULL, ...) {
  args <- ct(list(display = format, result = result, download = download))
  if (length(args) > 0) args <- paste("&", paste(names(args), args, sep = "="), sep = "", collapse = "")
  if (is.null(id)) {
    path <- 'view'
  } else {
    id <- paste(id, collapse = ",")
    path <- file.path('view', id)
  }
  path <- paste0(path, args)
  enar_GET(path, file = file, ...)
}
