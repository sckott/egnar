#' Fetch data
#'
#' @export
#' @param id Ids of various sorts, combine in a vector for > 1
#' @param result result option, one of: sequence_release, assembly, ...
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return data.frame
#' @examples \dontrun{
#' en_fetch(id = 'Taxon:4235')
#' en_fetch(id = c('Taxon:4235', 'Taxon:6543'))
#' en_fetch(id = c('Taxon:Human', 'Taxon:Cat', 'Taxon:Mouse', 'Taxon:Zebrafish'))
#' }
en_fetch <- function(id = NULL, result = NULL, ...) {
  args <- ct(list(display = 'xml', result = result))
  if (length(args) > 0) args <- paste0("&", paste(names(args), args, sep = "="))
  if (is.null(id)) {
    path <- 'view'
  } else {
    id <- paste(id, collapse = ",")
    path <- file.path('view', id)
  }
  path <- paste0(path, args)
  enar_parse(enar_GET(path, ...))
}
