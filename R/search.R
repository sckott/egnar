#' Search for data
#'
#' @export
#' @param query Query terms
#' @param result result option, one of: sequence_release, assembly, ...
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return data.frame
#' @examples \dontrun{
#' en_search(query = '"geo_circ(-0.587,-90.5713,170)"', result='sequence_release')
#' en_search(query = '"tax_eq(10090)"', result = 'assembly')
#' ## failing
#' # en_search(query = '"tax_tree(7147) AND dataclass="STD"')
#' }
en_search <- function(query = NULL, result = NULL, ...) {
  args <- ct(list(query = query, display = 'xml', result = result))
  enar_parse(enar_GET(path = 'warehouse/search', args, ...))
}
