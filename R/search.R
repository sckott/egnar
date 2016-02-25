#' Search for data
#'
#' @export
#' @param query Query terms
#' @param result result option, one of: sequence_release, assembly, ...
#' @param limit Number of results to return
#' @param offset Result number to start at
#' @param max Max results
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return data.frame
#' @examples \dontrun{
#' en_search(query = '"geo_circ(-0.587,-90.5713,170)"', result='sequence_release')
#' en_search(query = '"geo_circ(-0.587,-90.5713,170)"', result='sequence_release', limit = 1)
#' en_search(query = '"tax_eq(10090)"', result = 'assembly')
#' ## failing
#' # en_search(query = '"tax_tree(7147) AND dataclass="STD"')
#' }
en_search <- function(query = NULL, result = NULL, limit = NULL, offset = NULL,
                      max = NULL, ...) {
  args <- ct(list(query = query, display = 'xml', result = result,
                  length = limit, offset = offset, max = max))
  eparse(cont(enar_GET(path = 'warehouse/search', args, ...)))
}
