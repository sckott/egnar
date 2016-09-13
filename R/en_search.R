#' Search for data
#'
#' @export
#' @param query (character) Query terms
#' @param result (character) result option, one of: sequence_release,
#' assembly, coding_release, coding_update, read_run (are there others?)
#' @param limit (integer) Number of results to return
#' @param offset (integer) Result number to start at
#' @param max (integer) Max results
#' @param ... Curl options passed to \code{\link[httr]{GET}}
#' @return A list of lists, where each nested list is a record. in the future
#' we hope to parse to a single data.frame
#' @references \url{http://www.ebi.ac.uk/ena/browse/search-rest}
#' @examples \dontrun{
#' # sequence_release
#' res <- en_search(query = '"geo_circ(-0.587,-90.5713,170)"', result='sequence_release')
#' lapply(res, "[[", "seq")
#' ## another eg.
#' en_search(query = '"geo_circ(-0.587,-90.5713,170)"', result='sequence_release', limit = 1)
#'
#' # assembly
#' res <- en_search(query = '"tax_eq(10090)"', result = 'assembly')
#' dplyr::bind_rows(lapply(res, "[[", "chromosomes"), .id = "id")
#'
#' # coding_release
#' res <- en_search(query = '"tax_tree(7147) AND dataclass="STD""', result = 'coding_release', limit = 5)
#' vapply(res, "[[", "", "desc")
#'
#' # coding_update
#' res <- en_search(query = '"tax_tree(7147) AND dataclass="STD""', result = 'coding_update', limit = 5)
#' res$AAA08526$refs
#' res$AAA08526$features
#'
#' ## failing
#' # en_search(query = '"tax_tree(7147) AND dataclass="STD"')
#' }
en_search <- function(query = NULL, result = NULL, limit = NULL, offset = NULL,
                      max = NULL, ...) {
  res <- c("sequence_release", "assembly", "coding_release", "coding_update")
  if (!result %in% res) {
    stop("'result' must be one of: ", paste0(res, collapse = ", "), call. = FALSE)
  }
  args <- ct(list(query = query, display = 'xml', result = result,
                  length = limit, offset = offset, max = max))
  xml <- eparse(cont(enar_GET(path = 'warehouse/search', args, ...)), "xml")
  switch(
    result,
    sequence_release = set_names(xml, parse_sequence_release),
    assembly = set_names(xml, parse_assembly),
    coding_release = set_names(xml, parse_coding_release),
    coding_update = set_names(xml, parse_coding_release)
  )
}

set_names <- function(x, func) {
  stats::setNames(
    lapply(xml_children(x), func),
    vapply(xml_children(x), xml_attr, "", attr = "accession")
  )
}

parse_sequence_release <- function(z) {
  list(
    atts = as.list(xml_attrs(z)),
    desc = single2txt(z, "description"),
    refs = {
      dplyr::bind_rows(lapply(xml_find_all(z, "reference"), function(w) {
        tt <- unlist(as_list(w), FALSE)
        tibble::as_data_frame(c(
          tt[names(tt) != "author"],
          author = paste0(unlist(unname(tt[names(tt) == "author"])), collapse = "; ")
        ))
      }))
    },
    xref = as.list(xml_attrs(xml_find_first(z, "xref"))),
    lineage = unname(unlist(xml_attrs(xml_find_all(z, "feature//taxon//lineage//taxon")))),
    dloop_location = xml_attr(xml_find_first(z, "feature[@name=\"D-loop\"]"), "location"),
    seq = single2txt(z, "sequence")
  )
}

parse_coding_release <- function(z) {
  list(
    atts = as.list(xml_attrs(z)),
    desc = single2txt(z, "description"),
    refs = {
      dplyr::bind_rows(lapply(xml_find_all(z, "reference"), function(w) {
        tt <- unlist(as_list(w), FALSE)
        tibble::as_data_frame(c(
          tt[names(tt) != "author"],
          author = paste0(unlist(unname(tt[names(tt) == "author"])), collapse = "; ")
        ))
      }))
    },
    xref = lapply(xml_find_all(z, "xref"), function(z) as.list(xml_attrs(z))),
    lineage = unname(unlist(xml_attrs(xml_find_all(z, "feature//taxon//lineage//taxon")))),
    features = {
      tmp <- xml_find_first(z, "feature[@name=\"CDS\"]")
      list(
        attributes = as.list(xml_attrs(tmp)),
        xrefs = dplyr::bind_rows(
          lapply(xml_find_all(tmp, "xref"), function(z) tibble::as_data_frame(as.list(xml_attrs(z))))
        ),
        qualifiers = as.list(stats::setNames(xml_text(xml_find_all(tmp, "qualifier//value")), xml_attr(xml_find_all(tmp, "qualifier"), "name")))
      )
    },
    seq = single2txt(z, "sequence")
  )
}

parse_assembly <- function(z) {
  list(
    atts = as.list(xml_attrs(z)),
    identifiers = children2dat(xml_find_first(z, "IDENTIFIERS")),
    title = single2txt(z, "TITLE"),
    description = single2txt(z, "DESCRIPTION"),
    name = single2txt(z, "NAME"),
    assembly_level = single2txt(z, "ASSEMBLY_LEVEL"),
    genome_representation = single2txt(z, "GENOME_REPRESENTATION"),
    taxon = children2dat(xml_find_first(z, "TAXON")),
    study_ref = children2dat(xml_find_first(z, "STUDY_REF//IDENTIFIERS")),
    chromosomes = {
      dplyr::bind_rows(
        lapply(xml_children(xml_find_first(z, "CHROMOSOMES")), function(b) {
          tibble::data_frame(
            accession = xml_attr(b, "accession"),
            name = single2txt(b, "NAME"),
            type = single2txt(b, "TYPE")
          )
        })
      )
    },
    assembly_links = {
      dplyr::bind_rows(
        lapply(xml_find_all(z, "ASSEMBLY_LINKS//ASSEMBLY_LINK//URL_LINK"), as_list_tbl)
      )
    },
    assembly_attributes = {
      dplyr::bind_rows(
        lapply(xml_find_all(z, "ASSEMBLY_ATTRIBUTES//ASSEMBLY_ATTRIBUTE"), as_list_tbl)
      )
    }
  )
}

children2dat <- function(x) {
  sapply(xml_children(x), function(y) {
    as.list(stats::setNames(xml_name(y), xml_text(y)))
  })
}

single2txt <- function(x, name) {
  xml_text(xml_find_first(x, name))
}

as_list_ <- function(x) {
  unlist(as_list(x), FALSE)
}

as_list_tbl <- function(x) {
  tibble::as_data_frame(unlist(as_list(x), FALSE))
}

