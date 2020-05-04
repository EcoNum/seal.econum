#' Print aa3 object
#'
#' @param x aa3 object
#' @param ... further arguments (not used)
#'
#' @return La fonction imprime la structure, les metadonnees, les methodes
#' utilisees pour les mesures et les parametres des regressions lineaires de
#' l'objet aa3 fourni.
#'
#' @export
#' @importFrom dplyr glimpse
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#'
#' @examples
#' #todo
print.aa3 <- function(x, ...) {
  # DATA
  cat("\n","An EcoNumData object with :", "\n")
  dplyr::glimpse(x)
  #print(visdat::vis_dat(x))

  # METADATA
  if ("metadata" %in% names(attributes(x))) {
    cat("\n", "Metadata : ", "\n")
    if (is.null(attr(x, "metadata")$topic)) {
      dplyr::bind_rows(attributes(x)$metadata[1:6]) %>.%
        dplyr::mutate(., topic = "NULL") %>.%
        as.data.frame(.) %>.%
        print(.)
    } else {
      dplyr::bind_rows(attributes(x)$metadata) %>.%
        as.data.frame(.) %>.%
        print(.)
    }
  }

  # METHODE
  if ("method" %in% names(attributes(x))) {
    if (strsplit(attr(x, "metadata")$sample, split = "-", fixed = TRUE)[[1]][2] == "orga") {
      cat("\n", "Use organic method :", "\n")
    } else if (strsplit(attr(x, "metadata")$sample, split = "-", fixed = TRUE)[[1]][2] == "inorga") {
      cat("\n", "Use inorganic method :", "\n")
    }
    print(attr(x, "method"))
  }

  # LINEAR MODEL
  if ("calb_lm" %in% names(attributes(x))) {
    cat("\n", "Linear Model parameters : ", "\n")
    if ("calb_lm_old" %in% names(attributes(x))) {
      x2 <- attr(x, "calb_lm_old")[grepl("old", attr(x, "calb_lm_old")$std_name),]
      dplyr::bind_rows(attr(x, "calb_lm"), x2)
    } else {
      print(attr(x, which = "calb_lm" ))
    }
  }
  invisible(x)
}
