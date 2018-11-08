#' Print aa3 object
#'
#' @param obj aa3 object
#'
#' @return La fonction imprime la structure, les metadonnees, les methodes
#' utilisees pour les mesures et les parametres des regressions lineaires de
#' l'objet aa3 fourni.
#'
#' @export
#' @importFrom dplyr glimpse
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom visdat vis_dat
#'
#' @examples
#' # aa3_combine <- convert_aa3("inst/extra_data/181018E.TXT", "inst/extra_data/181018E.xlsx", project = "test")
#' # print(aa3_combine)
#'

print.aa3 <- function(obj){
  # DATA
  cat("\n","An EcoNumData object with :", "\n")
  dplyr::glimpse(obj)
  print(visdat::vis_dat(obj))

  # METADATA
  if ("metadata" %in% names(attributes(obj))) {
    cat("\n", "Metadata : ", "\n")
    if (is.null(attr(obj, which = "metadata")$topic)) {
      dplyr::bind_rows(attributes(obj)$metadata[1:6]) %>.%
        dplyr::mutate(., topic = "NULL") %>.%
        as.data.frame(.) %>.%
        print(.)
    } else {
      dplyr::bind_rows(attributes(obj)$metadata) %>.%
        as.data.frame(.) %>.%
        print(.)
    }
  }

  # METHODE
  if ("method" %in% names(attributes(obj))) {
    if (stringr::str_split(attr(obj, which = "metadata")$sample,
                           pattern = "-")[[1]][2] == "orga") {
      cat("\n", "Use organic method :", "\n")
    } else if (stringr::str_split(attr(obj, which = "metadata")$sample,
                             pattern = "-")[[1]][2] == "inorga") {
      cat("\n", "Use inorganic method :", "\n")
    }

    print(attr(obj, which = "method"))
  }

  # LINEAR MODEL
  if ("calb_lm" %in% names(attributes(obj))) {
    cat("\n", "Linear Model parameters : ", "\n")
    if ("calb_lm_old" %in% names(attributes(obj))) {
      attr(obj,
           which = "calb_lm_old")[stringr::str_detect(attr(obj,
                                                       which = "calb_lm_old")$std_name,
                                                  pattern = "old"),] -> x

      dplyr::bind_rows(attr(obj, which = "calb_lm"), x)

    } else {
      print(attr(obj, which = "calb_lm" ))
    }
  }

}
