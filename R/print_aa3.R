
print.aa3 <- function(obj){
  # DATA
  cat("An EcoNumData object with :", "\n")
  # print(dplyr::as_tibble(obj))
  dplyr::glimpse(obj)

  # METADATA
  if ("metadata" %in% names(attributes(obj))) {
    cat("\n", "Metadata : ", "\n")
    if (is.null(attr(obj, which = "metadata")$topic)) {
      dplyr::bind_rows(attributes(obj)$metadata[1:6]) %>.%
        mutate(., topic = "NULL") %>.%
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
