
print.aa3 <- function(aa3){

  as_tibble(aa3)

  # METADATA
  if(is.null(attr(aa3, which = "metadata")$topic)){
    (dplyr::bind_rows(attributes(aa3)$metadata[1:6]) %>.%
      mutate(., topic = "NULL"))
  } else {
    (dplyr::bind_rows(attributes(aa3)$metadata))
  }

}
