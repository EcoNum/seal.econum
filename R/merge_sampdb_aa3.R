#' merge_sampdb_aa3
#'
#' @param sampdb_org calb_aa3$sampdb for organic
#' @param sampdb_inorg calb_aa3$sampdb for inorganic
#'
#' @return un dataframe avec les donnees combinées des analyses organiques et
#' inorganiques ("Ptot_conc", "Ntot_conc", "PO4_conc", "NOx_conc", "NO2_conc",
#' "NO3_conc", "NH4_conc")
#'
#' @export
#'
#' @import lubridate
#' @import stringr
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' # TO DO
#'
merge_sampdb_aa3 <- function(sampdb_org, sampdb_inorg) {

  # Identify all nutrient values
  nutrient = c("Ptot_conc", "Ntot_conc", "PO4_conc", "NOx_conc",
               "NO2_conc", "NO3_conc", "NH4_conc")
  nutrient_old = paste(nutrient, "old", sep = "_")

  for (i in nutrient_new) {
    if (i %in% c(names(sampdb_inorg), names(sampdb_org))) {
      nutrient <- c(nutrient, i)
    }
  }

  sort(nutrient) -> nutrient

  # JOIN sampdb_org and sampdb_inorg
  sampdb_org %>.%
    dplyr::left_join(., sampdb_inorg, by = "sample_id") %>.%
    # Calcul NO3_conc
    dplyr::select(., NO3_conc = NOx_conc - NO2_conc,
           calb_orga_date = lubridate::date(date_time.x),
           calb_inorga_date = lubridate::date(date_time.y)) %>.%
    dplyr::select(., sample.x, sample_date.x, nutrient, sample_id, project.x,
           calb_orga_date, orga_filename, calb_inorga_date,
           inorga_filename, authors.x, comment.x) -> samp_org_inorg

  # Rename df colname
  names(samp_org_inorg) <- stringr::str_replace(names(samp_org_inorg),
                                                pattern = "\\.x",
                                                replacement = "")

  return(samp_org_inorg)
}
