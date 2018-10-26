#' Build database
#'
#' @param aa3_combine aa3 Data
#'
#' @return 2 data frame constitués des données de calibration dans le premier et
#' des données pour les échantillons dans le second.
#' @export
#'
#' @import lubridate
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom stats na.omit
#' @importFrom tidyr gather
#'
#' @examples
#' # convert_aa3("inst/extra_data/181018E.TXT",
#' # "inst/extra_data/181018E.xlsx") -> aa3_combine
#' # plot_aa3(aa3_combine)
#' # new_calb_aa3(aa3_combine, filter = list(Ptot = c(50,25), Ntot = c(0.1, 0.5)))
#' # build_db_aa3(aa3_combine)
#'
build_db_aa3 <- function(aa3_combine){

  # Check_1 : aa3 class
  if ( !("aa3" %in% class(aa3_combine)) ) {
    stop("class is not aa3")
  }

  # Attributes
  attribute_list <- attributes(aa3_combine)

  # CALB_db_list
  calb_db_list <- list()

  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                         pattern = "values")] -> values
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                         pattern = "std")] -> std
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                         pattern = "conc")] -> conc

  for (i in seq_along(values)) {

    nutrient <- c(std[i], values[i])

    aa3_combine[aa3_combine$sample_type == "CALB",  ] %>.%
      dplyr::select(., sample_type, date_time, nutrient) %>.%
      stats::na.omit(.) -> calb_data

    calb_data %>.%
      dplyr::mutate(., id = paste(lubridate::date(calb_data$date_time),
                                  stringr::str_split(values[i],
                                                     pattern = "_")[[1]][1],
                                  calb_data[[std[i]]], sep = "_"),
                    project = attr(aa3_combine,
                                   which = "metadata")$sample) -> calb_db
    calb_db %>.%
      tidyr::gather(., key = "std_type", value = "concentration", 3) -> calb_db

    calb_db %>.%
      dplyr::mutate(., std_type = stringr::str_split(calb_db$std_type,
                                                     pattern = "_")[[1]][1],
                    units_ = attr(aa3_combine,
                                  which = "method")$unit[[i]]) -> calb_db

    names(calb_db)[3] <- "value"

    calb_db %>.%
      dplyr::select(., id, date_time, project, std_type, value,
                    concentration, units_ ) -> calb_db

    calb_db_list[[i]] <- calb_db
  }

  # Create CALB_DB
  calb_db <- dplyr::bind_rows(calb_db_list)
  calb_db %>.%
    dplyr::filter(., value != "NA") -> calb_db

  # Attributes calb_db
  attr(calb_db, "class") <- c("aa3", "data.frame")
  attr(calb_db, "method") <- attribute_list$method
  attr(calb_db, "metadata") <- attribute_list$metadata
  attr(calb_db, "lm_df") <- attribute_list$lm_df

  # Create SAMP_DB

  # SAMP data
  aa3_combine[aa3_combine$sample_type == "SAMP",] %>.%
    dplyr::mutate(., filename = attr(aa3_combine,
                                     which = "metadata")$sample) -> samp_db

  paste(stringr::str_split(attr(aa3_combine, which = "metadata")$sample,
                           pattern = "-")[[1]][2], "filename", sep = "_") -> filename

  names(samp_db)[length(samp_db)] <- filename

  # Identify all nutrient values for select
  nutrient = sort(c(conc, values))
  nutrient_old = paste(nutrient, "old", sep = "_")

  for (i in nutrient_old) {
    if (i %in% names(samp_db)) {
      nutrient <- c(nutrient, i)
    }
  }

  # select sampdb
  samp_db %>.%
    dplyr::select(., sample_id, sample, sample_date, nutrient, project,
                  filename, date_time, authors, comment) ->  samp_db

  # attributes samp_db
  attr(samp_db, "class") <- c("aa3", "data.frame")
  attr(samp_db, "method") <- attribute_list$method
  attr(samp_db, "metadata") <- attribute_list$metadata
  attr(samp_db, "lm_df") <- attribute_list$lm_df

  # INFO
  if ("lm_df" %in% names(attribute_list)) {
    data.frame(attribute_list$metadata, method = attribute_list$method,
               lm = attribute_list$lm_df) %>.%
      dplyr::mutate(., id = paste(rownames(.), sample, sep = "_")) -> info_db
  } else {
    data.frame(attribute_list$metadata, method = attribute_list$method) %>.%
      dplyr::mutate(., id = paste(rownames(.), sample, sep = "_")) -> info_db
  }

  # RETURN
  database <- list(calb_db = calb_db, samp_db = samp_db, info_db = info_db)
  return(database)

  }
