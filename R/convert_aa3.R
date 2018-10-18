#' Conversion of aa3 file txt and excell
#'
#' @param file_aa3_txt txt file
#' @param file_aa3_xlsx xlsx file
#'
#' @return un objet converti
#' @import stringr
#' @import readr
#' @import lubridate
#' @import readxl
#' @export
#'
#' @examples
#'
#' #TODO
#'
convert_aa3 <- function(file_aa3_txt, file_aa3_xlsx){
  # Import metadata and extract informaation
  header_read <- readr::read_lines(file_aa3_txt, n_max = 13,
                                   locale = readr::locale(encoding = "LATIN1"))
  header <- stringr::str_extract_all(header_read,
                                     "(-?µ?\\w+:?\\.?-?/?\\w*:?/?\\d*)")

  # Rename list elements
  sapply(header, `[[`, 1) -> names(header)

  # paste Comment
  if (lengths(header["COMM"]) > 2) {
    header$COMM[2] <- paste(header$COMM[-1], collapse = " ")
    header$COMM <- header$COMM[1:2]
  }

  # Check_1
  header_control <- c("ANAL", "RUN", "DATE", "TIME", "OPER", "COMM", "TYPE",
                      "CHAN", "METH", "UNIT", "Base", "Gain", "Lamp")
  length_list_element <- c(2,2,2,2,2,2,4,4,4,4,4,4,4)

  if (sum(names(header) != header_control) != 0 |
      sum(lengths(header) != length_list_element) != 0) {
    stop("attention : erreur durant l importation ou l extraction des metadonnees")
  }

  # Change NO3 in NOx
  stringr::str_replace_all(header$METH, "NO3", "NOx") -> header$METH

  # Extract nutrients that are analysed
  results <- header$METH[-1]

  # Prepare names of variables for raw_data
  stds <- paste(results, "std", sep = "_")
  concs <- paste(results, "conc", sep = "_")
  vals <- paste(results, "values", sep = "_")

  # Change in data NPinorganique.ANL in inorganique
  header$ANAL[2][header$ANAL[2]  == "NPinorganique.ANL" ] <- "inorga"
  header$ANAL[2][header$ANAL[2]  == "NPorganique.ANL" ] <- "orga"

  # Create a several list with the method information
  method <- list()
  for (i in 2:4) {
    method[[i - 1]] <- list(method = header$METH[i],
                            unit = header$UNIT[i],
                            base = header$Base[i],
                            gain = header$Gain[i],
                            lamp = header$Lamp[i])
  }
  names(method) <- c("channel_1","channel_2","channel_3")

  # Create a list with the meta information
  meta <- list(sample = paste(sub("\\.RUN$", "", header$RUN[2]),
                              header$ANAL[2],sep = "-"),
               sample_date = lubridate::dmy_hms(paste(header$DATE[2],
                                                      header$TIME[2])),
               author = header$OPER[2],
               date = lubridate::dmy(header$DATE[2]),
               comment = header$COMM[2])

  # Extract raw data
  raw_data <- readr::read_delim(file = file_aa3_txt, delim = ";", skip = 13)

  # Select variables in raw_data
  raw_data <- raw_data[ , -c(3, 5:7, 18, 19)]

  # Rename the columns in raw_data
  names(raw_data) <- c("sample_id", "peak_number", "sample_type",
                       "date_time", stds[1], concs[1], vals[1], stds[2],
                       concs[2], vals[2], stds[3], concs[3], vals[3])

  # recoding type of variable
  raw_data$sample_type <- as.factor(raw_data$sample_type)
  raw_data$date_time <- lubridate::dmy_hms(raw_data$date_time)

  # Add units informations
  for (i in 1:3) {
    attr(raw_data[[stds[i]]], "units") <- method[[i]]$unit
    attr(raw_data[[concs[i]]], "units") <- method[[i]]$unit
  }

  # import xlsx file
  add_data <- readxl::read_xlsx(file_aa3_xlsx, sheet = "data")

  raw_data <- dplyr::left_join(raw_data, add_data, by = "sample_id")

  # SAMP contient des valeurs manquantes
  for (i in which(raw_data$sample_type == "SAMP")) {
    if (is.na(raw_data$project[i]) == TRUE) {
      stop("Attention : Presence de valeurs manquantes dans la colonnes project, le fichier xlsx et txt ne correspondent pas entièrement.")
    }
  }
  attr(raw_data, "method") <- method
  attr(raw_data, "metadata") <- meta
  return(raw_data)
}
