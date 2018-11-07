
build_calbdb_aa3 <- function(obj) {

  # Attributes
  attribute_list <- attributes(obj)

  #### CALB DATABASE ####

  # values, std and conc names vectors
  names(obj)[stringr::str_detect(names(obj),
                                 pattern = "values$")] -> values
  names(obj)[stringr::str_detect(names(obj),
                                 pattern = "std$")] -> std
  names(aa3_combine)[stringr::str_detect(names(obj),
                                         pattern = "conc$")] -> conc

  #### PRIM, DRIFT ####

  obj %>.%
    dplyr::filter(., sample_type %in% c("PRIM", "DRIF")) %>.%
    dplyr::mutate(., filename = attr(obj,
                                     which = "metadata")$sample) -> prim_drift_all

  # Change sample_id
  stringr::str_locate(prim_drift_all$sample_id,
                      pattern = "\\d{1}(.)\\d{1}") -> locate
  which(stringr::str_detect(prim_drift_all$sample_id,
                            pattern = "\\d{1}(.)\\d{1}")) -> drift_change
  for (i in drift_change){
    stringr::str_sub(prim_drift_all$sample_id[i],
                     start = locate[i,1] + 1, end = locate[i,2] - 1) <- "-"
  }

  stringr::str_remove_all(prim_drift_all$sample_id,
                          pattern = " ") -> prim_drift_all$sample_id

  # PRIM - DRIFT database
  prim_drift_all %>.%
    dplyr::select(., sample_id, peak_number, sample_type,
                  date_time, filename, values) %>.%
    tidyr::gather(., key = "std_type", value = "value", values) -> prim_drift

  stringr::str_split(prim_drift$std_type, patter = "_") %>.%
    lapply(., `[[`, 1) -> id_nutrient

  prim_drift %>.%
    dplyr::mutate(., id = paste(lubridate::date(prim_drift$date_time),
                                id_nutrient,
                                sample_id,
                                peak_number,
                                sep = "_"),
                  std_type = as.character(id_nutrient)) %>.%
    dplyr::select(., -sample_id) -> prim_drift

  # Add concentration to PRIM - DRIFT dataset
  prim_drift_all %>.%
    dplyr::select(., sample_id, date_time, peak_number, conc) %>.%
    tidyr::gather(., key = "std_type",
                  value = "concentration", conc) -> prim_drift_conc

  prim_drift_conc %>.%
    dplyr::mutate(., id = paste(lubridate::date(prim_drift$date_time),
                                id_nutrient,
                                sample_id,
                                peak_number,
                                sep = "_")) %>.%
    dplyr::select(., id, concentration) -> prim_drift_conc

  prim_drift %>.%
    dplyr::inner_join(., prim_drift_conc, by = "id")  %>.%
    dplyr::arrange(., id, date_time, sample_type, filename, std_type, value,
                   concentration) -> prim_drift_db

  remove(prim_drift_all, prim_drift_conc, id_nutrient,
         locate, drift_change, i, prim_drift )

  #### CALB ####

  # CALB_db_list
  calb_db_list <- list()

  # nutient names
  attr(obj, which = "method")$method -> nutri_names

  for (i in seq_along(nutri_names)){
    # row number
    row_num <- which(attr(obj, which = "calb_lm") == nutri_names[i])

    # Filter_conc ?
    if ("filter_conc" %in% names(attr(obj, which = "calb_lm")) &
        !is.null(attr(obj, which = "calb_lm")$filter_conc[[row_num]])) {

      attr(obj, which = "calb_lm")$filter_conc[[i]] -> filter

      c(paste(std[i],"old", sep = "_"),
        paste(values[i], "old", sep = "_")) -> nutrient

    } else {

      c(std[i], values[i]) -> nutrient
      filter <- NULL

    }

    obj[obj$sample_type == "CALB",  ] %>.%
      dplyr::select(., sample_type, date_time, nutrient) %>.%
      stats::na.omit(.) -> calb_data

    calb_data %>.%
      dplyr::mutate(., id = paste(lubridate::date(calb_data$date_time),
                                  nutri_names[i],
                                  calb_data[[nutrient[1]]], sep = "_"),
                    filename = attr(obj,
                                    which = "metadata")$sample)  %>.%
      tidyr::gather(., key = "std_type", value = "concentration", 3) -> calb_db

    calb_db %>.%
      dplyr::mutate(., std_type = stringr::str_split(calb_db$std_type,
                                                     pattern = "_")[[1]][1]) -> calb_db

    names(calb_db)[3] <- "value"

    calb_db$std_type[calb_db$concentration %in% filter] <- paste(nutri_names[i],
                                                                 "old",
                                                                 sep = "_")

    calb_db %>.%
      dplyr::select(., id, date_time, sample_type, filename, std_type, value,
                    concentration) -> calb_db

    calb_db_list[[i]] <- calb_db
  }

  #### CALB_DB ####

  calb_db <- dplyr::bind_rows(calb_db_list)
  dplyr::filter(calb_db, value != "NA") -> calb_db
  dplyr::bind_rows(calb_db, prim_drift_db) -> calb_db

  # Attributes calb_db
  attr(calb_db, "class") <- "data.frame"
  attr(calb_db, "method") <- attribute_list$method
  attr(calb_db, "calb_lm") <- attribute_list$calb_lm
  attr(calb_db, "calb_lm_old") <- attribute_list$calb_lm_old
  attr(calb_db, "metadata") <- attribute_list$metadata

  return(calb_db)

}

build_sampdb_aa3 <- function(obj) {

  # Attributes
  attribute_list <- attributes(obj)

  obj[obj$sample_type == "SAMP",] %>.%
    dplyr::mutate(., filename = attr(obj,
                                     which = "metadata")$sample) -> samp_db

  paste(stringr::str_split(attr(obj, which = "metadata")$sample,
                           pattern = "-")[[1]][2],
        "filename", sep = "_") -> filename

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

  # Attributes calb_db
  attr(samp_db, "class") <- "data.frame"
  attr(samp_db, "method") <- attribute_list$method
  attr(samp_db, "calb_lm") <- attribute_list$calb_lm
  attr(samp_db, "calb_lm_old") <- attribute_list$calb_lm_old
  attr(samp_db, "metadata") <- attribute_list$metadata

  return(samp_db)
}

build_metadb_aa3 <- function(obj) {

  if ("metadata" %in% names(attributes(obj))) {
    if (is.null(attr(obj, which = "metadata")$topic)) {
      dplyr::bind_rows(attributes(obj)$metadata[1:6]) %>.%
        dplyr::mutate(., topic = "NULL") %>.%
        as.data.frame(.) -> df
    } else {
      dplyr::bind_rows(attributes(obj)$metadata) %>.%
        as.data.frame(.) -> df
    }
  }

  names(df)[2] <- "filename"
  df %>.%
    dplyr::mutate(., id = paste(date, project, sep = "_")) %>.%
    dplyr::select(., id, project, date, filename, author,
                  comment, topic, -sample_date) -> df

  df_length <- length(df)

  paste(rep(rownames(attr(obj, which = "method")), 5),
        names(attr(obj, which = "method")), sep = "_") %>.%
    sort(.) -> cnames

  stringr::str_split(cnames, pattern = "_") -> elements

  for (i in seq_along(cnames)){

    which(rownames(attr(obj,
                        which = "method")) == paste(elements[[i]][[1]],
                                                    elements[[i]][[2]],
                                                    sep = "_")) -> rownum

    cbind(df, attr(obj,which = "method")[rownum, elements[[i]][[3]]]) -> df
    names(df)[i + df_length] <- cnames[i]
  }

  return(df)
}

build_database_aa3 <- function(obj){

  # Check_1 : aa3 class
  if ( !("aa3" %in% class(obj)) ) {
    stop("class is not aa3")
  }

  # calb database
  calb_db <- build_calbdb_aa3(obj)
  print(calb_db)

  # samp database
  samp_db <- build_sampdb_aa3(obj)
  print(samp_db)

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

  # Metadata/Method database
  meta_db <- build_metadb_aa3(obj)
  print(meta_db)


}


