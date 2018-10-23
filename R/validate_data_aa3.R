#' Validate_data_aa3
#'
#' @param aa3_combine aa3 Data
#' @param filter_list liste de concentration a supprimer pour recalculer
#' la régression. ( ex : list(Ptot = c(1,2)) -> filter )
#'
#' @return Une liste contenant un dataframe avec les donnees CALB,
#' une liste de dataframe avec les paramètres des regressions et
#' un dataframe avec les donnees SAMP eventuellement recalculee. Affiche
#' egalement les graphes des nouvelles courbes de calibration.
#'
#' @export
#'
#' @import flow
#' @import stringr
#' @import stats
#' @import ggplot2
#' @import chart
#' @import lubridate
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom tidyr gather
#' @importFrom broom tidy
#' @importFrom broom glance
#' @importFrom ggrepel geom_text_repel
#'
#' @examples
#' #TODO
#'
validate_data_aa3 <- function(aa3_combine, filter_list = NULL) {

  # Check_1 : aa3 class
  if ( !("aa3" %in% class(aa3_combine)) ) {
    stop("class is not aa3")
  }

  # Check_2 : filter_list
  nutrient_ctrl <- c("Ptot", "NO2", "NOx", "Ntot", "NH4", "PO4")

  if ( !is.null(filter_list) ) {
    if ( sum(!(names(filter_list) %in% nutrient_ctrl)) != 0 ) {
      stop("Attention : pas de noms pour les elements de la liste ou pas de
           correspondance, utiliser un ou plusieurs des noms suivants :
           'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
  }
  }

  # output list
  calb_db_list <- list()
  lm_list <- list()

  #
  (names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                 pattern = "values")] -> values)
  (names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                 pattern = "std")] -> std)
  (names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                 pattern = "conc")] -> conc)
  attr(aa3_combine, which = "metadata")$sample -> samp_name

  # SAMP data
  aa3_combine[aa3_combine$sample_type == "SAMP",] %>.%
    dplyr::mutate(., filename = attr(aa3_combine,
                                     which = "metadata")$sample) -> samp_db

  paste(stringr::str_split(attr(aa3_combine, which = "metadata")$sample,
                           pattern = "-")[[1]][2], "filename", sep = "_") -> filename

  names(samp_db)[length(samp_db)] <- filename

  # CALB DATA & Calc new conc

  for (i in 1:length(values)) {

    aa3_combine[aa3_combine$sample_type == "CALB",  ] %>.%
      dplyr::select(., sample_type, date_time, std[i], values[i]) %>.%
      stats::na.omit(.) -> calb_data

    if (stringr::str_split(values[i], pattern = "_")[[1]][1] %in%
          names(filter_list)) {

      filter_num <- which(names(filter_list) ==
                            stringr::str_split(values[i], pattern = "_")[[1]][1])

      # Check conc_list
      if ( sum(!(filter_list[[filter_num]] %in% calb_data[[3]])) != 0 ){
        stop("Attention : concentration non valide")
      }

      calb_data[which(calb_data[[3]] %in% filter_list[[filter_num]]),
                c(values[i], std[i])] <- NA

      # lm

      if (sum(!is.na(calb_data[,4])) <= 3) {
        stop("Attention seulement 3 points pour le calcul de la regression")
      }

      lm(calb_data, formula = as.formula(paste(values[i] ,"~", std[i]))) -> lm_mod
      broom::tidy(lm_mod) -> lm_para
      broom::glance(lm_mod) -> lm_mod_stat
      lm_para$n <- sum(!is.na(calb_data[,4]))

      lm_list[[i]] <- list(param = lm_para, mod_stat = lm_mod_stat)
      names(lm_list)[i] <- paste(stringr::str_split(values[i],
                                                    pattern = "_")[[1]][1])

      # Equation
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                       list(a = format(lm_para$estimate[1], digits = 2),
                            b = format(lm_para$estimate[2], digits = 2),
                            r2 = format(lm_mod_stat$r.squared, digits = 3)))
      eq <- as.character(as.expression(eq))

      chart::chart(calb_data,
                   formula = as.formula(paste(values[i] ,"~", std[i])),
                   na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        geom_smooth(method = "lm", na.rm = TRUE) +
        ggtitle(stringr::str_split(values[i], pattern = "_")[[1]][1]) +
        geom_text(x = 2*(diff(range(calb_data[,3], na.rm = TRUE)))/5 ,
                  y = max(calb_data[,4], na.rm = TRUE),
                  label = eq, parse = TRUE) +
        ggrepel::geom_text_repel(aes(label = calb_data[,3]), na.rm = TRUE,
                                 nudge_y = 1.5, nudge_x = 1.5, direction = "both",
                                 segment.size = 0.2) -> graphe

      print(graphe)

      # add new nutrient values
      cnum <- which(names(samp_db) == conc[i])
      names(samp_db)[cnum] <- paste(conc[i], "old", sep = "_")
      samp_db %>.%
        dplyr::mutate(., new = round((samp_db[,paste(values[i])] -
                                        lm_para$estimate[1]) /
                                       lm_para$estimate[2],3)) -> samp_db

      names(samp_db)[length(samp_db)] <- paste(conc[i])


    } else {

      if (sum(!is.na(calb_data[,4])) <= 3){
        stop("Attention seulement 3 points pour le calcul de la regression")
      }

      # lm
      lm(calb_data, formula = as.formula(paste(values[i] ,"~", std[i]))) -> lm_mod
      broom::tidy(lm_mod) -> lm_para
      broom::glance(lm_mod) -> lm_mod_stat
      lm_para$n <- sum(!is.na(calb_data[,4]))

      lm_list[[i]] <- list(param = lm_para, mod_stat = lm_mod_stat)
      names(lm_list)[i] <- paste(stringr::str_split(values[i],
                                                    pattern = "_")[[1]][1])
    }

    calb_data %>.%
      dplyr::mutate(., id_cal = paste(lubridate::date(calb_data$date_time),
                                      stringr::str_split(values[i],
                                                         pattern = "_")[[1]][1],
                                      calb_data[[std[i]]], sep = "_"),
                    date = lubridate::date(calb_data$date_time),
                    time = strftime(calb_data$date_time, format = "%H:%M:%S"),
                    project_id = attr(aa3_combine,
                                      which = "metadata")$sample) -> calb_db

    calb_db %>.%
      tidyr::gather(., key = "std_type", value = "concentration", 3) -> calb_db

    dplyr::mutate(calb_db, std_type = stringr::str_split(calb_db$std_type,
                                                           pattern = "_")[[1]][1],
                  units_ = attr(aa3_combine,
                                which = "method")[[i]]$unit ) -> calb_db

    names(calb_db)[3] <- "value"

    calb_db %>.%
      dplyr::select(., id_cal, project_id, date, time, std_type, units_,
                    concentration, value) -> calb_db_list[[i]]
  }

  calb_db <- dplyr::bind_rows(calb_db_list)
  calb_db %>.%
    dplyr::filter(., value != "NA") -> calb_db

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

  # valid_data list
  valid_data <- list(calb_db = calb_db,
                     regression = lm_list,
                     samp_db = samp_db)

  # attributes

  attr(valid_data$calb_db, "filter") <- filter_list

  attr(valid_data$samp_db, "filter") <- filter_list

  attr(valid_data$samp_db, "regression") <- lm_list

  return(valid_data)

}
