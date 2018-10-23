aa3_combine <- convert_aa3("../protocol_seal_aa3/data/raw/181018E.TXT", "../protocol_seal_aa3/data/raw/181018E.xlsx")

filter_list <- list(Ptot = c(50, 25), Ntot = c(0.1, 0.5, 1), NO2 = 10)

filter_l = list(Ntot = c(0.1, 0.5, 1, 2, 5))

validate_data_aa3 <- function(aa3_combine, filter_list = NULL) {

  # Check_1 : aa3 class
  if ( !("aa3" %in% class(aa3_combine)) ) {
    stop("class is not aa3")
  }

  # Check_2 : filter_list
  nutrient_ctrl <- c("Ptot", "NO2", "NOx", "Ntot", "NH4", "PO4")

  if ( !is.null(filter_list) ) {
    if ( !(names(filter_list) %in% nutrient_ctrl) ) {
      stop("Attention : pas de noms pour les elements de la liste ou pas de
           correspondance, utiliser un ou plusieurs des noms suivants :
           'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
  }
  }

  # output list
  calb_db_list <- list()
  # graph_lm <- list()
  lm_list <- list()

  #
  (names(aa3_combine)[str_detect(names(aa3_combine),
                                 pattern = "values")] -> values)
  (names(aa3_combine)[str_detect(names(aa3_combine),
                                 pattern = "std")] -> std)
  (names(aa3_combine)[str_detect(names(aa3_combine),
                                 pattern = "conc")] -> conc)
  attr(aa3_combine, which = "metadata")$sample -> samp_name
  # j = 1

  # SAMP data

  aa3_combine[aa3_combine$sample_type == "SAMP",] %>.%
    mutate(., filename = attr(aa3_combine, which = "metadata")$sample) -> samp_df

  paste(stringr::str_split(attr(aa3_combine, which = "metadata")$sample, pattern = "-")[[1]][2],
        "filename", sep = "_") -> filename

  names(samp_df)[length(samp_df)] <- filename

  # CALB DATA & Calc new conc

  for (i in 1:length(values)) {

    aa3_combine[aa3_combine$sample_type == "CALB",  ] %>.%
      dplyr::select(., sample_type, date_time, std[i], values[i]) %>.%
      stats::na.omit(.) -> calb_data

    if (stringr::str_split(values[i], pattern = "_")[[1]][1] %in%
          names(filter_list)) {

      filter_num <- which(names(filter_list) ==
                            stringr::str_split(values[i], pattern = "_")[[1]][1])

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
                   formula = as.formula(paste(values[i] ,"~", std[i])), na.rm = TRUE) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(stringr::str_split(values[i], pattern = "_")[[1]][1]) +
        geom_text(x = 2*(diff(range(calb_data[,3], na.rm = TRUE)))/5 ,
                  y = max(calb_data[,4], na.rm = TRUE),
                  label = eq, parse = TRUE) +
        ggrepel::geom_text_repel(aes(label = calb_data[,3], na.rm = TRUE), nudge_y = 1.5,
                                 nudge_x = 1.5, direction = "both",
                                 segment.size = 0.2) -> graphe

      print(graphe)

      # graphe -> graph_lm[[j]]
      # names(graph_lm)[j] <- paste(values[i])
      # j+1 -> j

      # add new nutrient values
      cnum <- which(names(samp_df) == conc[i])
      names(samp_df)[cnum] <- paste(conc[i], "old", sep = "_")
      samp_df %>.%
        dplyr::mutate(., new = round((samp_df[,paste(values[i])] -
                                        lm_para$estimate[1]) /
                                       lm_para$estimate[2],3)) -> samp_df

      names(samp_df)[length(samp_df)] <- paste(conc[i])


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

  calb_db <- bind_rows(calb_db_list)
  calb_db %>.%
    dplyr::filter(., value != "NA") -> calb_db

  # Identify all nutrient values
  nutrient = sort(c(conc, values))
  nutrient_old = paste(nutrient, "old", sep = "_")

  for (i in nutrient_old) {
    if (i %in% names(samp_df)) {
      nutrient <- c(nutrient, i)
    }
  }

  samp_df %>.%
  dplyr::select(., sample_id, sample, sample_date, nutrient, project,
                   filename, date_time, authors, comment) ->  samp_df

  valid_data <- list(calb_db = calb_db,
                     regression = lm_list,
                     samp_db = samp_df)

  # if (!is.null(filter_list)) {
  #   class(valid_data) <- c("valid_aa3", "filter_aa3", "aa3", "data.frame")
  # } else {
  #   class(valid_data) <- c("valid_aa3", "aa3", "data.frame")
  # }

  return(valid_data)

}
