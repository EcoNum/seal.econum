#' calb_aa3 function
#'
#' @param aa3_combine aa3 Data
#' @param filter_list liste de concentration a supprimer pour recalculer
#' la regression. ( ex : list(Ptot = c(1,2)) -> filter )
#'
#' @return un dataframe
#' @export
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom chart chart
#'
#' @examples
#' # aa3_combine <- convert_aa3("inst/extra_data/181018E.TXT", "inst/extra_data/181018E.xlsx")
#' # filter_list <- list(Ptot =  c(50, 25), Ntot = c(0.1, 0.5, 1, 2))
#' # plot_aa3(aa3_combine)
#' # calb_aa3(aa3_combine, filter_list)
#'
calb_aa3 <- function(aa3_combine, filter_list) {

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

  # values, std and conc vectors for nutrient
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                          pattern = "values")] -> values
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                          pattern = "std")] -> std
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                          pattern = "conc")] -> conc

  # Output list
  lm_list <- list()

  # attribute_list
  attribute_list <- attributes(aa3_combine)

  # unit vect
  unit_v <- stringr::str_detect(attr(aa3_combine, which = "method")$unit,
                                   "\u03BCmol/L")
  unit_vect <- c()
  unit_vect[unit_v] <- "\u03BCmol/L"
  unit_vect[!unit_v] <- attr(aa3_combine, which = "method")$unit[!unit_v]

  for (i in 1:length(values)) {

    # nutrient name
    nutri_name <-  stringr::str_split(values[i], pattern = "_")[[1]][1]

    # IF ... nutrient in filter_list
    if (nutri_name %in% names(filter_list)) {

      # Check conc_list
      if ( sum(!(filter_list[[nutri_name]] %in% aa3_combine[,std[i]])) != 0 ) {
        stop("Attention : concentration non valide")
      }

      # Replace values by NA
      aa3_combine[which(aa3_combine[,std[i]] %in% filter_list[[nutri_name]] &
                          aa3_combine$sample_type == "CALB"),
                  c(std[i], conc[i], values[i]) ] <- NA

      # Calb_data
      aa3_combine[aa3_combine$sample_type == "CALB", c(std[i], values[i])] %>.%
        stats::na.omit(.) -> calb

      # Check n(std)
      if ( length(calb[[std[i]]]) <= 3) {
        stop("Attention seulement 3 points pour le calcul de la regression")
      }

      # lm
      lm_mod <- stats::lm(calb, formula = stats::as.formula(paste(values[i] ,
                                                                  "~", std[i])))

      data.frame(std_name = paste(nutri_name, "new", sep = "_"),
                 intercept = lm_mod$coefficients[[1]],
                 values = lm_mod$coefficients[[2]],
                 r_squared = round(summary(lm_mod)$r.squared,digits = 4),
                 n = length(calb[[std[i]]]),
                 filter_conc = I(filter_list[nutri_name])
                 ) ->  lm_list[[i]]

      names(lm_list)[i] <- paste(nutri_name)

      # Equation
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                       list(a = format(lm_mod$coefficients[[1]], digits = 2),
                            b = format(lm_mod$coefficients[[2]], digits = 2),
                            r2 = format(summary(lm_mod)$r.squared, digits = 3)))
      eq <- as.character(as.expression(eq))

      # lm graph
      chart::chart(calb,
                   formula = stats::as.formula(paste(values[i] ,"~", std[i])),
                   na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        geom_smooth(method = "lm", na.rm = TRUE) +
        ggtitle(stringr::str_split(values[i], pattern = "_")[[1]][1]) +
        geom_text(x = 2*(diff(range(calb[[std[i]]], na.rm = TRUE)))/5 ,
                  y = max( calb[[values[i]]] ),
                  label = eq, parse = TRUE) +
        xlab(paste0(nutri_name," [",unit_vect[i], "]")) +
        ggrepel::geom_text_repel(aes(label = calb[[std[i]]]), na.rm = TRUE,
                                 nudge_y = 1.5, nudge_x = 1.5,
                                 direction = "both",
                                 segment.size = 0.2) -> graphe
      print(graphe)

      # add new nutrient values
      cnum <- which(names(aa3_combine) == conc[i])
      names(aa3_combine)[cnum] <- paste(conc[i], "old", sep = "_")
      aa3_combine %>.%
        dplyr::mutate(., new = round((aa3_combine[,values[i]] -
                                        lm_mod$coefficients[[1]]) /
                                          lm_mod$coefficients[[2]],3)
                      ) -> aa3_combine

      names(aa3_combine)[length(aa3_combine)] <- paste(conc[i])

    } else {

      # lm ... IF no filter
      # Check n(std)
      if (sum( !is.na(aa3_combine[aa3_combine$sample_type == "CALB",
                                  std[i]]) ) <= 3) {
        stop("Attention seulement 3 points pour le calcul de la regression")
      }

      lm_mod <- stats::lm(aa3_combine,
                          formula = stats::as.formula(paste(values[i],"~",std[i])))

      data.frame(std_name = paste(nutri_name),
                 intercept = lm_mod$coefficients[[1]],
                 values = lm_mod$coefficients[[2]],
                 r_squared = round(summary(lm_mod)$r.squared,digits = 3),
                 n = dplyr::filter(aa3_combine, sample_type == "CALB") %>.%
                   dplyr::select(., std[i]) %>.%
                   sum(!is.na(.))
                 ) ->  lm_list[[i]]

      names(lm_list)[i] <- paste(nutri_name)

    }

  }

  lm_df <- dplyr::bind_rows(lm_list)

  print(lm_df)

  attr(aa3_combine, "class") <- c("aa3", "data.frame")
  attr(aa3_combine, "method") <- attribute_list$method
  attr(aa3_combine, "metadata") <- attribute_list$metadata
  attr(aa3_combine, "lm_df") <- lm_df

  return(aa3_combine)
}


