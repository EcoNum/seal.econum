#' calb_aa3 function
#'
#' @param obj aa3 Data
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
#' # todo
calb.aa3 <- function(obj, filter_list = NULL) {

  # Check_2 : filter_list
  if (!is.null(filter_list)) {
    if (sum(!(names(filter_list) %in% attr(obj, "method")$method)) != 0 ) {
      stop("Attention : pas de noms pour les elements de la liste ou pas de
           correspondance, utiliser un ou plusieurs des noms suivants :
           'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
    }
  }

  # Values, std and conc vectors for nutrient
  values <- names(obj)[stringr::str_detect(names(obj), pattern = "values")]
  std <- names(obj)[stringr::str_detect(names(obj), pattern = "std")]
  conc <- names(obj)[stringr::str_detect(names(obj), pattern = "conc")]
  stringr::str_split(values, pattern = "_") %>%
    sapply(`[[`, 1) ->
    nutri_name

  # Output list
  lm_list <- list()

  # attribute_list
  attribute_list <- attributes(obj)

  for (i in 1:length(values)) {
    if (nutri_name[i] %in% names(filter_list)) {

      # Check conc_list
      if (sum(!(filter_list[[nutri_name[i]]] %in% obj[,std[i]])) != 0)
        stop("concentration non valide")

      # Replace values by NA
      obj$old_val <- obj[[values[i]]]
      names(obj)[length(obj)] <- paste(values[i], "old", sep = "_")

      obj$old_std <- obj[[std[i]]]
      names(obj)[length(obj)] <- paste(std[i], "old", sep = "_")

      obj[which(obj[,std[i]] %in% filter_list[[nutri_name[i]]] &
        obj$sample_type == "CALB"), c(std[i], values[i]) ] <- NA

      # Calb_data
      obj[obj$sample_type == "CALB", c(std[i], values[i])] %>%
        stats::na.omit() ->
        calb

      # Check n(std)
      if ( length(calb[[std[i]]]) <= 3)
        stop("seulement 3 points pour le calcul de la regression")

      # lm
      lm_mod <- stats::lm(data = calb, stats::as.formula(paste(values[i] , "~", std[i])))

      lm_list[[i]] <- data.frame(
        std_name    = paste(nutri_name[i]),
        intercept   = lm_mod$coefficients[[1]],
        values      = lm_mod$coefficients[[2]],
        r_squared   = round(summary(lm_mod)$r.squared,digits = 4),
        n           = length(calb[[std[i]]]),
        filter_conc = I(filter_list[nutri_name[i]])
      )

      names(lm_list)[i] <- paste(nutri_name[i])

      # Add new nutrient values
      obj$old_conc <- obj[[conc[i]]]
      names(obj)[length(obj)] <- paste(conc[i], "old", sep = "_")

      obj[conc[i]] <- round((obj[,values[i]] - lm_mod$coefficients[[1]]) /
        lm_mod$coefficients[[2]], 3)

      attribute_list$calb_lm$std_name[i] <- paste(nutri_name[i], "old", sep = "_")

    } else {
      lm_list[[i]] <- attribute_list$calb_lm[i, ]
      names(lm_list)[i] <- paste(nutri_name[i])
    }
  }

  lm_df <- dplyr::bind_rows(lm_list)

  print(lm_df)

  attr(obj, "class") <- attribute_list$class
  attr(obj, "method") <- attribute_list$method
  attr(obj, "calb_lm") <- lm_df
  attr(obj, "calb_lm_old") <- attribute_list$calb_lm
  attr(obj, "metadata") <- attribute_list$metadata

  obj
}
