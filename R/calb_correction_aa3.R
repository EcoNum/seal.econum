#' calb_correction
#'
#' @param aa3 calb_aa3 Data
#' @param filter_list liste de concentration a supprimer pour recalculer
#' la régression. (ex : list(Ptot = c(1,2)) -> filter )
#'
#' @return Une liste contenant un dataframe avec les donnees CALB, les graphes
#' de controle pour la calibration et la ou les nouvelle(s) droite(s)
#' de calibration, un dataframe avec les parametres des regressions et
#' un dataframe avec les donnees SAMP recalculee.
#'
#' @export
#'
#' @import  ggplot2
#' @importFrom stats lm
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @examples
#' # TO DO
#'
calb_correction_aa3 <- function(aa3, filter_list) {

  if ( !("calb_aa3" %in% class(aa3)) ){
    stop("class is not calb_aa3")
  }

  calb_df <- aa3$calbdb

  # Check_1 : names of list element
  nutrient_ctrl <- c("Ptot", "NO2", "NOx", "Ntot", "NH4", "PO4")
  if ( is.null(names(filter_list)) ||
       !(names(filter_list) %in% nutrient_ctrl) ) {
    stop("Attention : pas de noms pour les elements de la liste ou pas de
         correspondance, utiliser un ou plusieurs des noms suivants :
         'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
  }

  # graph lists status
  graph_list <- list()
  calb_lm_list <- list()

  for (i in 1:length(names(filter_list))) {
    type <- names(filter_list[i])
    conc <- filter_list[[i]]

    # CALB DATA select
    calb_df[calb_df$std_type ==  type & !(calb_df$concentration %in% conc),] -> calb

    # new linear model
    stats::lm(data = calb, formula = values ~ concentration ) -> lm_mod

    calb_lm_list[[i]] <- data.frame(std_name = paste(type, "new", sep = "_"),
                                    intercept = lm_mod$coefficients[1],
                                    values = lm_mod$coefficients[2],
                                    r_squared = round(summary(lm_mod)$r.squared,
                                                      digits = 4),
                                    n = length(calb$concentration))
    names(calb_lm_list)[i] <- paste(type, "new", sep = "_")

    # Equation
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(calb_lm_list[[i]]$intercept, digits = 2),
                          b = format(calb_lm_list[[i]]$values, digits = 2),
                          r2 = format(calb_lm_list[[i]]$r_squared, digits = 3)))
    eq <- as.character(as.expression(eq))

    # graph
    graph_list[[i]] <- ggplot(calb, aes(calb$concentration, calb$values)) +
      geom_point() +
      labs( x = "Standard", y = "Values") +
      geom_text(x = 2*(diff(range(calb$concentration)))/5 , y = max(calb$values),
                label = eq, parse = TRUE) +
      ggrepel::geom_text_repel(aes(label = calb$concentration), nudge_y = 1.5,
                               nudge_x = 1.5, direction = "both",
                               segment.size = 0.2) +
      geom_smooth(method = "lm") +
      ggtitle(paste(type, "new", sep = "_")) +
      theme_bw()

    names(graph_list)[i] <- paste(type, "new", sep = "_")

    # add new nutrient values
    cname <- paste(type, "conc", sep = "_")
    cnum <- which(names(aa3$sampdb) == cname)
    names(aa3$sampdb)[cnum] <- paste(type, "conc_old", sep = "_")
    aa3$sampdb %>.%
      dplyr::mutate(., new = round((aa3$sampdb[,paste(type, "values", sep = "_")] -
                               lm_mod$coefficients[[1]]) /
                              lm_mod$coefficients[[2]],3)) -> aa3$sampdb

    names(aa3$sampdb)[length(aa3$sampdb)] <- paste(type,"conc", sep = "_")
  }

  # add new graph, regression parameter and nutrient values in EcoNumData
  aa3$graph <- append(aa3$graph, graph_list)
  aa3$regression <- dplyr::bind_rows(aa3$regression, calb_lm_list)

  return(aa3)
  }
