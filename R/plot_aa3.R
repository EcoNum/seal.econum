#' plot aa3 file
#'
#' @param obj aa3 object
#' @param graph_type definir le type de graphique que l'on souhaite sortir, par
#' defaut "NO". "ALL" pour sortir le graphique avec toute les donnees, "lm" pour
#' sortir le graphique de la régression pour la calibration
#' @param old_data logical value to use data before modification, by default
#' FALSE
#'
#' @return Une liste contenant un dataframe avec les donnees CALB,
#' les graphes de controle pour la calibration, un dataframe avec les parametres
#' des regressions et un dataframe avec les donnees SAMP filtrees
#' @export
#' @import stringr
#' @importFrom chart chart
#' @importFrom stats na.omit
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @import ggplot2
#' @importFrom  dplyr select
#' @importFrom broom tidy
#' @importFrom broom glance
#' @importFrom ggrepel geom_text_repel
#' @importFrom chart ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
#'
#'
#' @examples
#' #TODO
#'

plot.aa3 <- function(obj, graph_type = "NO", old_data = FALSE){

  if (old_data){
    if (!("calb_lm_old" %in% names(attributes(obj)))) {
        stop("objet ne contient pas de donnees modifiees")
    }
  }

  if (!(graph_type %in% c("NO","ALL","lm"))){
    stop("valeur non valable, utiliser 'ALL' pour representer toutes les valeurs
         ou 'lm' pour representer les droites de calibration")
  }

  attr(x = obj, which = "metadata")$sample -> samp_name

  if (old_data) {
    (names(obj)[str_detect(names(obj), pattern = "values_old")] -> a)
    (names(obj)[str_detect(names(obj), pattern = "std_old")] -> b)
    stringr::str_split(a, pattern = "_") %>.%
      sapply(., `[[`, 1) %>.%
      paste(., "old", sep = "_") -> nutri_name
    attr(obj, "calb_lm_old") -> calb_lm
  } else {
    (names(obj)[str_detect(names(obj), pattern = "values$")] -> a)
    (names(obj)[str_detect(names(obj), pattern = "std$")] -> b)
    stringr::str_split(a, pattern = "_") %>.%
      sapply(., `[[`, 1) -> nutri_name
    attr(obj, "calb_lm") -> calb_lm
  }

  # create a list
  graph_aa3 <- list()

  for (i in 1:length(a)) {
    x <- chart::chart(obj,
                      formula = stats::as.formula(paste(a[i], "~",
                                                        "date_time%color=%sample_type%group=%1"))) +
      geom_line() +
      geom_point(na.rm = TRUE) +
      theme(legend.direction = "horizontal", legend.position = "bottom") +
      guides(col = guide_legend(title = "Sample",title.position = "top"))


    # Equation

    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(calb_lm[calb_lm$std_name == nutri_name[i],
                                             "intercept"], digits = 2),
                          b = format(calb_lm[calb_lm$std_name == nutri_name[i],
                                             "values"], digits = 2),
                          r2 = format(calb_lm[calb_lm$std_name == nutri_name[i],
                                              "r_squared"], digits = 3)))
    eq <- as.character(as.expression(eq))
    x3 <- chart::chart(obj,
                       formula = as.formula(paste(a[i], "~", b[i]))) +
      geom_point(na.rm = TRUE) +
      geom_abline(intercept = calb_lm[calb_lm$std_name == nutri_name[i],
                                      "intercept"],
                  slope = calb_lm[calb_lm$std_name == nutri_name[i],
                                  "values"]) +
      labs( y = a[i], x = b[i]) +
      scale_y_continuous(limits = range(obj[which(obj$sample_type == "CALB" &
                                                    obj[,b[i]] != "NA"),
                     a[i]], na.rm = TRUE)) +
      geom_text(x = 2*(diff(range(obj[which(obj$sample_type == "CALB" &
                                              obj[,b[i]] != "NA"), b[i]])))/5 ,
                y = max(obj[which(obj$sample_type == "CALB" &
                                    obj[,b[i]] != "NA"), a[i]]),
                label = eq, parse = TRUE) +
      ggrepel::geom_text_repel(aes(label = obj[obj[,b[i]] != "NA", b[i]]),
                               nudge_y = 1.5, nudge_x = 1.5, direction = "both",
                               segment.size = 0.2, na.rm = TRUE)

    if (graph_type == "ALL") {
      graph_aa3[[i]] <- x + ggtitle(nutri_name[i])
      names(graph_aa3)[i] <- paste(a[i])
    } else if (graph_type == "lm") {
      graph_aa3[[i]] <- x3 + ggtitle(nutri_name[i])
      names(graph_aa3)[i] <- paste(a[i])
    } else {
      x4 <- chart::ggarrange(x, x3)
      ggpubr::annotate_figure(x4,
                              top = ggpubr::text_grob(a[i], size =  14,
                                                      face = "bold"),
                              bottom = ggpubr::text_grob(paste0("Data source: ",
                                                                samp_name),
                                                         color = "blue",
                                                         hjust = 1,x = 1,
                                                         face = "italic",
                                                         size = 10)) -> x5
      graph_aa3[[i]] <- x5
      names(graph_aa3)[i] <- paste(a[i])
    }
  }
  return(graph_aa3)
}
