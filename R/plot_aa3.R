#' plot aa3 file
#'
#' @param x aa3 object
#' @param y not used
#' @param graph_type definir le type de graphique que l'on souhaite sortir, par
#' defaut "NO". "ALL" pour sortir le graphique avec toute les donnees, "lm" pour
#' sortir le graphique de la régression pour la calibration
#' @param old_data logical value to use data before modification, by default
#' FALSE
#' @param ... further arguments (not used)
#'
#' @return Une liste contenant un dataframe avec les donnees CALB,
#' les graphes de controle pour la calibration, un dataframe avec les parametres
#' des regressions et un dataframe avec les donnees SAMP filtrees
#' @export
#' @importFrom chart chart
#' @importFrom stats na.omit
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @import ggplot2
#' @importFrom  dplyr select
#' @importFrom chart ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
#'
#' @examples
#' #TODO
plot.aa3 <- function(x, y, graph_type = "NO", old_data = FALSE, ...) {

  if (isTRUE(old_data)) {
    if (!("calb_lm_old" %in% names(attributes(x))))
        stop("objet ne contient pas de donnees modifiees")
  }

  if (!(graph_type %in% c("NO","ALL","lm"))) {
    stop("valeur non valable, utiliser 'ALL' pour representer toutes les valeurs
         ou 'lm' pour representer les droites de calibration")
  }

  samp_name <- attr(x, which = "metadata")$sample

  if (isTRUE(old_data)) {
    a <- names(x)[grepl("values_old", names(x))]
    b <- names(x)[grepl("std_old", names(x))]
    strsplit(a, split = "_", fixed = TRUE) -> .
      sapply(., `[[`, 1) -> .
      paste(., "old", sep = "_") -> nutri_name
    attr(x, "calb_lm_old") -> calb_lm
  } else {
    a <- names(x)[grepl("values$", names(x))]
    b <- names(x)[grepl("std$", names(x))]
    strsplit(a, split = "_", fixed = TRUE) -> .
    sapply(.,  `[[`, 1) -> nutri_name
    calb_lm <- attr(x, "calb_lm")
  }

  # Create a list
  graph_aa3 <- list()

  for (i in 1:length(a)) {
    x2 <- chart::chart(data = x, stats::as.formula(paste(a[i], "~",
      "date_time%color=%sample_type%group=%1"))) +
      ggplot2::geom_line() +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::theme(legend.direction = "horizontal", legend.position = "bottom") +
      ggplot2::guides(col = ggplot2::guide_legend(title = "Sample",
        title.position = "top"))

    # Equation
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
      list(
        a = format(calb_lm[calb_lm$std_name == nutri_name[i],
          "intercept"], digits = 2),
        b = format(calb_lm[calb_lm$std_name == nutri_name[i],
          "values"], digits = 2),
        r2 = format(calb_lm[calb_lm$std_name == nutri_name[i],
          "r_squared"], digits = 3)))
    eq <- as.character(as.expression(eq))
    x3 <- chart::chart(data = x, as.formula(paste(a[i], "~", b[i]))) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::geom_abline(intercept = calb_lm[calb_lm$std_name == nutri_name[i],
        "intercept"], slope = calb_lm[calb_lm$std_name == nutri_name[i], "values"]) +
      ggplot2::labs( y = a[i], x = b[i]) +
      ggplot2::scale_y_continuous(limits = range(x[which(x$sample_type == "CALB" &
        x[, b[i]] != "NA"), a[i]], na.rm = TRUE)) +
      ggplot2::geom_text(x = 2*(diff(range(x[which(x$sample_type == "CALB" &
        x[, b[i]] != "NA"), b[i]])))/5 ,
        y = max(x[which(x$sample_type == "CALB" & x[,b[i]] != "NA"), a[i]]),
        label = eq, parse = TRUE) #+
      #ggrepel::geom_text_repel(ggplot2::aes(label = x[x[,b[i]] != "NA", b[i]]),
      #  nudge_y = 1.5, nudge_x = 1.5, direction = "both",
      #  segment.size = 0.2, na.rm = TRUE)

    if (graph_type == "ALL") {
      graph_aa3[[i]] <- x2 + ggplot2::ggtitle(nutri_name[i])
      names(graph_aa3)[i] <- paste(a[i])
    } else if (graph_type == "lm") {
      graph_aa3[[i]] <- x3 + #ggplot2::ggtitle(nutri_name[i])
      ggplot2::labs(title = nutri_name[i], subtitle = paste0("Data source: ", samp_name))
      names(graph_aa3)[i] <- paste(a[i])
    } else {
      x4 <- chart::ggarrange(x2, x3)
      ggpubr::annotate_figure(x4,
        top = ggpubr::text_grob(a[i], size =  14, face = "bold"),
        bottom = ggpubr::text_grob(paste0("Data source: ", samp_name),
        color = "blue", hjust = 1,x = 1, face = "italic", size = 10)
      ) -> x5
      graph_aa3[[i]] <- x5
      names(graph_aa3)[i] <- paste(a[i])
    }
  }
  graph_aa3
}
