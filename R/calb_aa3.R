#' Title
#'
#' @param x aa3 data
#'
#' @return a list containing a data.frame with the parameters of the regression (intercept, values, r squared and n) and a list of graphs for each method of dosage (plot of all data and linear regression plot)
#' @import flow
#' @import ggplot2
#' @import ggpubr
#' @import stats
#' @import lubridate
#' @import stringr
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom ggrepel geom_text_repel
#' @export
#'
#' @examples
#' # TODO
#'
calb_aa3 <- function(x){

  # Paramètres
  param <- list(method_1 = list(col = c(5,7)),
                method_2 = list(col = c(8,10)),
                method_3 = list(col = c(11,13)))

  # Sample
  attr(x = x, which = "metadata")$sample -> samp_name

  # Lists
  graph_list <- list()
  calb_lm_list <- list()
  calb_db_list <- list()

  for (i in seq_along(param)) {
    # Samp Data
    x[,c(3:4, param[[i]]$col)] -> samp

    # method & sample
    attr(x = x, which = "method")[[i]]$method -> met

    # all_values plot
    all_values_plot <- ggplot(samp, aes(x = date_time, y = samp[,4],
                                        col = sample_type, group = 1)) +
      geom_point() +
      geom_line() +
      labs(y = "Values") +
      theme_bw() +
      theme(legend.direction = "horizontal", legend.position = "bottom") +
      guides(col = guide_legend(title = "Sample",title.position = "top"))

    # CALB DATA
    samp[samp$sample_type == "CALB",  ]  %>.%
      stats::na.omit(.) -> calb_data

    calb_data[ ,3:4] -> calb

    # CALB DATABASE
    calb_data %>.%
      dplyr::mutate(., id_cal = paste(lubridate::date(calb_data$date_time),
                               stringr::str_split(colnames(calb_data)[4],
                                         pattern = "_")[[1]][1],
                               calb_data[[3]], sep = "_"),
             date = lubridate::date(calb_data$date_time),
             time = strftime(calb_data$date_time, format = "%H:%M:%S"),
             project_id = attr(x, which = "metadata")$sample) -> calb_data

    calb_data %>.%
      tidyr::gather(., key = "std_type", value = "concentration", 3) -> calb_data

    dplyr::mutate(calb_data, std_type = stringr::str_split(calb_data$std_type,
                                           pattern = "_")[[1]][1],
           units_ = attr(x, which = "method")[[i]]$unit ) -> calb_data

    names(calb_data)[3] <- "values"

    calb_data %>.%
      dplyr::select(., id_cal, project_id, date, time, std_type, units_,
             concentration, values) -> calb_db_list[[i]]

    # CALB DATABASE (R base)
    # calb_data$id_cal <- paste(lubridate::date(calb_data$date_time),
    #                      str_split(colnames(calb_data)[4],
    #                      pattern = "_")[[1]][1],
    #                      calb_data[[3]], sep = "_")
    # calb_data$date <- lubridate::date(calb_data$date_time)
    # calb_data$time <- strftime(calb_data$date_time, format = "%H:%M:%S")
    # calb_data$project_id <- attr(x, which = "metadata")$sample

    # calb_data %>.%
    #   tidyr::gather(., key = "std_type",
    #                    value = "concentration", 3) -> calb_data
    # calb_data$std_type <- str_split(calb_data$std_type, pattern = "_")[[1]][1]
    # names(calb_data)[3] <- "values"
    # calb_data[,c("id_cal", "project_id", "date", "time", "std_type",
    #           "concentration", "values")] -> calb_db_list[[i]]

    names(calb_db_list)[i] <- unique(calb_data$std_type)

    # linear model
    lmod <- lm(as.formula(paste0("calb$", names(calb)[2], "~ calb$", names(calb)[1])))
    data.frame(std_name = attr(x = x, which = "method")[[i]]$method,
               intercept = lmod$coefficients[1], values = lmod$coefficients[2],
               r_squared = round(summary(lmod)$r.squared,digits = 4),
               n = length(calb[,1])) -> calb_lm_list[[i]]
    names(calb_lm_list)[i] <- met

    # Equation
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(calb_lm_list[[i]]$intercept, digits = 2),
                          b = format(calb_lm_list[[i]]$values, digits = 2),
                          r2 = format(calb_lm_list[[i]]$r_squared, digits = 3)))
    eq <- as.character(as.expression(eq))

    # graph
    calb_plot <- ggplot(calb, aes(calb[,1], calb[,2])) +
      geom_point() +
      labs( x = "Standard", y = "Values") +
      geom_text(x = 2*(diff(range(calb[,1])))/5 , y = max(calb[,2]),
                label = eq, parse = TRUE) +
      ggrepel::geom_text_repel(aes(label = calb[,1]), nudge_y = 1.5,
                               nudge_x = 1.5, direction = "both",
                               segment.size = 0.2) +
      geom_smooth(method = "lm") +
      theme_bw()

    # combine plot
    ggarrange(all_values_plot, calb_plot) -> combine_plot
    annotate_figure(combine_plot,
                            top = text_grob(met, size =  14, face = "bold"),
                            bottom = text_grob(paste0("Data source: ", samp_name),
                                               color = "blue", hjust = 1,
                                               x = 1, face = "italic",
                                               size = 10)) -> graph_list[[i]]
    names(graph_list)[i] <- met
  }

  # CALIBRATION list
  dplyr::bind_rows(calb_lm_list)  -> lm_tab

  dplyr::bind_rows(calb_db_list) -> calb_db

  x[x$sample_type == "SAMP",] -> samp_df

  calibration <- (list(calbdb = calb_db, regression = lm_tab,
                       graph = graph_list, sampdb = samp_df))
  return(calibration)
}
