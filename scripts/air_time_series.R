#
# Author: Neel Dhulipala
# Project: Air Partners
#
# Function to create a time series plot for indoor/outdoor concentrations of
# particulate matter in the air. 
# @args
# - data: (data.frame) collection of air quality data to be plotted
# - colname: (string) name of column from which you want to plot (e.g. opc_pm1,
#            opc_pm25, neph_bin10, etc.)
# - logical_array: (string) column indicating before and after purifier (or
#                  other air controller) is installed; column in `data` should
#                  be of form logical (parse using as.logical()) and be FALSE
#                  prior to installation and TRUE afterwards
# - summary: (tbl; data.frame) basic metrics of the data containing mean,
#            median, and IQR)
# - start_dates: (data.frame; opt) range of dates from before installation
# - end_dates: (data.frame; opt) range of dates from after installation

library(ggplot2)
library(tidyverse)
library(cowplot)
library(dplyr)

air_time_series <- function(data,
                            colname,
                            logical_array,
                            summary,
                            start_dates = data[!(data$logical_array), ],
                            end_dates = data[data$logical_array, ]) {

  # Helper conditions for making plots
  install_cond <-
    which(
      !(summary[logical_array]) &
      summary$bin1 == sub("_.*", "", colname) &
      summary$bin2 == sub(".*_", "", colname)
    )
  uninstall_cond <-
    which(
      summary[logical_array] &
      summary$bin1 == sub("_.*", "", colname) &
      summary$bin2 == sub(".*_", "", colname)
    )

  # Function to pass strings as aes mapping parameters
  str_to_aes <- function(aes_string) {
    eval(parse(text = paste0("aes(x = timestamp, y = ratio_", colname, ")")))
  }

  # Create time series plot
  before <-
    start_dates %>%
      ggplot(str_to_aes(colname)) +
      geom_line(color = "navy", alpha = 0.2) +
      geom_hline(
        yintercept = as.numeric(
          summary[install_cond, "mean"]
        ),
        color = "red",
        lty = "dashed"
      ) +
      geom_hline(
        yintercept = as.numeric(
          summary[install_cond, "median"]
        ),
        color = "darkorange",
        lty = "dashed"
      ) +
      geom_ribbon(
        aes(
          ymin = as.numeric(summary[install_cond, "twentyfifth"]),
          ymax = as.numeric(summary[install_cond, "seventyfifth"])
        ),
        alpha = 0.2,
        color = "darkgreen",
      ) +
      ggtitle("Before HEPA") +
      scale_y_log10(limits = c(1e-2, 1e2)) +
      theme(plot.margin = unit(c(2, 0, 2, 0.5), "cm"))
  after <-
    end_dates %>%
      ggplot(str_to_aes(colname)) +
      geom_line(color = "navy", alpha = 0.2) +
      geom_hline(
        yintercept = as.numeric(
          summary[uninstall_cond, "mean"]
        ),
        color = "red",
        lty = "dashed"
      ) +
      geom_hline(
        yintercept = as.numeric(
          summary[uninstall_cond, "median"]
        ),
        color = "darkorange",
        lty = "dashed"
      ) +
      geom_ribbon(
        aes(
          ymin = as.numeric(summary[uninstall_cond, "twentyfifth"]),
          ymax = as.numeric(summary[uninstall_cond, "seventyfifth"])
        ),
        alpha = 0.2,
        color = "darkgreen",
      ) +
      ylab("") +
      ggtitle("After HEPA") +
      scale_y_log10(limits = c(1e-2, 1e2)) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(2, 0.5, 2, 0), "cm"))
  plot_grid(before, after)

}
