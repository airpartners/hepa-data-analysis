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
      ggtitle("PM2.5 Before HEPA") +
      scale_y_log10(limits = c(1e-2, 1e2)) +
      theme(plot.margin = unit(c(5, 2.5, 5, 0.5), "cm"))
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
      ggtitle("PM2.5 After HEPA") +
      scale_y_log10(limits = c(1e-2, 1e2)) +
      theme(plot.margin = unit(c(5, 0.5, 5, -1.5), "cm"))
  plot_grid(before, after)

}