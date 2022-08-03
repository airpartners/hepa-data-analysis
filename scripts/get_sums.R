#
# Author: Neel Dhulipala
# Project: AIr Partners
#
# Contains functions to get summary statistics on basic statistics of model
# (mean, median, IQR, etc.) and decay constant statistics.

library("tidyverse")
library("pracma")
library("dplyr")
source("curve_fitting.R")

# Return basic summary statistics
get_basic_sum <- function(data, colname, logical_array) {
  summary <-
    data %>%
      group_by(logical_array) %>%
      summarize(across(starts_with("ratio"),
                    list(fifth = ~ quantile(.x, .05, na.rm = TRUE),
                          twentyfifth = ~ quantile(.x, .25, na.rm = TRUE),
                          seventyfifth = ~ quantile(.x, .75, na.rm = TRUE),
                          nintyfifth = ~ quantile(.x, .95, na.rm = TRUE),
                          mean = ~ mean(.x, na.rm = TRUE),
                          median = ~ median(.x, na.rm = TRUE)
                          )
                    )
              ) %>%
      pivot_longer( # nolint
        names_to = c(NA, "bin1", "bin2", ".value"),
        names_sep = "_",
        cols = -c(logical_array)
      )
  summary
}


# Returns summary of decay constants for certain column in air quality dataset.
get_decay_sum <- function(data, colname, logical_array) {
  # Create variable for time series
  in_out <- as.numeric(unlist(data[colname]))
  # Get peaks of dataset
  in_out[is.na(in_out)] <- 0
  peaks <- findpeaks(in_out,
                    nups = 1,
                    ndowns = 1,
                    minpeakheight = 3,
                    minpeakdistance = 200,
                    threshold = 0)
  # Get nearest valleys to those peaks
  valleys <- get_first_valleys(in_out, peaks, 2) # nolint
  # Get the decay values between each peak and valley highlighted
  decays <- curve_fitting(in_out, peaks, valleys) # nolint
  # Get index of when HEPA was installed, since calculations so far made by
  # index
  hepa_install_idx <- match(TRUE, as.logical(unlist(data[logical_array])))

  # Create summary of decay values for before and after
  decays$logic <- decays["peak_idx"] >= hepa_install_idx
  decays["peak_width"] <- decays["valley_idx"] - decays["peak_idx"]

  decay_sum <-
    decays %>%
      group_by(logic) %>% # nolint
      summarize(
        k_val_mean = mean(k_val, na.rm = TRUE), #nolint
        k_val_median = median(k_val, na.rm = TRUE),
        k_val_min = min(k_val, na.rm = TRUE),
        k_val_max = max(k_val, na.rm = TRUE),
        peak_hgt_mean = mean(peak_hgt, na.rm = TRUE), #nolint
        peak_wdh_mean = mean(peak_width, na.rm = TRUE) #nolint
    )
  decay_sum
}