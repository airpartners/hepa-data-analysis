#
# Author: Neel Dhulipala
# Project: Air Partners
#
# Script to calculate the exponential decay of PM concentrations after plume.
#
# When a source releases pollutants into the air and finishes, the
# concentration of pollution naturally decays over time. These decay
# functions are exponential in nature and can be fitted to the following model:
#
#   A = yf + (y0 - yf)e^(-kt)
#
# Where t is time, y0 is the initial value of the decay, yf is the asymptotic
# value of the decay, A is the concentration of PM at time t, and k is the
# decay constant.
#
# To test the efficacy of HEPA air purifiers, looking at the decay constants of
# these curves before and after a purifier is installed can be important. A
# purifier that proves to increase the decay constant, i.e. cause the pollutants
# in the air to diffuse out of a space at a faster rate, can be argued to be
# effective.
#
# This script provides the functions necessary to take air quality data from
# indoor sensors and get the decay constants of these decay curves.

library("dplyr")
library("pracma")

# Find the first local minimum after the index of peak_row in y
# that occurs below the min_threshold.
get_first_valleys <- function(data, peaks, min_threshold) {
  # Iterate through rows of peaks matrix
  valley_mat <- matrix(nrow = nrow(peaks), ncol = 2)
  for (row in 1:nrow(peaks)) {
    idx <- peaks[row, 2]
    n <- TRUE
    # create while loop to check for local minima
    while (n) {
      # if we're at the end of y, break out of loop
      if (idx == length(data)) {
        n <- FALSE
      } else {
        # otherwise, get slope (approximately)
        # NOTE: difference between idx and (idx+1) is 1
        slope <- data[idx + 1] - data[idx]
        if (data[idx] <= min_threshold && slope > 0) {
          n <- FALSE
        } else {
          idx <- idx + 1
        }
      }
    }
    # set values of matrix so that column 1 is height of valley and 2 is index
    valley_mat[row, 1] <- data[idx]
    valley_mat[row, 2] <- idx
  }
  valley_mat
}

# Exponential curve fitting for air quality data; returns dataframe containing
# k-values of curves
# TODO: nls function returns various weird errors, diagnose why these errors
# appear
# e.g. "number of iterations", "singular gradient", "NaNs, Inf, etc.", etc.
curve_fitting <- function(data, peaks, valleys) {
    # Create empty DataFrame for storing k values
  alphas.data <- data.frame(
    "peak_idx" = numeric(0),
    "valley_idx" = numeric(0),
    "peak_hgt" = numeric(0),
    "k_val" = numeric(0),
    "conv_tol" = numeric(0)
  )
  # Define parameters for curve fitting function for each row
  for (row in 1:nrow(peaks)) {
    row <- as.double(row)
    i_range <- peaks[row, 2]:valleys[row, 2]
    sect <- data[i_range]
    t <- i_range - peaks[row, 2] + 1
    df <- data.frame(t = t, y = sect)
    # Get exponential fit
    nlc <- nls.control(maxiter = 1000)
    fit <- try(nls(y ~ SSasymp(t, yf, y0, log_alpha), data = df, control = nlc))
    if (class(fit) != "nls") {
      next
    }
    # Get parameters of the fit
    params <- coef(fit)
    # Extract the log_alpha value and put it in form e^(log(a)) to get a
    log_alpha <- as.double(params["log_alpha"])
    alpha <- exp(log_alpha)
    # Get achieved convergence tolerance as metric for accuracy of fit
    # NOTE: R^2 value can be calculated but is not a useful metric
    # for nonlinear models
    conv <- fit$convInfo$finTol
    # Add alpha to dataframe
    alphas.newdata <- data.frame(
      "peak_idx" = c(peaks[row, 2]),
      "valley_idx" = c(valleys[row, 2]),
      "peak_hgt" = c(peaks[row, 1]),
      "k_val" = c(alpha),
      "conv_tol" = c(conv)
    )
    alphas.data <- rbind(alphas.data, alphas.newdata)
  }
  alphas.data <- arrange_all(alphas.data)
}
