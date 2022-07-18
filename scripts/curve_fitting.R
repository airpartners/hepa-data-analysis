#
# Author: Neel Dhulipala
# Project: Air Partners
#
# Create script to detect peaks
#

if (!require("pracma")) {
  install.packages("pracma")
  library("pracma")
}

x <- seq(0, 1, len = 1024)
pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.40, 0.44, 0.65, 0.76, 0.78, 0.8)
hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 1.1, 5.1)
wdt <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.5)

p_signal <- numeric(length(x))
for (i in seq(along = pos)) {
  p_signal <- p_signal + hgt[i] / (1 + abs((x - pos[i]) / wdt[i]))^4
}

get_peaks <- function(col) {
  # Find peaks of any given data column in accordance with how peaks are defined
  # for getting exponential decay functions.
  #
  findpeaks(col,
            ndowns = 10,
            minpeakheight = 3,
            threshold = 0,
            sortstr = TRUE)
}

get_first_valleys <- function(y, peaks, min_threshold) {
  # Find the first local minimum after the index of peak_row in y
  # that occurs below the min_threshold.

  valley_mat <- matrix(nrow = nrow(peaks), ncol = 2)
  for (row in 1:nrow(peaks)) {
    idx <- peaks[row, 2]
    n <- TRUE
    # create while loop to check for local minima
    while (n) {
      # if we're at the end of y, break out of loop
      print(idx)
      if (idx == length(y)) {
        n <- FALSE
      } else {
        # otherwise, get slope (approximately)
        slope <- y[idx + 1] - y[idx] # difference between idx and (idx+1) is 1
        if (y[idx] <= min_threshold && slope >= 0) {
          n <- FALSE
        } else {
          idx <- idx + 1
        }
      }
    }
    # set values of matrix so that column 1 is height of valley and 2 is index
    valley_mat[row, 1] <- y[idx]
    valley_mat[row, 2] <- idx
  }
  valley_mat
}

peaks <- get_peaks(p_signal)
print(peaks)
valleys <- get_first_valleys(p_signal, peaks, 3)
print(valleys)

plot(p_signal, type = "l",
  main = "PM levels over time",
  xlab = "Time",
  ylab = "PM concentrations (ug/m^3)",
  col = "navy")
  grid()
points(peaks[, 2], peaks[, 1], pch = 20, col = "maroon")
points(valleys[, 2], valleys[, 1], pch = 20, col = "darkgreen")
