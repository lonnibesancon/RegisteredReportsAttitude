################################################################
# Bootstrap CI Helper Functions.
# Computes bootstrap confidence intervals with the sensible defaults.
# 2014 Pierre Dragicevic
################################################################

# For more details
# See http://www.mayin.org/ajayshah/KB/R/documents/boot.html for boot
# and (Kirby and Gerlanc, 2013) http://web.williams.edu/Psychology/Faculty/Kirby/bootes-kirby-gerlanc-in-press.pdf for bootES and for referencing the bootstrap method in your paper

if (!require("boot")) {
  install.packages("boot")
  library(boot)
}
if (!require("PropCIs")) {
  install.packages("PropCIs")
  library(PropCIs)
}

# If set to TRUE, bootstrap intervals remain the same across executions.
deterministic <- TRUE

# The number of bootstrap resamples. Typically recommended is >= 1,000.
# The higher the number, the more precise the interval but also the slower
# the computation.
replicates <- 10000

# The method for computing intervals. The adjusted bootstrap
# percentile (BCa) method is recommended by (Kirby and Gerlanc, 2013)
# and should work in most cases. For other methods type help(boot.ci).

# FIXME: changing this does not work
intervalMethod <- "bca"

# Number of significant digits used for text output. Using many digits
# is not recommended as it gives a misleading impression of precision.
significantDigits <- 3

#####################################################################

# Statistics

samplemedian <- function(x, d) {
  return(median(x[d]))
}

samplemean <- function(x, d) {
  return(mean(x[d]))
}

# Data transformations

# No transformation, yields arithmetic means.
identityTransform <- c(
  function(x) {return (x)}, # the transformation
  function(x) {return (x)}, # the inverse transformation
  TRUE                      # TRUE if increasing, FALSE otherwise
)

# Log transformation, yields geometric means.
logTransform <- c(
  function(x) {return (log(x))}, # the transformation
  function(x) {return (exp(x))}, # the inverse transformation
  TRUE                         # TRUE if increasing, FALSE otherwise
)

# Inverse transformation, yields harmonic means.
inverseTransform <- c(
  function(x) {return (1/x)}, # the transformation
  function(x) {return (1/x)}, # the inverse transformation
  FALSE                     # TRUE if increasing, FALSE otherwise
)

# Returns the point estimate and confidence interval in an array of length 3
bootstrapCI <- function(statistic, datapoints) {
  # Compute the point estimate
  pointEstimate <- statistic(datapoints)
  # Case with a single observation
  if (length(datapoints) == 1) return (c(pointEstimate, NA, NA))
  # Make the rest of the code deterministic
  if (deterministic) set.seed(0)
  # Generate bootstrap replicates
  b <- boot(datapoints, statistic, R = replicates, parallel="multicore")
  # Compute interval
  ci <- boot.ci(b, type = intervalMethod)
  # Return the point estimate and CI bounds
  # You can print the ci object for more info and debug
  lowerBound <- ci$bca[4]
  upperBound <- ci$bca[5]
  return(c(pointEstimate, lowerBound, upperBound))
}

# Returns the mean and its confidence interval in an array of length 3
bootstrapMeanCI <- function(datapoints) {
  return(bootstrapCI(samplemean, datapoints))
}

# Returns the median and its confidence interval in an array of length 3
bootstrapMedianCI <- function(datapoints) {
  return(bootstrapCI(samplemedian, datapoints))
}

# Returns the mean and its "classical" confidence interval in an array of length 3.
# Uses the t-distribution confidence interval for samples that are approximately normally
# distributed and/or large. Not a bootstrap method but included here for convenience.
exactMeanCI <- function(datapoints, transformation = identityTransform) {
  datapoints <- transformation[[1]](datapoints)
  pointEstimate <- mean(datapoints)
  ttest <- t.test(datapoints)
  lowerBound <- ttest[4]$conf.int[1]
  upperBound <- ttest[4]$conf.int[2]
  if (transformation[[3]])
    return(transformation[[2]](c(pointEstimate, lowerBound, upperBound)))
  else
    return(transformation[[2]](c(pointEstimate, upperBound, lowerBound)))
}

# Returns a percentage and its confidence interval, for binary observations.
# Uses a confidence interval method for proportions. Not a bootstrap method but
# included here for convenience.
percentCI <- function(datapoints, value) {
  ncorrect <- length(datapoints[datapoints == value])
  total <- length(datapoints)
  percent <- 100 * ncorrect / total
  proportionCI <- midPci(x = ncorrect, n = total, conf.level = 0.95)
  percentCIlow <- 100 * proportionCI$conf.int[1]
  percentCIhigh <- 100 * proportionCI$conf.int[2]
  return (c(percent, percentCIlow, percentCIhigh))
}

# Returns the point estimate and confidence interval in a human-legible text format
bootstrapCI.text <- function(statistic, datapoints, unit) {
  result <- bootstrapCI(statistic, datapoints)
  point <- result[1]
  interval <- c(result[2], result[3])
  # Format results in human-legible format using APA style
  text <- paste(
    prettyNum(point, digits=significantDigits),
    unit,
    ", 95% CI [",
    prettyNum(interval[1], digits=significantDigits),
    unit,
    ", ",
    prettyNum(interval[2], digits=significantDigits),
    unit,
    "]",
    sep = "")
  return(text)
}

# Returns the mean and its confidence interval in a human-legible text format
bootstrapMeanCI.text <- function(datapoints, unit) {
  return(bootstrapCI.text(samplemean, datapoints, unit))
}

# Returns the median and its confidence interval in a human-legible text format
bootstrapMedianCI.text <- function(datapoints, unit) {
  return(bootstrapCI.text(samplemedian, datapoints, unit))
}