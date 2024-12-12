#' Impute Median Inside The Missing Values (data frame)
#'
#' @param data Data Frame, Data to impute the median
#' @returns Data Frame that have missing values have replace with the median values
#' @examples
#' df <- data.frame(
#' Age = c(45,20,23,24,NA,19),
#' Weight = c(81,NA,55,60,42,78),
#' Height = c(155,178,181,162, NA, 144))
#' imputer_median(example_df))



imputer_median <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  for (i in 1:ncol(data)) {
    if (is.numeric(data[,i])) {
      data[,i] <- ifelse(is.na(data[,i]), median(data[,i], na.rm = TRUE), data[, i])
    }
  }

  return(data)
}






