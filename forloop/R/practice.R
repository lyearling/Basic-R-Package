#' Title takes as input a data frame and returns a vector of column means
#'
#' @param df inputs a data frame
#'
#' @return returns a vector of column means
#' @export
#'
#' @examples 
#' sample_df <- data.frame(
#' A = c(1, 2, 3),
#' B = c(4, 5, 6),
#' C = c(7, 8, 9))
#' col_means(sample_df)
col_means <- function(df) {
  num_cols <- ncol(df)
  means <- numeric(num_cols)
  
  for (i in 1:num_cols) {
    means[i] <- mean(df[[i]]) 
  }
  
  return(means)
}




#' Title count_na() returns how many NA’s there are in a vector
#'
#' @param vec inputs a vector
#'
#' @return the count of NA's in a vector
#' @export
#'
#' @examples 
#' count_na(c(1, NA, 3, NA, 5, NA))
count_na <- function(vec) {
  count <- 0  
  
  for (element in vec) {
    if (is.na(element)) {
      count <- count + 1
    }
  }
  
  return(count)
}
