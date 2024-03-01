col_means <- function(df) {
  num_cols <- ncol(df)
  means <- numeric(num_cols)
  
  for (i in 1:num_cols) {
    means[i] <- mean(df[[i]]) 
  }
  
  return(means)
}


count_na <- function(vec) {
  count <- 0  
  
  for (element in vec) {
    if (is.na(element)) {
      count <- count + 1
    }
  }
  
  return(count)
}


