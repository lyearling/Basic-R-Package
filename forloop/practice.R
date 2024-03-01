col_means <- function(df) {
  num_cols <- ncol(df)
  means <- numeric(num_cols)
  
  for (i in 1:num_cols) {
    means[i] <- mean(df[[i]]) 
  }
  
  return(means)
}


