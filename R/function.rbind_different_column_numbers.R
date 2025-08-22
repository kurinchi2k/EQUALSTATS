function.rbind_different_column_numbers <- function(list, include_columns) {
  x = list
  if (missing(include_columns)) {
    include_columns <- "Yes"
  }
  if (length(x) < 2) {
    output <- x[[1]]
  } else {
    # Default for combining multiple data frames has to have the same column numbers and the column names have to be the same
    # This can be problematic when multiple data frames have to added to the same data frame row-wise
    # This function overcomes this problem by changing the column names to default, creating a header from the name of the list (if available) and adding column names from each data frame as a new row
    if (is.null(names(x))) {
      headers <- rep("", length(x))
    } else {
      headers <- names(x)
    }
    # get the maximum columns across different data frames
    column_numbers <- sapply(1:length(x),function(z) {ncol(x[[z]])})
    max_columns <- max(column_numbers)
    z <- list()
    output_each_data_frame <- list()
    # Process the files in such a way that is easy to convert to html format later
    for (i in 1:length(x)) {
      z[[3]] <- data.frame(x[[i]], check.names = FALSE)
      if (column_numbers[i] < max_columns) {
        z[[3]][,(column_numbers[i]+1):max_columns] <- NA
        colnames(z[[3]])[(column_numbers[i]+1):max_columns] <- ""
      }
      if (headers[i] != "") {
        z[[1]] < -data.frame(matrix(NA, nrow = 1, ncol = max_columns), row.names = paste0("Header_",i))
        z[[1]][1,1] <- headers[i]
        colnames(z[[1]]) <- paste0("V",1:max_columns)
      }
      if (include_columns == "Yes") {
        z[[2]] <- data.frame(matrix(NA, nrow = 1, ncol = max_columns), row.names = paste0("Column_names_",i))
        z[[2]][1,] <- colnames(z[[3]])
        colnames(z[[2]]) <- paste0("V",1:max_columns)
      }
      colnames(z[[3]]) <- paste0("V",1:max_columns)
      output_each_data_frame[[i]] <- do.call(rbind.data.frame,z)
    }
    output <- do.call(rbind.data.frame, output_each_data_frame)
  }
  return(output)
}
