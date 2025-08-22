\name{function.rbind_different_column_numbers}
\alias{function.rbind_different_column_numbers}
\title{
Combine Multiple Dataframes with Different Column Numbers
}
\description{
\pkg{Base} function \code{rbind.data.frame} requires that the multiple data frames to be combined must have the same column numbers and names. For producing reports for 'EQUAL-STATS', data frames with different column numbers and names are required. This function allows this combination.
}
\usage{
function.rbind_different_column_numbers(list, include_columns)
}
\arguments{
\item{list}{A list of data frames to be combined provided as a \code{list} object.}
\item{include_columns}{Whether the column names of each data frame should be included as the first row indicated as \code{"Yes"} or \code{"No"}. Default is \code{"Yes"}. It is unsual to require \code{"No"} in this function.}
}
\value{
\item{output}{A data frame with the multiple rows combined.}
}
\references{
\url{https://sites.google.com/view/equal-group/home}
}
\author{
Kurinchi Gurusamy
}
\note{
This is part of a suite of functions required to allow 'EQUAL-STATS' program to run. This is unlikely to be used as a stand alone function.
}
\seealso{
\code{\link[base:rbind.data.frame]{base::rbind.data.frame()}}
}
\concept{ EQUAL-STATS }
\examples{
# Create a simulated data frames
df <- cbind.data.frame(
  Age = rnorm(3000, 40, 10),
  Height = rnorm(3000,165,15),
  `Length of hospital stay` = rpois(3000, 8),
  `Number of infections` = rpois(3000, 3)
)
# Calculate means and standard deviations for normally distributed variables
# and median and upper and lower quartiles for non-normally distributed variables
summary_measures <- lapply(1:ncol(df), function(y) {
  if (y<=2) {
    output <- cbind.data.frame(
      Variable = colnames(df)[y],
      Mean = mean(df[,y]),
      `Standard deviation` = sd(df[,y])
    )
  } else {
    output <- cbind.data.frame(
      Variable = colnames(df)[y],
      Median = quantile(df[,y], probs = 0.5),
      `Lower quartile` = quantile(df[,y], probs = 0.25),
      `Upper quartile` = quantile(df[,y], probs = 0.75)
    )
  }
  return(output)
})
# Combine the normally and non-normally distributed variables
normally_distributed_variables <- rbind.data.frame(summary_measures[[1]], summary_measures[[2]])
non_normally_distributed_variables <- rbind.data.frame(summary_measures[[3]], summary_measures[[4]])
# Combining the variables in a single data frame using
# rbind.data.frame causes error
combined_data_frame <- try(rbind.data.frame(normally_distributed_variables,
non_normally_distributed_variables))
combined_data_frame
# Combining the variables in a single data_frame using
# function.rbind_different_column_numbers does not cause error
# Note that data frames must be supplied as a list
# (any number of data frames can be present in the list)
# Final function ####
combined_data_frame_new_function <- function.rbind_different_column_numbers(
list(normally_distributed_variables, non_normally_distributed_variables)
)
combined_data_frame_new_function
}
