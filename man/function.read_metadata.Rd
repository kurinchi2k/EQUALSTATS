\name{function.read_metadata}
\alias{function.read_metadata}
\title{
Change the Data Type and Reference Category
}
\description{
When an user uploads a file in 'EQUAL-STATS' program, the program classify the variable types based on the nature of the data uploaded using the function function.read_data. However, the levels in the categorical data are determined by alphabetical order, which may not be appropriate in many situations, particularly when needs to compare a new treatment to the standard treatment (reference category) and when the event and no event have to be defined correctly. This can be addressed by uploading metadata.
}
\usage{
function.read_metadata(rv, metadata_file_path)
}
\arguments{
\item{rv}{A list supplied by 'EQUAL-STATS' application based on user input.}
\item{metadata_file_path}{The path to the metadata file.}
}
\value{
\item{outcome}{Whether the data types and the order of the categories (levels/factors) were modified successfully.}
\item{message}{The message displayed to the user after the processing. This message also contains the reason for failure if the modification was unsuccessful.}
\item{data}{Imported data (with the structure modified as per the metadata)}
\item{any_type}{All variables in the data}
\item{quantitative}{Quantitative variables in the data}
\item{counts}{Count variables in the data}
\item{categorical}{Categorical variables in the data}
\item{nominal}{Categorical variables without any order in the data}
\item{binary}{Categorical variables with only two possible values (factors/levels) in the data}
\item{ordinal}{Ordered categorical variables}
\item{date}{Any variables that were declared as date and could be convered to date}
\item{date}{Any variables that were declared as time and could be convered to time}
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
\code{\link[=function.read_data]{function.read_data()}}
}
\concept{ EQUAL-STATS }
\examples{
# Create simulated data ####
data <- cbind.data.frame(
  `Subject ID` = c(
    "S0001", "S0002", "S0003", "S0004", "S0005",
    "S0006", "S0007", "S0008", "S0009", "S0010",
    "S0011", "S0012", "S0013", "S0014", "S0015",
    "S0016", "S0017", "S0018", "S0019", "S0020",
    "S0021", "S0022", "S0023", "S0024", "S0025",
    "S0026", "S0027", "S0028", "S0029", "S0030"),
  `Centre` = c(
    "C_0001", "C_0002", "C_0002", "C_0002", "C_0002",
    "C_0001", "C_0001", "C_0003", "C_0001", "C_0003",
    "C_0001", "C_0002", "C_0002", "C_0001", "C_0003",
    "C_0002", "C_0002", "C_0003", "C_0001", "C_0002",
    "C_0002", "C_0002", "C_0002", "C_0003", "C_0002",
    "C_0001", "C_0003", "C_0001", "C_0001", "C_0001"),
  `Treatment` = c(
    "Intensive rehabilitation", "Intensive rehabilitation", "Standard rehabilitation",
    "Intensive rehabilitation", "Intensive rehabilitation", "Intensive rehabilitation",
    "Intensive rehabilitation", "Intensive rehabilitation", "Intensive rehabilitation",
    "Standard rehabilitation", "Intensive rehabilitation", "Standard rehabilitation",
    "Standard rehabilitation", "Intensive rehabilitation", "Intensive rehabilitation",
    "Intensive rehabilitation", "Standard rehabilitation", "Standard rehabilitation",
    "Intensive rehabilitation", "Standard rehabilitation", "Intensive rehabilitation",
    "Intensive rehabilitation", "Standard rehabilitation", "Intensive rehabilitation",
    "Intensive rehabilitation", "Standard rehabilitation", "Standard rehabilitation",
    "Intensive rehabilitation", "Standard rehabilitation", "Intensive rehabilitation"),
  `Obesity status` = c(
    "Obese", "Non-obese", "Obese", "Non-obese", "Non-obese",
    "Obese", "Obese", "Obese", "Non-obese", "Obese",
    "Non-obese", "Non-obese", "Obese", "Non-obese", "Obese",
    "Obese", "Non-obese", "Obese", "Obese", "Obese",
    "Non-obese", "Non-obese", "Non-obese", "Obese", "Obese",
    "Non-obese", "Obese", "Obese", "Obese", "Obese"),
  `Unable to walk independently at 6 weeks` = c(
    "unable", "able", "able", "unable", "able",
    "able", "unable", "unable", "unable", "unable",
    "able", "unable", "able", "unable", "unable",
    "able", "unable", "unable", "unable", "unable",
    "able", "able", "able", "able", "unable",
    "able", "able", "unable", "able", "unable"),
  `Mobility score at 6 months` = c(
    86, 65.1, 48, 99.8, 73.4, 70, 74.7, 36.5, 64.6, 85.4,
    41.7, 60.1, 73.3, 42.4, 55.3, 47.3, 85.9, 63, 64.6, 101.8,
    108.1, 72.3, 96.4, 87.5, 66.2, 92.9, 47.7, 55.8, 56.4, 133.8),
  `Pain at 6 weeks` = c(
    "3_severe", "1_mild", "1_mild", "2_moderate", "1_mild",
    "1_mild", "2_moderate", "2_moderate", "1_mild", "3_severe",
    "1_mild", "2_moderate", "1_mild", "3_severe", "3_severe",
    "1_mild", "2_moderate", "3_severe", "2_moderate", "2_moderate",
    "1_mild", "1_mild", "1_mild", "1_mild", "2_moderate",
    "1_mild", "1_mild", "2_moderate", "1_mild", "2_moderate"),
  `Number of falls within 6 months` = c(
    3, 2, 3, 2, 2, 1, 4, 2, 2, 5,
    3, 2, 2, 2, 5, 3, 2, 2, 3, 4,
    3, 1, 2, 2, 2, 7, 2, 1, 1, 8),
  `Mobility score at 12 months` = c(
    90, 69.1, 52, 103.8, 77.4, 74, 78.7, 40.5, 68.6, 89.4,
    45.7, 64.1, 77.3, 46.4, 59.3, 51.3, 89.9, 67, 68.6, 105.8,
    112.1, 76.3, 100.4, 91.5, 70.2, 96.9, 51.7, 59.8, 60.4, 137.8)
)
# Create a meta-data file
# The first row indicates data type and the second row indicates
# the reference category (for all types of categorical variables)
metadata_first_two_rows <- cbind.data.frame(
c("nominal","S0001"), c("nominal", "C_0001"), c("binary", "Standard rehabilitation"),
c("binary", "Non-obese"), c("binary","able"), c("quantitative", NA), c("ordinal", "1_mild"),
c("counts", NA), c("quantitative", NA))
colnames(metadata_first_two_rows) <- colnames(data)
# The subsequent rows indicate the levels in the user-defined order
# for all categorical variables
# For this simulation, we will change the reference category
# but retain the remaining default order of variable levels
# First find the maximum number of levels
maximum_number_of_levels <- max(as.numeric(sapply(1:ncol(data), function(y) {
  if ((metadata_first_two_rows[1,y] == "quantitative") |
  (metadata_first_two_rows[1,y] == "counts")) {
    NA
  } else {
    nlevels(factor(data[,y]))
  }
})), na.rm = TRUE)
metadata_subsequent_rows <- lapply(1:ncol(data), function(y) {
  if ((metadata_first_two_rows[1,y] == "quantitative") |
  (metadata_first_two_rows[1,y] == "counts")) {
    output <- rep(NA,(maximum_number_of_levels-1))
  } else {
    categories <- sort(unique(data[,y]))
    categories <- categories[categories != metadata_first_two_rows[2,y]]
    output <- c(categories, rep(NA,(maximum_number_of_levels-1-length(categories))))
  }
}
)
metadata_subsequent_rows <- do.call(cbind.data.frame, metadata_subsequent_rows)
colnames(metadata_subsequent_rows) <- colnames(data)
metadata <- rbind.data.frame(metadata_first_two_rows, metadata_subsequent_rows)
# Simulate lists provided by EQUAL-STATS ####
entry <- list()
entry <- lapply(1:15, function(x) entry[[x]] <- '')
rv <- list(
  StorageFolder = tempdir(),
  first_menu_choice = "Summary_Measures",
  second_menu_choice = NA,
  entry = entry,
  import_data = NULL,
  same_row_different_row = NA,
  submit_button_to_appear = FALSE,
  summary_measures_choices = c("EQUAL-STATS choice", "Total observations",
  "Missing observations", "Available observations"),
  analysis_outcome = list(),
  code = list(),
  plan = list(),
  results = list(),
  plots_list = list(),
  reports = list()
)
# Store the data and metadata in a folder
data_file_path = paste0(tempdir(), "/data.csv")
write.csv(data, file = data_file_path, row.names = FALSE, na = "")
metadata_file_path = paste0(tempdir(), "/metadata.csv")
write.csv(metadata, file = metadata_file_path, row.names = FALSE, na = "")
# Load the necessary packages and functions
library(stringr)
# Read the data
rv$import_data <- function.read_data(data_file_path)
# Final function ####
meta_data_implemented_data_types <- function.read_metadata(rv, metadata_file_path)
}
