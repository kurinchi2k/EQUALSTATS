\name{function.read_data}
\alias{function.read_data}
\title{
Read a CSV File and Classify Variable Type
}
\description{
When an user uploads a file in 'EQUAL-STATS' program, the program can automatically classify the variable types based on the nature of the data uploaded. The data and data types are stored in memory. This then determines the options available for questions and the analysis performed. The variable types can be altered using function.read_metadata.
}
\usage{
function.read_data(data_file_path)
}
\arguments{
\item{data_file_path}{The path to the data file.}
}
\value{
\item{outcome}{Whether the import was successful.}
\item{message}{The message displayed to the user after the processing. This message also contains the reason for failure if the import was unsuccessful.}
\item{data}{Imported data}
\item{any_type}{All variables in the data}
\item{quantitative}{Quantitative variables in the data}
\item{counts}{Count variables in the data}
\item{categorical}{Categorical variables in the data}
\item{nominal}{Categorical variables without any order in the data}
\item{binary}{Categorical variables with only two possible categories (factors/levels) in the data}
\item{ordinal}{Ordered categorical variables}
\item{date}{Any variables that appear like date}
\item{time}{Any variables that appear like time}
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
\code{\link[=function.read_metadata]{function.read_metadata()}}
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
# Store this in a folder
data_file_path = paste0(tempdir(), "/data.csv")
write.csv(data, file = data_file_path, row.names = FALSE, na = "")
# Load the necessary packages
library(stringr)
# Final function ####
imported_data_types <- function.read_data(data_file_path)
}
