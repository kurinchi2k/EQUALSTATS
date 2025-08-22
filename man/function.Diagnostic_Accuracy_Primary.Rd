\name{function.Diagnostic_Accuracy_Primary}
\alias{function.Diagnostic_Accuracy_Primary}
\title{
Calculate the Diagnostic accuracy using Primary Data
}
\description{
Uses \pkg{ThresholdROC} and \pkg{pROC} to calculate the diagnostic accuracy of a test using a reference standard. This function is used when primary data is available. Use \code{\link[=function.Diagnostic_Accuracy_Tables]{function.Diagnostic_Accuracy_Tables()}} when the data is provided as a 2 x 2 contigency table.
}
\usage{
function.Diagnostic_Accuracy_Primary(Predefined_lists, rv)
}
\arguments{
\item{Predefined_lists}{A list supplied by 'EQUAL-STATS' application}
\item{rv}{A list supplied by 'EQUAL-STATS' application based on user input}
}
\value{
\item{analysis_outcome}{Whether the analysis was performed successfullly}
\item{plan}{Plan used for analysis}
\item{code}{Part of code generated for performing the analysis in a standalone version of R}
\item{results}{Analysis results}
\item{results_display}{In order to present a single table, multiple results are combined. This results in some numbers stored as text and can cause very wide tables in the shiny output. This issue is fixed wth some modifications to the results table for display purposes.}
\item{plots_list}{A list of plots generated. Returns "" if no plots are generated.}
\item{plots_list_display}{In the shiny application, only one figure is displayed. Therefore, a composite image is created from the plots for display purposes. Some analysis functions may return \code{NULL}.}
\item{selections}{Selections made by the user for display.}
\item{display_table}{Whether the results table should be displayed in the shiny app.}
\item{display_plot}{Whether the plot should be displayed in the shiny app.}
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
function.submit_choices
function.Diagnostic_Accuracy_Tables
\code{\link[pROC:roc]{pROC::roc()}}
\code{\link[pROC:ggroc]{pROC::ggroc()}}
\code{\link[ThresholdROC:diagnostic]{ThresholdROC::diagnostic()}}
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
    112.1, 76.3, 100.4, 91.5, 70.2, 96.9, 51.7, 59.8, 60.4, 137.8) ,
  `Admission to care home` = c(
    "Not admitted", "Not admitted", "Admitted", "Not admitted", "Admitted",
    "Admitted", "Not admitted", "Admitted", "Admitted", "Not admitted",
    "Admitted", "Admitted", "Not admitted", "Not admitted", "Not admitted",
    "Not admitted", "Admitted", "Admitted", "Not admitted", "Not admitted",
    "Not admitted", "Admitted", "Not admitted", "Admitted", "Admitted",
    "Admitted", "Admitted", "Admitted", "Admitted", "Not admitted"),
  `Follow-up` = c(
    10, 8, 8, 8, 12, 12, 11, 10, 8, 7,
    8, 6, 9, 6, 9, 8, 10, 8, 11, 9,
    6, 9, 12, 9, 8, 11, 12, 9, 10, 11)
)
# Simulate lists provided by EQUAL-STATS
Predefined_lists <- list(
  main_menu = c(
    'Calculate summary measures',
    'Create plots',
    'Check distribution',
    'Compare sample mean versus population mean',
    'Compare groups/variables (independent samples)',
    'Compare groups/variables (paired samples or repeated measures)',
    'Find the correlation (quantitative variables)',
    'Calculate measurement error',
    'Find the diagnostic accuracy (primary data)',
    'Perform sample size and power calculations (primary data)',
    'Perform survival analysis',
    'Perform regression analysis',
    'Analyse time series',
    'Perform mixed-effects regression',
    'Perform multivariate regression',
    'Generate hypothesis',
    'Perform sample size and power calculations (effect size approach)',
    'Make correct conclusions (effect size approach)',
    'Find the diagnostic accuracy (tabulated data)'
  ),
  menu_short = c(
    'Summary_Measures',
    'Create_Plots',
    'Check_Distribution',
    'Compare_Sample_Pop_Means',
    'Compare_Groups',
    'Repeated_Measures',
    'Correlation',
    'Measurement_Error',
    'Diagnostic_Accuracy_Primary',
    'Sample_Size_Calculations_Primary',
    'Survival_Analysis',
    'Regression_Analysis',
    'Time_Series',
    'Mixed_Effects_Regression',
    'Multivariate_Regression',
    'Generate_Hypothesis',
    'Sample_Size_Calculations_Effect_size',
    'Make_Conclusions_Effect_size',
    'Diagnostic_Accuracy_Tables'
  )
)
entry <- list()
entry <- lapply(1:15, function(x) entry[[x]] <- '')
rv <- list(
  StorageFolder = tempdir(),
  first_menu_choice = NA,
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
# Store the data in a folder
data_file_path = paste0(tempdir(), "/data.csv")
write.csv(data, file = data_file_path, row.names = FALSE, na = "")
# Load the necessary packages and functions ####
library(stringr)
library(pROC)
library(ThresholdROC)
library(ggplot2)
rv$import_data <- function.read_data(data_file_path)
# Update choices ####
rv$first_menu_choice <- "Diagnostic_Accuracy_Primary"
rv$second_menu_choice <- NA
rv$entry[[1]] <- "Treatment"
rv$entry[[2]] <- "Unable to walk independently at 6 weeks"
# Final function ####
Results <- function.Diagnostic_Accuracy_Primary(Predefined_lists, rv)
}
