\name{function.Generate_Hypothesis}
\alias{function.Generate_Hypothesis}
\title{
Generate Research Hypothesis
}
\description{
Generates the research hypothesis that can be used in grant applications, study protocols, and scientific reports when information is provided in plain language.
}
\usage{
function.Generate_Hypothesis(Predefined_lists, rv)
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
}
\concept{ EQUAL-STATS }
\examples{
# Simulate lists provided by EQUAL-STATS ####
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
# Load the necessary packages and functions ####
library(stringr)
rv$first_menu_choice <- "Generate_Hypothesis"
rv$second_menu_choice <- NA
rv$entry[[1]] <- "Intensive rehabilitation"
rv$entry[[2]] <- "Standard rehabilitation"
rv$entry[[3]] <- "Intervention is better or worse than comparator"
rv$entry[[4]] <- "Mobility score"
rv$entry[[5]] <- "Higher values of the outcome (or more events) are better for the subject"
rv$entry[[6]] <- 10
rv$entry[[7]] <- ""
# Final function ####
Results <-  function.Generate_Hypothesis(Predefined_lists, rv)
}
