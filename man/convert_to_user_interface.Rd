\name{convert_to_user_interface}
\alias{convert_to_user_interface}
\title{
Converts the Compiled Questions to User Interface
}
\description{
Obtains the compiled questions related to a particular selection and converts each of these questions them to shiny commands so that user interface is created.
}
\usage{
convert_to_user_interface(UI_name, label, entry_text, rv)
}
\arguments{
\item{UI_name}{Text provided by the \code{\link[=compile_questions]{compile_questions()}} function}
\item{label}{Text provided by the \code{\link[=compile_questions]{compile_questions()}} function}
\item{entry_text}{Text provided by the \code{\link[=compile_questions]{compile_questions()}} function}
\item{rv}{A list supplied by 'EQUAL-STATS' application based on user input}
}
\value{
\item{output_1}{User interface initial text}
\item{output_2}{User interface updated text}
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
\concept{ EQUAL-STATS }
\examples{
# Simulate lists provided by EQUAL-STATS ####
entry <- list()
entry <- lapply(1:15, function(x) entry[[x]] <- '')
UI_name <- "entry_1_UI"
label = "Select the variable for which summary measures are required"
entry_text = "\%_\%selectInput\%_\%rv$import_data$any_type"
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
# Packages required to run
library(stringr)
# Final function ####
UI_texts <- convert_to_user_interface(UI_name = UI_name,
label = label, entry_text = entry_text, rv = rv)
}
