\name{text_to_evaluate_first_menu_selection}
\alias{text_to_evaluate_first_menu_selection}
\title{
Converts the List Provided to Create User Interface
}
\description{
Obtains the processed list related to a particular selection and converts them to questions, if there are no second level selections so that the shiny user interface is created. If there are second level selections, it creates the text for second menu.
}
\usage{
text_to_evaluate_first_menu_selection(Predefined_lists, rv)
}
\arguments{
\item{Predefined_lists}{A list supplied by 'EQUAL-STATS' application based on user input}
\item{rv}{A list supplied by 'EQUAL-STATS' application based on user input}
}
\value{
\item{output}{Either a list of texts to create the shiny interface for questions when there are no second menu choices or character text to create the second menu.}
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
Predefined_lists <- list(
main_menu = c(
'Calculate summary measures',
'Create plots'
),
menu_short = c(
'Summary_Measures',
'Create_Plots'
),
second_menu_choices = c(
'',
'EQUAL-STATS choice\%__\%Histogram'
),
label_1 = c(
'Select the variable for which summary measures are required',
'Select the variable\%__\%Select the variable'
),
label_2 = c(
'Select the variable for which you want separate summary (optional)',
'NULL\%__\%NULL'
),
label_3 = c(
'Select the summary measures that you want in the report',
'Enter the title for the plot\%__\%Enter the title for the plot'
),
label_4 = c(
'',
'Select the variable\%__\%Select the variable'
),
label_5 = c(
'',
''
),
label_6 = c(
  '',
  ''
),
label_7 = c(
  '',
  ''
),
label_8 = c(
  '',
  ''
),
label_9 = c(
'',
''
),
label_10 = c(
'',
''
),
label_11 = c(
'',
''
),
label_12 = c(
'',
''
),
label_13 = c(
'',
''
),
label_14 = c(
'',
''
),
label_15 = c(
'',
''
),
entry_1 = c(
'\%_\%selectInput\%_\%rv$import_data$any_type',
'\%_\%selectInput\%_\%rv$import_data$any_type'
),
entry_2 = c(
'\%_\%selectInput\%_\%c("",setdiff(rv$import_data$categorical, rv$entry[[1]]))',
'NULL\%__\%NULL'
),
entry_3 = c(
'\%_\%checkbox\%_\%rv$summary_measures_choices',
'\%_\%text\%_\%"Plot title"\%__\%\%_\%text\%_\%"Plot title"'
),
entry_4 = c(
'',
'\%_\%selectInput\%_\%rv$entry[[1]]\%__\%\%_\%selectInput\%_\%rv$entry[[1]]'
),
entry_5 = c(
'',
''
),
entry_6 = c(
'',
''
),
entry_7 = c(
'',
''
),
entry_8 = c(
'',
''
),
entry_9 = c(
'',
''
),
entry_10 = c(
'',
''
),
entry_11 = c(
'',
''
),
entry_12 = c(
'',
''
),
entry_13 = c(
'',
''
),
entry_14 = c(
'',
''
),
entry_15 = c(
'',
''
),
mandatory_1 = c(
'yes',
'yes\%__\%yes'
),
mandatory_2 = c(
'no',
'NULL\%__\%NULL'
),
mandatory_3 = c(
'yes',
'no\%__\%no'
),
mandatory_4 = c(
'',
'no\%__\%no'
),
mandatory_5 = c(
'',
''
),
mandatory_6 = c(
'',
''
),
mandatory_7 = c(
'',
''
),
mandatory_8 = c(
'',
''
),
mandatory_9 = c(
'',
''
),
mandatory_10 = c(
'',
''
),
mandatory_11 = c(
'',
''
),
mandatory_12 = c(
'',
''
),
mandatory_13 = c(
'',
''
),
mandatory_14 = c(
'',
''
),
mandatory_15 = c(
'',
''
),
numeric_exemptions = c(
'',
''
)
)
rv <- list(
  StorageFolder = tempdir(),
  first_menu_choice = "Create_Plots",
  second_menu_choice = "EQUAL-STATS choice",
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
# Functions and packages required to run
library(stringr)
# Final function ####
# When there is no second menu
rv$first_menu_choice <- "Summary_Measures"
rv$second_menu_choice <- NA
first_menu_questions <- text_to_evaluate_first_menu_selection(
Predefined_lists = Predefined_lists, rv = rv)
# When there is a second menu
rv$first_menu_choice <- "Create_Plots"
rv$second_menu_choice <- "EQUAL-STATS choice"
second_menu_text <- text_to_evaluate_first_menu_selection(
Predefined_lists = Predefined_lists, rv = rv)
}
