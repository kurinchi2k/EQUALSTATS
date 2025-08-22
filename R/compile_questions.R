globalVariables(c("ACF", "Actual", "Actual.values", "AN0001_results", "Average",
                  "Categories", "cooks_distance", "descriptive_summary",
                  "fitted_values", "group", "hat_values","Lag",
                  "outcome","Predicted", "Predicted.values",
                  "rainbow", "raw_residuals", "standardised_residuals",
                  "subject_id", "Time", "time_of_measurement", "y"),
                "EQUALSTATS", add = TRUE)
compile_questions <- function(Predefined_lists, rv) {
  menu_choice <- rv$first_menu_choice
  second_menu_choice <- rv$second_menu_choice
  # Create a dataframe to manage things easily
  details <- data.frame(Predefined_lists[names(Predefined_lists)[is.na(match(names(Predefined_lists),c("show_menu", "hide_menu", "hide_second_menu")))]])
  # Find the function number
  function_number <- match(menu_choice, Predefined_lists$menu_short)
  # Using stems get the labels, entry_text, entered values
  labels <- as.vector(unlist(details[function_number, colnames(details)[substr(colnames(details), 1, 6) == "label_"]]))
  labels <- labels[labels != ""]
  entry_texts <- as.vector(unlist(details[function_number, colnames(details)[substr(colnames(details), 1, 6) == "entry_"]]))
  entry_texts <- entry_texts[1:length(labels)]
  mandatory_entries <- as.vector(unlist(details[function_number, colnames(details)[substr(colnames(details), 1, 10) == "mandatory_"]]))
  mandatory_entries <- mandatory_entries[1:length(labels)]
  UI_names <- as.vector(paste0("entry_", 1:length(labels), "_UI"))
  if (! is.na(second_menu_choice)) {
    second_menu_function_number <- match(second_menu_choice, str_split(details$second_menu_choices[function_number],"%__%")[[1]])
    labels <- sapply(1:length(labels), function(x) {
      str_split(labels[x],"%__%")[[1]][second_menu_function_number]
    })
    entry_texts <- sapply(1:length(entry_texts), function(x) {
      str_split(entry_texts[x],"%__%")[[1]][second_menu_function_number]
    })
    mandatory_entries <- sapply(1:length(mandatory_entries), function(x) {
      str_split(mandatory_entries[x],"%__%")[[1]][second_menu_function_number]
    })
  }
  UI_texts <- sapply(1:length(labels), function(x) {
    if ((! is.null(labels[x])) & (labels[x] != "NULL")) {
      convert_to_user_interface(UI_name = UI_names[x], label = labels[x], entry_text = entry_texts[x], rv = rv)
    } else {
      list(
        paste0('output$', UI_names[x], ' <- NULL'),
        paste0('output$', UI_names[x], ' <- NULL')
      )
    }
  })
  UI_texts <- t(UI_texts)
  UI_Initial_texts <- c(unlist(UI_texts[,1]),
                        if(length(labels) < 15) {paste0('output$', 'entry_', (length(labels)+1):15, '_UI <- NULL')}
  )
  UI_Update_texts <- c(unlist(UI_texts[,2]),
                       if(length(labels) < 15) {paste0('output$', 'entry_', (length(labels)+1):15, '_UI <- NULL')}
  )
  submit_button_appear_text <- paste0('if(',
                                      paste0('(TRUE %in% (rv$entry[[',which(mandatory_entries == "yes"),']] != ""))', collapse = ' & '),
                                      ') {rv$submit_button_to_appear <- TRUE} else {rv$submit_button_to_appear <- FALSE}'
  )
  UI_texts <- list(UI_Initial_texts, UI_Update_texts, submit_button_appear_text)
  return(UI_texts)
}
