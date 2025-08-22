text_to_evaluate_first_menu_selection <- function(Predefined_lists, rv) {
  menu_choice <- rv$first_menu_choice
  # First find the number of second menu choices
  if (Predefined_lists$second_menu_choices[Predefined_lists$menu_short == menu_choice] != "") {
    second_menu <- Predefined_lists$second_menu_choices[Predefined_lists$menu_short == menu_choice]
    split_second_menu <- str_split(second_menu, "%__%")
    output <- paste0(
      'output$second_menu_', 1:length(split_second_menu[[1]]), '_UI <- renderUI(
      actionButton("second_menu_',1:length(split_second_menu[[1]]), '", "',split_second_menu[[1]],'")
      )', collapse = ";\n"
    )
  } else {
    output <- paste0(compile_questions(Predefined_lists = Predefined_lists, rv = rv)[[1]], collapse = ";\n")
  }
  return(output)
}
