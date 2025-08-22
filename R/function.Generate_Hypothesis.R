function.Generate_Hypothesis <- function(Predefined_lists, rv){
  # Lists
  plan <- {cbind.data.frame(
    analysis_number = paste0("AN", formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
    first_menu_choice = rv$first_menu_choice,
    second_menu_choice = rv$second_menu_choice,
    entry_1 = paste0(rv$entry[[1]], collapse = "%_%"),
    entry_2 = paste0(rv$entry[[2]], collapse = "%_%"),
    entry_3 = paste0(rv$entry[[3]], collapse = "%_%"),
    entry_4 = paste0(rv$entry[[4]], collapse = "%_%"),
    entry_5 = paste0(rv$entry[[5]], collapse = "%_%"),
    entry_6 = paste0(rv$entry[[6]], collapse = "%_%"),
    entry_7 = paste0(rv$entry[[7]], collapse = "%_%"),
    entry_8 = paste0(rv$entry[[8]], collapse = "%_%"),
    entry_9 = paste0(rv$entry[[9]], collapse = "%_%"),
    entry_10 = paste0(rv$entry[[10]], collapse = "%_%"),
    entry_11 = paste0(rv$entry[[11]], collapse = "%_%"),
    entry_12 = paste0(rv$entry[[12]], collapse = "%_%"),
    entry_13 = paste0(rv$entry[[13]], collapse = "%_%"),
    entry_14 = paste0(rv$entry[[14]], collapse = "%_%"),
    entry_15 = paste0(rv$entry[[15]], collapse = "%_%"),
    same_row_different_row = ""
  )}
  selections <- {paste0(
    '<b>entry_1: </b>', paste0(rv$entry[[1]], collapse = "; "), '<br>',
    '<b>entry_2: </b>', paste0(rv$entry[[2]], collapse = "; "), '<br>',
    '<b>entry_3: </b>', paste0(rv$entry[[3]], collapse = "; "), '<br>',
    '<b>entry_4: </b>', paste0(rv$entry[[4]], collapse = "; "), '<br>',
    '<b>entry_5: </b>', paste0(rv$entry[[5]], collapse = "; "), '<br>',
    '<b>entry_6: </b>', paste0(rv$entry[[6]], collapse = "; "), '<br>',
    '<b>entry_7: </b>', paste0(rv$entry[[7]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- "', rv$entry[[1]], '"\n',
    'rv$entry[[2]] <- "', rv$entry[[2]], '"\n',
    'rv$entry[[3]] <- "', rv$entry[[3]], '"\n',
    'rv$entry[[4]] <- "', rv$entry[[4]], '"\n',
    'rv$entry[[5]] <- "', rv$entry[[5]], '"\n',
    'rv$entry[[6]] <- ', rv$entry[[6]], '\n',
    'rv$entry[[7]] <- "', rv$entry[[7]], '"\n',
    'AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_results <- function.',rv$first_menu_choice,'(Predefined_lists, rv)', '\n',
    if(length(rv$plan) == 0){
      'if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(AN0001_results$plots_list, paste0(AN0001_results$plots_list,"_copy")))}
'
    } else {
      paste0(
        'AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$results[2,1] <- "AN',formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'"', '\n',
        'if (TRUE %in% (AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list != "")) {invisible(file.rename(AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list, str_replace_all(AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list, "/AN0001_", "/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_")))}', '\n')
    },
    'write.table(x = AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$results, append = TRUE, file = paste0(rv$StorageFolder, "/results.csv"), sep = ",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")', '\n'
  )}
  null_hypothesis <-
    if (rv$entry[[3]] == "Intervention is better or worse than comparator") {
      if (rv$entry[[6]] == 0) {
        paste0("There is no difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], ".")
      } else {
        paste0("The difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], " is less than ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
      }
    } else if (rv$entry[[3]] == "Intervention is better than comparator") {
      if (rv$entry[[6]] == 0) {
        "It is impossible to design a trial to find if the intervention is better than comparator with a margin of absolutely no difference."
      } else {
        if (rv$entry[[5]] == "Higher values of the outcome (or more events) are better for the subject") {
          paste0("The increase in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is less than ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        } else {
          paste0("The decrease in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is less than ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        }
      }
    } else if (rv$entry[[3]] == "Intervention is not worse than comparator") {
      if (rv$entry[[6]] == 0) {
        "It is impossible to design a trial with a non-inferiority margin of absolutely no difference."
      } else {
        if (rv$entry[[5]] == "Higher values of the outcome (or more events) are better for the subject") {
          paste0("The decrease in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is more than ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        } else {
          paste0("The increase in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is more than ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        }
      }
    } else if (rv$entry[[3]] == "Intervention is equivalent to the comparator") {
      if (rv$entry[[6]] == 0) {
        "It is impossible to design a trial with an equivalence margin of absolutely no difference."
      } else {
        paste0("The difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], " is more than ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
      }
    }
  alternate_hypothesis <-
    if (rv$entry[[3]] == "Intervention is better or worse than comparator") {
      if (rv$entry[[6]] == 0) {
        paste0("There is a difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], ".")
      } else {
        paste0("The difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], " is at least ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
      }
    } else if (rv$entry[[3]] == "Intervention is better than comparator") {
      if (rv$entry[[6]] == 0) {
        "It is impossible to design a trial to find if the intervention is better than comparator with a margin of absolutely no difference."
      } else {
        if (rv$entry[[5]] == "Higher values of the outcome (or more events) are better for the subject") {
          paste0("The increase in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is at least ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        } else {
          paste0("The decrease in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is at least ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        }
      }
    } else if (rv$entry[[3]] == "Intervention is not worse than comparator") {
      if (rv$entry[[6]] == 0) {
        "It is impossible to design a trial with a non-inferiority margin of absolutely no difference."
      } else {
        if (rv$entry[[5]] == "Higher values of the outcome (or more events) are better for the subject") {
          paste0("The decrease in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]]," is less than or equal to ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        } else {
          paste0("The increase in ",  rv$entry[[4]], " with ", rv$entry[[1]], " versus ", rv$entry[[2]], " is less than or equal to ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
        }
      }
    } else if (rv$entry[[3]] == "Intervention is equivalent to the comparator") {
      if (rv$entry[[6]] == 0) {
        "It is impossible to design a trial with an equivalence margin of absolutely no difference."
      } else {
        paste0("The difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], " is less than or equal to ", rv$entry[[6]], if(rv$entry[[7]] != ""){paste0(" ", rv$entry[[7]])}, ".")
      }
    }
  other_comments <-
    if ((rv$entry[[3]] == "Intervention is better or worse than comparator") & (rv$entry[[6]] == 0)) {
      paste0("Although you want to test the null hypothesis that there is no difference in ",  rv$entry[[4]], " between ", rv$entry[[1]], " and ", rv$entry[[2]], ", you must enter the expected difference between the groups during sample size calculations.")
    } else if (rv$entry[[6]] == 0) {
      null_hypothesis
    } else {
      paste0('Choose "', rv$entry[[3]], '" in sample size calculations and enter ', rv$entry[[6]], " in the desired or expected difference.")
    }
  results_display <- data.frame(
    `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
    `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
    `Analysis outcome` = "Successful",
    `Null hypothesis` = null_hypothesis,
    `Alternate hypothesis` = alternate_hypothesis,
    `Other comments` = other_comments,
    check.names = FALSE
  )
  results <- rbind.data.frame(colnames(results_display), results_display)
  display_table <- TRUE
  plots_list <- ""
  plots_list_display <- plots_list
  display_plot <- FALSE
  analysis_outcome <- "Successful"
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list, plots_list_display = plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
