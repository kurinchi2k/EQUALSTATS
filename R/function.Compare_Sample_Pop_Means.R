function.Compare_Sample_Pop_Means <- function(Predefined_lists, rv){
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
    '<b>entry_2: </b>', paste0(rv$entry[[2]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- ', ifelse(length(rv$entry[[1]]) > 1,
                                paste0('c("', paste0(rv$entry[[1]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[1]],'"')), '\n',
    'rv$entry[[2]] <- ', rv$entry[[2]], '\n',
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
  variable <- rv$import_data$data[,rv$entry[[1]]]
  variable <- variable[! is.na(variable)]
  if (rv$second_menu_choice == "Quantitative variable") {
    test_results <- t.test(variable, mu = rv$entry[[2]], na.action=na.omit)
    results_display <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Successful",
      `Variable name` = rv$entry[[1]],
      `Sample mean` = as.numeric(test_results$estimate),
      `Sample lower confidence interval` = as.numeric(test_results$conf.int[1]),
      `Sample upper confidence interval` = as.numeric(test_results$conf.int[2]),
      `Population mean` = rv$entry[[2]],
      Statistic = "T",
      `Statistic value` = test_results$statistic,
      `P value` = test_results$p.value,
      check.names = FALSE
    )
  } else {
    totals_each_category <- data.frame(table(variable))
    totals_each_category$totals <- length(variable)
    binom_test_results <- lapply(1:nrow(totals_each_category), function(x) {
      binom_test_results_each_row <- binom.test(totals_each_category$Freq[x],length(variable),rv$entry[[2]])
      data.frame(
        Category = as.character(totals_each_category$variable[x]),
        `Sample mean` = as.numeric(binom_test_results_each_row$estimate),
        `Sample lower confidence interval` = as.numeric(binom_test_results_each_row$conf.int[1]),
        `Sample upper confidence interval` = as.numeric(binom_test_results_each_row$conf.int[2]),
        `Population mean` = rv$entry[[2]],
        `P value` = binom_test_results_each_row$p.value,
        check.names = FALSE
      )
    })
    binom_test_results <- do.call(rbind.data.frame, binom_test_results)
    results_display <- data.frame(
      `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(totals_each_category)-1)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = c("Successful",  rep(NA, nrow(totals_each_category)-1)),
      `Variable name` = c(rv$entry[[1]], rep(NA, nrow(totals_each_category)-1)),
      binom_test_results,
      check.names = FALSE
    )
  }
  results <- rbind.data.frame(colnames(results_display),results_display)
  # No plots for this function
  plots_list <- ""
  plots_list_display <- ""
  analysis_outcome <- "Successful"
  display_table <- TRUE
  display_plot <- FALSE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
