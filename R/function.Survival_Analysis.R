function.Survival_Analysis <- function(Predefined_lists, rv){
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
    '<b>entry_3: </b>', paste0(rv$entry[[3]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- ', ifelse(length(rv$entry[[1]]) > 1,
                                paste0('c("', paste0(rv$entry[[1]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[1]],'"')), '\n',
    'rv$entry[[2]] <- ', ifelse(length(rv$entry[[2]]) > 1,
                                paste0('c("', paste0(rv$entry[[2]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[2]],'"')), '\n',
    'rv$entry[[3]] <- ', ifelse(length(rv$entry[[3]]) > 1,
                                paste0('c("', paste0(rv$entry[[3]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[3]],'"')), '\n',
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
  # Get data
  data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]], if(rv$entry[[3]] != ""){rv$entry[[3]]})]
  data <- na.omit(data)
  func.keep_present_categories_only <- function(variable) {
    factors_in_variable <- levels(data[,variable])
    factors_in_variable <- factors_in_variable[! is.na(match(factors_in_variable, data[,variable]))]
    factor(as.character(data[,variable]), levels = factors_in_variable)
  }
  data[,1] <- func.keep_present_categories_only(rv$entry[[1]])
  if (rv$entry[[3]] != "") {
    data[,3] <- func.keep_present_categories_only(rv$entry[[3]])
  }
  if (nrow(data) >= 2) {
    data[,1] <- as.numeric(data[,1])
    regression_text <- paste0('coxph(Surv(`',
                              rv$entry[[2]],'`, `',rv$entry[[1]],
                              '`) ~ 1',
                              if (rv$entry[[3]] != "") {
                                paste0(' + `', rv$entry[[3]] ,'`')
                              },
                              ', data = data)'
    )
    regression_results <- eval(parse(text = regression_text))
    if(rv$entry[[3]] !="") {
      results_display <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Analysis outcome` = "Successful",
        Outcome = rv$entry[[1]],
        `Follow-up` = rv$entry[[2]],
        Group = levels(data[,3])[2:nlevels(data[,3])],
        `Hazard ratio` = summary(regression_results)$coefficients[2],
        `Hazard ratio - LCI` = summary(regression_results)$coefficients[1] - (qnorm(0.975) * summary(regression_results)$coefficients[3]),
        `Hazard ratio - UCI` = summary(regression_results)$coefficients[1] + (qnorm(0.975) * summary(regression_results)$coefficients[3]),
        `P value` = summary(regression_results)$coefficients[5],
        AIC = AIC(regression_results),
        BIC = BIC(regression_results),
        `-2 Log likelihood` = logLik(regression_results),
        `Log rank test P value` = summary(regression_results)$sctest[3],
        `Wald test P value` = summary(regression_results)$waldtest[3],
        check.names = FALSE
      )
      colnames(data) <- c("status", "time", "group")
      plot_title <- paste0(rv$entry[[1]], " (by ", rv$entry[[3]], ")")
      survival_plot <- ggsurvfit(survfit2(Surv(time, status) ~ 1 + group, data = data), linetype_aes = TRUE) + labs(title = plot_title, x = "Follow-up time", y = paste0("Probability of no '", rv$entry[[1]],"'")) + add_censor_mark() + add_confidence_interval(alpha=0.1) + scale_ggsurvfit() + add_risktable() + theme_classic() + theme(legend.position = "bottom")
    } else {
      results_display <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Analysis outcome` = "Successful",
        Outcome = rv$entry[[1]],
        `Follow-up` = rv$entry[[2]],
        check.names = FALSE
      )
      colnames(data) <- c("status", "time")
      plot_title <- rv$entry[[1]]
      survival_plot <- ggsurvfit(survfit2(Surv(time, status) ~ 1, data = data)) + labs(title = plot_title, x = "Follow-up time", y = paste0("Probability of no '", rv$entry[[1]],"'")) + add_censor_mark() + add_confidence_interval(alpha=0.1) + scale_ggsurvfit() + add_risktable() + theme_classic()
    }
    # Plots
    suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_', substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'KM_plot.png'), plot = survival_plot)))
    plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                         substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'KM_plot.png')
    plots_list_display <- plots_list
    display_plot <- TRUE
    analysis_outcome <- "Successful"
  } else {
    results_display <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      Outcome = rv$entry[[1]],
      `Follow-up` = rv$entry[[2]],
      if(rv$entry[[3]] !="") {Group = rv$entry[[3]]},
      `Reason for unsuccesful analysis` = "There were very few observations to perform an analysis. There must be at least two valid observations to perform a successful analysis.",
      check.names = FALSE
    )
    plots_list <- ""
    plots_list_display <- plots_list
    analysis_outcome <- "Unsuccessful"
    display_plot <- FALSE
  }
  results <- rbind.data.frame(
    colnames(results_display),
    results_display
  )
  display_table <- TRUE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
