function.Diagnostic_Accuracy_Tables <- function(Predefined_lists, rv){
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
    '<b>entry_4: </b>', paste0(rv$entry[[4]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- ', rv$entry[[1]],  '\n',
    'rv$entry[[2]] <- ', rv$entry[[2]],  '\n',
    'rv$entry[[3]] <- ', rv$entry[[3]],  '\n',
    'rv$entry[[4]] <- ', rv$entry[[4]],  '\n',
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
  if ((rv$entry[[1]] > 0) & (rv$entry[[4]] > 0) & (rv$entry[[2]] >= 0) & (rv$entry[[3]] >= 0)) {
    # For ROC, need to get a dataframe
    data <- cbind.data.frame(
      `Index test` = factor(c(rep("positive", (rv$entry[[1]])), rep("positive", (rv$entry[[2]])), rep("negative", (rv$entry[[3]])), rep("negative", (rv$entry[[4]]))), levels = c("positive", "negative")),
      `Reference standard` = factor(c(rep("positive", (rv$entry[[1]])), rep("negative", (rv$entry[[2]])), rep("positive", (rv$entry[[3]])), rep("negative", (rv$entry[[4]]))), levels = c("positive", "negative"))
    )
    # Do the analysis only if at least two rows are present
    if (nrow(data) >= 2) {
      DTAR_table <- structure(c(rv$entry[[1]],rv$entry[[2]],rv$entry[[3]],rv$entry[[4]]), dim=c(2,2))
      test_results <- data.frame(diagnostic(DTAR_table,"exact"))
      test_results <- cbind.data.frame(rownames(test_results),test_results)
      colnames(test_results) <- c("Measure", "Point estimate", "Lower CI", "Upper CI")
      present_levels_1 <- levels(data[,1])
      present_levels_2 <- levels(data[,2])
      if ((length(present_levels_1) == 2) & (length(present_levels_2) == 2)) {
        plot_title <- "Diagnostic accuracy\nIndex test vs Reference standard"
        AUROC <- suppressMessages(suppressWarnings(try( roc(as.numeric(data[,1]),as.numeric(data[,2]), ci = TRUE, ci.alpha = 0.95, print.auc=TRUE), silent = TRUE)))
        if (str_detect(AUROC[[1]][1], "Error")) {
          AUROC_text <- c(NA, NA, NA)
          ROC_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = paste0(plot_title, "\nThe area under the curve could not be calculated. This is probably because of sparse data.")), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
        } else {
          ROC_plot <- suppressMessages(suppressWarnings(ggroc(AUROC) + ggtitle(plot_title) + geom_abline(aes(slope=1,intercept=1),linetype=2) +  geom_text(aes(x = 0.3, y = 0.2, label = paste0('AUROC:', "\n",formatC(AUROC$auc, digits = 2, format = "f"),' (', ifelse(is.na(AUROC$ci[1]), NA,  formatC(AUROC$ci[1], digits = 2, format = "f")), ', ', ifelse(is.na(AUROC$ci[3]), NA,  formatC(AUROC$ci[3], digits = 2, format = "f")), ')')), size = 2, hjust=0, vjust =-1)))
          AUROC_text <- c(AUROC$auc, AUROC$ci[1], AUROC$ci[3])
        }
      } else {
        AUROC_text <- c(NA, NA, NA)
        ROC_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = paste0(plot_title, "\nThere must be exactly two categories in the diagnostic test and reference standard to create a receiver operating characteristic plot.\nThis condition was not met.\nSo, the area under the curve could not be calculated.")), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
      }
      test_results <- rbind.data.frame(
        test_results,
        data.frame(
          `Measure` = "Area under the curve",
          `Point estimate` = AUROC_text[1],
          `Lower CI` = AUROC_text[2],
          `Upper CI` = AUROC_text[3],
          check.names = FALSE
        )
      )
      suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                                 substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png'),
                                               plot = ROC_plot)))
      plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                           substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png')
      plots_list_display <- plots_list
      display_plot <- TRUE
      analysis_outcome <- "Successful"
      results_display <- data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(test_results)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")), rep(NA, nrow(test_results)-1)),
        `Analysis outcome` = c("Successful", rep(NA, nrow(test_results)-1)),
        `True positive` = c(rv$entry[[1]], rep(NA, nrow(test_results)-1)),
        `False negative` = c(rv$entry[[2]], rep(NA, nrow(test_results)-1)),
        `False positive` = c(rv$entry[[3]], rep(NA, nrow(test_results)-1)),
        `True negative` = c(rv$entry[[4]], rep(NA, nrow(test_results)-1)),
        `Number of observations` = c(nrow(data), rep(NA, nrow(test_results)-1)),
        test_results,
        check.names = FALSE
      )
      plots_list_display <- plots_list
      analysis_outcome <- "Successful"
    } else {
      results_display <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Analysis outcome` = "Unsuccessful",
        `True positive` = rv$entry[[1]],
        `False negative` = rv$entry[[2]],
        `False positive` = rv$entry[[3]],
        `True negative` = rv$entry[[4]],
        `Reason for unsuccesful analysis` = "There were very few observations to perform an analysis. There must be at least two observations to perform a successful analysis.",
        check.names = FALSE
      )
      plots_list <- ""
      plots_list_display <- plots_list
      analysis_outcome <- "Unsuccessful"
      display_plot <- FALSE
    }
  } else {
    results_display <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      `True positive` = rv$entry[[1]],
      `False negative` = rv$entry[[2]],
      `False positive` = rv$entry[[3]],
      `True negative` = rv$entry[[4]],
      `Reason for unsuccesful analysis` = "The information on true positive, false negative, false positive, and true negative must be complete. There must be at least one true positive and one true negative result to perform the analysis.",
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
