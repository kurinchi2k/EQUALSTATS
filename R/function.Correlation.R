function.Correlation <- function(Predefined_lists, rv){
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
    'rv$entry[[2]] <- ', ifelse(length(rv$entry[[2]]) > 1,
                                paste0('c("', paste0(rv$entry[[2]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[2]],'"')), '\n',
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
  # Check normality
  func.check.normality <- function(variable) {
    # Test normality through skewness, kurtosis, Shapiro-Wilk test, and Kolmogrov-Smirnov test
    # Skewness - the confidence intervals do not overlap (-0.5) to 0.5
    skewness <- suppressWarnings(try(Skew(variable, conf.level = 0.95), silent = TRUE))
    if (str_detect(skewness[[1]][1], "Error")) {
      skewness <- "Not possible to determine"
    } else if ((skewness[3] < (-0.5)) | (skewness[2] > 0.5)) {
      skewness <- "Non-normal"
    } else {
      skewness <- "No evidence that it is non-normal"
    }
    kurtosis <- suppressWarnings(try(Kurt(variable, conf.level = 0.95), silent = TRUE))
    if (str_detect(kurtosis[[1]][1], "Error")) {
      kurtosis <- "Not possible to determine"
    } else if ((kurtosis[2] + 3) > 3) { # kurtosis provided is excess kurtosis. To get the actual kurtosis add 3
      kurtosis <- "Non-normal"
    } else {
      kurtosis <- "No evidence that it is non-normal"
    }
    # Shapiro-Wilk test: This may not work for more than 5000 observations
    shapiro.wilk <- suppressWarnings(try(shapiro.test(variable[!is.na(variable)]), silent = TRUE))
    if (str_detect(shapiro.wilk[[1]][1], "Error")) {
      if (length(variable) > 5000) {
        shapiro.wilk <- "No evidence that it is non-normal"
      } else {
        shapiro.wilk <- "Not possible to determine"
      }
    } else if (shapiro.wilk$p.value <= 0.10) {
      shapiro.wilk <- "Non-normal"
    } else {
      shapiro.wilk <- "No evidence that it is non-normal"
    }
    ks <- suppressWarnings(ks.test(variable,"pnorm", mean=mean(variable), sd=sd(variable)))
    if (is.na(ks$p.value)) {
      ks <- "Not possible to determine"
    } else if (ks$p.value <= 0.10) {
      ks <- "Non-normal"
    } else {
      ks <- "No evidence that it is non-normal"
    }
    normality_results <- c(skewness, kurtosis, shapiro.wilk, ks)
    table(normality_results)
    # Now at least one is "Non-normal" or at least three have "Not possible to determine"
    if (
      ("Non-normal" %in% normality_results) |
      (length(normality_results[normality_results == "Not possible to determine"]) > 2)
    ) {
      distribution <- "Non-normal"
    } else {
      distribution <- "No evidence that it is non-normal"
    }
    return(distribution)
  }
  # Summary
  func.summary.quantitative <- function(variable, prefix, normality) {
    if (normality == TRUE) {
      summary <- c(prefix, length(variable), mean(variable), sd(variable))
      names(summary) <- c("Variable","Number of observations", "Mean", "Standard deviation")
    } else {
      summary <- c(prefix, length(variable), summary(variable)[c(3,2,5)])
      names(summary) <- c("Variable","Number of observations", "Median", "Lower quartile", "Upper quartile")
    }
    return(summary)
  }
  # Now with the data
  data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
  data <- na.omit(data)
  # Do the analysis only if at least two rows are present
  if (nrow(data) >= 2) {
    # Calculate the correlation coefficient
    if (rv$second_menu_choice == "EQUAL-STATS choice") {
      # If normal: Pearson correlation coefficient; otherwise Spearman correlation coefficient
      normality <- func.check.normality(variable = data[,1])
      descriptive_summary_1 <- func.summary.quantitative(variable = data[,1], prefix = rv$entry[[1]], normality = (normality == "No evidence that it is non-normal"))
      descriptive_summary_2 <- func.summary.quantitative(variable = data[,2], prefix = rv$entry[[2]], normality =  (normality == "No evidence that it is non-normal"))
      correlation <- cor(data, method = ifelse(normality == "No evidence that it is non-normal", "pearson", "spearman"))
    } else if (rv$second_menu_choice == "Pearson correlation coefficient") {
      descriptive_summary_1 <- func.summary.quantitative(variable = data[,1], prefix = rv$entry[[1]], normality = TRUE)
      descriptive_summary_2 <- func.summary.quantitative(variable = data[,2], prefix = rv$entry[[2]], normality = TRUE)
      correlation <- cor(data, method = "pearson")
    } else if (rv$second_menu_choice == "Spearman correlation coefficient") {
      descriptive_summary_1 <- func.summary.quantitative(variable = data[,1], prefix = rv$entry[[1]], normality = FALSE)
      descriptive_summary_2 <- func.summary.quantitative(variable = data[,2], prefix = rv$entry[[2]], normality = FALSE)
      correlation <- cor(data, method = "spearman")
    }
    p.mat <- cor_pmat(data)
    descriptive_summary <- data.frame(rbind(descriptive_summary_1, descriptive_summary_2), check.names = FALSE)
    results_display <- data.frame(
      `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
      `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
      `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
      `First variable` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
      `Second variable` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
      `Method used` = c(if (rv$second_menu_choice == "EQUAL-STATS choice") {
        ifelse(normality == "No evidence that it is non-normal", "Pearson correlation coefficient", "Spearman correlation coefficient")
      } else {
        rv$second_menu_choice
      }, rep(NA, nrow(descriptive_summary)-1)),
      Variable = descriptive_summary$Variable,
      sapply(descriptive_summary[,colnames(descriptive_summary) != "Variable"], as.numeric),
      Correlation = c(correlation[1,2], rep(NA, nrow(descriptive_summary)-1)),
      `P value` = c(p.mat[1,2], rep(NA, nrow(descriptive_summary)-1)),
      check.names = FALSE
    )
    # Create plots
    plot_title <- paste0(rv$entry[[1]], " by ", rv$entry[[2]])
    plot <- ggcorrplot(correlation, method = "square", lab = TRUE, p.mat = p.mat, outline.color = "white") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5), axis.text.x = element_text(hjust =0)) + ggtitle(plot_title) + scale_y_discrete(limits=rev) + scale_x_discrete(position = "top") + geom_text(aes(x = 0, y = 0, label ="* Numbers indicate correlation coefficient."), size = 3.5, hjust=0, vjust =-2.5)+ geom_text(aes(x = 0, y = 0, label ="**Crossed out numbers (if any) indicate that the correlation was not significant."), size = 3.5, hjust=0, vjust =-1)
    suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                               substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_correlation_plot.png'),
                                             plot = plot)))
    plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                         substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_correlation_plot.png')
    plots_list_display <- plots_list
    analysis_outcome <- "Successful"
    display_plot <- TRUE
  } else {
    results_display <- data.frame(
      `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
      `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
      `Analysis outcome` = c("Unsuccessful",  rep(NA, nrow(descriptive_summary)-1)),
      `First variable` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
      `Second variable` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
      `Reason for unsuccesful analysis` = "There were very few observations to perform an analysis. There must be at least two valid observations of the first variable in at least two groups to perform a successful analysis.",
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
