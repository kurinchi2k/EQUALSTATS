function.Summary_Measures <- function(Predefined_lists, rv){
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
  report_preferences <- {list(
    categorical = list(
      mandatory_1 = c("Included rows"),
      mandatory_2 = c(paste0(rv$entry[[1]], ": Categories")),
      EQUAL_STATS_choice = c("Total observations", "Missing observations", "Available observations", "Proportion", "Proportion - LCI", "Proportion - UCI")
    ),
    quantitative = list(
      EQUAL_STATS_choice_1 = c("Total observations", "Missing observations", "Available observations"),
      EQUAL_STATS_choice_2 = c("Min.", "Max."),
      EQUAL_STATS_choice_4 = c("Kurtosis", "Kurtosis - LCI", "Kurtosis - UCI", "Skewness", "Skewness - LCI", "Skewness - UCI", "Shapiro-Wilk W-statistic", "Shapiro-Wilk p-value", "Kolmogrov-Smirnov D-statistic", "Kolmogrov-Smirnov p-value")
    )
  )}
  # Functions for summary
  func.categorical <- function(variable, variable_name) {
    summary <- list()
    summary$total <- length(variable)
    summary$missing <- length(variable[is.na(variable)])
    if (summary$total > summary$missing) {
      if(nlevels(variable) == 2) {
        summary$summary <- data.frame(sapply(
          1:nlevels(variable), function(x) {
            as.numeric(BinomCI(length(variable[(!is.na(variable)) & variable == levels(variable)[x]]), length(variable[! is.na(variable)]), conf.level = 0.95))
          }
        ), check.names = FALSE)
        colnames(summary$summary) <- levels(variable)
        row.names(summary$summary) <- c("Proportion", "Proportion - LCI", "Proportion - UCI")
      } else {
        summary$summary <- data.frame(t(MultinomCI(table(variable), conf.level = 0.95)),row.names = c("Proportion", "Proportion - LCI", "Proportion - UCI"), check.names = FALSE)
      }
    } else {
      summary$summary <- data.frame(
        `All levels` = c(NA, NA, NA),
        check.names = FALSE,
        row.names = c("Proportion", "Proportion - LCI", "Proportion - UCI"),
        check.rows = FALSE
      )
    }
    results <- data.frame(
      row.names = c("Total observations", "Missing observations", "Available observations"),
      c(summary$total, summary$missing, (summary$total - summary$missing)),
      check.names = FALSE
    )
    colnames(results) <- variable_name
    results <- function.rbind_different_column_numbers(list(results, summary$summary))
    return(results)
  }
  func.quantitative <- function(variable) {
    summary <- list()
    summary$total <- length(variable)
    summary$missing <- length(variable[is.na(variable)])
    if ((summary$total - summary$missing) > 1) {
      summary$summary <- summary(variable[!is.na(variable)])
      summary$sd <- sd(variable[!is.na(variable)])
      summary$mean_ci <- suppressWarnings(try(MeanCI(variable[!is.na(variable)], conf.level = 0.95)[2:3], silent = TRUE))
      if (length(summary$mean_ci) < 2) {
        summary$mean_ci <- c(NA, NA)
      }
      summary$median_ci <- suppressWarnings(try(MedianCI(variable[!is.na(variable)], conf.level = 0.95)[2:3], silent = TRUE))
      if (length(summary$median_ci) < 2) {
        summary$median_ci <- c(NA, NA)
      }
      summary$kurtosis <- suppressWarnings(try(Kurt(variable[!is.na(variable)], conf.level = 0.95), silent = TRUE))
      if (str_detect(summary$kurtosis[[1]][1], "Error")) {
        summary$kurtosis <- c(NA, NA, NA)
      } else {
        summary$kurtosis <- summary$kurtosis + 3 # kurtosis provided is excess kurtosis. To get the actual kurtosis add 3
      }
      summary$skewness <- suppressWarnings(try(Skew(variable[!is.na(variable)], conf.level = 0.95), silent = TRUE))
      if (str_detect(summary$skewness[[1]][1], "Error")) {
        summary$skewness <- c(NA, NA, NA)
      }
      summary$shapiro <- suppressWarnings(try(shapiro.test(variable[!is.na(variable)]), silent = TRUE))
      if (str_detect(summary$shapiro[[1]][1], "Error")) {
        summary$shapiro <- c(NA, NA)
      } else {
        summary$shapiro <- c(summary$shapiro$statistic, summary$shapiro$p.value)
      }
      summary$ks <- suppressWarnings(ks.test(variable[!is.na(variable)],
                                             "pnorm",
                                             mean=mean(variable[!is.na(variable)]),
                                             sd=sd(variable[!is.na(variable)])
      ))
      summary$ks <- c(summary$ks$statistic, summary$ks$p.value)
    } else {
      if ((summary$total == summary$missing)) {
        summary$summary <- rep(NA, 6)
      } else {
        summary$summary <- c(variable[! is.na(variable)], NA, variable[! is.na(variable)], variable[! is.na(variable)], NA, variable[! is.na(variable)])
      }
      names(summary$summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
      summary$sd <- NA
      summary$mean_ci <- c(NA, NA)
      summary$median_ci <- c(NA, NA)
      summary$kurtosis <- c(NA, NA, NA)
      summary$skewness <- c(NA, NA, NA)
      summary$shapiro <- c(NA, NA)
      summary$ks <- c(NA, NA)
    }
    results <- data.frame(
      row.names = c("Total observations", "Missing observations", "Available observations", names(summary$summary),
                    "Standard deviation","Mean - LCI", "Mean - UCI", "Median - LCI", "Median - UCI",
                    "Kurtosis", "Kurtosis - LCI", "Kurtosis - UCI", "Skewness", "Skewness - LCI", "Skewness - UCI",
                    "Shapiro-Wilk W-statistic", "Shapiro-Wilk p-value", "Kolmogrov-Smirnov D-statistic", "Kolmogrov-Smirnov p-value"),
      summary = c(summary$total, summary$missing, (summary$total - summary$missing), as.numeric(summary$summary),
                  summary$sd, as.numeric(summary$mean_ci),as.numeric(summary$median_ci),
                  as.numeric(summary$kurtosis), as.numeric(summary$skewness),
                  as.numeric(summary$shapiro), as.numeric(summary$ks)
      )
    )
    return(results)
  }
  # Some generic processing of data if there is a second variable
  if (rv$entry[[2]] != "") {
    eachgroup <- list()
    data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
    factors_in_variable_2 <- levels(data[,2])
    is.ordinal <- (rv$entry[[2]] %in%  rv$import_data$ordinal)
    if (TRUE %in% (is.na(data[,2]))) {
      factors_in_variable_2 <- c(levels(data[,2]), "missing")
      data[,2] <- as.character(data[,2])
      data[,2][is.na(data[,2])] <- "missing"
      data[,2] <- factor(data[,2], levels = factors_in_variable_2, ordered = is.ordinal)
    }
  }
  # Find if the variable is categorical or quantitative
  if (rv$entry[[1]] %in% rv$import_data$categorical) {
    overall <- func.categorical(variable = rv$import_data$data[,rv$entry[[1]]], variable_name = "All")
    if (rv$entry[[2]] != "") {
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.categorical(variable = data[data[,2] == levels(data[,2])[i],1],
                                           variable_name = paste0(rv$entry[[2]], " = ", levels(data[,2])[i]))
      }
      results <- cbind.data.frame(
        overall,
        do.call(cbind.data.frame, eachgroup)
      )
    } else {
      results <- overall
    }
    row.names(results)[row.names(results) == "Column_names_1"] <- "Included rows"
    row.names(results)[row.names(results) == "Column_names_2"] <- paste0(rv$entry[[1]], ": Categories")
    # Keep only the necessary rows
    if (!("EQUAL-STATS choice" %in% rv$entry[[3]])){
      rows_to_keep <- as.vector(c(report_preferences$categorical$mandatory_1,
                                  if("Total observations" %in% rv$entry[[3]]){"Total observations"},
                                  if("Missing observations" %in% rv$entry[[3]]){"Missing observations"},
                                  if("Available observations" %in% rv$entry[[3]]){"Available observations"},
                                  report_preferences$categorical$mandatory_2,
                                  if("Proportions" %in% rv$entry[[3]]){"Proportion"},
                                  if("Confidence intervals for proportions" %in% rv$entry[[3]]){c("Proportion - LCI", "Proportion - UCI")}
      ))
      results <- results[rows_to_keep,]
    }
  } else {
    overall <- func.quantitative(variable = rv$import_data$data[,rv$entry[[1]]])
    colnames(overall) <- "All"
    if (rv$entry[[2]] != "") {
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.quantitative(variable = data[data[,2] == levels(data[,2])[i],1])
        colnames(eachgroup[[i]]) <- paste0(rv$entry[[2]], " = ", levels(data[,2])[i])
      }
      results <- cbind.data.frame(
        overall,
        do.call(cbind.data.frame, eachgroup)
      )
    } else {
      results <- overall
    }
    # Keep only the necessary rows
    if (!("EQUAL-STATS choice" %in% rv$entry[[3]])){
      rows_to_keep <- as.vector(c(
        if("Total observations" %in% rv$entry[[3]]){"Total observations"},
        if("Missing observations" %in% rv$entry[[3]]){"Missing observations"},
        if("Available observations" %in% rv$entry[[3]]){"Available observations"},
        if("Mean" %in% rv$entry[[3]]){"Mean"},
        if("Standard deviation" %in% rv$entry[[3]]){"Standard deviation"},
        if("Confidence intervals for mean" %in% rv$entry[[3]]){c("Mean - LCI", "Mean - UCI")},
        if("Median" %in% rv$entry[[3]]){"Median"},
        if("Quartiles" %in% rv$entry[[3]]){c("1st Qu.", "3rd Qu.")},
        if("Confidence intervals for median" %in% rv$entry[[3]]){c("Median - LCI", "Median - UCI")},
        if("Minimum" %in% rv$entry[[3]]){"Min."},
        if("Maximum" %in% rv$entry[[3]]){"Max."},
        if("Kurtosis" %in% rv$entry[[3]]){c("Kurtosis", "Kurtosis - LCI", "Kurtosis - UCI")},
        if("Skewness" %in% rv$entry[[3]]){c("Skewness", "Skewness - LCI", "Skewness - UCI")},
        if("Shapiro-Wilk test for normality" %in% rv$entry[[3]]){c("Shapiro-Wilk W-statistic", "Shapiro-Wilk p-value")},
        if("Kolmogrov-Smirnov test for normality" %in% rv$entry[[3]]){c("Kolmogrov-Smirnov D-statistic", "Kolmogrov-Smirnov p-value")}
      ))
    } else {
      # Test normality through skewness, kurtosis, Shapiro-Wilk test, and Kolmogrov-Smirnov test
      # Do this only for the main observations and not each subgroup
      # Skewness - the confidence intervals do not overlap (-0.5) to 0.5
      skewness_CI <- results[c("Skewness - LCI", "Skewness - UCI"),"All"]
      if ((is.na(skewness_CI[1])) | (is.na(skewness_CI[2]))) {
        skewness <- "Not possible to determine"
      } else if ((skewness_CI[2] < (-0.5)) | (skewness_CI[1] > 0.5)) {
        skewness <- "Non-normal"
      } else {
        skewness <- "No evidence that it is non-normal"
      }
      kurtosis_CI <- results[c("Kurtosis - LCI", "Kurtosis - UCI"),"All"]
      # Kurtosis (this is not excess kurtosis) does not overlap 3 (and entirely to the right of 3)
      if ((is.na(kurtosis_CI[1])) | (is.na(kurtosis_CI[2]))) {
        kurtosis <- "Not possible to determine"
      } else if (kurtosis_CI[1] > 3) {
        kurtosis <- "Non-normal"
      } else {
        kurtosis <- "No evidence that it is non-normal"
      }
      # Shapiro-Wilk test: This may not work for more than 5000 observations
      if (is.na(results["Shapiro-Wilk p-value","All"])) {
        if (results["Available observations", "All"] > 5000) {
          shapiro.wilk <- "No evidence that it is non-normal"
        } else {
          shapiro.wilk <- "Not possible to determine"
        }
      } else if (results["Shapiro-Wilk p-value","All"] <= 0.10) {
        shapiro.wilk <- "Non-normal"
      } else {
        shapiro.wilk <- "No evidence that it is non-normal"
      }
      # Shapiro-Wilk test: This may not work for more than 5000 observations
      if (is.na(results["Kolmogrov-Smirnov p-value","All"])) {
        ks <- "Not possible to determine"
      } else if (results["Kolmogrov-Smirnov p-value","All"] <= 0.10) {
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
      rows_to_keep <- as.vector(c(
        report_preferences$quantitative$EQUAL_STATS_choice_1,
        report_preferences$quantitative$EQUAL_STATS_choice_2,
        if (distribution == "Non-normal") {
          c("Median", "1st Qu.", "3rd Qu.")
        } else {
          c("Mean", "Standard deviation")
        },
        report_preferences$quantitative$EQUAL_STATS_choice_4
      ))
    }
    if (ncol(results) == 1) {
      results <- data.frame(All = results[rows_to_keep,], row.names = rows_to_keep)
    } else {
      results <- results[rows_to_keep,]
    }
  }
  results <- rbind.data.frame(
    c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, (ncol(results)-1))),
    c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")), rep(NA, (ncol(results)-1))),
    c("Successful", rep(NA, (ncol(results)-1))),
    c(rv$entry[[1]], rep(NA, (ncol(results)-1))),
    if (rv$entry[[1]] %in% rv$import_data$quantitative) {
      colnames(results)
    },
    results
  )
  row.names(results)[1:4] <- c("Analysis number" , "Analysis type" , "Analysis outcome" , "Variable name")
  if ((rv$entry[[1]] %in% rv$import_data$quantitative)) {row.names(results)[5] <- "Included rows"}
  results <- cbind.data.frame(
    parameters = row.names(results),
    results
  )
  results$parameters[substr(results$parameters,1, 7) == "Header_"] <- ""
  results$parameters[substr(results$parameters,1, 13) == "Column_names_"] <- ""
  results <- t(results)
  # For display in R-shiny app some modifications are necessary
  results_display <- data.frame(results, check.names = FALSE, check.rows = FALSE)
  results_display <- results_display[2:nrow(results_display),]
  numeric_rows <- setdiff(colnames(results_display), c("Analysis number", "Analysis outcome", "Analysis type","Variable name", "Included rows", report_preferences$categorical$mandatory_2))
  results_display[,numeric_rows] <- sapply(results_display[,numeric_rows], as.numeric)
  # List of plots
  # Nothing for this function
  plots_list <- ""
  plots_list_display <- ""
  analysis_outcome <- "Successful"
  display_table <- TRUE
  display_plot <- FALSE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list, plots_list_display = plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
