function.Multivariate_Regression <- function(Predefined_lists, rv){
  # Lists ####
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
    'rv$entry[[1]] <- ', ifelse(length(rv$entry[[1]]) > 1,
                                paste0('c("', paste0(rv$entry[[1]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[1]],'"')), '\n',
    'rv$entry[[2]] <- ', ifelse(length(rv$entry[[2]]) > 1,
                                paste0('c("', paste0(rv$entry[[2]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[2]],'"')), '\n',
    'rv$entry[[3]] <- ', ifelse(length(rv$entry[[3]]) > 1,
                                paste0('c("', paste0(rv$entry[[3]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[3]],'"')), '\n',
    'rv$entry[[4]] <- ', ifelse(length(rv$entry[[4]]) > 1,
                                paste0('c("', paste0(rv$entry[[4]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[4]],'"')), '\n',
    'rv$entry[[5]] <- ', ifelse(length(rv$entry[[5]]) > 1,
                                paste0('c("', paste0(rv$entry[[5]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[5]],'"')), '\n',
    'rv$entry[[6]] <- ', ifelse(length(rv$entry[[6]]) > 1,
                                paste0('c("', paste0(rv$entry[[6]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[6]],'"')), '\n',
    'rv$entry[[7]] <- ', ifelse(length(rv$entry[[7]]) > 1,
                                paste0('c("', paste0(rv$entry[[7]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[7]],'"')), '\n',
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
  func.keep_present_categories_only <- function(variable) {
    factors_in_variable <- levels(data[,variable])
    is.ordinal <- (variable %in%  rv$import_data$ordinal)
    factors_in_variable <- factors_in_variable[! is.na(match(factors_in_variable, data[,variable]))]
    factor(as.character(data[,variable]), levels = factors_in_variable, ordered = is.ordinal)
  }
  func.boxcox_transformation <- function(variable){BoxCox(data[,variable],  BoxCoxLambda(data[,variable], method = "loglik"))}
  func.gmc <- function(variable){data[,variable] - mean(data[,variable])}
  # Summary
  func.summary.categorical <- function(variable) {
    if(nlevels(variable) == 2) {
      summary <- data.frame(sapply(
        1:nlevels(variable), function(x) {
          as.numeric(BinomCI(length(variable[(!is.na(variable)) & variable == levels(variable)[x]]), length(variable[! is.na(variable)]), conf.level = 0.95))
        }
      ), check.names = FALSE)
      colnames(summary) <- levels(variable)
      row.names(summary) <- c("Proportion", "Proportion - LCI", "Proportion - UCI")
    } else {
      summary <- data.frame(t(MultinomCI(table(variable), conf.level = 0.95)),row.names = c("Proportion", "Proportion - LCI", "Proportion - UCI"), check.names = FALSE)
    }
    return(summary)
  }
  func.summary.quantitative <- function(variable) {
    summary <- list()
    if (length(variable) > 1) {
      summary$summary <- summary(variable)
      summary$sd <- sd(variable)
      summary$kurtosis <- suppressWarnings(try(Kurt(variable, conf.level = 0.95), silent = TRUE))
      if (str_detect(summary$kurtosis[[1]][1], "Error")) {
        summary$kurtosis <- c(NA, NA, NA)
      } else {
        summary$kurtosis <- summary$kurtosis + 3 # kurtosis provided is excess kurtosis. To get the actual kurtosis add 3
      }
      summary$skewness <- suppressWarnings(try(Skew(variable, conf.level = 0.95), silent = TRUE))
      if (str_detect(summary$skewness[[1]][1], "Error")) {
        summary$skewness <- c(NA, NA, NA)
      }
      summary$shapiro <- suppressWarnings(try(shapiro.test(variable), silent = TRUE))
      if (str_detect(summary$shapiro[[1]][1], "Error")) {
        summary$shapiro <- NA
      } else {
        summary$shapiro <- summary$shapiro$p.value
      }
      summary$ks <- suppressWarnings(ks.test(variable,
                                             "pnorm",
                                             mean=mean(variable),
                                             sd=sd(variable)
      ))
      summary$ks <- summary$ks$p.value
    } else {
      summary$summary <- c(variable[! is.na(variable)], NA, variable[! is.na(variable)], variable[! is.na(variable)], NA, variable[! is.na(variable)])
      names(summary$summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
      summary$sd <- NA
      summary$kurtosis <- c(NA, NA, NA)
      summary$skewness <- c(NA, NA, NA)
      summary$shapiro <- NA
      summary$ks <- NA
    }
    results <- data.frame(
      row.names = c(names(summary$summary),
                    "Standard deviation",
                    "Kurtosis", "Kurtosis - LCI", "Kurtosis - UCI", "Skewness", "Skewness - LCI", "Skewness - UCI",
                    "Shapiro-Wilk p-value", "Kolmogrov-Smirnov p-value"),
      summary = c(as.numeric(summary$summary),
                  summary$sd,
                  as.numeric(summary$kurtosis), as.numeric(summary$skewness),
                  as.numeric(summary$shapiro), as.numeric(summary$ks)
      )
    )
    results <- data.frame(
      summary = results[c("Min.", "Max.", "Mean", "Standard deviation", "Median", "1st Qu.", "3rd Qu.", "Kurtosis", "Kurtosis - LCI", "Kurtosis - UCI", "Skewness", "Skewness - LCI", "Skewness - UCI", "Shapiro-Wilk p-value", "Kolmogrov-Smirnov p-value"),],
      row.names = c("Min.", "Max.", "Mean", "Standard deviation", "Median", "1st Qu.", "3rd Qu.", "Kurtosis", "Kurtosis - LCI", "Kurtosis - UCI", "Skewness", "Skewness - LCI", "Skewness - UCI", "Shapiro-Wilk p-value", "Kolmogrov-Smirnov p-value"),
      check.rows = FALSE)
    return(results)
  }
  # Now with the data
  data <- rv$import_data$data[,c(rv$entry[[1]], if(rv$entry[[2]] != ""){rv$entry[[2]]}, if(TRUE %in% (rv$entry[[3]] != "")){rv$entry[[3]]}, if(TRUE %in% (rv$entry[[4]] != "")){rv$entry[[4]]})]
  # First complete case analysis
  data <- na.omit(data)
  # Note the variable types
  outcome <- rv$entry[[1]]
  mandatory_predictors <- rv$entry[[3]][rv$entry[[3]] != ""]
  optional_predictors <- rv$entry[[4]][rv$entry[[4]] != ""]
  variables_present <- c(outcome, mandatory_predictors, optional_predictors)
  categorical_present <- c(outcome, mandatory_predictors, optional_predictors)[c(outcome, mandatory_predictors, optional_predictors) %in% rv$import_data$categorical]
  categorical_present <- categorical_present[categorical_present != ""]
  # Keep only the present categories for categorical variables
  if (length(categorical_present) > 0) {data[,categorical_present] <- lapply(categorical_present, func.keep_present_categories_only)}
  quantitative_present <- c(outcome, mandatory_predictors, optional_predictors)[c(outcome, mandatory_predictors, optional_predictors) %in% rv$import_data$quantitative]
  quantitative_present <- quantitative_present[quantitative_present != ""]
  # Descriptive summary
  descriptive_summary <- lapply(1:length(variables_present), function(x){
    if (variables_present[x] %in% categorical_present) {
      summary <- t(func.summary.categorical(variable = data[,variables_present[x]]))
      summary <- suppressWarnings(cbind.data.frame(Variable = c(variables_present[x], rep(NA,nrow(summary)-1)),Categories = row.names(summary), summary,
                                                   `Min.` = NA, `Max.` = NA, `Mean` = NA, `Standard deviation` = NA, `Median` = NA, `1st Qu.` = NA, `3rd Qu.` = NA, `Kurtosis` = NA, `Kurtosis - LCI` = NA, `Kurtosis - UCI` = NA, `Skewness` = NA, `Skewness - LCI` = NA, `Skewness - UCI` = NA, `Shapiro-Wilk p-value` = NA, `Kolmogrov-Smirnov p-value` = NA
      ))
    } else {
      summary <- data.frame(t(func.summary.quantitative(variable = data[,variables_present[x]])), check.names = FALSE)
      summary <- suppressWarnings(cbind.data.frame(Variable = variables_present[x], `Categories` = NA, `Proportion` = NA, `Proportion - LCI` = NA, `Proportion - UCI` = NA, summary))
    }
    return(summary)
  })
  descriptive_summary <- suppressWarnings(do.call(rbind, descriptive_summary))
  descriptive_summary <- descriptive_summary[, do.call(c,lapply(1:ncol(descriptive_summary), function(x){if (length(descriptive_summary[,x][! is.na(descriptive_summary[,x])]) > 0){x}}))]
  # For managing the results, add an underscore to the end of all categorical independent variables
  mandatory_predictors[mandatory_predictors %in% categorical_present] <- paste0(mandatory_predictors[mandatory_predictors %in% categorical_present], "_")
  optional_predictors[optional_predictors %in% categorical_present] <- paste0(optional_predictors[optional_predictors %in% categorical_present], "_")
  colnames(data)[colnames(data) %in% setdiff(categorical_present, outcome)] <- paste0(colnames(data)[colnames(data) %in% setdiff(categorical_present, outcome)], "_")
  # Apply boxcox transformation and grand mean centering as appropriate
  if (rv$second_menu_choice != "EQUAL-STATS choice") {
    boxcox_transformation <- c(rv$entry[[5]])
    boxcox_transformation <- boxcox_transformation[boxcox_transformation != ""]
    if (length(boxcox_transformation) > 0) {data[,boxcox_transformation] <- lapply(boxcox_transformation, func.boxcox_transformation)}
    gmc <- c(rv$entry[[6]])
    gmc <- gmc[gmc !=""]
    if (length(gmc) > 0) {data[,gmc] <- lapply(gmc, func.gmc)}
  } else {
    # Transform the variables and check whether the kurtosis or skewness has improved by at least 10%, but not if it is counts
    quantitative_present_not_counts <- setdiff(quantitative_present, rv$import_data$counts)
    if (length(quantitative_present_not_counts) > 0) {
      check_boxcox <- data.frame(lapply(quantitative_present_not_counts, func.boxcox_transformation))
      colnames(check_boxcox) <- quantitative_present_not_counts
      untransformed_skewness <- sapply(1:length(quantitative_present_not_counts), function(x) {
        skewness <- suppressWarnings(try(Skew(data[,quantitative_present_not_counts[x]]), silent = TRUE))
        if (str_detect(skewness[[1]][1], "Error")) {
          NA
        } else {
          skewness
        }
      })
      transformed_skewness <- sapply(1:length(quantitative_present_not_counts), function(x) {
        skewness <- suppressWarnings(try(Skew(check_boxcox[,quantitative_present_not_counts[x]]), silent = TRUE))
        if (str_detect(skewness[[1]][1], "Error")) {
          NA
        } else {
          skewness
        }
      })
      percent_change_skewness <- abs((transformed_skewness - untransformed_skewness)/untransformed_skewness)
      untransformed_kurtosis <- sapply(1:length(quantitative_present_not_counts), function(x) {
        kurtosis <- suppressWarnings(try(Kurt(data[,quantitative_present_not_counts[x]]), silent = TRUE))
        if (str_detect(kurtosis[[1]][1], "Error")) {
          NA
        } else {
          kurtosis
        }
      })
      transformed_kurtosis <- sapply(1:length(quantitative_present_not_counts), function(x) {
        kurtosis <- suppressWarnings(try(Kurt(check_boxcox[,quantitative_present_not_counts[x]]), silent = TRUE))
        if (str_detect(kurtosis[[1]][1], "Error")) {
          NA
        } else {
          kurtosis
        }
      })
      percent_change_kurtosis <- abs((transformed_kurtosis - untransformed_kurtosis)/untransformed_kurtosis)
      boxcox_transformation <- quantitative_present_not_counts[(percent_change_skewness > 0.1) | (percent_change_kurtosis > 0.1)]
      if (length(boxcox_transformation) > 0) {data[,boxcox_transformation] <- lapply(boxcox_transformation, func.boxcox_transformation)}
      gmc <- setdiff(quantitative_present_not_counts, boxcox_transformation)
      if (length(gmc) > 0) {data[,gmc] <- lapply(gmc, func.gmc)}
    }
  }
  follow_up <- rv$entry[[2]][rv$entry[[2]] != ""]
  # Interaction and regression type
  if (rv$second_menu_choice != "EQUAL-STATS choice") {
    if (rv$entry[[7]] == "Yes") {operator <- '*'} else {operator <- '+'}
    regression_type <- rv$second_menu_choice
  } else {
    operator <- '*'
    regression_type <-
      if (rv$entry[[1]][1] %in% rv$import_data$binary) {
        "Logistic regression"
      } else if (rv$entry[[1]][1] %in% rv$import_data$ordinal) {
        "Ordinal logistic regression"
      } else if (rv$entry[[1]][1] %in% rv$import_data$nominal) {
        "Multinomial logistic regression"
      } else {
        if (is.null(rv$import_data$counts)) {
          "Linear regression"
        } else if (rv$entry[[1]][1] %in% rv$import_data$counts){
          "Poisson regression"
        } else {
          "Linear regression"
        }
      }
  }
  # Now the regression text
  # Create a data frame with texts to be inserted
  regression_reference <- list(
    regression_type = c("Linear regression", "Logistic regression", "Multinomial logistic regression", "Ordinal logistic regression", "Poisson regression", "Cox regression"),
    regression_prefix = c("lm(", "glm(", "multinom(", "polr(", "glm(", paste0("coxph(Surv(`", rv$entry[[2]], "`, as.numeric(")),
    stepwise_type = c("linear", "logit", "", "", "poisson", "cox"),
    outcome_end = c(" ~ ", " ~ ", " ~ ", " ~ ", " ~ ", ")) ~ "),
    data_prefix = c("", ", family = binomial", ", trace = FALSE",", Hess = TRUE", ", family = poisson",""),
    plots_list = list(list("correlation_plot", "Bland_Altman_plot", "actual_vs_predicted_plot" ,"residuals_vs_fitted", "normality_residuals", "standardised_residuals", "leverage", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "ROC_plot", "leverage", "composite_plot"),list("correlation_plot", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "composite_plot"),list("correlation_plot", "Bland_Altman_plot", "residuals_vs_fitted", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "ROC_plot", "residuals_vs_fitted", "composite_plot"))
  )
  # Outcome text
  if (length(rv$entry[[1]]) == 1) {
    outcome_text <- paste0("`", outcome, "`")
  } else {
    outcome_text <- paste0("cbind(", paste0("`", outcome, "`", collapse = ", "),")")
  }
  regression_text_1 <- paste0(
    regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
    outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
    ifelse(length(mandatory_predictors) == 0, 1, paste0("`", mandatory_predictors, "`", collapse = paste0(' ', operator, ' '))),
    regression_reference$data_prefix[regression_reference$regression_type == regression_type],
    ", data = data)"
  )[1]
  regression_results_1 <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_1)), silent = TRUE)))
  # After first regression ####
  if (TRUE %in% (str_detect(regression_results_1[[1]][1],"Error"))) {
    results <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      `Reason` = "The analysis did not run correctly even with mandatory variables. Please try after removing interactions or with fewer mandatory variables.",
      check.names = FALSE
    )
    results_display <- results
    plots_list <- ""
    plots_list_display <- ""
    display_table <- TRUE
    display_plot <- FALSE
    analysis_outcome <- "Unsuccessful"
  } else {
    if (is.null(regression_results_1$convergence)) {
      if (is.null(regression_results_1$converged)) {
        convergence <- TRUE
      } else {
        convergence <- regression_results_1$converged
      }
    } else {convergence <- (regression_results_1$convergence == 0)}
    if (convergence == FALSE){
      results <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Analysis outcome` = "Unsuccessful",
        `Reason` = "There was no convergence even with mandatory variables. Please try with fewer outcome variables, mandatory variables, or interactions.",
        check.names = FALSE
      )
      results_display <- results
      plots_list <- ""
      plots_list_display <- ""
      display_table <- TRUE
      display_plot <- FALSE
      analysis_outcome <- "Unsuccessful"
    } else {
      if (length(optional_predictors) == 0) {regression_results_2 <- regression_results_1} else {
        names_file <- cbind.data.frame(
          old_names = colnames(data),
          new_names = paste0("variable", formatC(1:ncol(data), flag = "0", digits = 3) ,"_"),
          old_names_with_gravel = paste0("`", colnames(data), "`")
        )
        names_file$type[names_file$old_names %in% outcome] <- "outcome"
        if (rv$entry[[2]] != "") {names_file$type[names_file$old_names == rv$entry[[2]]] <- "follow_up"}
        names_file$type[is.na(names_file$type)] <- "predictors"
        names_file$optional_predictor <- (names_file$old_names %in% optional_predictors)
        data_for_analysis <- data
        colnames(data_for_analysis) <- names_file$new_names
        mandatory_predictors_2 <- names_file$new_names[(names_file$type == "predictors") & (names_file$optional_predictor == FALSE)]
        optional_predictors_2 <- names_file$new_names[(names_file$type == "predictors") & (names_file$optional_predictor == TRUE)]
        regression_interim_text_1 <- lapply(1:length(outcome),function(x) {
          paste0(
            regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
            paste0("variable",formatC(x, flag = "0", digits = 3) ,"_"), regression_reference$outcome_end[regression_reference$regression_type == regression_type],
            ifelse(length(mandatory_predictors_2) == 0, 1, paste0(mandatory_predictors_2, collapse = ' + ')),
            regression_reference$data_prefix[regression_reference$regression_type == regression_type],
            ", data = data_for_analysis)"
          )[1]
        })
        regression_interim_text_2 <- lapply(1:length(outcome),function(x) {
          paste0(
            "stats::step(regression_interim_results_1[[",x, "]], scope = list(upper = ~ ",
            paste0("`", c(mandatory_predictors_2, optional_predictors_2), "`", collapse = paste0(" ", operator, " ")), ", ",
            "lower = ~ ", ifelse(length(mandatory_predictors_2) == 0, 1, paste0("`", mandatory_predictors_2, "`", collapse = ' + ')),
            "), trace = FALSE)"
          )
        })
        regression_interim_results_1 <- lapply(1:length(outcome),function(x) {suppressMessages(suppressWarnings(try(eval(parse(text = regression_interim_text_1[[x]])), silent = TRUE)))})
        regression_interim_results_2 <- lapply(1:length(outcome),function(x) {suppressMessages(suppressWarnings(try(eval(parse(text = regression_interim_text_2[[x]])), silent = TRUE)))})
        regression_interim_results <- lapply(1:length(outcome),function(x) {
          if ((TRUE %in% (str_detect(regression_interim_results_1[[x]][[1]][1],"Error"))) |
              (TRUE %in% (str_detect(regression_interim_results_2[[x]][[1]][1],"Error")))
          ) {
            output <- "Error"
          } else {
            if (regression_type == "Multinomial logistic regression") {
              output <- regression_interim_results_2[[x]]$coefnames
              output <- output[substr(output,1,8) == "variable"]
            } else {
              output <- regression_interim_results_2[[x]]$coefficients
              output <- names(output[substr(names(output),1,8) == "variable"])
            }
          }
        })
        regression_interim_results <- do.call(c,regression_interim_results)
        # Remove the parts dealing with the levels
        variables_present <- cbind.data.frame(
          variables_present = regression_interim_results,
          interactions = str_detect(regression_interim_results, ":")
        )
        variables_present$old_names[variables_present$interactions == FALSE] <- names_file$old_names[as.numeric(substr(variables_present$variables_present[variables_present$interactions == FALSE], 9,12))]
        non_interaction_terms <- paste0("`",unique(variables_present$old_names[variables_present$interactions == FALSE]) ,"`")
        interaction_terms <- variables_present$variables_present[variables_present$interactions == TRUE]
        if (length(interaction_terms) > 0) {
          interaction_terms <- sapply(1:length(interaction_terms), function(x) {
            terms_split <- str_split(interaction_terms[x], ":")[[1]]
            paste0("(",paste0("`", names_file$old_names[as.numeric(substr(terms_split, 9,12))], "`",collapse = " * ") ,")")
          })
        }
        regression_text_2 <- paste0(
          regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
          outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
          paste0(c(non_interaction_terms, interaction_terms), collapse = " + "),
          regression_reference$data_prefix[regression_reference$regression_type == regression_type],
          ", data = data)"
        )[1]
        regression_results_2 <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_2)), silent = TRUE)))
        if (TRUE %in% (str_detect(regression_results_2[[1]][1],"Error"))) {
          regression_results_2 <- regression_results_1
        } else {
          if (is.null(regression_results_2$convergence)) {
            if (is.null(regression_results_2$converged)) {
              convergence <- TRUE
            } else {
              convergence <- regression_results_2$converged
            }
          } else {
            convergence <- (regression_results_2$convergence == 0)
          }
          if (convergence == FALSE) {
            regression_results_2 <- regression_results_1
          }
        }
      }
      # Get the coefficients
      summary_regression_results_2 <- summary(regression_results_2)
      if (regression_type == "Linear regression") {
        regression_coefficients <- lapply(1:length(summary_regression_results_2), function(x) {
          regression_coefficients <- data.frame(summary_regression_results_2[[x]]$coefficients, check.rows = FALSE, check.names = FALSE)
          regression_coefficients <- cbind.data.frame(
            `Response variable` = str_remove_all(names(summary_regression_results_2)[x], "Response "),
            `Variable` = str_remove_all(str_replace_all(row.names(regression_coefficients), fixed("(Intercept)"), "Intercept"),"`"),
            regression_coefficients
          )
        })
        names(regression_coefficients) <- outcome
      } else {
        regression_coefficients <- data.frame(summary_regression_results_2$coefficients, check.rows = FALSE, check.names = FALSE)
        regression_coefficients <- cbind.data.frame(
          `Variable` = str_remove_all(str_replace_all(row.names(regression_coefficients), fixed("(Intercept)"), "Intercept"),"`"),
          regression_coefficients
        )
      }
      # Further processing depends upon the different regression types
      if (regression_type == "Linear regression") {
        results_step_1 <- lapply(1:length(summary_regression_results_2), function(x) {
          regression_coefficients <- cbind.data.frame(
            regression_coefficients[[x]][,1:4],
            `Estimate lower conf. int` = regression_coefficients[[x]]$Estimate - qnorm(0.975) * regression_coefficients[[x]]$`Std. Error`,
            `Estimate upper conf. int` = regression_coefficients[[x]]$Estimate + qnorm(0.975) * regression_coefficients[[x]]$`Std. Error`,
            `t value` = regression_coefficients[[x]][,5],
            `P value` = regression_coefficients[[x]][,6]
          )
          actual_versus_predicted <- cbind.data.frame(Actual = data[,outcome[x]], Predicted = fitted.values(regression_results_2)[,x])
          concordance_correlation_coefficient <- CCC(actual_versus_predicted[,1], actual_versus_predicted[,2], ci = "z-transform", conf.level = 0.95, na.rm= TRUE)
          correlation_coefficient <- concordance_correlation_coefficient$rho.c
          colnames(correlation_coefficient) <- c("Concordance correlation coefficient - Point estimate","Concordance correlation coefficient - LCI", "Concordance correlation coefficient - UCI")
          bland_altman <- concordance_correlation_coefficient$blalt
          mean_difference <- mean(bland_altman$delta)
          se_difference <- (var(bland_altman$delta))^0.5
          regression_fit_statistics <- cbind.data.frame(
            `Residual Standard Error` = summary_regression_results_2[[x]]$sigma,
            `Degrees of freedom (Residual Standard Error)` = summary_regression_results_2[[x]]$df[2],
            `Adjusted R-square` = summary_regression_results_2[[x]]$adj.r.squared,
            `F statistic` = summary_regression_results_2[[x]]$fstatistic[1],
            `Degrees of freedom (F statistic) ` = paste0(summary_regression_results_2[[x]]$fstatistic[2], "; ", summary_regression_results_2[[x]]$fstatistic[3]),
            `P value` = pf(summary_regression_results_2[[x]]$fstatistic[1], summary_regression_results_2[[x]]$fstatistic[2], summary_regression_results_2[[x]]$fstatistic[3], lower.tail = FALSE, log.p = FALSE),
            correlation_coefficient
          )
          regression_coefficients <- cbind.data.frame(regression_coefficients, regression_fit_statistics)
          if (nrow(regression_coefficients) > 1) {
            regression_coefficients[2:nrow(regression_coefficients), (colnames(regression_coefficients) %in% colnames(regression_fit_statistics))] <- NA
          }
          list(regression_coefficients = regression_coefficients, bland_altman = bland_altman, mean_difference = mean_difference, se_difference = se_difference, actual_versus_predicted = actual_versus_predicted)
        })
        names(results_step_1) <- outcome
        regression_coefficients <- do.call(rbind.data.frame,lapply(1:length(summary_regression_results_2), function(x) {
          results_step_1[[x]]$regression_coefficients
        }))
      } else if (regression_type == "Logistic regression") {
        regression_coefficients <- data.frame(summary_regression_results_2$coefficients, check.rows = FALSE, check.names = FALSE)
        regression_coefficients <- cbind.data.frame(
          `Variable` = str_remove_all(str_replace_all(row.names(regression_coefficients), fixed("(Intercept)"), "Intercept"),"`"),
          regression_coefficients
        )
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Odds ratio` = exp(regression_coefficients$Estimate),
          `Odds ratio lower conf. int` = exp(regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`),
          `Odds ratio upper conf. int` = exp(regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`),
          `z value` = regression_coefficients[,4],
          `P value` = regression_coefficients[,5]
        )
        # The null model to find the overall P value
        regression_text_null_model <- paste0(
          regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
          outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
          1,
          regression_reference$data_prefix[regression_reference$regression_type == regression_type],
          ", data = data)"
        )[1]
        regression_results_null_model <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_null_model)), silent = TRUE)))
        model_comparison <- suppressMessages(suppressWarnings(try(anova(regression_results_null_model, regression_results_2, test = 'Chisq'), silent = TRUE)))
        if (str_detect(model_comparison[[1]][1], "Error")) {
          model_comparison <- suppressMessages(suppressWarnings(try(lrtest(regression_results_null_model, regression_results_2), silent = TRUE)))
          if (str_detect(model_comparison[[1]][1], "Error")) {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = "Not able to compare the models",
              `Degrees of freedom (LR statistic)` = "Not able to compare the models",
              `P value` = "Not able to compare the models"
            )
          } else {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = model_comparison$Chisq[2],
              `Degrees of freedom (LR statistic)` = model_comparison$Df[2],
              `P value` = model_comparison$`Pr(>Chisq)`[2]
            )
          }
        } else {
          regression_fit_statistics <- cbind.data.frame(
            `Residual deviance` = model_comparison$`Resid. Dev`[2],
            `Degrees of freedom (Residual deviance)` = model_comparison$`Resid. Df`[2],
            `AIC` = AIC(regression_results_2),
            `BIC` = BIC(regression_results_2),
            `-2 Log likelihood` = unlist(logLik(regression_results_2)),
            `P value` = model_comparison$`Pr(>Chi)`[2]
          )
        }
        # Predictions
        predicted_probability <- predict(regression_results_2, type="response")
        AUROC <- suppressWarnings(suppressMessages(try(roc(data[,1], predicted_probability, ci = TRUE, ci.alpha = 0.95, print.auc=TRUE), silent = TRUE)))
        if (str_detect(AUROC[[1]][1], "Error")) {
          actual_vs_predicted <- data.frame(Predictions = "Not able to predict because of difficulty in finding optimal threshold", check.names = FALSE)
        } else {
          optimal_threshold <- coords(AUROC, "best", ret="threshold")
          predicted_values <- data.frame(
            `Actual values` = data[,1],
            `Predicted values` = as.factor(sapply(1:length(predicted_probability), function(x) {
              eval(parse(text = paste0(
                'ifelse(predicted_probability[x]',AUROC$direction,'optimal_threshold, levels(data[,1])[1], levels(data[,1])[2])'
              )))
            })),
            check.names = FALSE
          )
          actual_vs_predicted <- data.frame(unclass(table(predicted_values)), check.names = FALSE)
          actual_vs_predicted <- cbind.data.frame(
            colnames(actual_vs_predicted),
            actual_vs_predicted
          )
          colnames(actual_vs_predicted) <- c(paste0("Actual ", rv$entry[[1]][length(rv$entry[[1]])] ), paste0("Predicted as ", colnames(actual_vs_predicted)[2:ncol(actual_vs_predicted)]))
          AUROC <- suppressWarnings(suppressMessages(try(roc(as.numeric(predicted_values$`Actual values`), as.numeric(predicted_values$`Predicted values`), ci = TRUE, ci.alpha = 0.95, print.auc=TRUE), silent = TRUE)))
        }
      } else if (regression_type == "Multinomial logistic regression") {
        regression_coefficients <- t(summary_regression_results_2$coefficients)
        # Standard errors
        regression_se <- t(summary_regression_results_2$standard.errors)
        # Odds ratios
        regression_odds_ratio <- lapply(1:ncol(regression_coefficients), function(x) {
          odds_ratio <- cbind.data.frame(
            `Odds ratio` = exp(regression_coefficients[,x]),
            `Odds ratio lower conf. int` = exp(regression_coefficients[,x] - qnorm(0.975) * regression_se[,x]),
            `Odds ratio upper conf. int` = exp(regression_coefficients[,x] + qnorm(0.975) * regression_se[,x])
          )
          colnames(odds_ratio) <- paste0(colnames(odds_ratio), "_", colnames(regression_coefficients)[x])
          return(odds_ratio)
        })
        regression_odds_ratio <- do.call(cbind.data.frame, regression_odds_ratio)
        # P values
        regression_p_value <- lapply(1:ncol(regression_coefficients), function(x) {
          p_values <- cbind.data.frame(
            `P value` = (1 - pnorm(abs(regression_coefficients[,x]/regression_se[,x]), 0, 1)) * 2
          )
          colnames(p_values) <- paste0(colnames(p_values), "_", colnames(regression_coefficients)[x])
          return(p_values)
        })
        regression_p_value <- do.call(cbind.data.frame, regression_p_value)
        colnames(regression_coefficients) <- paste0("Estimate_", colnames(regression_coefficients))
        colnames(regression_se) <- paste0("se_", colnames(regression_se))
        regression_coefficients <- cbind.data.frame(
          Variable = str_remove_all(str_replace_all(row.names(regression_coefficients), fixed("(Intercept)"), "Intercept"),"`"),
          regression_coefficients,
          regression_se,
          regression_odds_ratio,
          regression_p_value
        )
        # The null model to find the overall P value
        regression_text_null_model <- paste0(
          regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
          outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
          1,
          regression_reference$data_prefix[regression_reference$regression_type == regression_type],
          ", data = data)"
        )[1]
        regression_results_null_model <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_null_model)), silent = TRUE)))
        model_comparison <- suppressMessages(suppressWarnings(try(anova(regression_results_null_model, regression_results_2, test = 'Chisq'), silent = TRUE)))
        if (str_detect(model_comparison[[1]][1], "Error")) {
          model_comparison <- suppressMessages(suppressWarnings(try(lrtest(regression_results_null_model, regression_results_2), silent = TRUE)))
          if (str_detect(model_comparison[[1]][1], "Error")) {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = "Not able to compare the models",
              `Degrees of freedom (LR statistic)` = "Not able to compare the models",
              `P value` = "Not able to compare the models"
            )
          } else {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = ifelse(is.null(model_comparison$Chisq[2]), model_comparison$`Pr(Chi)`,model_comparison$Chisq[2]),
              `Degrees of freedom (LR statistic)` = ifelse(is.null(model_comparison$Df[2]), model_comparison$`   Df`[2],model_comparison$Df[2]),
              `P value` = ifelse(is.null(model_comparison$`Pr(>Chisq)`[2]), model_comparison$`Pr(Chi)`[2],model_comparison$`Pr(>Chisq)`[2])
            )
          }
        } else {
          regression_fit_statistics <- cbind.data.frame(
            `Residual deviance` = model_comparison$`Resid. Dev`[2],
            `Degrees of freedom (Residual deviance)` = model_comparison$`Resid. df`[2],
            `AIC` = AIC(regression_results_2),
            `BIC` = BIC(regression_results_2),
            `-2 Log likelihood` = unlist(logLik(regression_results_2)),
            `LR chi-square statistic` = model_comparison$`LR stat.`[2],
            `Degrees of freedom (LR statistic)` = model_comparison$`   Df`[2],
            `P value` = model_comparison$`Pr(Chi)`[2]
          )
        }
      } else if (regression_type == "Ordinal logistic regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Odds ratio` = exp(regression_coefficients$Value),
          `Odds ratio lower conf. int` = exp(regression_coefficients$Value - qnorm(0.975) * regression_coefficients$`Std. Error`),
          `Odds ratio upper conf. int` = exp(regression_coefficients$Value + qnorm(0.975) * regression_coefficients$`Std. Error`),
          `t value` = regression_coefficients[,4],
          `P value` = pnorm(abs(regression_coefficients[, 3]), lower.tail = FALSE) * 2
        )
        # The null model to find the overall P value
        regression_text_null_model <- paste0(
          regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
          outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
          1,
          regression_reference$data_prefix[regression_reference$regression_type == regression_type],
          ", data = data)"
        )[1]
        regression_results_null_model <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_null_model)), silent = TRUE)))
        model_comparison <- suppressMessages(suppressWarnings(try(anova(regression_results_null_model, regression_results_2, test = 'Chisq'), silent = TRUE)))
        if (str_detect(model_comparison[[1]][1], "Error")) {
          model_comparison <- suppressMessages(suppressWarnings(try(lrtest(regression_results_null_model, regression_results_2), silent = TRUE)))
          if (str_detect(model_comparison[[1]][1], "Error")) {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = "Not able to compare the models",
              `Degrees of freedom (LR statistic)` = "Not able to compare the models",
              `P value` = "Not able to compare the models"
            )
          } else {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = model_comparison$Chisq[2],
              `Degrees of freedom (LR statistic)` = model_comparison$Df[2],
              `P value` = model_comparison$`Pr(>Chisq)`[2]
            )
          }
        } else {
          regression_fit_statistics <- cbind.data.frame(
            `Residual deviance` = model_comparison$`Resid. Dev`[2],
            `Degrees of freedom (Residual deviance)` = model_comparison$`Resid. df`[2],
            `AIC` = AIC(regression_results_2),
            `BIC` = BIC(regression_results_2),
            `-2 Log likelihood` = unlist(logLik(regression_results_2)),
            `LR chi-square statistic` = model_comparison$`LR stat.`[2],
            `Degrees of freedom (LR statistic)` = model_comparison$`   Df`[2],
            `P value` = model_comparison$`Pr(Chi)`[2]
          )
        }
        # Predictions
        predicted_values <- data.frame(
          `Actual values` = data[,1],
          `Predicted values` = predict(regression_results_2, newdata = data, "class"),
          check.names = FALSE
        )
        actual_vs_predicted <- data.frame(unclass(table(predicted_values)), check.names = FALSE)
        actual_vs_predicted <- cbind.data.frame(
          colnames(actual_vs_predicted),
          actual_vs_predicted
        )
        colnames(actual_vs_predicted) <- c(paste0("Actual ", rv$entry[[1]]), paste0("Predicted as ", colnames(actual_vs_predicted)[2:ncol(actual_vs_predicted)]))
      } else if (regression_type == "Poisson regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Rate ratio` = exp(regression_coefficients$Estimate),
          `Rate ratio lower conf. int` = exp(regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`),
          `Rate ratio upper conf. int` = exp(regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`),
          `z value` = regression_coefficients[,4],
          `P value` = regression_coefficients[,5]
        )
        # The null model to find the overall P value
        regression_text_null_model <- paste0(
          regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
          outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
          1,
          regression_reference$data_prefix[regression_reference$regression_type == regression_type],
          ", data = data)"
        )[1]
        regression_results_null_model <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_null_model)), silent = TRUE)))
        model_comparison <- suppressMessages(suppressWarnings(try(anova(regression_results_null_model, regression_results_2, test = 'Chisq'), silent = TRUE)))
        if (str_detect(model_comparison[[1]][1], "Error")) {
          model_comparison <- suppressMessages(suppressWarnings(try(lrtest(regression_results_null_model, regression_results_2), silent = TRUE)))
          if (str_detect(model_comparison[[1]][1], "Error")) {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = "Not able to compare the models",
              `Degrees of freedom (LR statistic)` = "Not able to compare the models",
              `P value` = "Not able to compare the models"
            )
          } else {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = model_comparison$Chisq[2],
              `Degrees of freedom (LR statistic)` = model_comparison$Df[2],
              `P value` = model_comparison$`Pr(>Chisq)`[2]
            )
          }
        } else {
          regression_fit_statistics <- cbind.data.frame(
            `Residual deviance` = model_comparison$`Resid. Dev`[2],
            `Degrees of freedom (Residual deviance)` = model_comparison$`Resid. Df`[2],
            `AIC` = AIC(regression_results_2),
            `BIC` = BIC(regression_results_2),
            `-2 Log likelihood` = unlist(logLik(regression_results_2)),
            `P value` = model_comparison$`Pr(>Chi)`[2]
          )
        }
        actual_versus_predicted <- cbind.data.frame(Actual = data[,1], Predicted = regression_results_2$fitted.values)
        concordance_correlation_coefficient <- CCC(actual_versus_predicted[,1], actual_versus_predicted[,2], ci = "z-transform", conf.level = 0.95, na.rm= TRUE)
        correlation_coefficient <- concordance_correlation_coefficient$rho.c
        colnames(correlation_coefficient) <- c("Concordance correlation coefficient - Point estimate","Concordance correlation coefficient - LCI", "Concordance correlation coefficient - UCI")
        bland_altman <- concordance_correlation_coefficient$blalt
        mean_difference <- mean(bland_altman$delta)
        se_difference <- (var(bland_altman$delta))^0.5
        regression_fit_statistics <- cbind.data.frame(regression_fit_statistics, correlation_coefficient)
      } else if (regression_type == "Cox regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,c(1,2,4)],
          `Hazard ratio` = exp(regression_coefficients$coef),
          `Hazard ratio lower conf. int` = exp(regression_coefficients$coef - qnorm(0.975) * regression_coefficients$`se(coef)`),
          `Hazard ratio upper conf. int` = exp(regression_coefficients$coef + qnorm(0.975) * regression_coefficients$`se(coef)`),
          `z value` = regression_coefficients[,5],
          `P value` = regression_coefficients[,6]
        )
        # The null model to find the overall P value
        regression_text_null_model <- paste0(
          regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
          outcome_text, regression_reference$outcome_end[regression_reference$regression_type == regression_type],
          1,
          regression_reference$data_prefix[regression_reference$regression_type == regression_type],
          ", data = data)"
        )[1]
        regression_results_null_model <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_null_model)), silent = TRUE)))
        model_comparison <- suppressMessages(suppressWarnings(try(anova(regression_results_null_model, regression_results_2, test = 'Chisq'), silent = TRUE)))
        if (str_detect(model_comparison[[1]][1], "Error")) {
          model_comparison <- suppressMessages(suppressWarnings(try(lrtest(regression_results_null_model, regression_results_2), silent = TRUE)))
          if (str_detect(model_comparison[[1]][1], "Error")) {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = "Not able to compare the models",
              `Degrees of freedom (LR statistic)` = "Not able to compare the models",
              `Likelihood ratio test P value` = "Not able to compare the models",
              `Log rank test P value` = summary_regression_results_2$sctest[3],
              `Wald test P value` = summary_regression_results_2$waldtest[3]
            )
          } else {
            regression_fit_statistics <- cbind.data.frame(
              `AIC` = AIC(regression_results_2),
              `BIC` = BIC(regression_results_2),
              `-2 Log likelihood` = unlist(logLik(regression_results_2)),
              `LR chi-square statistic` = model_comparison$Chisq[2],
              `Degrees of freedom (LR statistic)` = model_comparison$Df[2],
              `Likelihood ratio test P value` = model_comparison$`Pr(>Chisq)`[2],
              `Log rank test P value` = summary_regression_results_2$sctest[3],
              `Wald test P value` = summary_regression_results_2$waldtest[3]
            )
          }
        } else {
          regression_fit_statistics <- cbind.data.frame(
            `AIC` = AIC(regression_results_2),
            `BIC` = BIC(regression_results_2),
            `-2 Log likelihood` = unlist(logLik(regression_results_2)),
            `LR chi-square statistic` = model_comparison$Chisq[2],
            `Degrees of freedom (LR statistic)` = model_comparison$Df[2],
            `Likelihood ratio test P value` = model_comparison$`Pr(>|Chi|)`[2],
            `Log rank test P value` = summary_regression_results_2$sctest[3],
            `Wald test P value` = summary_regression_results_2$waldtest[3]
          )
        }
        # Predictions
        predicted_probability <- predict(regression_results_2, type="survival")
        AUROC <- suppressWarnings(suppressMessages(try(roc(data[,1], predicted_probability, ci = TRUE, ci.alpha = 0.95, print.auc=TRUE), silent = TRUE)))
        if (str_detect(AUROC[[1]][1], "Error")) {
          actual_vs_predicted <- data.frame(Predictions = "Not able to predict because of difficulty in finding optimal threshold", check.names = FALSE)
        } else {
          optimal_threshold <- coords(AUROC, "best", ret="threshold")
          predicted_values <- data.frame(
            `Actual values` = data[,1],
            `Predicted values` = as.factor(sapply(1:length(predicted_probability), function(x) {
              eval(parse(text = paste0(
                'ifelse(predicted_probability[x]',AUROC$direction,'optimal_threshold, levels(data[,1])[1], levels(data[,1])[2])'
              )))
            })),
            check.names = FALSE
          )
          actual_vs_predicted <- data.frame(unclass(table(predicted_values)), check.names = FALSE)
          actual_vs_predicted <- cbind.data.frame(
            colnames(actual_vs_predicted),
            actual_vs_predicted
          )
          colnames(actual_vs_predicted) <- c(paste0("Actual ", rv$entry[[1]]), paste0("Predicted as ", colnames(actual_vs_predicted)[2:ncol(actual_vs_predicted)]))
          AUROC <- suppressWarnings(suppressMessages(try(roc(as.numeric(predicted_values$`Actual values`), as.numeric(predicted_values$`Predicted values`), ci = TRUE, ci.alpha = 0.95, print.auc=TRUE), silent = TRUE)))
        }
      }
      # Create a version for display with numbers rounded to 4 digits
      descriptive_summary_display <- descriptive_summary
      regression_coefficients_display <- regression_coefficients
      descriptive_summary_display[,sapply(descriptive_summary_display, is.numeric)] <- sapply(descriptive_summary_display[,sapply(descriptive_summary_display, is.numeric)], function(x){round(x,4)})
      regression_coefficients_display[,sapply(regression_coefficients_display, is.numeric)] <- sapply(regression_coefficients_display[,sapply(regression_coefficients_display, is.numeric)], function(x){round(x,4)})
      # General description
      general_description <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Regression type` = regression_type,
        `Analysis outcome` = "Successful",
        `Total number of observations` = nrow(rv$import_data$data),
        `Number of complete observations` = nrow(data),
        Outcome = paste0(rv$entry[[1]], collapse = "; "),
        check.names = FALSE
      )
      if ((regression_type == "Logistic regression") | (regression_type == "Ordinal logistic regression") | (regression_type == "Cox regression")) {
        regression_fit_statistics_display <- regression_fit_statistics
        results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, regression_fit_statistics, actual_vs_predicted))
        results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, regression_fit_statistics_display, actual_vs_predicted))
        plot_title_combined <- ggdraw() + draw_label(paste0(rv$entry[[1]][length(rv$entry[[1]])], '(', regression_type, ')'), color="darkgreen", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
      } else if (regression_type == "Multinomial logistic regression") {
        regression_fit_statistics_display <- regression_fit_statistics
        results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, regression_fit_statistics))
        results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, regression_fit_statistics_display))
        plot_title_combined <- ggdraw() + draw_label(paste0(rv$entry[[1]][length(rv$entry[[1]])], '(', regression_type, ')'), color="darkgreen", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
      } else {
        results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients))
        results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display))
      }
      # Plots ####
      if (length(quantitative_present) > 1) {
        data_correlation <- data[,quantitative_present]
        correlation <- cor(data_correlation, method = "pearson")
        p.mat <- cor_pmat(data_correlation)
        correlation_plot <- ggcorrplot(correlation, method = "circle", lab = TRUE, p.mat = p.mat, outline.color = "white") + theme_classic()  + theme(plot.title = element_text(color="navyblue", size=12, face="bold", hjust = 0.5), axis.text.x = element_text(hjust =0)) + ggtitle("Correlation plot") + scale_y_discrete(limits=rev) + scale_x_discrete(position = "top") + geom_text(aes(x = 0, y = 0, label ="* Numbers indicate correlation coefficient."), size = 2, hjust=0, vjust =-2.5)+ geom_text(aes(x = 0, y = 0, label ="**Crossed out numbers (if any) indicate that the correlation was not significant."), size = 2, hjust = 0, vjust = -1) + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
      } else {
        correlation_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = "There were fewer than two quantitative variables.\nSo correlation plot was not created"), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
      }
      # Diagnostics: This differs for different regressions
      if (regression_type == "Linear regression") {
        plots_each_outcome <- list()
        for (x in 1:length(summary_regression_results_2)) {
          plots_each_outcome[[x]] <- list()
          plots_each_outcome[[x]]$plot_title_combined <- ggdraw() + draw_label(paste0(rv$entry[[1]][x], '(', regression_type, ')'), color="darkgreen", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
          plots_each_outcome[[x]]$bland_altman <- results_step_1[[x]]$bland_altman
          plots_each_outcome[[x]]$mean_difference <- results_step_1[[x]]$mean_difference
          plots_each_outcome[[x]]$se_difference <- results_step_1[[x]]$se_difference
          plots_each_outcome[[x]]$actual_versus_predicted <- results_step_1[[x]]$actual_versus_predicted
          plots_each_outcome[[x]]$residuals_df <- cbind.data.frame(residuals = residuals(regression_results_2)[,x], standardised_residuals = rstandard(regression_results_2)[,x], fitted = regression_results_2$fitted.values[,x], hatvalues = hatvalues(regression_results_2), cooks.distance = cooks.distance(regression_results_2)[,x])
          plots_each_outcome[[x]]$Bland_Altman_plot <- ggplot(data = plots_each_outcome[[x]]$bland_altman,aes(x = plots_each_outcome[[x]]$bland_altman[,1], y = plots_each_outcome[[x]]$bland_altman[,2]))+ geom_point() + xlab("Means") + ylab("Differences") + ylim(round_near(min(plots_each_outcome[[x]]$bland_altman[,2])*0.5),round_near(max(plots_each_outcome[[x]]$bland_altman[,2])*2)) + geom_hline(yintercept=plots_each_outcome[[x]]$mean_difference, lty = 1, col = "gray") + geom_hline(yintercept=plots_each_outcome[[x]]$mean_difference - (2 * plots_each_outcome[[x]]$se_difference), lty = 2, col = "gray") + geom_hline(yintercept=plots_each_outcome[[x]]$mean_difference + (2 * plots_each_outcome[[x]]$se_difference), lty = 2, col = "gray") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle("Actual versus predicted: Bland-Altman plot")
          plots_each_outcome[[x]]$actual_vs_predicted_plot <- suppressMessages(suppressWarnings(try(ggplot(data = plots_each_outcome[[x]]$actual_versus_predicted, aes(x= Actual, y = Predicted)) + geom_point() + ggtitle("Predicted versus actual values") + theme_classic() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)), silent = TRUE)))
          plots_each_outcome[[x]]$residuals_vs_fitted <- ggplot(data=plots_each_outcome[[x]]$residuals_df, aes(x=fitted, y=residuals)) + geom_point() + stat_smooth(method="loess") + geom_hline(yintercept=0, linetype="dashed") + xlab("Fitted values") + ylab("Residuals") + ggtitle("Residuals versus Fitted") + theme(plot.title = element_text(hjust=0.5))
          plots_each_outcome[[x]]$normality_residuals <- ggplot(data=plots_each_outcome[[x]]$residuals_df, aes(sample = standardised_residuals)) + stat_qq() +  stat_qq_line(linetype=2, color="grey")+ ggtitle("Q-Q plot") + theme(plot.title = element_text(hjust=0.5)) + xlab("Theoretical quantiles")+ ylab("Sample quantiles")
          plots_each_outcome[[x]]$standardised_residuals <- ggplot(data=plots_each_outcome[[x]]$residuals_df, aes(x= fitted, y=sqrt(abs(standardised_residuals)))) + geom_point(na.rm=TRUE) + stat_smooth(method="loess", na.rm = TRUE) + xlab("Fitted Values") + ylab(expression(sqrt("Standardised residuals"))) + ggtitle("Scale-Location") + theme(plot.title = element_text(hjust=0.5))
          plots_each_outcome[[x]]$leverage <- ggplot(data=plots_each_outcome[[x]]$residuals_df, aes(x= hatvalues, y= standardised_residuals)) + geom_point(aes(size= cooks.distance), na.rm=TRUE) + stat_smooth(method="loess", na.rm=TRUE) + xlab("Leverage") + ylab("Standardised Residuals") + ggtitle("Residuals vs Leverage") + theme(plot.title = element_text(hjust=0.5)) + labs(size="Cook's distance")
          plots_each_outcome[[x]]$correlation_plot <- correlation_plot
          plots_each_outcome[[x]]$composite_plot <- ggdraw() + draw_label("There are multiple outcomes.\nTherefore no composite plot\nhas been created.\nThe individual plots for each outcome\n are available for download.", color="darkgreen", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
        }
      } else if (regression_type == "Logistic regression") {
        if (str_detect(AUROC[[1]][1], "Error")) {
          ROC_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = "There was difficulty in predicting the optimal threshold.\nTherefore, it is not possible to create the ROC plot."), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
          actual_vs_predicted_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = "There was difficulty in predicting the optimal threshold.\nTherefore, it is not possible to create the actual versus predicted plot."), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
        } else {
          ROC_plot <- ggroc(AUROC) + ggtitle("Prediction accuracy") + geom_abline(aes(slope=1,intercept=1),linetype=2) +  geom_text(aes(x = 0.3, y = 0.2, label = paste0('AUROC:', "\n",formatC(AUROC$ci[2], digits = 2, format = "f"),' (', formatC(AUROC$ci[1], digits = 2, format = "f"), ', ', formatC(AUROC$ci[3], digits = 2, format = "f"), ')')), size = 2, hjust=0, vjust =-1)
          frequencies <- data.frame(table(predicted_values))
          frequencies <- frequencies[frequencies$Freq > 0,]
          colour_list <- as.vector(ifelse(frequencies$Actual.values == frequencies$Predicted.values,"darkred","darkgreen"))
          actual_vs_predicted_plot <- ggplot(frequencies, aes(x = Actual.values, y = Predicted.values, size = Freq, colour = colour_list)) + geom_point() + xlab(paste(rv$entry[[1]], ": Actual")) + ylab(paste(rv$entry[[1]], ": Predicted")) + theme(plot.title = element_text(color="navyblue", size=12, face="bold", hjust = 0.5)) + ggtitle(paste(rv$entry[[1]], "\n","Actual versus Predicted")) + labs(size="Counts", color = "Prediction")  + scale_colour_discrete(labels=c("Wrong", "Correct"))
        }
        leverage <- ggplot(data=regression_results_2, aes(x= hatvalues(regression_results_2), y= rstandard(regression_results_2))) + geom_point(aes(size= cooks.distance(regression_results_2)), na.rm=TRUE) + stat_smooth(method="loess", na.rm=TRUE) + xlab("Leverage") + ylab("Standardised Residuals") + ggtitle("Residuals vs Leverage") + theme(plot.title = element_text(hjust=0.5)) + labs(size="Cook's distance")
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot, plot_grid(actual_vs_predicted_plot, ROC_plot, ncol = 2), leverage,
                                                                      ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)),1,1)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      } else if ((regression_type == "Multinomial logistic regression") | regression_type == "Ordinal logistic regression") {
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot,
                                                                      ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)))) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      } else if (regression_type == "Poisson regression") {
        Bland_Altman_plot <- ggplot(data = bland_altman,aes(x = bland_altman[,1], y = bland_altman[,2]))+ geom_point() + xlab("Means") + ylab("Differences") + xlim(round_near(min(min(actual_versus_predicted[,1]),min(actual_versus_predicted[,2]))*0.5),round_near(max(max(actual_versus_predicted[,1]),max(actual_versus_predicted[,2]))*2)) + ylim(round_near(min(bland_altman[,2])*0.5),round_near(max(bland_altman[,2])*2)) + geom_hline(yintercept=mean_difference, lty = 1, col = "gray") + geom_hline(yintercept=mean_difference - (2 * se_difference), lty = 2, col = "gray") + geom_hline(yintercept=mean_difference + (2 * se_difference), lty = 2, col = "gray") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle("Actual versus predicted: Bland-Altman plot")
        residuals_vs_fitted <- ggplot(data=regression_results_2, aes(x=fitted(regression_results_2), y=resid(regression_results_2))) + geom_point() + stat_smooth(method="loess") + geom_hline(yintercept=0, linetype="dashed") + xlab("Fitted values") + ylab("Residuals") + ggtitle("Residuals versus Fitted") + theme(plot.title = element_text(hjust=0.5))
        leverage <- ggplot(data=regression_results_2, aes(x= hatvalues(regression_results_2), y= rstandard(regression_results_2))) + geom_point(aes(size= cooks.distance(regression_results_2)), na.rm=TRUE) + stat_smooth(method="loess", na.rm=TRUE) + xlab("Leverage") + ylab("Standardised Residuals") + ggtitle("Residuals vs Leverage") + theme(plot.title = element_text(hjust=0.5)) + labs(size="Cook's distance")
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, plot_grid(Bland_Altman_plot, correlation_plot, residuals_vs_fitted, leverage, ncol = 2),
                                                                      ncol=1, rel_heights = c(0.1, 2)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      } else if (regression_type == "Cox regression") {
        if (str_detect(AUROC[[1]][1], "Error")) {
          ROC_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = "There was difficulty in predicting the optimal threshold.\nTherefore, it is not possible to create the ROC plot."), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
          actual_vs_predicted_plot <- ggplot() + geom_text(aes(x = 0, y = 0, label = "There was difficulty in predicting the optimal threshold.\nTherefore, it is not possible to create the actual versus predicted plot."), size = 4, hjust = 0.5, vjust = 0.5) + theme(axis.text = element_blank(),axis.title= element_blank(),axis.ticks= element_blank())
        } else {
          ROC_plot <- ggroc(AUROC) + ggtitle("Prediction accuracy") + geom_abline(aes(slope=1,intercept=1),linetype=2) +  geom_text(aes(x = 0.3, y = 0.2, label = paste0('AUROC:', "\n",formatC(AUROC$ci[2], digits = 2, format = "f"),' (', formatC(AUROC$ci[1], digits = 2, format = "f"), ', ', formatC(AUROC$ci[3], digits = 2, format = "f"), ')')), size = 2, hjust=0, vjust =-1)
          frequencies <- data.frame(table(predicted_values))
          frequencies <- frequencies[frequencies$Freq > 0,]
          colour_list <- as.vector(ifelse(frequencies$Actual.values == frequencies$Predicted.values,"darkred","darkgreen"))
          actual_vs_predicted_plot <- ggplot(frequencies, aes(x = Actual.values, y = Predicted.values, size = Freq, colour = colour_list)) + geom_point() + xlab(paste(rv$entry[[1]], ": Actual")) + ylab(paste(rv$entry[[1]], ": Predicted")) + theme(plot.title = element_text(color="navyblue", size=12, face="bold", hjust = 0.5)) + ggtitle(paste(rv$entry[[1]], "\n","Actual versus Predicted")) + labs(size="Counts", color = "Prediction")  + scale_colour_discrete(labels=c("Wrong", "Correct"))
        }
        residuals_vs_fitted <- ggplot(data=cbind.data.frame(fitted(regression_results_2), as.vector(resid(regression_results_2))), aes(x=fitted(regression_results_2), y= as.vector(resid(regression_results_2)))) + geom_point() + stat_smooth(method="loess") + geom_hline(yintercept=0, linetype="dashed") + xlab("Fitted values") + ylab("Residuals") + ggtitle("Residuals versus Fitted") + theme(plot.title = element_text(hjust=0.5))
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot, plot_grid(actual_vs_predicted_plot, ROC_plot, ncol = 2), residuals_vs_fitted,
                                                                      ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)),1,1)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      }
      if (regression_type == "Linear regression") {
        plots_list_pre <- list()
        for (i in 1:length(rv$entry[[1]])) {
          plot_title <- paste0(rv$entry[[1]][i], '_', regression_type)
          plots_list_pre[[i]] <- sapply(1:length(regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]]), function(x){
            eval(parse(text = paste0(
              "suppressWarnings(suppressMessages(try(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                                                   substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"','.png'),
                                                 plot = plots_each_outcome[[",i,"]]$",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"), silent = TRUE)))"
            )))
          })
        }
        plots_list <- do.call(c,plots_list_pre)
      } else {
        plot_title <- paste0(rv$entry[[1]][length(rv$entry[[1]])], '_', regression_type)
        plots_list <- sapply(1:length(regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]]), function(x){
          eval(parse(text = paste0(
            "suppressWarnings(suppressMessages(try(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                                                   substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"','.png'),
                                                 plot = ",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"), silent = TRUE)))"
          )))
        })
      }
      plots_list_display <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                   substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','composite_plot','.png')
      display_table <- TRUE
      display_plot <- TRUE
      analysis_outcome <- "Successful"
    }
  }
  # Output ####
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
