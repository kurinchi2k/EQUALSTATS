function.Mixed_Effects_Regression <- function(Predefined_lists, rv){
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
    '<b>entry_7: </b>', paste0(rv$entry[[7]], collapse = "; "), '<br>',
    '<b>entry_8: </b>', paste0(rv$entry[[8]], collapse = "; "), '<br>'
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
    'rv$entry[[8]] <- ', ifelse(length(rv$entry[[8]]) > 1,
                                paste0('c("', paste0(rv$entry[[8]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[8]],'"')), '\n',
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
  func.identify_nested_crossed_random_effects <- function(variables, data, regression_type) {
    # Create all pairs
    variable_pairs <- lapply(1:(length(variables)-1), function(x){
      interim <- lapply((x+1):length(variables), function(y){
        c(variables[x], variables[y])
      })
      interim <- do.call(rbind.data.frame, interim)
      colnames(interim) <- c("variable_1", "variable_2")
      return(interim)
    })
    variable_pairs <- do.call(rbind.data.frame, variable_pairs)
    variable_pairs$relative_order <- NA
    random_effects_type <- "Unknown"
    for (i in 1:nrow(variable_pairs)) {
      comparison <- cbind.data.frame(
        variable_1 = as.factor(as.character(unlist(data[,variable_pairs$variable_1[i]]))),
        variable_2 = as.factor(as.character(unlist(data[,variable_pairs$variable_2[i]])))
      )
      levels_variable_2_in_variable_1 <- lapply(1:nlevels(comparison$variable_1), function(x) {
        as.character(unique(comparison$variable_2[comparison$variable_1 == levels(comparison$variable_1)[x]]))
      })
      n_levels_variable_2_in_variable_1 <- sapply(levels_variable_2_in_variable_1, length)
      levels_variable_1_in_variable_2 <- lapply(1:nlevels(comparison$variable_2), function(x) {
        as.character(unique(comparison$variable_1[comparison$variable_2 == levels(comparison$variable_2)[x]]))
      })
      n_levels_variable_1_in_variable_2 <- sapply(levels_variable_1_in_variable_2, length)
      if ((max(n_levels_variable_2_in_variable_1) > 1) & (max(n_levels_variable_1_in_variable_2) > 1)) {
        random_effects_type <- "Crossed"
        break
      } else if (max(n_levels_variable_2_in_variable_1) > 1) {
        overlap_between_levels <- lapply(1:(length(levels_variable_2_in_variable_1)-1), function(x) {
          overlaps <-  lapply((x+1):length(levels_variable_2_in_variable_1), function(y){
            intersect(levels_variable_2_in_variable_1[[x]], levels_variable_2_in_variable_1[[y]])
          })
          overlaps <- do.call(c,overlaps)
        })
        overlap_between_levels <- do.call(c, overlap_between_levels)
        if (length(overlap_between_levels) > 0) {
          random_effects_type <- "Crossed"
          break
        } else {
          variable_pairs$relative_order[i] <- variable_pairs$variable_1[i]
        }
      } else if (max(n_levels_variable_1_in_variable_2) > 1) {
        overlap_between_levels <- lapply(1:(length(levels_variable_1_in_variable_2)-1), function(x) {
          overlaps <-  lapply((x+1):length(levels_variable_1_in_variable_2), function(y){
            intersect(levels_variable_1_in_variable_2[[x]], levels_variable_1_in_variable_2[[y]])
          })
          overlaps <- do.call(c,overlaps)
        })
        overlap_between_levels <- do.call(c, overlap_between_levels)
        if (length(overlap_between_levels) > 0) {
          random_effects_type <- "Crossed"
          break
        } else {
          variable_pairs$relative_order[i] <- variable_pairs$variable_2[i]
        }
      } else {
        variable_pairs$relative_order[i] <- "identical"
      }
    }
    if (random_effects_type == "Unknown") {
      random_effects_type <- "Nested"
      identical_pairs <- variable_pairs[variable_pairs$relative_order == "identical", ]
      if (nrow(identical_pairs) > 0) {
        for (i in 1:nrow(identical_pairs)) {
          variable_to_remove <- identical_pairs$variable_2[i]
          variable_pairs <- variable_pairs[((variable_pairs$variable_1 != variable_to_remove) & (variable_pairs$variable_2 != variable_to_remove)),]
        }
      }
      ordered_variables <- lapply(1:length(variables), function(x) {
        cbind.data.frame(
          variable = variables[x],
          frequency = length(variable_pairs$relative_order[variable_pairs$relative_order == variables[x]])
        )
      })
      ordered_variables <- do.call(rbind.data.frame, ordered_variables)
      ordered_variables <- ordered_variables[order(ordered_variables$frequency, decreasing = TRUE),1]
      if (regression_type == "Multinomial logistic regression") {
        new_names <- make.names(ordered_variables)
        random_effects_text <- list(old_names = ordered_variables, new_names = new_names, RE_text = paste0("~ 1 | ", paste0("`", new_names, "`", collapse = "/")))
      } else if (regression_type == "Ordinal logistic regression"){
        # At least one of the nested random-effects variable should have three levels; otherwise they become fixed-effect variables
        nlevels_variables <- sapply(ordered_variables, function(y) {nlevels(data[,y])})
        if (max(nlevels_variables) > 2) {
          random_effects_text <- list(RE_text = paste0("(1 | ", paste0("`", ordered_variables, "`", collapse = "/"), ")"), FE_variables = NA)
        } else {
          random_effects_text <- list(RE_text = NA, FE_variables = ordered_variables)
        }
      } else {
        random_effects_text <- paste0("(1 | ", paste0("`", ordered_variables, "`", collapse = "/"), ")")
      }
    } else {
      if (regression_type == "Multinomial logistic regression") {
        new_names <- make.names(variables)
        random_effects_text <- list(old_names = variables, new_names = new_names, RE_text = paste0("list(", paste0("~ 1 | `", new_names, "`", collapse = ", "), ")"))
      } else if (regression_type == "Ordinal logistic regression") {
        # Keep only the random_effects variables with at least 3 levels in the random effects for ordinal logistic regression
        nlevels_variables <- sapply(variables, function(y) {nlevels(data[,y])})
        random_effects_text <- list(RE_text = paste0("(1 | `", variables[(nlevels_variables) > 2], "`)", collapse = " + "), FE_variables = variables[(nlevels_variables) <= 2])
      } else {
        random_effects_text <- paste0("(1 | `", variables, "`)", collapse = " + ")
      }
    }
    return(random_effects_text)
  }
  func.convergence <- function(regression_results) {
    if (regression_type == "Cox regression") {
      0
    } else if (regression_type == "Multinomial logistic regression") {
      if (regression_results$converged == TRUE) {0} else {1}
    } else if (regression_type == "Ordinal logistic regression") {
      if (! is.na(random_effects_text_pre$RE_text)) {regression_results$optRes$convergence} else {regression_results$convergence$code}
    } else {
      regression_results@optinfo$conv$opt
    }
  }
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
  data <- rv$import_data$data[,c(rv$entry[[1]], if(rv$entry[[2]] != ""){rv$entry[[2]]}, rv$entry[[3]], if(TRUE %in% (rv$entry[[4]] != "")){rv$entry[[4]]}, if(TRUE %in% (rv$entry[[5]] != "")){rv$entry[[5]]})]
  # First complete case analysis
  data <- na.omit(data)
  # Note the variable types
  outcome <- rv$entry[[1]]
  mandatory_predictors <- rv$entry[[4]][rv$entry[[4]] != ""]
  optional_predictors <- rv$entry[[5]][rv$entry[[5]] != ""]
  variables_present <- c(outcome, rv$entry[[3]], mandatory_predictors, optional_predictors)
  categorical_present <- c(outcome, rv$entry[[3]], mandatory_predictors, optional_predictors)[c(outcome, rv$entry[[3]], mandatory_predictors, optional_predictors) %in% rv$import_data$categorical]
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
  colnames(data)[colnames(data) %in% setdiff(categorical_present, c(outcome, rv$entry[[3]]))] <- paste0(colnames(data)[colnames(data) %in% setdiff(categorical_present, c(outcome, rv$entry[[3]]))], "_")
  # Remove optional predictors with only one level
  if (length(optional_predictors) > 0) {
    more_than_one_level <- sapply(1:length(optional_predictors), function(x) {
      if (optional_predictors[x] %in% rv$import_data$quantitative) {
        TRUE
      } else {
        (nlevels(data[,optional_predictors[x]]) > 1)
      }
    })
    optional_predictors <- optional_predictors[more_than_one_level]
  }
  # Apply boxcox transformation and grand mean centering as appropriate
  if (rv$second_menu_choice != "EQUAL-STATS choice") {
    boxcox_transformation <- c(rv$entry[[6]])
    boxcox_transformation <- boxcox_transformation[boxcox_transformation != ""]
    if (length(boxcox_transformation) > 0) {data[,boxcox_transformation] <- lapply(boxcox_transformation, func.boxcox_transformation)}
    gmc <- c(rv$entry[[7]])
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
    if (rv$entry[[8]] == "Yes") {operator <- '*'} else {operator <- '+'}
    regression_type <- rv$second_menu_choice
  } else {
    operator <- '*'
    regression_type <-
      if (rv$entry[[1]] %in% rv$import_data$binary) {
        "Logistic regression"
      } else if (rv$entry[[1]] %in% rv$import_data$ordinal) {
        "Ordinal logistic regression"
      } else if (rv$entry[[1]] %in% rv$import_data$nominal) {
        "Multinomial logistic regression"
      } else {
        if (is.null(rv$import_data$counts)) {
          "Linear regression"
        } else if (rv$entry[[1]] %in% rv$import_data$counts){
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
    regression_prefix = c("lme4::lmer(", "lme4::glmer(", "mclogit::mblogit(", "ordinal::clmm(", "lme4::glmer(", paste0("coxme::coxme(Surv(`", rv$entry[[2]], "`, as.numeric(")),
    regression_prefix_fixed = c("lm(", "glm(", "multinom(", "ordinal::clm(", "glm(", paste0("coxph(Surv(`", rv$entry[[2]], "`, as.numeric(")),
    outcome_end = c(" ~ ", " ~ ", " ~ ", " ~ ", " ~ ", ")) ~ "),
    data_prefix = c("", ", family = binomial", ", trace = FALSE",", Hess = TRUE", ", family = poisson",""),
    convergence = c("@optinfo$conv$opt", "@optinfo$conv$opt", "$converged","$optRes$convergence", "@optinfo$conv$opt",NA),
    plots_list = list(list("correlation_plot", "Bland_Altman_plot", "actual_vs_predicted_plot" ,"residuals_vs_fitted", "normality_residuals", "standardised_residuals", "leverage", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "ROC_plot", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "composite_plot"),list("correlation_plot", "Bland_Altman_plot", "residuals_vs_fitted", "composite_plot"),list("correlation_plot", "actual_vs_predicted_plot", "ROC_plot", "composite_plot"))
  )
  # Random effects and regression_text_1: Ordinal logistic regression takes random effects above 2 levels only; others take this at any level
  if (regression_type == "Multinomial logistic regression") {
    if (length(rv$entry[[3]]) > 1) {
      random_effects_text_pre <- func.identify_nested_crossed_random_effects(variables = rv$entry[[3]], data = data, regression_type = regression_type)
    } else {
      random_effects_text_pre <- list(old_names = rv$entry[[3]], new_names = make.names(rv$entry[[3]]), RE_text = paste0("~1 | `", make.names(rv$entry[[3]]), "`"))
    }
    colnames(data)[match(random_effects_text_pre$old_names, colnames(data))] <- random_effects_text_pre$new_names
    regression_text_1 <- paste0(
      regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
      "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
      ifelse(length(mandatory_predictors) == 0, 1, paste0(paste0("`", mandatory_predictors, "`", collapse = ' + '))),
      ", random = ", random_effects_text_pre$RE_text,
      regression_reference$data_prefix[regression_reference$regression_type == regression_type],
      ", data = data, control = mmclogit.control(trace = FALSE))"
    )[1]
  } else if (regression_type == "Ordinal logistic regression") {
    if (length(rv$entry[[3]]) > 1) {
      random_effects_text_pre <- func.identify_nested_crossed_random_effects(variables = rv$entry[[3]], data = data, regression_type = regression_type)
    } else {
      if (nlevels(data[,rv$entry[[3]]]) > 2) {
        random_effects_text_pre <- list(RE_text = paste0("(1 | `", rv$entry[[3]], "`)"), FE_variables = NA)
      } else {
        random_effects_text_pre <- list(RE_text = NA, FE_variables = rv$entry[[3]])
      }
    }
    if (! TRUE %in% (is.na(random_effects_text_pre$FE_variables))) {
      colnames(data)[match(random_effects_text_pre$FE_variables, colnames(data))] <- paste0(random_effects_text_pre$FE_variables, "_")
      mandatory_predictors <- c(mandatory_predictors, paste0(random_effects_text_pre$FE_variables, "_"))
    }
    if (! is.na(random_effects_text_pre$RE_text)) {
      random_effects_text <- random_effects_text_pre$RE_text
      regression_text_1 <- paste0(
        regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
        "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
        ifelse(length(mandatory_predictors) == 0, random_effects_text, paste0(paste0("`", mandatory_predictors, "`", collapse = ' + '), ' + ', random_effects_text)),
        regression_reference$data_prefix[regression_reference$regression_type == regression_type],
        ", data = data)"
      )[1]
    } else {
      regression_text_1 <- paste0(
        regression_reference$regression_prefix_fixed[regression_reference$regression_type == regression_type],
        "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
        ifelse(length(mandatory_predictors) == 0, 1, paste0(paste0("`", mandatory_predictors, "`", collapse = ' + '))),
        regression_reference$data_prefix[regression_reference$regression_type == regression_type],
        ", data = data)"
      )[1]
    }
  } else {
    if (length(rv$entry[[3]]) > 1) {
      random_effects_text <- func.identify_nested_crossed_random_effects(variables = rv$entry[[3]], data = data, regression_type = regression_type)
    } else {
      random_effects_text <- paste0("(1 | `", rv$entry[[3]], "`)")
    }
    regression_text_1 <- paste0(
      regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
      "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
      ifelse(length(mandatory_predictors) == 0, random_effects_text, paste0(paste0("`", mandatory_predictors, "`", collapse = ' + '), ' + ', random_effects_text)),
      regression_reference$data_prefix[regression_reference$regression_type == regression_type],
      ", data = data)"
    )[1]
  }
  regression_results_1 <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_text_1)), silent = TRUE)))
  # Regression text results 1 onwards ####
  if ((regression_type == "Linear regression") | (regression_type == "Logistic regression") | (regression_type == "Poisson regression")) {Error <- (suppressWarnings(isS4(regression_results_1)) == FALSE)} else {Error <- (TRUE %in% suppressWarnings(str_detect(regression_results_1[[1]][1],"Error")))}
  if (Error == TRUE) {
    results <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      `Reason` = "The analysis did not run correctly with the variables entered. Please try with fixed-effect, fewer variables, and interactions.",
      check.names = FALSE
    )
    results_display <- results
    plots_list <- ""
    plots_list_display <- ""
    display_table <- TRUE
    display_plot <- FALSE
    analysis_outcome <- "Unsuccessful"
  } else {
    convergence <- func.convergence(regression_results = regression_results_1)
    if (convergence > 0) {
      results <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Analysis outcome` = "Unsuccessful",
        `Reason` = "There was no convergence even with mandatory variables. Please try with fewer mandatory variables.",
        check.names = FALSE
      )
      results_display <- results
      plots_list <- ""
      plots_list_display <- ""
      display_table <- TRUE
      display_plot <- FALSE
      analysis_outcome <- "Unsuccessful"
    } else {
      if (length(optional_predictors) == 0) {
        regression_results <- regression_results_1
      } else {
        if (regression_type == "Multinomial logistic regression") {
          regression_interim_text <- paste0(
            regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
            "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
            ifelse(length(c(mandatory_predictors, optional_predictors)) == 0, 1, paste0(paste0("`", c(mandatory_predictors,optional_predictors), "`", collapse = ' + '))),
            ", random = ", random_effects_text_pre$RE_text,
            regression_reference$data_prefix[regression_reference$regression_type == regression_type],
            ", data = data, control = mmclogit.control(trace = FALSE), na.action = 'na.fail')"
          )[1]
        } else if (regression_type == "Ordinal logistic regression") {
          if (! is.na(random_effects_text_pre$RE_text)) {
            regression_interim_text <- paste0(
              regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
              "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
              ifelse(length(c(mandatory_predictors, optional_predictors)) == 0, random_effects_text, paste0(paste0("`", c(mandatory_predictors,optional_predictors), "`", collapse = ' + '), ' + ', random_effects_text)),
              regression_reference$data_prefix[regression_reference$regression_type == regression_type],
              ", data = data, na.action = 'na.fail')"
            )[1]
          } else {
            regression_interim_text <- paste0(
              regression_reference$regression_prefix_fixed[regression_reference$regression_type == regression_type],
              "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
              ifelse(length(c(mandatory_predictors, optional_predictors)) == 0, 1, paste0(paste0("`", c(mandatory_predictors,optional_predictors), "`", collapse = ' + '))),
              regression_reference$data_prefix[regression_reference$regression_type == regression_type],
              ", data = data, na.action = 'na.fail')"
            )[1]
          }
        } else {
          regression_interim_text <- paste0(
            regression_reference$regression_prefix[regression_reference$regression_type == regression_type],
            "`", outcome, "`", regression_reference$outcome_end[regression_reference$regression_type == regression_type],
            ifelse(length(c(mandatory_predictors, optional_predictors)) == 0, random_effects_text, paste0(paste0("`", c(mandatory_predictors,optional_predictors), "`", collapse = ' + '), ' + ', random_effects_text)),
            regression_reference$data_prefix[regression_reference$regression_type == regression_type],
            ", data = data, na.action = 'na.fail')"
          )[1]
        }
        regression_interim_results <- suppressMessages(suppressWarnings(try(eval(parse(text = regression_interim_text)), silent = TRUE)))
        if ((regression_type == "Linear regression") | (regression_type == "Logistic regression") | (regression_type == "Poisson regression")) {Error <- (suppressWarnings(isS4(regression_interim_results)) == FALSE)} else {Error <- (TRUE %in% suppressWarnings(str_detect(regression_interim_results[[1]][1],"Error")))}
        if (Error == TRUE) {regression_results <- regression_results_1} else {
          regression_interim_results_2 <- suppressMessages(suppressWarnings(try(eval(parse(text = paste0(
            'MuMIn::dredge(global.model = regression_interim_results, beta = "none", fixed = c(', paste0('"`', mandatory_predictors, '`"', collapse = ", "), "), trace = 2)"
          ))), silent = TRUE)))
          if (TRUE %in% suppressWarnings(str_detect(regression_interim_results_2[[1]][1],"Error"))) {
            regression_results <- regression_results_1
            regression_results_2 <- regression_results_1
          } else {
            # Despite specifying that mandatory predictors should be kept, in some models, the top model shown does not contain mandatory variables
            check_mandatory_predictors <- regression_interim_results_2
            colnames(check_mandatory_predictors) <- str_remove_all(colnames(check_mandatory_predictors), "`")
            check_mandatory_predictors$checks <- if (length(mandatory_predictors) > 1) {
              rowSums(is.na(check_mandatory_predictors[,mandatory_predictors]))
            } else {
              ifelse(is.na(check_mandatory_predictors[,mandatory_predictors]) == FALSE, 0,1)
            }
            regression_interim_results_2 <- regression_interim_results_2[check_mandatory_predictors$checks == 0, ]
            regression_results_2 <- suppressMessages(suppressWarnings(MuMIn::get.models(regression_interim_results_2, subset = 1)[[1]]))
          }
          if ((regression_type == "Linear regression") | (regression_type == "Logistic regression") | (regression_type == "Poisson regression")) {
            Error <- (suppressWarnings(isS4(regression_results_2)) == FALSE)
          } else {
            Error <- (TRUE %in% suppressWarnings(str_detect(regression_results_2[[1]][1],"Error")))
          }
          if (Error == TRUE) {
            regression_results <- regression_results_1
          } else if (regression_type == "Cox regression") {
            regression_results <- regression_results_2
          } else if ((regression_type == "Multinomial logistic regression") | (regression_type == "Ordinal logistic regression")) {
            convergence <- func.convergence(regression_results_2)
            if (convergence == 0) {
              regression_results <- regression_results_2
            } else {
              regression_results <- regression_results_1
            }
          } else {
            regression_all_fit_results <- suppressMessages(suppressWarnings(try(allFit(regression_results_2, verbose = FALSE),silent = TRUE)))
            summary_all_fit_results <- suppressMessages(suppressWarnings(try(summary(regression_all_fit_results),silent = TRUE)))
            if (suppressWarnings(isS4(regression_results_1)) == FALSE) {
              convergence <- func.convergence(regression_results_2)
              if (convergence > 0) {
                regression_results <- regression_results_1
              } else {
                regression_results <- regression_results_2
              }
            } else {
              regression_results <- suppressWarnings(regression_all_fit_results[which.min(summary_all_fit_results$llik)][[1]])
              convergence <- func.convergence(regression_results)
              if (convergence > 1) {
                convergence <- func.convergence(regression_results_2)
                if (convergence > 1) {
                  regression_results <- regression_results_1
                } else {
                  regression_results <- regression_results_2
                }
              }
            }
          }
        }
      }
      # Get the coefficients ####
      summary_regression_results <- suppressWarnings(summary(regression_results))
      regression_coefficients <- data.frame(summary_regression_results$coefficients, check.rows = FALSE, check.names = FALSE)
      regression_coefficients <- cbind.data.frame(
        Variable = str_remove_all(str_replace_all(row.names(regression_coefficients), fixed("(Intercept)"), "Intercept"),"`"),
        regression_coefficients
      )
      # Remove entire NAN rows that occurs with ordinal logistic regression
      if (regression_type == "Ordinal logistic regression") {
        NaNs <- sapply(1:ncol(regression_coefficients), function(y) {
          sum(is.nan(regression_coefficients[,y]))
        })
        regression_coefficients <- regression_coefficients[,colnames(regression_coefficients)[(NaNs < nrow(regression_coefficients))]]
      }
      # Further processing depends upon the different regression types
      if (regression_type == "Linear regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Estimate lower conf. int` = regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`,
          `Estimate upper conf. int` = regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`,
          `t value` = regression_coefficients[,4]
        )
        # Test statistics
        ANOVA_fixed_effects_regression_results <- suppressMessages(anova(regression_results))
        test_statistics <- cbind.data.frame(
          Variable = row.names(ANOVA_fixed_effects_regression_results),
          `Statistic` = rep("F value", nrow(ANOVA_fixed_effects_regression_results)),
          `Statistic value` = ANOVA_fixed_effects_regression_results[,"F value"]
        )
        actual_versus_predicted <- cbind.data.frame(Actual = data[,1], Predicted = fitted(regression_results))
        concordance_correlation_coefficient <- CCC(actual_versus_predicted[,1], actual_versus_predicted[,2], ci = "z-transform", conf.level = 0.95, na.rm= TRUE)
        correlation_coefficient <- concordance_correlation_coefficient$rho.c
        colnames(correlation_coefficient) <- c("Concordance correlation coefficient - Point estimate","Concordance correlation coefficient - LCI", "Concordance correlation coefficient - UCI")
        bland_altman <- concordance_correlation_coefficient$blalt
        mean_difference <- mean(bland_altman$delta)
        se_difference <- (var(bland_altman$delta))^0.5
        regression_fit_statistics <- cbind.data.frame(
          `AIC` = AIC(regression_results),
          `BIC` = BIC(regression_results),
          correlation_coefficient
        )
      } else if (regression_type == "Logistic regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Odds ratio` = exp(regression_coefficients$Estimate),
          `Odds ratio lower conf. int` = exp(regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`),
          `Odds ratio upper conf. int` = exp(regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`),
          `z value` = regression_coefficients[,4],
          `P value` = regression_coefficients[,5]
        )
        regression_fit_statistics <- cbind.data.frame(
          `AIC` = AIC(regression_results),
          `BIC` = BIC(regression_results),
          `-2 Log likelihood` = unlist(logLik(regression_results))
        )
        # Predictions
        predicted_probability <- predict(regression_results, type="response")
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
      } else if (regression_type == "Multinomial logistic regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Odds ratio` = exp(regression_coefficients$Estimate),
          `Odds ratio lower conf. int` = exp(regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`),
          `Odds ratio upper conf. int` = exp(regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`),
          `z value` = regression_coefficients[,4],
          `P value` = regression_coefficients[,5]
        )
        regression_fit_statistics <- cbind.data.frame(
          `AIC` = AIC(regression_results),
          `BIC` = BIC(regression_results),
          `-2 Log likelihood` = unlist(logLik(regression_results))
        )
        # Predictions
        predicted_probabilities <- data.frame(predict(regression_results, newdata = data, type = "response"))
        predicted_probabilities$maximum_probability <- sapply(1:nrow(predicted_probabilities), function(x) {
          colnames(predicted_probabilities)[which.max(predicted_probabilities[x,])]
        })
        predicted_probabilities$maximum_probability <- factor(predicted_probabilities$maximum_probability, levels = levels(data[,1]))
        predicted_values <- data.frame(
          `Actual values` = data[,1],
          `Predicted values` = predicted_probabilities$maximum_probability,
          check.names = FALSE
        )
        actual_vs_predicted <- data.frame(unclass(table(predicted_values)), check.names = FALSE)
        actual_vs_predicted <- cbind.data.frame(
          colnames(actual_vs_predicted),
          actual_vs_predicted
        )
        colnames(actual_vs_predicted) <- c(paste0("Actual ", rv$entry[[1]]), paste0("Predicted as ", colnames(actual_vs_predicted)[2:ncol(actual_vs_predicted)]))
      } else if (regression_type == "Ordinal logistic regression") {
        if (is.na(random_effects_text_pre$RE_text)) {
          regression_coefficients <- cbind.data.frame(
            regression_coefficients[,1:3],
            `Odds ratio` = exp(regression_coefficients$Estimate),
            `Odds ratio lower conf. int` = exp(regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`),
            `Odds ratio upper conf. int` = exp(regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`),
            `z value` = regression_coefficients[,4],
            `P value` = regression_coefficients[,5]
          )
        } else {
          regression_coefficients <- cbind.data.frame(
            regression_coefficients[,1:2],
            `Odds ratio` = exp(regression_coefficients$Estimate)
          )
        }
        regression_fit_statistics <- cbind.data.frame(
          `AIC` = AIC(regression_results),
          `BIC` = BIC(regression_results),
          `-2 Log likelihood` = unlist(logLik(regression_results))
        )
        # Predictions not available for CLMM yet
        if (is.na(random_effects_text_pre$RE_text)) {
          predicted_values <- data.frame(
            `Actual values` = data[,1],
            `Predicted values` = predict(regression_results, type = "class"),
            check.names = FALSE
          )
          colnames(predicted_values) <- c("Actual values", "Predicted values")
          actual_vs_predicted <- data.frame(unclass(table(predicted_values)), check.names = FALSE)
          actual_vs_predicted <- cbind.data.frame(
            colnames(actual_vs_predicted),
            actual_vs_predicted
          )
          colnames(actual_vs_predicted) <- c(paste0("Actual ", rv$entry[[1]]), paste0("Predicted as ", colnames(actual_vs_predicted)[2:ncol(actual_vs_predicted)]))
        }
      } else if (regression_type == "Poisson regression") {
        regression_coefficients <- cbind.data.frame(
          regression_coefficients[,1:3],
          `Rate ratio` = exp(regression_coefficients$Estimate),
          `Rate ratio lower conf. int` = exp(regression_coefficients$Estimate - qnorm(0.975) * regression_coefficients$`Std. Error`),
          `Rate ratio upper conf. int` = exp(regression_coefficients$Estimate + qnorm(0.975) * regression_coefficients$`Std. Error`),
          `z value` = regression_coefficients[,4],
          `P value` = regression_coefficients[,5]
        )
        regression_fit_statistics <- cbind.data.frame(
          `AIC` = AIC(regression_results),
          `BIC` = BIC(regression_results),
          `-2 Log likelihood` = unlist(logLik(regression_results))
        )
        actual_versus_predicted <- cbind.data.frame(Actual = data[,1], Predicted = fitted(regression_results))
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
        regression_fit_statistics <- cbind.data.frame(
          `AIC` = AIC(regression_results),
          `BIC` = BIC(regression_results),
          `-2 Log likelihood` = unlist(logLik(regression_results))
        )
        # Predictions
        Predictions <- cbind.data.frame(
          order = as.numeric(row.names(data)),
          outcome = data[,rv$entry[[1]]],
          time = data[,rv$entry[[2]]],
          linear_predictor = predict(regression_results, type="lp")
        )
        baseline_hazard <- eval(parse(text = paste0(
          'basehaz(coxph(Surv(`',rv$entry[[2]],'`, as.numeric(`',rv$entry[[1]],'`)) ~ 1, data = data, , method = "breslow"))'
        )))
        Predictions <- merge(Predictions, baseline_hazard)
        Predictions <- Predictions[order(Predictions$order),]
        Predictions$survival_probability <- exp(-(exp(Predictions$linear_predictor) * Predictions$hazard))
        AUROC <- suppressWarnings(suppressMessages(try(roc(Predictions$outcome, Predictions$survival_probability, ci = TRUE, ci.alpha = 0.95, print.auc=TRUE), silent = TRUE)))
        if (str_detect(AUROC[[1]][1], "Error")) {
          actual_vs_predicted <- data.frame(Predictions = "Not able to predict because of difficulty in finding optimal threshold", check.names = FALSE)
        } else {
          optimal_threshold <- coords(AUROC, "best", ret="threshold")
          predicted_values <- data.frame(
            `Actual values` = data[,1],
            `Predicted values` = as.factor(sapply(1:length(Predictions$survival_probability), function(x) {
              eval(parse(text = paste0(
                'ifelse(Predictions$survival_probability[x]',AUROC$direction,'optimal_threshold, levels(data[,rv$entry[[1]]])[1], levels(data[,rv$entry[[1]]])[2])'
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
      # Summary ####
      # Create a version for display with numbers rounded to 4 digits
      descriptive_summary_display <- descriptive_summary
      regression_coefficients_display <- regression_coefficients
      regression_fit_statistics_display <- regression_fit_statistics
      descriptive_summary_display[,sapply(descriptive_summary_display, is.numeric)] <- sapply(descriptive_summary_display[,sapply(descriptive_summary_display, is.numeric)], function(x){round(x,4)})
      regression_coefficients_display[,sapply(regression_coefficients_display, is.numeric)] <- sapply(regression_coefficients_display[,sapply(regression_coefficients_display, is.numeric)], function(x){round(x,4)})
      regression_fit_statistics_display[,sapply(regression_fit_statistics_display, is.numeric)] <- sapply(regression_fit_statistics_display[,sapply(regression_fit_statistics_display, is.numeric)], function(x){round(x,4)})
      # General description
      general_description <- data.frame(
        `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
        `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
        `Regression type` = regression_type,
        `Analysis outcome` = "Successful",
        `Total number of observations` = nrow(rv$import_data$data),
        `Number of complete observations` = nrow(data),
        Outcome = rv$entry[[1]],
        check.names = FALSE
      )
      if ((regression_type == "Linear regression") ) {
        test_statistics_display <- test_statistics
        test_statistics_display[,sapply(test_statistics_display, is.numeric)] <- sapply(test_statistics_display[,sapply(test_statistics_display, is.numeric)], function(x){round(x,4)})
        results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, test_statistics, regression_fit_statistics))
        results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, test_statistics_display, regression_fit_statistics_display))
      } else if (regression_type == "Ordinal logistic regression") {
        if (! is.na(random_effects_text_pre$RE_text)) {
          results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, regression_fit_statistics))
          results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, regression_fit_statistics_display))
        } else {
          results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, regression_fit_statistics, actual_vs_predicted))
          results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, regression_fit_statistics_display, actual_vs_predicted))
        }
      } else if (regression_type == "Poisson regression") {
        results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, regression_fit_statistics))
        results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, regression_fit_statistics_display))
      } else {
        results <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary, regression_coefficients, regression_fit_statistics, actual_vs_predicted))
        results_display <- function.rbind_different_column_numbers(list = list(general_description, descriptive_summary_display, regression_coefficients_display, regression_fit_statistics_display, actual_vs_predicted))
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
      plot_title_combined <- ggdraw() + draw_label(paste0(rv$entry[[1]], '(Mixed ', regression_type, ')'), color="darkgreen", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
      # Diagnostics: This differs for different regressions
      if (regression_type == "Linear regression") {
        residuals_plots_file <- cbind.data.frame(
          fitted_values = fitted(regression_results),
          raw_residuals = residuals(regression_results),
          standardised_residuals = residuals(regression_results, type = "pearson", scaled = TRUE),
          hat_values = hatvalues(regression_results),
          cooks_distance = cooks.distance(regression_results)
        )
        Bland_Altman_plot <- ggplot(data = bland_altman,aes(x = bland_altman[,1], y = bland_altman[,2]))+ geom_point() + xlab("Means") + ylab("Differences") + xlim(round_near(min(min(actual_versus_predicted[,1]),min(actual_versus_predicted[,2]))*0.5),round_near(max(max(actual_versus_predicted[,1]),max(actual_versus_predicted[,2]))*2)) + ylim(round_near(min(bland_altman[,2])*0.5),round_near(max(bland_altman[,2])*2)) + geom_hline(yintercept=mean_difference, lty = 1, col = "gray") + geom_hline(yintercept=mean_difference - (2 * se_difference), lty = 2, col = "gray") + geom_hline(yintercept=mean_difference + (2 * se_difference), lty = 2, col = "gray") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle("Actual versus predicted: Bland-Altman plot")
        actual_vs_predicted_plot <- suppressMessages(suppressWarnings(try(ggplot(actual_versus_predicted, aes(x= Actual, y = Predicted)) + geom_point() + ggtitle("Predicted versus actual values") + theme_classic() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)), silent = TRUE)))
        residuals_vs_fitted <- ggplot(data=residuals_plots_file, aes(x=fitted_values, y=raw_residuals)) + geom_point() + stat_smooth(method="loess") + geom_hline(yintercept=0, linetype="dashed") + xlab("Fitted values") + ylab("Residuals") + ggtitle("Residuals versus Fitted") + theme(plot.title = element_text(hjust=0.5))
        normality_residuals <- ggplot(data=residuals_plots_file, aes(sample = standardised_residuals)) + stat_qq() +  stat_qq_line(linetype=2, color="grey")+ ggtitle("Q-Q plot") + theme(plot.title = element_text(hjust=0.5)) + xlab("Theoretical quantiles")+ ylab("Sample quantiles")
        standardised_residuals <- ggplot(data=residuals_plots_file, aes(x= fitted_values, y=sqrt(abs(standardised_residuals)))) + geom_point(na.rm=TRUE) + stat_smooth(method="loess", na.rm = TRUE) + xlab("Fitted Values") + ylab(expression(sqrt("Standardised residuals"))) + ggtitle("Scale-Location") + theme(plot.title = element_text(hjust=0.5))
        leverage <- ggplot(data=residuals_plots_file, aes(x= hat_values, y= standardised_residuals)) + geom_point(aes(size= cooks_distance), na.rm=TRUE) + stat_smooth(method="loess", na.rm=TRUE) + xlab("Leverage") + ylab("Standardised Residuals") + ggtitle("Residuals vs Leverage") + theme(plot.title = element_text(hjust=0.5)) + labs(size="Cook's distance")
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, Bland_Altman_plot, plot_grid(actual_vs_predicted_plot, correlation_plot, residuals_vs_fitted, normality_residuals, standardised_residuals, leverage, ncol = 2),
                                                                      ncol=1, rel_heights = c(0.1, 1, 3)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
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
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot, plot_grid(actual_vs_predicted_plot, ROC_plot, ncol = 2),
                                                                      ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)),1)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      } else if (regression_type == "Ordinal logistic regression") {
        if (is.na(random_effects_text_pre$RE_text)) {
          frequencies <- data.frame(table(predicted_values))
          frequencies <- frequencies[frequencies$Freq > 0,]
          colour_list <- as.vector(ifelse(frequencies$Actual.values == frequencies$Predicted.values,"darkred","darkgreen"))
          actual_vs_predicted_plot <- ggplot(frequencies, aes(x = Actual.values, y = Predicted.values, size = Freq, colour = colour_list)) + geom_point() + xlab(paste(rv$entry[[1]], ": Actual")) + ylab(paste(rv$entry[[1]], ": Predicted")) + theme(plot.title = element_text(color="navyblue", size=12, face="bold", hjust = 0.5)) + ggtitle(paste(rv$entry[[1]], "\n","Actual versus Predicted")) + labs(size="Counts", color = "Prediction")  + scale_colour_discrete(labels=c("Wrong", "Correct"))
          composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot, actual_vs_predicted_plot,
                                                                        ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)),1)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
        }
      } else if (regression_type == "Multinomial logistic regression") {
        frequencies <- data.frame(table(predicted_values))
        frequencies <- frequencies[frequencies$Freq > 0,]
        colour_list <- as.vector(ifelse(frequencies$Actual.values == frequencies$Predicted.values,"darkred","darkgreen"))
        actual_vs_predicted_plot <- ggplot(frequencies, aes(x = Actual.values, y = Predicted.values, size = Freq, colour = colour_list)) + geom_point() + xlab(paste(rv$entry[[1]], ": Actual")) + ylab(paste(rv$entry[[1]], ": Predicted")) + theme(plot.title = element_text(color="navyblue", size=12, face="bold", hjust = 0.5)) + ggtitle(paste(rv$entry[[1]], "\n","Actual versus Predicted")) + labs(size="Counts", color = "Prediction")  + scale_colour_discrete(labels=c("Wrong", "Correct"))
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot, actual_vs_predicted_plot,
                                                                      ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)),1)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      } else if (regression_type == "Poisson regression") {
        residuals_plots_file <- cbind.data.frame(
          fitted_values = fitted(regression_results),
          raw_residuals = residuals(regression_results)
        )
        Bland_Altman_plot <- ggplot(data = bland_altman,aes(x = bland_altman[,1], y = bland_altman[,2]))+ geom_point() + xlab("Means") + ylab("Differences") + xlim(round_near(min(min(actual_versus_predicted[,1]),min(actual_versus_predicted[,2]))*0.5),round_near(max(max(actual_versus_predicted[,1]),max(actual_versus_predicted[,2]))*2)) + ylim(round_near(min(bland_altman[,2])*0.5),round_near(max(bland_altman[,2])*2)) + geom_hline(yintercept=mean_difference, lty = 1, col = "gray") + geom_hline(yintercept=mean_difference - (2 * se_difference), lty = 2, col = "gray") + geom_hline(yintercept=mean_difference + (2 * se_difference), lty = 2, col = "gray") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle("Actual versus predicted: Bland-Altman plot")
        residuals_vs_fitted <- ggplot(data=residuals_plots_file, aes(x=fitted_values, y=raw_residuals)) + geom_point() + stat_smooth(method="loess") + geom_hline(yintercept=0, linetype="dashed") + xlab("Fitted values") + ylab("Residuals") + ggtitle("Residuals versus Fitted") + theme(plot.title = element_text(hjust=0.5))
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, plot_grid(Bland_Altman_plot, correlation_plot, residuals_vs_fitted, ncol = 2),
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
        composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, correlation_plot, plot_grid(actual_vs_predicted_plot, ROC_plot, ncol = 2),
                                                                      ncol=1, rel_heights = c(0.1, (ifelse(length(quantitative_present)>1, 1,0.25)),1)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
      }
      if (regression_type != "Ordinal logistic regression") {
        plot_title <- paste0(rv$entry[[1]], '(Mixed Effects ', regression_type, ')')
        plots_list <- sapply(1:length(regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]]), function(x){
          eval(parse(text = paste0(
            "suppressWarnings(suppressMessages(try(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                                                   substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"','.png'),
                                                 plot = ",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"), silent = TRUE)))"
          )))
        })
        plots_list_display <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                     substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','composite_plot','.png')
        display_plot <- TRUE
      } else {
        if (is.na(random_effects_text_pre$RE_text)) {
          plot_title <- paste0(rv$entry[[1]], '(Mixed Effects ', regression_type, ')')
          plots_list <- sapply(1:length(regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]]), function(x){
            eval(parse(text = paste0(
              "suppressWarnings(suppressMessages(try(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                                                   substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"','.png'),
                                                 plot = ",regression_reference$plots_list[[which(regression_reference$regression_type == regression_type)]][x],"), silent = TRUE)))"
            )))
          })
          plots_list_display <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                       substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','composite_plot','.png')
          display_plot <- TRUE
        } else {
          plots_list <- ""
          plots_list_display <- ""
          display_plot <- FALSE
        }
      }
      display_table <- TRUE
      analysis_outcome <- "Successful"
    }
  }
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
