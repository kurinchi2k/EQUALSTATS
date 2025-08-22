function.Sample_Size_Calculations_Primary <- function(Predefined_lists, rv){
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
    'rv$entry[[3]] <- ', ifelse(rv$entry[[3]] == "", '""', rv$entry[[3]]), '\n',
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
  data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
  data <- na.omit(data)
  func.keep_present_categories_only <- function(variable) {
    factors_in_variable <- levels(data[,variable])
    factors_in_variable <- factors_in_variable[! is.na(match(factors_in_variable, data[,variable]))]
    factor(as.character(data[,variable]), levels = factors_in_variable)
  }
  if (is.factor(data[,1])) {data[,1] <- func.keep_present_categories_only(rv$entry[[1]])}
  data[,2] <- func.keep_present_categories_only(rv$entry[[2]])
  # Do the analysis only if at least two rows are present in at least two categories
  number_of_observations_each_category <- lapply(1:nlevels(data[,2]), function(x) {
    cbind.data.frame(
      level = levels(data[,2])[x],
      `Number of observations` = length(data[data[,2] == levels(data[,2])[x],1])
    )
  })
  number_of_observations_each_category <- do.call(rbind.data.frame, number_of_observations_each_category)
  # Keep only the levels with at least two observations
  number_of_observations_each_category <- number_of_observations_each_category[number_of_observations_each_category$`Number of observations` >= 2,]
  data <- data[data[,2] %in% number_of_observations_each_category$level, ]
  data[,2] <- func.keep_present_categories_only(rv$entry[[2]])
  if (nlevels(data[,2]) >= 2) {
    alpha_power <- data.frame(
      alpha = rep(0.05, 4),
      power = c(0.80, 0.85, 0.90, 0.95)
    )
    plot_title <- paste0(rv$entry[[1]], " (by ", rv$entry[[2]], ")")
    if (rv$entry[[1]] %in% rv$import_data$quantitative) {
      plot <- ggplot(data, aes(x = data[,2], y = data[,1] , colour= data[,2])) + geom_boxplot() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + xlab(rv$entry[[2]]) + ylab(rv$entry[[1]]) + ggtitle(plot_title) + labs(colour=rv$entry[[2]])
      if (nlevels(data[,2]) == 2) {
        test_results <- t.test(data[,1] ~ data[,2])
        effect_size <- cbind.data.frame(test_results$estimate[2] - test_results$estimate[1],sd(data[,1]),(test_results$estimate[2] - test_results$estimate[1])/sd(data[,1]))
        colnames(effect_size) <- c("mean_difference","sd", "effect_size")
        # Sample size calculations
        desired_effect_size <- ifelse(rv$entry[[3]] == "", effect_size$effect_size, as.numeric(rv$entry[[3]])/sd(data[,1]))
        desired_mean_difference <- ifelse(rv$entry[[3]] == "", effect_size$mean_difference, as.numeric(rv$entry[[3]]))
        sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
          cbind.data.frame(
            `Desired mean difference` = desired_mean_difference,
            `Based on:` = if(rv$entry[[3]] != "") {
              "Entered desired mean difference"
            } else {
              paste0("Observed mean difference in ", rv$entry[[1]] ," between ", levels(data[,2])[2], " and ", levels(data[,2])[1])
            },
            alpha_power[z,],
            `Total sample size required` =
              ceiling(pwr.t.test(n =NULL, d = desired_effect_size, sig.level = alpha_power$alpha[z], power = alpha_power$power[z], type = "two.sample")$n)*2
          )
        }), silent = TRUE))
        power_calculations <- suppressWarnings(try(data.frame(Category = levels(data[,2])[2], Power = pwr.t.test(n = nrow(data)/2, d = desired_effect_size, sig.level = 0.05, power = NULL, type = "two.sample")$power), silent = TRUE))
        suppressWarnings({if (str_detect(sample_size_calculations[[1]][1], "Error")) {
          sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
            cbind.data.frame(
              `Desired mean difference` = effect_size$mean_difference,
              `Based on:` = paste0("The entered desired mean difference is too large based on the data uploaded. Therefore, the sample size calculations were based on the observed mean difference in ", rv$entry[[1]] ," between ", levels(data[,2])[2], " and ", levels(data[,2])[1]),
              alpha_power[z,],
              `Total sample size required` =
                ceiling(pwr.t.test(n =NULL, d = effect_size$effect_size, sig.level = alpha_power$alpha[z], power = alpha_power$power[z], type = "two.sample")$n)*2
            )
          }), silent = TRUE))
          power_calculations <- suppressWarnings(try(data.frame(Category = levels(data[,2])[2], Power = pwr.t.test(n = nrow(data)/2, d = effect_size$effect_size, sig.level = 0.05, power = NULL, type = "two.sample")$power), silent = TRUE))
        }
          if (str_detect(sample_size_calculations[[1]][1], "Error")) {
            sample_size_calculations <- cbind.data.frame(
              `Desired mean difference` = desired_mean_difference,
              `Observed mean difference` = effect_size$mean_difference,
              Comment = "The sample size calculations could not be performed for either the desired or observed mean difference. This is probably due to sparse data or too much variability in the data."
            )
            power_calculations <- cbind.data.frame(
              Category = levels(data[,2])[2],
              Power = "The power calculations could not be performed for either the desired or observed mean difference. This is probably due to sparse data or too much variability in the data."
            )
          } else {
            sample_size_calculations <- do.call(rbind.data.frame, sample_size_calculations)
          }
        })
      } else {
        test_results <- aov(data[,1] ~ data[,2])
        effect_size <- as.data.frame(TukeyHSD(test_results)[1])
        effect_size <- effect_size[(1:nlevels(data[,2])-1),]
        effect_size <-cbind(levels(data[,2])[2:nlevels(data[,2])],effect_size[,1:3],
                            tabulate(data[,2])[2:nlevels(data[,2])] + tabulate(data[,2])[1])
        colnames(effect_size) <- c("Category", "Difference","Difference - LCI", "Difference - UCI", "Number of participants")
        effect_size$sd <- sqrt(effect_size$`Number of participants`) * (abs(effect_size$`Difference - UCI` - effect_size$`Difference - LCI`)/3.92)
        effect_size$effect_size <- effect_size$Difference/effect_size$sd
        # Sample size calculations
        if (rv$entry[[3]] != "") {
          desired_effect_size <- as.numeric(rv$entry[[3]])/sd(data[,1])
          desired_mean_difference <- as.numeric(rv$entry[[3]])
        } else {
          desired_effect_size <- effect_size$effect_size
          desired_mean_difference <- effect_size$Difference
        }
        sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
          cbind.data.frame(
            `Desired mean difference` = desired_mean_difference,
            `Based on:` = if(rv$entry[[3]] != "") {
              "Entered desired mean difference"
            } else {
              paste0("Observed mean difference in ", rv$entry[[1]] ," between ", effect_size$Category, " and ", levels(data[,2])[1])
            },
            alpha_power[z,],
            `Total sample size required` = ceiling(sapply(1:length(desired_effect_size), function(x) {pwr.anova.test(k = nlevels(data[,2]), n = NULL, f = abs(desired_effect_size[x]), sig.level = alpha_power$alpha[z], power = alpha_power$power[z])$n * nlevels(data[,2])}))
          )
        }), silent = TRUE))
        # Power calculations
        power_calculations <- suppressWarnings(try(sapply(1:nrow(effect_size), function(x) {pwr.anova.test(k = nlevels(data[,2]), n = nrow(data), f = abs(desired_effect_size), sig.level = 0.05, power = NULL)$power}), silent = TRUE))
        suppressWarnings({
          if (str_detect(sample_size_calculations[[1]][1], "Error")) {
            sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
              cbind.data.frame(
                `Desired mean difference` = effect_size$Difference,
                `Based on:` = paste0("The entered desired mean difference is too large based on the data uploaded. Therefore, the sample size calculations were based on the observed mean difference in ", rv$entry[[1]] ," between ", effect_size$Category, " and ", levels(data[,2])[1]),
                alpha_power[z,],
                `Total sample size required` = ceiling(sapply(1:length(desired_effect_size), function(x) {pwr.anova.test(k = nlevels(data[,2]), n = NULL, f = abs(effect_size$effect_size[x]), sig.level = alpha_power$alpha[z], power = alpha_power$power[z])$n * nlevels(data[,2])}))
              )
            }), silent = TRUE))
            power_calculations <- suppressWarnings(try(sapply(1:nrow(effect_size), function(x) {pwr.anova.test(k = nlevels(data[,2]), n = nrow(data), f = abs(effect_size$effect_size[x]), sig.level = 0.05, power = NULL)$power}), silent = TRUE))
          }
          if (str_detect(sample_size_calculations[[1]][1], "Error")) {
            sample_size_calculations <- cbind.data.frame(
              `Desired mean difference` = desired_mean_difference,
              `Observed mean difference` = effect_size$mean_difference,
              Comment = "The sample size calculations could not be performed for either the desired or observed mean difference. This is probably due to sparse data or too much variability in the data."
            )
            power_calculations <- cbind.data.frame(
              Power = "The power calculations could not be performed for either the desired or observed mean difference. This is probably due to sparse data or too much variability in the data."
            )
          } else {
            sample_size_calculations <- do.call(rbind.data.frame, sample_size_calculations)
            sample_size_calculations <- sample_size_calculations[order(sample_size_calculations$`Based on:`),]
          }
          power_calculations <- cbind.data.frame(Category = effect_size$Category, Power = power_calculations)
        })
      }
    } else {
      # Bar plot
      # Create a new column where the aggregate data is calculated - use 1 to start with
      data[,3] <- rep(1, nrow(data))
      data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
      plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
      data <- data[,1:2]
      # Effect size calculations
      category_totals <- data.frame(colSums(data.frame(unclass(table(data)))))
      category_totals <- data.frame(Category = row.names(category_totals), Totals = category_totals[,1])
      colnames(category_totals) <- c("Category", "Totals")
      proportions <- data.frame(table(data))
      proportions <- data.frame(proportions, Totals = category_totals$Totals[proportions[,2]])
      if (nlevels(data[,1]) == 2) {
        proportions <- cbind.data.frame(proportions, BinomCI(proportions[,3], proportions[,4], method="clopper-pearson"))
      } else {
        confidence_intervals <- lapply(1:nlevels(data[,2]), function(x) {
          MultinomCI(proportions$Freq[proportions[,2] == levels(data[,2])[x]], method = "sisonglaz")
        })
        confidence_intervals <- do.call(rbind.data.frame, confidence_intervals)
        proportions <- cbind.data.frame(proportions, confidence_intervals)
      }
      category_totals$Number <- proportions$Freq[proportions[,1] == levels(data[,1])[2]]
      effect_size <- category_totals[,c(1,3,2)]
      effect_size$Proportion <- effect_size$Number/effect_size$Totals
      effect_size$`Difference in proportion` <- effect_size$Proportion - effect_size$Proportion[1]
      effect_size$`Unpooled variance` <- effect_size$Proportion * (1-effect_size$Proportion)
      effect_size$pooled_sd <- ((effect_size$Totals - 1)*effect_size$`Unpooled variance` + (effect_size$Totals[1] - 1) * effect_size$`Unpooled variance`[1])/(effect_size$Totals + effect_size$Totals[1] - 2)
      effect_size$effect_size <- effect_size$`Difference in proportion`/effect_size$pooled_sd
      effect_size <- effect_size[-1,]
      # Sample size calculations
      if (rv$entry[[3]] != "") {
        pooled_sd <- ((table(data[,1])[2]/nrow(data))*(table(data[,1])[1]/nrow(data)))^0.5
        desired_effect_size <- as.numeric(rv$entry[[3]])/pooled_sd
        desired_difference_in_proportion <- as.numeric(rv$entry[[3]])
      } else {
        desired_effect_size <- effect_size$effect_size
        desired_difference_in_proportion <- effect_size$`Difference in proportion`
      }
      sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
        cbind.data.frame(
          `Desired difference in proportion` = desired_difference_in_proportion,
          `Based on:` = if(rv$entry[[3]] != "") {
            "Entered desired difference in proportion"
          } else {
            paste0("Observed difference in proportion of ", rv$entry[[1]] ," between ", effect_size$Category, " and ", levels(data[,2])[1])
          },
          alpha_power[z,],
          `Total sample size required` = ceiling(sapply(1:length(desired_effect_size), function(x) {pwr.anova.test(k = nlevels(data[,2]), n = NULL, f = abs(desired_effect_size[x]), sig.level = alpha_power$alpha[z], power = alpha_power$power[z])$n * nlevels(data[,2])}))
        )
      }), silent = TRUE))
      # Power calculations
      power_calculations <- cbind.data.frame(
        Category = effect_size$Category,
        Power = pwr.t.test(n = effect_size$Totals, d = desired_effect_size, sig.level = 0.05, power = NULL, type = "two.sample")$power
      )
      suppressWarnings({if (str_detect(sample_size_calculations[[1]][1], "Error")) {
        sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
          cbind.data.frame(
            `Desired difference in proportion` = effect_size$`Difference in proportion`,
            `Based on:` = paste0("The entered desired difference in proportion is too large based on the data uploaded. Therefore, the sample size calculations were based on the observed difference in proportion of ", rv$entry[[1]] ," between ", effect_size$Category, " and ", levels(data[,2])[1]),
            alpha_power[z,],
            `Total sample size required` = ceiling(sapply(1:length(desired_effect_size), function(x) {pwr.anova.test(k = nlevels(data[,2]), n = NULL, f = abs(effect_size$effect_size[x]), sig.level = alpha_power$alpha[z], power = alpha_power$power[z])$n * nlevels(data[,2])}))
          )
        }), silent = TRUE))
        # Power calculations
        power_calculations <- cbind.data.frame(
          Category = effect_size$Category,
          Power = pwr.t.test(n = effect_size$Totals, d = effect_size$effect_size, sig.level = 0.05, power = NULL, type = "two.sample")$power
        )
      }
        if (str_detect(sample_size_calculations[[1]][1], "Error")) {
          sample_size_calculations <- cbind.data.frame(
            `Desired difference in proportion` = desired_difference_in_proportion,
            `Observed difference in proportion` = effect_size$`Difference in proportion`,
            Comment = "The sample size calculations could not be calculated for either the desired or observed difference in proportion. This is probably due to sparse data."
          )
          power_calculations <- cbind.data.frame(
            Category = effect_size$Category,
            Power = "The power calculations could not be performed for either the desired or observed  difference in proportion. This is probably due to sparse data."
          )
        } else {
          sample_size_calculations <- do.call(rbind.data.frame, sample_size_calculations)
          sample_size_calculations <- sample_size_calculations[order(sample_size_calculations$`Based on:`),]
        }
      })
    }
    colnames(power_calculations)[1] <- paste0(rv$entry[[2]]," (versus ", levels(data[,2])[1],")")
    # Create version for display
    power_calculations_display <- power_calculations
    sample_size_calculations_display <- sample_size_calculations
    power_calculations_display[,sapply(power_calculations_display, is.numeric)] <- sapply(power_calculations_display[,sapply(power_calculations_display, is.numeric)], function(x){round(x,4)})
    sample_size_calculations_display[,sapply(sample_size_calculations_display, is.numeric)] <- sapply(sample_size_calculations_display[,sapply(sample_size_calculations_display, is.numeric)], function(x){round(x,4)})
    general_description <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Successful",
      `Total number of observations` = nrow(rv$import_data$data),
      `Number of complete observations` = nrow(data),
      Outcome = rv$entry[[1]],
      Group = rv$entry[[2]],
      check.names = FALSE
    )
    results <- function.rbind_different_column_numbers(list = list(general_description, power_calculations, sample_size_calculations))
    results_display <- function.rbind_different_column_numbers(list = list(general_description, power_calculations_display, sample_size_calculations_display))
    # Plots
    suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                               substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png'),
                                             plot = plot)))
    plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                         substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png')
    plots_list_display <- plots_list
    display_plot <- TRUE
    analysis_outcome <- "Successful"
  } else {
    results_display <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      Outcome = rv$entry[[1]],
      Group = rv$entry[[2]],
      `Reason for unsuccesful analysis` = "There were very few observations in each group to perform an analysis. There must be at least two observations in each group to perform a successful analysis.",
      check.names = FALSE
    )
    plots_list <- ""
    plots_list_display <- plots_list
    analysis_outcome <- "Unsuccessful"
    display_plot <- FALSE
  }
  display_table <- TRUE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
