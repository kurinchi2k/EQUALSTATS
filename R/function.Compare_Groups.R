function.Compare_Groups <- function(Predefined_lists, rv){
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
  # Some generic processing of data to remove unrepresented factors
  func.keep_present_categories_only <- function(variable, data) {
    factors_in_variable <- levels(data[,variable])
    is.ordinal <- (variable %in%  rv$import_data$ordinal)
    factors_in_variable <- factors_in_variable[! is.na(match(factors_in_variable, data[,variable]))]
    factor(as.character(data[,variable]), levels = factors_in_variable, ordered = is.ordinal)
  }
  # Summary
  func.summary.categorical <- function(variable, prefix) {
    if(nlevels(variable) == 2) {
      summary <- data.frame(sapply(
        1:nlevels(variable), function(x) {
          as.numeric(BinomCI(length(variable[(!is.na(variable)) & variable == levels(variable)[x]]), length(variable[! is.na(variable)]), conf.level = 0.95))
        }
      ), check.names = FALSE)
      colnames(summary) <- paste0(prefix, ": ", levels(variable))
      row.names(summary) <- c("Proportion", "Proportion - LCI", "Proportion - UCI")
    } else {
      summary <- data.frame(t(MultinomCI(table(variable), conf.level = 0.95)),row.names = c("Proportion", "Proportion - LCI", "Proportion - UCI"), check.names = FALSE)
      colnames(summary) <- paste0(prefix, ": ", colnames(summary))
    }
    return(summary)
  }
  func.summary.quantitative <- function(variable, prefix, normality) {
    if (length(variable) == 1) {
      summary <- c(prefix, variable, NA, NA)
    } else {
      if (normality == TRUE) {
        mean_ci <- suppressWarnings(try(MeanCI(variable, conf.level = 0.95), silent = TRUE))
        if (length(mean_ci) < 3) {mean_ci <- c(NA, NA, NA)}
        summary <- c(prefix, as.numeric(mean_ci))
      } else {
        median_ci <- suppressWarnings(try(MedianCI(variable, conf.level = 0.95), silent = TRUE))
        if (length(median_ci) < 3) {median_ci <- c(NA, NA, NA)}
        summary <- c(prefix, as.numeric(median_ci))
      }
    }
    return(summary)
  }
  # Calculating the confidence intervals for difference in proportion
  diff_proportion <- function(data, alpha) {
    z = qnorm(1-alpha/2)
    contingency_table <- table(data[,2],data[,1])
    # If this is ordinal, differences should be calculated for cumulative proportions
    if (rv$entry[[2]] %in% rv$import_data$ordinal) {
      if (nrow(contingency_table) > 2) {
        cumulative_proportions_versus_reference <- lapply(3:nrow(contingency_table), function(x) {
          colSums(contingency_table[2:x,])
        })
        contingency_table <- rbind(contingency_table[1:2,],  do.call(rbind, cumulative_proportions_versus_reference))
        row.names(contingency_table)[3] <- paste0(levels(data[,2])[2], " or ", levels(data[,2])[3])
        if (nrow(contingency_table) > 3) {
          row.names(contingency_table)[4:nrow(contingency_table)] <- sapply(4:nrow(contingency_table), function(x) {
            paste0(paste0(levels(data[,2])[2:(x-1)], collapse = ", "), ", or ",  levels(data[,2])[x])
          })
        }
      }
    }
    contingency_tables_list <- lapply(2:nrow(contingency_table), function(x) {
      each_event <- lapply(2:ncol(contingency_table), function(y) {
        cbind.data.frame(
          Comparison = paste0(row.names(contingency_table)[x], " vs ", row.names(contingency_table)[1], ": ",
                              colnames(contingency_table)[y],
                              " vs ", colnames(contingency_table)[1]),
          not_rc = contingency_table[1,1],
          rc = contingency_table[1,y],
          not_rt = contingency_table[x,1],
          rt = contingency_table[x,y]
        )
      })
      each_group_vs_reference <- if(length(each_event) > 1) {do.call(rbind.data.frame, each_event)} else {each_event[[1]]}
      return(each_group_vs_reference)
    })
    contingency_tables <- if (length(contingency_tables_list) > 1) {do.call(rbind.data.frame, contingency_tables_list)} else {contingency_tables_list[[1]]}
    contingency_tables$nt <- contingency_tables$rt + contingency_tables$not_rt
    contingency_tables$pt <- contingency_tables$rt/contingency_tables$nt
    contingency_tables$qt <- 1-contingency_tables$pt
    contingency_tables$At <- 2*contingency_tables$rt + z^2
    contingency_tables$Bt <- z*(z^2 + 4 * contingency_tables$rt * contingency_tables$qt)^0.5
    contingency_tables$Ct <- 2 *(contingency_tables$nt + z^2)
    contingency_tables$lt <- (contingency_tables$At - contingency_tables$Bt)/contingency_tables$Ct
    contingency_tables$ut <- (contingency_tables$At + contingency_tables$Bt)/contingency_tables$Ct
    contingency_tables$nc <- contingency_tables$rc + contingency_tables$not_rc
    contingency_tables$pc <- contingency_tables$rc/contingency_tables$nc
    contingency_tables$qc <- 1-contingency_tables$pc
    contingency_tables$Ac <- 2*contingency_tables$rc + z^2
    contingency_tables$Bc <- z*(z^2 + 4 * contingency_tables$rc * contingency_tables$qc)^0.5
    contingency_tables$Cc <- 2 *(contingency_tables$nc + z^2)
    contingency_tables$lc <- (contingency_tables$Ac - contingency_tables$Bc)/contingency_tables$Cc
    contingency_tables$uc <- (contingency_tables$Ac + contingency_tables$Bc)/contingency_tables$Cc
    contingency_tables$Difference <- contingency_tables$pt - contingency_tables$pc
    contingency_tables$`Difference - LCI` <- contingency_tables$Difference -
      ((contingency_tables$pt - contingency_tables$lt)^2 + (contingency_tables$uc - contingency_tables$pc)^2)^0.5
    contingency_tables$`Difference - UCI` <- contingency_tables$Difference +
      ((contingency_tables$pc - contingency_tables$lc)^2 + (contingency_tables$ut - contingency_tables$pt)^2)^0.5
    Differences <- contingency_tables[,c("Comparison", "Difference", "Difference - LCI", "Difference - UCI")]
    return(Differences)
  }
  # Calculating the bootstrap confidence intervals for difference in median
  diff_median <- function(data, x){diff(tapply(data[,1][x], data[,2][x], FUN = median, na.rm = TRUE))}
  if (rv$entry[[3]] == "") {alpha = 0.05} else {alpha = as.numeric(rv$entry[[3]])}
  # Now with the data
  data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
  data <- na.omit(data)
  data[,2] <- func.keep_present_categories_only(rv$entry[[2]], data)
  # Get the number of observations for each level of data
  data_table <- data.frame(table(data[,2]), check.names = FALSE)
  # Do the analysis only if at least two rows are present for at least two levels
  if (length(data_table$Freq[data_table$Freq >= 2]) >= 2) {
    if (rv$entry[[1]] %in% rv$import_data$categorical) {
      data[,1] <- func.keep_present_categories_only(rv$entry[[1]], data)
      eachgroup <- list()
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.summary.categorical(variable = data[data[,2] == levels(data[,2])[i],1], prefix = levels(data[,2])[i])
      }
      descriptive_summary <- t(do.call(cbind.data.frame, eachgroup))
      descriptive_summary <- cbind.data.frame(Categories = row.names(descriptive_summary), descriptive_summary)
    }
    # Perform tests
    if (rv$second_menu_choice == "EQUAL-STATS choice") {
      # If this is a categorical binary variable, it is Fisher's exact test
      # If outcome variable is binary and exposure variable is ordinal, it is Cochrane-Armitage trend test
      # For other categorical variables, it is Chi-square test
      if (rv$entry[[1]] %in% rv$import_data$categorical) {
        differences <- diff_proportion(data, alpha)
        if ((rv$entry[[1]] %in% rv$import_data$binary) & (rv$entry[[2]] %in% rv$import_data$binary)) {
          test_results <- fisher.test(table(data[,2],data[,1]))
          results_display <- data.frame(
            `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
            `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
            `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
            `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
            `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
            `Test used` = c("Fisher's exact test", rep(NA, nrow(descriptive_summary)-1)),
            `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
            descriptive_summary,
            check.names = FALSE
          )
        } else if ((rv$entry[[1]] %in% rv$import_data$binary) & (rv$entry[[2]] %in% rv$import_data$ordinal)) {
          contingency_table <- table(data[,1],data[,2])
          test_results <- suppressWarnings(prop.trend.test(x = contingency_table[2,], n = (contingency_table[1,] + contingency_table[2,])))
          results_display <- data.frame(
            `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
            `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
            `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
            `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
            `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
            `Test used` = c("Chi-squared test for trend in proportions", rep(NA, nrow(descriptive_summary)-1)),
            Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
            `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
            `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
            descriptive_summary,
            check.names = FALSE
          )
        } else {
          test_results <- suppressWarnings(chisq.test(table(data[,2],data[,1])))
          results_display <- data.frame(
            `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
            `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
            `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
            `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
            `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
            `Test used` = c("Chi-squared test", rep(NA, nrow(descriptive_summary)-1)),
            Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
            `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
            `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
            descriptive_summary,
            check.names = FALSE
          )
        }
      } else {
        # For quantitative variables, if normal: two levels = T-test; multiple levels = ANOVA; if non-normal: two levels = Mann-Whitney; multiple levels = Kruskal-Wallis test
        normality <- func.check.normality(variable = data[,1])
        eachgroup <- list()
        for (i in 1:nlevels(data[,2])) {
          eachgroup[[i]] <- func.summary.quantitative(variable = data[data[,2] == levels(data[,2])[i],1], prefix = levels(data[,2])[i], normality = (normality == "No evidence that it is non-normal"))
        }
        descriptive_summary <- do.call(rbind.data.frame, eachgroup)
        descriptive_summary[,2:4] <- sapply(descriptive_summary[,2:4], as.numeric)
        if (normality == "No evidence that it is non-normal") {
          colnames(descriptive_summary) <- c("Categories", "Mean", "Mean - LCI", "Mean - LCI")
          if (nrow(data_table) == 2) {
            test_results <- t.test(data[,1] ~ data[,2], conf.level = (1-alpha))
            results_display <- suppressWarnings(data.frame(
              `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
              `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
              `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
              `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
              `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
              descriptive_summary,
              `Difference in means` = c(NA, test_results$estimate[2] - test_results$estimate[1]),
              `Difference in means - LCI` = c(NA, - test_results$conf.int[2]),
              `Difference in means - UCI` = c(NA, - test_results$conf.int[1]),
              `Test used` = c("T-test", rep(NA, nrow(descriptive_summary)-1)),
              Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
              `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
              `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
              check.names = FALSE
            ))
          } else {
            test_results <- summary(aov(data[,1] ~ data[,2]))[[1]]
            differences <- lapply(1:nlevels(data[,2]), function(x) {
              if (x == 1) {
                cbind.data.frame(
                  `Difference in means` = NA,
                  `Difference in means - LCI` = NA,
                  `Difference in means - UCI` = NA
                )
              } else {
                data_x_1 <- data[((data[,2] == levels(data[,2])[1]) | (data[,2] == levels(data[,2])[x])),]
                data_x_1[,2] <- func.keep_present_categories_only(rv$entry[[2]], data_x_1)
                test_results_x <- t.test(data_x_1[,1] ~ data_x_1[,2], conf.level = (1-alpha))
                cbind.data.frame(
                  `Difference in means` = test_results_x$estimate[2] - test_results_x$estimate[1],
                  `Difference in means - LCI` = - test_results_x$conf.int[2],
                  `Difference in means - UCI` = - test_results_x$conf.int[1]
                )
              }
            })
            results_display <- data.frame(
              `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
              `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
              `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
              `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
              `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
              descriptive_summary,
              do.call(rbind.data.frame, differences),
              `Test used` = c("One-way ANOVA", rep(NA, nrow(descriptive_summary)-1)),
              Statistic = c("F", rep(NA, nrow(descriptive_summary)-1)),
              `Statistic value` = c(as.numeric(test_results$`F value`[1]), rep(NA, nrow(descriptive_summary)-1)),
              `P value` = c(as.numeric(test_results$`Pr(>F)`[1]), rep(NA, nrow(descriptive_summary)-1)),
              check.names = FALSE
            )
          }
        } else {
          colnames(descriptive_summary) <- c("Categories", "Median", "Median - LCI", "Median - UCI")
          if (nrow(data_table) == 2) {
            test_results <- suppressWarnings(wilcox.test(data[,1] ~ data[,2]))
            bootstrap_CI <- suppressWarnings(try(boot::boot(data, statistic = diff_median, 10000), silent = TRUE))
            if (str_detect(bootstrap_CI[[1]][1], "Error")) {
              bootstrap_CI <- suppressWarnings(try(boot::boot(data, statistic = diff_median, 10000), silent = TRUE))
            }
            if (str_detect(bootstrap_CI[[1]][1], "Error")) {
              differences <- cbind.data.frame(
                `Difference in medians` = "Not estimable",
                `Difference in medians - LCI` = "Not estimable",
                `Difference in medians - UCI` = "Not estimable"
              )
            } else {
              differences <- cbind.data.frame(
                `Difference in medians` = quantile(bootstrap_CI$t, 0.5, na.rm = TRUE),
                `Difference in medians - LCI` = quantile(bootstrap_CI$t, alpha/2, na.rm = TRUE),
                `Difference in medians - UCI` = quantile(bootstrap_CI$t, 1-alpha/2, na.rm = TRUE)
              )
            }
            results_display <- suppressWarnings(data.frame(
              `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
              `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
              `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
              `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
              `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
              descriptive_summary,
              differences,
              `Test used` = c("Mann-Whitney U test", rep(NA, nrow(descriptive_summary)-1)),
              Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
              `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
              `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
              check.names = FALSE
            ))
          } else {
            test_results <- suppressWarnings(kruskal.test(data[,1] ~ data[,2]))
            differences <- lapply(1:nlevels(data[,2]), function(x) {
              if (x == 1) {
                cbind.data.frame(
                  `Difference in medians` = NA,
                  `Difference in medians - LCI` = NA,
                  `Difference in medians - UCI` = NA
                )
              } else {
                data_x_1 <- data[((data[,2] == levels(data[,2])[1]) | (data[,2] == levels(data[,2])[x])),]
                data_x_1[,2] <- func.keep_present_categories_only(rv$entry[[2]], data_x_1)
                bootstrap_CI <- suppressWarnings(try(boot::boot(data_x_1, statistic = diff_median, 10000), silent = TRUE))
                if (str_detect(bootstrap_CI[[1]][1], "Error")) {
                  bootstrap_CI <- suppressWarnings(try(boot::boot(data_x_1, statistic = diff_median, 10000), silent = TRUE))
                }
                if (str_detect(bootstrap_CI[[1]][1], "Error")) {
                  cbind.data.frame(
                    `Difference in medians` = "Not estimable",
                    `Difference in medians - LCI` = "Not estimable",
                    `Difference in medians - UCI` = "Not estimable"
                  )
                } else {
                  cbind.data.frame(
                    `Difference in medians` = quantile(bootstrap_CI$t, 0.5, na.rm = TRUE),
                    `Difference in medians - LCI` = quantile(bootstrap_CI$t, alpha/2, na.rm = TRUE),
                    `Difference in medians - UCI` = quantile(bootstrap_CI$t, 1-alpha/2, na.rm = TRUE)
                  )
                }
              }
            })
            results_display <- data.frame(
              `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
              `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
              `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
              `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
              `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
              descriptive_summary,
              do.call(rbind.data.frame, differences),
              `Test used` = c("Kruskal-Wallis Test", rep(NA, nrow(descriptive_summary)-1)),
              Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
              `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
              `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
              check.names = FALSE
            )
          }
        }
      }
    } else if (rv$second_menu_choice == "Fishers exact test") {
      test_results <- fisher.test(table(data[,2],data[,1]))
      differences <- diff_proportion(data, alpha)
      results_display <- data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        check.names = FALSE
      )
    } else if (rv$second_menu_choice == "Chi-square test") {
      test_results <- suppressWarnings(chisq.test(table(data[,2],data[,1])))
      differences <- diff_proportion(data, alpha)
      results_display <- data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        check.names = FALSE
      )
    } else if (rv$second_menu_choice == "Chi-square test for trend") {
      contingency_table <- table(data[,1],data[,2])
      test_results <- suppressWarnings(prop.trend.test(x = contingency_table[2,], n = (contingency_table[1,] + contingency_table[2,])))
      differences <- diff_proportion(data, alpha)
      results_display <- data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        `Test used` = c("Chi-squared test for trend in proportions", rep(NA, nrow(descriptive_summary)-1)),
        Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        check.names = FALSE
      )
    } else if (rv$second_menu_choice == "T-test") {
      eachgroup <- list()
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.summary.quantitative(variable = data[data[,2] == levels(data[,2])[i],1], prefix = levels(data[,2])[i], normality = TRUE)
      }
      descriptive_summary <- do.call(rbind.data.frame, eachgroup)
      descriptive_summary[,2:4] <- sapply(descriptive_summary[,2:4], as.numeric)
      colnames(descriptive_summary) <- c("Categories", "Mean", "Mean - LCI", "Mean - LCI")
      test_results <- t.test(data[,1] ~ data[,2], conf.level = (1-alpha))
      results_display <- suppressWarnings(data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        `Difference in means` = c(NA, test_results$estimate[2] - test_results$estimate[1]),
        `Difference in means - LCI` = c(NA, - test_results$conf.int[2]),
        `Difference in means - UCI` = c(NA, - test_results$conf.int[1]),
        `Test used` = c("T-test", rep(NA, nrow(descriptive_summary)-1)),
        Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
        check.names = FALSE
      ))
    } else if (rv$second_menu_choice == "One-way ANOVA") {
      eachgroup <- list()
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.summary.quantitative(variable = data[data[,2] == levels(data[,2])[i],1], prefix = levels(data[,2])[i], normality = TRUE)
      }
      descriptive_summary <- do.call(rbind.data.frame, eachgroup)
      descriptive_summary[,2:4] <- sapply(descriptive_summary[,2:4], as.numeric)
      colnames(descriptive_summary) <- c("Categories", "Mean", "Mean - LCI", "Mean - LCI")
      test_results <- summary(aov(data[,1] ~ data[,2]))[[1]]
      differences <- lapply(1:nlevels(data[,2]), function(x) {
        if (x == 1) {
          cbind.data.frame(
            `Difference in means` = NA,
            `Difference in means - LCI` = NA,
            `Difference in means - UCI` = NA
          )
        } else {
          data_x_1 <- data[((data[,2] == levels(data[,2])[1]) | (data[,2] == levels(data[,2])[x])),]
          data_x_1[,2] <- func.keep_present_categories_only(rv$entry[[2]], data_x_1)
          test_results_x <- t.test(data_x_1[,1] ~ data_x_1[,2], conf.level = (1-alpha))
          cbind.data.frame(
            `Difference in means` = test_results_x$estimate[2] - test_results_x$estimate[1],
            `Difference in means - LCI` = - test_results_x$conf.int[2],
            `Difference in means - UCI` = - test_results_x$conf.int[1]
          )
        }
      })
      results_display <- data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        do.call(rbind.data.frame, differences),
        `Test used` = c("One-way ANOVA", rep(NA, nrow(descriptive_summary)-1)),
        Statistic = c("F", rep(NA, nrow(descriptive_summary)-1)),
        `Statistic value` = c(as.numeric(test_results$`F value`[1]), rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$`Pr(>F)`[1]), rep(NA, nrow(descriptive_summary)-1)),
        check.names = FALSE
      )
    } else if (rv$second_menu_choice == "Mann-Whitney U test") {
      eachgroup <- list()
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.summary.quantitative(variable = data[data[,2] == levels(data[,2])[i],1], prefix = levels(data[,2])[i], normality = FALSE)
      }
      descriptive_summary <- do.call(rbind.data.frame, eachgroup)
      descriptive_summary[,2:4] <- sapply(descriptive_summary[,2:4], as.numeric)
      colnames(descriptive_summary) <- c("Categories", "Median", "Median - LCI", "Median - UCI")
      test_results <- suppressWarnings(wilcox.test(data[,1] ~ data[,2]))
      bootstrap_CI <- suppressWarnings(try(boot::boot(data, statistic = diff_median, 10000), silent = TRUE))
      if (str_detect(bootstrap_CI[[1]][1], "Error")) {
        bootstrap_CI <- suppressWarnings(try(boot::boot(data, statistic = diff_median, 10000), silent = TRUE))
      }
      if (str_detect(bootstrap_CI[[1]][1], "Error")) {
        differences <- cbind.data.frame(
          `Difference in medians` = "Not estimable",
          `Difference in medians - LCI` = "Not estimable",
          `Difference in medians - UCI` = "Not estimable"
        )
      } else {
        differences <- cbind.data.frame(
          `Difference in medians` = quantile(bootstrap_CI$t, 0.5, na.rm = TRUE),
          `Difference in medians - LCI` = quantile(bootstrap_CI$t, alpha/2, na.rm = TRUE),
          `Difference in medians - UCI` = quantile(bootstrap_CI$t, 1-alpha/2, na.rm = TRUE)
        )
      }
      results_display <- suppressWarnings(data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        differences,
        `Test used` = c("Mann-Whitney U test", rep(NA, nrow(descriptive_summary)-1)),
        Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
        check.names = FALSE
      ))
    } else if (rv$second_menu_choice == "Kruskal-Wallis Test") {
      eachgroup <- list()
      for (i in 1:nlevels(data[,2])) {
        eachgroup[[i]] <- func.summary.quantitative(variable = data[data[,2] == levels(data[,2])[i],1], prefix = levels(data[,2])[i], normality = FALSE)
      }
      descriptive_summary <- do.call(rbind.data.frame, eachgroup)
      descriptive_summary[,2:4] <- sapply(descriptive_summary[,2:4], as.numeric)
      colnames(descriptive_summary) <- c("Categories", "Median", "Median - LCI", "Median - UCI")
      test_results <- suppressWarnings(kruskal.test(data[,1] ~ data[,2]))
      differences <- lapply(1:nlevels(data[,2]), function(x) {
        if (x == 1) {
          cbind.data.frame(
            `Difference in medians` = NA,
            `Difference in medians - LCI` = NA,
            `Difference in medians - UCI` = NA
          )
        } else {
          data_x_1 <- data[((data[,2] == levels(data[,2])[1]) | (data[,2] == levels(data[,2])[x])),]
          data_x_1[,2] <- func.keep_present_categories_only(rv$entry[[2]], data_x_1)
          bootstrap_CI <- suppressWarnings(try(boot::boot(data_x_1, statistic = diff_median, 10000), silent = TRUE))
          if (str_detect(bootstrap_CI[[1]][1], "Error")) {
            bootstrap_CI <- suppressWarnings(try(boot::boot(data_x_1, statistic = diff_median, 10000), silent = TRUE))
          }
          if (str_detect(bootstrap_CI[[1]][1], "Error")) {
            cbind.data.frame(
              `Difference in medians` = "Not estimable",
              `Difference in medians - LCI` = "Not estimable",
              `Difference in medians - UCI` = "Not estimable"
            )
          } else {
            cbind.data.frame(
              `Difference in medians` = quantile(bootstrap_CI$t, 0.5, na.rm = TRUE),
              `Difference in medians - LCI` = quantile(bootstrap_CI$t, alpha/2, na.rm = TRUE),
              `Difference in medians - UCI` = quantile(bootstrap_CI$t, 1-alpha/2, na.rm = TRUE)
            )
          }
        }
      })
      results_display <- data.frame(
        `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
        `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
        `Analysis outcome` = c("Successful",  rep(NA, nrow(descriptive_summary)-1)),
        `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
        `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
        descriptive_summary,
        do.call(rbind.data.frame, differences),
        `Test used` = c("Kruskal-Wallis Test", rep(NA, nrow(descriptive_summary)-1)),
        Statistic = c(names(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `Statistic value` = c(as.numeric(test_results$statistic), rep(NA, nrow(descriptive_summary)-1)),
        `P value` = c(as.numeric(test_results$p.value), rep(NA, nrow(descriptive_summary)-1)),
        check.names = FALSE
      )
    }
    # Rounding for display - Different procedures for categorical and quantitative outcomes
    if (rv$entry[[1]] %in% rv$import_data$categorical) {
      results <- rbind.data.frame(
        colnames(results_display),
        results_display
      )
      results <- function.rbind_different_column_numbers(list(results, differences))
      results_display[,sapply(results_display, is.numeric)] <- sapply(results_display[,sapply(results_display, is.numeric)], function(x){round(x,4)})
      differences_display <- differences
      differences_display[,sapply(differences_display, is.numeric)] <- sapply(differences_display[,sapply(differences_display, is.numeric)], function(x){round(x,4)})
      results_display <- function.rbind_different_column_numbers(list(results_display, differences_display))
    } else {
      results <- rbind.data.frame(
        colnames(results_display),
        results_display
      )
    }
    # Create plots
    plot_title <- paste0(rv$entry[[1]], " by ", rv$entry[[2]])
    # Clustered bar plots for categorical variables and bar plots and box plots for quantitative variables
    if (rv$entry[[1]] %in% rv$import_data$categorical) {
      # Create a new column where the aggregate data is calculated - use 1 to start with
      data[,3] <- rep(1, nrow(data))
      data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
      barplot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
      suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                                 substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png'),
                                               plot = barplot)))
      plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                           substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png')
      plots_list_display <- plots_list
    } else {
      boxplot_display <-ggplot(data, aes(x= data[,2], y= data[,1] , colour= data[,2])) + geom_boxplot() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + xlab(rv$entry[[2]]) + ylab(rv$entry[[1]]) + labs(colour=rv$entry[[2]])
      data_2 <- data.frame(Average = descriptive_summary[,2], Categories = descriptive_summary[,1])
      lower_limit <- ifelse(min(data_2$Average) < 0, pretty(min(data_2$Average)), 0)
      upper_limit <- pretty(data_2$Average)[length(pretty(data_2$Average))]
      y_ticks <- pretty(lower_limit:upper_limit, n = 4)
      lower_limit <- min(y_ticks)
      upper_limit <- max(y_ticks)
      barplot_display <- ggplot(data_2, aes(x = Categories, y = Average, fill = Categories)) + geom_bar(position = "dodge", stat="identity") +
        theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) +
        ylab(colnames(descriptive_summary)[2]) + xlab(rv$entry[[2]]) +
        labs(fill=levels(rv$entry[[2]])) + theme(axis.title.x = element_blank()) + scale_y_continuous(limits = c(lower_limit, upper_limit), breaks = y_ticks)
      plot_title_combined <- ggdraw() + draw_label(plot_title, color="navyblue", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
      composite_plot <- plot_grid(plot_title_combined, boxplot_display, barplot_display, ncol=1, rel_heights = c(0.1,1,1)) + theme(plot.background = element_rect(fill = "white", colour = NA))
      boxplot <- boxplot_display + ggtitle(plot_title)
      barplot <- barplot_display + ggtitle(plot_title)
      suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                                 substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_boxplot.png'),
                                               plot = boxplot)))
      suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                                 substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_barplot.png'),
                                               plot = barplot)))
      suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                                 substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_composite_plot.png'),
                                               plot = composite_plot)))
      plots_list <- c(
        paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
               substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_boxplot.png'),
        paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
               substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_barplot.png'),
        paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
               substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_composite_plot.png')
      )
      plots_list_display <-     paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                       substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_composite_plot.png')
    }
    analysis_outcome <- "Successful"
    display_plot <- TRUE
  } else {
    results_display <- data.frame(
      `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
      `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
      `Analysis outcome` = c("Unsuccessful",  rep(NA, nrow(descriptive_summary)-1)),
      `Variable name` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
      `Group name` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
      `Reason for unsuccesful analysis` = "There were very few observations to perform an analysis. There must be at least two valid observations of the first variable in at least two groups to perform a successful analysis.",
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
