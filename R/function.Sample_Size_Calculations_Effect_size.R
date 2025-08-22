function.Sample_Size_Calculations_Effect_size <- function(Predefined_lists, rv){
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
    'rv$entry[[1]] <- ', rv$entry[[1]], '\n',
    'rv$entry[[2]] <- ', rv$entry[[2]], '\n',
    'rv$entry[[3]] <- "', rv$entry[[3]], '"\n',
    'rv$entry[[4]] <- "', rv$entry[[4]], '"\n',
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
  alpha_power <- data.frame(
    alpha = c(rep(0.05,3), rep(0.01,3), rep(0.001,3)),
    power = rep(c(0.80, 0.90, 0.95),3)
  )
  if ((rv$entry[[3]] == "Intervention is better than comparator") | (rv$entry[[3]] == "Intervention is not worse than comparator")) {
    alpha <- alpha_power$alpha*2
    power <- alpha_power$power
  } else if (rv$entry[[3]] == "Intervention is equivalent to the comparator") {
    alpha <- alpha_power$alpha*2
    power <- 1-((1-alpha_power$power)/2)
  } else {
    alpha <- alpha_power$alpha
    power <- alpha_power$power
  }
  if (rv$entry[[4]] == "Dependent samples") {
    type = "paired"
  } else {
    type = "two.sample"
  }
  if (rv$second_menu_choice == "Intervention study (continuous outcome)") {
    desired_effect_size <- abs(rv$entry[[1]]/rv$entry[[2]])
    if (desired_effect_size == 0) {desired_effect_size <- 0.01}
    desired_mean_difference <- rv$entry[[1]]
    sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
      cbind.data.frame(
        `Desired mean difference` = desired_mean_difference,
        alpha_power[z,],
        `Sample size required (each group)` =
          ceiling(pwr.t.test(n =NULL, d = desired_effect_size, sig.level = alpha[z], power = power[z], type = type)$n)
      )
    }), silent = TRUE))
    if (str_detect(sample_size_calculations[[1]][1], "Error")) {
      sample_size_calculations <- cbind.data.frame(
        `Desired mean difference` = desired_mean_difference,
        alpha_power,
        `Sample size required (each group)` = "The standard deviation is too small compared to the desired difference"
      )
    } else {
      sample_size_calculations <- do.call(rbind.data.frame, sample_size_calculations)
      sample_size_calculations$`Total sample size required` <- sample_size_calculations$`Sample size required (each group)`*2
    }
  } else if (rv$second_menu_choice == "Intervention study (binary outcome)") {
    desired_effect_size <- abs(ES.h(rv$entry[[1]], rv$entry[[2]]))
    if (desired_effect_size == 0) {desired_effect_size <- 0.01}
    desired_difference_in_propoportion <- rv$entry[[1]] - rv$entry[[2]]
    sample_size_calculations <- suppressWarnings(try(lapply(1:nrow(alpha_power), function(z) {
      cbind.data.frame(
        `Desired difference in proportion` = desired_difference_in_propoportion,
        alpha_power[z,],
        `Sample size required (each group)` =
          if (rv$entry[[4]] == "Dependent samples") {
            ceiling(pwr.t.test(n =NULL, d = desired_effect_size, sig.level = alpha[z], power = power[z], type = type)$n)
          } else {
            ceiling(pwr.2p.test(h=desired_effect_size, n = NULL, sig.level = alpha[z], power[z])$n)
          }
      )
    }), silent = TRUE))
    if (str_detect(sample_size_calculations[[1]][1], "Error")) {
      sample_size_calculations <- cbind.data.frame(
        `Desired difference in proportion` = desired_difference_in_propoportion,
        alpha_power,
        `Sample size required (each group)` = "The desired difference in proportions is too large"
      )
    } else {
      sample_size_calculations <- do.call(rbind.data.frame, sample_size_calculations)
      sample_size_calculations$`Total sample size required` <- sample_size_calculations$`Sample size required (each group)`*2
    }
  }
  # Create version for display
  sample_size_calculations_display <- sample_size_calculations
  sample_size_calculations_display[,sapply(sample_size_calculations_display, is.numeric)] <- sapply(sample_size_calculations_display[,sapply(sample_size_calculations_display, is.numeric)], function(x){round(x,4)})
  general_description <- data.frame(
    `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
    `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
    `Analysis outcome` = "Successful",
    check.names = FALSE
  )
  results <- function.rbind_different_column_numbers(list = list(general_description, sample_size_calculations))
  results_display <- function.rbind_different_column_numbers(list = list(general_description, sample_size_calculations_display))
  # Plots
  display_table <- TRUE
  plots_list <- ""
  plots_list_display <- plots_list
  display_plot <- FALSE
  analysis_outcome <- "Successful"
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list, plots_list_display = plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
