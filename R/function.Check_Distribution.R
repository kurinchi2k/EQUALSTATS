function.Check_Distribution <- function(Predefined_lists, rv){
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
    '<b>entry_1: </b>', paste0(rv$entry[[1]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- ', ifelse(length(rv$entry[[1]]) > 1,
                                paste0('c("', paste0(rv$entry[[1]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[1]],'"')), '\n',
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
  # Perform tests
  results_shapiro <- suppressWarnings(try(shapiro.test(variable[!is.na(variable)]), silent = TRUE))
  if (str_detect(results_shapiro[[1]][1], "Error")) {
    results_shapiro <- c(NA, NA)
  } else {
    results_shapiro <- as.numeric(c(results_shapiro$statistic, results_shapiro$p.value))
  }
  results_norm <- suppressWarnings(ks.test(variable[!is.na(variable)],"pnorm",mean=mean(variable[!is.na(variable)]),sd=sd(variable[!is.na(variable)])))
  results_norm <- as.numeric(c(results_norm$statistic, results_norm$p.value))
  results_unif <- suppressWarnings(ks.test(variable[!is.na(variable)],"punif",min=min(variable[!is.na(variable)]),max=max(variable[!is.na(variable)])))
  results_unif <- as.numeric(c(results_unif$statistic, results_unif$p.value))
  results_pois <- suppressWarnings(ks.test(variable[!is.na(variable)],"ppois",lambda=mean(variable[!is.na(variable)])))
  results_pois <- as.numeric(c(results_pois$statistic, results_pois$p.value))
  results_lnorm <- suppressWarnings(ks.test(variable[!is.na(variable)],"plnorm",meanlog=mean(log(variable[!is.na(variable)])),sdlog=sd(log(variable[!is.na(variable)]))))
  results_lnorm <- as.numeric(c(results_lnorm$statistic, results_lnorm$p.value))
  results_exp <- suppressWarnings(ks.test(variable[!is.na(variable)],"pexp",rate=1/mean(variable[!is.na(variable)])))
  results_exp <- as.numeric(c(results_exp$statistic, results_exp$p.value))
  # Create tables
  results_display <- data.frame(
    `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, 5)),
    `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, 5)),
    `Analysis outcome` = c("Successful",  rep(NA, 5)),
    `Variable name` = c(rv$entry[[1]], rep(NA, 5)),
    Distribution = c("Normal Distribution: Shapiro-Wilk", "Normal Distribution: Kolmogrov-Smirnov",
                     "Uniform", "Poisson", "Log-normal", "Exponential"),
    Statistic = c("W", rep("D",5)),
    `Statistic value` = c(results_shapiro[1], results_norm[1], results_unif[1], results_pois[1], results_lnorm[1], results_exp[1]),
    `P value` = c(results_shapiro[2], results_norm[2], results_unif[2], results_pois[2], results_lnorm[2], results_exp[2]),
    check.names = FALSE
  )
  results <- rbind.data.frame(
    colnames(results_display),
    results_display
  )
  # Create plots
  # Acceptable types of data for ggplot: data frames or data frame like objects
  data <- data.frame(variable[!is.na(variable)])
  colnames(data) <- rv$entry[[1]]
  histogram <- ggplot(data, aes(x= data[,1])) + geom_histogram(binwidth=1, color="blue", fill="lightblue", aes(y=after_stat(density))) + geom_density(color="navyblue") + xlab(rv$entry[[1]]) + ggtitle(paste0(rv$entry[[1]], ": ", "Histogram")) + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5))
  boxplot <- ggplot(data, aes(y= data[,1])) + geom_boxplot(color="blue", fill="lightblue") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + ggtitle(paste0(rv$entry[[1]], ": ", "Box plot"))
  qqplot <- ggplot(data, aes(sample = data[,1])) + stat_qq() +  stat_qq_line()+ ggtitle(paste0(rv$entry[[1]], ": ", "Q-Q plot")) + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)) + xlab("Theoretical quantiles")+ ylab("Sample quantiles")
  suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                             substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_histogram.png'),
                                           plot = histogram)))
  suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                             substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_boxplot.png'),
                                           plot = boxplot)))
  suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                             substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_qqplot.png'),
                                           plot = qqplot)))
  # Composite plot
  plot_title <- ggdraw() + draw_label(rv$entry[[1]], color="black", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
  histogram <- histogram + ggtitle("Histogram") + theme(plot.title = element_text(color="navyblue", size=10, face="bold", hjust=0.5))
  boxplot <- boxplot + ggtitle("Box plot") + theme(plot.title = element_text(color="navyblue", size=10, face="bold", hjust=0.5))
  qqplot <- qqplot + ggtitle("Q-Q plot") + theme(plot.title = element_text(color="navyblue", size=10, face="bold", hjust=0.5))
  composite_plot <- plot_grid(plot_title, histogram, plot_grid(boxplot, qqplot, ncol = 2), ncol=1, rel_heights = c(0.1,1,1)) + theme(plot.background = element_rect(fill = "white", colour = NA))
  suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                             substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_composite_plot.png'),
                                           plot = composite_plot)))
  plots_list <- c(
    paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
           substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_histogram.png'),
    paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
           substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_boxplot.png'),
    paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
           substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_qqplot.png'),
    paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
           substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_composite_plot.png')
  )
  plots_list_display <-     paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                   substr(str_replace_all(rv$entry[[1]], "[^[:alnum:]]","_"), 1, 80) ,'_composite_plot.png')
  analysis_outcome <- "Successful"
  display_table <- TRUE
  display_plot <- TRUE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
