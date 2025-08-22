function.plan_upload_no_data <- function(plan_file_path, Predefined_lists, rv, no_data_choices){
  plan <- read.csv(plan_file_path, check.names = FALSE, na.strings = c(""," ","  ", "NA"))
  # First find whether the column names match
  if (FALSE %in% (colnames(plan) == c("analysis_number", "first_menu_choice", "second_menu_choice", "entry_1", "entry_2", "entry_3", "entry_4", "entry_5", "entry_6", "entry_7", "entry_8", "entry_9", "entry_10", "entry_11", "entry_12", "entry_13", "entry_14", "entry_15", "same_row_different_row"))) {
    text_to_evaluate <- paste0(
      "output$htmlMessage <- renderUI(HTML('<h2 style = ",
      '"color:darkred; background-color:yellow"><b>The column names in the uploaded plan does not match with the column names of a plan prepared using this program. Please upload a plan prepared using this program or click on "Start data analysis" to create a plan', "'))"
    )
  } else {
    # Find whether the first menu choices match with those of the program
    if (TRUE %in% is.na(match(plan$first_menu_choice, no_data_choices))) {
      text_to_evaluate <- paste0(
        "output$htmlMessage <- renderUI(HTML('<h2 style = ",
        '"color:darkred; background-color:yellow"><b>The uploaded plan does not match with the functions available without data upload for this program. Please upload a plan prepared using this program or click on "Start data analysis" to create a plan', "'))"
      )
    } else {
      # Find whether the second menu choices match with the corresponding first menu choices
      second_menu_choice_passed_check <- sapply(1:nrow(plan), function(x) {
        second_menu_choice <- str_split(Predefined_lists$second_menu_choices[Predefined_lists$menu_short == plan$first_menu_choice[x]], "%__%")
        if (second_menu_choice == "") {
          outcome = "passed check"
        } else {
          if (is.na(plan$second_menu_choice[x]) | (!plan$second_menu_choice[x] %in% second_menu_choice[[1]])) {
            outcome = "failed check"
          } else {
            outcome = "passed check"
          }
        }
        return(outcome)
      })
      if (TRUE %in% (second_menu_choice_passed_check == "failed check")) {
        text_to_evaluate <- paste0(
          "output$htmlMessage <- renderUI(HTML('<h2 style = ",
          '"color:darkred; background-color:yellow"><b>The uploaded plan does not match with the functions available without data upload for this program. Please upload a plan prepared using this program or click on "Start data analysis" to create a plan', "'))"
        )
      } else {
        # Perform the analysis
        plots_list <- list()
        withProgress(message = 'Performing analysis...', value = 0, {
          for (i in 1:nrow(plan)){
            incProgress(1/nrow(plan), detail = paste0(i, " of ", nrow(plan), " analyses" ))
            rv$first_menu_choice <- plan$first_menu_choice[i]
            rv$second_menu_choice <- plan$second_menu_choice[i]
            rv$entry[[1]] <- ifelse(is.na(plan$entry_1[i]), "", plan$entry_1[i])
            rv$entry[[2]] <- ifelse(is.na(plan$entry_2[i]), "", plan$entry_2[i])
            rv$entry[[3]] <- ifelse(is.na(plan$entry_3[i]), "", plan$entry_3[i])
            rv$entry[[4]] <- ifelse(is.na(plan$entry_4[i]), "", plan$entry_4[i])
            rv$entry[[5]] <- ifelse(is.na(plan$entry_5[i]), "", plan$entry_5[i])
            rv$entry[[6]] <- ifelse(is.na(plan$entry_6[i]), "", plan$entry_6[i])
            rv$entry[[7]] <- ifelse(is.na(plan$entry_7[i]), "", plan$entry_7[i])
            rv$entry[[8]] <- ifelse(is.na(plan$entry_8[i]), "", plan$entry_8[i])
            rv$entry[[9]] <- ifelse(is.na(plan$entry_9[i]), "", plan$entry_9[i])
            rv$entry[[10]] <- ifelse(is.na(plan$entry_10[i]), "", plan$entry_10[i])
            rv$entry[[11]] <- ifelse(is.na(plan$entry_11[i]), "", plan$entry_11[i])
            rv$entry[[12]] <- ifelse(is.na(plan$entry_12[i]), "", plan$entry_12[i])
            rv$entry[[13]] <- ifelse(is.na(plan$entry_13[i]), "", plan$entry_13[i])
            rv$entry[[14]] <- ifelse(is.na(plan$entry_14[i]), "", plan$entry_14[i])
            rv$entry[[15]] <- ifelse(is.na(plan$entry_15[i]), "", plan$entry_15[i])
            rv$same_row_different_row <- plan$same_row_different_row[i]
            Analysis_results <- suppressWarnings(try(eval(parse(text = paste0("function." ,plan$first_menu_choice[i],"(Predefined_lists, rv)"))), silent = TRUE))
            if (suppressWarnings(str_detect(Analysis_results[[1]][1], "Error"))) {
              eval(parse(text = paste0(plan$analysis_number[i], "_results <- list()")))
              eval(parse(text = paste0(plan$analysis_number[i], "_results$plots_list <- ''")))
              eval(parse(text = paste0(plan$analysis_number[i], "_results$results <-
              cbind.data.frame(
                `Analysis number` = plan$analysis_number[i],
                `Analysis type` = paste0(plan$first_menu_choice[i], ': ', plan$second_menu_choice[i]),
                `Analysis outcome` = 'Unsuccessful',
                `Reason for unsucessful analysis` = 'One or more fields in the data or plan may have been altered since the plan was created. Please create a new plan for this analysis using this programme.'
              )
                                       ")))
              plots_list[[i]] <- ""
            } else {
              eval(parse(text = paste0(plan$analysis_number[i], "_results <- Analysis_results")))
              if (plan$analysis_number[i] == "AN0001") {
                if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(AN0001_results$plots_list, paste0(AN0001_results$plots_list,"_copy")))}
              } else {
                eval(parse(text = paste0(
                  'if (TRUE %in% (', plan$analysis_number[i], '_results$plots_list != "")) {invisible(file.rename(',plan$analysis_number[i],'_results$plots_list, str_replace_all(',plan$analysis_number[i],'_results$plots_list, "/AN0001_", "/',plan$analysis_number[i],'_")))}'
                )))
              }
            }
            if (i > 1){
              eval(parse(text = paste0(
                plan$analysis_number[i], '_results$results[2,1] <- "', plan$analysis_number[i],'"'
              )))
              eval(parse(text = paste0(
                'write.table(x = ', plan$analysis_number[i], '_results$results, append = TRUE, file = paste0(rv$StorageFolder, "/results.csv"), sep = ",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")'
              )))
            } else {
              eval(parse(text = paste0(
                'write.table(x = ', plan$analysis_number[i], '_results$results, append = FALSE, file = paste0(rv$StorageFolder, "/results.csv"), sep = ",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")'
              )))
            }
            eval(parse(text = paste0(
              'if (TRUE %in% (', plan$analysis_number[i], '_results$plots_list != "")) {plots_list[[i]] <- str_replace_all(',plan$analysis_number[i],'_results$plots_list, "/AN0001_", "/',plan$analysis_number[i],'_")}'
            )))
          }
          if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(paste0(AN0001_results$plots_list,"_copy"), AN0001_results$plots_list))}
          plots_list <- do.call(c,plots_list)
          plots_list <- plots_list[plots_list != ""]
          zip(zipfile = paste0(rv$StorageFolder, "/Results.zip"), files = c(paste0(rv$StorageFolder, "/results.csv"), plots_list), mode = "cherry-pick")
          text_to_evaluate <- paste0(
            "output$htmlMessage <- renderUI(HTML('<h2 style = ",
            '"color:darkgreen; background-color:yellow"><b>The results are available from below',"')
            )",
            '
output$click_plan_upload_no_data_UI <- NULL
output$plan_upload_no_data_UI <- NULL
output$show_menu_no_data_UI <- NULL
output$Download_Results_UI <- renderUI(downloadButton("Download_Results", paste0("Download the analysis results (for the uploaded plan)" )))
output$Download_Results <- downloadHandler(filename = "Results.zip", content = function(file) {file.copy(paste0(rv$StorageFolder, "/Results.zip"), file)})
'
          )
        })
      }
    }
  }
  return(text_to_evaluate)
}
