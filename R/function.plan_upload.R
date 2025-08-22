function.plan_upload <- function(plan_file_path, Predefined_lists, rv){
  plan <- read.csv(plan_file_path, check.names = FALSE, na.strings = c(""," ","  ", "NA"))
  # First find whether the column names match
  if (FALSE %in% (colnames(plan) == c("analysis_number", "first_menu_choice", "second_menu_choice", "entry_1", "entry_2", "entry_3", "entry_4", "entry_5", "entry_6", "entry_7", "entry_8", "entry_9", "entry_10", "entry_11", "entry_12", "entry_13", "entry_14", "entry_15", "same_row_different_row"))) {
    text_to_evaluate <- paste0(
      "output$htmlMessage <- renderUI(HTML('<h2 style = ",
      '"color:darkred; background-color:yellow"><b>The column names in the uploaded plan does not match with the column names of a plan prepared using this program. Please upload a plan prepared using this program or click on "Start data analysis" to create a plan', "'))"
    )
  } else {
    # Find whether the first menu choices match with those of the program
    if (TRUE %in% is.na(match(plan$first_menu_choice, Predefined_lists$menu_short))) {
      text_to_evaluate <- paste0(
        "output$htmlMessage <- renderUI(HTML('<h2 style = ",
        '"color:darkred; background-color:yellow"><b>The uploaded plan does not match with the functions for this program. Please upload a plan prepared using this program or click on "Start data analysis" to create a plan', "'))"
      )
    } else {
      # Find whether the second menu choices match with the corresponding first menu choices
      second_menu_choice_passed_check <- sapply(1:nrow(plan), function(x) {
        second_menu_choice <- str_split(Predefined_lists$second_menu_choices[Predefined_lists$menu_short == plan$first_menu_choice[x]], "%__%")
        if (length(second_menu_choice) == 0) {
          outcome = "passed check"
        } else if (second_menu_choice == "") {
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
          '"color:darkred; background-color:yellow"><b>The uploaded plan does not match with the functions for this program. Please upload a plan prepared using this program or click on "Start data analysis" to create a plan', "'))"
        )
      } else {
        # Check whether the entries match the fields in the uploaded data
        # Numeric fields have to be excluded in this check
        entries_match_data_fields_check <- sapply(1:nrow(plan), function(x) {
          entries <- unlist(plan[x,  colnames(plan)[substr(colnames(plan),1,5) == "entry"]])
          numeric_exemptions <- str_split(Predefined_lists$numeric_exemptions[Predefined_lists$menu_short == plan$first_menu_choice[x]], "%__%")
          if (length(numeric_exemptions) == 0) {
            numeric_exemptions_logical <- rep(TRUE, 15)
          } else if (numeric_exemptions != "") {
            second_menu_choice <- str_split(Predefined_lists$second_menu_choices[Predefined_lists$menu_short == plan$first_menu_choice[x]], "%__%")
            if (second_menu_choice == "") {
              numeric_exemptions_information <- numeric_exemptions[[1]]
            } else {
              # Find the second menu choice position
              second_menu_choice_position <- match(plan$second_menu_choice[x], second_menu_choice[[1]])
              numeric_exemptions_information <- numeric_exemptions[[1]][second_menu_choice_position]
            }
            numeric_exemptions_information <- str_split(numeric_exemptions_information, "%_%")[[1]]
            numeric_exemptions_logical <- (numeric_exemptions_information == "no")
            numeric_exemptions_logical <- c(numeric_exemptions_logical, rep(TRUE,(15 - length(numeric_exemptions_logical))))
          } else {
            numeric_exemptions_logical <- rep(TRUE, 15)
          }
          entries <- entries[numeric_exemptions_logical]
          entries <- entries[entries != "EQUAL-STATS choice"]
          entries <- entries[!is.na(entries)]
          entries <- sapply(1:length(entries), function(z) {
            str_split(entries[z],"%_%")
          })
          entries <- do.call(c, entries)
          if (TRUE %in% is.na(match(entries, colnames(rv$import_data$data)))) {
            outcome = "failed check"
          } else {
            outcome = "passed check"
          }
          return(outcome)
        })
        if (TRUE %in% (entries_match_data_fields_check == "failed check")) {
          text_to_evaluate <- paste0(
            "output$htmlMessage <- renderUI(HTML('<h2 style = ",
            '"color:darkred; background-color:yellow"><b>The uploaded plan does not match with the column names of the uploaded data. Please upload a plan prepared for this data using this program or click on "Start data analysis" to create a plan', "'))"
          )
        } else {
          # Perform the analysis
          plots_list <- list()
          withProgress(message = 'Performing analysis...', value = 0, {
            for (i in 1:nrow(plan)){
              incProgress(1/nrow(plan), detail = paste0(i, " of ", nrow(plan), " analyses" ))
              rv$first_menu_choice <- plan$first_menu_choice[i]
              rv$second_menu_choice <- plan$second_menu_choice[i]
              rv$entry[[1]] <- if (is.na(plan$entry_1[i])) {""} else {do.call(c,str_split(plan$entry_1[i],"%_%"))}
              rv$entry[[2]] <- if (is.na(plan$entry_2[i])) {""} else {do.call(c,str_split(plan$entry_2[i],"%_%"))}
              rv$entry[[3]] <- if (is.na(plan$entry_3[i])) {""} else {do.call(c,str_split(plan$entry_3[i],"%_%"))}
              rv$entry[[4]] <- if (is.na(plan$entry_4[i])) {""} else {do.call(c,str_split(plan$entry_4[i],"%_%"))}
              rv$entry[[5]] <- if (is.na(plan$entry_5[i])) {""} else {do.call(c,str_split(plan$entry_5[i],"%_%"))}
              rv$entry[[6]] <- if (is.na(plan$entry_6[i])) {""} else {do.call(c,str_split(plan$entry_6[i],"%_%"))}
              rv$entry[[7]] <- if (is.na(plan$entry_7[i])) {""} else {do.call(c,str_split(plan$entry_7[i],"%_%"))}
              rv$entry[[8]] <- if (is.na(plan$entry_8[i])) {""} else {do.call(c,str_split(plan$entry_8[i],"%_%"))}
              rv$entry[[9]] <- if (is.na(plan$entry_9[i])) {""} else {do.call(c,str_split(plan$entry_9[i],"%_%"))}
              rv$entry[[10]] <- if (is.na(plan$entry_10[i])) {""} else {do.call(c,str_split(plan$entry_10[i],"%_%"))}
              rv$entry[[11]] <- if (is.na(plan$entry_11[i])) {""} else {do.call(c,str_split(plan$entry_11[i],"%_%"))}
              rv$entry[[12]] <- if (is.na(plan$entry_12[i])) {""} else {do.call(c,str_split(plan$entry_12[i],"%_%"))}
              rv$entry[[13]] <- if (is.na(plan$entry_13[i])) {""} else {do.call(c,str_split(plan$entry_13[i],"%_%"))}
              rv$entry[[14]] <- if (is.na(plan$entry_14[i])) {""} else {do.call(c,str_split(plan$entry_14[i],"%_%"))}
              rv$entry[[15]] <- if (is.na(plan$entry_15[i])) {""} else {do.call(c,str_split(plan$entry_15[i],"%_%"))}
              rv$same_row_different_row <- plan$same_row_different_row[i]
              Analysis_results <- suppressWarnings(try(eval(parse(text = paste0("function.", plan$first_menu_choice[i],"(Predefined_lists, rv)"))), silent = TRUE))
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
output$click_metadata_upload_UI <- NULL
output$metadata_upload_UI <- NULL
output$click_plan_upload_UI <- NULL
output$plan_upload_UI <- NULL
output$show_menu_UI <- NULL
rv$same_row_different_row <- NA
output$Download_Results_UI <- renderUI(downloadButton("Download_Results", paste0("Download the analysis results (for the uploaded plan)" )))
output$Download_Results <- downloadHandler(filename = "Results.zip", content = function(file) {file.copy(paste0(rv$StorageFolder, "/Results.zip"), file)})
'
            )
          })
        }
      }
    }
  }
  return(text_to_evaluate)
}
