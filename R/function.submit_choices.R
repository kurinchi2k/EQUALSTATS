function.submit_choices <- function(Predefined_lists, rv, code_prefix, no_data_choices){
  Analysis_results <- eval(parse(text = paste0("function.", rv$first_menu_choice, "(Predefined_lists, rv)")))
  results_number <- length(rv$analysis_outcome)+1
  rv$analysis_outcome[[results_number]] <- Analysis_results$analysis_outcome
  rv$code[[results_number]] <- Analysis_results$code
  rv$plan[[results_number]] <- Analysis_results$plan
  rv$results[[results_number]] <- Analysis_results$results
  rv$plots_list[[results_number]] <- Analysis_results$plots_list
  write.table(x = code_prefix, append = FALSE, file = paste0(rv$StorageFolder, "/code.R"), sep = "\t", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")
  rv$reports$code <- do.call(rbind.data.frame, rv$code)
  if (! rv$first_menu_choice %in% no_data_choices) {
    storage_text <- '
# Data input and results storage ####
  data_file_path <- choose.files("", caption = "Upload the data file", multi = FALSE, filters = c("*.csv", "*.csv"))
  rv$import_data <- function.read_data(data_file_path = data_file_path)
  metadata_file_path <- choose.files("", caption = "Upload the metadata file (if you used a metadata file while creating the statistical plan)", multi = FALSE, filters = c("*.csv", "*.csv"))
  if (length(metadata_file_path) > 0) {
    metadata_output <- function.read_metadata(rv, metadata_file_path)
    if (metadata_output$outcome == "Successful") {
      rv$import_data <- metadata_output
    }
  }
  rv$StorageFolder <- paste0(substr(data_file_path,1,str_locate_all(data_file_path, fixed("\\\\"))[[1]][nrow(str_locate_all(data_file_path, fixed("\\\\"))[[1]]),1]),"Analysis_Results_", str_replace_all(str_replace_all(str_remove_all(str_remove_all(Sys.time(), "-"), ":")," ","_"),"\\\\.","_"))
'
  } else {
    storage_text <- '# Results storage ####
  rv$StorageFolder <- paste0(path.expand("~/"),"Analysis_Results_", str_replace_all(str_replace_all(str_remove_all(str_remove_all(Sys.time(), "-"), ":")," ","_"),"\\\\.","_"))
'
  }
  storage_text <- paste0(storage_text, 'if(!file.exists(rv$StorageFolder)){
  dir.create(rv$StorageFolder)
  } else {
    unlink(rv$StorageFolder, recursive = TRUE)
    dir.create(rv$StorageFolder)
  }
# Data analysis ####
  ')
  write.table(x = storage_text, append = TRUE, file = paste0(rv$StorageFolder, "/code.R"), sep = "\t", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")
  write.table(x = rv$reports$code, append = TRUE, file = paste0(rv$StorageFolder, "/code.R"), sep = "\t", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")
  if (! rv$first_menu_choice %in% no_data_choices) {
    write.table(x = 'if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(paste0(AN0001_results$plots_list,"_copy"), AN0001_results$plots_list))}
  if (length(metadata_file_path) > 0) {
   cat(paste0(metadata_output$message, "\nThe results are available from the folder: ", rv$StorageFolder))
   } else {
   cat(paste0("No metadata file was used in the analysis", "\nThe results are available from the folder: ", rv$StorageFolder))
   }', append = TRUE, file = paste0(rv$StorageFolder, "/code.R"), sep = "\t", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")
  } else {
    write.table(x = 'if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(paste0(AN0001_results$plots_list,"_copy"), AN0001_results$plots_list))}
   cat(paste0("The results are available from the folder: ", rv$StorageFolder))', append = TRUE, file = paste0(rv$StorageFolder, "/code.R"), sep = "\t", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")
  }
  rv$reports$plan <- do.call(rbind.data.frame, rv$plan)
  write.table(x = rv$reports$plan, file = paste0(rv$StorageFolder, "/plan.csv"), sep = ",", row.names = FALSE, col.names = TRUE, na = "", quote = FALSE, qmethod = "double")
  rv$reports$results <- function.rbind_different_column_numbers(rv$results, include_columns = "no")
  write.table(x = rv$reports$results, file = paste0(rv$StorageFolder, "/results.csv"), sep = ",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")
  rv$reports$plots_list <- do.call(c,rv$plots_list)
  rv$reports$plots_list <- rv$reports$plots_list[rv$reports$plots_list != ""]
  # Create a zip folder
  zip(zipfile = paste0(rv$StorageFolder, "/Results.zip"), files = c(paste0(rv$StorageFolder, "/code.R"), paste0(rv$StorageFolder, "/plan.csv"), paste0(rv$StorageFolder, "/results.csv"), rv$reports$plots_list), mode = "cherry-pick")
  return(Analysis_results)
}
