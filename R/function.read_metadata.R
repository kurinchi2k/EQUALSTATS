function.read_metadata <- function(rv, metadata_file_path) {
  data <- rv$import_data$data
  if (nrow(data) > 0) {
    metadata <- read.csv(metadata_file_path, check.names = FALSE, na.strings = c(""," ","  ", "NA"))
    if ((ncol(metadata) == ncol(data)) & (!FALSE %in% (colnames(metadata) == colnames(data)))) {
      if (nrow(metadata) >= 1) {
        if (!FALSE %in% (metadata[1,] %in% c("binary", "nominal", "ordinal", "counts", "quantitative", "date", "time"))) {
          # Check whether the counts and quantitative are numeric data and convert data types to factors
          checks_conversions <- lapply(1:ncol(data), function(y) {
            if (length(data[!is.na(data[,y]), y]) == 0) {
              data_check <- "failed"
              message <- "There was no data in this field. Therefore, data classification for this method could not be verified."
              column_data <- data[,y]
              final_data_type <- "unknown"
            } else {
              data_type <- metadata[1,y]
              if ((data_type == "counts") | (data_type == "quantitative")) {
                if (is.numeric(data[,y])) {
                  data_check <- "passed"
                  message <- "Successfully modified as per metadata"
                  column_data <- data[,y]
                  final_data_type <- data_type
                } else {
                  data_check <- "failed"
                  message <- paste0("This column was classified as ", data_type, ". However, there were non-numerical data in this column. Therefore, this column was classified as a categorical variable. Default methods were used for reference category.")
                  column_data <- data[,y]
                  if (colnames(data)[y] %in% rv$import_data$binary) {
                    final_data_type <- "binary"
                  } else if (colnames(data)[y] %in% rv$import_data$ordinal) {
                    final_data_type <- "ordinal"
                  } else {
                    final_data_type <- "nominal"
                  }
                }
              } else if ((data_type == "binary") | (data_type == "nominal") | (data_type == "ordinal")) {
                if (nrow(metadata) >=2) {
                  factor_levels <- unique(metadata[2:nrow(metadata), y])
                  factor_levels <- factor_levels[! is.na(factor_levels)]
                  if (TRUE %in% is.na(match(levels(as.factor(data[,y])), factor_levels))) {
                    data_check <- "failed"
                    message <- "One or more categories in the data were not present in the metadata provided. Therefore, default methods were used for reference category"
                    column_data <- data[,y]
                    if (colnames(data)[y] %in% rv$import_data$binary) {
                      final_data_type <- "binary"
                    } else if (colnames(data)[y] %in% rv$import_data$ordinal) {
                      final_data_type <- "ordinal"
                    } else {
                      final_data_type <- "nominal"
                    }
                  } else {
                    data_check <- "passed"
                    message <- "Successfully modified as per metadata"
                    column_data <- factor(as.character(data[,y]), levels = factor_levels, ordered = (data_type == "ordinal"))
                    final_data_type <- data_type
                  }
                } else {
                  data_check <- "failed"
                  message <- "The categories were not present in the metadata provided. Therefore, default methods were used for reference category"
                  column_data <- data[,y]
                  if (colnames(data)[y] %in% rv$import_data$binary) {
                    final_data_type <- "binary"
                  } else if (colnames(data)[y] %in% rv$import_data$ordinal) {
                    final_data_type <- "ordinal"
                  } else {
                    final_data_type <- "nominal"
                  }
                }
              } else if (data_type == "date") {
                character_field <- as.character(data[,y])
                character_field <- character_field[! is.na(character_field)]
                if (length(character_field) > 0) {
                  # Number of characters must be 10 in a date field
                  characters_count <- nchar(character_field)
                  if (length(unique(characters_count)) > 1) {
                    field_type <- "non-date character field"
                    converted_field <- NA
                  } else if (unique(characters_count) != 10) {
                    field_type <- "non-date character field"
                    converted_field <- NA
                  } else {
                    # If forward slash is present, it must be exactly two
                    forward_slash_count <- str_count(character_field, "/")
                    if (length(unique(forward_slash_count)) > 1) {
                      field_type <- "non-date character field"
                      converted_field <- NA
                    } else if (unique(forward_slash_count) != 2) {
                      # If hyphen is present, it must be exactly two
                      hyphen_count <- str_count(character_field, "-")
                      if (length(unique(hyphen_count)) > 1) {
                        field_type <- "non-date character field"
                        converted_field <- NA
                      } else if (unique(hyphen_count) != 2) {
                        # If dot is present, it must be exactly two
                        dot_count <- str_count(character_field, "\\.")
                        if (length(unique(dot_count)) > 1) {
                          field_type <- "non-date character field"
                          converted_field <- NA
                        } else if (unique(dot_count) != 2) {
                          field_type <- "non-date character field"
                          converted_field <- NA
                        } else {
                          field_split <- do.call(rbind.data.frame, str_split(character_field,"\\."))
                          colnames(field_split) <- paste0("X",1:3)
                          # Number of characters
                          nchar1 <- nchar(field_split$X1)
                          nchar2 <- nchar(field_split$X2)
                          nchar3 <- nchar(field_split$X3)
                          unique_nchar1 <- unique(nchar1)
                          unique_nchar2 <- unique(nchar2)
                          unique_nchar3 <- unique(nchar3)
                          if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                            field_type <- "non-date character field"
                            converted_field <- NA
                          } else {
                            combination <- paste0(unique_nchar1, unique_nchar2, unique_nchar3)
                            if (combination == "224") {
                              # Check whether the first field or second field is month
                              converted_field <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X2, "-", field_split$X1), tz = "GMT"), silent = TRUE)
                              if (str_detect(converted_field[[1]][1], "Error")) {
                                converted_field <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X1, "-", field_split$X2), tz = "GMT"), silent = TRUE)
                              }
                              if (str_detect(converted_field[[1]][1], "Error")) {
                                field_type <- "non-date character field"
                                converted_field <- NA
                              } else {
                                field_type <- "date"
                              }
                            } else if (combination == "422") {
                              converted_field <- try(as.POSIXct(paste0(field_split$X1, "-", field_split$X2, "-", field_split$X3), tz = "GMT"), silent = TRUE)
                              if (str_detect(converted_field[[1]][1], "Error")) {
                                field_type <- "non-date character field"
                                converted_field <- NA
                              } else {
                                field_type <- "date"
                              }
                            } else {
                              field_type <- "non-date character field"
                              converted_field <- NA
                            }
                          }
                        }
                      } else {
                        field_split <- do.call(rbind.data.frame, str_split(character_field,"-"))
                        colnames(field_split) <- paste0("X",1:3)
                        # Number of characters
                        nchar1 <- nchar(field_split$X1)
                        nchar2 <- nchar(field_split$X2)
                        nchar3 <- nchar(field_split$X3)
                        unique_nchar1 <- unique(nchar1)
                        unique_nchar2 <- unique(nchar2)
                        unique_nchar3 <- unique(nchar3)
                        if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                          field_type <- "non-date character field"
                          converted_field <- NA
                        } else {
                          combination <- paste0(unique_nchar1, unique_nchar2, unique_nchar3)
                          if (combination == "224") {
                            # Check whether the first field or second field is month
                            converted_field <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X2, "-", field_split$X1), tz = "GMT"), silent = TRUE)
                            if (str_detect(converted_field[[1]][1], "Error")) {
                              converted_field <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X1, "-", field_split$X2), tz = "GMT"), silent = TRUE)
                            }
                            if (str_detect(converted_field[[1]][1], "Error")) {
                              field_type <- "non-date character field"
                              converted_field <- NA
                            } else {
                              field_type <- "date"
                            }
                          } else if (combination == "422") {
                            converted_field <- try(as.POSIXct(paste0(field_split$X1, "-", field_split$X2, "-", field_split$X3), tz = "GMT"), silent = TRUE)
                            if (str_detect(converted_field[[1]][1], "Error")) {
                              field_type <- "non-date character field"
                              converted_field <- NA
                            } else {
                              field_type <- "date"
                            }
                          } else {
                            field_type <- "non-date character field"
                            converted_field <- NA
                          }
                        }
                      }
                    } else {
                      field_split <- do.call(rbind.data.frame, str_split(character_field,"/"))
                      colnames(field_split) <- paste0("X",1:3)
                      # Number of characters
                      nchar1 <- nchar(field_split$X1)
                      nchar2 <- nchar(field_split$X2)
                      nchar3 <- nchar(field_split$X3)
                      unique_nchar1 <- unique(nchar1)
                      unique_nchar2 <- unique(nchar2)
                      unique_nchar3 <- unique(nchar3)
                      if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                        field_type <- "non-date character field"
                        converted_field <- NA
                      } else {
                        combination <- paste0(unique_nchar1, unique_nchar2, unique_nchar3)
                        if (combination == "224") {
                          # Check whether the first field or second field is month
                          converted_field <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X2, "-", field_split$X1), tz = "GMT"), silent = TRUE)
                          if (str_detect(converted_field[[1]][1], "Error")) {
                            converted_field <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X1, "-", field_split$X2), tz = "GMT"), silent = TRUE)
                          }
                          if (str_detect(converted_field[[1]][1], "Error")) {
                            field_type <- "non-date character field"
                            converted_field <- NA
                          } else {
                            field_type <- "date"
                          }
                        } else if (combination == "422") {
                          converted_field <- try(as.POSIXct(paste0(field_split$X1, "-", field_split$X2, "-", field_split$X3), tz = "GMT"), silent = TRUE)
                          if (str_detect(converted_field[[1]][1], "Error")) {
                            field_type <- "non-date character field"
                            converted_field <- NA
                          } else {
                            field_type <- "date"
                          }
                        } else {
                          field_type <- "non-date character field"
                          converted_field <- NA
                        }
                      }
                    }
                  }
                } else {
                  field_type <- "Unknown"
                  converted_field <- NA
                }
                if (field_type == "date") {
                  data_check <- "passed"
                  message <- "Successfully modified as per metadata"
                  column_data <- data[,y]
                  final_data_type <- data_type
                } else {
                  data_check <- "failed"
                  message <- paste0("This column was classified as ", data_type, ". However, this field was not recognised as date. Therefore, this column was classified as a categorical variable. Default methods were used for reference category.")
                  column_data <- data[,y]
                  if (colnames(data)[y] %in% rv$import_data$binary) {
                    final_data_type <- "binary"
                  } else if (colnames(data)[y] %in% rv$import_data$ordinal) {
                    final_data_type <- "ordinal"
                  } else {
                    final_data_type <- "nominal"
                  }
                }
              } else if (data_type == "time") {
                if (is.numeric(data[,y])) {
                  time <- FALSE
                } else {
                  character <- as.character(data[,y])
                  character <- character[! is.na(character)]
                  if (length(character) > 0) {
                    characters_count <- nchar(character)
                    if (length(unique(characters_count)) > 1) {
                      time <- FALSE
                    } else if (unique(characters_count) != 8) {
                      time <- FALSE
                    } else {
                      colon_count <- str_count(character, ":")
                      if (length(unique(colon_count)) > 1) {
                        time <- FALSE
                      } else if (unique(colon_count) != 2) {
                        time <- FALSE
                      } else {
                        field_split <- do.call(rbind.data.frame, str_split(character,":"))
                        colnames(field_split) <- paste0("X",1:3)
                        # Number of characters
                        nchar1 <- nchar(field_split$X1)
                        nchar2 <- nchar(field_split$X2)
                        nchar3 <- nchar(field_split$X3)
                        unique_nchar1 <- unique(nchar1)
                        unique_nchar2 <- unique(nchar2)
                        unique_nchar3 <- unique(nchar3)
                        if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                          time <- FALSE
                        } else if ((unique_nchar1 != 2) | (unique_nchar2 != 2) | (unique_nchar1 != 2)) {
                          time <- FALSE
                        } else if ((TRUE %in% (field_split$X1 > 23)) | (TRUE %in% (field_split$X2 > 59)) | (TRUE %in% (field_split$X1 > 59))) {
                          time <- FALSE
                        } else {
                          time <- TRUE
                        }
                      }
                    }
                  }
                }
                if (time == TRUE) {
                  data_check <- "passed"
                  message <- "Successfully modified as per metadata"
                  column_data <- data[,y]
                  final_data_type <- data_type
                } else {
                  data_check <- "failed"
                  message <- paste0("This column was classified as ", data_type, ". However, this field was not recognised as time. Therefore, this column was classified as a categorical variable. Default methods were used for reference category.")
                  column_data <- data[,y]
                  if (colnames(data)[y] %in% rv$import_data$binary) {
                    final_data_type <- "binary"
                  } else if (colnames(data)[y] %in% rv$import_data$ordinal) {
                    final_data_type <- "ordinal"
                  } else {
                    final_data_type <- "nominal"
                  }
                }
              }
            }
            output <- list(column_data, data_check, message, final_data_type)
            return(output)
          })
          names(checks_conversions) <- colnames(data)
          data <- do.call(cbind.data.frame, lapply(checks_conversions,'[[',1))
          message <- paste0(colnames(data), ": ", do.call(c, lapply(checks_conversions,'[[',3)), collapse = "\n")
          final_data_type <- do.call(c, lapply(checks_conversions,'[[',4))
          binary <- c("",colnames(data)[final_data_type == "binary"])
          nominal <- c("",colnames(data)[final_data_type == "nominal"])
          ordinal <- c("",colnames(data)[final_data_type == "ordinal"])
          categorical <- unique(c(binary, nominal, ordinal))
          counts <- c("",colnames(data)[final_data_type == "counts"])
          quantitative <- c(counts,colnames(data)[final_data_type == "quantitative"])
          date <- c("",colnames(data)[final_data_type == "date"])
          time <- c("",colnames(data)[final_data_type == "time"])
          any_type <- c("", colnames(data))
          output <- list(outcome = "Successful", message = message,
                         data = data, any_type = any_type,
                         quantitative = quantitative, counts = counts,
                         categorical = categorical, nominal = nominal, binary = binary, ordinal = ordinal,
                         date = date, time = time)
        } else {
          output <- list(outcome = "Unsuccessful", message = 'The second row in the metadata file should only contain one of the following types: "binary", "nominal", "ordinal", "counts", or "quantitative". Please upload a file with correct data types in the metadata file or continue with the default classifications by EQUAL-STATS')
        }
      } else {
        output <- list(outcome = "Unsuccessful", message = 'The metadata file did not contain sufficient information. The minimum information required are the data types of each variable. Please upload a file with information in the metadata file or continue with the default classifications by EQUAL-STATS')
      }
    } else {
      output <- list(outcome = "Unsuccessful", message = 'The column names in the metadata file did not match with the column names in the data. Please upload a file with the metadata file matching the column names of the data or continue with the default classifications by EQUAL-STATS')
    }
  } else {
    output <- list(outcome = "Unsuccessful", message = "There was no data. First upload data file and then the meta data file")
  }
}
