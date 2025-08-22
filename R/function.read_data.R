function.read_data <- function(data_file_path) {
  data <- read.csv(data_file_path, check.names = FALSE, na.strings = c(""," ","  ", "NA"))
  if (nrow(data) > 0) {
    func.recognise_date <- function(data) {
      lapply(1:ncol(data), function(y) {
        if (is.numeric(data[,y])) {
          field_type <- "numeric"
          converted_field <- NA
        } else if (length(data[!is.na(data[,y]), y]) == 0) {
          field_type <- "unknown"
          converted_field <- NA
        } else {
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
        }
        output <- list(field_type = field_type, converted_field = converted_field)
        return(output)
      })
    }
    func.recognise_time <- function(data) {
      sapply(1:ncol(data), function(y) {
        if (is.numeric(data[,y])) {
          time_field <- FALSE
        } else if (length(data[!is.na(data[,y]), y]) == 0) {
          time_field <- FALSE
        } else {
          character_field <- as.character(data[,y])
          character_field <- character_field[! is.na(character_field)]
          if (length(character_field) > 0) {
            characters_count <- nchar(character_field)
            if (length(unique(characters_count)) > 1) {
              time_field <- FALSE
            } else if (unique(characters_count) != 8) {
              time_field <- FALSE
            } else {
              colon_count <- str_count(character_field, ":")
              if (length(unique(colon_count)) > 1) {
                time_field <- FALSE
              } else if (unique(colon_count) != 2) {
                time_field <- FALSE
              } else {
                field_split <- do.call(rbind.data.frame, str_split(character_field,":"))
                colnames(field_split) <- paste0("X",1:3)
                # Number of characters
                nchar1 <- nchar(field_split$X1)
                nchar2 <- nchar(field_split$X2)
                nchar3 <- nchar(field_split$X3)
                unique_nchar1 <- unique(nchar1)
                unique_nchar2 <- unique(nchar2)
                unique_nchar3 <- unique(nchar3)
                if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                  time_field <- FALSE
                } else if ((unique_nchar1 != 2) | (unique_nchar2 != 2) | (unique_nchar1 != 2)) {
                  time_field <- FALSE
                } else if ((TRUE %in% (field_split$X1 > 23)) | (TRUE %in% (field_split$X2 > 59)) | (TRUE %in% (field_split$X1 > 59))) {
                  time_field <- FALSE
                } else {
                  time_field <- TRUE
                }
              }
            }
          }
        }
      })
    }
    # Remove columns which are all NA
    not_na_length <- sapply(1:ncol(data), function(y) {length(data[!is.na(data[,y]), y])})
    data <- data[,(not_na_length != 0)]
    # Declare numerical variables - call it quantitative; declare the remaining as categorical
    any_type <- c("",colnames(data))
    quantitative <- c("",colnames(data[,unlist(lapply(data, is.numeric))]))
    categorical <- c("",colnames(data[,unlist(lapply(data, is.character))]))
    # Declare categorical variables as factors
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
    counts <- as.vector(c("",unlist(sapply(1:length(quantitative), function(x) {
      if (quantitative[x] != "") {
        if (substr(quantitative[x],1,10) == "Number of ") {
          quantitative[x]
        }
      }
    }))))
    binary <- as.vector(c("",unlist(sapply(1:length(categorical), function(x) {
      if (categorical[x] != "") {
        if (nlevels(data[,categorical[x]]) == 2){
          categorical[x]
        }
      }
    }))))
    ordinal <- as.vector(c("",unlist(sapply(1:length(categorical), function(x) {
      if (categorical[x] != "") {
        if (!FALSE %in% (substr(levels(data[,categorical[x]]),2,2) == "_")) {
          categorical[x]
        }
      }
    }))))
    nominal <- as.vector(c("", setdiff(categorical, ordinal)))
    # Date and time variables
    processed_data <- func.recognise_date(data)
    data_converted <- lapply(1:ncol(data), function(y) {
      if (processed_data[[y]][[1]] == "date") {
        column_data <- as.character(data[,y])
        column_data[! is.na(column_data)] <- processed_data[[y]][[2]]
      } else {
        column_data <- data[,y]
      }
      return(column_data)
    })
    names(data_converted) <- colnames(data)
    data_converted <- do.call(cbind.data.frame, data_converted)
    date <- c("", colnames(data_converted)[sapply(1:ncol(data), function(y) {processed_data[[y]][[1]] == "date"})])
    time <- c("", colnames(data_converted)[func.recognise_time(data_converted)])
    output <- list(outcome = "Successful", message = "Working on uploaded data",
                   data = data, any_type = any_type,
                   quantitative = quantitative, counts = counts,
                   categorical = categorical, nominal = nominal, binary = binary, ordinal = ordinal,
                   date = date, time = time)
  } else {
    output <- list("Unsuccessful","The uploaded file did not have any data. Please upload a file with data")
  }
}
