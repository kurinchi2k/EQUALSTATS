convert_to_user_interface <- function(UI_name, label, entry_text, rv) {
  same_row_different_row = rv$same_row_different_row
  if (substr(label, 1, 6) == "ifelse") {
    label <- eval(parse(text = label))
  }
  if(length(label) == 0) {label <- "NULL"}
  if (label == "NULL") {label <- NULL}
  input_type <- str_split(entry_text,"%_%")[[1]][2]
  input_parameters <- str_split(entry_text,"%_%")[[1]][3]
  if (substr(input_parameters, 1, 6) == "ifelse") {
    input_parameters <- eval(parse(text = str_replace_all(input_parameters, '\"\"', "''")))
  }
  if(length(input_parameters) == 0) {input_parameters <- "NULL"}
  if (input_parameters == "NULL") {input_parameters <- NULL}
  if ((is.null(label)) | (is.null(input_parameters))) {
    output_1 <- paste0('output$', UI_name, ' <- NULL')
    output_2 <- paste0('output$', UI_name, ' <- NULL')
  } else {
    output_1 <- paste0('output$', UI_name, ' <- renderUI(',
                       if (input_type == "numeric") {
                         paste0('numericInput("', substr(UI_name,1,nchar(UI_name)-3),'",',
                                '"', label, '", ',
                                'min = ', suppressWarnings(as.numeric(str_split(entry_text,"%_%")[[1]][3])), ', ',
                                'max = ', suppressWarnings(as.numeric(str_split(entry_text,"%_%")[[1]][4])), ', ',
                                'value = ', suppressWarnings(as.numeric(str_split(entry_text,"%_%")[[1]][5])),
                                ', width = "70%"',
                                '))'
                         )
                       } else if (input_type == "slider") {
                         paste0('sliderInput("', substr(UI_name,1,nchar(UI_name)-3),'",',
                                '"', label, '", ',
                                'min = ', as.numeric(str_split(entry_text,"%_%")[[1]][3]), ', ',
                                'max = ', as.numeric(str_split(entry_text,"%_%")[[1]][4]), ', ',
                                'value = ', as.numeric(str_split(entry_text,"%_%")[[1]][5]),
                                ', width = "70%"',
                                '))'
                         )
                       } else if (input_type == "checkbox") {
                         paste0('checkboxGroupInput("', substr(UI_name,1,nchar(UI_name)-3),'",',
                                '"', label, '", ',
                                'selected = character(0), inline = TRUE,',
                                'choices = ', input_parameters,
                                ', width = "70%"',
                                '))'
                         )
                       } else if (input_type == "selectInput") {
                         paste0('selectInput("', substr(UI_name,1,nchar(UI_name)-3),'",',
                                '"', label, '", ',
                                'selected = "", ',
                                'choices = ', input_parameters,
                                ', width = "70%"',
                                '))'
                         )
                       } else if (input_type == "text") {
                         paste0('textInput("', substr(UI_name,1,nchar(UI_name)-3),'",',
                                '"', label, '", ',
                                'value = "", ',
                                'placeholder = ', text = input_parameters,
                                ', width = "70%"',
                                '))'
                         )
                       }
    )
    output_2 <-
      if (input_type == "numeric") {
        paste0('updateNumericInput(inputId = "', substr(UI_name,1,nchar(UI_name)-3),'",',
               'value = rv$entry[[',str_remove(substr(UI_name,1,nchar(UI_name)-3),'entry_'),']]
               )'
        )
      } else if (input_type == "slider") {
        paste0('updateSliderInput(inputId = "', substr(UI_name,1,nchar(UI_name)-3),'",',
               'value = rv$entry[[',str_remove(substr(UI_name,1,nchar(UI_name)-3),'entry_'),']]
               )'
        )
      } else if (input_type == "checkbox") {
        paste0('updateCheckboxGroupInput(inputId = "', substr(UI_name,1,nchar(UI_name)-3),'",',
               'selected = rv$entry[[',str_remove(substr(UI_name,1,nchar(UI_name)-3),'entry_'),']]
               )'
        )
      } else if (input_type == "selectInput") {
        paste0('updateSelectInput(inputId = "', substr(UI_name,1,nchar(UI_name)-3),'",',
               'selected = rv$entry[[',str_remove(substr(UI_name,1,nchar(UI_name)-3),'entry_'),']]
               )'
        )
      } else if (input_type == "text") {
        paste0('updateTextInput(inputId = "', substr(UI_name,1,nchar(UI_name)-3),'",',
               'value = rv$entry[[',str_remove(substr(UI_name,1,nchar(UI_name)-3),'entry_'),']]
               )'
        )
      }
  }
  output <- list(output_1, output_2)
  return(output)
}
