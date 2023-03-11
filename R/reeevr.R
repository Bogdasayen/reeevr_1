
require(tidyxl)
require(seqinr)

#' Convert Excel workbook to R script
#' 
#' @param workbook_name filename of .xlsx workbook
#' @param output_file filename of .R script to store output
#' @return NULL
#' @examples 
#' reeevr("test_workbook.xlsx", "test_output.R")
#' @export

reeevr <- function(workbook_name, output_file) {
  sheet_names <- xlsx_sheet_names(workbook_name)
  all_sheets <- list()
  for(sheet_name in sheet_names) {
    all_sheets[[sheet_name]] <- xlsx_cells(workbook_name, sheets = sheet_name)
    all_sheets[[sheet_name]] <- add_sheet_name_to_formulae(all_sheets[[sheet_name]])
  }
  all_sheets <- add_inverted_commas(all_sheets)
  names(all_sheets) <- sheet_names
  
  
  # Identify sheet with the input parameters
  input_sheet_name <- grep("input", sheet_names, ignore.case = TRUE, value = TRUE)
  print(paste0("The input parameters are stored in sheet ", input_sheet_name))
  input_sheet <- all_sheets[[input_sheet_name]]
  
  # Put the input parameters into a data frame
  input_parameters <- data.frame(matrix(nrow = 0, ncol = 7, dimnames = list(NULL,
                                                                            c("name", "r_variable_name", "row", "col", "value", "cell_name", "sheet_name"))))
  
  for(i_cell in 1:dim(input_sheet)[1]) {
    temp_parameter_name <- input_sheet$character[i_cell]
    if(!is.na(temp_parameter_name) & !
       is.element(temp_parameter_name, c("value", "Value", "mean", "name", "Name", "SD"))) {
      temp_list <- list()
      # Format of variable names tries to follow Tidyverse style guide
      temp_list$name <- temp_parameter_name
      temp_list$r_variable_name <- tolower(gsub(" ", "_", temp_parameter_name))
      temp_list$row = input_sheet[i_cell, c("row")]
      # Assumes the parameter value is one column to the left
      temp_list$col = input_sheet[i_cell, c("col")] + 1
      temp_list$value = input_sheet[input_sheet$row == input_sheet[i_cell, c("row")] &
                                      (input_sheet$col == input_sheet[i_cell, c("col")] + 1), "content"]
      
      temp_list$cell_name <- input_sheet[input_sheet$row == input_sheet[i_cell, c("row")] &
                                           (input_sheet$col == input_sheet[i_cell, c("col")] + 1), "address"]
      temp_list$sheet_name <- input_sheet_name
      input_parameters <- rbind(input_parameters, temp_list)
    }
  }
  
  # Identify sheet with the results
  results_sheet_name <- grep("results", sheet_names, ignore.case = TRUE)
  possible_results_sheets <- grepl("GUI", sheet_names, ignore.case = TRUE) |
    grepl("frontend", sheet_names, ignore.case = TRUE) |
    grepl("output", sheet_names, ignore.case = TRUE) |
    grepl("main", sheet_names, ignore.case = TRUE)
  results_sheet_name <- sheet_names[which(possible_results_sheets)]
  
  
  
  print(paste0("The results are stored in sheet ", results_sheet_name))
  results_sheet <- all_sheets[[results_sheet_name]]
  
  icer_label_index <- which(grepl("ICER", results_sheet$character))
  # First suppose output is a table and 'ICER' is a column name
  icer_row = results_sheet[icer_label_index, "row"] + 1
  icer_col = results_sheet[icer_label_index, "col"]
  icer_formula <- results_sheet[results_sheet$row == icer_row & 
                                  results_sheet$col == icer_col, "formula"]
  # If it's not a formula at all...
  if(is_blank(icer_formula)) {
    # Then try 'ICER' is a row name
    icer_row = results_sheet[icer_label_index, "row"]
    icer_col = results_sheet[icer_label_index, "col"] + 1
    icer_formula <- results_sheet[results_sheet$row == icer_row & 
                                    results_sheet$col == icer_col, "formula"]
  }
  # If can't find it get user input
  if(is_blank(icer_formula)) {
    print(paste0("Unable to locate ICER formula please input corect row and column on ", results_sheet_name))
    icer_row <- as.numeric(readline(prompt = "Input row number: "))
    icer_col <- as.numeric(readline(prompt = "Input column number: "))
    icer_formula <- results_sheet[results_sheet$row == icer_row & 
                                    results_sheet$col == icer_col, "formula"]
  }
  
  # Use a function to re-express in basic cells
  icer_formula <- formula_to_base(icer_formula, workbook_name)
  
  # Replace Excel cells with R input parameter names
  icer_formula_r <- icer_formula
  for(i_parameter in 1:dim(input_parameters)[1]) {
    icer_formula_r <- gsub(paste0("'", input_parameters$sheet_name[i_parameter], "'!",
                                  input_parameters$cell_name[i_parameter]),
                           input_parameters$r_variable_name[i_parameter],
                           icer_formula_r)
  }
  
  # Create an R script that defines the input parameters
  input_parameters_script <- paste0("# Input parameters from sheet ", input_sheet_name)
  
  for(i_parameter in 1:nrow(input_parameters)) {
    input_parameters_script <- paste0(input_parameters_script, "\n", input_parameters[i_parameter, "r_variable_name"],
                                      " <- ", input_parameters[i_parameter, "value"])
    
  }
  
  # Add the ICER calculation
  model_script <- paste0(input_parameters_script, "\n\n",
                         "# Calculate the ICER \n",
                         "icer <- ",
                         icer_formula_r)
  cat(model_script, file = output_file)
  
  return(NULL)
}
