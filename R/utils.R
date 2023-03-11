

require(tidyxl)
require(seqinr)


#' Add !prefix to any formulae without a sheet !prefix
#' 
#' @param workbook_name input_Sheet 
#' @param input_sheet a data.frame exported by tidyxl::xlsx_cells
#' @return a data.frame in form exported by tidyxl::xlsx_cells
#' @examples 
#' temp_input <- xlsx_cells("test_workbook.xlsx", sheets = sheet_name)
#' temp_output <- add_sheet_name_to_formulae(temp_input)
#' @export
add_sheet_name_to_formulae <- function(input_sheet) {
  # Find any formulae
  formulae_index <- which(!is.na(input_sheet$formula))
  for(formula_index in formulae_index) {
    # Loop through non-empty cells and replace where found
    temp_formula <- input_sheet$formula[formula_index]
    for(i_cell in 1:dim(input_sheet)[1]) {
      # Temporarily replace other !prefix sheets
      cell_name <- input_sheet$address[i_cell]
      temp_formula <- gsub(paste0("!", cell_name), "99-99-99", temp_formula)
      temp_formula <- gsub(cell_name, paste0("'", input_sheet$sheet[i_cell], "'!", cell_name), temp_formula)
      temp_formula <- gsub("99-99-99", paste0("!", cell_name), temp_formula)
    }
    input_sheet$formula[formula_index] <- temp_formula
  }
  return(input_sheet)
}

#' Add inverted commas ' around any sheet name in Excel workbook for consistency
#' 
#' @param all_sheets list of data.frames exported by tidyxl::xlsx_cells 
#' @return list of data.frames in format exported by tidyxl::xlsx_cells
#' @examples 
#' temp_output_list <- add_inverted_commas(temp_input_list)
#' @export
add_inverted_commas <- function(all_sheets) {
  # Extract the sheet names in case not set correctly
  sheet_names <- c()
  for(i_sheet in 1:length(all_sheets)) {
    sheet_names <- c(sheet_names, unique(all_sheets[[i_sheet]]$sheet))
  }
  for(i_sheet in 1:length(all_sheets)) {
    # Find any formulae
    formulae_index <- which(!is.na(all_sheets[[i_sheet]]$formula))
    for(formula_index in formulae_index) {
      temp_formula <- all_sheets[[i_sheet]]$formula[formula_index]
      for(sheet_name in sheet_names) {
        # First replace those instances that already have commas
        temp_formula <- gsub(paste0("'", sheet_name, "'"),
                             "99-99-99", temp_formula)
        # Replace those that do not
        temp_formula <- gsub(sheet_name, 
                             paste0("'", sheet_name, "'"),
                             temp_formula)
        # Re-insert those already with commas
        temp_formula <- gsub("99-99-99",
                             paste0("'", sheet_name, "'"),
                             temp_formula)
      } # End inner loop over sheet_names
      all_sheets[[i_sheet]]$formula[formula_index] <- temp_formula
    }
  }
  return(all_sheets)  
}


#' Convert a cell formula into expression of single cells rather than formulae
#' 
#' @param cell_formula string with the cell formula
#' @param workbook_name filename with location of workbook
#' @return string with modified cell formula
#' @examples 
#' temp_output <- formula_to_base(cell_formula = "Engine!G5/Engine!G6", workbook_name = "test_workbook.xlsx")
#' @export
formula_to_base <- function(cell_formula = "Engine!G5/Engine!G6", workbook_name) {
  sheet_names <- xlsx_sheet_names(workbook_name)
  all_sheets <- list()
  for(sheet_name in sheet_names) {
    all_sheets[[sheet_name]] <- xlsx_cells(workbook_name, sheets = sheet_name)
    all_sheets[[sheet_name]] <- add_sheet_name_to_formulae(all_sheets[[sheet_name]])
  }
  all_sheets <- add_inverted_commas(all_sheets)
  names(all_sheets) <- sheet_names
  
  no_replacements <- FALSE
  # Check if any term is a formula 
  # If yes re-express the first and start loop again
  while(!no_replacements) {
    no_replacements <- TRUE  
    for(i_sheet in 1:length(all_sheets)) {
      # Only need to check the formulae cells
      if(sum(!is.na(all_sheets[[i_sheet]]$formula)) > 0) {
        for(i_cell in which(!is.na(all_sheets[[i_sheet]]$formula))) {
          new_formula <- gsub(paste0("'", all_sheets[[i_sheet]]$sheet[i_cell], "'!", all_sheets[[i_sheet]]$address[i_cell]),
                              paste0("(", all_sheets[[i_sheet]]$formula[i_cell], ")"), 
                              cell_formula)
          # Flag that a replacement was made if new formula is different
          no_replacements <- no_replacements & new_formula == cell_formula
          cell_formula <- new_formula
        } # End for loop over i_cell
      }
    }# End for loop over i_sheet
    
  } # End while
  
  return(cell_formula)
}

