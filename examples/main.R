


# Test script to read Excel model
# Howard Thom 3-Jan-2022

#devtools::install_version("tidyxl", version = "0.2.3", repos = "http://cran.us.r-project.org")
#devtools::install_github("nacnudus/tidyxl")

#library(devtools) # Make sure that the devtools library is loaded
#install_github("Bogdasayen/reeevr_1");

example_workbook_name <- "test_workbook_2.xlsx"
example_output_file <- "test_output.R"


reeevr(workbook_name = "test_workbook_2.xlsx", "test_output.R")


# Form a simple summary of named cells
named_cells <- xlsx_names("test_workbook_2.xlsx")
n_named_cells <- dim(named_cells)[1]
named_cells_summary <- matrix(nrow = n_named_cells, ncol = 2)
colnames(named_cells_summary) <- c("name", "location")
for(i_named_cell in 1:n_named_cells) {
  # Probably not good to coerce into matrix of strings
  named_cells_summary[i_named_cell, ] <- as.matrix(named_cells[i_named_cell, c("name", "formula")])
}
