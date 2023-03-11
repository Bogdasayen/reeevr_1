


# Test script to read Excel model
# Howard Thom 3-Jan-2022

#devtools::install_version("tidyxl", version = "0.2.3", repos = "http://cran.us.r-project.org")
#devtools::install_github("nacnudus/tidyxl")

#library(devtools) # Make sure that the devtools library is loaded
#install_github("Bogdasayen/reeevr_1");

example_workbook_name <- "examples/test_workbook_2.xlsx"
example_output_file <- "examples/test_output.R"


reeevr(example_workbook_name, example_output_file)
