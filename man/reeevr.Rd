% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reeevr.R
\name{reeevr}
\alias{reeevr}
\title{Convert Excel workbook to R script}
\usage{
reeevr(workbook_name, output_file)
}
\arguments{
\item{workbook_name}{filename of .xlsx workbook}

\item{output_file}{filename of .R script to store output}
}
\description{
Convert Excel workbook to R script
}
\examples{
reeevr("test_workbook.xlsx", "test_output.R")
}
