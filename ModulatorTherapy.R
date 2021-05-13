library(readxl)
library(dplyr)
library(ggplot2)

# Function to read all the data in excel sheets
readExcel <- function(filename, tibble = FALSE){
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  lapply(x, function(x) x[, colSums(is.na(x)) == 0])
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Read data in all sheets
modulator <- readExcel("20210513_updated_healthy_iontophoresis.xlsx")

# Basic exploratory data analysis (EDA) and visualization

p <- ggplot(modulator[[1]], aes(x=SR, y = C_predose_smooth_2min) ) + 
    geom_point(size=2, shape=19)
