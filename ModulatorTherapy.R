library(readxl)
library(ggplot2)
readExcel <- function(filename, tibble = FALSE){
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

modulator <- readExcel("20210513_updated_healthy_iontophoresis.xlsx")
