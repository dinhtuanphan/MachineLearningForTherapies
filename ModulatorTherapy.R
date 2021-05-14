library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

# Function to read all the data in excel sheets
readExcel <- function(filename, tibble = FALSE){
  sheets <<- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  lapply(x, function(x) x[, colSums(is.na(x)) == 0])
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Read data in all sheets
modulator <- readExcel("20210513_updated_healthy_iontophoresis.xlsx")

# Basic exploratory data analysis (EDA) and visualization

# Plotting function
myplot <- function(i){
  p <- ggplot(modulator[[i]], aes(x=SR, y = C_predose_smooth_2min) ) + 
    geom_point() +
    geom_smooth(method = lm) +
    xlim(0, 0.8) +
    ylim(0, 60)
  p + labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
}

# Create list of plots
plist <- lapply(1:25, myplot)

# Plot all together with cowplot
cowplot::plot_grid(plotlist = plist, align = "hv",labels = sheets[1:25]
                   , label_size = 10
                   , label_x = 0.4, label_y = 0.95
                  )

