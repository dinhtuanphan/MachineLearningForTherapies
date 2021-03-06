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
modulator <- readExcel("20210514_updated_healthy_iontophoresis.xlsx")

# Basic exploratory data analysis (EDA) and visualization

# Plotting function
myplot <- function(i){
  p <- ggplot(modulator[[i]], aes(x=`Time (min)...8`, y = `5-minute moving avg (mM)`) ) + 
    geom_point() +
    geom_smooth() +
    xlim(0, 40) +
    ylim(0, 60)
  # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
  p + labs (x = "Time (min)", y = "C (mM)")
}

# Create list of plots
plist <- lapply(1:length(modulator), myplot)

# Plot all together with cowplot
cowplot::plot_grid(plotlist = plist, align = "hv",labels = sheets[1:length(modulator)]
                   , label_size = 10
                   , label_x = 0.4, label_y = 0.95
                  )

