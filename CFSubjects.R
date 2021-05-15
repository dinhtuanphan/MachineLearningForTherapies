library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

# Function to read all the data in excel sheets
readExcel <- function(filename, tibble = FALSE) {
  sheets <<- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  lapply(x, function(x)
    x[, colSums(is.na(x)) == 0])
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Read data in all sheets
cf <- readExcel("20210514_updated_CF_subjects_iontophoresis.xlsx")

# Basic exploratory data analysis (EDA) and visualization

# Plotting function

myplot <- function(i) {
  tryCatch(
    expr = {
      p <-
        ggplot(cf[[i]],
               aes(x = `Time (min)...8`, y = `5-minute moving avg (mM)...11`)) +
        geom_point() +
        geom_smooth() +
        xlim(0, 40) +
        ylim(0, 120)
      # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
      p + labs (x = "Time (min)", y = "C (mM)")
    },
    error = function(e) {
      message('Caught an error!')
      print(e)
      return(NA)
    },
    warning = function(w) {
      message('Caught an warning!')
      print(w)
      return(NA)
    },
    finally = {
      message('All done, quitting.')
    }
  )
}


# Create list of plots
plist <- lapply(1:10, myplot)

# Plot all together with cowplot
cowplot::plot_grid(
  plotlist = plist,
  align = "hv",
  labels = sheets[1:length(cf)],
  label_size = 10,
  label_x = 0.4,
  label_y = 0.95
)
