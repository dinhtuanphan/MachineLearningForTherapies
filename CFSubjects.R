library(readxl)
library(tidyverse)
library(cowplot)

# Function to read all the data in excel sheets
readExcel <- function(filename, tibble = FALSE) {
  sheets <<- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  x <- lapply(x, function(x)
    x[,!apply(is.na(x), 2, all)])
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Read data in all sheets
cf <- readExcel("20210517_updated_CF_subjects_iontophoresis.xlsx")
cf <- cf[-c(46, 47, 48)]

# Subsetting data into two groups
# homogeneous F508del/F508del
homo <- subset(cf, sapply(cf, function(x) x$Genotype[1] =='F508del/F508del'))

# heterogeneous F508del/G551D
hete <- subset(cf, sapply(cf, function(x) x$Genotype[1] =='F508del/G551D'))
names(homo)
names(hete)

# Basic exploratory data analysis (EDA) and visualization

# Plotting function

myplot <- function(i) {
  tryCatch(
    expr = {
      p <-
        ggplot(cf[[i]],
               aes(x = `SR...28`, y = `C_postdose_smooth_2min`)) +
        geom_point() +
        geom_smooth(method = "lm",
                    formula = "y~x",
                    se = TRUE) +
        xlim(0, 1) +
        ylim(0, 120) +
        # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
        # xlab("Time (min)") +
        # ylab("C (mM)")
        xlab(expression(Sweat ~ rate ~ (mu * "L" ~ min ^ -1 ~ cm ^ -2))) +
        ylab("C (mM)")
      p + ggtitle(sheets[i]) + theme(plot.title = element_text(hjust = 0.5))
    },
    error = function(e) {
      message('Caught an error!')
      print(e)
      return(NULL)
    },
    warning = function(w) {
      message('Caught a warning!')
      print(w)
      return(NULL)
    }
  )
}

# Create list of plots
plist <- lapply(1:length(cf), myplot)


# Open an image file
png(
  file = "saving_plot.png",
  width = 17,
  height = 11,
  units = 'in',
  res = 600
)

# Plot all together with cowplot
cowplot::plot_grid(plotlist = plist,
                   align = "hv")

# Close the image file
dev.off()


