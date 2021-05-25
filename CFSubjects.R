library(readxl)
library(tidyverse)
library(cowplot)
library(ggplot2)

# Function to read all the data in excel sheets
readExcel <- function(filename, tibble = FALSE) {
  sheets <<- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  x <- lapply(x, function(x)
    x[, !apply(is.na(x), 2, all)])
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Read data in all sheets
cf <- readExcel("20210518_updated_CF_subjects_iontophoresis.xlsx")
cf <- cf[-c(46, 47, 48)]

# Subsetting data into two groups
# homogeneous F508del/F508del
homo <-
  subset(cf, sapply(cf, function(x)
    x$Genotype[1] == 'F508del/F508del'))

# heterogeneous F508del/G551D
hete <-
  subset(cf, sapply(cf, function(x)
    x$Genotype[1] == 'F508del/G551D'))
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

  
# Plot for each subject
# C vs time
p1 <-
  ggplot(cf[[11]],
         aes(x = `Time (min)...8`, y = `5-minute moving avg (mM)...11`)) +
  geom_point() +
  geom_smooth(method = "loess",
              formula = "y~x",
              se = TRUE) +
  xlim(0, 40) +
  ylim(0, 120) +
  # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
  xlab("Time (min)") +
  ylab("C (mM)")
  # xlab(expression(Sweat ~ rate ~ (mu * "L" ~ min ^ -1 ~ cm ^ -2))) +
  # ylab("C (mM)")
p1 <- p1 + ggtitle(sheets[11]) + theme(plot.title = element_text(hjust = 0.5))


# C vs SR
p2 <-
  ggplot(cf[[11]],
         aes(x = `SR...14`, y = `C_predose_smooth_2min`)) +
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
p2 <- p2 + ggtitle(sheets[11]) + theme(plot.title = element_text(hjust = 0.5))

# macroduct chloride data predost
macroduct <- data.frame(nrow =2, ncol = 2)
colnames(macroduct) <- c("Macroduct", "Value")
macroduct[1,1] <- "1"
macroduct[2,1] <- "2"
macroduct[1,2] <- cf[[11]]$`Macroduct Concentration 1 (mM)`[1]
macroduct[2,2] <- cf[[11]]$`Macroduct Concentration 2 (mM)`[1]
macroduct <- as.data.frame(macroduct)
p3 <-  ggplot(macroduct,
               aes(x = Macroduct, y = Value)) +
  geom_bar(stat="identity", fill = "steelblue") +
  
  # xlim(0, 1) +
  ylim(0, 120) +
  # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
  # xlab("Time (min)") +
  # ylab("C (mM)")
  xlab("Macroduct") +
  ylab("C (mM)")
p3 <- p3 + ggtitle(sheets[11]) + theme(plot.title = element_text(hjust = 0.5))

# Postdose

p4 <-
  ggplot(cf[[11]],
         aes(x = `Time (min)...22`, y = `5-minute moving avg (mM)...25`)) +
  geom_point() +
  geom_smooth(method = "loess",
              formula = "y~x",
              se = TRUE) +
  xlim(0, 40) +
  ylim(0, 120) +
  # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
  xlab("Time (min)") +
  ylab("C (mM)")
# xlab(expression(Sweat ~ rate ~ (mu * "L" ~ min ^ -1 ~ cm ^ -2))) +
# ylab("C (mM)")
p4 <- p4 + ggtitle(sheets[11]) + theme(plot.title = element_text(hjust = 0.5))


# C vs SR
p5 <-
  ggplot(cf[[11]],
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
p5 <- p5 + ggtitle(sheets[11]) + theme(plot.title = element_text(hjust = 0.5))


# macroduct chloride data postdost
macroduct <- data.frame(nrow =2, ncol = 2)
colnames(macroduct) <- c("Macroduct", "Value")
macroduct[1,1] <- "3"
macroduct[2,1] <- "4"
macroduct[1,2] <- cf[[11]]$`Macroduct Concentration 3 (mM)`[1]
macroduct[2,2] <- cf[[11]]$`Macroduct Concentration 4 (mM)`[1]
macroduct <- as.data.frame(macroduct)
p6 <-  ggplot(macroduct,
              aes(x = Macroduct, y = Value)) +
  geom_bar(stat="identity", fill = "steelblue") +
  
  # xlim(0, 1) +
  ylim(0, 120) +
  # labs (x = expression(Sweat~rate~(mu*"L"~min^-1~cm^-2)), y = "C (mM)")
  # xlab("Time (min)") +
  # ylab("C (mM)")
  xlab("Macroduct") +
  ylab("C (mM)")
p6 <- p6 + ggtitle(sheets[11]) + theme(plot.title = element_text(hjust = 0.5))

# plot_grid(p1,p2,p3,p4,p5,p6)

# Open an image file
png(
  file = "CM02_T1.png",
  width = 8,
  height = 5,
  units = 'in',
  res = 600
)

plot_grid(p1,p2,p3,p4,p5,p6,
          labels = c("Predose","","","Postdose","",""),
          label_size = 10,
          hjust = 0,
          vjust = 0.9)

# Close the image file
dev.off()

