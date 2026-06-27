# ==============================================================================
# Project: ESG & Environmental Data (Soil Erosion)
# Description: Simple linear regression and correlation testing to model 
#              soil loss against vegetation cover.
# ==============================================================================

# 1. Setup and Data Import -----------------------------------------------------
getwd()
list.files()
options(scipen=9999) 
# Import dataset
er <- read.csv("Erosion.csv", stringsAsFactors = T)
# Initial Data Inspection (Commented out for production)
View(er)
str(er)
!is.na(er)

# 2. Exploratory Data Analysis & Correlation -----------------------------------
par(mfrow=c(1,1))
plot(er$vegcover, er$soilloss, col="blue", ylab = "Soil Loss", xlab = "Vegetation Cover", pch = 19)
# Test for correlation (Pearson's)
cor.test(er$vegcover, er$soilloss)

# 3. Simple Linear Regression Modelling ----------------------------------------
# Model soil loss as a function of vegetation cover (Y ~ X)
er.m1 <- lm(er$soilloss~er$vegcover)
View(er.m1)
# Review model summary (R-squared, p-values, coefficients)
summary(er.m1)
# c = Y-intercept = 21.66. m = er$vegcover(x) = -0.18
# Add the line of best fit to the scatter plot
abline(lm(er$soilloss~er$vegcover), col="black")