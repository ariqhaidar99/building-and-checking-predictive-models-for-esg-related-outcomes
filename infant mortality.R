# ==============================================================================
# Project: ESG & Social Indicators (Global Infant Mortality)
# Description: Simple and multiple linear regression evaluating the impact of 
#              birth rate, per capita income, and farming populations.
# ==============================================================================

# 1. Setup and Data Import -----------------------------------------------------
getwd()
list.files()
options(scipen=9999)
im <- read.csv("InfantMortality.csv", stringsAsFactors = T)
View(im)
str(im)
!is.na(im)

# 2. Exploratory Data Analysis -------------------------------------------------
# Visualise all relationships simultaneously
pairs(im, main = "Infant Mortality Dataset Pairs Plot")

# 3. Simple Linear Regression Models -------------------------------------------
# Evaluating single explanatory variables against Infant Mortality Rate (RV)
# Model 1: Birth Rate
par(mfrow=c(1,1))
plot(im$BirthRate, im$IMRate, col="red", ylab = "Infant Mortality Rate", xlab = "Birth Rate", 
     main="Infant Mortality Rate vs Birth Rate", pch = 19)
im.m1 <- lm(im$BirthRate~im$IMRate)
summary(im.m1) # Adjusted R Value, 42.14%
abline(lm(im$BirthRate~im$IMRate), col="black")
# Model 2: Per Capita Income
plot(im$PerCapitaIncome, im$IMRate, col="blue", ylab = "Infant Mortality Rate", xlab = "Per Capita Income", pch = 19)
im.m2 <- lm(im$PerCapitaIncome~im$IMRate)
summary(im.m2) # Adjusted R Value, 53.99%
abline(lm(im$PerCapitaIncome~im$IMRate), col="black")
# Model 3: Population on Farms
plot(im$PoponFarms, im$IMRate, col="green", ylab = "Infant Mortality Rate", xlab = "Population on Farms", pch = 19)
im.m3 <- lm(im$PoponFarms~im$IMRate)
summary(im.m3) # Adjusted R Value, 46.66%
abline(lm(im$PoponFarms~im$IMRate), col="black")

# 4. Multiple Linear Regression & Model Selection ------------------------------
# Maximal Model: Include all explanatory variables
im.mm1 <- lm(im$IMRate ~ im$BirthRate + im$PerCapitaIncome + im$PoponFarms)
summary(im.mm1)
View(im.mm1)
# Updated Model: Remove non-significant terms (Population on Farms) to find Minimum Sufficient Model
im.mm2 <- update(im.mm1, ~. -im$PoponFarms)
summary(im.mm2)
# y = m1x + m2x+ c
# Equation derived: IMRate = 1.31*(BirthRate) - 0.04*(PerCapitaIncome) + 42.76

# 5. Model Diagnostics ---------------------------------------------------------
# Check residuals for normality and constant variance
par(mfrow = c(2, 2))
plot(im.mm2)
