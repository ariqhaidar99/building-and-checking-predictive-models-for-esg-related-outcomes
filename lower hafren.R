# ==============================================================================
# Project: ESG & Water Quality Analysis (Lower Hafren River)
# Description: Multiple linear regression and residual diagnostics modelling 
#              Dissolved Organic Carbon (DOC) against flow, conductivity, and temp.
# ==============================================================================

# 1. Setup and Data Import -----------------------------------------------------
options(scipen = 999)
haf <- read.csv("LowerHafren.csv", stringsAsFactors = T)

# 2. Exploratory Data Analysis -------------------------------------------------
pairs(haf, main = "Lower Hafren Water Quality Pairs Plot")
summary(haf)

# 3. Multiple Linear Regression Modelling --------------------------------------
# Simple baseline model
haf_simple <- lm(haf$DOC ~ haf$Flow)
summary(haf_simple)
# Maximal Model: DOC as a function of Flow, Conductivity, and Temperature
haf.max <- lm(haf$Flow ~ haf$DOC + haf $Cond + haf$Temp)
summary(haf.max)

# 4. Model Diagnostics ---------------------------------------------------------
# Evaluate the health of the model residuals
par(mfrow = c(2, 2))
plot(haf.max)