library(magrittr)
library(forecast)
library(astsa)

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Step 0: Data Prep ####

load(paste0(getwd(),"/finalproject.RData"))

export_data <- ts(finalPro_data$Exports,1960,2017)

#### Step 1: Plot Data Identify Outliers ####

# No outliers, general downward trend. Not stationary
export_data %>% plot

#### Step: 2 Transform Data ####

# log helps data be more stationary but impact on ACF/PACF is small 
#export_data %<>% log
#export_data %<>% sqrt
#export_data <- 1 / export_data

#export_data <- export_data^2 

#### Step 3: Take Differences If Not Stationary ####

# 1st order difference looks stationary
export_data_diff <- export_data %>% diff
export_data_diff %>% plot

# mean is close to 0
export_data_diff %>% mean

#### Step 4: Examine ACF/PACF ####

# ACF implies MA(1)
# PACF implies AR(2)

export_data_diff %>% acf
export_data_diff %>% pacf


#### Step 5: Create Models ####

ma_model <- sarima(export_data,0,1,1,no.constant = TRUE)
ar_model_2 <- sarima(export_data,2,1,0,no.constant = TRUE)


#### Step 6: Check Residuals Are White Noise ####

#### Step 7: Forcast ####

forcast_model <- arima(export_data, order = c(0,1,1))

forecast(forcast_model,h = 10) %>% autoplot
