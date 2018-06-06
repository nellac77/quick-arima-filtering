# (1)  Load the packages needed for the analysis
library(lubridate) # to help with the date
library(ggplot2) # pretty plots
library(forecast)
library(tseries) # for time series analysis

# (2)  Bring in the raw data and take a look at it
cdma.data <- read.csv('cdma_data.csv')
cdma.data$TSTAMP <- as.Date(cdma.data$TSTAMP, "%m-%d-%Y")
cdma.data.attempts <- cdma.data$CDMA_DATA_ATTEMPTS

elec.data <- cdma.data$US_Electricity_Supply_.GWh.

# Correlation between CDMA data attempts and US Electric Supply
cor(cdma.data.attempts, elec.data)

# Prewhiten Electricity data with ARIMA filter
arima.filter <- auto.arima(ts(elec.data, frequency = 12))
arima.filter

# The residuals are the filtered electricity data
elec.filt <- residuals(arima.filter)

# Verifying the residuals expose the noise properties
acf(residuals(arima.filter))
shapiro.test(residuals(arima.filter))

# Filter the voice data using the ARIMA filter
voice.data.filt <- residuals(Arima(cdma.data.attempts,
                                   order = c(1, 0, 0),
                                   seasonal = list(order = c(1, 0, 0), period = 12),
                                   fixed = c(0.4028, 0.8862, NA)))

# Apply traditional correlation to the filtered data set
cor(voice.data.filt, elec.filt)
