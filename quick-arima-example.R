# (1)  Load the packages needed for the analysis
library(lubridate) # to help with the date
library(ggplot2) # pretty plots
library(forecast)
library(tseries) # for time series analysis

# (2)  Bring in the raw data and take a look at it
data <- read.csv('cdma_data_gasoline.csv')
data$TSTAMP <- as.Date(data$TSTAMP, "%m-%d-%Y")

cdma.data.attempts <- data$CDMA_VOICE_ATTEMPTS
gas.data <- data$Gasoline
usage.data <- data$VOICE_USAGE

# Correlation between CDMA data attempts and US Gas Spending
cor(cdma.data.attempts, gas.data, use = "complete.obs")

# Correlation between CDMA data attempts and CDMA Data Usage
cor(cdma.data.attempts, usage.data, use = "complete.obs")

# Prewhiten Gasoline data with ARIMA filter
gas.arima.filter <- auto.arima(ts(gas.data, frequency = 12))
gas.arima.filter

# Prewhiten CDMA Data Usage data with ARIMA filter
usage.arima.filter <- auto.arima(ts(usage.data, frequency = 12))
usage.arima.filter

# The residuals are the filtered gas data
gas.filt <- residuals(gas.arima.filter)

# The residuals are the filtered usage data
usage.filt <- residuals(usage.arima.filter)

# Verifying the residuals expose the noise properties
acf(residuals(gas.arima.filter))
shapiro.test(residuals(gas.arima.filter))

acf(residuals(usage.arima.filter))
shapiro.test(residuals(usage.arima.filter))

# Filter the CDMA attempts data using the ARIMA filters
attempts.data.gas.filt <- residuals(Arima(cdma.data.attempts,
                                   order = c(0, 1, 1),
                                   seasonal = list(order = c(1, 0, 0), period = 12),
                                   fixed = c(0.2664, 0.8256)))

attempts.data.usage.filt <- residuals(Arima(cdma.data.attempts,
                                          order = c(1, 1, 0),
                                          seasonal = list(order = c(1, 0, 0), period = 12),
                                          fixed = c(-0.5769, 0.5552)))

# Apply traditional correlation to the filtered data set
cor(attempts.data.gas.filt, gas.filt, use = "complete.obs")
cor(attempts.data.usage.filt, usage.filt, use = "complete.obs")
