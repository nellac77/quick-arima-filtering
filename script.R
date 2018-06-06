# (1)  Load the packages needed for the analysis
library(lubridate) # to help with the date
library(ggplot2) # pretty plots
library(forecast)
library(tseries) # for time series analysis

# (2)  Bring in the raw data and take a look at it

data <- read.csv('cdma_data_gasoline.csv', stringsAsFactors = FALSE)

# class(data$TSTAMP) # View the date column class
#
# head(data) # View a bit of data
#
# data$TSTAMP <- as.POSIXlt(data$TSTAMP, format= "%m-%d-%Y") # Convert the time column to date class
#
# head(data) # Review
#
# data_ts <- ts(data, start = 2013, freq = 12) # Convert data frame to time series
#
# data_ts <- data_ts[,c(2:4)] # Adhoc af, but since proper ts object, I don't need TSTAMP field anymore
#
# # Quick look with base plot
# plot(data_ts)
# plot(aggregate(data_ts))
# boxplot(data_ts ~ cycle(data_ts))
#
# # Splt the fields up
# cdma.attepts.ts <- ts(data_ts[,1])
# gasoline.ts <- ts(data_ts[,2])
# voice.usage.ts <- ts(data_ts[,3])
#
# # test plot these values
# plot(cbind(cdma.attepts.ts, gasoline.ts, voice.usage.ts))
#
# # Any obvious correlation? Use the ITE (Interoccular Trauma Examination)!
# plot(as.vector(gasoline.ts), as.vector(voice.usage.ts),
#      xlab = 'Gasoline Consumed',
#      ylab = 'Voice Usage')
# abline(reg = lm(voice.usage.ts ~ gasoline.ts))
# cor(gasoline.ts, voice.usage.ts) # Correlation between these two series, pre-filtering

# Quick look with ggplot
data$Date <- as.Date(data$TSTAMP, "%m-%d-%Y")

ggplot(data, aes(Date, Gasoline)) +
  geom_line() +
  scale_x_date('month') +
  ylab("Gasoline Spending") +
  xlab("")

# Impute missing values in the CDMA Voice Attempts field
gasoline_ts <- ts(data[, c('Gasoline')])

data$clean_gasoline <- tsclean(gasoline_ts)

ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_gasoline)) +
  ylab('Cleaned Gasoline')

# Smoothing the series (moving average) - use the clean data, no outliers
data$clean_gasoline_ma <- ma(data$clean_gasoline, order = 12)
data$clean_gasoline_ma60 <- ma(data$clean_gasoline, order = 60)

ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_gasoline, colour = "Gasoline Spending")) +
  geom_line(data = data, aes(x = Date, y = clean_gasoline_ma, colour = "Monthly Moving Average")) +
  geom_line(data = data, aes(x = Date, y = clean_gasoline_ma30, colour = "Freq 60 Moving Average")) +
  ylab('Gasoline Spending')

## Decomposing the time series data

# Calculate the seasonal component with stl(), which uses smoothing, and subtract the
# seasonality from the original -> NOTE! Not enough periodicity for this...
clean_gasoline_ma <- ts(na.omit(data$clean_gasoline_ma), frequency = 12)
decomp <- stl(clean_gasoline_ma, s.window = "periodic")
deseasonal_gas <- seasadj(decomp)
plot(decomp)


# Test for stationarity with Augmented Dickey-Fuller (ADF) test
adf.test(clean_gasoline_ma, alternative = 'stationary')

# Autocorrelations and Choosing the model's order
Acf(clean_gasoline_ma, main='')
Pacf(clean_gasoline_ma, main='')

gas_d1 = diff(deseasonal_gas, differences = 1)
plot(gas_d1)
adf.test(gas_d1, alternative = "stationary")

# Autocorrelation again
Acf(gas_d1, main='ACF for Differenced Series')
Pacf(gas_d1, main='PACF for Differenced Series')

# (6) Fitting the ARIMA model
auto.arima(deseasonal_gas, seasonal=FALSE)

# (7) Evaluate and Iterate
fit <- auto.arima(deseasonal_gas, seasonal=TRUE)
tsdisplay(residuals(fit), lag.max=45, main='(0,2,0) Model Residuals')

fcast <- forecast(fit, h=30)
plot(fcast)
