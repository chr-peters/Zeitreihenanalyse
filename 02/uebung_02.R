data <- read.table('Aufgabe05.txt', header = TRUE)
timeseries <- ts(data)

# estimate trend
trendModel <- lm(timeseries ~ time(timeseries))
trend <- predict(trendModel, newdata = time(timeseries))

# estimate seasonality
season <- timeseries - trend
plot(season)

seasonSpectrum <- spectrum(season)
maxFreqIndex <- which.max(seasonSpectrum$spec)
maxFreq <- seasonSpectrum$freq[maxFreqIndex]
maxFreqDensity <- seasonSpectrum$spec[maxFreqIndex]

seasonModel <- function(x) {
  #maxFreqDensity * sin(2 * pi * maxFreq * x)
  200 * sin(2 * pi * maxFreq * x)
}

error <- season - seasonModel(time(timeseries))
plot(error)