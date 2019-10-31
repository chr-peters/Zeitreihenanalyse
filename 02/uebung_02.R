# 4)
# b) Erwartungswert der Fehlerterme 1, Fehler groesser als 0
#    Oft zur Schaetzung benutzt: ln => E(ln(u)) = 0
#    (Siehe auch Jensensche Ungleichung)
#
# c) Zu 1 multiplizieren, geometrisches Mittel sollte 1 sein

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