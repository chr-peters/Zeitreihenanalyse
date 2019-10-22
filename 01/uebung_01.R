library(fma)

# 1)

sonnenflecken <- read.table('sonnenflecken.txt', header = TRUE)
stromproduktion <- read.table('stromproduktion.txt', skip = 2, header = TRUE)
creditspreads <- read.table('CDS Spreads.txt', skip = 2, header = TRUE, stringsAsFactors = FALSE)

# a)
plot(sonnenflecken)
lines(sonnenflecken)
mean(sonnenflecken$Sonnenflecken)
var(sonnenflecken$Sonnenflecken)

stromproduktion$Datum <- as.Date(paste0(stromproduktion$Jahr, '-', stromproduktion$Monat, '-01'))
plot(stromproduktion$Datum, stromproduktion$Strom)
lines(stromproduktion$Datum, stromproduktion$Strom)
mean(stromproduktion$Strom)
var(stromproduktion$Strom)

creditspreads$Datum <- as.Date(creditspreads$Datum, format='%d/%m/%Y')
plot(creditspreads)
lines(creditspreads)
mean(creditspreads$CDSSpreads)
var(creditspreads$CDSSpreads)

# b)
acf(sonnenflecken$Sonnenflecken)

# estimate and remove the trend
stromproduktion$log_Strom <- log(stromproduktion$Strom)
stromTrendModel <- lm(stromproduktion$log_Strom ~ stromproduktion$Datum)
stromproduktion$log_Strom_noTrend <- stromproduktion$log_Strom - predict(stromTrendModel, newdata = stromproduktion$Datum)

plot(stromproduktion$Datum, stromproduktion$log_Strom_noTrend)
lines(stromproduktion$Datum, stromproduktion$log_Strom_noTrend)

acf(stromproduktion$log_Strom_noTrend)

acf(creditspreads$CDSSpreads)

# 2)

plot(elec)
