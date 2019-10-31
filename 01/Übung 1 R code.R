# Aufgabe 1


### Sonnenflecken


# Daten-Import
sonne = read.table(file.choose(), header=T)
attach(sonne)

View(sonne)

# Optional -> Time-Series-Objekt; 
sonne_ts = ts(data=Sonnenflecken, start=1770, end=1869)

# Grafik
plot(Jahr, Sonnenflecken, type="l", xlab="Zeit",ylab="Anzahl Sonnenflecken", main="Anzahl der Sonnenflecken zwischen 1770 und 1869")
# alternativ
plot(sonne_ts, type="l", xlab="Zeit",ylab="Anzahl Sonnenflecken", main="Anzahl der Sonnenflecken zwischen 1770 und 1869")

# Kein Trend, stark ausgeprägte Saisonfigur, die sich ungefähr alle 10-12 Jahre wiederholt 
# Die Zeitreihe ist stabil - empirische Kenngrößen und ACF können auch anhand der Rohdaten berechnet und sinnvoll interpretiert werden

# Mittelwert
mean(Sonnenflecken)
# 46.6701

# Varianz und Std. Abweichung
var(Sonnenflecken)
sd(Sonnenflecken)

# Autokovarianz
sonne_acov <- acf(sonne_ts, type = "covariance", plot = TRUE)

# Autokorrelation: normiert
acf(Sonnenflecken, main="Korrelogramm der Zeitreihe der Anzahlen der Sonnenflecken",lag.max=25)

# Hohe positive Autokorrelation für h=10, 11 Hinweis auf eine Saisonfigur der Länge 10 oder 11
# negative Autokorrelation für tau=5 -> Beobachtungen, die 5 Jahre auseinanderliegen, sind tendenziell negativ korreliert
# Passt zum Plot der Daten: etwa alle 11 Jahre steigt die Zahl der Flecken an, um dann zwischen 
# 2 Anstiegen (5-6 Jahre davor bzw. danach) nimmt ab

# Never use attach without a detach
detach(sonne)





### Stromproduktion

strom <- read.table(file.choose(), header=T, skip=1) #este Zeile überspringen
attach(strom)

View(strom)

# TS-Objekt
strom_ts = ts(data=Strom, start=c(1955,1), end=c(1979, 12), frequency=12)

plot(strom_ts, type="l", ylab="Produzierter Strom", main="Produzierter Strom in der BRD von 1955 bis 1979")

# Steigender linearer Trend, vermutlich Änderung der Trendsteigung um 1969
# Variable Saisonfigur, die Varianz der saisonalen Schwankungen nimmt zu mit steigendem Niveau -> multiplikatives Komponentenmodell
# Transformation zur Stabilisierung der Varianz erforderlich

# Was passiert mit Differenzenbildung?
diff_strom = diff(strom_ts, lag=1)

plot(diff_strom, type="l",  ylab="Produzierter Strom: Differenzen", main="Differenzen der Stromproduktion in der BRD von 1955 bis 1979")
# Der Trend wird eliminiert, allerdings die Varinz ist nicht stabil


# Wachstumsraten? (Y(t)-Y(t-1))/Y(t-1), *100 für %
wraten_strom_p <- 100*diff_strom/strom_ts[-length(strom_ts)] # strom_ts[-length(strom_ts)] gibt den Vektor strom_ts ohne die letzte Beobachtung an

plot(wraten_strom_p, type="l",  ylab="Wachstumsrate in %", main="Wachstumsraten der Stromproduktion in der BRD von 1955 bis 1979")
# Der Trend wird eliminiert, und auch die Varianz wird stabilisiert

# Mittelwert
mean(wraten_strom_p)
# 0.7816087 : im Durchschnitt wächst die Stromproduktion um weginer als 1% im Vergleich zum Vormonat

# Varianz und Std. Abweichung
var(wraten_strom_p)
sd(wraten_strom_p)
# Die berechneten Wachstumsraten weisen eine relativ große Varianz au: Standardabweichung ist 6.8%

# Autokorrelationsfunktion
acf(wraten_strom_p, main="Korrelogramm der Wachstumsraten zum Vormonat des produzierten Stroms in der BRD", lag.max=50)

# positive Korrelation für Werte, die etwa 12 Monate auseinanderliegen
# negative Korrelation für Werte, die etwa 6 Monate auseinanderliegen
# Erklärung: hoher Stromverbrauch im Winter, geringer Stromverbrauch im Sommer
# also: positive Korrelation, wenn die Winter- oder Sommermonate jeweils untereinander verglichen werden
# negative Korrelation, wenn Sommer- mit Wintermonaten verglichen werden



# Alternative zu Wachstumsraten: Differenzen (Ansteigen der Werte verhindern) der logarithmierten Werte (Ansteigen der Varianz verhindern)
ln_strom = log(strom_ts) #in R, log steht für ln (und es existiert noch log10)

plot(ln_strom, type="l", ylab="ln Stromproduktion", main="Logarithmierte Stromproduktion in der BRD von 1955 bis 1979")
# Stabile Varianz, Trend vorhanden, linear/quadratisch

#Trendelimination durch Differenzenbildung
diff_ln_strom = diff(ln_strom, lag=1)

plot(diff_ln_strom, type="l", ylab="Diffenrenzen in ln Stromproduktion", main="Differenzen der logarithmierten Stromproduktion")

# Vergleich Wachstumsraten und diff_ln: gering
plot(wraten_strom_p/100 - diff_ln_strom)


# Wachstumsraten im Vergleich zum Vorjahr
diff12_strom = diff(strom_ts, lag=12)

# Wachstumsraten? (Y(t)-Y(t-12))/Y(t-12), *100 fuer %
wraten12_strom_p  = 100*diff12_strom/(window(strom_ts, start=c(1955,1), end= c(1978,12))) # strom_ts[-length(strom_ts)] gibt den Vektor strom_ts ohne die letzte Beobachtung an

plot(wraten12_strom_p, type="l",  ylab="Wachstumsrate in %", main="Wachstumsraten der Stromproduktion im Vergleich zu dem selben Monat im Vorjahr")
# Keine Saisonalitaet mehr, vielleicht leichter absteigender Trend -> langfristige Entwicklung
# Seasonal differencing at lag k eliminates saisonality with period k


# Mittelwert
mean(wraten12_strom_p)
# 6.339736 : average annual growth rate

# Varianz und Std. Abweichung
var(wraten12_strom_p)
sd(wraten12_strom_p)

# Autokorrelationsfunktion
acf(wraten12_strom_p, main="Korrelogramm der Wachstumsraten im Vergleich zu dem selben Monat im Vorjahr", lag.max=50)
# ACF typisch fuer Zeitreihen mit Trend; er scheint aber stochastisch zu sein


detach(strom)



### Credit spreads

spreads = read.table(file.choose(),header=T,skip=1)

cdsspreads = spreads[,2]
zeit = as.Date(spreads[,1],"%d/%m/%Y")

plot(zeit,cdsspreads,type="l", main="Annualisierte CDS Spreads Für Deutschland in Basispunkten \n von Februar 2009 bis August 2012",xlab="Zeit",ylab="CDS-Spreads")
# Kein det. Trend, keine Saisonalität. Stohastischer trend? Keine systematische Veränderung der Zeitreihe

diff_cds = diff(cdsspreads)
plot(zeit[2:length(zeit)], diff_cds, type="l", main="Differenzen der annualisierten CDS-Spreads",ylab="Differenzen")
#kein Trend,  Volatilit�tscluster

mean(diff_cds)
sd(diff_cds[-ceiling(0.65*length(zeit))]) #the first 65% of the sample
sd(diff_cds[ceiling(0.65*length(zeit)):length(zeit)-1]) #the remaining 35% of the sample

acf(cdsspreads, main="Korrelogramm der Zeitreihe der CDS Spreads", lag.max=50)
# aufeinanderfolgende Werte sind stark abhängig

acf(diff_cds, main="Korrelogramm der Zeitreihe der CDS Spreads", lag.max=50)
# keine Saisonfigur




### Exercise 3

library(fma)

# plot the data
par(mfrow=c(1,1))
ts.plot(elec, main="Monatliche Stromproduktion in Australien", ylab=" Stromproduktion in Millionen KWh")


### Data preparation

# see what the data looks like
elec

#check length of time series
length(elec)

# 476 observations, but last four months of 1995 are missing
# use only data from 1954 to 1994
elec = elec [1:468]

# assign a year to each observation
year = rep (1956:1994 , each = 12)

# split data by year
elec_year = split(elec, year)


### BoxCox transformation: use yearly data for auxiliary regression to compute alpha 

# compute mean value for each year
mittel = do.call(c, lapply(elec_year, mean))

# compute std. deviations for each year
sdw = do.call(c, lapply(elec_year, sd))

# auxiliary regression: OLS
reg_boxcox = lm(log(sdw) ~ log( mittel))

# compute lambda = 1-alpha
lambda = 1 - coefficients(reg_boxcox)[2]
#0.2228528 

#Let's see what we've done
plot (log(sdw) ~ log( mittel), xlab=expression (log(bar(x))), 
      ylab =expression (log( sigma )), main=expression( paste(" Bestimmung von", " ", lambda)))

# add the fitted regression line
abline (reg_boxcox)
text (8.5, 5.5 , labels = expression(paste(lambda , "" ,"= 0.223")))
# not too bad!

# write a function for the Box-Cox transformation
boxcox <- function (data, lambda){
  if( lambda == 0){
    log( data )
  }
  else ( data^lambda-1)/ lambda
}

# transform
elec_boxcox = boxcox(elec, lambda)

# check what this gives us
plot (elec_boxcox , type = "l", main = " Transformierte Produktion",
      xaxt="n", xlab = "Jahr ", ylab = " Transformierte Produktion ")
axis (1, seq (1956, 1994), at = seq (1 ,468 ,12) )
# much better! Stable variance

# compare the results with the built-in function for Box-Cox transformation
library (forecast)

plot(BoxCox(elec, lambda), type = "l", main = " Transformierte Produktion ", xaxt = "n", xlab = " Jahr ", 
    ylab = " Transformierte Produktion ")
(axis (1, seq(1956 ,1994), at = seq(1 ,468 ,12)))
#same result

### Linear or quadratic trend? Linear doesn't look plausible, so fit quadratic

# time vector
time = 1:468

# Design matrix
X = cbind (1, time, time^2)

# Dependent variable
Y = elec_boxcox

# OLS
Beta = solve (t(X)%*%X)%*%t(X)%*%Y
# 1.757306e+01
# time  4.597341e-02
# -2.597934e-05


# regression fit
elec_trend = X%*%(Beta)

# Detrend observed data
elec_trendstat = elec_boxcox - elec_trend

#check if trend has been eliminated
plot (elec_trendstat , type = "l", main = " Trendbereinigte transformierte Produktion",
      xaxt="n", xlab = "Jahr ", ylab = " Trendbereinigte transformierte Produktion ")
axis (1, seq (1956, 1994), at = seq (1 ,468 ,12) )
# wonderful!

### ACF - allow for enough lags in order to see saisonality

acf(elec_trendstat, lag = 50)
#peaks at 12, 24, 36, ... months -> yearly cycle

### Computing the seasonal effects

# compute the global mean
equer = mean(elec_trendstat)

# compute the mean for each month over the years:
ej_quer = numeric(12)

for(i in 1:12) {
  ej_quer [i] = mean (elec_trendstat [seq(i, 468 , by = 12) ,])
}

# compute the seasonal figure
elec_saison = ej_quer - equer

# check what it looks like
plot (elec_saison, type = "l", xlab = " Monat ", main = " Darstellung der
Saisonfigur ", ylab = " Saisonkoeffizient ")

# do the seasonal components sum up to zero?
sum( elec_saison )

### OLS estimation of trend and seasonal components
Z = cbind (1, time , time^2)

# matrix with seasonal components
S = kronecker(rep (1, 39), rbind (diag (12 -1), -1))

# solve using the formulae given in the lecture
beta.hat  = solve((t(Z)%*%Z) - t(Z)%*%S%*% solve(t(S)%*%S)%*%t(S)%*%Z)%*%(t(Z) - t(Z)%*%S%*%solve(t(S)%*%S)%*%t(S))%*%Y
delta.hat = solve((t(S)%*%S) - t(S)%*%Z%*% solve(t(Z)%*%Z)%*%t(Z)%*%S)%*%(t(S) - t(S)%*%Z%*%solve(t(Z)%*%Z)%*%t(Z))%*%Y

# Result:
#beta.hat
#[,1]
#1.757860e+01
#time  4.594466e-02
#-2.596294e-05

#delta.hat
#[,1]
#[1,] -0.54303898
#[2,] -0.70574445
#[3,] -0.15103998
#[4,] -0.40424062
#[5,]  0.33995224
#[6,]  0.53246729
#[7,]  0.95168263
#[8,]  0.71201855
#[9,]  0.07125885
#[10,] -0.02438814
#[11,] -0.33252967

# model fit
fit = Z %*% beta.hat + S %*% delta.hat

# check out what it looks like
plot(fit , type = "l", xlab = "t", main = " Angepasstes Modell ")
# very much like the original series! 

# plot the seasonal component
plot((S %*% delta.hat)[1:12] , type = "l", xlab = "t", main = "Darstellung der Saisonfigur ", ylab = " Saisonkomponente ")
# exactly what we saw before!

# check what the residuals look like
res = elec_boxcox - fit

plot(res , type = "l", ylab = " Residuen ", xlab = "t")

# There is structure in the residuals -> looks like autocorrelation is present. Check:

acf(res , main = "ACF der Residuen ")

# the autocorrelation in the random component is quite significant indeed 
# -> it must be accounted for by techniques which we'll cover later in the course 
