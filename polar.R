## Polar Bear Sales ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/polar.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors = FALSE)
str(df)

# Defining ts object #
xt = ts(data = df$SALES, start = c(2011, 1),
  end = c(2016, 12), frequency = 12, deltat = 1/12)

# Figure 10.1 #
plot(xt, main = 'Figure 10.1. Polar Bear monthly sales',
     xlab = '', ylab = 'Sales (units)')

# Fitting a linear trend #
N = nrow(df)
t = 1:N
trend = lm(xt ~ t)
summary(trend)
at = trend$fitted.values

# Figure 10.2 #
plot(xt, main = 'Figure 10.2. Polar Bear data with linear trend',
  xlab = '', ylab = 'Sales (units)')
lines(2011 + (t-1)/12, at, lty = 2)

# Calculating the seasonal factors #
month = substr(df$MONTH, 6, 7)
st = tapply(xt/at, month, mean)

# Figure 10.3 #
plot(st, type = 'b', pch = 16, main = 'Figure 10.3. Seasonal factors', 
  xlab = 'Month', ylab = '')

# Figure 10.4 #
st = rep(st, 6)
plot(xt, main = 'Figure 10.4. Predicted versus actual sales',
  xlab = '', ylab = 'Sales (units)')
lines(2011 + (t-1)/12, at*st, lty = 2)

# Prediction error #
zt = ts(xt - at*st, start = c(2011, 1), end = c(2016, 12),
  frequency = 12, deltat = 1/12)

# Figure 10.5 #
plot(zt, main = 'Figure 10.5. Prediction error',
  xlab = '', ylab = 'Sales (units)')

# Forecasting year 2017 #
a1 = trend$coefficients[1]
a2 = trend$coefficients[2]
ft = (a1 + a2*(73:84))*st[1:12]

# Table 10.1 #
tab1 = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG',
  'SEP', 'OCT', 'NOV', 'DEC')
tab2 = round(a1 + a2*(73:84), 1)
tab3 = round(st[1:12], 2)
tab4 = round(ft, 1)
cbind(tab1, tab2, tab3, tab4)
