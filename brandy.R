## Brandy Consumption in Australia ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/brandy.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors=FALSE)
str(df)

# Figure 12.3 #
xt = ts(data = df$SALES, start=c(1985,1),
  end=c(2014,2), frequency = 4, deltat = 1/4)
plot(xt,
  main = 'Figure 12.3. Brandy quarterly sales (1985-2013)',
  xlab = '', ylab = 'Brandy sales (1000 litres alcohol)')

# Dropping year 2014 #
df = df[1:(nrow(df) - 2), ]

# Figure 12.4 #
xt = ts(data = df$SALES, start = c(1985,1),
  end = c(2013, 4), frequency = 4, deltat = 1/4)
N = nrow(df)
t = 1:N
lt = lm(xt ~ t)$fitted.values
qt = lm(xt ~ t + I(t^2))$fitted.values
plot(xt,
  main = 'Figure 12.4. Brandy sales with linear and quadratic trends',
  xlab = '', ylab = 'Brandy sales (1000 litres alcohol)')
lines(1985 + (t - 1)/4, lt)
lines(1985 + (t - 1)/4, qt, lty = 2)

# Figure 12.5 #
mat = (1/8)*xt[1:(N - 4)] + (1/4)*xt[2:(N - 3)] +
  (1/4)*xt[3:(N - 2)] + (1/4)*xt[4:(N - 1)] + (1/8)*xt[5:N]
mat = c(NA, NA, mat, NA, NA)
plot(xt, main = 'Figure 12.5. Brandy sales with MA trend',
   xlab = '', ylab = 'Brandy sales (1000 litres alcohol)')
lines(c(NA, NA, 1985 + (t[2:(N - 3)])/4, NA, NA), mat, lty = 2)

# Dropping years 1985-99 #
df = df[65:N, ]
xt = ts(data = df$SALES, start=c(2001,1),
  end = c(2013,4), frequency = 4, deltat = 1/4)
qt = qt[65:N]

# Figure 12.6 #
quarter = substr(df$DATE, 6, 7)
seasonals = tapply(xt/qt, quarter, mean)
st = rep(seasonals, 13)
dt = xt/st
plot(xt, main = 'Figure 12.6. Deseasonalized brandy sales',
     xlab = '', ylab = 'Brandy sales (1000 litres alcohol)')
t = 1:52
lines(2001 + (t - 1)/4, dt, lty=2)

# Figure 12.7 #
et = dt[1]
for(i in 2:52) et[i] = 0.2*dt[i] + 0.8*et[i - 1]
plot(xt, 
  main = 'Figure 12.7. Brandy sales with exponential smoothing trend',
  xlab = '', ylab = 'Brandy sales (1000 litres alcohol)')
lines(2001 + (t - 1)/4, et, lty=2)
