## Bayou Beer Sales ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/bayou.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors = FALSE)
str(df)

# Figure 11.1 #
xt = ts(data = df$SALES, start= c(2008,1),
  end = c(2017, 12), frequency = 12, deltat = 1/12)
plot(xt, main ='Figure 11.1 Bayou Beer sales',
  xlab = '', ylab = 'Thousand barrels')

# Splitting the data set #
df1 = df[df$YEAR < 2017, ]
df2 = df[df$YEAR == 2017, ]

# Fitting a linear trend (2008-2016) # 
t = 1:108
lmod = lm(formula = SALES ~ t, data = df1)
summary(lmod)
ltrend = lmod$fitted.values

# Fitting a quadratic trend (2008-2016) # 
t2 = t^2
qmod = lm(formula = SALES ~ t + t2, data = df1)
summary(qmod)
qtrend = qmod$fitted.values

# Figure 11.2 #
plot(xt,
  main = 'Figure 11.2. Bayou Beer sales with linear and quadratic trends',
  xlab = '', ylab = 'Thousand barrels')
lines(2008 + (t - 1)/12, ltrend)
lines(2008 + (t - 1)/12, qtrend, lty = 2)

# Calculating the seasonals #
month = rep(1:12, 9)
seasonal = tapply(df1$SALES - qtrend, month, mean)

# Figure 11.3 #
plot(seasonal, main = 'Figure 11.3. Seasonals',
  xlab = 'Month', ylab = 'Thousand barrels', type = 'b',
  pch = 16)

# Predicted sales and prediction error #
pred_sales = qtrend + rep(seasonal, 9)
pred_error = df1$SALES - pred_sales

# Figure 11.4 #
plot(xt, main = 'Figure 11.4. Predicted vs. actual sales',
  xlab = '', ylab = 'Thousand barrels')
lines(2008 + (t - 1)/12, pred_sales, lty = 2)

# Figure 11.5 #
et = ts(data = pred_error, start = c(2008, 1), 
  end = c(2017, 12), frequency = 12, deltat = 1/12)
plot(et, main = 'Figure 11.5. Prediction error',
  xlab = '', ylab = 'Thousand barrels')

# Forecasting year 2017 #
b0 = qmod$coefficients[1]
b1 = qmod$coefficients[2]
b2 = qmod$coefficients[3]
forecast = b0 + b1*(109:120) + b2*(109:120)^2 + seasonal

# Table 11.1 #
tab1 = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
  'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
tab2 = round(b0 + b1*(109:120) + b2*(109:120)^2, 3)
tab3 = round(seasonal, 3)
tab4 = round(forecast, 3)
tab5 = round(df2$SALES, 3)
tab6 = round(df2$SALES - forecast, 3)
cbind(tab1, tab2, tab3, tab4, tab5, tab6)
