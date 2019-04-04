## Supermercados Andinos ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/mcanela-iese/'
url2 = 'Quants_Book/master/andinos.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors=FALSE)
str(df)

# Figure 13.1 #
xt = ts(data = df$sales, start = c(2013, 1),
  end = c(2017, 6), frequency = 12, deltat = 1/12)
plot(xt, main = 'Figure 13.1. Rib eye monthly sales',
  xlab = '', ylab = 'Sales (units)')

# Holt-Winters approximation (1) #
alpha = 0.2
beta = 0.2
gamma = 0.2
xt = ts(data = xt[1:48], start = c(2013, 1),
  end = c(2016, 12), frequency = 12, deltat = 1/12)
st = rep(1, 12)
at = xt[1]/st[1]
bt = 0
for(i in 2:12) {
  at[i] = alpha*xt[i]/st[i] +
    (1 - alpha)*(at[i - 1] + bt[i - 1])
  bt[i] = beta*(at[i] - at[i - 1]) + (1 - beta)*bt[i - 1]
}
for(i in 13:48) {
  at[i] = alpha*(xt[i]/st[i - 12]) +
    (1 - alpha)*(at[i - 1] + bt[i - 1])
  bt[i] = beta*(at[i] - at[i - 1]) + (1 - beta)*bt[i - 1]
  st[i] = gamma*(xt[i]/at[i]) + (1 - gamma)*st[i - 12]
}
hwt = at*st

# Figure 13.2 #
plot(xt, main = 'Figure 13.2. Rib eye sales with HW trend (alpha = 0.2)',
  xlab = '', ylab = 'Sales (units)')
t = 1:48
lines(2013 + (t-1)/12, at, lty = 2)

# Holt-Winters forecasting (1) #
at[49:54] = at[48] + (1:6)*bt[48]
hwt[49:54] = at[49:54]*st[37:42]

# Holt-Winters approximation (2) #
alpha = 0.5
beta = 0.2
gamma = 0.2
xt = ts(data = xt[1:48], start = c(2013, 1),
  end = c(2016, 12), frequency = 12, deltat = 1/12)
st = rep(1, 12)
at = xt[1]/st[1]
bt = 0
for(i in 2:12) {
  at[i] = alpha*xt[i]/st[i] +
    (1 - alpha)*(at[i - 1] + bt[i - 1])
  bt[i] = beta*(at[i] - at[i - 1]) + (1 - beta)*bt[i - 1]
}
for(i in 13:48) {
  at[i] = alpha*(xt[i]/st[i - 12]) +
    (1 - alpha)*(at[i - 1] + bt[i - 1])
  bt[i] = beta*(at[i] - at[i - 1]) + (1 - beta)*bt[i - 1]
  st[i] = gamma*(xt[i]/at[i]) + (1 - gamma)*st[i - 12]
}
hwt = at*st

# Figure 13.3 #
plot(xt, main = 'Figure 13.3. Rib eye sales with HW trend (alpha = 0.5)',
  xlab = '', ylab = 'Sales (units)')
t = 1:48
lines(2013 + (t-1)/12, at, lty = 2)

# Holt-Winters forecasting (2) #
at[49:54] = at[48] + (1:6)*bt[48]
hwt[49:54] = at[49:54]*st[37:42]
