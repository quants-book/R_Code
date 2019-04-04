## Tata Auto Daily Returns ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/tata.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors=FALSE)
str(df)
N = length(df$PRICE)

# Calculating the daily returns #
return = 100*(df$PRICE[-1]/df$PRICE[-N] - 1)

# Summary statistics #
mean(return)
sd(return)

# Calculating 95% limits #
lower = mean(return) - 2*sd(return)
lower
upper = mean(return) + 2*sd(return)
upper

# Counting the observations outside the limits #
sum(return < lower)
sum(return > upper)
mean(return < lower | return > upper)

# Getting the dates #
date = df$DATE[-1]
date[return < lower]
date[return > upper]

# Figure 1.1 #
plot(return ~ as.Date(date),
  main = 'Figure 1.1. Tata Auto daily returns (line plot)',
  xlab = '', ylab = 'Daily returns (%)', type='l')

# Figure 1.2 #
hist(return,
  main = 'Figure 1.2. Tata Auto daily returns (histogram)',
  xlab = 'Daily returns (%)', ylab = 'Proportion',
  freq = FALSE)

# Figure 1.3 #
hist(return,
  main = 'Figure 1.3. Matching the normal distribution',
  xlab = 'Daily returns (%)', ylab = 'Proportion',
  freq = FALSE)
lines(seq(-6, 6, by = 0.01), dnorm(seq(-6, 6, by = 0.01),
  mean = mean(return), sd = sd(return)), lty = 4, lwd = 1.5)
