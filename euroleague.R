## The Euroleague Final Four ##

# Setting time locale to English (this may not be needed) #
Sys.setlocale('LC_TIME', 'English')

# Importing data #
url1 = 'https://raw.githubusercontent.com/mcanela-iese/'
url2 = 'Quants_Book/master/euroleague.csv'
url = paste0(url1, url2)
df_raw = read.csv(url, stringsAsFactors=FALSE)
str(df_raw)

# Raw data analysis #
mean(df_raw$count)
sd(df_raw$count)
median(df_raw$count)
quantile(df_raw$count, probs=c(0.25, 0.75))

# Figure 2.4 #
plot(count/10^3 ~ as.POSIXct(minute), data = df_raw,
  main = 'Figure 2.4. Tweets per minute (line plot)',
  xlab = '', ylab = 'Tweets (thousands)', type = 'l')

# Figure 2.5 #
hist(df_raw$count,
  main = 'Figure 2.5. Tweets per minute (histogram)',
  xlab = 'Tweets', ylab = 'Frequency')

# Aggregating data #
df_raw$hour = substr(df_raw$minute, 1, 13)
df_raw$hour = paste0(df_raw$hour, ':00:00')
df_agg = aggregate(count ~ hour, data=df_raw, sum)

# Aggregate data analysis #
mean(df_agg$count)
sd(df_agg$count)
median(df_agg$count)

# Figure 2.6 #
plot(count/10^3 ~ as.POSIXct(hour), data = df_agg,
  main = 'Figure 2.6. Tweets per hour (line plot)',
  xlab = '', ylab = 'Tweets (thousands)', type = 'l')

# Figure 2.7 #
hist(df_agg$count/10^3,
  main = 'Figure 2.7. Tweets per hour (histogram)',
  xlab = 'Tweets (thousands)', ylab = 'Frequency')

# Figure 2.8 #
plot(log(count) ~ as.POSIXct(hour), data = df_agg,
  main = 'Figure 2.8. Tweets per hour, log scale (line plot)',
  xlab = '', ylab = 'Tweets (log))', type = 'l')

# Figure 2.9 #
hist(log(df_agg$count),
  main = 'Figure 2.9. Tweets per hour, log scale (histogram)',
  xlab = 'Tweets (log)', ylab = 'Proportion', freq = FALSE)
lines(seq(4, 12, by=0.01), dnorm(seq(4, 12, by=0.01),
  mean=mean(log(df_agg$count)), sd=sd(log(df_agg$count))), lty=4, lwd=1.5)
