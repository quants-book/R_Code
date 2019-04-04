## Diesel Consumption ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/mcanela-iese/'
url2 = 'Quants_Book/master/diesel.csv'
url = paste0(url1, url2)
df = read.csv(url)
str(df)

# Regression line (1) #
mod1 = lm(formula = EFFICIENCY ~ POWER, data = df)
summary(mod1)

# Figure 7.1 #
plot(formula = EFFICIENCY ~ POWER, data = df,
  main = 'Figure 7.1. Fuel efficiency vs. power use (R = -0.339)',
  xlab = 'Power use (%)', ylab = 'Fuel efficiency (km/l)', pch = 20)
abline(coefficients(mod1))

# Regression line (2) #
mod2 = lm(formula = EFFICIENCY ~ IDLE, data = df)
summary(mod2)

# Figure 7.2 #
plot(formula = EFFICIENCY ~ IDLE, data = df,
  main = 'Figure 7.2. Fuel efficiency vs. idle time (R = -0.256)',
  xlab = 'Idle time (%)', ylab = 'Fuel efficiency (km/l)', pch = 20)
abline(coefficients(mod2))

# Multiple regression analysis #
mod3 = lm(formula = EFFICIENCY ~ ., data = df)
summary(mod3)

# Splitting the sample #
f = function(x) c(mean(x), sd(x))
tapply(df$EFFICIENCY, df$DAIMLER, f)
tapply(df$IDLE, df$DAIMLER, f)
tapply(df$POWER, df$DAIMLER, f)
table(df$DAIMLER, df$YEARS)
mod4.1 = lm(formula = EFFICIENCY ~ IDLE + POWER + YEARS,
  data = df[df$DAIMLER==1,])
summary(mod4.1)
mod4.0 = lm(formula = EFFICIENCY ~ IDLE + POWER + YEARS,
  data = df[df$DAIMLER==0,])
summary(mod4.0)

# Analysis with interaction terms #
mod5 = lm(formula = EFFICIENCY ~ DAIMLER + IDLE +
  I(DAIMLER*IDLE) + POWER + I(DAIMLER*POWER) + YEARS +
  I(DAIMLER*YEARS), data = df)
summary(mod5)

