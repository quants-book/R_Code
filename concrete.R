## Concrete Resistance ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/mcanela-iese/'
url2 = 'Quants_Book/master/concrete.csv'
url = paste0(url1, url2)
df = read.csv(url)
str(df)

# Regression line (1) #
mod1 = lm(formula = Resistance ~ Cement, data = df)
summary(mod1)

# Figure 4.1 #
plot(formula = Resistance ~ Cement, data = df,
  main = 'Figure 4.1. Resistance vs. cement (R = 0.786)',
  xlab = "Cement (kg/m3)", ylab = "Resistance (kg/cm2)",
  pch = 20)
abline(coefficients(mod1))

# Regression line (2) #
mod2 = lm(formula = Resistance ~ Additives, data = df)
summary(mod2)

# Figure 4.2 #
plot(formula = Resistance ~ Additives, data = df,
  main = 'Figure 4.2. Resistance vs. additives (R = 0.646)',
  xlab = "Additives (kg/m3)", ylab = "Resistance (kg/cm2)",
  pch = 20)
abline(coefficients(mod2))

# Regression line (3) #
mod3 = lm(formula = Resistance ~ Water, data = df)
summary(mod3)

# Figure 4.3 #
plot(formula = Resistance ~ Water, data = df,
  main = 'Figure 4.3. Resistance vs. water (R = 0.104)',
  xlab = "Water (kg/m3)", ylab = "Resistance (kg/cm2)",
  pch = 20)
abline(coefficients(mod3))

# Multiple regression analysis #
mod4 = lm(formula = Resistance ~ Cement + Additives +
  Water, data = df)
summary(mod4)

# Figure 4.4 #
plot(formula = Resistance ~ mod4$fitted.values, data = df,
  main = 'Figure 4.4. Resistance vs. predicted resistance',
  xlab = "Predicted resistance (kg/cm2)", ylab = "Resistance (kg/cm2)",
  pch = 20)
mod5 = lm(formula = Resistance ~ mod4$fitted.values, data = df)
abline(coefficients(mod5))
