## Orange Juice Pricing ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/mcanela-iese/'
url2 = 'Quants_Book/master/orange.csv'
url = paste0(url1, url2)
df = read.csv(url)
str(df)

# Summary statistics #
summary(df)

# Regression line #
mod1 = lm(formula = Mshare ~ MMaid, data = df)
summary(mod1)

# Multiple regression analysis (1) #
mod2 = lm(formula = Mshare ~ TropPremium + Trop + MMaid +
  Aldi, data = df)
summary(mod2)

# Correlation analysis #
round(cor(df[, -1]), 3)

# Multiple regression analysis (2) #
mod3 = lm(formula = Mshare ~ Trop + MMaid + Aldi,
  data = df)
summary(mod3)

# Multiple regression analysis (3) #
mod4 = lm(formula = Mshare ~ Trop + MMaid, data = df)
summary(mod4)
