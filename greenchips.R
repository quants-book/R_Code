## Predicting Sales from Price ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/greenchips.csv'
url = paste0(url1, url2)
df = read.csv(url)
str(df)

# Summary statistics #
c(mean(df$SALES), sd(df$SALES), cor(df$SALES, df$PRICE))
c(mean(df$PRICE), sd(df$PRICE))

# Regression line #
model = lm(formula = SALES ~ PRICE, data = df)
summary(model)

# Figure 3.2 #
plot(formula = SALES ~ PRICE, data = df,
  main = 'Figure 3.2. Regression line (R = -0.881)',
  xlab = 'Price (euros)', ylab = 'Sales (1000s)',
  pch = 16)
abline(coefficients(model))
