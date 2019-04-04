## Gender Discrimination ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/gender.csv'
url = paste0(url1, url2)
df = read.csv(url)
str(df)

# Creating dummy for male gender #
df$MALE = as.numeric(df$GENDER == 'Male') 

# Gender salary gap #
tapply(df$SALARY, df$MALE, mean)

# Gender experience gap #
tapply(df$TENURE, df$MALE, mean)

# Regression line #
mod1 = lm(formula = SALARY ~ TENURE, data = df)
summary(mod1)

# Figure 6.1 #
pdf("fig 6.1.pdf", width = 3.25, height = 3.5, pointsize = 7)
plot(formula = SALARY ~ TENURE, data = df,
  main = 'Figure 6.1. Salary vs. tenure (R = 0.549)',
  xlab = 'Tenure (years)', ylab = 'Salary (US dollars)',
  pch = 20)
abline(coefficients(mod1))
dev.off()

# Regression analysis (1) #
mod2 = lm(formula = SALARY ~ MALE, data = df)
summary(mod2)

# Regression analysis (2) #
mod3 = lm(formula = SALARY ~ TENURE + MALE, data = df)
summary(mod3)

