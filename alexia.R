## Default at Alexia Bank ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/quants-book/'
url2 = 'CSV_Files/master/alexia.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors=FALSE)
str(df)

# Data description #
table(df$GENDER, df$DEFAULT)
tapply(df$DEFAULT == 'Yes', df$GENDER, mean)
table(df$PART, df$DEFAULT)
tapply(df$DEFAULT == 'Yes', df$PART, mean)

# Regression analysis #
mod = lm(formula = (DEFAULT == 'Yes') ~ GENDER + AGE + CITIZEN +
  PART + INCOME + BALANCE, data = df)
summary(mod)

# Predictive scores #
scores = mod$fitted.values

# Confusion matrix (cutoff = 0.5) #
conf1 = table(scores > 0.5, df$DEFAULT == 'Yes')
conf1
acc1 = sum(diag(conf1))/sum(conf1)
acc1
tp1 = conf1['TRUE', 'TRUE']/sum(conf1[, 'TRUE'])
tp1
fp1 = conf1['TRUE', 'FALSE']/sum(conf1[, 'FALSE'])
fp1

# Confusion matrix (cutoff = 0.4) #
conf2 = table(scores > 0.4, df$DEFAULT == 'Yes')
conf2
acc2 = sum(diag(conf2))/sum(conf2)
acc2
tp2 = conf2['TRUE', 'TRUE']/sum(conf2[, 'TRUE'])
tp2
fp2 = conf2['TRUE', 'FALSE']/sum(conf2[, 'FALSE'])
fp2

# Confusion matrix (cutoff = 0.3) #
conf3 = table(scores > 0.3, df$DEFAULT == 'Yes')
conf3
acc3 = sum(diag(conf3))/sum(conf3)
acc3
tp3 = conf3['TRUE', 'TRUE']/sum(conf3[, 'TRUE'])
tp3
fp3 = conf3['TRUE', 'FALSE']/sum(conf3[, 'FALSE'])
fp3
