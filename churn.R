## The Churn Model ##

# Importing data #
url1 = 'https://raw.githubusercontent.com/mcanela-iese/'
url2 = 'Quants_Book/master/churn.csv'
url = paste0(url1, url2)
df = read.csv(url, stringsAsFactors=FALSE)
str(df)

# Churning rate #
sum(df$CHURN)

# Crosstabulation of data plan variables #
table(df$DATAPLAN, df$DATAGB)

# Splitting the data set #
train = sample(1:5000, size = 2500, replace = FALSE)
df_train = df[train, ]
df_test = df[-train, ]

# Regression equation #
fm = CHURN ~ ACLENGTH + INTPLAN + DATAPLAN + OMMIN + OMCALL + 
  OTMIN + OTCALL + NGMIN + NGCALL + IMIN + ICALL + CUSCALL
mod = lm(formula = fm, data = df_train) 
summary(mod)

# Evaluation in the training set (cutoff = 0.19) #
pred_train = predict(mod, newdata = df_train)
conf_train = table(pred_train > 0.19, df_train$CHURN)
conf_train
acc_train = sum(diag(conf_train))/sum(conf_train)
tp_train = conf_train['TRUE', '1']/sum(conf_train[, '1'])
tp_train
fp_train = conf_train['TRUE', '0']/sum(conf_train[, '0'])
fp_train

# Evaluation in the test set(cutoff = 0.19) #
pred_test = predict(mod, newdata = df_test)
conf_test = table(pred_test > 0.19, df_test$CHURN)
conf_test
acc_test = sum(diag(conf_test))/sum(conf_test)
acc_test
tp_test = conf_test['TRUE', '1']/sum(conf_test[, '1'])
tp_test
fp_test = conf_test['TRUE', '0']/sum(conf_test[, '0'])
fp_test
