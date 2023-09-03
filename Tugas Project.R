#Memahami kebutuhan bisnis

df <- read.csv('mobil_mesin_harga.csv') # memasukkan data ke dalam RStudio
View(df) 

#Memahami data 
summary(df) # melihat summary dari data 

plot(df) # Membuat grafik dari data yang ada dengan memplotkan pada sumbu x dan y 

library(corrgram)
corrgram(df,upper.panel = panel.pie) #melihat korelasi antara kekuatan mesin dan harga 

#Menyiapkan data 
sapply(df, function(x)sum(is.na(x))) #memastikan bahwa data tidak ada yang NA(NOt Available)

library(caTools)
set.seed(123) # membuat random sampling 

split = sample.split(df$Harga,SplitRatio = 0.8) #memisahkan data dengan 
training_set = subset(df,split == TRUE)         #training data 80% dan test data 20%
test_set = subset(df,split == FALSE)

#Membuat Model Linear Regresion 
regressor = lm(formula = Harga ~ KekuatanMesin,
               data = training_set)

y_pred = predict(regressor,newdata = test_set)

show_prediction = cbind(test_set,y_pred)

View(show_prediction)

#Evaluasi dengan Mean Absolut Eror 
MAPE = function(y_actual,y_predict){
  mean(abs((y_actual-y_predict)/y_actual))*100
}
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
LR_MAPE = MAPE(test_set[,2],y_pred) # Using MAPE error metrics to check for the error rate and accuracy level
LR_R = RSQUARE(test_set[,2],y_pred) # Using R-SQUARE error metrics to check for the error rate and accuracy level
Accuracy_Linear = 100 - LR_MAPE

print("Mean Absolute Percentil Eror: ")
LR_MAPE
print("R-Square: ")
print(LR_R)
print('Accuracy of Linear Regression: ')
print(Accuracy_Linear)

summary(regressor)$r.squared 
