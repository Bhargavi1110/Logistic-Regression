data <- read.csv("C://Users//JARVIS//Desktop//Bhargavi//College//Sem 4//Predictive Analysis//Logistic Regression//diabetes2.csv", header = TRUE)
data = data.frame(data)

y = data$Outcome
x1 = data$Pregnancies
x2 = data$Glucose
x3 = data$BloodPressure
x4 = data$SkinThickness
x5 = data$Insulin
x6 = data$BMI
x7 = data$DiabetesPedigreeFunction
x8 = data$Age


logreg = glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8, family = binomial(link = "logit"))
t = summary(logreg)
print("The logistic regression equation is given as: ")
print("y = -8.404 + (0.123*x1) + (0.035*x2) + (-0.013*x3) + (0.0006*x4) + (-0.0011*x5) + (0.089*x6) + (0.9451*x7) + (0.01486*x8)")
#t$coefficients
#t$coefficients[1]


#row8
#data can be taken as input also and then passed to predict
x = data.frame(x1=10, x2=115, x3=0, x4=0, x5=0, x6=35.3, x7=0.134, x8=29)
if(predict(logreg, x) > 0.5) {
  print("The person is diabetic")
}else{
  print("The person is not diabetic")
}



x1_val = seq(from=min(x1), to=max(x1), length.out=length(x1))
x2_val = seq(from=min(x2), to=max(x2), length.out=length(x2))
x3_val = seq(from=min(x3), to=max(x3), length.out=length(x3))
x4_val = seq(from=min(x4), to=max(x4), length.out=length(x4))
x5_val = seq(from=min(x5), to=max(x5), length.out=length(x5))
x6_val = seq(from=min(x6), to=max(x6), length.out=length(x6))
x7_val = seq(from=min(x7), to=max(x7), length.out=length(x7))
x8_val = seq(from=min(x8), to=max(x8), length.out=length(x8))

y_pred = -8.404 + (0.123*x1_val) + (0.035*x2_val) + (-0.013*x3_val) + (0.0006*x4_val) + (-0.0011*x5_val) + (0.089*x6_val) + (0.9451*x7_val) + (0.01486*x8_val)
y_ans = exp(y_pred)/(1+exp(y_pred))
plot.new()
par(mfrow=c(2, 4))
plot(x1_val,y_ans, col = 'Blue', type='l', xlab = 'Pregnancies', ylab = 'Diabetic')
plot(x2_val,y_ans, col = 'Red', type='l', xlab = 'Glucose', ylab = 'Diabetic')
plot(x3_val,y_ans, col = 'Green', type='l', xlab = 'BP', ylab = 'Diabetic')
plot(x4_val,y_ans, col = 'coral', type='l', xlab = 'Skin Thickness', ylab = 'Diabetic')
plot(x5_val,y_ans, col = 'orange', type='l', xlab = 'Insulin', ylab = 'Diabetic')
plot(x6_val,y_ans, col = 'Violet', type='l', xlab = 'BMI', ylab = 'Diabetic')
plot(x7_val,y_ans, col = 'antiquewhite4', type='l', xlab = 'Pedigree Function', ylab = 'Diabetic')
plot(x8_val,y_ans, col = 'Turquoise', type='l', xlab = 'Age', ylab = 'Diabetic')
