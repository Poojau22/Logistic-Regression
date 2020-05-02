#Linear Regression
Fit =lm(ATTORNEY~ factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS, data = claimants)
summary(Fit)
plot(Fit)

#Logistic Regression
Logit =glm(ATTORNEY~ factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+
             LOSS,family='binomial', data = claimants)
summary(Logit)

#odds Ratio
exp(coef(Logit))

#Confusion Matrix Table
prob = predict(Logit,type = c("response"), claimants)
prob
confusion<-table(prob>0.5, claimants$ATTORNEY)
confusion

#Model Accuracy
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy

#ROC curve
install.packages("pROC")
library(pROC)

roccurve<-roc(claimants$ATTORNEY~prob)
plot(roccurve)

auc<-auc(claimants$ATTORNEY~prob)
auc
