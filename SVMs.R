setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Model-Comparison')
require(ggplot2)
list.files('data')
df <- read.csv('data/df.csv')
head(df)
# data visualization
png('df.png')
ggplot(data = df,aes(x=X,y=Y,color=Label))+geom_point()
dev.off()

# model test
logit.fit <- glm(Label~.,family=binomial(link='logit'),data=df)
logit.prediction <- ifelse(predict(logit.fit)>0,1,0)
mean(with(df,logit.prediction==Label))
mean(with(df,0==Label))

# SVMs
library(e1071)
svm.fit <- svm(Label~.,data=df)
svm.predictions <- ifelse(predict(svm.fit)>0,1,0)
mean(with(df,svm.predictions==Label))

# plot predictions
df <- cbind(df,data.frame(Logit=ifelse(predict(logit.fit)>0,1,0),SVM=ifelse(predict(svm.fit)>0,1,0)))
head(df)
require(reshape2)
predictions <- melt(df,id.vars=c('X','Y'))
head(predictions)
png('predictions.png',height=800,width=600)
ggplot(predictions, aes(X,Y, color=factor(value)))+geom_point()+facet_grid(variable~.)
dev.off()

# Kernel trick - linear, polynomial, radial, sigmoid