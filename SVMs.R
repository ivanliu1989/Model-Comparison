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
df <- df[,c('X','Y','Label')]
linear.svm.fit <- svm(Label~X+Y, data=df, kernel='linear')
with(df,mean(Label== ifelse(predict(linear.svm.fit)>0,1,0)))

polynomial.svm.fit <- svm(Label~X+Y, data=df, kernel='polynomial')
with(df,mean(Label== ifelse(predict(polynomial.svm.fit)>0,1,0)))

radial.svm.fit <- svm(Label~X+Y, data=df, kernel='radial')
with(df,mean(Label== ifelse(predict(radial.svm.fit)>0,1,0)))

sigmoid.svm.fit <- svm(Label~X+Y, data=df, kernel='sigmoid')
with(df,mean(Label== ifelse(predict(sigmoid.svm.fit)>0,1,0)))

df<- cbind(df,data.frame(LinearSVM = ifelse(predict(linear.svm.fit) > 0, 1, 0),
                         PolynomialSVM = ifelse(predict(polynomial.svm.fit) > 0, 1, 0),
                         RadialSVM = ifelse(predict(radial.svm.fit) > 0, 1, 0),
                         SigmoidSVM = ifelse(predict(sigmoid.svm.fit) > 0, 1, 0)))
predictions <- melt(df, id.vars = c('X','Y'))
png('kernel.png',height = 600,width=800)
ggplot(predictions, aes(X,Y,color=factor(value)))+geom_point()+facet_grid(variable~.)
dev.off()

# tuning degree hyperparameter
polynomial.degree3.svm.fit <- svm(Label~X+Y, data=df,kernel = 'polynomial', degree=3)
with(df, mean(Label != ifelse(predict(polynomial.degree3.svm.fit)>0,1,0)))

polynomial.degree5.svm.fit <- svm(Label~X+Y, data=df,kernel = 'polynomial', degree=5)
with(df, mean(Label != ifelse(predict(polynomial.degree5.svm.fit)>0,1,0)))

polynomial.degree10.svm.fit <- svm(Label~X+Y, data=df,kernel = 'polynomial', degree=10)
with(df, mean(Label != ifelse(predict(polynomial.degree10.svm.fit)>0,1,0)))

polynomial.degree12.svm.fit <- svm(Label~X+Y, data=df,kernel = 'polynomial', degree=12)
with(df, mean(Label != ifelse(predict(polynomial.degree12.svm.fit)>0,1,0)))

# plot 
df <- df[,c('X','Y','Label')]
df <- cbind(df, data.frame(Degree3SVM = ifelse(predict(polynomial.degree3.svm.fit) > 0,
                                               1,
                                               0),
                           Degree5SVM = ifelse(predict(polynomial.degree5.svm.fit) > 0,
                                               1,
                                               0),
                           Degree10SVM = ifelse(predict(polynomial.degree10.svm.fit) > 0,
                                                1,
                                                0),
                           Degree12SVM = ifelse(predict(polynomial.degree12.svm.fit) > 0,
                                                1,
                                                0)))
predictions <- melt(df,id.vars = c('X','Y'))
png('degree_hyperparameter.png',height=600,width=800)
ggplot(predictions, aes(X,Y,color=factor(value)))+geom_point()+facet_grid(variable~.)
dev.off()

# tuning cost hyperparameter
radial.cost1.svm.fit <- svm(Label ~ X + Y,
                            data = df,
                            kernel = 'radial',
                            cost = 1)
with(df, mean(Label == ifelse(predict(radial.cost1.svm.fit) > 0, 1, 0)))
radial.cost2.svm.fit <- svm(Label ~ X + Y,
                            data = df,kernel = 'radial',
                            cost = 2)
with(df, mean(Label == ifelse(predict(radial.cost2.svm.fit) > 0, 1, 0)))
radial.cost3.svm.fit <- svm(Label ~ X + Y,
                            data = df,
                            kernel = 'radial',
                            cost = 3)
with(df, mean(Label == ifelse(predict(radial.cost3.svm.fit) > 0, 1, 0)))
radial.cost4.svm.fit <- svm(Label ~ X + Y,
                            data = df,
                            kernel = 'radial',
                            cost = 4)
with(df, mean(Label == ifelse(predict(radial.cost4.svm.fit) > 0, 1, 0)))