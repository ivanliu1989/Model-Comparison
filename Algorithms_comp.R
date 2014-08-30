setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Model-Comparison')
load('data/dtm.RData')
set.seed(1)
training.indices <- sort(sample(1:nrow(dtm), round(0.5 * nrow(dtm))))
test.indices <- which(! 1:nrow(dtm) %in% training.indices)
train.x <- dtm[training.indices, 3:ncol(dtm)]
train.y <- dtm[training.indices, 1]
test.x <- dtm[test.indices, 3:ncol(dtm)]
test.y <- dtm[test.indices, 1]
rm(dtm)

library(glmnet)
regularized.logit.fit <- glmnet(train.x, train.y, family = c('binomial'))
lambdas <- regularized.logit.fit$lambda
performance <- data.frame()
for (lambda in lambdas)
{
    predictions <- predict(regularized.logit.fit, test.x, s = lambda)
    predictions <- as.numeric(predictions > 0)
    mse <- mean(predictions != test.y)
    performance <- rbind(performance, data.frame(Lambda = lambda, MSE = mse))
}
png('Lambda.png')
ggplot(performance, aes(x = Lambda, y = MSE)) +
    geom_point() +
    scale_x_log10()
dev.off()
# best point
best.lambda <- with(performance, max(Lambda[which(MSE == min(MSE))]))
