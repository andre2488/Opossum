DATA=read.csv("DATA_ANDRE.csv", header = T)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(DATA))

## set the seed to make your partition reproducible
set.seed(123)
train_DATA <- sample(seq_len(nrow(DATA)), size = smp_size)

train <- DATA[train_DATA, ]
test <- DATA[-train_DATA, ]

BLIBLI_11<-glm(WMCinYear~UbicNacimientoRur_Urb+
                 YearsLivingInIQT.s+YearsInSchool.s+
                 MenoresDeEdad.s+MontlyIncomeInHouse.s+
                 ProblemsToForest+MontlyIncomeInHouse.s:YearsLivingInIQT.s+
                 ProblemsToForest:YearsInSchool.s,
               data = train,family = "binomial")
summary(BLIBLI_11)
car::vif(BLIBLI_11)
cooksd <- cooks.distance(BLIBLI_11)
sample_size <- nrow(train)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")

influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
train_screen <- train[-influential, ]

BLIBLI_11<-glm(WMCinYear~UbicNacimientoRur_Urb+YearsLivingInIQT.s+
               MenoresDeEdad.s+MontlyIncomeInHouse.s+
               ProblemsToForest+MontlyIncomeInHouse.s:YearsLivingInIQT.s,
               data = train_screen,family = binomial)
summary(BLIBLI_11)

library(ROCR)
predict <- predict(BLIBLI_11, test, type = 'response')
ROCRpred <- prediction(predict, test$WMCinYear)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))


library(caret)
test$good_pred <- as.numeric(ifelse(predict > 0.70, "1", "0"))
test$good_pred  <- as.factor(test$good_pred)
test$WMCinYear  <- as.factor(test$WMCinYear)
caret::confusionMatrix(test$good_pred, test$WMCinYear)
