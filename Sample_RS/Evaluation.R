# load required libraries
libs <- c("recosystem", "lsa", "proxy", "rjson", "tm", "dplyr", "magrittr", 
          "data.table", "wordcloud", "RColorBrewer", "recosystem", "wordcloud")
sapply(libs, require, character.only = T)
setwd("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Sem2\\Text Analytics\\CA\\")
load("Final\\reviews_bizname2.RData")
load("Final\\dt.RData")

# CB model Evaluation using top nCBEval users that have most number of reviews
set.seed(1000)
nCBEval <- 100
EvaluateCBUsers <- reviews_bizname2 %>% group_by(user_id) %>% summarise(count = n()) %>% arrange(-count) %>% head(nCBEval) %$% user_id
EvaluateCB <- reviews_bizname2 %>% dplyr::filter(user_id %in% EvaluateCBUsers)
cbTrainInd <- sample(seq(1:nrow(EvaluateCB)), size = nrow(EvaluateCB)*0.9)
cbTestInd <- seq(1:nrow(EvaluateCB))[!(1:nrow(EvaluateCB)) %in% cbTrainInd]

cb <- EvaluateCB
categories <- gsub(" ", "-",cb$categories)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = cb$name, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))
catsMat <- as.matrix(corpusCatdtm)
catsMat <- catsMat[, -which(colnames(catsMat) %in% c("restaurants", "food"))]

cbTrain <- catsMat[cbTrainInd, ]
cbTest <- catsMat[cbTestInd, ]

temp1cb <- as.data.table(apply(cbTrain, 2, function(x) x*cb[cbTrainInd, ]$stars))
temp1cbSums <- colSums(temp1cb)
temp1cbNorm <- temp1cbSums/sum(temp1cbSums)

# use test dataset restaurants as cuisine universe so that we can measure error later
cbTest1 <- cbind(cbTest, cb[cbTestInd, ])
temp2cb <- cbTest1[, 1:(ncol(cbTest1)-ncol(cb))] * temp1cbNorm
cbReco <- data.table(business_id = cbTest1$business_id , scores = apply(temp2cb, 1, sum)) %>% distinct %>% arrange(-scores)

#normalize the predicted scores to force output to be between 1 to 5
cbReco$Norm1to5 <- ((cbReco$scores-min(cbReco$scores))/(max(cbReco$scores)-min(cbReco$scores)))*4 + 1
cbEvaltemp <- merge(cbReco, select(cb, business_id, stars), by.x = "business_id", by.y = "business_id", all.x = T)
cbEvalFinal <- cbEvaltemp %>% group_by(business_id) %>% summarise(Pred = mean(Norm1to5), Actual = mean(stars)) %>% as.data.table
RMSE.CB <- sqrt(mean(cbEvalFinal$Pred - cbEvalFinal$Actual)^2)
MAE.CB <- mean(abs(cbEvalFinal$Pred - cbEvalFinal$Actual))



# CF Evaluation
ratingsMatrixTrain <- reviews_bizname2 %>% select(user = user_id, item = business_id, ratings = stars, city = city, address = address, type = categories)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)

set.seed(1000)
trainInd <- sample(1:nrow(ratingsMatrixTrain), size = 0.9*nrow(ratingsMatrixTrain))
testInd <- (1:nrow(ratingsMatrixTrain))[!(1:nrow(ratingsMatrixTrain)) %in% trainInd]
trainset <- ratingsMatrixTrain[trainInd, ]
testset <- ratingsMatrixTrain[testInd, ]

train_data <- data_memory(user_index = trainset$user, item_index = trainset$item, rating = trainset$ratings, index1= TRUE)
test_data <- data_memory(user_index = testset$user, item_index = testset$item, rating = testset$ratings, index1= TRUE)

reco <- Reco()
reco$train(train_data, opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 30))
test_set = data_memory(testset$user, testset$item)
testset$pred <- reco$predict(test_set, out_memory())
testset$pred2 <- ((testset$pred-min(testset$pred))/(max(testset$pred)-min(testset$pred)))*4+1
RMSE.CF <- sqrt(mean(testset$pred2 - testset$ratings)^2)
MAE.CF <- mean(abs(testset$pred2 - testset$ratings))



# Final CB and CF weights
errorMat <- data.table(RMSE.CB = RMSE.CB, RMSE.CF = RMSE.CF, MAE.CB = MAE.CB, MAE.CF = MAE.CF)
cfWeight = 1-(errorMat$RMSE.CF/sum(c(errorMat$RMSE.CB, errorMat$RMSE.CF)))
cbWeight = 1-(errorMat$RMSE.CB/sum(c(errorMat$RMSE.CB, errorMat$RMSE.CF)))
