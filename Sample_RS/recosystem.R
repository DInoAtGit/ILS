libs <- c("recosystem", "lsa", "proxy", "rjson", "tm", "dplyr", "magrittr", 
          "data.table", "wordcloud", "RColorBrewer", "recosystem", "wordcloud")
sapply(libs, require, character.only = T)
setwd("C:\\Dino\\Git\\ILS\\ILS\\Sample_RS\\System")
load("NLP3.RData")
load("reviews_bizname2.RData")
bizname <- fread(file = "Full\\yelp_business.csv")
bizname <- bizname %>% dplyr::filter(business_id %in% unique(reviews_bizname2$business_id)) %>% as.data.table


#### NO NEED TO RUN, THESE HAVE BEEN PREPPED... TO LOOK AT THE MODELS RECOMMENDATION RUN FROM LINE 187
# data preparation
biz <- fread(file = "Full\\yelp_review.csv")
biz %<>% select(-useful, -funny, -cool)
bizname <- fread(file = "Full\\yelp_business.csv")
bizname %<>% select(business_id, name, address, state, city, review_count, categories)

reviews_bizname <- merge(biz, bizname, by= "business_id")
reviews_bizname$name <- gsub("\"", "", reviews_bizname$name)
reviews_bizname %>% group_by(state) %>% summarise(count = n()) %>% arrange(-count)
reviews_bizname %<>% dplyr::filter(state == "NV")

# generate feature matrix
containsRestaurant <- grepl("Restaurant" , reviews_bizname$categories)
reviews_bizname <- reviews_bizname[containsRestaurant, ]
reviews_bizname$text <- as.character(reviews_bizname$text)

reviews_bizname2 <- as.data.table(reviews_bizname)
set.seed(1000)
names <- sample(unique(reviews_bizname2$business_id), size = 2000)
reviews_bizname2 %<>% dplyr::filter(business_id %in% names) 
colnames(reviews_bizname2)
dim(reviews_bizname2)


# Do text analytics, get implicit features of each restaurant
sapply(nlpPackages(), require, character.only = T)
mystopwords <- c("be", stopwords(), "go", "")
initCoreNLP(type = "english_fast")
getVerbsNouns = function (x) {
  tok <- getToken(annotateString(x))
  lem <- tok %>% dplyr::filter(substr(POS,1,1) %in% c("N", "V")) %$% lemma
  return(lem)
}

res <- data.table()
ind <- unique(reviews_bizname2$business_id)
saveNum <- seq(from=20, to=2000, by=20)
for (i in 1:2000) {
  temp <- reviews_bizname2 %>% dplyr::filter(business_id == ind[i]) 
  
  allPOS <- c()
  ind2 <- nrow(temp)
  for (j in 1:ind2) {
    nlpPOS <- try(getVerbsNouns(temp$text[j]))
    allPOS <- c(allPOS, nlpPOS)
  }
  allPOS2 <- removeWords(allPOS, words = mystopwords)
  allPOS3 <- tolower(paste(unique(allPOS2), collapse = ' '))
  
  tempOut <- data.table(restaurant = ind[i], NLP_data = allPOS3)
  res <- rbind(res, tempOut)
  
  if (i %in% saveNum) {
    save(res, file = "NLP3.RData")
  }
  
  print(sprintf("%s out of 2000 done", i))
}
res$NLP_data <- iconv(res$NLP_data, "UTF-8", "ASCII", sub = '')
res$NLP_data <- gsub("[\\/]", "", res$NLP_data)


# dt is the final document-term matrix of res, after all sparsity reduction is done
corpus <- Corpus(VectorSource(res$NLP_data))
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightBin))
dtm_slim <- removeSparseTerms(dtm, 0.995)
dtm_slim2 <- as.matrix(dtm_slim)
dt <- as.data.table(as.matrix(dtm_slim2))
dt <- cbind(bizname = unique(res$restaurant), dt)
save(dt, file = "Final\\dt.RData")


# Cuisine Data Exploration --> we can outut some possible choices that user can choose from if we building UI? 
categories <- gsub(" ", "-",reviews_bizname2$categories)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = reviews_bizname2$name, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightTfIdf))
catsMat <- as.matrix(corpusCatdtm)
cats <- colnames(catsMat)
sort(colSums(catsMat), decreasing = T) %>% head
wordcloud(cats, colSums(catsMat), colors=dark2 <- brewer.pal(6, "Dark2"), random.order = F, max.words = 100)




#### START HERE ####
# recommender script starts here
libs <- c("recosystem", "lsa", "proxy", "rjson", "tm", "dplyr", "magrittr", 
          "data.table", "wordcloud", "RColorBrewer", "recosystem", "wordcloud")
sapply(libs, require, character.only = T)
load("Final\\reviews_bizname2.RData")
load("Final\\dt.RData")
topNRes <- 500
serendipityNumber = 5

# new user will input a cuisine and some keywords
cuisine <- "thai" 
keywords <- c("rice","pineapple", "fishcake", "seafood", "green", "friendly") %>% tolower

# content based recommendation
#create a new binary matrix to filter out relevant restaurants based on user-input keywords
newMat <- data.table("NEW USER INPUT" , matrix(0, nrow = 1, ncol = ncol(dt)-1)) 
newMat[, which(colnames(dt) %in% keywords)] <- 1
colnames(newMat) <- colnames(dt)
newMat <- as.data.frame(newMat)[, keywords]


# calculate based on cosine distance, output top 300 closest restaurants 
distances <- c(dist(newMat, as.data.frame(dt)[, keywords], method = "cosine"))
choiceUniverse <- data.table(bizname = dt$bizname ,dist = distances) %>% arrange(dist) %>% as.data.table %>% head(topNRes)


# reviews_bizname2 is the data table by reviews
# I filter only the restaurants of which the categories column consists of the cuisine that user input above
resSubset <- reviews_bizname2 %>% as.data.table
resSubset2 <- resSubset[grepl(tolower(cuisine), tolower(resSubset$categories)),]


# from the restaurants above, i find which restaurants exist in both the names filtered by keywords and by cuisine
# this set of restaurants will most closely match the cuisine that user wants, and the other (keywords) that they wish to see in the restaurant
resSubset2.1 <- resSubset2 %>% dplyr::filter(business_id %in% choiceUniverse$bizname) %>% as.data.table

# I introduce 5 restaurants that MIGHT NOT be from the cuisine that user input from the restaurant list generated using the keywords only
serendipity <- resSubset %>% dplyr::filter(business_id %in% choiceUniverse$bizname, !business_id %in% resSubset2.1$business_id, !business_id %in% resSubset2$business_id) %$% business_id %>% unique
serendipity_restaurants <- resSubset %>% dplyr::filter(business_id %in% sample(serendipity, serendipityNumber))


# out of the existing users in the data that we have, I find one that is an expert based on the number of restaurants that he has reviewed
# if keyword inputs are nonsensical, there will be very few outputs that are intersecting between keywords restaurants and cuisine restaurants
# if there are less than 10 restaurants in the universe, we fall back to just using the cuisine input and we ignore the keywords input
# resSubset3 is the final reviews data that satisfies both keywords and cuisine
# although resSubset3 includes the serendipity restaurants now, the closest user will still only be taken from either resSubset2 or resSubset2.1
if (length(unique(resSubset2.1$business_id)) >= 30) {
  resSubset3 <- rbind(resSubset2.1, serendipity_restaurants)
  closestUser <- resSubset2.1 %>% group_by(user_id) %>% summarise(count = n()) %>% arrange(-count) %>% head(3) %$% user_id
  closestUserCF <- closestUser[1] 
} else {
  resSubset3 <- rbind(resSubset2, serendipity_restaurants)
  closestUser <- resSubset2 %>% group_by(user_id) %>% summarise(count = n()) %>% arrange(-count) %>% head(3) %$% user_id
  closestUserCF <- closestUser[1] 
}


# from resSubset3, I find which restaurants were rated by the representative user. This is cb.
cb <- resSubset3 %>% dplyr::filter(user_id %in% closestUser)
categories <- gsub(" ", "-",cb$categories)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = cb$name, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))

# I then create a DTM and convert all individual category words into columns features with binary weighting. This is catsMat
# I use binary weighting because I am basically just interested in whether this restuarant has this feature (word)
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))
catsMat <- as.matrix(corpusCatdtm)

# I then clean the categories column of catsMat, and remove the words 'restaurant' and the input cuisine because these columns will all have values '1'
catsMat <- catsMat[, -which(colnames(catsMat) %in% c("restaurants", cuisine, "food"))]


# I multiply row-wise the scores with what the representative user has rated them
# Afterwhich i take the sum of the columns, which gives me which gives me the implicit score that the user has given to each feature
temp1cb <- as.data.table(apply(catsMat, 2, function(x) x*cb$stars))
temp1cbSums <- colSums(temp1cb)

# I normalize these numbers and get temp1cbNorm
temp1cbNorm <- temp1cbSums/sum(temp1cbSums)

# I create my cuisine universe of restaurants from resSubset3
# These restaurants all have the same column names (features of the restaurants) as temp1cbNorm above
cuisineResUniverse <- resSubset3
categories <- gsub(" ", "-",cuisineResUniverse$categories)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = cuisineResUniverse$name, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))
cuisineUniverse_catsMat <- as.matrix(corpusCatdtm)
cuisineUniverse_catsMat <- cuisineUniverse_catsMat[, names(temp1cbNorm)]


# I impute the scores based on these of the cuisine universe data features by multiplying the binary feature inputs with the normalized feature scores calculated in temp1cbNorm, 
# I then sort them from largest to smallest 
temp2cb <- t(cuisineUniverse_catsMat) * temp1cbNorm
cbReco <- data.table(business_id = cuisineResUniverse$business_id , scores = apply(t(temp2cb), 1, sum)) %>% distinct %>% arrange(-scores)

# I get the relevant details like address, city and save these sorted recommendations as CBrecommended
out <- merge(cbReco, resSubset3, by.x = "business_id", by.y = "business_id", all.x = T) %>% select(Restaurant = name, PredScore = scores, City = city, Address = address) %>% arrange(-PredScore) %>% distinct()
out$Address <- gsub(pattern = "\"", "", out$Address)
CBrecommended <- out


# These are the top 10 recommendations by content based approach
head(CBrecommended, 10)


# CB model Evaluation using top nCBEval users that have most number of reviews
nCBEval <- 300
EvaluateCBUsers <- reviews_bizname2 %>% group_by(user_id) %>% summarise(count = n()) %>% arrange(-count) %>% head(nCBEval) %$% user_id
EvaluateCB <- reviews_bizname2 %>% dplyr::filter(user_id %in% EvaluateCBUsers)
cbTrainInd <- sample(seq(1:nrow(EvaluateCB)), size = nrow(EvaluateCB)*0.9)
cbTestInd <- seq(1:nrow(EvaluateCB))[!(1:nrow(EvaluateCB)) %in% cbTrainInd]
cbtrain <- EvaluateCB[cbTrainInd, ]
cbTest <- EvaluateCB[cbTrainInd, ]

cb <- cbtrain
categories <- gsub(" ", "-",cb$categories)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = cb$name, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))

catsMat <- as.matrix(corpusCatdtm)
catsMat <- catsMat[, -which(colnames(catsMat) %in% c("restaurants", "food"))]
temp1cb <- as.data.table(apply(catsMat, 2, function(x) x*cb$stars))
temp1cbSums <- colSums(temp1cb)
temp1cbNorm <- temp1cbSums/sum(temp1cbSums)

# use test dataset restaurants as cuisine universe so that we can measure error later
cuisineResUniverse <- cbTest
categories <- gsub(" ", "-",cuisineResUniverse$categories)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = cuisineResUniverse$name, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))
cuisineUniverse_catsMat <- as.matrix(corpusCatdtm)
cuisineUniverse_catsMat <- cuisineUniverse_catsMat[, names(temp1cbNorm)]

temp2cb <- t(cuisineUniverse_catsMat) * temp1cbNorm
cbReco <- data.table(business_id = cuisineResUniverse$business_id , scores = apply(t(temp2cb), 1, sum)) %>% distinct %>% arrange(-scores)

#normalize the predicted scores to force output to be between 1 to 5
cbReco$Norm1to5 <- ((cbReco$scores-min(cbReco$scores))/(max(cbReco$scores)-min(cbReco$scores)))*4 + 1
cbEvaltemp <- merge(cbReco, select(cbTest, business_id, stars), by.x = "business_id", by.y = "business_id", all.x = T)
cbEvalFinal <- cbEvaltemp %>% group_by(business_id) %>% summarise(Pred = mean(Norm1to5), Actual = mean(stars)) %>% as.data.table
RMSE.CB <- sqrt(mean(cbEvalFinal$Pred - cbEvalFinal$Actual)^2)
MAE.CB <- mean(abs(cbEvalFinal$Pred - cbEvalFinal$Actual))



# CF using ALS
# But first, i evaluate the CF model
ratingsMatrixTrain <- reviews_bizname2 %>% select(user = user_id, item = business_id, ratings = stars, city = city, address = address, type = categories)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)

# CF model evaluation
trainInd <- sample(1:nrow(ratingsMatrixTrain), size = 0.9*nrow(ratingsMatrixTrain))
testInd <- (1:nrow(ratingsMatrixTrain))[!(1:nrow(ratingsMatrixTrain)) %in% trainInd]
trainset <- ratingsMatrixTrain[trainInd, ]
testset <- ratingsMatrixTrain[testInd, ]

train_data <- data_memory(user_index = trainset$user, item_index = trainset$item, rating = trainset$ratings, index1= TRUE)
test_data <- data_memory(user_index = testset$user, item_index = testset$item, rating = testset$ratings, index1= TRUE)

reco <- Reco()
#tuned <- reco$tune(train_data = train_data, opts = c(dim = c(10, 20, 30), costp_12 = 0.1, costq_12 = 0.1, 
#                                                     lrate = c(0.1, 0.2), niter = 30))

reco$train(train_data, opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 30))
test_set = data_memory(testset$user, testset$item)
testset$pred <- reco$predict(test_set, out_memory())
testset$pred2 <- ((testset$pred-min(testset$pred))/(max(testset$pred)-min(testset$pred)))*4+1
RMSE.CF <- sqrt(mean(testset$pred2 - testset$ratings)^2)
MAE.CF <- mean(abs(testset$pred2 - testset$ratings))


# I build a CF recommendation model using recosystem library using ALS algorithm
ratingsMatrixTrain <- resSubset3 %>% select(user = user_id, item = business_id, ratings = stars, city = city, address = address, type = categories)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)
uniqUsers <- length(unique(ratingsMatrixTrain$user))
uniqItems <- length(unique(ratingsMatrixTrain$item))

reco <- Reco()
reco$train(data_memory(user_index = ratingsMatrixTrain$user, item_index = ratingsMatrixTrain$item, rating = ratingsMatrixTrain$ratings),
           opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 30))

closestUser_Index <- rep(which(levels(ratingsMatrixTrain$user) == closestUserCF), each = uniqItems)
pred <- reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
tempfinalReco <- data.table(business_id = unique(ratingsMatrixTrain$item), scores = pred)
out = merge(tempfinalReco, resSubset3, by.x = "business_id", by.y = "business_id", all.x = T) %>% select(Restaurant = name, PredScore = scores, City = city, Address = address) %>% arrange(-PredScore) %>% distinct()
out$Address <- gsub(pattern = "\"", "", out$Address)

CFrecommended <- out
head(CFrecommended,10) %>% select(Restaurant, PredScore, Address)
head(CBrecommended,10) %>% select(Restaurant, PredScore, Address)


# normalize both scores from CB and CF by rankings weighted by their RMSE
errorMat <- data.table(RMSE.CB = RMSE.CB, RMSE.CF = RMSE.CF, MAE.CB = MAE.CB, MAE.CF = MAE.CF)
cfWeight = 1-(errorMat$RMSE.CF/sum(c(errorMat$RMSE.CB, errorMat$RMSE.CF)))
cbWeight = 1-(errorMat$RMSE.CB/sum(c(errorMat$RMSE.CB, errorMat$RMSE.CF)))
CFrecommended$norm <- rank(-CFrecommended$PredScore, ties.method = "min")
CBrecommended$norm <- rank(-CBrecommended$PredScore, ties.method = "min")

# I create a new column to index the restaurants
CFrecommended$ind <- paste0(CFrecommended$Address, CFrecommended$Restaurant, CFrecommended$City)
CBrecommended$ind <- paste0(CBrecommended$Address, CBrecommended$Restaurant, CBrecommended$City)
finalcombined <- merge(CFrecommended, CBrecommended, by = "ind")

# I take the average of the normalized score for both CB model and CF model and recommend top 10 restaurants (smaller aggregated ranking is good)
finalcombined$Final.Rank <- cfWeight*finalcombined$norm.x + cbWeight*finalcombined$norm.y
finalcombined2 <- finalcombined %>% arrange(Final.Rank) %>% select(Restaurant = Restaurant.x, Final.Rank, City = City.x, Address = Address.x) %>% head(10)
finalcombined2

# check the number of top 10 in hybrid that was recommended by CF and CB individually
sum((finalcombined2 %$% Restaurant) %in% (head(CFrecommended,10) %$% Restaurant))
sum((finalcombined2 %$% Restaurant) %in% (head(CBrecommended,10) %$% Restaurant))
