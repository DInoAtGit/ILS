libs <- c("recosystem", "lsa", "proxy", "rjson", "tm", "dplyr", "magrittr", 
          "data.table", "wordcloud", "RColorBrewer", "recosystem", "wordcloud", "shiny")
sapply(libs, require, character.only = T)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("reviews_bizname2.RData")
load("dt.RData")
load("cat_sel.RData")
getwd()



calcRec <- function (c, w) {
  # parameters
  topNRes <- 500
  serendipityNumber = 5
  #  weights for recommender systems
  cfWeight = 0.7115431
  cbWeight = 0.2884539
  
  # new user will input a cuisine and some keywords
  cuisine <- c
  keywords <- w
  
  # content based recommendation
  #create a new binary matrix to filter out relevant restaurants based on user-input keywords
  newMat <-
    data.table("NEW USER INPUT" , matrix(0, nrow = 1, ncol = ncol(dt) - 1))
  newMat[, which(colnames(dt) %in% keywords)] <- 1
  colnames(newMat) <- colnames(dt)
  newMat <- as.data.frame(newMat)[, which(colnames(dt) %in% keywords)]
  
  
  # calculate based on cosine distance, output top 300 closest restaurants
  distances <-
    c(dist(newMat, as.data.frame(dt)[, which(colnames(dt) %in% keywords)], method = "cosine"))
  choiceUniverse <-
    data.table(bizname = dt$bizname , dist = distances) %>% arrange(dist) %>% as.data.table %>% head(topNRes)
  
  
  # reviews_bizname2 is the data table by reviews
  # I filter only the restaurants of which the categories column consists of the cuisine that user input above
  resSubset <- reviews_bizname2 %>% as.data.table
  resSubset$categories <- gsub(" ", "-", resSubset$categories)
  resSubset$categories <- gsub(";", " ", resSubset$categories)
  resSubset$categories <- gsub("/", " ", resSubset$categories)
  resSubset$categories <- gsub("-&-", "&", resSubset$categories)
  resSubset2 <-
    resSubset[grepl(tolower(cuisine), tolower(resSubset$categories)), ]
  
  # tempDT <- data.table(name = resSubset$name, categories = resSubset$categories)
  # catSplited <- unlist(apply(tempDT[,2], 1, function(x) strsplit(x, "\\s+")))
  # catSelection <- sort(unique(catSplited))
  # save(catSelection, file = "cat_sel.RData")
  
  # from the restaurants above, i find which restaurants exist in both the names filtered by keywords and by cuisine
  # this set of restaurants will most closely match the cuisine that user wants, and the other (keywords) that they wish to see in the restaurant
  resSubset2.1 <-
    resSubset2 %>% dplyr::filter(business_id %in% choiceUniverse$bizname) %>% as.data.table
  
  # I introduce 5 restaurants that MIGHT NOT be from the cuisine that user input from the restaurant list generated using the keywords only
  serendipity <-
    resSubset %>% dplyr::filter(
      business_id %in% choiceUniverse$bizname,
      !business_id %in% resSubset2.1$business_id,
      !business_id %in% resSubset2$business_id
    ) %$% business_id %>% unique
  serendipity_restaurants <-
    resSubset %>% dplyr::filter(business_id %in% sample(serendipity, serendipityNumber))
  
  
  # out of the existing users in the data that we have, I find one that is an expert based on the number of restaurants that he has reviewed
  # if keyword inputs are nonsensical, there will be very few outputs that are intersecting between keywords restaurants and cuisine restaurants
  # if there are less than 10 restaurants in the universe, we fall back to just using the cuisine input and we ignore the keywords input
  # resSubset3 is the final reviews data that satisfies both keywords and cuisine
  # although resSubset3 includes the serendipity restaurants now, the closest user will still only be taken from either resSubset2 or resSubset2.1
  if (length(unique(resSubset2.1$business_id)) >= 30) {
    resSubset3 <- rbind(resSubset2.1, serendipity_restaurants)
    closestUser <-
      resSubset2.1 %>% group_by(user_id) %>% summarise(count = n()) %>% arrange(-count) %>% head(3) %$% user_id
    closestUserCF <- closestUser[1]
  } else {
    resSubset3 <- rbind(resSubset2, serendipity_restaurants)
    closestUser <-
      resSubset2 %>% group_by(user_id) %>% summarise(count = n()) %>% arrange(-count) %>% head(3) %$% user_id
    closestUserCF <- closestUser[1]
  }
  
  
  # from resSubset3, I find which restaurants were rated by the representative user. This is cb.
  cb <- resSubset3 %>% dplyr::filter(user_id %in% closestUser)
  categoriesdt <-
    data.table(name = cb$name, categories = cb$categories)
  corpusCat <- Corpus(VectorSource(categoriesdt$categories))
  
  # I then create a DTM and convert all individual category words into columns features with binary weighting. This is catsMat
  # I use binary weighting because I am basically just interested in whether this restuarant has this feature (word)
  corpusCatdtm <-
    DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))
  catsMat <- as.matrix(corpusCatdtm)
  
  # I then clean the categories column of catsMat, and remove the words 'restaurant' and the input cuisine because these columns will all have values '1'
  catsMat <-
    catsMat[,-which(colnames(catsMat) %in% c("restaurants", cuisine, "food"))]
  
  
  # I multiply row-wise the scores with what the representative user has rated them
  # Afterwhich i take the sum of the columns, which gives me which gives me the implicit score that the user has given to each feature
  temp1cb <- as.data.table(apply(catsMat, 2, function(x)
    x * cb$stars))
  temp1cbSums <- colSums(temp1cb)
  
  # I normalize these numbers and get temp1cbNorm
  temp1cbNorm <- temp1cbSums / sum(temp1cbSums)
  
  # I create my cuisine universe of restaurants from resSubset3
  # These restaurants all have the same column names (features of the restaurants) as temp1cbNorm above
  cuisineResUniverse <- resSubset3
  categoriesdt <-
    data.table(name = cuisineResUniverse$name, categories = cuisineResUniverse$categories)
  corpusCat <- Corpus(VectorSource(categoriesdt$categories))
  corpusCatdtm <-
    DocumentTermMatrix(corpusCat, control = list(weighting = weightBin))
  cuisineUniverse_catsMat <- as.matrix(corpusCatdtm)
  cuisineUniverse_catsMat <-
    cuisineUniverse_catsMat[, names(temp1cbNorm)]
  
  
  # I impute the scores based on these of the cuisine universe data features by multiplying the binary feature inputs with the normalized feature scores calculated in temp1cbNorm,
  # I then sort them from largest to smallest
  temp2cb <- t(cuisineUniverse_catsMat) * temp1cbNorm
  cbReco <-
    data.table(business_id = cuisineResUniverse$business_id ,
               scores = apply(t(temp2cb), 1, sum)) %>% distinct %>% arrange(-scores)
  
  # I get the relevant details like address, city and save these sorted recommendations as CBrecommended
  out <-
    merge(cbReco,
          resSubset3,
          by.x = "business_id",
          by.y = "business_id",
          all.x = T) %>% select(
            Restaurant = name,
            PredScore = scores,
            City = city,
            Address = address
          ) %>% arrange(-PredScore) %>% distinct()
  out$Address <- gsub(pattern = "\"", "", out$Address)
  CBrecommended <- out
  
  
  # CF using ALS
  ratingsMatrixTrain <-
    resSubset3 %>% select(
      user = user_id,
      item = business_id,
      ratings = stars,
      city = city,
      address = address,
      type = categories
    )
  ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
  ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)
  uniqUsers <- length(unique(ratingsMatrixTrain$user))
  uniqItems <- length(unique(ratingsMatrixTrain$item))
  
  reco <- Reco()
  reco$train(
    data_memory(
      user_index = ratingsMatrixTrain$user,
      item_index = ratingsMatrixTrain$item,
      rating = ratingsMatrixTrain$ratings
    ),
    opts = c(
      dim = 10,
      costp_12 = 0.1,
      costq_12 = 0.1,
      lrate = 0.08,
      niter = 30
    )
  )
  
  closestUser_Index <-
    rep(which(levels(ratingsMatrixTrain$user) == closestUserCF), each = uniqItems)
  pred <-
    reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
  tempfinalReco <-
    data.table(business_id = unique(ratingsMatrixTrain$item),
               scores = pred)
  out = merge(
    tempfinalReco,
    resSubset3,
    by.x = "business_id",
    by.y = "business_id",
    all.x = T
  ) %>% select(
    Restaurant = name,
    PredScore = scores,
    City = city,
    Address = address
  ) %>% arrange(-PredScore) %>% distinct()
  out$Address <- gsub(pattern = "\"", "", out$Address)
  
  CFrecommended <- out
  
  
  # Hybrid model using error weighted average
  CFrecommended$norm <-
    rank(-CFrecommended$PredScore, ties.method = "min")
  CBrecommended$norm <-
    rank(-CBrecommended$PredScore, ties.method = "min")
  
  # I create a new column to index the restaurants
  CFrecommended$ind <-
    paste0(CFrecommended$Address,
           CFrecommended$Restaurant,
           CFrecommended$City)
  CBrecommended$ind <-
    paste0(CBrecommended$Address,
           CBrecommended$Restaurant,
           CBrecommended$City)
  finalcombined <- merge(CFrecommended, CBrecommended, by = "ind")
  
  finalcombined$Restaurant.x = iconv(finalcombined$Restaurant.x, "UTF-8", "ASCII", sub = '')
  finalcombined$Restaurant.y = iconv(finalcombined$Restaurant.y, "UTF-8", "ASCII", sub = '')
  
  # I take the average of the normalized score for both CB model and CF model and recommend top 10 restaurants (smaller aggregated ranking is good)
  finalcombined$Final.Rank <-
    cfWeight * finalcombined$norm.x + cbWeight * finalcombined$norm.y
  finalcombined2 <-
    finalcombined %>% arrange(Final.Rank) %>% select(
      Restaurant = Restaurant.x,
      City = City.x,
      Address = Address.x
      ) %>% head(10)
  return(finalcombined2)
}

# Sys.time()-now

ui <- fluidPage(
  titlePanel("Welcome to Yelp RS!"),
  fluidRow(
    column(
      5,
      selectizeInput(
        'select_category',
        h3('Cuisine..'),
        br(),
        br(),
        choices = catSelection,
        multiple = TRUE,
        options = list(maxItems = 1)
      )
    ),
    column(
      5,
      textInput(
        "text_food",
        h3("Any preferences.."),
        placeholder = "eg: outdoors, dessert, sweet foods.."
      )
    )
  ),
  fluidRow(
    column(2,
           actionButton("go", "Get restaurants!", style="color: #fff;
                        background-color: #337ab7;
                        border-color: #2e6da4"))
  ),
  mainPanel(
    br(),
    br(),
    h4("Here are the recommended restaurants for you.."),
    br(),
    tableOutput("res_table"))
)

server <- function(input, output) {
  
  # new user will input a cuisine and some keywords
  results <- reactive({
    inputWords <- tolower(strsplit(gsub("[^[:alnum:] ]", "", input$text_food), " +")[[1]])
    # Make sure requirements are met
    req(input,inputWords)
    calcRec(input$select_category, inputWords)
  })
  
  output$res_table <- renderTable({
    # if (is.null(results()))
    #   return(NULL)
    input$go
    isolate(results())
  })
}

shinyApp(ui, server)


