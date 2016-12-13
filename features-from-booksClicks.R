##############################################################
## Script3 - Feature Creation Using Book and click rate
# We use these  features for the purpose of training KNN
#Model
##############################################################
##install.packages('dplyr')
library(dplyr)
##install.packages('class')
library(class)
##############################################################
## Configure followinng variables
##############################################################
setwd("~/Final/")
trainPath= "sampledTrain.csv"
testPath <- "sampledTest.csv"

##############################################################
## Load data
##############################################################
train = read.csv(trainPath)
test  = read.csv(testPath)
cat("loaded data in train and test \n")

##############################################################
## feature Engineering with R
##############################################################


## 1. click and book rate for hotel_cluster/srch_destionation_id
## Click and book rate of hotel_cluster per serch_estination_id
##############################################################

data1 = select(train, user_id, is_booking, srch_destination_id, hotel_cluster)
dest_cluster = group_by(train, srch_destination_id, hotel_cluster)
data.destn.clust <- summarize(dest_cluster,click=length(is_booking[is_booking==0]), book = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("srch_destination_id","hotel_cluster"))
## for test 
test <- merge(test, data.destn.clust, by=c("srch_destination_id","hotel_cluster"), all.x = TRUE)

rm(data1, dest_cluster, data.destn.clust) 

data1 = select(train, user_id, is_booking, srch_destination_id)
dest_cluster = group_by(train, srch_destination_id)
data.destn.clust <- summarize(dest_cluster,total_click=length(is_booking[is_booking==0]), total_book = sum(is_booking))
train <- merge(train, data.destn.clust, by="srch_destination_id")
test <- merge(test, data.destn.clust, by="srch_destination_id", all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

train$clickRate <- round(train$click/train$total_click,3)
train$bookRate <- round(train$book/train$total_click,3)

test$clickRate <- round(test$click/test$total_click,3)
test$bookRate <- round(test$book/test$total_click,3)

cat("Step 1: 5% \n")
## 2. click and book rate for hotel_country
## Click and book rate of hotel_cluster per (hotel country, hotel_market)
##############################################################

data1 = select(train, user_id, hotel_country, hotel_market, hotel_cluster, is_booking)
dest_cluster = group_by(train, hotel_country, hotel_market, hotel_cluster)
data.destn.clust <- summarize(dest_cluster,clickPerM=length(is_booking[is_booking==0]), bookPerM = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("hotel_country", "hotel_market","hotel_cluster"))
test <- merge(test, data.destn.clust, by=c("hotel_country", "hotel_market","hotel_cluster"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

data1 = select(train, user_id, hotel_country, hotel_market, is_booking)
dest_cluster = group_by(train, hotel_country, hotel_market)
data.destn.clust <- summarize(dest_cluster,total_clickM=length(is_booking[is_booking==0]), total_bookM = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("hotel_country", "hotel_market"))
test <- merge(test, data.destn.clust, by=c("hotel_country", "hotel_market"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

train$clickMRate <- round(train$clickPerM/train$total_clickM,3)
train$bookMRate <- round(train$bookPerM/train$total_bookM,3)

test$clickMRate <- round(test$clickPerM/test$total_clickM,3)
test$bookMRate <- round(test$bookPerM/test$total_bookM,3)

cat("step 2: 10% \n")
## 3. click and book rate for hotel_country
## Click and book rate of hotel_cluster per (hotel country)
##############################################################

data1 = select(train, hotel_country, hotel_cluster, is_booking)
dest_cluster = group_by(train, hotel_country, hotel_cluster)
data.destn.clust <- summarize(dest_cluster,clickPerHC=length(is_booking[is_booking==0]), bookPerHC = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("hotel_country", "hotel_cluster"))
test <- merge(test, data.destn.clust, by=c("hotel_country", "hotel_cluster"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

data1 = select(train, user_id, hotel_country, is_booking)
dest_cluster = group_by(train, hotel_country)
data.destn.clust <- summarize(dest_cluster,total_clickHC=length(is_booking[is_booking==0]), total_bookHC = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("hotel_country"))
test <- merge(test, data.destn.clust, by=c("hotel_country"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

train$clickHCRate <- round(train$clickPerHC/train$total_clickHC,3)
train$bookHCRate <- round(train$bookPerHC/train$total_bookHC,3)
test$clickHCRate <- round(test$clickPerHC/test$total_clickHC,3)
test$bookHCRate <- round(test$bookPerHC/test$total_bookHC,3)

cat("step3: 15%\n")
## 4. Cluster poplarity based on user
## Click and book rate of hotel_cluster per (user_location, user_location_region, user_location_city)
##############################################################

data1 = select(train, user_location_city, user_location_region, user_location_country, hotel_cluster, is_booking)
dest_cluster = group_by(train, user_location_city, user_location_region, user_location_country, hotel_cluster)
data.destn.clust <- summarize(dest_cluster,clickPerU=length(is_booking[is_booking==0]), bookPerU = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("user_location_city", "user_location_region", "user_location_country","hotel_cluster"))
test <- merge(test, data.destn.clust, by=c("user_location_city", "user_location_region", "user_location_country","hotel_cluster"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 


data1 = select(train, user_location_city, user_location_region, user_location_country, is_booking)
dest_cluster = group_by(train, user_location_city, user_location_region, user_location_country)
data.destn.clust <- summarize(dest_cluster,total_clickU=length(is_booking[is_booking==0]), total_bookU = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("user_location_city", "user_location_region", "user_location_country"))
test <- merge(test, data.destn.clust, by=c("user_location_city", "user_location_region", "user_location_country"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

train$clickURate <- round(train$clickPerU/train$total_clickU,3)
train$bookURate <- round(train$bookPerU/train$total_bookU,3)

test$clickURate <- round(test$clickPerU/test$total_clickU,3)
test$bookURate <- round(test$bookPerU/test$total_bookU,3)

cat("step4 : 20% \n")
## 5. Cluster poplarity based on user
## Click and book rate of hotel_cluster per (user_location_region, user_location_country)
##############################################################

data1 = select(train, user_location_region, user_location_country, hotel_cluster, is_booking)
dest_cluster = group_by(train, user_location_region, user_location_country, hotel_cluster)
data.destn.clust <- summarize(dest_cluster,clickPerUCR=length(is_booking[is_booking==0]), bookPerUCR = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("user_location_region", "user_location_country","hotel_cluster"))
test <- merge(test, data.destn.clust, by=c("user_location_region", "user_location_country","hotel_cluster"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 


data1 = select(train, user_location_region, user_location_country, hotel_cluster, is_booking)
dest_cluster = group_by(train, user_location_region, user_location_country)
data.destn.clust <- summarize(dest_cluster,total_clickUCR=length(is_booking[is_booking==0]), total_bookUCR = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("user_location_region", "user_location_country"))
test <- merge(test, data.destn.clust, by=c("user_location_region", "user_location_country"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

train$clickUCRRate <- round(train$clickPerUCR/train$total_clickUCR,3)
train$bookUCRRate <- round(train$bookPerUCR/train$total_bookUCR,3)

test$clickUCRRate <- round(test$clickPerUCR/test$total_clickUCR,3)
test$bookUCRRate <- round(test$bookPerUCR/test$total_bookUCR,3)

cat("step 5: 25% \n")
## 6. Cluster poplarity based on user
## Click and book rate of hotel_cluster per (user_location_country)
##############################################################

data1 = select(train, user_location_country, hotel_cluster, is_booking)
dest_cluster = group_by(train, user_location_country, hotel_cluster)
data.destn.clust <- summarize(dest_cluster,clickPerUC=length(is_booking[is_booking==0]), bookPerUC = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("user_location_country","hotel_cluster"))
test <- merge(test, data.destn.clust, by=c("user_location_country","hotel_cluster"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 


data1 = select(train, user_location_country, hotel_cluster, is_booking)
dest_cluster = group_by(train, user_location_country)
data.destn.clust <- summarize(dest_cluster,total_clickUC=length(is_booking[is_booking==0]), total_bookUC = sum(is_booking))
train <- merge(train, data.destn.clust, by=c("user_location_country"))
test <- merge(test, data.destn.clust, by=c("user_location_country"), all.x = TRUE)
rm(data1, dest_cluster, data.destn.clust) 

train$clickUCRate <- round(train$clickPerUC/train$total_clickUC,3)
train$bookUCRate <- round(train$bookPerUC/train$total_bookUC,3)

test$clickUCRate <- round(test$clickPerUC/test$total_clickUC,3)
test$bookUCRate <- round(test$bookPerUC/test$total_bookUC,3)

cat("step 6: 30%\n")

##############################################################
##                     KNN Model
##############################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KNN Data Preparation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

columnTrain = colnames(train)
selectKNN = grep('Rate', columnTrain)
train.sample = select(train, selectKNN)

columnTrain = colnames(test)
selectKNN = grep('Rate', columnTrain)
test.sample = select(test, selectKNN)

cat("data preparation for KNN \n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Handling NAs, NAN, INF, and Zero before performing KNN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handle NA : 99
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train.sample[is.na(train.sample)] <- 99
test.sample[is.na(test.sample)] <- 99

cat("handleed NA \n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handle NA : Zero
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

handleZero =  function(df) {
  colcnt = ncol(df)  
  for(i in names(df)){
    if (i < colcnt){
      df[[i]] <-  sapply(df[[i]], function(x) { if (x == 0 || x == '0') { x <- 88} else {x = x} })
    }
  }  
  return(df)
}

train.sample = handleZero(train.sample)
test.sample = handleZero(test.sample)

cat("handled Zero \n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add target column in Train and test sampel dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

train.sample = cbind(train.sample, train$hotel_cluster)
test.sample = cbind(test.sample, test$hotel_cluster)

colnames(train.sample)[13] <- "hotel_cluster"
colnames(test.sample)[13] <- "hotel_cluster"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Due to large size of the data, we will only use 60%
## to build the model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

smp_size <- floor(0.6 * nrow(train.sample))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train.sample)), size = smp_size)

train.sample <- train.sample[train_ind, ]   
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data Preparation for Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trainPredictors = train.sample[, -13]
trainTarget = train.sample[, 13]
testPredictors = test.sample[, -13]
testTarget = test.sample[, 13]

rm(train, test)
cat("added target \n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Deal with inf 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
DT <- data.table(trainPredictors)
invisible(lapply(names(DT),function(.name) set(DT, which(is.infinite(DT[[.name]])), j = .name,value = 77)))
trainPredictors = DT

DT <- data.table(testPredictors)
invisible(lapply(names(DT),function(.name) set(DT, which(is.infinite(DT[[.name]])), j = .name,value = 77)))
testPredictors = DT

cat("dealt with Inf \n")
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use Knn from the librarry class to make the prediction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prediction = knn(trainPredictors, testPredictors, trainTarget, k=5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accuracy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Accuracy = mean(prediction == testTarget)

Accuracy

cat("DONE!\n")
