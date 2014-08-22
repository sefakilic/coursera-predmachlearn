
## ----cache=TRUE----------------------------------------------------------
library(RCurl)
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
train_data <- read.csv(text=getURL(train_url), na.strings=c("", "NA"))
test_data <- read.csv(text=getURL(test_url), na.strings=c("", "NA"))


## ------------------------------------------------------------------------
train_data$X <- NULL


## ------------------------------------------------------------------------
cols_to_remove <- c("user_name", "raw_timestamp_part_1",
                    "raw_timestamp_part_2", "cvtd_timestamp")
for (col in cols_to_remove) {
    train_data[, col] <- NULL
}


## ------------------------------------------------------------------------
NAs <- apply(train_data,2,function(x) {sum(is.na(x))})
train_data <- train_data[,which(NAs == 0)]


## ----message=FALSE-------------------------------------------------------
library(caret)
nsv <- nearZeroVar(train_data)
train_data <- train_data[-nsv]
test_data <- test_data[-nsv]


## ------------------------------------------------------------------------
names(train_data)


## ----cache=TRUE----------------------------------------------------------
library(randomForest)
set.seed(1)
obs <- c()
preds <- c()
for(i in 1:10) {
    intrain = sample(1:dim(train_data)[1], size=dim(train_data)[1] * 0.8, replace=F)
    train_cross = train_data[intrain,]
    test_cross = train_data[-intrain,]
    rf <- randomForest(classe ~ ., data=train_cross)
    obs <- c(obs, test_cross$classe)
    preds <- c(preds, predict(rf, test_cross))
}


## ------------------------------------------------------------------------
conf_mat <- confusionMatrix(table(preds, obs))
conf_mat$table


## ----cache=TRUE----------------------------------------------------------
model <- randomForest(classe ~ ., data=train_data)


