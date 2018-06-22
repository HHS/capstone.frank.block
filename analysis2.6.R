#===============================================================================
# Current version: app2.6.1
# 
# Previous version: app2.5.2_pump_anes_defib_vent_4vs4_extra_input 
#
# Note: This app contains 4 individual xbgoost models and can make predictions on 4 device types.
# 
# New features:
#   
# Models are updated with all available data by 2017-09-27
# Add a function to process synonyms (e.g. pm -> scheduled)
#
# 2017-10-04
# Shufang Ci
#===============================================================================

# library
packages_to_use = c("quanteda", "caret", "plyr", "textstem", "xgboost")
install_load = function(packages){
  to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(to_install)){
    install.packages(to_install, repos='http://cran.us.r-project.org', dependencies = TRUE)
  }
  lapply(packages,library, character.only = TRUE)
}
install_load(packages_to_use)

set.seed(123)
devicename = "vent" # use "pump", "anes", "defib", or "vent"
foldername = devicename

# training/testing data split
train_test_split <- function(filename, prob = 0.8){
  df <- read.csv(filename)
  df <- df[sample(1:nrow(df)),] 
  inTrain <- createDataPartition(y=df$outcome, p=prob, list=F)
  training <- df[inTrain,]
  testing <- df[-inTrain,]
  strname <- strsplit(filename, "[.]")[[1]][1]
  write.csv(training, paste(strname, "_training.csv", sep=""), row.names=F)
  write.csv(testing, paste(strname, "_testing.csv", sep=""), row.names=F)
}

# data preprocessing
data_proc <- function(dataframe, pred.col=""){
  # select unique text and label related columns and merge
  if (pred.col != ""){
    dataframe <- unique(dataframe)
  }
  
  df <- dataframe[, c("wo_type_code", "wo_problem_code", 
                      "wo_problem_description", "wo_repair_description")]
  devices <- tolower(dataframe$device_type)
#   df$device[grepl("pump", devices)] = "infusionpump"
#   df$device[grepl("anes", devices)] = "anesthesiasystem"
#   df$device[grepl("defib", devices)] = "defibrillator"
#   df$device[grepl("vent", devices)] = "ventilator"
#   df <- data.frame(text = with(df, paste(device, wo_type_code, wo_problem_code, 
#                                          wo_problem_description, wo_repair_description)))
  df <- data.frame(text = with(df, paste(wo_type_code, wo_problem_code, 
                                         wo_problem_description, wo_repair_description)))

  # lemmatize, remove punctuations and numbers from the text
  df$text <- gsub("\\s*\\([^\\)]+\\)","", df$text)
  df$text <- tolower(df$text)
  df$text <- lemmatize_strings(df$text)
  df$text <- gsub("[[:punct:]]","", df$text)
  df$text <- gsub("[[:digit:]]","", df$text)
  df$text <- gsub("[^[:alpha:]///' ]", "", df$text)
  
  # Replace synonyms
  ref <- read.csv("syn_reference.csv", header=T, stringsAsFactors = F)
  find_word <- function(x, ref, text) gsub(paste('\\<', ref[x,1], '\\>', sep=''), ref[x,2], text)
  for (i in 1:nrow(ref)) {df$text = find_word(i, ref, df$text)}
  
  # construct documant term frequency matrix
  text_corpus <- corpus(df$text)
  text_dfm <- dfm(text_corpus, remove=stopwords("english"), stem=FALSE)
  
  if (pred.col == "") n_count_min = 1 else n_count_min <- round(nrow(df)/1000)
  
  text_dfm <- dfm_trim(text_dfm, min_count=n_count_min, min_docfreq=n_count_min)
  text_dfm_tfidf <- dfm_weight(text_dfm, type='tfidf')
  text_df <- convert(text_dfm_tfidf, to="data.frame")
  
  if (pred.col == "") text_df$outcome <- NA else text_df$outcome <- dataframe[, c(pred.col)]
  
  return(list(processed_text = df$text, processed_dfm = text_df))
}

train_test_split(filename=paste(foldername, "/", devicename, "_flagged.csv", sep=""))

training <- read.csv(paste(foldername, "/", devicename, "_flagged_training.csv", sep=""))
training <- data_proc(training, pred.col="outcome")$processed_dfm
inVal <- createDataPartition(y=training$outcome, p=0.2, list=F)
validating <- training[inVal,]
training <- training[-inVal,]
out_col <- c('outcome')

################## Model training ################## 
searchGrid <- expand.grid(subsample = c(0.3, 1), 
                          colsample_bytree = c(0.3, 1),
                          max_tree_depth = c(4, 8))

aucsHyperparameters <- apply(searchGrid, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentMaxTreeDepth  <- parameterList[["max_tree_depth"]]
  
  xgbcv <- xgb.cv(data=as.matrix(training[,!(names(training) %in% out_col)]), 
                  label=training$outcome,
                  booster="gbtree", eval_metric="auc", objective="binary:logistic", 
                  showsd=TRUE, verbose=TRUE,
                  eta=0.2, gamma=1, min_child_weight=1,
                  nrounds=100, early_stopping_rounds=10, nfold=3,
                  max.depth=currentMaxTreeDepth, subsample=currentSubsampleRate, colsample_bytree=currentColsampleRate)

  bestnrounds <- xgbcv$best_ntreelimit
  bestAUC <- xgbcv$evaluation_log$test_auc_mean[bestnrounds]
  return(c(bestAUC, bestnrounds, currentMaxTreeDepth, currentSubsampleRate, currentColsampleRate))
})
row.names(aucsHyperparameters) <- c("bestAUC", "bestnround", "currentMaxTreeDepth", "SubsampleRate", "ColsampleRate")
aucsHyperparameters <- t(aucsHyperparameters)

#----------------------------------- Break here: check the four parameters and continue
NRound        = 25
MaxTreeDepth  = 4
SubsampleRate = 1.0
ColsampleRate = 0.3
#----------------------------------- 

# xgboost training
fit_xgb <- xgboost(data=as.matrix(training[,!(names(training) %in% out_col)]), 
                   label=training$outcome,
                   booster="gbtree", eval_metric="auc", objective="binary:logistic",
                   eta=0.2, gamma=1, min_child_weight=1, 
                   max_depth=MaxTreeDepth, subsample=SubsampleRate, colsample_bytree=ColsampleRate,
                   nrounds=NRound)

outcome_pred <- predict(fit_xgb, as.matrix(validating[,!(names(validating) %in% out_col)]))
outcome_pred_xgb <- as.numeric(outcome_pred > 0.2)
confusionMatrix(outcome_pred_xgb, validating$outcome)

# find important features from xgboost
imp_feature <- xgb.importance(names(training), fit_xgb)
write.csv(imp_feature, paste(foldername, "/imp_feature.csv", sep=''), row.names=F)
#xgb.plot.tree(model=fit_xgb)

saveRDS(fit_xgb, paste(foldername, "/xgboost_", devicename, ".rds", sep=""))
fit_xgb <- readRDS(paste(foldername, "/xgboost_", devicename, ".rds", sep=""))

################## Prediction ################## 
fit <- fit_xgb

training_names <- colnames(training)
write.csv(training_names, paste(foldername, "/training_names.csv", sep=''), row.names=F)
training_names <- read.csv(paste(foldername, "/training_names.csv", sep=''))
training_names <- training_names$x

# testing data processing
testing <- read.csv(paste(foldername, "/", devicename, "_flagged_testing.csv", sep=""))

test_true <- testing$outcome
testing$outcome <- NULL
testing <- data_proc(testing, pred.col="")$processed_dfm
testing_values <- data.frame(testing[,intersect(colnames(testing), training_names)])
testing_names <- read.table(textConnection(""), col.names=training_names, colClasses="integer")

testing <- rbind.fill(testing_names, testing_values)
testing[is.na(testing)] <- 0
names(testing) <- gsub("[.]", "", names(testing)) 

# prediction on the test data
test_pred <- predict(fit, as.matrix(testing), type='class')
test_pred <- as.numeric(test_pred > 0.2)
confusionMatrix(test_pred, test_true)

################## Find new words ################## 
word_bag <- read.csv(paste(foldername, "/", devicename, "_flagged.csv", sep=""))
word_bag <- data_proc(word_bag, pred.col='')$processed_text
word_bag <- unique(unlist(lapply(word_bag, strsplit, " ")))
saveRDS(word_bag, paste(foldername, "/word_bag_", devicename, ".rds", sep=""))
word_bag <- readRDS(paste(foldername, "/word_bag_", devicename, ".rds", sep=""))

word_bag <- c()
for (devicename in c("pump", "anes", "defib", "vent")){
  bag <- readRDS(paste(devicename, "/word_bag_", devicename, ".rds", sep=""))
  word_bag <- c(word_bag, bag)
}
word_bag <- unique(word_bag)
saveRDS(word_bag, "word_bag_4devices.rds")

new_word <- read.csv("Q1 2017 WA data.csv")
new_word <- data_proc(new_word, pred.col='')$processed_text
# new_word <- unique(unlist(lapply(new_word, strsplit, " ")))
# new_word[!new_word %in% word_bag]
find_new <- function(new_string, old_word_bag){
  new_string <- unlist(lapply(new_string, strsplit, " ")) 
  new_string[!new_string %in% old_word_bag]
}
data.frame(table(unlist(lapply(new_word, find_new, word_bag))))
