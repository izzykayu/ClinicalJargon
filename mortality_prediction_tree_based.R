setwd("/work")
library(packrat)
on()

library(readr)
library(tidyverse)
library(tm)
library(tidytext)
library(NLP)
# this dataframe contains the notes as well as engineered features
df <- read_csv("/work/data/DF_ZEROSTART_moreFESept2.csv")
trim <- function (x) gsub('^\\s+|\\s+$', '', x)   # function to trim spaces of columns
multi.fun <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)} 

nums <- sapply(df, is.numeric)
numericdf <- df[,c(nums)]
names(numericdf)
summary(numericdf)
# [1] "id"                    "NOTE_CSN_ID"          
# [3] "NOTE_ID"               "PAT_ENC_CSN_ID"       
# [5] "PAT_MRN_ID"            "label"                
# [7] "FILE_TIME_DAY"         "FILE_TIME_MONTH"      
# [9] "FILE_TIME_WDAY"        "FILE_TIME_WEEK"       
# [11] "FILE_TIME_HOUR"        "N_Negative"           
# [13] "N_Positive"            "N_Anticipation"       
# [15] "N_Trust"               "N_Fear"               
# [17] "N_Sadness"             "N_Joy"                
# [19] "N_Disgust"             "N_Anger"              
# [21] "sentiment"             "len"                  
# [23] "qmark"                 "SentimentScores"      
# [25] "Ncommas"               "Nsemicolumns"         
# [27] "Ncolons"               "Nblank"               
# [29] "Nother"                "Ncapitalfirst"        
# [31] "Ncapital"              "Nnumber"              
# [33] "Nwords"                "Nunique"              
# [35] "Nrepeated"             "UniquenessRatio"      
# [37] "Wmean"                 "Wmedian"              
# [39] "Wsd"                   "char_count"           
# [41] "word_count"            "word_density"         
# [43] "punctuation_count"     "title_word_count"     
# [45] "upper_case_word_count" "stopword_count"       
# [47] "lexical_diversity"     "count_numeric"        
# [49] "num_periods"           "num_quotas"           
# [51] "noun_count"            "verb_count"           
# [53] "adj_count"             "adv_count"            
# [55] "pron_count"            "link_counts"

makeFeatures <- function(df) {
  
  labeledTerms = makeDTM(df,tfidfFlag = 0)
  
  ## Preparing the features for the XGBoost Model
  
  features <- colnames(labeledTerms)
  
  for (f in features) {
    if ((class(labeledTerms[[f]])=="factor") || (class(labeledTerms[[f]])=="character")) {
      levels <- unique(labeledTerms[[f]])
      labeledTerms[[f]] <- as.numeric(factor(labeledTerms[[f]], levels=levels))
    }
  }
  
  return(labeledTerms)
}

train <- df %>% filter(TVT_GROUP=='TRAIN')
labeledTerms = makeFeatures(train)
head(labeledTerms)
test <- df %>% filter(TVT_GROUP=='VAL')
labeledTermsTest <- makeFeatures(test)
head(labeledTermsTest)

labeledTerms$N_Negative <- train$N_Negative
labeledTerms$N_Positive <- train$N_Positive
labeledTerms$N_Anticipation <- train$N_Anticipation
labeledTerms$N_Trust <- train$N_Trust
labeledTerms$N_fear <- train$N_Fear 
labeledTerms$N_Sadness<- train$N_Sadness 
labeledTerms$N_Joy <- train$N_Joy 
labeledTerms$N_Disgust <- train$N_Disgust
labeledTerms$N_Anger <- train$N_Anger
labeledTerms$SentimentScore <- train$sentiment
labeledTerms$len <- train$len

labeledTermsTest$N_Negative <- test$N_Negative
labeledTermsTest$N_Positive <- test$N_Positive
labeledTermsTest$N_Anticipation <- test$N_Anticipation
labeledTermsTest$N_Trust <- test$N_Trust
labeledTermsTest$N_fear <- test$N_Fear 
labeledTermsTest$N_Sadness<- test$N_Sadness 
labeledTermsTest$N_Joy <- test$N_Joy 
labeledTermsTest$N_Disgust <- test$N_Disgust
labeledTermsTest$N_Anger <- test$N_Anger
labeledTermsTest$SentimentScore <- test$sentiment
labeledTermsTest$len <- test$len

labeledTerms$N_qmark <- train$qmark
labeledTerms$N_commas <- train$Ncommas
labeledTerms$N_semicolons <- train$Nsemicolumns
labeledTerms$N_colons <- train$Ncolons
labeledTerms$N_blank <- train$Nblank
labeledTerms$N_other<- train$Nother 
labeledTerms$N_capitalfirst <- train$Ncapitalfirst
labeledTerms$N_capital <- train$Ncapital
labeledTerms$N_num <- train$Nnumber
labeledTerms$N_words<- train$Nwords
labeledTerms$N_unique<- train$Nunique

# now test part
labeledTermsTest$N_qmark <- test$qmark
labeledTermsTest$N_commas <- test$Ncommas
labeledTermsTest$N_semicolons <- test$Nsemicolumns
labeledTermsTest$N_colons <- test$Ncolons
labeledTermsTest$N_blank <- test$Nblank
labeledTermsTest$N_other<- test$Nother 
labeledTermsTest$N_capitalfirst <- test$Ncapitalfirst
labeledTermsTest$N_capital <- test$Ncapital
labeledTermsTest$N_num <- test$Nnumber
labeledTermsTest$N_words<- test$Nwords
labeledTermsTest$N_unique<- test$Nunique

labeledTerms$UniquenessRatio <- train$UniquenessRatio
labeledTerms$N_repeated <- train$Nrepeated
labeledTerms$Wmean <- train$Wmean
labeledTerms$Wmedian <- train$Wmedian
labeledTerms$Wsd <- train$Wsd
labeledTerms$N_char<- train$char_count 
labeledTerms$word_density <- train$word_density
labeledTerms$N_punct <- train$punctuation_count
labeledTerms$N_title_word <- train$title_word_count
labeledTerms$N_uppercasewords<- train$upper_case_word_count
labeledTerms$N_stopwords<- train$stopword_count

labeledTermsTest$UniquenessRatio <- test$UniquenessRatio
labeledTermsTest$N_repeated <- test$Nrepeated
labeledTermsTest$Wmean <- test$Wmean
labeledTermsTest$Wmedian <- test$Wmedian
labeledTermsTest$Wsd <- test$Wsd
labeledTermsTest$N_char<- test$char_count 
labeledTermsTest$word_density <- test$word_density
labeledTermsTest$N_punct <- test$punctuation_count
labeledTermsTest$N_title_word <- test$title_word_count
labeledTermsTest$N_uppercasewords<- test$upper_case_word_count
labeledTermsTest$N_stopwords<- test$stopword_count

labeledTerms$lexical_diversity <- train$lexical_diversity
labeledTerms$N_periods <- train$num_periods
labeledTerms$N_quotas<- train$num_quotas

labeledTerms$N_nouns <- train$noun_count
labeledTerms$N_verbs <- train$verb_count
labeledTerms$N_adj <- train$adj_count
labeledTerms$N_adv<- train$adv_count
labeledTerms$N_pron <- train$pron_count

labeledTermsTest$lexical_diversity <- test$lexical_diversity
labeledTermsTest$N_periods <- test$num_periods
labeledTermsTest$N_quotas<- test$num_quotas

labeledTermsTest$N_nouns <- test$noun_count
labeledTermsTest$N_verbs <- test$verb_count
labeledTermsTest$N_adj <- test$adj_count
labeledTermsTest$N_adv<- test$adv_count
labeledTermsTest$N_pron <- test$pron_count

dim(labeledTerms)
dim(labeledTermsTest)
# [1] 23437  4460
# [1] 7610 4708
colnamesSame = intersect(colnames(labeledTerms),colnames(labeledTermsTest))
labeledTerms = labeledTerms[ , (colnames(labeledTerms) %in% colnamesSame)]
labeledTermsTest = labeledTermsTest[ , (colnames(labeledTermsTest) %in% colnamesSame)]
dim(labeledTerms)
dim(labeledTermsTest)
## the new

print(table(train$LABEL_STR))
print(table(test$LABEL_STR))

library(gmodels)
# lucky 13
set.seed(13)
library(survival)
objControl <- trainControl(method='cv', number=10, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
model <- train(labeledTerms,train$LABEL_STR,
                                   method='gbm', # using boosting
                                   metric = "ROC",
                                   trControl=objControl)

#This is an SGB model using cv, two class summary, class probs and boosting it by using gradient boosting method, and 10 #cross validations.

importance <- as.data.frame(varImp(model)[1])
importance <- cbind(row.names(importance), Importance=importance)
row.names(importance)<-NULL
names(importance) <- c("Feature","Importance")
importance %>% arrange(desc(Importance)) %>%
  mutate(Feature=factor(Feature,levels=as.character(Feature))) %>%
  dplyr::slice(1:15) %>%
  ggplot() + geom_bar(aes(x=Feature,y=(Importance)), fill="#c0deed",stat="identity") +
  theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1, size=8),axis.ticks.x = element_blank(), legend.position = "none") +ylab("Importance") +ggtitle("Feature Importance for Detecting 2-Month Mortality from Clinical Text") + theme_minimal() + coord_flip() + labs(subtitle="Stochastic Gradient Boosting")
plot(model)

# maximum ROC statistic
max(model[["results"]][["ROC"]])  # max ROC
# maximum sensitiv statistic
max(model[["results"]][["Sens"]]) # max sensitivity
# maximum specificity statistic
max(model[["results"]][["Spec"]]) # max specificity ]
model[['results']]
library(pROC)

sgbProbs <- predict(model, labeledTerms, type = "prob")
sgbProbsConfusion <- predict(model,  labeledTerms, type = "raw")
sgbROC <-
   roc(predictor = sgbProbs$POS, response = test$LABEL_STR, levels = rev(levels(test$LABEL_STR)))

histogram(~sgbProbs$POS|test$LABEL_STR, xlab = "Probability of ", col="#7FC7AF")
model
sgbROC
print(head(sgbProbs))
solSGB <- data.frame('id' = test$id, sgbProbs, 'LABEL_STR' = test$LABEL_STR) 
write.csv(solSGB, "/work/data/solutionsStochasticGradientBoosting.csv")
# making sure no changes
table(test$LABEL_STR)
table(train$LABEL_STR)
test$LABEL_STR <- as.factor(ifelse(test$label == 1, 'POS','NEG'))
table(test$LABEL_STR)
library(e1071)
confusionMatrix(solSGB$PredictedLabels, test$LABEL_STR, positive="POS")
# Confusion Matrix and Statistics

#           Reference
# Prediction  NEG  POS
#       NEG 7504   93
#       POS   12    1
                                          
#               Accuracy : 0.9862          
#                  95% CI : (0.9833, 0.9887)
#     No Information Rate : 0.9876          
#     P-Value [Acc > NIR] : 0.8824          
                                          
#                   Kappa : 0.0157          
#  Mcnemar's Test P-Value : 5.847e-15       
                                          
#             Sensitivity : 0.0106383       
#             Specificity : 0.9984034       
#          Pos Pred Value : 0.0769231       
#          Neg Pred Value : 0.9877583       
#              Prevalence : 0.0123522       
#          Detection Rate : 0.0001314       
#   Detection Prevalence : 0.0017083       
#       Balanced Accuracy : 0.5045209       
                                          
#       'Positive' Class : POS  
## the infamous xgbboost

library(xgboost)
# creating second model
formula = LABEL_STR ~ . 
# cross validation 10 times to do less overfitting
fitControl <- trainControl(method="cv",number = 10,classProbs=TRUE, summaryFunction=mnLogLoss)

 xgbGrid <- expand.grid(nrounds = 500, 
                        max_depth = 3, 
                        eta = .05, 
                        gamma = 0, 
                        colsample_bytree = .8, 
                        min_child_weight = 1, 
                        subsample = 1) 


 set.seed(13) 

  XGB = train(formula, data = LabeledTerms, 
                  method = "xgbTree",trControl = fitControl, 
                  tuneGrid = xgbGrid,na.action = na.pass,metric="LogLoss", maximize=FALSE) 

 importance = varImp( XGB) 

 varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                             Importance = round(importance[[1]]$Overall,2)) 

 # Create a rank variable based on importance 
 rankImportance <- varImportance %>% 
   mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>% 
   head(20) 

 rankImportancefull = rankImportance 

 ggplot(rankImportancefull, aes(x = reorder(Variables, Importance),  
                            y = Importance)) +  
   geom_bar(stat='identity',colour=values[1], fill = "plum1", alpha=0.5) +  
   geom_text(aes(x = Variables, y = 1, label = Rank),  
             hjust=0, vjust=.5, size = 3, colour = values[1],  
             fontface = 'bold') +  
   labs(x = 'Feature', title = 'Relative Feature Importance in Predicting   note Class', subtitle="XGBoost Model") +  
   coord_flip() + theme_classic()  

xgbProbs <- predict(XGB, labeledTermsTest, type = "prob", na.action = na.pass)
xgbConfusion <- predict(XGB,  labeledTermsTest, type = "raw", na.action = na.pass)
 # Save the solution 
xgbROC <-
   roc(predictor = xgbProbs$POS, response = test$LABEL_STR, levels = rev(levels(test$LABEL_STR)))
 # Save the solution 
solXGB <- data.frame('id' = test$id, xgbProbs, 'PredictedLabels'=xgbProbsConfusion, 'LABEL_STR' = test$LABEL_STR) 
write.csv(solXGB, "/work/data/solXGB_FINAL.csv") 
head(solXGB)
xgbROC
#Data: xgbProbs$POS in 94 controls (test$LABEL_STR POS) > 7516 cases (test$LABEL_STR NEG).
#Area under the curve: 0.9087
# as you can see the AUC is much higher for xgboost than sgb model
library(e1071)
confusionMatrix(solXGB$PredictedLabels, test$LABEL_STR, positive="POS")
# Confusion Matrix and Statistics

#           Reference
# Prediction  NEG  POS
#       NEG 7509   85
#       POS    7    9
                                          
#               Accuracy : 0.9879          
#                  95% CI : (0.9852, 0.9902)
#     No Information Rate : 0.9876          
#     P-Value [Acc > NIR] : 0.4447          
                                          
#                   Kappa : 0.1606          
#  Mcnemar's Test P-Value : 9.923e-16       
                                          
#             Sensitivity : 0.095745        
#             Specificity : 0.999069        
#          Pos Pred Value : 0.562500        
#          Neg Pred Value : 0.988807        
#              Prevalence : 0.012352        
#          Detection Rate : 0.001183        
#   Detection Prevalence : 0.002102        
#       Balanced Accuracy : 0.547407        
                                          
#       'Positive' Class : POS   

temp.pred = ROCR::prediction(solXGB$POS, test$label)
temp.perf = ROCR::performance(temp.pred, "prec", "rec")
namee = paste0("N = ", nrow(labeledTermsTest))
l = labs(subtitle=namee)
temp.pr <- tibble(ppv = temp.perf@y.values[[1]],
                  rec = temp.perf@x.values[[1]])
library(ggplot2)
p2 <- ggplot(temp.pr, aes(rec, ppv)) +
 # geom_vline(xintercept = 1, col="white") +
  geom_hline(yintercept = 1, col="white")  +
  geom_line(col=wes_palettes$Royal1[1]) +geom_point(alpha = 0.2,col=wes_palettes$Royal1[1], pch=21) +
  theme_minimal() + ggtitle("XGBoost Precision-Recall Curve")+ l
p2
# prec-recall curve

f1_perf = ROCR::performance(temp.pred, "f")
# f1_perf

f1_perf@y.name
summary(na.omit(unlist(f1_perf@y.values)))
#[1] "Precision-Recall F measure"
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02083 0.03971 0.05882 0.08022 0.09967 0.33143
 train(formula, data=train, method="glm", family=binomial, trControl=train_control)

## MODEL GLMNET

set.seed(13)
library(caret)
objControl <- trainControl(method='cv', number=10, returnResamp='none')
# this uses the factor still
glmModel<-train(labeledTerms, train$LABEL_STR, method='glmnet', trControl=objControl)

predictionsGLM = predict(glmModel,labeledTermsTest,type = 'prob')
GLMConfusion = predict(glmModel,labeledTermsTest,type = 'raw')
# Save the solution to a dataframe
solGLM <- data.frame('id' = test$id, predictionsGLM, "PredictedLabels"=GLMConfusion, 'LABEL_STR' = test$LABEL_STR)
# Write it to file
write.csv(solGLM, '/work/data/GLMNetsept10_2018.csv')
modelglmROC <- roc(predictor = predictionsGLM$POS,
response = test$LABEL_STR,
levels = rev(levels(test$LABEL_STR)))

glmModel
modelglmROC# 
