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
# Stochastic Gradient Boosting 

# 23437 samples
#  4124 predictor
#     2 classes: 'NEG', 'POS' 

# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 21093, 21093, 21093, 21093, 21094, 21093, ... 
# Resampling results across tuning parameters:

#   interaction.depth  n.trees  ROC        Sens       Spec     
#   1                   50      0.8171379  0.9988351  0.1127692
#   1                  100      0.8538054  0.9984899  0.1127692
#   1                  150      0.8621542  0.9983173  0.1129231
#   2                   50      0.8183856  0.9980585  0.1127692
#   2                  100      0.8383358  0.9977565  0.1164615
#   2                  150      0.8459335  0.9976702  0.1241538
#   3                   50      0.8421022  0.9978428  0.1163077
#   3                  100      0.8549894  0.9973682  0.1124615
#   3                  150      0.8526889  0.9972388  0.1320000

# Tuning parameter 'shrinkage' was held constant at a value of 0.1

# Tuning parameter 'n.minobsinnode' was held constant at a value of 10
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees =
#  150, interaction.depth = 1, shrinkage = 0.1 and n.minobsinnode = 10.

# Call:
# roc.default(response = test$LABEL_STR, predictor = sgbProbs$POS,     levels = rev(levels(test$LABEL_STR)))

# Data: sgbProbs$POS in 94 controls (test$LABEL_STR POS) > 7516 cases (test$LABEL_STR NEG).

sgbROC
#Area under the curve: 0.6606
print(table(train$LABEL_STR))
print(table(test$LABEL_STR))
print(head(sgbProbs))
sgbDF=data.frame("id"=test$id, "PredictedLabels"=sgbProbsConfusion, sgbProbs, "ActualLabels"=test$LABEL_STR, "TrueLabels01"=test$label)
head(sgbDF)
write.csv(x = sgbDF, file="/work/data/sbgDF_sept9_2018.csv", append = FALSE)
predicted = sgbDF$PredictedLabels
actual = test$LABEL_STR
mat <- confusionMatrix(predicted, actual, positive="POS")
mat$table
#           Reference
# Prediction  NEG  POS
#        NEG 7504   93
#        POS   12    1
mat$overall
#       Accuracy          Kappa  AccuracyLower  AccuracyUpper 
#   9.862024e-01   1.573736e-02   9.833214e-01   9.887014e-01 
#   AccuracyNull AccuracyPValue  McnemarPValue 
#   9.876478e-01   8.824259e-01   5.847217e-15
mat$byClass
#          Sensitivity          Specificity       Pos Pred Value 
#          0.010638298          0.998403406          0.076923077 
#       Neg Pred Value            Precision               Recall 
#          0.987758326          0.076923077          0.010638298 
#                   F1           Prevalence       Detection Rate 
#          0.018691589          0.012352168          0.000131406 
# Detection Prevalence    Balanced Accuracy 
#          0.001708279          0.504520852 
# making sure no changes

wes_palettes <- list(
  BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
  BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E"),
  Rushmore1 = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B"),
  Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089"),
  Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
  Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
  Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
  Chevalier1 = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C"),
  FantasticFox1 = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20"),
  Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A"),
  Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F"),
  Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
  Cavalcanti1 = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"),
  GrandBudapest1 = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236"),
  GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4")
)

# function for optimal cut
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x - 0)^2 + (y-1)^2
      ind = which(d == min(d))
      c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
        cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
  }

temp.pred = ROCR::prediction(sgbDF$POS, sgbDF$TrueLabels01)
temp.perf = ROCR::performance(temp.pred, "prec", "rec")
namee = paste0("N = ", nrow(sgbDF))
l = labs(subtitle=namee)
temp.pr <- tibble(ppv = temp.perf@y.values[[1]],
                  rec = temp.perf@x.values[[1]])
library(ggplot2)
p1 <- ggplot(temp.pr, aes(rec, ppv)) +
 # geom_vline(xintercept = 1, col="white") +
  geom_hline(yintercept = 1, col="white")  +
  geom_line(col=wes_palettes$Royal1[1]) +geom_point(alpha = 0.2,col=wes_palettes$Royal1[1], pch=21) +
  theme_minimal() + ggtitle("SGB Precision-Recall Curve")+ l#+ labs(subtitle="N = 5168156")
p1

#formula
temp.pr <- tibble(ppv = temp.perf@y.values[[1]],
                  rec = temp.perf@x.values[[1]])

f1_perf = ROCR::performance(temp.pred, "f")
perf = ROCR::performance(temp.pred, "prec", "rec")

plot.prc <- tibble(ppv.raw = perf@y.values[[1]],
                   recall = perf@x.values[[1]],
                   cutoff = perf@alpha.values[[1]]) %>%
  #arrange(desc(recall)) %>%
  arrange(cutoff) %>%
  ## interpolated PR curve
  mutate(precision = cummax(ppv.raw)) %>%
  arrange(desc(cutoff))
(plot.prc$ppv.raw)

plot.prc %>% filter(ppv.raw > 0.6)
perf@x.name
summary(unlist(perf@x.values))
#[1] "Recall"
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0000  0.1489  0.2234  0.2526  0.3723  1.0000 

perf@y.name
summary(na.omit(unlist(perf@y.values)))
# [1] "Precision"
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.03529 0.04015 0.04890 0.06051 0.16667
f1_perf@y.name
summary(na.omit(unlist(f1_perf@y.values)))
#[1] "Precision-Recall F measure"
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.06286 0.06730 0.06791 0.07636 0.09489
## as you can see, SGB did not perform great in this case!!
# our logreg baseline models did better XP
####
### NOW PREPARING FOR NEXT TREE BASED METHOD

table(test$LABEL_STR)
table(train$LABEL_STR)
test$LABEL_STR <- as.factor(ifelse(test$label == 1, 'POS','NEG'))
table(test$LABEL_STR)


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

  XGB = train(formula, data = LabeledTermsTrain, 
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

predictionsXGB = predict( XGB,LabeledTermsTest,type = 'prob')  
XGBConfusionpredictions = predict( XGB,LabeledTermsTest,type = 'raw')  

 # Save the solution 
solXGB <- data.frame('id' = test$id, predictionsXGB, 'LABEL_STR' = test$LABEL_STR) 
xgboostROC <- roc(predictor = predictionsXGB$POS, 
response = test$LABEL_STR, 
levels = rev(levels(test$LABEL_STR))) 
write.csv(solXGB, "/work/data/solXGB_FINAL.csv") 
