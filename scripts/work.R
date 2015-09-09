#------------------------------------------------------------------------------
#
#       Load Libraries
#
#-------------------------------------------------------------------------------
library(ROCR)
library(gmodels)


#-------------------------------------------------------------------------------
#
#       set Variables 1) seed  2) sample size ratio and 3) sample density (theata) 
#
#-------------------------------------------------------------------------------

set.seed(6678)
sample_ratio <- 0.85
theta <- 0.05
pop_pos_obs_ratio <- 0.15 

#----------------------------------------------------------------------------
#
#     Load data and adjust positive obs density 
#
#----------------------------------------------------------------------------

# Load data into system 
spam <- read.csv("C:\\Users\\Patrick\\OneDrive\\R_Projects\\10_Weeks\\week_02_Spambase\\data\\spambase.data", header=FALSE)
# add column names 
cols <- read.table("C:\\Users\\Patrick\\OneDrive\\R_Projects\\10_Weeks\\week_02_Spambase\\data\\cols.txt", quote="\"", stringsAsFactors=FALSE)
colnames(spam) <- cols$V1
rm(cols)
spam$class <- factor(spam$class, labels=c("spam", "ham"))

# now cut down the positive obs to a realistic level
neg_obs_pop <- subset(spam, class=="ham")
pos_obs_pop <- subset(spam, class=="spam")
pos_obs_keep <- sample(1:nrow(pos_obs_pop), nrow(pos_obs_pop) * pop_pos_obs_ratio)
pos_obs_pop <- pos_obs_pop[pos_obs_keep,]
spam <- rbind(pos_obs_pop, neg_obs_pop)


#----------------------------------------------------------------------------
#
#  NORMAL RUN 
# 
#----------------------------------------------------------------------------


# create test and training sets 
inTrain <- sample(1:nrow(spam), nrow(spam) * sample_ratio) # select 85% of the items 
train <- spam[inTrain, ]
test <- spam[-inTrain, ]
rm(inTrain)

# Lets start with a standard glm 
model <- glm(train$class ~ ., data=train, family=binomial(link="logit"))
predicted <- predict(model, test, type=c("response" ))
results <- as.data.frame(cbind(test$class, predicted))
colnames(results) <- c("actual", "odds")
results$pred <- ifelse(results$odds < 0.5, 0, 1)
results$actual <- ifelse(results$actual == 2, 1, 0)


#------------------------------------------------------------------------
#
#       Cost Functions 
# 
#------------------------------------------------------------------------


# Contingency Table 

with(results, CrossTable(actual, pred, prop.chisq=FALSE, 
                         prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, 
                         format="SPSS"))



# precision recall graph 
pred <- prediction(results$pred, results$actual)
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf <- performance(pred,"tpr","fpr")
auc.perf <- performance(pred, measure="auc")
auc.perf@y.values
ROC.Val <- auc.perf@y.values
main.label <- paste("ROC Normal Curve - AUC=", ROC.Val)
plot(perf, colorize=TRUE, main=main.label)
abline(a=0, b=1)


#-------------------------------------------------------------------------
#
#  UNBALANCED ANALYSIS
#
#-------------------------------------------------------------------------


# create test and training sets 
inTrain <- sample(1:nrow(spam), nrow(spam) * sample_ratio) # select 85% of the items 
train <- spam[inTrain, ]
test <- spam[-inTrain, ]
rm(inTrain)

# Now unbalance the training set 
# initially theta is set to 5% which means that 95% of 
# the observations in the training set are positive

neg_obs <- subset(train, class=="ham")
pos_obs <- subset(train, class=="spam")
neg_obs_keep <- sample(1:nrow(neg_obs), nrow(neg_obs) * theta)
neg_obs <- neg_obs[neg_obs_keep,]
unbal_train <- rbind(pos_obs, neg_obs)
train <- unbal_train 

# Lets start with a standard glm 
model <- glm(train$class ~ ., data=train, family=binomial(link="logit"))
predicted <- predict(model, test, type=c("response" ))
results <- as.data.frame(cbind(test$class, predicted))
colnames(results) <- c("actual", "odds")
results$pred <- ifelse(results$odds < 0.5, 0, 1)
results$actual <- ifelse(results$actual == 2, 1, 0)

# Contingency Table 
with(results, CrossTable(actual, pred, prop.chisq=FALSE, 
                         prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, 
                         format="SPSS"))


# precision recall graph 
pred <- prediction(results$pred, results$actual)
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf <- performance(pred,"tpr","fpr")
auc.perf <- performance(pred, measure="auc")
auc.perf@y.values
ROC.Val <- auc.perf@y.values
main.label <- paste("ROC Unbalanced Curve - AUC=", ROC.Val)
plot(perf, colorize=TRUE, main=main.label)
abline(a=0, b=1)

