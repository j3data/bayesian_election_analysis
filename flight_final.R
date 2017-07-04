
#############################
### Import cleaned dataset ##
#############################

library(data.table)

# To summarize the ETL process:
# - variables have been converted to nominal and ordinal factors
# - dates and times have been parsed
# - empty variables have been dropped
# - NAs have been resolved

# Now, load the cleaned dataset and perform analysis
githubURL <- 'https://cdn.rawgit.com/j3data/predicting_delayed_flights/master/dataset.rds'
PHL <- as.data.table(readRDS(gzcon(url(githubURL))))
PHL$id <- seq.int(nrow(PHL))
setkey(PHL,id)

dim(PHL)
# n = 72498

# Examine data
summary(PHL)

snip <- cbind(order=seq_len(ncol(PHL)),t(PHL[DepDel15==1][1:3]))
snip


##########################
### Examine the data    ##
##########################

library(ggplot2)
source("http://peterhaschke.com/Code/multiplot.R")

summary(PHL)

qylab <- ylab('Delayed Flights')
qrotate <- theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))

p1 <- qplot(Month.nom, DepDelayMinutes, data = PHL)  + qylab + xlab('Month')
p2 <- qplot(DayofMonth.nom, DepDelay, data = PHL)    + qylab + xlab('Day of Month')
p3 <- qplot(DayOfWeek.nom, DepDelay, data = PHL)     + qylab + xlab('Day of Week (1 = Monday)')
p4 <- qplot(UniqueCarrier, DepDelay, data = PHL)     + qylab + xlab('Carrier')
p5 <- qplot(DistanceGroup.nom, DepDelay, data = PHL) + qylab + xlab('Distance Group')
p6 <- qplot(DepTimeBlk.nom, DepDelay, data = PHL)    + qylab + xlab('Scheduled Departure Time') + qrotate
p7 <- qplot(ArrTimeBlk.ord, DepDelay, data = PHL)    + qylab + xlab('Scheduled Arrival Time') + qrotate

multiplot(p1, p2, p3, p4, p5, p6, p7, cols=1)

#######################
### Model Elements   ##
#######################

# Similar elements are on the same line. Pick one per line.

# Predictor:
# 
# Month        | Month.nom     | Month.ord     |
# DayofMonth   | DayofMonth.nom| DayofMonth.ord|
# DayOfWeek    | DayOfWeek.nom | DayOfWeek.ord |
# UniqueCarrier| 
# Dest         | CRSElapsedTime| Distance      | DistanceGroup  | DistanceGroup.nom| DistanceGroup.ord
# CRSDepTime   | CRSDepTime.dt | DepTimeBlk    | DepTimeBlk.nom | DepTimeBlk.ord
# CRSArrTime   | CRSArrTime.dt | ArrTimeBlk    | ArrTimeBlk.nom | ArrTimeBlk.ord

# Response:
# 
# DepDelay     | DepDel15      | DepDel15.nom  | DepDelayMinutes|

#######################
### Divide data set  ##
#######################

## 75% of the sample size
n <- nrow(PHL)
smp_size <- floor(0.75 * n)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(n), size = smp_size)

PHL.train <- PHL[train_ind, ]
PHL.train$id.train <- seq.int(nrow(PHL.train))
setkey(PHL.train,id.train)

PHL.test <- PHL[-train_ind, ]
PHL.test$id.test <- seq.int(nrow(PHL.test))
setkey(PHL.test,id.test)


dim(PHL.train)
# n=54373
dim(PHL.test)
# n=18125


#####################################
### Stepwise Selection Algorithm   ##
#####################################

null <- glm(DepDel15.nom ~ 1, family=binomial(link='logit'),data=PHL.train)

full <- glm(DepDel15.nom ~ Month.nom + DayofMonth.nom + DayOfWeek.nom + Dest + DistanceGroup.nom +
              CRSDepTime.dt + DepTimeBlk.nom + CRSArrTime.dt + ArrTimeBlk.nom + UniqueCarrier, 
            family=binomial(link='logit'),data=PHL.train)

n <- nrow(PHL.train)

# AIC Stepwise Selection
model.aic.step <- step(null, scope=list(upper=full), direction = "both", k = 2)

# BIC Stepwise Selection
model.bic.step <- step(null, scope=list(upper=full), direction = "both", k = log(n))


# results of procedure
model.aic.step <- glm(formula = DepDel15.nom ~ DepTimeBlk.nom + Month.nom + UniqueCarrier + DayOfWeek.nom + 
                        Dest + DayofMonth.nom + ArrTimeBlk.nom + CRSArrTime.dt,
                      family = binomial(link = "logit"), data = PHL.train)

model.bic.step <- glm(formula = DepDel15.nom ~ DepTimeBlk.nom + Month.nom + UniqueCarrier + DayOfWeek.nom,
                      family = binomial(link = "logit"), data = PHL.train)

# The AIC model contains all elements of the BIC model plus four additional variables:
#     Dest, DayofMonth.nom, ArrTimeBlk.nom, CRSArrTime.dt

# Let's proceed with the BIC model and conduct LRT's for the four additional variables.

###########################################
## Model Comparison: Incremental Tests   ##
###########################################

# Let's test the four additional variables:
#   Dest, DayofMonth.nom, ArrTimeBlk.nom, CRSArrTime.dt

library(lmtest)
library(car)

# Partial F-tests are not appropriate for logistic regression models.
# Solution: Likelihood Ratio Chi-Square Tests to compare nested models.

# Variance Inflation Factors (VIFs) will be calculated at each step to check for multcolinearity.
# AIC and BIC terms will be calculated to compare model fit
# AUC will be calculated to score modedl

n           <- nrow(PHL.train)
cols        <- c('Month.nom','DayofMonth.nom','DayOfWeek.nom','Dest','DistanceGroup.nom',
                 'DepTimeBlk.nom','UniqueCarrier','ArrTimeBlk.nom','CRSArrTime.dt')
test.data   <- subset(PHL.test, select=cols)
truth       <- PHL.test$DepDel15.nom

getAUC      <- function(model, test.data, truth.vec) {
  library(ROCR)
  library(lmtest)
  p                <- predict(model, newdata=test.data, type="response")
  pr               <- prediction(p, truth.vec)
  auc              <- performance(pr, measure = "auc")
  auc              <- auc@y.values[[1]]
  return (auc)
}


# Test 0: Compare BIC Model to null model
model0 <- model.bic.step
AIC(model0, k=2)                  # AIC
AIC(model0, k=log(n))             # BIC
getAUC(model0, test.data, truth)  # AUC
lrtest(model0)                    # Likelihood Ratio Test
# Saturated model is significant
vif(model0)
# Accepted model0
modelLRT <- model0



# Test 1: Test 'Dest'
model1 <- update(modelLRT, . ~ . + Dest)
AIC(model1, k=2)                  # AIC
AIC(model1, k=log(n))             # BIC
getAUC(model1, test.data, truth)  # AUC
lrtest(modelLRT, model1)          # Likelihood Ratio Test
# Destination increases log likelihood
vif(model1)
# Accept model1.
modelLRT <- model1



# Test 2: Test 'DayofMonth.nom'
model2 <- update(modelLRT, . ~ . + DayofMonth.nom)
AIC(model2, k=2)                  # AIC
AIC(model2, k=log(n))             # BIC
getAUC(model2, test.data, truth)  # AUC
lrtest(modelLRT, model2)          # Likelihood Ratio Test
# Day of month increases log likelihood
vif(model2)
# Accept model2.
modelLRT <- model2



# Test 3: Test 'ArrTimeBlk.nom'
model3 <- update(modelLRT, . ~ . + ArrTimeBlk.nom)
AIC(model3, k=2)                  # AIC
AIC(model3, k=log(n))             # BIC
getAUC(model3, test.data, truth)  # AUC
lrtest(modelLRT, model3)          # Likelihood Ratio Test
# Arrival time Block increases log likelihood
vif(model3)
# Accept model3.
modelLRT <- model3



# Test 4: Test 'CRSArrTime.dt'
model4 <- update(modelLRT, . ~ . + CRSArrTime.dt)
AIC(model4, k=2)                  # AIC
AIC(model4, k=log(n))             # BIC
getAUC(model4, test.data, truth)  # AUC
lrtest(modelLRT, model4)          # Likelihood Ratio Test
# Scheduled Arrival Time increases log likelihood
vif(model4)
# Problem! CRSArrTime.dt has a very large VIF (22.45).
# DO not accept model4.


# All four variables increase log likelihood. Three have been accepted after checking VIFs.
# Finally, let's check if interaction is significant


# Test 5: Test 'UniqueCarrier*Dest'
model5 <- update(modelLRT, . ~ . + UniqueCarrier*Dest)
AIC(model5, k=2)                  # AIC
AIC(model5, k=log(n))             # BIC
getAUC(model5, test.data, truth)  # AUC
lrtest(modelLRT, model5)          # Likelihood Ratio Test

# VIFs not available due to aliased interaction terms.
# Not all carriers fly to all destinations.
vif(model6)
alias(model5)
modelLRT <- model5
# The interaction is significant, accept model5.


# Test 6: Test 'DayofMonth.nom*DayOfWeek.nom'
model6 <- update(modelLRT, . ~ . + DayofMonth.nom*DayOfWeek.nom)
AIC(model6, k=2)                  # AIC
AIC(model6, k=log(n))             # BIC
getAUC(model6, test.data, truth)  # AUC
lrtest(modelLRT, model6)          # Likelihood Ratio Test
modelLRT <- model6

file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)
save(model6,file="model6.Rda")
#load("model6.Rda")


# When fitting models, it is possible to increase the likelihood by adding parameters, 
# but doing so may result in overfitting. Both BIC and AIC attempt to resolve this problem
# by introducing a penalty term for the number of parameters in the model; the penalty term
# is larger in BIC than in AIC. 

# AIC and likelihood ratio test (LRT) have different purposes.

# AIC tells you whether it pays to have a richer model when your goal is approximating the 
# underlying data generating process the best you can in terms of Kullback-Leibler distance.
# LRT tells you whether at a chosen confidence level you can reject the hypothesis that some 
# restrictions on the richer model hold (e.g. some elements in the richer model are redundant).
# You would use AIC if your goal is model selection for forecasting. You would use likelihood 
# ratio test for significance testing. Different goals call for different tools.

###############################
### Examine Bad Predictions  ##
###############################

file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)
load("model6.Rda")

cols        <- c('Month.nom','DayofMonth.nom','DayOfWeek.nom','Dest','DistanceGroup.nom',
                 'DepTimeBlk.nom','UniqueCarrier','ArrTimeBlk.nom','CRSArrTime.dt')
test.data   <- subset(PHL.test, select=cols)
model       <- model6

# Accuracy
raw.results      <- predict(model,newdata=test.data,type='response')
fitted.results   <- ifelse(raw.results > 0.40,1,0)                         # Use p=0.40 as the 0/1 prediction split point
                                                                           # ~50% accurate when delay is predicted. 
misClasificError <- mean(fitted.results != PHL.test$DepDel15.nom)
print(paste('Accuracy',1-misClasificError))

# Predicted versus Actual Delays
DT.result        <- data.table(as.data.frame(cbind.data.frame(PHL.test,raw.results,fitted.results)))
del <- DT.result[,.N, by = .(Delayed=DepDel15,Predicted=fitted.results)]
pander(del)

file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)
save(del,file="del.Rda")
#load("data.Rda")


# Where did we miss a delay
DT.result[DepDel15==1 & fitted.results==0,.N, by = .(DepDelayMinutes, FlightDate.dt)]

# Missed Delays by date
result.date <- DT.result[DepDel15==1 & fitted.results==0]

# Plot all delays vs missed delays
qylab1 <- ylab('Missed Delays')
qylab2 <- ylab('All Delays')
p1 <- qplot(FlightDate.dt, DepDelayMinutes, data = result.date) + qylab1 + xlab('Month')  # Missed delays
p2 <- qplot(FlightDate.dt, DepDelayMinutes, data = PHL) + qylab2 + xlab('Month')          # All delays
multiplot(p1, p2, cols=1)

# on which days did the highest number of delays occurr
miss <- DT.result[DepDel15==1 & fitted.results==0,.N, by = FlightDate.dt]          # Missed delays
miss <- miss[order(-N)][1:50]
miss

real <- PHL[DepDel15==1,.N, by = FlightDate.dt]                                    # All delays
real <- real[order(-N)][1:100]
real

# types of delays
# http://aspmhelp.faa.gov/index.php/Types_of_Delay

# what is the cause of the delay
DT.delay <- PHL[DepDel15==1,.(CarrierDelay=sum(CarrierDelay.num),WeatherDelay=sum(WeatherDelay.num),
                              NASDelay=sum(NASDelay.num),SecurityDelay=sum(SecurityDelay.num),
                              LateAircraftDelay=sum(LateAircraftDelay.num),.N), by = FlightDate.dt]
DT.delay <- DT.delay[order(-N)][1:50]
DT.delay

# Of the missed delays, what is the cause of the delay
DT.miss <- DT.result[DepDel15==1 & fitted.results==0,
                     .(CarrierDelay=sum(CarrierDelay.num),WeatherDelay=sum(WeatherDelay.num),
                       NASDelay=sum(NASDelay.num),SecurityDelay=sum(SecurityDelay.num),
                       LateAircraftDelay=sum(LateAircraftDelay.num),.N), by = FlightDate.dt]
DT.miss <- DT.miss[order(-N)][1:50]
DT.miss

# comment
# of the biggest un predicted delays, the is a pattern where one day of bad weather causes a bunch of delays, 
# then the subsequent days also have high delays is reaction to the weather. 
# there are delays even if there are no weather problems on the subsequent day.
# this is not un expected for the philadelphia area.
# re 4/3-9 and 12/17-18 and 1/7-8
# overall, i think the model looks like a good fit.


###############################
### Interpreting the Model   ##
###############################

file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)
load("model6.Rda")

library(ggplot2)
library(broom)
library(pander)

model <- model6
data.table(tidy(model))[190:363]

# Coefficients:
deptime            <- data.table(tidy(model))[2:19,c(1,2,5)]
month              <- data.table(tidy(model))[20:30,c(1,2,5)]
carrier            <- data.table(tidy(model))[31:39,c(1,2,5)]
day.of.week        <- data.table(tidy(model))[40:45,c(1,2,5)]
dest               <- data.table(tidy(model))[46:95,c(1,2,5)]
day.of.month       <- data.table(tidy(model))[96:125,c(1,2,5)]
arrtime            <- data.table(tidy(model))[126:143,c(1,2,5)]
carrerXdest        <- data.table(tidy(model))[144:190,c(1,2,5)]
DayMonthxDayWeek   <- data.table(tidy(model))[192:363,c(1,2,5)]


# labels
deptime$labels           <- substring(deptime$term, 15, 23)
month$labels             <- as.numeric(substring(month$term, 10, 12))
carrier$labels           <- substring(carrier$term, 14, 23)
day.of.week$labels       <- as.numeric(substring(day.of.week$term, 14, 23))
dest$labels              <- substring(dest$term, 5, 15)
day.of.month$labels      <- as.numeric(substring(day.of.month$term, 15, 17))
arrtime$labels           <- substring(arrtime$term, 15, 23)
carrerXdest$labels       <- paste(substring(carrerXdest$term, 14, 16),substring(carrerXdest$term, 21, 23),sep = '')
DayMonthxDayWeek$labels  <- paste(substring(DayMonthxDayWeek$term, 14, 15),substring(DayMonthxDayWeek$term, 30, 31),sep = '')

# chart settings
qylab <- ylab('Regression Coefficient')
qqtheme <- theme(axis.title.y=element_text(margin=margin(0,8,0,0))) +
               theme(axis.title.x=element_text(margin=margin(8,0,0,0))) +
               theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))

# Departure Time
qplot(labels, estimate, data = deptime) + qylab + xlab('Departure Time') + qqtheme

# Carrier
panderOptions('keep.trailing.zeros', TRUE)
panderOptions('round', 4)

lab <- c('Alaska Airlines','JetBlue Airways','Delta Air Lines','ExpressJet Airlines','Frontier Airlines',
            'Spirit Air Lines','SkyWest Airlines','United Air Lines','Southwest Airlines')
carrier.print <- data.table(Description=lab,carrier[,c('labels','estimate','p.value')])
carrier.print <- setorder(carrier.print, -estimate)
pander(carrier.print)

file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)
save(carrier.print,file="carrier.print")
#load("carrier.print")


# DayOfWeek
qplot(labels, estimate, data = day.of.week) + qylab + xlab('Day of Week') + qqtheme

lab <- c('Tuesday','Wednesday','Thursday','Friday','Saturday', 'Sunday')
ready_table <- data.table(Description=lab,day.of.week[,c('estimate','p.value')])
pander(ready_table)

file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)
save(ready_table,file="ready_table")
#load("ready_table")


# Dest
qplot(labels, estimate, data = dest) + qylab + xlab('Destination') + qqtheme

PHL.train[(Dest %in% c('STL'))]


# Month
month$Month <- c('2017-02-01','2017-03-01','2017-04-01','2016-05-01','2016-06-01','2016-07-01','2016-08-01',
                 '2016-09-01','2016-10-01','2016-11-01','2016-12-01')
month$Month <- as.Date(month$Month)

qplot(Month, estimate, data = month) + qylab + xlab('Month')

#####################
### GLMnet Model   ##
#####################

library(glmnet)

# chose predictors
cols <- c("DepTimeBlk.nom","Month.nom","UniqueCarrier","DayOfWeek.nom","Dest","DayofMonth.nom","ArrTimeBlk.nom")

x_train <- model.matrix( ~ . + UniqueCarrier*Dest + DayOfWeek.nom*DayofMonth.nom, subset(PHL.train, select=cols))
dim(x_train)

x_test  <- model.matrix( ~ . + UniqueCarrier*Dest + DayOfWeek.nom*DayofMonth.nom, subset(PHL.test, select=cols))
dim(x_test)


# make sure factor levels match

a <- colnames(x_train)
b <- colnames(x_test)

setdiff(a,b)
# in train not test:
# DestILM: Wilmington NC International Airport
# DestSAV: Savannah/Hilton Head International Airport

setdiff(b,a)
# in test not train:
# (none)

PHL.train[Dest  %in% c('SAV','ILM')]

# there are 5 flights SAV and ILM in the train dataset.
# since there are 0 flights to ILM and SAV in the test dataset, they are causing a design matrix mismatch
# since it is only 5 flights, i am jsut going to remove them from the training set and rerun.

PHL.train.sub <- PHL.train[!(Dest %in% c('SAV','ILM'))]

# rerun design matrix with SAV and ILM Destinations excluded
x_train <- model.matrix( ~ .-1, subset(PHL.train.sub, select=cols))
dim(x_train)


# fit a model

fit = glmnet(x=x_train,
             y = as.factor(PHL.train.sub$DepDel15.nom), 
             intercept=FALSE ,
             family = "binomial", 
             alpha=1                   # the lasso penalty
)

plot(fit, xvar = "dev", label = TRUE)


cvfit = cv.glmnet(x=x_train,
                  y = as.factor(PHL.train.sub$DepDel15.nom), 
                  intercept=FALSE ,
                  family = "binomial", 
                  alpha=1,                   # the lasso penalty
                  nfolds=10                  # default
)

plot(cvfit)
glance(cvfit)
print(cvfit)


# CV Fitted Probabilities and AUC
pred             <- predict(cvfit, newx = x_test, s = 'lambda.min', type = "response", na.action = na.pass)
pred.full        <- cbind.data.frame(PHL.test,prob=pred)
pr               <- prediction(pred.full$prob.1, pred.full$DepDel15.nom)
auc              <- performance(pr, measure = "auc")
auc              <- auc@y.values[[1]]
auc
prf              <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
quantile(pred)


# CV Model Paramters
c          <- coef(cvfit, s = "lambda.min")
c.index    <- which(c!=0)
c.labels   <- row.names(c)[c.index]              # Elements of cvfit model
c.values   <- c[c.index]

params     <- data.table(label=c.labels,coefficient=c.values)
params