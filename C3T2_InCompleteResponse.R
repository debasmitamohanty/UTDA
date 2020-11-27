# Title: caret-pipeline-adv-regression-wy

# Last update: 2020.10

# File: 
# Project name: 


###############
# Project Notes
###############

# Summarize project: This is an intermediate R pipeline to help students how to 
# integrate FE and to better understand how to organize a pipeline in R using 
# the the caret package. This pipeline incorporates feature selection.  

# Summarize top model and/or filtered dataset
# The top model was model_name used with ds_name.

###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())



# get working directory
getwd()
#> getwd()
#[1] "/Users/debasmitamohanty/UTDAC3.Rproj"
# set working directory
setwd("/Users/debasmitamohanty/UTDAC3.Rproj")
dir()


################
# Load packages
################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
#install.packages("dplyr")
install.packages("lattice")
install.packages("ggplot")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("C50")
install.packages("inum")
install.packages("foreach")
install.packages("iterators")
install.packages("parallel")
#install.packages((rpart.plot))
library(caret)
library(corrplot)
library(readr)
library(mlbench)
library(C50)
library(ggplot2)
library(lattice)
library(lava)
library(purrr)
#library(dplyr)

#####################
# Parallel processing
#####################

# NOTE: Be sure to use the correct package for your operating system. 
# Remember that all packages should loaded in the 'Load packages' section.

#--- for OSX ---#
install.packages("doMC")  # install in 'Load packages' section above 
library(doMC)
detectCores()   # detect number of cores
registerDoMC(cores = 4)  # set number of cores (don't use all available)
library(parallel)
#--- for Win ---#
install.packages("doParallel") # install in 'Load packages' section above
library(doParallel)  # load in the 'Load Packages' section above
detectCores()  # detect number of cores
cl <- makeCluster(2)  # select number of cores
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
stopCluster(cl)

##############
# Import data
##############

##--- Load raw datasets ---##

# Load Train/Existing data (Dataset 1)

oob_CR <- read.csv("SurveyData/CompleteResponses.csv", stringsAsFactors = FALSE, header=T)


str(oob_CR) 
#'data.frame':	9898 obs. of  7 variables:
#  $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : int  0 1 0 1 0 1 1 1 0 1 ...

##--- Load Predict/New data (Dataset 2) ---##

##--- Load preprocessed datasets that have been saved ---##

read.csv("SurveyData/CompleteResponses.csv", stringsAsFactors = FALSE, header=T) 

#LOAD DATA (Survey Incomplete used for final predictions)
read.csv("SurveyData/SurveyIncomplete.csv", stringsAsFactors = FALSE, header=T)

oob_IR <- read.csv("SurveyData/SurveyIncomplete.csv", stringsAsFactors = FALSE, header=T)

###############
# Save datasets
###############




################
# Evaluate data
################

##--- Dataset 1 ---##

str(oob_CR)  

#> str(oob_CR)
#'data.frame':	9898 obs. of  7 variables:
 # $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : int  0 1 0 1 0 1 1 1 0 1 ...

names(oob_CR)
#> names(oob_CR)
#[1] "salary"  "age"     "elevel"  "car"     "zipcode" "credit"  "brand" 

head(oob_CR)
#> head(oob_CR)
#salary age elevel car zipcode    credit brand
#1 119806.54  45      0  14       4 442037.71     0
#2 106880.48  63      1  11       6  45007.18     1
#3  78020.75  23      0  15       2  48795.32     0
#4  63689.94  51      3   6       5  40888.88     1
#5  50873.62  20      3  14       4 352951.50     0
#6 130812.74  56      3  14       3 135943.02     1
tail(oob_CR)
#> tail(oob_CR)
#salary age elevel car zipcode   credit brand
#9893  28751.26  60      2  10       0      0.0     1
#9894  87580.91  75      1  18       8 282511.9     1
#9895 129181.38  75      2   7       4 384871.4     1
#9896  97828.09  66      2  15       0 399446.7     1
#9897  20000.00  24      1  14       1 223204.6     1
#9898  96430.16  34      1   2       7 224029.8     0
# check for missing values 
anyNA(oob_CR)
#> anyNA(oob_CR)
#[1] FALSE

anyNA(oob_CR)

##--- Dataset 2 ---##



#############
# Preprocess
#############

##--- Dataset 1 ---##

# remove ID and obvious features
oob_CR$X <- NULL   # remove ID
str(oob_CR) # 9898 obs. of  7 variables
#'data.frame':	9898 obs. of  7 variables:
#  $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : int  0 1 0 1 0 1 1 1 0 1 ...
# rename a column
names(ds) <- c("ColumnName","ColumnName","ColumnName") 
# change data types
#ds$ColumnName <- as.typeofdata(ds$ColumnName)
oob_CR$brand <- as.factor(oob_CR$brand)
# handle missing values (if applicable) 
na.omit(oob_CR$Salary)
na.omit(oob_CR$credit)
na.omit(oob_CR$brand)
na.omit(oob_CR$car)
na.omit(oob_CR$age)
na.omit(oob_CR$zipcode)
na.omit(oob_CR$elevel)
na.omit(oob_CR$brand)
is.na(oob_CR)
na.exclude(oob_CR$brand) 
na.exclude(oob_CR$age)
na.exclude(oob_CR$Salary)        
oob_CR$credit[is.na(oob_CR$credit)] <- mean(oob_CR$credit,na.rm = TRUE)
na.omit(oob_CR$credit)
na.omit(oob_CR$credit)

str(oob_CR)
#> str(oob_CR)
#'data.frame':	9898 obs. of  7 variables:
#  $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : Factor w/ 2 levels "0","1": 1 2 1 2 1 2 2 2 1 2 .


##--- Dataset 2 ---##

# If there is a dataset with unseen data to make predictions on, then preprocess 
# here to make sure that it is preprossed the same as the training dataset.


is.na(oob_IR)
#incomplete dataset must be changed to factor too!
oob_IR$brand <-as.factor(oob_IR$brand)
str(oob_IR)
#> str(oob_IR)
#'data.frame':	9898 obs. of  7 variables:
#  $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : Factor w/ 2 levels "0","1": 1 2 1 2 1 2 2 2 1 2 ...

#####################
# EDA/Visualizations
#####################

# statistics
summary(oob_CR)

#> summary(oob_CR)
#salary            age            elevel           car           zipcode          credit       brand   
#Min.   : 20000   Min.   :20.00   Min.   :0.000   Min.   : 1.00   Min.   :0.000   Min.   :     0   0:3744  
#1st Qu.: 52082   1st Qu.:35.00   1st Qu.:1.000   1st Qu.: 6.00   1st Qu.:2.000   1st Qu.:120807   1:6154  
#Median : 84950   Median :50.00   Median :2.000   Median :11.00   Median :4.000   Median :250607           
#Mean   : 84871   Mean   :49.78   Mean   :1.983   Mean   :10.52   Mean   :4.041   Mean   :249176           
#3rd Qu.:117162   3rd Qu.:65.00   3rd Qu.:3.000   3rd Qu.:15.75   3rd Qu.:6.000   3rd Qu.:374640           
#Max.   :150000   Max.   :80.00   Max.   :4.000   Max.   :20.00   Max.   :8.000   Max.   :500000            



# plots
hist(oob_CR$brand)
plot(oob_CR$salary, oob_CR$brand)
plot(oob_CR$age, oob_CR$brand)
plot(oob_CR$credit, oob_CR$brand)
plot(oob_CR$car, oob_CR$brand)
plot(oob_CR$zipcode, oob_CR$brand)
qqnorm(oob_CR$age) # Be familiar with this plot, but don't spend a lot of time on it


################
# Sampling
################

# create 10% sample 
set.seed(998) # set random seed
oob_CR10p <- oob_CR[sample(1:nrow(oob_CR), round(nrow(oob_CR)*.1),replace=FALSE),]
nrow(oob_CR10p)
#> nrow(oob_CR10p)
#[1] 990
head(oob_CR10p) # ensure randomness
#> head(oob_CR10p)
#      salary   age elevel  car zipcode credit    brand
#294   27660.54  32      1   7       1 251119.4     1
#2432 146305.31  54      3   8       6 203622.0     1
#3452 123515.78  63      4  15       8 372859.4     1
#2206 125715.07  60      3   2       1 149851.6     1
#7400  69063.16  35      1  19       2 214024.0     0
#6235 132905.61  25      1   3       1 152524.1     1
# 1k sample
set.seed(998) # set random seed
oob_CR1k <- oob_CR[sample(1:nrow(oob_CR), 1000, replace=FALSE),]
nrow(oob_CR1k) # ensure number of obs
#> nrow(oob_CR1k)
#[1] 1000
head(oob_CR1k) # ensure randomness
#> head(oob_CR1k) 
#salary age elevel car zipcode   credit brand
#294   27660.54  32      1   7       1 251119.4     1
#2432 146305.31  54      3   8       6 203622.0     1
#3452 123515.78  63      4  15       8 372859.4     1
#2206 125715.07  60      3   2       1 149851.6     1
#7400  69063.16  35      1  19       2 214024.0     0
#6235 132905.61  25      1   3       1 152524.1     1

#######################
# Feature selection
#######################

#######################
# Correlation analysis
#######################

# good for num/int data 

# calculate correlation matrix for all vars
corrAll <- cor(oob_CR1k[,1:7])
# view the correlation matrix
corrAll
#> corrAll
#salary          age       elevel          car     zipcode       credit        brand
#salary   1.000000000  0.055076995 -0.037414288 -0.046247910  0.02332802 -0.009739923  0.215480485
#age      0.055076995  1.000000000  0.010634915  0.008112447 -0.00885010  0.062597727  0.030571117
#elevel  -0.037414288  0.010634915  1.000000000 -0.108955488  0.01805836 -0.009217754  0.039778583
#car     -0.046247910  0.008112447 -0.108955488  1.000000000  0.04768254 -0.059151818 -0.006925400
#zipcode  0.023328016 -0.008850100  0.018058364  0.047682542  1.00000000 -0.001716130  0.018158552
#credit  -0.009739923  0.062597727 -0.009217754 -0.059151818 -0.00171613  1.000000000 -0.003599491
#brand    0.215480485  0.030571117  0.039778583 -0.006925400  0.01815855 -0.003599491  1.000000000

# plot correlation matrix
corrplot(corrAll, method = "circle")
corrplot(corrAll, order = "hclust") # sorts based on level of collinearity
# find IVs that are highly corrected (ideally >0.90)
corrIV <- cor(oob_CR1k[,1:6])
# create object with indexes of highly corr features
corrIVhigh <- findCorrelation(corrIV, cutoff=0.8)   
# print indexes of highly correlated attributes
corrIVhigh
#> corrIVhigh
#integer(0)
# get var name of high corr IV
colnames(oob_CR1k[c(4)]) # "Car" 
# remove highly correlated features
corr_CR <- #code to remove highy corr features

############
# caret RFE 
############

# lmFuncs - linear model
 #rfFuncs - random forests
# nbFuncs - naive Bayes
# treebagFuncs - bagged trees


## ----C50---- ##

str(oob_CR) 
#> str(oob_CR) 

#'data.frame':	9898 obs. of  7 variables:
#  $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : Factor w/ 2 levels "0","1": 1 2 1 2 1 2 2 2 1 2 ...

###Auto Tune with C5.0###
#dataset = CompleteResponses
#Dep (Y) Variable = brand


library(C50)
#oob_CR$brand <- as.factor(oob_CR$brand)
treeModel <- C5.0(x = oob_CR[1:7423, -7],
                  y = oob_CR$brand[1:7423],
                  control = C5.0Control(winnow = TRUE))
summary(treeModel)
#> summary(treeModel)

#Call:
#  C5.0.default(x = oob_CR[1:7423, -7], y = oob_CR$brand[1:7423], control = C5.0Control(winnow = TRUE))


#C5.0 [Release 2.07 GPL Edition]  	Sat Nov 21 19:06:35 2020
-------------------------------
  
#  Class specified by attribute `outcome'

#Read 7423 cases (7 attributes) from undefined.data

#No attributes winnowed

#Decision tree:

#salary > 123755.3:
#:...salary > 130722.5: 1 (1109)
#:   salary <= 130722.5:
#:   :...age <= 36: 1 (115)
#:       age > 36:
#:       :...age > 59: 1 (122/1)
#:           age <= 59:
#:#           :...zipcode > 0: 1 (147/30)
#:               zipcode <= 0:
#:               :...credit <= 313861.1: 1 (11/1)
#:                   credit > 313861.1: 0 (11/2)
#salary <= 123755.3:
#:...salary <= 45570.48:
#    :...age <= 59: 1 (977/16)
 #   :   age > 59:
#    :   :...salary > 30261.04: 0 (269/13)
#    :       salary <= 30261.04:
#    :       :...salary <= 20278.24: 1 (39/1)
#    :           salary > 20278.24:
#    :           :...elevel > 2: 0 (75/25)
#    :               elevel <= 2:
#    :               :...age <= 61: 1 (8)
#    :                   age > 61:
#    :                   :...elevel <= 1:
#    :                       :...car > 15:
#    :                       :   :...credit <= 115470.9: 1 (2)
#    :                       :   :   credit > 115470.9: 0 (11/1)
#    :                       :   car <= 15:
#    :                       :   :...zipcode <= 1: 1 (13/1)
#    :                       :       zipcode > 1:
#    :                       :       :...salary <= 24972.87: 1 (17/3)
#    :                       :           salary > 24972.87: 0 (24/9)
#    :                       elevel > 1:
#    :                       :...age > 74: 0 (6)
#    :                           age <= 74:
#    :                           :...age <= 62: 0 (4)
#    :                               age > 62:
#    :                               :...zipcode > 5: 0 (6/1)
#    :                                   zipcode <= 5:
#    :                                   :...zipcode > 1: 1 (11/1)
 #   :                                       zipcode <= 1:
#    :                                       :...salary <= 24313.31: 0 (3)
#    :                                           salary > 24313.31: 1 (2)
#    salary > 45570.48:
#    :...salary > 100262.5:
#        :...age > 61: 1 (429/3)
#        :   age <= 61:
#        :   :...age <= 39:
#        :       :...salary > 103690.3: 1 (365/12)
#        :       :   salary <= 103690.3:
#        :       :   :...age <= 26: 1 (26/4)
#        :       :       age > 26:
#        :       :       :...zipcode <= 0: 1 (4)
#        :       :           zipcode > 0:
#        :       :           :...zipcode <= 3: 0 (16/3)
#        :       :               zipcode > 3: 1 (24/8)
#        :       age > 39:
#        :       :...age <= 57: 0 (385/24)
#        :           age > 57:
#        :           :...elevel > 3: 0 (12/2)
#        :               elevel <= 3:
#        :               :...age <= 59:
#        :                   :...salary <= 118499.5: 0 (26/6)
#        :                   :   salary > 118499.5: 1 (9/2)
#        :                   age > 59:
#        :                   :...credit > 103192.3: 1 (23/5)
#        :                       credit <= 103192.3:
#        :                       :...credit <= 32682.14: 1 (2)
#        :                           credit > 32682.14: 0 (4)
#        salary <= 100262.5:
#        :...age <= 40:
#            :...salary <= 53859.7:
#            :   :...salary <= 50756.89: 1 (110/32)
#            :   :   salary > 50756.89:
#            :   :   :...salary > 53569.58: 1 (5)
#            :   :       salary <= 53569.58:
#            :   :       :...age <= 38: 0 (42/10)
#            :   :           age > 38: 1 (6/1)
#            :   salary > 53859.7:
#            :   :...age <= 36: 0 (754/32)
#            :       age > 36:
#            :       :...salary > 77195.87: 0 (96/6)
#            :           salary <= 77195.87:
#            :           :...car > 12: 0 (36/4)
#            :               car <= 12:
#            :               :...car > 11: 1 (5)
#            :                   car <= 11:
#            :                   :...age <= 38:
#            :                       :...elevel <= 0: 0 (10)
#            :                       :   elevel > 0:
#            :                       :   :...salary <= 60695.87: 1 (5/1)
#            :                       :       salary > 60695.87: 0 (10/1)
#            :                       age > 38:
#            :                       :...car > 8: 0 (9/2)
#            :                           car <= 8:
#            :                           :...zipcode <= 7: 1 (10/1)
#            :                               zipcode > 7: 0 (2)
#            age > 40:
#            :...salary > 78797.88:
#                :...age <= 60: 0 (381/26)
#                :   age > 60: 1 (415/9)
#                salary <= 78797.88:
#                :...age > 59:
#                    :...salary > 72208.98: 0 (123/54)
#                    :   salary <= 72208.98:
#                    :   :...age > 61: 0 (464/7)
#                    :       age <= 61:
#                    :       :...age > 60: 0 (29/5)
#                    :           age <= 60:
#                    :           :...credit > 327084.7:
#                    :               :...credit <= 472994.7: 1 (7)
#                    :               :   credit > 472994.7: 0 (2)
#                    :               credit <= 327084.7:
#                    :               :...salary > 52695.69: 0 (9)
#                    :                   salary <= 52695.69:
#                    :                   :...salary <= 51502.51: 0 (7/2)
 #                   :                       salary > 51502.51: 1 (3)
#                    age <= 59:
 #                   :...salary > 72638.59: 0 (125/42)
#                        salary <= 72638.59:
#                        :...age <= 56: 1 (382/9)
#                            age > 56:
#                            :...age <= 57: 1 (22/4)
 #                               age > 57:
  #                              :...age <= 58: 1 (18/7)
   #                                 age > 58:
    #                                :...zipcode <= 2: 0 (8/2)
     #                                   zipcode > 2:
      #                                  :...credit > 334466.2: 1 (6)
       #                                     credit <= 334466.2:
#                                            :...elevel <= 1: 0 (6/1)
        #                                        elevel > 1:
 #                                               :...salary <= 68553.8: 1 (7)
  #                                                  salary > 68553.8: 0 (2)
#
#
##Evaluation on training data (7423 cases):
#
#	    Decision Tree   
	  ----------------  
#	  Size      Errors  

#	    68  432( 5.8%)   <<


#	   (a)   (b)    <-classified as
	  ----  ----
#	  2687   152    (a): class 0
#	   280  4304    (b): class 1


#	Attribute usage:

#	100.00%	salary
#	 85.06%	age
#	  4.45%	zipcode
#	  4.01%	elevel
#	  2.07%	car
#	  1.52%	credit


#Time: 0.0 secs



treeModel

#> treeModel


#Call:
#  C5.0.default(x = oob_CR[1:7423, -7], y = oob_CR$brand[1:7423], control = C5.0Control(winnow = TRUE))

#Classification Tree
#Number of samples: 7423 
#Number of predictors: 6 

#Tree size: 68 

#Non-standard options: attempt to group attributes, winnowing

plot(treeModel)

#library(rpart.plot)
#rpart.plot(treeModel, type = 4,extra = 1,clip.right.labs = F)
###############################
treeModelrule_mod <- C5.0(x = oob_CR[1:7423, -7],
                          y = oob_CR$brand[1:7423], rules = TRUE)

treeModelrule_mod 

#Call:
#  C5.0.default(x = oob_CR[1:7423, -7], y = oob_CR$brand[1:7423], rules = TRUE)

#Rule-Based Model
#Number of samples: 7423 
#Number of predictors: 6 

#Number of Rules: 29 

#Non-standard options: attempt to group attributes

summary(treeModelrule_mod )
#Call:
#  C5.0.default(x = oob_CR[1:7423, -7], y = oob_CR$brand[1:7423], rules = TRUE)


#C5.0 [Release 2.07 GPL Edition]  	Sat Nov 21 20:11:41 2020
-------------------------------
  
#  Class specified by attribute `outcome'

#Read 7423 cases (7 attributes) from undefined.data

#Rules:

#Rule 1: (19, lift 2.5)
#	salary > 20278.24
#	salary <= 45570.48
#	age > 74
#	elevel > 1
#	elevel <= 2
#	->  class 0  [0.952]

#Rule 2: (269/13, lift 2.5)
#	salary > 30261.04
#	salary <= 45570.48
#	age > 59
#	->  class 0  [0.948]

#Rule 3: (63/4, lift 2.4)
#	salary > 20278.24
#	salary <= 45570.48
#	age > 59
#	elevel > 1
#	zipcode <= 1
#	->  class 0  [0.923]

#Rule 4: (60/4, lift 2.4)
#	salary > 20278.24
#	salary <= 45570.48
#	age > 61
#	age <= 74
#	elevel > 1
#	zipcode > 5
#	->  class 0  [0.919]

#Rule 5: (22/1, lift 2.4)
#	salary > 20278.24
#	salary <= 45570.48
#	age > 61
#	elevel <= 1
#	car > 15
#	credit > 115470.9
#	->  class 0  [0.917]

#Rule 6: (335/34, lift 2.3)
#	salary > 24972.87
#	salary <= 45570.48
#	age > 61
#	->  class 0  [0.896]

#Rule 7: (6, lift 2.3)
#	salary <= 45570.48
#	age > 61
#	age <= 62
#	elevel > 1
#	elevel <= 2
#	->  class 0  [0.875]

#Rule 8: (6314/3475, lift 1.2)
#	salary <= 130722.5
#	->  class 0  [0.450]

#Rule 9: (1109, lift 1.6)
#	salary > 130722.5
#	->  class 1  [0.999]

#Rule 10: (1332/17, lift 1.6)
#	salary > 78797.88
#	age > 60
#	->  class 1  [0.987]

#Rule 11: (348/4, lift 1.6)
#	salary > 100262.5
#	age <= 26
#	->  class 1  [0.986]

#Rule 12: (773/11, lift 1.6)
#	salary > 100262.5
#	age > 59
#	elevel <= 3
	#->  class 1  [0.985]

#Rule 13: (511/8, lift 1.6)
#	salary > 118499.5
#	age > 57
#	elevel <= 3
#	->  class 1  [0.982]

#Rule 14: (868/16, lift 1.6)
#	salary > 103690.3
#	age <= 39
#	->  class 1  [0.980]

#Rule 15: (1339/30, lift 1.6)
#	salary > 123755.3
#	zipcode > 0
#	->  class 1  [0.977]

#Rule 16: (82/1, lift 1.6)
#	salary > 123755.3
#	age <= 59
#	zipcode <= 0
#	credit <= 313861.1
#	->  class 1  [0.976]

#Rule 17: (149/3, lift 1.6)
#	salary > 45570.48
#	salary <= 68553.8
#	age > 40
#	age <= 59
#	elevel > 1
#	zipcode > 2
#	->  class 1  [0.974]

#Rule 18: (890/26, lift 1.6)
#	salary <= 72638.59
#	age > 40
#	age <= 58
#	->  class 1  [0.970]

#Rule 19: (525/19, lift 1.6)
#	salary > 100262.5
#	age <= 39
#	zipcode > 3
#	->  class 1  [0.962]

#Rule 20: (310/12, lift 1.6)
#	salary <= 72208.98
#	age > 40
#	age <= 60
#	credit > 327084.7
#	credit <= 472994.7
#	->  class 1  [0.958]

#Rule 21: (111/4, lift 1.5)
#	salary > 100262.5
#	age <= 39
#	zipcode <= 0
#	->  class 1  [0.956]

#Rule 22: (40/1, lift 1.5)
#	salary <= 30261.04
#	elevel <= 1
#	car <= 15
#	zipcode <= 1
	#->  class 1  [0.952]

#Rule 23: (16, lift 1.5)
#	salary <= 78797.88
#	age > 58
#	age <= 59
#	zipcode > 2
#	credit > 334466.2
#	->  class 1  [0.944]

#Rule 24: (589/35, lift 1.5)
#	salary <= 50756.89
#	age <= 40
#	->  class 1  [0.939]

#Rule 25: (12, lift 1.5)
#	salary <= 77195.87
#	age > 36
#	age <= 40
#	car > 11
#	car <= 12
#	->  class 1  [0.929]

#Rule 26: (57/4, lift 1.5)
#	salary <= 30261.04
#	age <= 74
#	elevel > 1
#	elevel <= 2
#	zipcode > 1
#	zipcode <= 5
#	->  class 1  [0.915]

#Rule 27: (37/4, lift 1.4)
#	salary <= 77195.87
#	age > 38
#	age <= 40
#	car <= 8
#	zipcode <= 7
#	->  class 1  [0.872]

#Rule 28: (34/4, lift 1.4)
#	salary <= 60695.87
#	age > 36
#	age <= 38
#	elevel > 0
#	car <= 12
#	->  class 1  [0.861]

#Rule 29: (1467/371, lift 1.2)
#	salary <= 45570.48
#	->  class 1  [0.747]

#Default class: 1


#Evaluation on training data (7423 cases):

#	        Rules     
#	  ----------------
#	    No      Errors

#	    29  446( 6.0%)   <<


#	   (a)   (b)    <-classified as
	  ----  ----
#	  2665   174    (a): class 0
#	   272  4312    (b): class 1


#	Attribute usage:

#	100.00%	salary
#	 58.21%	age
#	 28.92%	zipcode
#	 16.87%	elevel
#	  5.64%	credit
#	  1.90%	car


#Time: 0.1 secs

#########################

###CARET Model using Manual Tune-Random Forest####use 5 diff mtry values (expand.grid)


# define the control using a random forest selection function (regression or classification)
RFcontrol <- rfeControl(functions=rfFuncs, method="cv", number=5, repeats=1)
# run the RFE algorithm
set.seed(7)
rfeRF <- rfe(oob_CR1k[,1:6], oob_CR1k[,7], sizes=c(1:6), rfeControl=RFcontrol)
rfeRF 
#Recursive feature selection

#Outer resampling method: Cross-Validated (5 fold) 

#Resampling performance over subset size:
  
#  Variables   RMSE Rsquared    MAE  RMSESD RsquaredSD   MAESD Selected
#1 0.4695   0.1510 0.3384 0.02027    0.05141 0.02514         
#2 0.2287   0.7762 0.1209 0.01733    0.02910 0.01128        *
#  3 0.2748   0.7054 0.2046 0.01279    0.04001 0.01134         
#4 0.3285   0.5994 0.2774 0.01751    0.04805 0.01624         
#5 0.3680   0.4944 0.3261 0.01139    0.04338 0.01032         
#6 0.3152   0.6174 0.2556 0.01559    0.05453 0.01502         

#The top 2 variables (out of 2):
 # salary, age

# plot the results
plot(rfeRF, type=c("g", "o"))
# show predictors used
predictors(rfeRF)
#> predictors(rfeRF)
#[1] "salary" "age" 
# Note results.  
varImp(rfeRF)
# Note results.
# Overall
#Overall
#salary 77.12310
#age    48.37873
##--- create ds with features using varImp from top model ---##

# create ds with predictors from varImp
rfeRF_CR <- oob_CR1k[,predictors(rfeRF)]
str(rfeRF_CR)
#> str(rfeRF_CR)
#'data.frame':	1000 obs. of  2 variables:
#  $ salary: num  27661 146305 123516 125715 69063 ...
#$ age   : int  32 54 63 60 35 25 78 23 62 20 ...
# add dv
rfeRF_CR$Brand <- oob_CR1k$brand
# confirm new ds
str(rfeRF_CR)
#> str(rfeRF_CR)
#'data.frame':	1000 obs. of  3 variables:
#  $ salary: num  27661 146305 123516 125715 69063 ...
#$ age   : int  32 54 63 60 35 25 78 23 62 20 ...
#$ Brand : int  1 1 1 1 0 1 1 1 1 0 ...


##############################
# Feature engineering
##############################

##################
# Train/test sets
##################


# oob_CR(#define a 75%/25% TRAIN/TEST SPLIT of the dataset)
set.seed(123) 
inTraining <- createDataPartition(oob_CR1k$brand, p=0.75, list=FALSE)
oobTrain <- oob_CR1k[inTraining,]   
oobTest <- oob_CR1k[-inTraining,]   
# verify number of obs 
nrow(oobTrain) # 751
nrow(oobTest)  # 249

################
# Train control
################

# set cross validation#10 fold CROSS VALIDATION (TRAIN CONTROL)
fitControl <- trainControl(method="repeatedcv", number=10, repeats=1) 

# default(#TRAIN C50 model with tune length 5)
####C5.0#### performance metrics
set.seed(123)
oobC5.0fit <- trainControl( method="repeatedcv",number = 10 , repeats = 1,  returnResamp="all")
# Choose the features and classes
data(oobTrain)
x <- oob_CR[c("age","salary","credit","elevel","zipcode","car")]
y <- oob_CR$brand
#TRAIN C50 model with tune length 5

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20,25,30,35,40), .model="tree" )

c50fit1<- train(x=x,y=y,tuneGrid=grid,trControl=fitControl,method="C5.0",tuneLength = 5,verbose=FALSE)

c50fit1


#9898 samples
#6 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 8909, 8908, 8907, 8909, 8909, 8908, ... 
#Resampling results across tuning parameters:
  
#  winnow  trials  Accuracy   Kappa    
#FALSE    1      0.8604725  0.7109322
#FALSE    5      0.9160393  0.8222384
#FALSE   10      0.9188689  0.8278549
#FALSE   15      0.9226082  0.8357411
#FALSE   20      0.9215967  0.8338040
#FALSE   25      0.9215965  0.8340708
#FALSE   30      0.9215966  0.8344464
#FALSE   35      0.9214960  0.8341291
#FALSE   40      0.9222029  0.8356292
#TRUE    1      0.8621883  0.7147400
#TRUE    5      0.9196771  0.8297485
#TRUE   10      0.9206873  0.8314341
#TRUE   15      0.9227087  0.8359533
#TRUE   20      0.9221032  0.8347716
#TRUE   25      0.9227094  0.8360444###
#TRUE   30      0.9223050  0.8353056
#TRUE   35      0.9222040  0.8351002
#TRUE   40      0.9223050  0.8352734

#Tuning parameter 'model' was held constant at a value of tree
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were trials = 25, model = tree and winnow = TRUE.
plot(c50fit1)
varImp(c50fit1)
#C5.0 variable importance

#Overall
#salary   100.00
#age      100.00
#zipcode   91.22
#elevel    86.59
#credit    85.03
#car        0.00
 
#######Random Forest#######
set.seed(123)
# manual grid
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))                      
# fit
system.time(oobRFfit <- train(brand~.,data=oobTrain,method="rf",
                  importance=T,
                  trControl=fitControl,
                  tuneGrid=rfGrid))
#TRAINING RESULTS                        
#oobRFfit
#Random Forest 

#751 samples
#6 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 675, 677, 676, 675, 676, 675, ... 
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa    
#1     0.7882219  0.5087106
#2     0.8802172  0.7372556
#3     0.9001844  0.7827088
#4     0.9107814  0.8059195####
#5     0.9080607  0.7992041

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 4.

plot(oobRFfit)
varImp(oobRFfit)
#> varImp(oobRFfit)
#rf variable importance

#Overall
#salary  100.0000
#age      73.0655
#elevel    3.0194
#credit    2.1798
#zipcode   0.3383
#car       0.0000



##################
# Model selection
##################
##COMPARE MODELS
resample_results <- resamples(list(RandomForest = rfFitM1, C5.0 = c50fit1))
resample_results
resample_results$values
summary(resample_results)
bwplot(resample_results)
diff_results <- diff(resample_results)
summary(diff_results, metric = "accuracy")
compare_models(c50fit1, rfFitM1)


#-- oob_CR --# compare samples

oobFitComp1k <- resamples(list(rf=oobRFfit, C5.0 = c50fit1 ))
oobFitComp1k
#Call:
#  resamples.default(x = list(rf = oobRFfit, C5.0 = c50fit1))

#Models: rf, C5.0 
#Number of resamples: 10 
#Performance metrics: Accuracy, Kappa 
#Time estimates for: everything, final model fit 
# output summary metrics for tuned models 
summary(oobFitComp1k)

#Call:
#  summary.resamples(object = oobFitComp1k)

#Models: rf, C5.0 
#Number of resamples: 10 

#Accuracy 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf   0.8552632 0.8933333 0.9066501 0.9107814 0.9324324 0.9736842    0
#C5.0 0.9141414 0.9186047 0.9247459 0.9227094 0.9269837 0.9283552    0

#Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf   0.6769706 0.7627038 0.8020805 0.8059195 0.8530580 0.9434524    0
#C5.0 0.8170851 0.8267390 0.8409742 0.8360444 0.8460259 0.8478499    0

###Plot###
bwplot(oobFitComp1k)

diff_results <- diff(oobFitComp1k)
summary(diff_results, metric = "accuracy")

#Call:
#  summary.diff.resamples(object = diff_results, metric = "accuracy")

#p-value adjustment: bonferroni 
#Upper diagonal: estimates of the difference
#Lower diagonal: p-value for H0: difference = 0

#Accuracy 
#rf     C5.0    
#rf          -0.01193
#C5.0 0.3567         

#Kappa 
#rf     C5.0    
#rf          -0.03012
#C5.0 0.3007  
compare_models(c50fit1,oobRFfit )

#compare_models(c50fit1,oobRFfit )

#One Sample t-test

#data:  x
#t = 0.97144, df = 9, p-value = 0.3567
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  -0.01584830  0.03970418
#sample estimates:
#  mean of x 
#0.01192794 



############################
# Predict testSet/validation
############################
##PREDICTIONS
#Predict with RandomForest (rfFitM1) on CompleteResponses Data
testPredrf1 <-predict(oobRFfit, oobTest )
summary(testPredrf1) 
#summary(testPredrf1)
#0   1 
#87 162 
postResample(testPredrf1, oobTest$brand)
#> postResample(testPredrf1, oobTest$brand)
#Accuracy     Kappa 
#0.9437751 0.8776327 
confusionMatrix(testPredrf1, oobTest$brand)
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0  82   5
#1   9 153

#Accuracy : 0.9438          
#95% CI : (0.9075, 0.9689)
#No Information Rate : 0.6345          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.8776          

#Mcnemar's Test P-Value : 0.4227          
                                          
 #           Sensitivity : 0.9011          
  #          Specificity : 0.9684          
  #       Pos Pred Value : 0.9425          
 #        Neg Pred Value : 0.9444          
 #            Prevalence : 0.3655          
  #       Detection Rate : 0.3293          
 #  Detection Prevalence : 0.3494          
  #    Balanced Accuracy : 0.9347          
                                          
  #  'Positive' Class : 0     


#### FINAL PREDICTION ON SURVEY INCOMPLETE DATA
# predict with RF
rfPred1 <- predict(oobRFfit , (oob_IR))
#Get Predicted preference totals (survey incomplete)
summary(rfPred1)
#> summary(rfPred1)
#0    1 
#1915   3085   

summary(oob_CR$brand)
#> summary(oob_CR$brand)
#0    1 
#3744 6154 ##Sony is preferred brand
                   
# performace measurment
postResample(rfPred1, oob_IR$brand)
#> postResample(rfPred1, oobTest$brand)
#Accuracy        Kappa 
#0.39360000 0.01246084 
#there is no Y value, no ground truth, data is corrupt/incorrectly caputured

summary(oob_IR$brand)
#> summary(oob_IR$brand)
#0    1 
#4937 63


# plot predicted verses actual
plot(rfPred1, oob_IR$brand)










