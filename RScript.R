

library(VIM)
library(mice)
library(dplyr)

# Data Reading
Simulated_Coles_Data <- read_excel("Simulated Coles Data.xlsx",
                                   +     sheet = "Coles Transactions")


#copy of Data Object
Simulated_Coles_Data_Copy = Simulated_Coles_Data


# Summary of Data
summary(Simulated_Coles_Data)
str(Simulated_Coles_Data)
nrow(Simulated_Coles_Data_Copy)
ncol(Simulated_Coles_Data)
sum(is.na(Simulated_Coles_Data))


#average value of missing data
proportionData = function(xValue) {sum(is.na(xValue))/length(xValue)*100}
apply(Simulated_Coles_Data, 2, proportionData)
sum(apply(Simulated_Coles_Data, 2, proportionData))


# display of missing values in pattern
md.pattern(Simulated_Coles_Data)
md.pairs(Simulated_Coles_Data)
plot(is.na(Simulated_Coles_Data$fruit))



# Find duplicate rows in Receipt ID
duplicated(Simulated_Coles_Data$ReceiptID)
which(duplicated(Simulated_Coles_Data$ReceiptID))

nrow(Simulated_Coles_Data)
ncol(Simulated_Coles_Data)
# There are 09 duplicate values in Receipt ID.


# Remove duplicate rows of the dataframe using ReceiptID variable
Simulated_Coles_Data = distinct(Simulated_Coles_Data,ReceiptID, .keep_all= TRUE)
nrow(Simulated_Coles_Data)

# find na values of each variable and so on
which(is.na(Simulated_Coles_Data$Value))
which(is.na(Simulated_Coles_Data$pmethod))
which(is.na(Simulated_Coles_Data$sex))
which(is.na(Simulated_Coles_Data$homeown))
which(is.na(Simulated_Coles_Data$income))
which(is.na(Simulated_Coles_Data$age))
which(is.na(Simulated_Coles_Data$PostCode))
sum(is.na(Simulated_Coles_Data))


# total sum of na values
sum(is.na(Simulated_Coles_Data$PostCode))

# checking and Replacing SOME missing variable values with mean, as it has only very small amount of missing values.


# 1:: Income have 1 missing values
boxplot(is.na(Simulated_Coles_Data$income))
which(is.na(Simulated_Coles_Data$income))
summary(is.na(sum(Simulated_Coles_Data$income)))

#Replace Income missing value with mean
Simulated_Coles_Data$income[is.na(Simulated_Coles_Data$income)] <- round(mean(Simulated_Coles_Data$income, na.rm = TRUE))


# 2:: checking missing value of Age
which(is.na(Simulated_Coles_Data$age))
summary(is.na(sum(Simulated_Coles_Data$age)))

#Replace age missing value with mean
Simulated_Coles_Data$age[is.na(Simulated_Coles_Data$age)] <- round(mean(Simulated_Coles_Data$age, na.rm = TRUE))



# 3:: checking missing value of fruit
is.na(Simulated_Coles_Data$fruit)
which(is.na(Simulated_Coles_Data$fruit))
summary(is.na(sum(Simulated_Coles_Data$fruit)))
sum(is.na(Simulated_Coles_Data$fruit))

#Replace fruit missing value as it's only have 02 values

# as fruit has 02 NA's,so we can replace with 0
fruit = as.factor(Simulated_Coles_Data$fruit)
summary(fruit)
# Replacing with 0
Simulated_Coles_Data$fruit[is.na(Simulated_Coles_Data$fruit)] = 0


# 4:: Replace cannedveg missing value as it's only have 01 values
# as cannedveg has 01 NA's, replace with 0

which(is.na(Simulated_Coles_Data$cannedveg))
cannedveg=as.factor(Simulated_Coles_Data$cannedveg)
summary(cannedveg)
# Replacing with 0
Simulated_Coles_Data$cannedveg[is.na(Simulated_Coles_Data$cannedveg)] = 0

# 5:: Replace cereal missing value as it's only have 09 values
# as cereal has 09 NA's, replace with 0

which(is.na(Simulated_Coles_Data$cereal))
sum(is.na(Simulated_Coles_Data$cereal))
cereal=as.factor(Simulated_Coles_Data$cereal)
summary(cereal)
# Replacing with 0
Simulated_Coles_Data$cereal[is.na(Simulated_Coles_Data$cereal)] = 0


# 6:: Replace PizzaBase missing value as it's only have 01 values
# as PizzaBase has 01 NA's, replace with 0

which(is.na(Simulated_Coles_Data$PizzaBase))
sum(is.na(Simulated_Coles_Data$PizzaBase))
PizzaBase=as.factor(Simulated_Coles_Data$PizzaBase)
summary(PizzaBase)
# Replacing with 0
Simulated_Coles_Data$PizzaBase[is.na(Simulated_Coles_Data$PizzaBase)] = 0


# 7:: Replace milk missing value as it's only have 01 values
# as milk has 01 NA's, replace with 0

which(is.na(Simulated_Coles_Data$milk))
sum(is.na(Simulated_Coles_Data$milk))
milk=as.factor(Simulated_Coles_Data$milk)
summary(milk)
# Replacing with 0
Simulated_Coles_Data$milk[is.na(Simulated_Coles_Data$milk)] = 0


# 8:: Replace confectionery missing value as it's only have 01 values
# as confectionery has 01 NA's, replace with 0

which(is.na(Simulated_Coles_Data$confectionery))
sum(is.na(Simulated_Coles_Data$confectionery))
confectionery=as.factor(Simulated_Coles_Data$confectionery)
summary(confectionery)
# Replacing with 0
Simulated_Coles_Data$confectionery[is.na(Simulated_Coles_Data$confectionery)] = 0


# 9:: Replace nchildren with median
Simulated_Coles_Data$nchildren[is.na(Simulated_Coles_Data$nchildren)] <- round(median(Simulated_Coles_Data$nchildren, na.rm = TRUE))
which(is.na(Simulated_Coles_Data$nchildren))
sum(is.na(Simulated_Coles_Data$nchildren))


# 10:: Dropping Variable Post Code :: Huge amount NA's (17%)
which(is.na(Simulated_Coles_Data$PostCode))
sum(is.na(Simulated_Coles_Data$PostCode))
summary(Simulated_Coles_Data$PostCode)

# Dropping the column :: PostCode
Simulated_Coles_Data = subset(Simulated_Coles_Data, select = -c(PostCode))

#  MISSING VALUES END  :: //////////////  *************** //////////

######------------------------------------------#######-----------------------------------------------------######
######------------------------------------------#######-----------------------------------------------------######
######------------------------------------------#######-----------------------------------------------------######

# ///////////////******************************////////////////////
#
#               .***      OUTLIERS DEALING . ***    //////////////
#
#///////////////*******************************///////////////////

#  CHECKING AND REMOVING EACH VARIABLE OUTLIERS

# 1. Value Variable
boxplot(Simulated_Coles_Data$Value, horizontal = FALSE)
summary(Simulated_Coles_Data$Value)
length(Simulated_Coles_Data$Value)

outlietValueBoxplot = boxplot(Simulated_Coles_Data$Value, horizontal = TRUE)$out

# Find INNER fence of UPPER BOUND of Value Variable:: Q3-Q1
# IQR = 116.0000 - 29.5337 = 86

IQR_Value = 116.0000 - 29.5337
valueInnerFence = round(116.000 + (1.5*IQR_Value)) 


# Find OUTER fence of UPPER BOUND of Value Variable:: Q3-Q1
valueOuterFence = round(116.000 + (3*86.4663))

# Quantiles of Value
quantile(Simulated_Coles_Data$Value)
dim(Simulated_Coles_Data)

# Discard Outliers 
outliersValueVariableDataSet  = subset(Simulated_Coles_Data,Simulated_Coles_Data$Value <valueOuterFence )
outliersValueVariableDataSet
# update Dataset
Simulated_Coles_Data = outliersValueVariableDataSet

# Verify DataSet
dim(Simulated_Coles_Data)
quantile(Simulated_Coles_Data$Value)
boxplot(Simulated_Coles_Data$Value, horizontal = TRUE)

#.................................////////

# 2. Payment Method Variable

boxplot.stats(Simulated_Coles_Data$pmethod)
summary(Simulated_Coles_Data$pmethod)
length(Simulated_Coles_Data$pmethod)
quantile(Simulated_Coles_Data$pmethod)

Simulated_Coles_Data$pmethod[!is.na(Simulated_Coles_Data$pmethod)&Simulated_Coles_Data$pmethod>4]= NA
summary(Simulated_Coles_Data)

# Convert to Catatrgorical Variabl to evaluate the Outliers 
Simulated_Coles_Data$pmethod = c(Simulated_Coles_Data$pmethod)
summary(Simulated_Coles_Data$pmethod)
is.factor("pmethod")
(Simulated_Coles_Data$pmethod = factor(Simulated_Coles_Data$pmethod))
levels(Simulated_Coles_Data$pmethod)
summary(Simulated_Coles_Data$pmethod)

# impute 2 to NA's as this number is HIGHEST
Simulated_Coles_Data$pmethod[is.na(Simulated_Coles_Data$pmethod)]="2"

paymentM = summary(Simulated_Coles_Data$pmethod)
barplot(paymentM, xlab="Payment Methods", ylab="Transactions", col=c("#CCCCCC","#104E8B", "#9DACBB"), names=c("Cash", "Credit Card", "Eftpos", "Others"))

#................................. //////// .........................



# 3. Sex Variable
boxplot(Simulated_Coles_Data$sex, horizontal = TRUE)
boxplot.stats(Simulated_Coles_Data$sex)
summary(Simulated_Coles_Data$sex)
length(Simulated_Coles_Data$sex)
quantile(Simulated_Coles_Data$sex)

# NO OUTLIERS

#.................................////////..................


# 4. Home Town  Variable
boxplot(Simulated_Coles_Data$homeown, horizontal = TRUE)
boxplot.stats(Simulated_Coles_Data$homeown)
summary(Simulated_Coles_Data$homeown)
length(Simulated_Coles_Data$homeown)
quantile(Simulated_Coles_Data$homeown)

Simulated_Coles_Data$homeown[!is.na(Simulated_Coles_Data$homeown)&Simulated_Coles_Data$homeown>3]= NA
summary(Simulated_Coles_Data)


# Convert to Catatrgorical Variabl to evaluate the Outliers 
Simulated_Coles_Data$homeown = c(Simulated_Coles_Data$homeown)
summary(Simulated_Coles_Data$homeown)
is.factor("homeown")
(Simulated_Coles_Data$homeown = factor(Simulated_Coles_Data$homeown))
levels(Simulated_Coles_Data$homeown)
summary(Simulated_Coles_Data$homeown)

# impute 1 to NA's as this number is HIGHEST
Simulated_Coles_Data$homeown[is.na(Simulated_Coles_Data$homeown)]= "1"

homeown = summary(Simulated_Coles_Data$homeown)
barplot(homeown, xlab="Home Town", ylab="Value", col=c("#CCCCCC","#104E8B"), names=c("YES", "No", "Unknown"))


#.................................////////..................


# 5. Income  Variable

boxplot(Simulated_Coles_Data$income)
boxplot.stats(Simulated_Coles_Data$income)

summary(Simulated_Coles_Data$income)
length(Simulated_Coles_Data$income)
quantile(Simulated_Coles_Data$income)

# # Find INNER fence of UPPER BOUND of INCOME Variable:: Q3-Q1
IQR_Income = IQR(Simulated_Coles_Data$income)
IncomeOuterFence = round(75379.04 + (1.5*IQR_Income))
IncomeOuterFence

# Quantiles of Value
quantile(Simulated_Coles_Data$income)
dim(Simulated_Coles_Data)

which(summary(Simulated_Coles_Data$income) > IncomeOuterFence)
sum(Simulated_Coles_Data$income > IncomeOuterFence)

# As 6162 records are more than the income OUTFERENCE, Repleace with the MEDIAN.   

medianIncome = round(median(Simulated_Coles_Data$income))
print(medianIncome)

Simulated_Coles_Data$income[which(Simulated_Coles_Data$income>IncomeOuterFence)] = medianIncome
quantile(Simulated_Coles_Data$income)

# Still have LOWER FENCE outlier, Need to replace less than LOWER BOUND INNER FENCE
# Find LOWER BOUND INNER FENCE
# Equation is: Lower bound (INNER FENCE) = Q1 - (1.5 * (Q3-Q1))
quantile(Simulated_Coles_Data$income)
incomeLowerBoundFence = round(65563.06 - (1.5*IQR_Income))

# As 3492 records are less than incomeLowerBoundFence Repleace with the MEDIAN.
Simulated_Coles_Data$income[which(Simulated_Coles_Data$income<incomeLowerBoundFence)] = medianIncome

# Verify DataSet
dim(Simulated_Coles_Data)
quantile(Simulated_Coles_Data$income)
boxplot(Simulated_Coles_Data$income, horizontal = TRUE)

#.................................////////..................


# 6. Age  Variable
boxplot(Simulated_Coles_Data$age, horizontal = TRUE)
boxplot.stats(Simulated_Coles_Data$age)
summary(Simulated_Coles_Data$age)
length(Simulated_Coles_Data$age)
quantile(Simulated_Coles_Data$age)

# Round the AGE
Simulated_Coles_Data$age <- round(Simulated_Coles_Data$age)
customerAge = Simulated_Coles_Data$age
hist(customerAge, breaks=c(0,10,20,30,40,50,60,70,80,90,100), xlab ="Age", 
     ylab="Numbers", main="Age Display", col=c("#CCCCCC","#104E8B"))

#.................................////////..................



# 7. nChildren  Variable

boxplot(Simulated_Coles_Data$nchildren)
boxplot.stats(Simulated_Coles_Data$nchildren)

summary(Simulated_Coles_Data$nchildren)
length(Simulated_Coles_Data$nchildren)
quantile(Simulated_Coles_Data$nchildren)

mediannChildren = round(median(Simulated_Coles_Data$nchildren))
mediannChildren

sum(Simulated_Coles_Data$nchildren[which(Simulated_Coles_Data$nchildren>5)])
Simulated_Coles_Data$nchildren[which(Simulated_Coles_Data$nchildren>5)] = mediannChildren

quantile(Simulated_Coles_Data$nchildren)
hist(Simulated_Coles_Data$nchildren,xlab="No of Childrens", main="No of Childern per customer", col=c("#CCCCCC","#104E8B"), horizontal = TRUE)


# Verify DataSet
dim(Simulated_Coles_Data)
quantile(Simulated_Coles_Data$nchildren)
boxplot(Simulated_Coles_Data$nchildren, horizontal = TRUE)

#.................................////////..................


######------------------------------------------#######-----------------------------------------------------######
######------------------------------------------#######-----------------------------------------------------######
######------------------------------------------#######-----------------------------------------------------######

# EXPORT FILE

write.csv(Simulated_Coles_Data,file = "G:/SimulatedColesExportData.csv", row.names = F)


# ///////////////******************************///////////////////
#
#               .***      MARKET BASKET ANALYSIS . ***    ////////
#
#///////////////*******************************///////////////////

# Install Arules Package
install.packages("arules")

library(Matrix)
library(arules)
library(arulesViz)


# READ EXPORTED FILED --- PRODUCTS INFO
importedData = read.csv("Purchases_Info.csv", header = TRUE, colClasses = "factor", sep = ',')

class(importedData)

# convert to 'transactions' class
impsimulatedColesData = as (importedData, "transactions") 
class(impsimulatedColesData)
str(impsimulatedColesData)
summary(impsimulatedColesData)


# DEFAULT SUPPORT AND CONFIDENCE LEVEL WHICH IS :: SUPPORT: 0.1, CONFIDENCE: 0.8
defaultARules = apriori(impsimulatedColesData)
sort(defaultARules, decreasing = FALSE)
plot(defaultARules[1:2500])

# INSPECT the Rules  :: NOTE:: TOP 20 RULES
inspectDT(defaultARules[1:30])

######------------------------------------------#######-----------------------------------------------------######

# CHANGE THE CONFIDENCE TO HIGHEST LEVEL 
highestConfidenceRules1 = apriori(impsimulatedColesData, parameter = list(support = 0.1, conf = 0.9, minlen = 1, maxlen = 100))

# INSPECT the Rules  :: NOTE:: TOP 20 RULES
inspect(highestConfidenceRules1[1:20])
inspectDT(highestConfidenceRules1[1:20])
plot(highestConfidenceRules1[1:20])

######------------------------------------------#######-----------------------------------------------------######

# CHANGE THE SUPPORT TO HIGHEST LEVEL 
HighestSupportRules1 = apriori(impsimulatedColesData, parameter = list(support = 0.9, conf = .1, minlen = 1,  maxlen = 100))

# INSPECT the Rules  :: NOTE:: TOP 20 RULES
inspectDT(HighestSupportRules1[1:20])
plot(HighestSupportRules1[1:20])
plot(highestConfidenceRules1[1:10], method = "graph")

######------------------------------------------#######-----------------------------------------------------######

# INSPECT the Rules  :: NOTE:: TOP 20 FREQUENT ITEMS RULES
frequentItems <- eclat (impsimulatedColesData, parameter = list(supp = 0.9, maxlen = 15))
inspectDT(frequentItems[1:20])
plot(frequentItems[1:20])


itemFrequencyPlot(impsimulatedColesData, topN = 50, cex.names= .8, type = "absolute", main = "Item Frequency 50")

######------------------------------------------#######-----------------------------------------------------######


# /////////////// ******************************  ///////////////////
#
#               .***      CKUSTER ANALYSIS . ***  //////////////////
#
#/////////////// ******************************   ///////////////////


# REQUIRED PACKAGES
install.packages("Hmisc")
install.packages('pvclust')
library(Hmisc)
library(pvclust)

# DIMESNSIONS OF AN OBJECT
dim(Simulated_Coles_Data)

# READ EXPORTED FILED FOR CLUSTERING
customerInfoImportedData = read.csv("Customers_Info.csv", header = TRUE, sep = ',')
dim(customerInfoImportedData)
str(customerInfoImportedData)
nrow(customerInfoImportedData)

# AS DATA IS TOO LARGE< TAKEING A SAMPLE OF 6000
filteredSampleData=customerInfoImportedData[sample(nrow(customerInfoImportedData), "6000"), ] 
nrow(filteredSampleData)

######------------------------------------------#######-----------------------------------------------------######


#####---------------------------     HIERARCHICAL CLUSTRING      -------------------------------------------######

# CACLCULATE EUCLIDEAN DISTANCE
euDistance = dist(filteredSampleData)

# CLUSTER DENDROGRAM WITH COMPLETE LINKAGE
hierCluster = hclust(euDistance, method = "complete")
plot(hierCluster)
#CLUSTER MEMBERSHIP , LOOKING FOR 4 CLUSTERS
clusMembership = cutree(hierCluster, 1)
table(clusMembership)

# CHECK CLUSTER MEANS (INTERPETETION)
aggregate(filteredSampleData, list(clusMembership), mean)

# SILLHOUTEE PLOT
library(cluster)
plot(silhouette(cutree(clusMembership,4),euDistance))


######------------------------------------------#######-----------------------------------------------------######


#####---------------------------     K-MEAN CLUSTRING      -------------------------------------------######

# 04 CLUSTER
kMeanCluster = kmeans(filteredSampleData, 4)
filteredSampleData

# PLOT INCOME ~ AGE
plot(age~income, filteredSampleData, col = kMeanCluster$cluster)

# PLOT AGE ~ PMethod
plot(Value~age, filteredSampleData, col = kMeanCluster$cluster)


clusplot(filteredSampleData, kMeanCluster$cluster,
         main ="2D rep of cluster", shade = TRUE, lables = 2, line = 0)

######------------------------------------------#######-----------------------------------------------------######
