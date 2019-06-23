# Data Analysis Tutorial
Data Analysis using Unsupervised learning -(Apriori and Cluster Analysis)
USING R STUDIO

# Data Reading
Simulated_Coles_Data <- read_excel("Simulated Coles Data.xlsx",
                                   +     sheet = "Coles Transactions")
                                                                    
# Find duplicate rows in Receipt ID
duplicated(Simulated_Coles_Data$ReceiptID)
which(duplicated(Simulated_Coles_Data$ReceiptID))

# 2:: checking missing value of Age
which(is.na(Simulated_Coles_Data$age))
summary(is.na(sum(Simulated_Coles_Data$age)))

  CHECKING AND REMOVING EACH VARIABLE OUTLIERS

# 1. Value Variable
boxplot(Simulated_Coles_Data$Value, horizontal = FALSE)
summary(Simulated_Coles_Data$Value)
length(Simulated_Coles_Data$Value)

outlietValueBoxplot = boxplot(Simulated_Coles_Data$Value, horizontal = TRUE)$out

# APRIORI ALGORITHM
# DEFAULT SUPPORT AND CONFIDENCE LEVEL WHICH IS :: SUPPORT: 0.1, CONFIDENCE: 0.8
defaultARules = apriori(impsimulatedColesData)
sort(defaultARules, decreasing = FALSE)
plot(defaultARules[1:2500])

# INSPECT the Rules  :: NOTE:: TOP 20 RULES
inspectDT(defaultARules[1:30]) 

# REQUIRED PACKAGES FOR CLUSTER ANALYSIS
install.packages("Hmisc")
install.packages('pvclust')
library(Hmisc)
library(pvclust)
