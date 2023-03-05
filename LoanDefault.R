
#laptop
setwd("G:/My Drive/LoanDefault")

# Import Data
library(readr)
  df <- read_csv("Loan_Default.csv")
View(Loan_Default)

# Packages
install.packages("caTools")
library(psych)
library(mosaic)
library(ggplot2)
library(data.table)
library(stringr)
library(DataExplorer)
library(dplyr)
library(caTools)
library(flexdashboard)

plot_intro(df)
plot_missing(df)
# Missing upfront_charges, Int_rate_spread, Rate_of_int, dtir1, Gender.

summary(df)
describe(df)
glimpse(df)
# 112031 - 0, and 36639 - 1. implies 1 is default.
table(df$Status)

colSums(is.na(df))

# see data type
str(df)


#Set up to look at proportion of catagorical data
df <- subset(df, select = -c(ID,LTV, dtir1,income,
                             property_value,term,Upfront_charges,Interest_rate_spread,
                             rate_of_interest,loan_amount))

# change age from a range to a int
df <- (transform(df, age = (as.numeric(factor(age, 
                                              levels = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"),
                                              labels = c(1, 2, 3, 4, 5, 6, 7)
)))))

# Put credit scores into a range
df$Credit_Score[df$Credit_Score >= 0 & df$Credit_Score <=629] <- "Bad"
df$Credit_Score[df$Credit_Score >= 630 & df$Credit_Score <= 689] <- "Fair"
df$Credit_Score[df$Credit_Score >= 690 & df$Credit_Score <= 719] <- "Good"
df$Credit_Score[df$Credit_Score >= 720 & df$Credit_Score <= 900] <- "Excellent"



for(i in colnames(df)) {
  print("----------------------")
  print(colnames(df[i]))
  print(prop.table(table(df$Status, df[[i]], useNA = "always")))
}



# look at missing values to default
index <- which(is.na(df$rate_of_interest)&is.na(df$Interest_rate_spread)&is.na(df$Upfront_charges))
table(df$Status[index])

# this determines there is a correlation between missing values and default


#index NA values to 1
df$missing <- 0
df$missing[index] <- 1

table(df$Status, df$missing)

#split data into train and test
split <- sample.split(df$Status, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)


#create model with missing values
dfTrain <- subset(test, select = c(Status, missing))


modelTrain <- glm(Status ~ ., data = dfTrain, family= "binomial")


anova(modelTrain, test = "Chisq")
summary(modelTrain)

#McFadden's Pseudo R^2
ll.null <- modelTrain$null.deviance/-2
ll.proposed <- modelTrain$deviance/-2

(ll.null - ll.proposed) / ll.null
# Train comes back with 0.981

# p-value
1 - pchisq(2*(ll.proposed - ll.null), df=(length(modelTrain$coefficients)-1))
#p=0


#now to look at train
dfTest <- subset(train, select = c(Status, missing))

modelTest <- glm(Status ~ ., data = dfTest, family = "binomial")


anova(modelTest, test = "Chisq")
summary(modelTest)


#McFadden's Pseudo R^2
ll.null <- modelTest$null.deviance/-2
ll.proposed <- modelTest$deviance/-2

(ll.null - ll.proposed) / ll.null
# test comes back with 0.9825

# p-value
1 - pchisq(2*(ll.proposed - ll.null), df=(length(modelTest$coefficients)-1))
#p=0


# logistic reg curve
ggplot(dfTrain, aes(x=missing, y=Status)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))



