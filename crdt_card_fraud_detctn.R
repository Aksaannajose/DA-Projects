#import the dataset
credit_card <- read.csv('C:\\Users\\SAMSUNG\\Downloads\\creditcard.csv')

#glance at the structure of the dataset
str(credit_card)

#convert class to a factor variable
credit_card$Class <- factor(credit_card$Class, levels = c(0, 1))

#get the summary of the data
summary(credit_card)

#count the missing values
sum(is.na(credit_card))

#-------------------------------------------------------------------------------

#get the distribution of fraud and legit transaction in the dataset
table(credit_card$Class)

#get the percentage of fraud and legit transaction in the dataset
prop.table(table(credit_card$Class))

#Pie chart of credit card transaction
labels <- c("legit", "fraud")
lables <- paste(labels, round(100*prop.table(table(credit_card$Class)), 2))
lables <- paste0(lables, "%")

pie(table(credit_card$Class), labels, col = c("orange", "red"),
    main = "Pie chart of Credit card transaction")

#-------------------------------------------------------------------------------

#no model predictions
predictions <- rep.int(0, nrow(credit_card))
predictions <- factor(predictions, levels = c(0, 1))

#install.packages('caret')
install.packages("caret")
library(caret)
#confusionMatrix(data = predictions.reference = credit_card$Class)
confusionMatrix(predictions, credit_card$Class)


#-------------------------------------------------------------------------------

library(dplyr)

set.seed(1)
credit_card = credit_card %>% sample_frac(0.1)

table(credit_card$Class)
library(ggplot2)


ggplot(data = credit_card, aes(x = V1, y = V2, color = factor(Class))) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = c("dodgerblue2", "red"))

#-------------------------------------------------------------------------------

#creating traing and test sers fraud detection model

install.packages('caTools')
library(caTools)
 
set.seed(123)

data_sample = sample.split(credit_card$Class, SplitRatio = 0.80)

train_data=subset(credit_card, data_sample==TRUE)
  
test_data=subset(credit_card, data_sample==FALSE)

dim(train_data)
dim(test_data)

#-------------------------------------------------------------------------------
#Random over sampling(ROS)
table(train_data$Class)

n_legit <- 20000
new_frac_legit <- 0.50
new_n_total <- n_legit/new_frac_legit # = 20000/7.50

install.packages('ROSE')
library(ROSE)

oversampling_result <- ovun.sample(Class ~ . ,
                                   data = train_data,
                                   method = "over",
                                   N = new_n_total,
                                   seed = 2019)





