#Import data
insurance_loss <- read.csv("D:/Dibimbing/Resources/Insurance_Loss.csv")
head(insurance_loss)

#check outlier
str(insurance_loss)
boxplot(insurance_loss$Losses)

#check missing value
summary(insurance_loss)

#create dummy variables
library(fastDummies)
insurance_loss <- dummy_cols(insurance_loss, select_columns = 'Gender')

insurance_loss <- dummy_cols(insurance_loss, select_columns = 'Married')

insurance_loss <- dummy_cols(insurance_loss, select_columns = 'Fuel.Type')

head(insurance_loss)


#percentage gender
library(dplyr)
table(insurance_loss$Gender)
percentage_female = paste0(round(7747/15290*100), '%')
percentage_male = paste0(round(7543/15290*100), '%')

aggregate(x=insurance_loss$Losses, by=list(insurance_loss$Gender), FUN=sum)

#fuel type
aggregate(x=insurance_loss$Losses, by=list(insurance_loss$Fuel.Type), FUN=sum)

#married
aggregate(x=insurance_loss$Losses, by=list(insurance_loss$Married), FUN=sum)

aggregate(x=insurance_loss$Losses, by=list(insurance_loss$Gender,
                                           insurance_loss$Married), FUN=sum)

#correlation
cor(insurance_loss$Losses,insurance_loss$Years.of.Driving.Experience)
cor.test(insurance_loss$Losses,insurance_loss$Years.of.Driving.Experience)

cor_insurance_loss = select(insurance_loss, Age, Years.of.Driving.Experience,
                            Vehicle.Age, Losses)
library(corrplot)
corrplot(cor(cor_insurance_loss),method="number")


