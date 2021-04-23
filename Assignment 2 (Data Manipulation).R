getwd()
setwd("D:/Dibimbing/Resources")
getwd()

library(dplyr)

salariesdata = read.csv('Salaries.csv')
salariesdata

dim(salariesdata)
str(salariesdata)

#Total employee
salariesdata %>% distinct(EmployeeName, .keep_all =  F)

#Year
salariesdata %>% distinct(Year, .keep_all =  F)

#Agency
salariesdata %>% distinct(Agency, .keep_all =  F)

#Total employee / year
filter(salariesdata, Year %in% 2011)
filter(salariesdata, Year %in% 2012)
filter(salariesdata, Year %in% 2013)
filter(salariesdata, Year %in% 2014)
  
#Average Base Pay / Year
salariesdata %>%
  group_by(Year) %>%
  summarise(average_basepay = mean(BasePay, na.rm = T))

#Average Overtime Pay / Year
salariesdata %>%
  group_by(Year) %>%
  summarise(average_overtimepay = mean(OvertimePay, na.rm = T))

#Average Total Pay / Year
salariesdata %>%
  group_by(Year) %>%
  summarise(average_totalpay = mean(TotalPay, na.rm = T))

#Average Total Pay Benefit / Year
salariesdata %>%
  group_by(Year) %>%
  summarise(average_totalpaybenefit = mean(TotalPayBenefits, na.rm = T))

#Average Base Pay
salariesdata %>%
  summarise(average_basepay = mean(BasePay, na.rm = T))

#Average Overtime Pay
salariesdata %>%
  summarise(average_overtimepay = mean(OvertimePay, na.rm = T))

#Average Total Pay
salariesdata %>%
  summarise(average_totalpay = mean(TotalPay, na.rm = T))

#Average Total Pay Benefit
salariesdata %>%
  summarise(average_totalpaybenefit = mean(TotalPayBenefits, na.rm = T))

#Max Total Pay Benefit / year
salariesdata %>%
  group_by(Year) %>%
  summarise(average_totalpaybenefit = max(TotalPayBenefits, na.rm = T))

#Min Total Pay Benefit /year
salariesdata %>%
  group_by(Year) %>%
  summarise(average_totalpaybenefit = min(TotalPayBenefits, na.rm = T))

#Max Total Pay Benefit
salariesdata %>%
  summarise(max_totalpaybenefit = max(TotalPayBenefits, na.rm = T))

#Min Total Pay Benefit
salariesdata %>%
  summarise(min_totalpaybenefit = min(TotalPayBenefits, na.rm = T))

#Employee with overtime pay
salariesdata %>%
  group_by(EmployeeName) %>%
  filter(OvertimePay > 0)

#Employee with other pay
salariesdata %>%
  group_by(EmployeeName) %>%
  filter(OtherPay > 0)

#Employee with benefits
salariesdata %>%
  group_by(EmployeeName) %>%
  filter(Benefits > 0)
