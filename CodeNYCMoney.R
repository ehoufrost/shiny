city_data <- read.csv("Citywide_Payroll_Data__Fiscal_Year_.csv", header = T)

install.packages("caret")

install.packages("ggplot")

library(caret)

library(tidyverse)

library(dplyr)

library(ggplot2)


negative_base <- city_data %>% filter(., Regular.Gross.Paid <= 0)
#1280 have regular pay less than 0
#7451 have pay at zero or less

negative_total <- city_data %>% filter(., Regular.Gross.Paid + Total.OT.Paid + Total.Other.Pay <= 0)

view(negative_money)
#1631 people make negative amount of money overall 

money_db <- city_data %>% filter(., Regular.Gross.Paid + Total.OT.Paid + Total.Other.Pay > 0) 

money_db <- mutate(money_db, total.pay = Regular.Gross.Paid + Total.OT.Paid + Total.Other.Pay)


#convert the start date column into a date type

money_db$Agency.Start.Date = as.Date(money_db$Agency.Start.Date , format = "%m/%d/%Y")

#create column that sees how long a person has been working in years
 
money_db <- mutate(money_db, days_working = as.Date("12/31/"+Fiscal.Year))

#get rid of people that started work before 1950

money_db <- money_db %>% filter(., money_db$Agency.Start.Date > as.Date("01/01/1950", format = "%m/%d/%Y"))

#get rid of people that somehow started work in the future

money_db <- money_db %>% filter(., money_db$Agency.Start.Date < as.Date("01/01/2019", format = "%m/%d/%Y"))

#get just the year

money_db <- mutate(money_db, year.start = format(as.Date(money_db$Agency.Start.Date, format="%d/%m/%Y"),"%Y"))

#get rid of whitespace in locations

money_db$Work.Location.Borough <- str_trim(money_db$Work.Location.Borough)

#all lowercase in locations

money_db$Work.Location.Borough <- toupper(money_db$Work.Location.Borough)

#see how many year people have been working for the city

money_db <- mutate(money_db, years.working = as.numeric(money_db$Fiscal.Year)- as.numeric(money_db$year.start) )

money_db <- filter(money_db, money_db$years.working >= 0)

test_other <- filter(money_db, is.na(money_db$Work.Location.Borough)==FALSE)

#lowercase all values
test_other$Work.Location.Borough <- tolower(test_other$Work.Location.Borough)

#make it so that only the 5 boroughs and "other show up"
five <- c("MANHATTAN","RICHMOND","BROOKLYN","BRONX","QUEENS")
fiveboroughs <- filter(money_db, money_db$Work.Location.Borough == five)

#scatterplot money vs experience
completeVec_poly <- complete.cases(money_db)


poly.moneyexperience <-poly((completeVec_poly$total.pay) ~ complete$years.working, money_db)


moneyexperience <- ggplot(money_db, aes(x=money_db$years.working, y=money_db$total.pay)) + geom_point() +
geom_line(data = log.moneyexperience, aes(x=money_db$years.working, y=money_db$total.pay, color = "Log Model"), size = 1, linetype = 2) 

moneyexperience


#money by place of work

pay_by_place <- ggplot(fiveboroughs, aes(x=fiveboroughs$Work.Location.Borough, y=fiveboroughs$total.pay), accending = TRUE) + 
  geom_boxplot() +
  ylim(0,150000)+
  ggtitle("Salary by NYC borough")+
  labs(y= "salary", x = "borough")
pay_by_place



#where people are working

recent_work <- filter(money_db,money_db$Fiscal.Year == 2018)
fiveboroughscount <- filter(recent_work, recent_work$Work.Location.Borough == five)






