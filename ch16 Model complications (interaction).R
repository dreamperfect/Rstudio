library(readxl)
library(dplyr)

ch16interactiondata<- read_excel("ch16.1 (General Linear Model (curvilinear, interaction, transforming y, nonlinear extrinsically linear (percentage change)).xlsx", 
                                 sheet = "Interaction Data table")

ch16interactiondata<-rename(ch16interactiondata, Price=Price...1,Advertising_Expenditure=`Advertising Expenditure ($1000s)...2`, Sales=`Sales (1000s)`, Price2=Price...4, Adv_exp_2=`Advertising Expenditure ($1000s)...5`, Sales2=`Sales (I000s)` )

ch16interactiondata


str(ch16interactiondata)

#Combine the excess columns

#Method 1
price<-ch16interactiondata$Price

price<-data.frame(price)

price

price2<-ch16interactiondata$Price2

price2<-data.frame(price=price2)

price2

bothprice<- bind_rows(price,price2)

bothprice


#Method 2 

dataframe=seq(1,24,1)

dataframe

dataframe<-data.frame(dataframe)

dataframe

dataframe$price<-c(ch16interactiondata$Price,ch16interactiondata$Price2)

dataframe

dataframe<-data.frame(price=dataframe$price)

dataframe

#Repeat whichever method for all excess columns

ch16interactiondata

ch16interactiondata$Advertising_Expenditure


dataframe$Adv_Expenditure<-c(ch16interactiondata$Advertising_Expenditure,ch16interactiondata$Adv_exp_2)

dataframe

dataframe$Sales<-c(ch16interactiondata$Sales,ch16interactiondata$Sales2)

dataframe

#Create pivot table

library(tidyr)

#Method1

pivottable <- pivot_wider(
  data = dataframe,
  id_cols = Adv_Expenditure,
  names_from = price,
  values_from = Sales,
  values_fn = list(Sales = mean)
)

pivottable

pivot<-as_tibble(pivottable)

pivot



#Method 2

# Create the pivot table
pivot_table <- dataframe %>%
  pivot_wider(
    names_from = price,
    values_from = Sales,
    names_prefix = "Price_",
    values_fn = mean
  )

# Print the pivot table
print(pivot_table)


##Visualize by mean sales of each price and advertising expenditure

grouped_data <- group_by(dataframe, price, Adv_Expenditure)

grouped_data



summary_data <- summarise(grouped_data, 
                          Mean_Sales = mean(Sales),
                          Median_Sales = median(Sales),
                          Max_Sales = max(Sales),
                          Min_Sales = min(Sales))


summary_data


summary_data$Adv_Expenditure<-factor(summary_data$Adv_Expenditure)

ggplot(summary_data, aes(x=price, y=Mean_Sales, color=Adv_Expenditure))+geom_point(size=3)+scale_x_continuous(limits=c(1.5,3.5),breaks=seq(1.5,3.5,0.5))


#Visualize by advertising expenditure and price 

summary_data

summary_data$Adv_Expenditure <- as.numeric(as.character(summary_data$Adv_Expenditure))

summary_data

summary_data$price<-as.factor(summary_data$price)

ggplot(summary_data, aes(x=Adv_Expenditure, y=Mean_Sales, color=price))+geom_point(size=3)+scale_x_continuous(limits=c(40,110),breaks=seq(40,110,10))



##Visualize all data points ##


#Comparison by Advertising Expenditure

str(dataframe)

dataframe$Adv_Expenditure<-factor(dataframe$Adv_Expenditure)


str(dataframe)

library(ggplot2)



ggplot(dataframe, aes(x=price, y=Sales,color=Adv_Expenditure))+geom_point(size=3)+scale_x_continuous(limits=c(1.5,3.5),breaks=seq(1.5,3.5,0.5))



dataframe

#Comparison by price

dataframe$price<-factor(dataframe$price)

str(dataframe)


ggplot(dataframe,aes(x=Adv_Expenditure,y=Sales,colour=price))+geom_point(size=3)


#Adress interaction:

dataframe

str(dataframe)

dataframe$Adv_Expenditure <- as.numeric(as.character(dataframe$Adv_Expenditure))

dataframe$price<- as.numeric(as.character(dataframe$price))

dataframe$x3<-dataframe$price*dataframe$Adv_Expenditure

dataframe

#Reorder so that sales is last column on right

dataframe <- dataframe[, c("price", "Adv_Expenditure", "x3", "Sales")]

dataframe

