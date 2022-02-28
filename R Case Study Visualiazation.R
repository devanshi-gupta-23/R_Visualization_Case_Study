setwd('C:/Users/user/Videos/Data Science 360 course/R/case study R/R - Visualization case study/R case study 3 (Visualization)')

#####################
# Dataset #
#####################
df <- read.csv('SalesData.csv')
#View(df)

#######################################
##### loading the libraries ###########
#######################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(plotrix)

######################## (1) ############################

df1 <- df[,c('Region','Sales2015','Sales2016')]
df2 <- gather(df1,key = 'salesyear',value = 'sales',2:3)
df3 <- df2 %>% dplyr::group_by(Region,salesyear) %>% dplyr::summarise(sales=round(sum(sales,na.rm = T),0))
ggplot(data=df3,aes(x=Region, y=sales, fill=salesyear))+
  geom_bar(stat = 'identity', position = position_dodge())+geom_text(aes(label=sales))+
  scale_y_continuous(labels = scales::comma)

######################## (2) ############################

d2 <- df %>% dplyr::group_by(Region) %>% dplyr::summarise(sales2016=round(sum(Sales2016,na.rm = T),0))


mylabels = paste0(d2$Region," - ", round((d2$sales2016 / sum(d2$sales2016))*100,1),"%")
pie(d2$sales2016, mylabels)

pie3D(d2$sales2016,labels = mylabels ,explode = 0.1, col = c("lightskyblue","royalblue3","turquoise4")) +
  title('Sales 2016')

######################## (3) ############################


df4 <- df[,c('Region','Tier','Sales2015','Sales2016')]
df5 <- gather(df4,key = 'salesyear',value = 'sales',3:4)
df6 <- df5 %>% dplyr::group_by(Region,Tier,salesyear) %>% dplyr::summarise(sales=round(sum(sales,na.rm = T),0))

ggplot2::ggplot(data = df6) + 
  aes(x = Tier , y = sales, fill = salesyear) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_grid(.~Region)+ scale_y_continuous(labels = scales::comma)


######################## (4) ############################

filter1 <- df[df$Region == 'East',]
View(filter1)

df7 <- filter1[,c('State','Sales2015','Sales2016')]
df8 <- gather(df7,key = 'salesyear',value = 'sales',2:3)
df9 <- df8 %>% dplyr::group_by(State,salesyear) %>% dplyr::summarise(sales=round(sum(sales,na.rm = T),0))

ggplot2::ggplot(data = df9) + 
  aes(x = State , y = sales, fill = salesyear) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_y_continuous(labels = scales::comma)

# In East Region, NY State registered a decline in sales 2016.

######################## (5) ############################

filter2 <- df[df$Tier == 'High',]
df10 <- filter2[,c('Division','Units2015','Units2016')]
df11 <- gather(df10,key = 'Unitsyear',value = 'Units',2:3)
df12 <- df11 %>% dplyr::group_by(Division,Unitsyear) %>% dplyr::summarise(TotalUnits=sum(Units,na.rm = T))

ggplot2::ggplot(data = df12) + 
  aes(x = Division , y = TotalUnits, fill = Unitsyear) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_y_continuous(labels = scales::comma)+ coord_flip()

# No such division

######################## (6) ############################

df$Qtr <- ifelse(df$Month=='Jan'|df$Month=='Feb'|df$Month=='Mar','Q1',
                                     ifelse(df$Month=='Apr'|df$Month=='May'|df$Month=='Jun','Q2',
                                            ifelse(df$Month=='Jul'|df$Month=='Aug'|df$Month=='Sep','Q3','Q4')))

######################## (7) ############################

agg6 <- df %>% dplyr::group_by(Qtr) %>% dplyr::summarise(TotalSales2015 = sum(Sales2015,na.rm=T),
                                                             TotalSales2016 = sum(Sales2016,na.rm=T))
df13 = gather(agg6,key = Year, value = Sales,-Qtr)

ggplot2::ggplot(data = df13) + 
  aes(x = Qtr , y = Sales, fill = Year) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_y_continuous(labels = scales::comma)

######################## (8) ############################

q1.8 <-  df %>% dplyr::group_by(Qtr,Tier )%>% dplyr::filter(Qtr=="Q1") %>% dplyr::summarise(TotalSales2015 = sum(Sales2015))
q2.8 <-  df %>% dplyr::group_by(Qtr,Tier) %>% dplyr::filter(Qtr=="Q2") %>% dplyr::summarise(TotalSales2015 = sum(Sales2015))
q3.8 <-  df %>% dplyr::group_by(Qtr,Tier) %>% dplyr::filter(Qtr=="Q3") %>% dplyr::summarise(TotalSales2015 = sum(Sales2015))
q4.8 <-  df %>% dplyr::group_by(Qtr,Tier) %>% dplyr::filter(Qtr=="Q4")%>% dplyr::summarise(TotalSales2015 = sum(Sales2015))

percent.1 <-  round(q1.8$TotalSales2015/sum(q1.8$TotalSales2015)*100,1)
percent.2 <-  round(q2.8$TotalSales2015/sum(q2.8$TotalSales2015)*100,1)
percent.3 <-  round(q3.8$TotalSales2015/sum(q3.8$TotalSales2015)*100,1)
percent.4 <-  round(q4.8$TotalSales2015/sum(q4.8$TotalSales2015)*100,1)


label1 <-  q1.8$Tier%>%paste(":",percent.1)%>%paste("%",sep = "")         
label2 <-  q2.8$Tier%>%paste(":",percent.2)%>%paste("%",sep = "")         
label3 <-  q3.8$Tier%>%paste(":",percent.3)%>%paste("%",sep = "")         
label4 <-  q4.8$Tier%>%paste(":",percent.4)%>%paste("%",sep = "")         

par(mfrow = c(2,2)) 
pie(q1.8$TotalSales2015,labels = label1 ,radius = 1,main = "Qtr 1")
pie(q2.8$TotalSales2015,labels = label2 ,radius = 1,main = "Qtr 2")
pie(q3.8$TotalSales2015,labels = label3, radius = 1,main = "Qtr 3")
pie(q4.8$TotalSales2015,labels = label4 ,radius = 1,main = "Qtr 4")



