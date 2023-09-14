##
##BikeShare EDA code
##

##Libraries
library(tidyverse)
library(gridExtra)
library(vroom)
bike <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")

#view(bike)
plot1 <- ggplot(data=bike, aes(x=temp, y=count)) +
  geom_point()+
  geom_smooth(se=FALSE)

plot2 <- ggplot()+
  geom_smooth(data=bike, aes(x=datetime, y=temp), color="blue")+
  geom_smooth(data=bike, aes(x=datetime, y =atemp ), color = "red")+
  labs(
    title = "Tempture(blue) vs feels like tempture(red)",
    x = "Date and Time",   # X-axis label
    y = "Temperature"      # Y-axis label
  )

plot3 <- ggplot()+
  geom_smooth(data=subset(bike, season == 1), aes(x= windspeed, y=humidity),alpha=0.5, fill="pink")+
  geom_smooth(data=subset(bike, season == 2), aes(x= windspeed, y=humidity),alpha=0.5, fill="green")+
  geom_smooth(data=subset(bike, season == 3), aes(x= windspeed, y=humidity),alpha=0.5, fill="orange")+
  geom_smooth(data=subset(bike, season == 4), aes(x= windspeed, y=humidity),alpha=0.5, fill="blue")+
  labs
    title = "windspeed vs humidity by season",
    x = "windspeed",   # X-axis label
    y = "Humidity"      # Y-axis label
  )
plot3

plot4 <- ggplot()+
  geom_smooth(data=bike, aes(x= datetime, y=humidity),se=False)+
 
plot4

grid.arrange(plot1, plot2,plot3,plot4, ncol = 2)
  