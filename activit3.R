# install.packages(c("dplyr","lubridate"))
# install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 = read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
climateChange = read.csv("/cloud/project/activity03/climate-change.csv")

climateChange$date = ymd(climateChange$Day)

colnames(datCO2)[4] = "CO2"
datCO2$Entity = as.factor(datCO2$Entity)

US = datCO2 %>%
  filter(Entity == "United States")

plot(US$Year,US$CO2, type="b",
     pch=19,
     xlab= "Year",
     ylab= "Fossil Fuel Emisions (billions of tons of c02)",
     yaxt="n")
axis(2, seq(0, 6000000000, by=2000000000),
    seq(0,6, by=2), las=2)

ggplot(US, aes(x=Year, y=CO2))+
  geom_point()+
  geom_line()+
  labs(y="US Fossil Fuel c02 Emissions (tons CO2)")+
  theme_classic()

NorthA = datCO2 %>%
  filter(Entity== "United States" | Entity== "Mexico" | Entity== "Canada")
  
ggplot(NorthA, aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  theme_classic()


northH = climateChange %>%
  filter(Entity == "Northern Hemisphere")
southH = climateChange %>%
  filter(Entity == "Southern Hemisphere")

plot(northH$date, northH$temperature_anomaly, 
     type="l",
     #pch=19,
     col= "lightblue",
     xlab= "Year",
     ylab= "Temperature Anomaly (Celcius)")
points(southH$date, southH$temperature_anomaly,
       type = "l",
       #pch = 19,
       col= "lightgreen")
legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("lightblue", "lightgreen"),
       pch=19, bty= "n")

hemispheres= climateChange %>%
  filter(Entity =="Northern Hemisphere" | Entity =="Southern Hemisphere")

ggplot(hemispheres, aes(x=date,y=temperature_anomaly, color=Entity))+
  geom_line()+
  labs(x="Year",y="Temperature Anomaly (Celcius)")+
  theme_classic()
sums= NorthA %>%
  group_by(Entity) %>%
summarise(sum(CO2))

barplot(sums$`sum(CO2)`, names.arg = sums$Entity, 
        ylab = ("Total CO2 Emissions (billions of tons of CO2)"), yaxt="n")
axis(2, seq(0, 5e+11, by=1e+11),
     seq(0,500, by=100), las=2)

#Homework 3
#Question 1.
countries = datCO2%>%
  group_by(Entity) %>%
  summarise(max(CO2))
southKorea = datCO2 %>%
  filter(Entity == "South Korea")
southKorea$CO2 = southKorea$CO2/1000000

ggplot(southKorea, aes(x=Year, y=CO2))+
  geom_line()+
  labs(y="C02 Emissions (Millions of Tons)", title="South Korea Fossil Fuel C02 Emissions")+
  theme_classic()+
  annotate("segment", # line label
               x=1997, # start x coordinate
               y=450, # start y coordinate
               xend=1997, # end x coordinate
               yend=500) + # end y coordinate
  annotate("text", # add text label
           x=1960, # center of label x coordinate
           y= 520, # center of label y coordinate
           label="Asian Financial Crisis") # label to add
# question 2
worldH = climateChange %>%
  filter(Entity == "World")
worldco2 = datCO2 %>%
  filter(Entity == "World") %>%
worldco2$CO2 = worldco2$CO2/1000000

ggplot(worldco2, aes(x=Year, y=CO2))+
  geom_line()+
  labs(y="C02 Emissions (Millions of Tons)", title="World Fossil Fuel C02 Emissions")+
  theme_classic()

ggplot(worldH, aes(x=date,y=temperature_anomaly))+
  geom_line()+
  labs(x="Year",y="Temperature Anomaly (Celcius)", title="World Temperature Anomoly")+
  theme_classic()

# Question 3
foodEmissions = read.csv("/cloud/project/question3/food-emissions-production-supply-chain.csv")
colnames(foodEmissions)[3] = "CO2"
ggplot()