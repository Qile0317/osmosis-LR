#Load Packages
library(base)
library(compiler)
library(tidyverse)

#note: there is definitely something wrong with the SI5 and SF5 data, two of the
#     _corresponding values are completely out of porportion. 

##imput data from experiment

#sweet potato initial
SI0 = c(2.15, 1.43, 2.01)
SI25 = c(2.5, 2.28, 2.56)
SI5 = c(2.21, 2.09, 2.35)
SI1 = c(1.31, 1.36, 1.17)
SI125 = c(2.24, NA, 1.67)

Sweet_potato_initial = c(SI0, SI125,SI25,SI5,SI1)

#potato initial
PI0 = c(3.23, 3.53, 3.91)
PI25 = c(3.77, 3.63, 4.48)
PI5 = c(4.53, 3.99, 3.02)
PI1 = c(2.49, 2.38, 2.54)
PI125 = c(4.68, 2.66, 2.69)

Potato_initial = c(PI0, PI125, PI25, PI5, PI1)

#sweet potato final 
SF0 = c(2.32, 1.55, 2.2)
SF25= c(2.69, 2.48, 2.73)
SF5<-c(2.36,2.26,2.5)
SF1<-c(1.39,1.42,1.25)
SF125<-c(2.45,NA,1.83)
Sweet_potato_final = c(SF0,SF125,SF25,SF5,SF1)

#potato final
PF0<-c(3.36,3.67,4.08)
PF25<-c(3.81,3.71,4.58)
PF5<-c(4.57,4.04,3.06)
PF1<-c(2.32,2.19,2.37)
PF125<-c(4.86,2.72,2.79)

Potato_final = c(PF0, PF125, PF25, PF5, PF1)

##percentage changes
#first differences
Potato_difference = (Potato_final-Potato_initial)
Sweet_potato_difference = (Sweet_potato_initial-Sweet_potato_final)

#Sweet Potato change
Sweet_potato_change= (Sweet_potato_difference/
                        Sweet_potato_initial)*100

#potato change 
Potato_change = (Potato_difference / Potato_initial)*100


# molarity column
molarity = c(0.00,0.00,0.00,0.125,0.125,0.125, 0.25,0.25,0.25,
             0.5,0.5,0.5, 1.00,1.00,1.00)

#combining all columns of data for presentation purposes
SP_Overall = data.frame(molarity,Sweet_potato_initial, 
                        Sweet_potato_final,Sweet_potato_difference,
                        Sweet_potato_change)
SP_Overall

PP_Overall = data.frame(molarity,Potato_initial, 
                        Potato_final,Potato_difference,
                        Potato_change)
PP_Overall

#Making into dataframes
SP=data.frame(molarity,Sweet_potato_change)
SP

PP=data.frame(molarity,Potato_change)
PP

#plots before LM for inspection
plot(molarity,Sweet_potato_change)
plot(molarity,Potato_change)

#Linear regression
#sweet potato
plot(molarity,Sweet_potato_change)
summary(lm(Sweet_potato_change ~ molarity))

#regular potato
plot(molarity,Potato_change)
summary(lm(Potato_change ~ molarity))

#Making scatterplot with combined data and lines:
Changes<-c(Sweet_potato_change,Potato_change)
Changes

molarityA <- c(molarity,molarity)
molarityA

plot(molarityA, Changes)
abline(-8.829,3.131)
abline(4.796,-11.081)

#identify type
SPI<-c("Sweet_Potato","Sweet_Potato","Sweet_Potato","Sweet_Potato","Sweet_Potato",
       "Sweet_Potato","Sweet_Potato","Sweet_Potato","Sweet_Potato","Sweet_Potato",
       "Sweet_Potato","Sweet_Potato","Sweet_Potato","Sweet_Potato","Sweet_Potato")
PI <- c("Potato","Potato","Potato","Potato","Potato",
        "Potato","Potato","Potato","Potato","Potato",
        "Potato","Potato","Potato","Potato","Potato")
TPI <- c(SPI,PI)

Changes<-c(Sweet_potato_change,Potato_change)
Changes

molarityA <- c(molarity,molarity)
molarityA


Final<-data.frame(molarityA,Changes)
Final

plot(Final)
abline(-8.503,6.565)
abline(4.303,-8.593)

Final2<-data.frame(Final,TPI)
Final2

ggplot(Final2, aes(molarityA,Changes,shape=TPI, colour=TPI, fill=TPI)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("molarity") +
  ylab("%change in mass") +
  ggtitle("Sweet Potato vs Potato Osmosis")
