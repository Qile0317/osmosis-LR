#Load Packages
library(base)
library(compiler)

#note: there is definitely something wrong with the SI5 and SF5 data, two of the
#     _corresponding values are completely out of porportion. 
##also, I need to make a ggplot with two different colors for different potatoes
# for better presentation, of course.

##imput data from experiment

#sweet potato initial
SI0 = c(3.485, 3.431, 3.473)
SI25 = c(3.983, 3.888, 3.912)
SI5 = c(4.637, 3.995, 4.253)
SI75 = c(3.637, 3.613, 3.527)
SI1 = c(5.026, 5.554, 4.441)

Sweet_potato_initial = c(SI0, SI25,SI5,SI75,SI1)

#potato initial
PI1 = c(4.916, 4.9, 5.584)
PI75 = c(4.616, 4.691, 4.537)
PI5 = c(4.192, 4.336, 4.125)
PI25 = c(3.816, 3.869, 3.952)
PI0 = c(3.438, 3.742, 3.749)

Potato_initial = c(PI0, PI25, PI5, PI75, PI1)

#sweet potato final 
SF0 = c(3.734, 3.773, 3.787)
SF25= c(4.203, 4.27, 4.185)
SF5<-c(4.23,4.614,4.503)
SF75<-c(3.754,3.746,3.658)
SF1<-c(5.156,5.662,4.568)
Sweet_potato_final = c(SF0,SF25,SF5,SF75,SF1)

#potato final
PF1<-c(4.731,4.675,5.314)
PF75<-c(4.478,4.586,4.426)
PF5<-c(4.192,4.412,4.121)
PF25<-c(3.919,3.983,4.054)
PF0<-c(3.573,3.868,3.891)

Potato_final = c(PF0, PF25, PF5, PF75, PF1)

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
molarity = c(0.00,0.00,0.00,0.25,0.25,0.25, 0.50,0.50,0.50,
             0.75,0.75,0.75, 1.00,1.00,1.00)

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
abline(-8.503,6.565)

#regular potato
plot(molarity,Potato_change)
summary(lm(Potato_change ~ molarity))
abline(4.303,-8.593)

#Making scatterplot with combined data and lines:
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

#ggplot with colours and labels:

ggplot(Final2, aes(molarityA,Changes,shape=TPI, colour=TPI, fill=TPI)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("molarity") +
  ylab("%change in mass") +
  ggtitle("Sweet Potato vs Potato Osmosis")