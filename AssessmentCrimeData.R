#Part 1


#check working directory
getwd()


#set new working directory
setwd("C:/Users/JOHNSON/Desktop/materialsfortheassignment")


#import your .csv file to the global environment
ACD <- read.csv("AssessmentCrimeData.csv", header=TRUE, sep = ",")

install.packages("glimpse")
glimpse(ACD)

#To get the summary of the data set
summary(ACD)

#To calculate the SD of all columns
sapply(ACD, sd)

par(mfrow = c(2,3))
hist(log10(ACD$Population/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Population Density')
hist(log10(ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Land Area(Hectares)')
hist(log10(ACD$Anti.Social.Behaviour/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10AntiSocialBehaviour')
hist(log10(ACD$Burglary/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Burglary Density')
hist(log10(ACD$Robbery/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Robbery Density')
hist(log10(ACD$Vehicle.Crimes/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Vehicle Crimes Density')

par(mfrow = c(2,3))
hist(log10(ACD$Violent.Crimes/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Vieolent Crimes Density')
hist(log10(ACD$Shoplifting/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Shoplifting Density')
hist(log10(ACD$Criminal.Damage...Arson/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Criminal Damage Density')
hist(log10(ACD$Other.Theft/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10 Other theft Density')
hist(log10(ACD$Drugs/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10 Drugs Density')
hist(log10(ACD$Other.Crimes/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10 Other Crimes Density')

par(mfrow = c(2,3))
hist(log10(ACD$Bike.Theft/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Bike Theft Density')
hist(log10(ACD$Possession.of.Weapons/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Possession of Weapons Density')
hist(log10(ACD$Public.Order/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Public Order Density')
hist(log10(ACD$Theft.From.the.Person/ACD$Land.Area.in.Hectares), main = '2019', xlab='log10Theft from the person Density')



#Part 2
#a)

attach(ACD)

#defining all models
model1 <- lm(log10(ACD$Anti.Social.Behaviour/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model2 <- lm(log10(ACD$Burglary/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model3 <- lm(log10(ACD$Robbery/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model4 <- lm(log10(ACD$Vehicle.Crimes/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model5 <- lm(log10(ACD$Violent.Crimes/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model6 <- lm(log10(ACD$Shoplifting/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model7 <- lm(log10(ACD$Criminal.Damage...Arson/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model8 <- lm(log10(ACD$Other.Theft/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model9 <- lm(log10(ACD$Drugs/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model10 <- lm(log10(ACD$Other.Crimes/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model11 <- lm(log10(ACD$Bike.Theft/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model12 <- lm(log10(ACD$Possession.of.Weapons/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model13 <- lm(log10(ACD$Public.Order/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))
model14 <- lm(log10(ACD$Theft.From.the.Person/ACD$Land.Area.in.Hectares) ~ log10(ACD$Population/ACD$Land.Area.in.Hectares))

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)


#visualize linearity
par(mfrow = c(1,3))


plot(log10(ACD$Anti.Social.Behaviour/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model1, col="blue", lwd = 3)

  #to check correlation coefficient on each graph, the following can be used:
  #cor(log10(ACD$Anti.Social.Behaviour/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))

plot(log10(ACD$Burglary/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model2, col="blue", lwd = 3)
plot(log10(ACD$Robbery/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model3, col="blue", lwd = 3)


plot(log10(ACD$Vehicle.Crimes/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model4, col="blue", lwd = 3)
plot(log10(ACD$Violent.Crimes/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model5, col="blue", lwd = 3)
plot(log10(ACD$Shoplifting/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model6, col="blue", lwd = 3)



plot(log10(ACD$Criminal.Damage...Arson/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model7, col="blue", lwd = 3)
plot(log10(ACD$Other.Theft/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model8, col="blue", lwd = 3)
plot(log10(ACD$Drugs/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model9, col="blue", lwd = 3)


plot(log10(ACD$Other.Crimes/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model10, col="blue", lwd = 3)
plot(log10(ACD$Bike.Theft/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model11, col="blue", lwd = 3)
plot(log10(ACD$Possession.of.Weapons/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model12, col="blue", lwd = 3)


plot(log10(ACD$Public.Order/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model13, col="blue", lwd = 3)
plot(log10(ACD$Theft.From.the.Person/ACD$Land.Area.in.Hectares),log10(ACD$Population/ACD$Land.Area.in.Hectares))
abline(model14, col="blue", lwd = 3)


#visualizing normality
par(mfrow = c(1,3))

hist(model1$residuals, main = "Histogram of ASB Density", xlab = "Residuals", cex.lab=1.5)
hist(model2$residuals, main = "Histogram of Burglary", xlab = "Residuals", cex.lab=1.5)
hist(model3$residuals, main = "Histogram of Robbery", xlab = "Residuals", cex.lab=1.5)

par(mfrow = c(1,3))
hist(model4$residuals, main = "Histogram of Vehicle Crimes", xlab = "Residuals", cex.lab=1.5)
hist(model5$residuals, main = "Histogram of Violent Crimes", xlab = "Residuals", cex.lab=1.5)
hist(model6$residuals, main = "Histogram of Shoplifting", xlab = "Residuals", cex.lab=1.5)

par(mfrow = c(1,3))
hist(model7$residuals, main = "Histogram of Criminal Damage", xlab = "Residuals", cex.lab=1.5)
hist(model8$residuals, main = "Histogram of Other Theft Density", xlab = "Residuals", cex.lab=1.5)
hist(model9$residuals, main = "Histogram of Drugs", xlab = "Residuals", cex.lab=1.5)

par(mfrow = c(1,3))
hist(model10$residuals, main = "Histogram of Other Crimes Density", xlab = "Residuals", cex.lab=1.5)
hist(model11$residuals, main = "Histogram of Bike Theft", xlab = "Residuals", cex.lab=1.5)
hist(model12$residuals, main = "Histogram of Possession of Weapons", xlab = "Residuals", cex.lab=1.5)

par(mfrow = c(1,3))
hist(model13$residuals, main = "Histogram of Public Order", xlab = "Residuals", cex.lab=1.5)
hist(model14$residuals, main = "Histogram of Theft from the person", xlab = "Residuals", cex.lab=1.5)


#Visuals for independence
par(mfrow = c(1,3))
residual1 <- residuals(model1)
plot(residual1[-length(residual1)], residual1[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main = "ASB Density", cex.lab = 1.5)
residual2 <- residuals(model2)
plot(residual2[-length(residual2)], residual2[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Burglary Density", cex.lab = 1.5)
residual3 <- residuals(model3)
plot(residual3[-length(residual3)], residual3[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Robbery Density", cex.lab = 1.5)


par(mfrow = c(1,3))
residual4 <- residuals(model4)
plot(residual4[-length(residual4)], residual4[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Vehicle Crimes Density", cex.lab = 1.5)
residual5 <- residuals(model5)
plot(residual5[-length(residual5)], residual5[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Violent Crimes Density", cex.lab = 1.5)
residual6 <- residuals(model6)
plot(residual6[-length(residual6)], residual6[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Shoplifting Density", cex.lab = 1.5)


par(mfrow = c(1,3))
residual7 <- residuals(model7)
plot(residual7[-length(residual7)], residual7[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Criminal Damage Density", cex.lab = 1.5)
residual8 <- residuals(model8)
plot(residual8[-length(residual8)], residual8[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Other Theft Density", cex.lab = 1.5)
residual9 <- residuals(model9)
plot(residual9[-length(residual9)], residual9[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Drugs Density", cex.lab = 1.5)


par(mfrow = c(1,3))
residual10 <- residuals(model10)
plot(residual10[-length(residual10)], residual10[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Other Crimes Density", cex.lab = 1.5)
residual11 <- residuals(model11)
plot(residual11[-length(residual11)], residual11[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Bike Theft Density", cex.lab = 1.5)
residual12 <- residuals(model12)
plot(residual12[-length(residual12)], residual12[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "POW Density", cex.lab = 1.5)


par(mfrow = c(1,3))
residual13 <- residuals(model13)
plot(residual13[-length(residual13)], residual13[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Public Order Density", cex.lab = 1.5)
residual14 <- residuals(model14)
plot(residual14[-length(residual14)], residual14[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),pch = 19, cex = 0.2, main= "Theft from the Person Density", cex.lab = 1.5)


#Checking for constant variance
par(mfrow = c(1,3))
plot(fitted(model1), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "ASB DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model2), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Burglary DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model3), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Robbery DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)


par(mfrow = c(1,3))
plot(fitted(model4), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Vehicle Crimes DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model5), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Violent Crimes DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model6), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Shoplifting DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)

par(mfrow = c(1,3))
plot(fitted(model7), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Criminal Damage DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model8), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Other Theft DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model9), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Drugs DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)

par(mfrow = c(1,3))
plot(fitted(model10), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Other Crimes DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model11), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "BBike Theft DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model12), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "POW DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)

par(mfrow = c(1,3))
plot(fitted(model13), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Public Order DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)
plot(fitted(model14), residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Theft from the person DENSITY", cex.lab = 1.5)
abline(h=0, lwd=2, col=2)


#b
  #Plotting this would need the plot section of R to be resized because of the dataset's size.
residuals<- data.frame(model1$residuals, model2$residuals,model3$residuals,
                       model4$residuals, model5$residuals, model6$residuals,
                       model7$residuals, model8$residuals, model9$residuals,
                       model10$residuals, model11$residuals, model12$residuals,
                       model13$residuals, model14$residuals)
colnames(residuals)<-c('ASB Density', 'Burglary Density', 'Robbery Density','Veh. Crimes Density', 
                       'Vio Crimes Density', 'Shoplifting Density', "Criminal Damage Density",
                       'Other Theft Density', 'Drugs Density', 'Other Crimes', 'Bike Theft', 'POW Density',
                       'Public Order Density', 'Theft from the person Density')
plot(residuals, cex = 0.2)


#heatmaps
  #installing all needed packages
install.packages("moments")
install.packages("gplots")
install.packages("ggplot2")
install.packages("car")
install.packages("nortest")
install.packages("proxy")

  #importing necessary libraries
library(moments)
library(gplots)
library(ggplot2)
library(car)
library(nortest)
library(proxy)

  #color palette
install.packages("RColorBrewer")

library(RColorBrewer)
ACD_palette<-colorRampPalette(c("cyan", "blue2", "blue3", "white", "orange", "red", "deeppink3"))(n=100)

variable_correlations<-cor(residuals, use = 'pairwise', method = 'pearson')

variable_correlations

heatmap.2(variable_correlations, cexRow=1, cexCol=1, density.info='none', trace='none',
          key.title='', key.xlab='', col=ACD_palette, breaks=seq(-1,1,length=101))


#dendogram
dist<-dist(residuals)
a<-hclust(dist, method = 'complete')
par(cex=0.4, cex.lab=0.5, mfrow=c(1,1))
plot(a, cex.axis=0.2)





#Part 3

#install packages
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages("tmap")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("png")
install.packages("rgeos")

#import libraries
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(ggplot2) # tidyverse data visualization package
library(rgdal)
library(png)
library(rgeos)


#download shape files to directory
setwd("C:/Users/JOHNSON/Desktop/materialsfortheassignment")
#change the directory location below
shp2<- shapefile("C:/Users/JOHNSON/Desktop/materialsfortheassignment/shapefiles2/shapefiles2/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx")


#import data
regions<- read.csv("AssessmentCrimeData.csv", header=TRUE, sep = ",")

# LSOA codes  match with the name in the shape files. In this case the columns name is "LSOA11CD".
shp_main<-subset(shp2, LSOA11CD %in% regions$LSOA)

#add variables to the shapefile. for instance crime data
shp_main$"LSOA Code"<-regions$LSOA
shp_main$"Region Name"<-regions$Name
shp_main$"Land Area"<- regions$Land.Area.in.Hectares
shp_main$"Population"<-regions$Population

shp_main$"Population Density"<- log10(regions$Population/regions$Land.Area.in.Hectares)
shp_main$"log10(ASB Density)"<- log10(regions$Anti.Social.Behaviour/regions$Land.Area.in.Hectares)
shp_main$"log10(Burglary Density)"<- log10(regions$Burglary/regions$Land.Area.in.Hectares)
shp_main$"log10(Robbery)"<- log10(regions$Robbery/regions$Land.Area.in.Hectares)
shp_main$"log10(Vehicle Crimes Density)"<- log10(regions$Vehicle.Crimes/regions$Land.Area.in.Hectares)
shp_main$"log10(Violent Crimes Density)"<- log10(regions$Violent.Crimes/regions$Land.Area.in.Hectares)
shp_main$"log10(Shoplifting Density)"<- log10(regions$Shoplifting/regions$Land.Area.in.Hectares)
shp_main$"log10(Criminal Damage Density)"<- log10(regions$Criminal.Damage...Arson/regions$Land.Area.in.Hectares)
shp_main$"log10(Other Theft Density)"<- log10(regions$Other.Theft/regions$Land.Area.in.Hectares)
shp_main$"log10(Drugs)"<- log10(regions$Drugs/regions$Land.Area.in.Hectares)
shp_main$"log10(Other Crimes Density)"<- log10(regions$Other.Crimes/regions$Land.Area.in.Hectares)
shp_main$"log10(Bike Theft Density)"<- log10(regions$Bike.Theft/regions$Land.Area.in.Hectares)
shp_main$"log10(POW Density)"<- log10(regions$Possession.of.Weapons/regions$Land.Area.in.Hectares)
shp_main$"log10(Public Order Density)"<- log10(regions$Public.Order/regions$Land.Area.in.Hectares)
shp_main$"log10(Theft from the Person Density)"<- log10(regions$Theft.From.the.Person/regions$Land.Area.in.Hectares)



#Derbyshire's population density
par(mfrow = c(1,3))
tm_shape(shp_main) +
  
  tm_polygons ("Population Density", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(ASB Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(ASB Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Burglary Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Burglary Density)") +
  tm_layout +
  tm_borders(lwd=1)


#Derbyshire's log10(Robbery Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Robbery Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Vehicle Crimes Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Vehicle Crimes Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Violent Crimes Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Violent Crimes Density)") +
  tm_layout +
  tm_borders(lwd=1)


#Derbyshire's shoplifting density
tm_shape(shp_main) +
  
  tm_polygons ("log10(shoplifting Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Criminal Damage Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Criminal Damage Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Other Theft Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Other Theft Density)") +
  tm_layout +
  tm_borders(lwd=1)


#Derbyshire's log10(Drugs Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Drugs)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Other Crimes Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Other Crimes Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Bike Theft Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Bike Theft Density)") +
  tm_layout +
  tm_borders(lwd=1)


#Derbyshire's log10(POW Crimes Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(POW Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Public Order Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Public Order Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)
#Derbyshire's log10(Theft From the Person Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(Theft from the Person Density)") +
  tm_layout +
  tm_borders(lwd=1)
