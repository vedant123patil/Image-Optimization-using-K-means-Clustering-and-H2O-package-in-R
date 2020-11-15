install.packages("h2o")
library(h2o)

#Default initializations to lauch h2o
h2o.init()
h2o.clusterInfo()

#Installation 
install.packages("jpeg")
library(jpeg)

#Read Image
img <- readJPEG("D:/Msc Data Analytics/SEMESTER 2/MACHINE LEARNING/ASSIGNMNET 2/Bird.jpg")
img
head(img)

#Clustring Algorithms & Visualization
install.packages("factoextra")
install.packages("gridExtra")

library(ggplot2)
library(factoextra)
library(gridExtra)

#Obtain the Dimensions of our Image
imgDM <- dim(img)

#Assign RGB Channels to Data Frame
imgRGB <- data.frame(
  x=rep(1:imgDM[2], each=imgDM[1]),
  y=rep(imgDM[1]:1, imgDM[2]),
  R= as.vector(img[,,1]),
  G= as.vector(img[,,2]),
  B= as.vector(img[,,3])
)
View(imgRGB)
# ggplot theme to be used
plotTheme <- function(){
  theme(
    panel.background=element_rect(size=3,
                                  colour = "black",
                                  fill = "white"),
    axis.ticks=element_line(size = 2),
    panel.grid.major = element_line(colour = "gray80",
                                    linetype = "dotted"),
    panel.grid.minor = element_line(colour = "gray90",
                                    linetype = "dashed"),
    
    axis.title.x = element_text(size = rel(1.2),
                                face = "bold"),
    axis.title.y = element_text(size = rel(1.2),
                                face = "bold"),
    plot.title = element_text(size = 20, face = "bold",
                              vjust = 1.5)
  )
}

#plot the image
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=rgb(imgRGB[c("R", "G", "B")]))+
  labs(title = "Original Image: Bird")+
  xlab("x")+
  ylab("y")+
  plotTheme()

#Use h2o cluster for storing image
bird.hex <- as.h2o(imgRGB)
head(bird.hex)

#Apply K-Means Clustering on the Image
#k=2
kMeans.2<-h2o.kmeans(training_frame = bird.hex, k = 2, x = c('R','G','B'))
kMeans.2@model
kColours.2<-rgb(kMeans.2$centers[kMeans.2$cluster,])
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kColours.2)+
  labs(title = paste(2, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

#k=3
kMeans.3<-kmeans(imgRGB[, c("R", "G", "B")], centers = 3)
kColours.3<-rgb(kMeans.3$centers[kMeans.3$cluster,])
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kColours.3)+
  labs(title = paste(3, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

#k=6
kMeans.6<-kmeans(imgRGB[, c("R", "G", "B")], centers = 6)
kColours.6<-rgb(kMeans.6$centers[kMeans.6$cluster,])
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kColours.6)+
  labs(title = paste(6, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

#k=2000
kMeans.2000<-kmeans(imgRGB[, c("R", "G", "B")], centers = 2000)
kColours.2000<-rgb(kMeans.2000$centers[kMeans.2000$cluster,])
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kColours.2000)+
  labs(title = paste(2000, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()
