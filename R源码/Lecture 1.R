x<-rnorm(100,mean=5,sd=0.1)
mean(x)
sd(x)
summary(x)

install.packages(¡®MVA¡¯)
library("MVA")
#############################
##### Lecture 1   Examples  #
#############################

### MVAboxcity
# close windows and clear variables
rm(list=ls(all=TRUE))
graphics.off()
# load data 'cities.txt'
# copy the file into the working directory and run 
# setwd("C:/Users")
# x  = read.table('cities.txt');
# or you can open a browse window and choose
x = read.table(file.choose(),header = F)
m1 = mean(as.matrix(x));
# Plot box plot
boxplot(x,xlab='World Cities', ylab="Values")
lines(c(0.8, 1.2),c(m1, m1),col="black",lty="dotted",lwd=1.2)
title('Boxplot')
# Five Number Summary
# R "quantile" function uses a different algorithm to calculate the sample quantiles than in the MVA book
#Therefore, the values using Matlab could differ from the Book values, but
#the difference is not great, and should not be significant.
#Easiest way to calculate Five Number Summary is
#quantile(population,[.025 .25 .50 .75 .975])
five = quantile(x[,1],c(.025, .25, .50, .75, .975));
#Display results
print('Five number summary, in millions')
print(five/100)

###MVAcontbank2
# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
# install and load library
install.packages("KernSmooth")
library(KernSmooth)
library(graphics)
# load data 
#xx<-read.table("bank2.dat")
xx = read.table(file.choose(),header = F)
d <- bkde2D(xx[, 5:6], bandwidth = 1.06*c(sd(xx[, 5]), sd(xx[, 6]))* 200^(-1/5))
contour(d$x1, d$x2, d$fhat, xlim = c(8.5, 12.5), ylim = c(137.5, 143), col = c("blue", "black", "yellow", "cyan", "red", "magenta", "green", "blue", "black"), lwd=3, cex.axis = 1)

###MVAcontbank3
rm(list=ls(all=TRUE))
install.packages("misc3d")
library(KernSmooth)
library(misc3d)
#xx<-read.table("bank2.dat")
xx = read.table(file.choose(),header = F)
d <- kde3d(xx[, 4], xx[, 5], xx[, 6], n = 15)
contour3d(d$d, level = c(max(d$d[10,10,])*.02, max(d$d[10,10,])*.5, max(d$d[10,10,])*1.3), fill = c(FALSE, FALSE, TRUE), col.mesh = c("green","red","blue") , engine = "standard", screen=list(z=210,x=-40,y=-295), scale=TRUE)


### MVAscabank56
rm(list=ls(all=TRUE))
graphics.off()
#x = read.table('bank2.dat');
x = read.table(file.choose(),header = F)
x56 = x[,5:6];
x1 = rep(1,100)
x2 = rep(2,100)
xx = cbind(x56,c(x1,x2))
plot(xx[,1],xx[,2], pch=c(xx[,3]), col=c(xx[,3]),frame=TRUE, axes=FALSE, ylab="", xlab="", ylim=c(137.5,142.5),xlim=c(7,13))
axis(side=1, at=seq(7,13,1), labels=seq(7,13,1))
axis(side=2, at=seq(138,142,1), labels=seq(138,142,1))
title("Swiss bank notes")

### MVAscabank456
rm(list=ls(all=TRUE))
graphics.off()
#x = read.table('bank2.dat');
x = read.table(file.choose(),header = F)
x456 = x[,4:6]
x1 = rep(1,100)
x2 = rep(2,100)
xx = c(x1,x2)
require(lattice)
cloud(x456[,3]~x456[,2]*x456[,1],pch=c(xx), col=c(xx), ticktype="detailed",
      main=expression(paste("Swiss bank notes")),
      screen=list(z=-90,x=-90, y=45),
      scales=list(arrows=FALSE,col="black",distance=1,tick.number=c(4,4,5),cex=.7,
                  z=list(labels=round(seq(138,142,length=6))),
                  x=list(labels=round(seq(7,14,length=6))),
                  y=list(labels=round(seq(7,12,length=6)))
      ),
      xlab=list(expression(paste("Lower inner frame (X4)")),rot=-10,cex=1.2),
      ylab=list("Upper inner frame (X5)",rot=10,cex=1.2),
      zlab=list("Diagonal (X6)", rot=90,cex=1.1))

### MVAdraftbank4
library(KernSmooth)
#data<-read.table("bank2.dat")
data = read.table(file.choose(),header = F)
x <- data
i=2
op <- par(mfrow=c(4, 4), cex=.2)
while(i<6){
  i <- i+1
  j <- 2
  while(j<6){
    j <- j+1
    if(i==j){
      plot(i, type="n", axes=FALSE, xlab="", ylab="", main=i, cex.main=5)}
    if(i<j){
      xx <- cbind(x[,i],x[,j],c(rep(0,100),rep(1,100)))
      zz <- bkde2D(xx[,-3], 0.4)
      contour(zz$x1, zz$x2, zz$fhat, nlevels=12, col=rainbow(20), drawlabels=FALSE, xlab="X", ylab="Y")}
    if(i>j){
      yy <- cbind(x[,i],x[,j],c(rep(0,100),rep(1,100)))
      plot(yy[,-3], pch=as.numeric(yy[,3]), xlab="X", ylab="Y", cex=3, col="blue")}
  }}
par(op)

### MVAfacebank10
rm(list=ls(all=TRUE))
graphics.off()
install.packages("aplpack") 
library(aplpack)
#x = read.table('bank2.dat');
x = read.table(file.choose(),header = F)
xx = x[91:110,]
ncolors=15
faces(xx, nrow = 4,face.type=1,scale=TRUE, col.nose = rainbow(ncolors), col.eyes = rainbow(ncolors, 
                                                                                           start = 0.6, end = 0.85), col.hair = terrain.colors(ncolors), 
      col.face = heat.colors(ncolors), col.lips = rainbow(ncolors, 
                                                          start = 0, end = 1), col.ears = rainbow(ncolors, start = 0, 
                                                                                                  end = 0.8), plot.faces = TRUE)

