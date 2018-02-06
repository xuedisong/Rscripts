#############################
##### Lecture 2   Examples  #
#############################
### basic matrix operations in R ###

### matrix 
# the matrix function
# R wants the data to be entered by columns starting with column one
# 1st arg: c(1,2,3,4,5,6) the values of the elements filling the columns
# 2nd arg: 3 the number of rows
# 3rd arg: 2 the number of columns

A=matrix(c(1,2,3,4,5,6),nrow=3,ncol=2, byrow=FALSE)
A

###Transpose of a Matrix
t(A)

###Multiplication by a Scalar
c=3
c*A

###Matrix Addition & Subtraction
B=matrix(c(1,1,1,1,1,1),3,2)
B
C=A+B
C
D=A-B
D

###Element-wise multiplication
A*B

###Matrix multiplication 
t(A)%*%B
B%*%t(A)

###Matrix determinant (square matrix)
A=matrix(c(1,2,3,4),nrow=2,ncol=2, byrow=FALSE)
A
det(A) 

###Matrix inverse (square matrix)
solve(A)
solve(A) %*% A
A %*% solve(A)


###Matrix trace (square matrix)
sum(diag(A))

### Matrix rank
## apply QR decomposition
qr(A)$rank

### Generalized inverse
# install and load library
library(MASS)
#ginv(A) returns the unique Moore-Penrose generalized inverse of A
install.packages("MASS")
ginv(A)
A=matrix(c(1,0,0,0),nrow=2,ncol=2, byrow=FALSE)
A
ginv(A)

### spectral decomposition
A=matrix(c(1,0.2,0.2,1),nrow=2,ncol=2, byrow=FALSE)
A
r=eigen(A)
r
Lambda=diag(r$values)
Gamma=r$vectors
Lambda
Gamma
t(Gamma)
Gamma %*% t(Gamma)
Gamma %*% Lambda %*% t(Gamma)

### Singular value decomposition
A=matrix(c(1,2,3,4,5,6),nrow=3,ncol=2, byrow=FALSE)
A
#s=svd(A)
#s$d = vector containing the singular values of A
#s$u = matrix with columns contain the left singular vectors of A
#s$v = matrix with columns contain the right singular vectors of A
s=svd(A)
s
Lambda=diag(s$d)
Gamma=s$u
Delta=t(s$v)
t(Gamma) %*% Gamma
t(Delta) %*% Delta
Gamma %*% Lambda %*% t(Delta)

### MVAscabank45 
rm(list=ls(all=TRUE))
graphics.off()
#x = read.table('bank2.dat');
x = read.table(file.choose(),header = F)
plot(x[,4],x[,5],ylab="", xlab="",ylim=c(7,13), xlim=c(7,13) )
title('Swiss bank notes')

plot(x[1:100,4],x[1:100,5],ylab="", xlab="",ylim=c(7,13), xlim=c(7,13) )
title('Genuine Swiss bank notes')

plot(x[101:200,4],x[101:200,5],ylab="", xlab="",ylim=c(7,13), xlim=c(7,13) )
title('Counterfeit Swiss bank notes')

cov(x)
cov(x[1:100,])
cov(x[101:200,])

cor(x)
cor(x[1:100,])
cor(x[101:200,])

#############################
#####  Examples  #
#############################


### MVAdenbank2 
rm(list=ls(all=TRUE))
#setwd("C:/...")
#x=read.table("bank2.dat")
x = read.table(file.choose(),header = F)
x4=x[1:100,4]
x5=x[1:100,5]
f4=density(x4)
f5=density(x5)
par(mfrow=c(1,2))
plot(f4,type="l",lwd=3,xlab="Lower Inner Frame (X4)",ylab="Density",main="Swiss bank notes",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)
plot(f5,type="l",lwd=3,xlab="Upper Inner Frame (X5)",ylab="Density",main="Swiss bank notes",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)


### MVAdenbank3
#Clear loaded variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
#Load libraries
install.packages("KernSmooth")
install.packages("graphics")
library(KernSmooth)
library(graphics)
#Set working directory
#setwd("C:/Users/stawz/Desktop/teaching/ST5210/Sem1 2014 2015/week 1")
#Read dataset
#xx=read.table("bank2.dat")
xx=read.table(file.choose(),header = F)
#Compute a kernel density estimates
dj <- bkde2D(xx[, 4:5], bandwidth = 1.06*c(sd(xx[, 4]), sd(xx[, 5]))* 200^(-1/5))
d1 <- bkde(xx[, 4], gridsize=51)
d2 <- bkde(xx[, 5], gridsize=51)
dp = (d1$y)%*%t(d2$y)
par(mfrow=c(1,2))
persp(d1$x, d2$x, dp, box=FALSE)
persp(dj$x1, dj$x2,dj$fhat, box=FALSE)

### MVAcontnorm
# Close windows and clear variables
rm(list=ls(all=TRUE))
graphics.off()
# Libraries (you might need to install them)
install.packages("MASS")
install.packages("mnormt")
library(MASS)
library(mnormt)
# inputs parameters
n     = 200                                 # number of draws
mu    = c(3,2)                              # mean vector
sig   = matrix(c(1, -1.5, -1.5, 4), ncol=2) # covariance matrix
# bivariate normal sample
set.seed(80)
y     = mvrnorm(n, mu, sig, 2)
# bivariate normal density
xgrid = seq( from=(mu[1]-3*sqrt(sig[1,1])),to=(mu[1]+3*sqrt(sig[1,1])),length.out=200 )
ygrid = seq( from=(mu[2]-3*sqrt(sig[2,2])),to=(mu[2]+3*sqrt(sig[2,2])),length.out=200 )
z     = outer( xgrid,ygrid,FUN=function(xgrid,ygrid){dmnorm(cbind(xgrid,ygrid),mean=mu,varcov=sig)} )
# Plot
par(mfrow=c(1,2))
# Scatter plot
plot(y, col="black", ylab="X2",xlab="X1",xlim=range(xgrid),ylim=range(ygrid))
title('Normal sample')
# Contour ellipses
contour(xgrid,ygrid,z,xlim=range(xgrid),ylim=range(ygrid),nlevels=10,col = c("blue", "black", "yellow", "cyan", "red", "magenta", "green", "blue", "black"), lwd=3, cex.axis = 1, xlab="X1", ylab="X2")
title('Contour Ellipses')

### MVAcondnorm 
# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
# install and load packages 
install.packages("scatterplot3d")
library(scatterplot3d)
#Input parameters
#mu=(0,0)'
Sigma	= matrix( c(1,-0.8,-0.8,2), 2, 2 )     					#Covariance matrix of the Bivariate Normal distribution
c 		= sqrt(Sigma[2,2]-Sigma[2,1]*1/(Sigma[1,1])*Sigma[1,2]) #SD of conditional f(X2|X1)
#Create grid for x1,x2 and compute conditional pdf
x1		= rep(seq(0,4.5,by=0.75), times=301)					#grid values for x1		
x2		= rep(seq(-10,5,by=0.05), each=length(unique(x1)))					#grid values for x2
f   	= dnorm(0,(x2-Sigma[1,1]/Sigma[1,2]*x1)/c,1)/c					#conditional pdf f(X2|X1)
xx1 	= cbind(x1,x2,f)
#Conditional means
xm  	= unique(x1)												#realized x1
m   	= cbind(xm, Sigma[2,1]*xm, rep(0,length(xm)))					#conditional mean
xmf 	= c(-2,xm,6)
#plot: shifts in the conditional density
s3d 	= scatterplot3d(xx1[,2],xx1[,1],xx1[,3],lwd=0.05, pch=20, ylab="", xlab="", zlab="",scale.y=0.7,cex.axis=1.1, angle=40)
s3d$points3d(m[,2],m[,1],m[,3], pch=3,col="red3", lwd=2)
s3d$points3d(Sigma[2,1]*xmf,xmf,rep(0,9), type="l",col="black", lwd=2)
title("Conditional Normal Densities f(X2|X1)")


