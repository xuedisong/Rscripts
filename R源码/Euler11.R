##11. 在以下这个20 20的网格中，四个处于同一对角线上的相邻数字用红色标了出来：
##这四个数字的乘积是：26  63  78  14 = 1788696。在这个20 20网格中，处于任何方
##向上（上，下，左，右或者对角线）的四个相邻数字的乘积的最大值是多少？

##读入数据.R
##Euler11.R.RData
##矩阵dat

##x为矩阵
mainf<-function(x){
  c4<-c()
  r<-nrow(x)
  for(i in 1:r){
    for(j in 1:(r-3)){
      c4<-cbind(c4,c(x[i,j],x[i,j+1],x[i,j+2],x[i,j+3]))
    }
  }
  
  c5<-c()
  r<-nrow(x)
  for(i in 1:(r-3)){
    for(j in 1:r){
      c5<-cbind(c5,c(x[i,j],x[i+1,j],x[i+2,j],x[i+3,j]))
    }
  }
  
  c6<-c()
  r<-nrow(x)
  for(i in 1:(r-3)){
    for(j in 1:(r-3)){
      c6<-cbind(c6,c(x[i,j],x[i+1,j+1],x[i+1,j+2],x[i+1,j+3]))
    }
  }
  
  c7<-c()
  r<-nrow(x)
  for(i in 4:r){
    for(j in 1:(r-3)){
      c7<-cbind(c7,c(x[i,j],x[i-1,j+1],x[i-2,j+2],x[i-3,j+3]))
    }
  }
  
  max(apply(cbind(c4,c5,c6,c7),2,prod))
}