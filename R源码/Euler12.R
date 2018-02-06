##12. 三角形数序列是由对自然数的连加构造而成的。所以第七个三角形数是1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 那么三角形数序列中的前十个是：
##1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
##下面我们列出前七个三角形数的约数：
##1: 1
##3: 1,3
##6: 1,2,3,6
##10: 1,2,5,10
##15: 1,3,5,15
##21: 1,3,7,21
##28: 1,2,4,7,14,28
##可以看出28是第一个拥有超过5个约数的三角形数。那么第一个拥有超过500个约数的三角形数是多少？

##ysfj.R
##导入组合包
library("combinat", lib.loc="D:/R-3.3.3/library")
##x为向量
##zuheji函数返回值不包括1和本体
zuheji<-function(x){
  l<-length(x)
  w<-c()
  for(i in 1:(l-1)){
    mat<-combn(x,i)
    w<-c(w,apply(mat,2,prod))
  }
  w
}

##i=3 start
##n为超过的值
mainf<-function(n){
  i<-3
  repeat{
    m<-sum(1:i)
    ys<-c(1,ysfj(m))
    lin<-unique(zuheji(ys))
  
    if(length(lin)>n)break
    i<-i+1
  }
  m
}