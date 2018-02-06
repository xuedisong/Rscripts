library("stringi", lib.loc="D:/R-3.3.3/library")
library("stringr", lib.loc="D:/R-3.3.3/library")
f<-function(a){  ##本程序用于输出整数的位数，并且返回它的逆序
  b<-as.character(a)
  e<-nchar(b)
  d<-seq(1,e)
  D<-""
  for(i in 1:e){
    d[i]<-substr(b,e+1-i,e+1-i)
    D<-str_c(D,d[i])
  }
  g<-as.numeric(D)
  h<-cbind(e,g)
  rownames(h)<-a
  colnames(h)<-c("位数","逆序数")
  return(h)
}