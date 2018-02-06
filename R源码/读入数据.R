##data.frame
tem1<-read.table("Euler13.txt",colClasses = c("character"))

##vector
tem2<-tem1[[1]]

##list
tem3<-strsplit(tem2,"")

##data.frame
tem4<-as.data.frame(tem3)

##matrix
tem5<-as.matrix(tem4)

##字符阵转化成数值阵
tem6<-matrix(as.numeric(tem5),nrow = nrow(tem5))

View(tem6)