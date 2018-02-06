dat<-function(){##上述被存至“Euler8.txt”文件,利用以下代码将数据存成长度为1000的数值向量dat
  ##读入数据.R,读入文件需修改
  ##data.frame
  tem1<-read.table("Euler8.txt",colClasses = c("character"))
  
  ##vector
  tem2<-tem1[[1]]
  
  ##list
  tem3<-strsplit(tem2,"")
  
  ##data.frame
  tem4<-as.data.frame(tem3)
  
  ##matrix
  tem5<-as.matrix(tem4)
  
  ##字符阵转化成数值阵，注意该阵是按列具有实际意义的，可能与原阵成转置
  tem6<-matrix(as.numeric(tem5),nrow = nrow(tem5))
  
  ##按列拉直处理
  as.vector(tem6)
}

mainf<-function(vec){##输入数值向量vec，输出连续五个数的最大乘积
  l<-length(vec)
  w<-c()
  for(i in 1:(l-4)){
    w<-c(w,prod(vec[i:(i+4)]))
  }
  max(w)
}

mainf(dat())##调用函数dat(),输出40824