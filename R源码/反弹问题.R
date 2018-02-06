f<-function(t){              ##t为次数
  FanT<-seq(1,t)
  LuC<-seq(1,t)
  FanT[1]<-50
  LuC[1]<-100
  e<-str_c('第',1,'次落地（单位：米）')
  for(i in 2:t){
    LuC[i]<-LuC[i-1]+2*FanT[i-1]
    FanT[i]<-FanT[i-1]/2
    e<-c(e,str_c('第',i,'次落地（单位：米）'))
  }
  r<-cbind(FanT,LuC)
  colnames(r)<-c('反弹的高度','路程')
  rownames(r)<-e
  return(r)
}