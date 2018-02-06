F<-function(a){
  r<-1
  for(i in 1:a){
    r<-r*i
  }
  return(r)
}
f<-function(b){
  r<-0
  a<-0
  for(i in 1:b){
    r<-r+F(i)
    a<-str_c(a,"+",i,"!")
  }
  e<-as.data.frame(r)
  rownames(e)<-str_c(a,"=")
  colnames(e)<-"李星延是小猪@-@"
  return(e)
}