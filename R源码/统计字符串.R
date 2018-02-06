char_TG<-function(a){
  ying<-0
  shu<-0
  kong<-0
  qita<-0
  for(i in 1:nchar(a)){
    b<-substr(a,i,i)
    if((b<='z'&b>='a')|(b>='A'&b<='Z'))ying<-ying+1
    if(b==' ')kong<-kong+1
    if(b>='0'&b<='9')shu<-shu+1 else{
      qita<-qita+1
    }
  }
  q<-cbind(ying,shu,kong,qita)
  rownames(q)<-'统计结果'
  colnames(q)<-c('英文','数字','空格','其他')
  return(q)
}