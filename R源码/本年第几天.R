f<-function(a,b,c){         ##a为年份，b为月份，c为日期，本程序判断该日期为本年的第几天
  yuefen1<-c(0,31,28,31,30,31,30,31,31,30,31,30,31)
  yuefen2<-c(0,31,29,31,30,31,30,31,31,30,31,30,31)
  if(a%%100==0){
    if(a%%400==0)yuefen<-yuefen2 else yuefen<-yuefen1
  }else{
    if(a%%4==0)yuefen<-yuefen2 else yuefen<-yuefen1
  }
  d<-sum(yuefen[0:(b-1)])+c ##该日期为这年的第d天
  return(d)
}