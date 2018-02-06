jiaohuan<-function(a){
  xv<-a%%10
  shang<-(a-a%%10)/10
  t<-xv
  if(shang>=10)xv<-jiaohuan(shang)
  shang<-t
  return(xv*+shang)
}
weishu<-function(a){
  for(i in 1:10000){

  }
}