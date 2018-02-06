##13. 找出以下100个50位数之和的前十位数字。

##读入数据.R

##对tem6操作,
mainf<-function(){
  b<-apply(tem6,1,sum)
  c<-c()
  for(i in 1:49){
    yv<-b[51-i]%%10
    shang<-(b[51-i]-yv)/10
    c[51-i]<-yv
    b[50-i]<-shang+b[50-i]
  }
  c[1]<-b[1]
  c
}