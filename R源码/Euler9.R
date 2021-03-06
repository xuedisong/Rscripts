##9. 一个毕达哥拉斯三元组是一个包含三个自然数的集合，a<b<c，满足条件：a2 + b2 = c2
##例如：32 + 42 = 9 + 16 = 25 = 52.
##已知存在并且只存在一个毕达哥拉斯三元组满足条件a + b + c = 1000。找出该三元组中abc的乘积。

mainf<-function(){
  for(i in 1:1000){
    for(j in 1:1000){
      c<-sqrt(i^2+j^2)
      if(c%%1==0&(i+j+c)==1000)return(i*j*c)
    }
  }
}