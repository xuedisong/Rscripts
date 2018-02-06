huibao<-(jt$收盘价[-1]-jt$收盘价[-length(jt$收盘价)])/jt$收盘价[-length(jt$收盘价)
                                                    

summary(huibao)

breaks<-c(-1,-0.006,0.007,3)
f<-cut(huibao,breaks,labels = as.factor(c("中","良","优")))
summary(f)




##读入csv数据，将交通的20支股票信息存于数据框jt中
jt= read.csv(file ="C:/Users/fenghua/Desktop/Zky/交通.csv")
##观察jt属性
class(jt)##类属性为数据框
str(jt)##检查数据框成员列属性
head(jt)##检查数据前6行
tail(jt)##检查数据后6行
sum(is.na(jt))##检查有无NA值

##将交通业的20支股票名称存于gu
gu<-unique(jt$X)
##下面将构造循环对20支股票中的每支进行分级处理
rate<-c()##构建向量，用以存贮各级所占比例
raten<-c()##构建向量，用以存贮各级的个数及总数
for(i in 1:20){
  zh<-jt[jt$X==gu[i],]##数据框zh为20支股票中的一支
  zhh<-na.omit(zh)##移除包含NA值的行
  a<-zhh$收盘价
  n<-length(a)##长度为n
  hb<-(a[-1]-a[-n])/a[-n]##计算回报率
  bs<-c(min(hb)-1,-0.006,0.007,max(hb)+1)##分级节点
  f<-cut(hb,bs,labels = as.factor(c("中","良","优")))
  ##(n-1)个因子组成的向量f,水平为中，优，良
  su<-summary(f)##对f进行汇总,各级的个数
  sun<-c(su,n-1)
  names(sun)[4]<-c("总数")
  suu<-su/(n-1)##各级所占比例
  rate<-cbind(rate,suu)##存贮各级所占比例
  raten<-cbind(raten,sun)
}
colnames(rate)<-gu##用股名标注数据框rate列名
colnames(raten)<-gu##用股名标注数据框rate列名
rate<-t(rate)##转置便于观察
raten<-t(raten)##转置便于观察
##交通行业中的20支股票通过回报率节点-0.006,0.007分级而成的数据框
jtt<-cbind(rate,raten)
colnames(jtt)[1:3]<-c("中占比","良占比","优占比")
jtt


##下面检验20个组（交通行业中的20支股票）的各级是否等比例
##调用函数prop.test,它有两个向量参数
##ns<-c(ns1,ns2,...,nsN)
##nt<-c(nt1,nt2,...,ntN)
##prop.test(ns,nt)
prop.test(jtt[,"中"],jtt[,"总数"])
##函数prop.test的输出产生一个p值为
##较小的p值意味着我们拒绝原假设，有证据说明20支股票之间的评级存在显著差异
prop.test(jtt[,"良"],jtt[,"总数"])
prop.test(jtt[,"优"],jtt[,"总数"])


##3.2
gu1<-list(jt,sp,mt)
##下面将构造循环对20支股票中的每支进行分级处理
rate<-c()##构建向量，用以存贮各级所占比例
raten<-c()##构建向量，用以存贮各级的个数及总数
for(i in 1:3){  
  zh<-gu1[[i]]##数据框zh为20支股票中的一支
  zhh<-na.omit(zh)##移除包含NA值的行
  a<-zhh$收盘价
  n<-length(a)##长度为n
  hb<-(a[-1]-a[-n])/a[-n]##计算回报率
  bs<-c(min(hb)-1,-0.006,0.007,max(hb)+1)##分级节点
  f<-cut(hb,bs,labels = as.factor(c("中","良","优")))
  ##(n-1)个因子组成的向量f,水平为中，优，良
  su<-summary(f)##对f进行汇总,各级的个数
  sun<-c(su,n-1)
  names(sun)[4]<-c("总数")
  suu<-su/(n-1)##各级所占比例
  rate<-cbind(rate,suu)##存贮各级所占比例
  raten<-cbind(raten,sun)
}
nm<-c("交通业","食品业","煤炭业")
colnames(rate)<-nm##用股名标注数据框rate列名
colnames(raten)<-nm##用股名标注数据框rate列名
rate<-t(rate)##转置便于观察
raten<-t(raten)##转置便于观察
##交通行业中的20支股票通过回报率节点-0.006,0.007分级而成的数据框
jtt<-cbind(rate,raten)
colnames(jtt)[1:3]<-c("中占比","良占比","优占比")
jtt

4.
wilcox.test(,mt)


hbb<-function(x){
  a<-na.omit(x)$收盘价
  n<-length(a)
  return((a[-1]-a[-n])/a[-n])
}
jiaotong<-hbb(jt)
meitan<-hbb(mt)
shipin<-hbb(sp)
comb<-stack(list(j=jiaotong,m=meitan,s=shipin))
pa<-pairwise.t.test(comb$values,comb$ind)
str(pa)

##1.