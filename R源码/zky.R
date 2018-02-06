zky = read.csv(file.choose(),header = T)##默认有标题，利用as.is=T为保留原格式

zky$时间<-as.Date(zky$时间)
zky$时间<-as.POSIXlt(zky$时间)

str(zky)##利用$检查类型,一般好多因子列
View(zky)##观看数据
head(zky)##命令行中显示
tail(zky)
##
cbind(as.POSIXlt(zky$时间)$year+1900,
      as.POSIXlt(zky$时间)$mon+1,
      as.POSIXlt(zky$时间)$mday)
q<-quarters(as.POSIXct(zky$时间))
##year,mon,mday,wday
##该年某一天yday
##Zky<-cbind(Zky[,c(1,2)],h,months(Zky$日期),quarters(Zky$日期),Zky[,c(4:8)])