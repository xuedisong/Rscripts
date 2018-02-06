##赵凯悦的数据导入习惯
##Excel中第一行为纯字符行名，不含括号等
##保存成csv文件(非数值型转化成因子)
zky = read.csv(file.choose(),header = T)##默认有标题，利用as.is=T为保留原格式
str(zky)##利用$检查类型,一般好多因子列
View(zky)##观看数据
head(zky)##命令行中显示