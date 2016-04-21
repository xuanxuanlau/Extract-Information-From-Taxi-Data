#读取原始数据
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = F,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)
#为经纬度设置精度
RawData$V4 <- sprintf("%.6f",RawData$V4)
RawData$V5 <- sprintf("%.6f",RawData$V5)

setorder(RawData, V1, V3) #按车辆ID和时间排序
write.csv(RawData, file = "/Users/xuanliu/Documents/学习/Data201507/20150731.csv", row.names = F,  quote = F)

