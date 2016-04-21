#提取31天都有数据的出租车(共4092辆)的OD点
#读取原始数据
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)

subset.ODdata <- function(carid.value, rawdata){
  return(rawdata[V1 == carid.value])
}

#lapply的第一个参数如果是data.frame或data.table的话，会出错
Needed.ODdata <- lapply(carid.fullmonth, subset.ODdata, rawdata = RawData)
#rbindlist可以将嵌套list转成data.table
Needed.ODdata <- rbindlist(Needed.ODdata, use.names = T) 

#添加标记 #小时# 的字段，24小时制，1表示0~1点时间段
OD.time <- parse_date_time(Needed.ODdata$V3, orders = "H! M! S!")
HOUR <- hour(OD.time)+1
result.data <- cbind(RawData, HOUR)

write.csv(result.data, file = "/Users/xuanliu/Documents/学习/SubsetOD201507/NeededOD20150731.csv", row.names = F,  quote = F)
