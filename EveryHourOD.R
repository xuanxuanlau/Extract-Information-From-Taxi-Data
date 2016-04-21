#提取每小时的OD点，并添加标记 小时 的字段
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)

OD.time <- parse_date_time(RawData$V3, orders = "H! M! S!")
HOUR <- hour(OD.time)+1
result.data <- cbind(RawData, HOUR)

write.csv(result.data, file = "/Users/xuanliu/Documents/学习/SubsetOD201507/NeededOD20150731.csv", row.names = F,  quote = F)
