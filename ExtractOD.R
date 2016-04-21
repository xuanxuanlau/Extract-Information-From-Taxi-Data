#提取OD点
#读取原始数据
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)

car.id <- unique(RawData$V1)
current.index <- 1
OD.index <- NULL
extract.OD <- function(one.car, all.OD.data, all.OD.index){
  onecar.OD <- all.OD.data[V1 == one.car, "V9" , with = F]
  OD.num <- nrow(onecar.OD)
  if(OD.num > 1){
    judge <- cbind(onecar.OD[1:OD.num-1],onecar.OD[2:OD.num])
    OD.index <<- append(OD.index, which(rowSums(judge) == 1) + current.index)
  }
  current.index <<- current.index + OD.num
}

lapply(car.id, extract.OD, all.OD.data = RawData, all.OD.index = OD.index)
write.csv(RawData[OD.index], file = "/Users/xuanliu/Documents/学习/OD201507/OD20150731.csv", row.names = F,  quote = F)

