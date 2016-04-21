#计算每辆车运营的天数

unique.carid.permonth <- list()
find.index <- function(index.value, carid.permonth){
  return(which(carid.permonth == index.value)) #返回一个carid在结果数组中的索引
}

#循环开始#
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)

unique.carid.perday <- list(unique(RawData$V1))
unique.carid.permonth <- c(unique.carid.permonth, unique.carid.perday)
#循环结束#

all.unique.carid <- unique(unlist(unique.carid.permonth))
unique.carid.everyday <- matrix(data = NA, nrow = 5870, ncol = 32)
unique.carid.everyday[ ,1] <- all.unique.carid

for(i in 1:31){
  #unique.carid.everyday：7月有数据的出租车和31天每天是否有数据
  index <- unlist(lapply(unlist(unique.carid.permonth[i]), find.index, carid.permonth = unique.carid.everyday[ ,1]))
  unique.carid.everyday[index,i+1] <- 1
}

#carid.fullmonth：31天都有数据的carid
carid.fullmonth <- unique.carid.everyday[test,1]
#输出全月都有数据的carID的csv中第一行为colname
write.csv(carid.fullmonth, file = "/Users/xuanliu/Documents/学习/AllOD201507/CaiIDFullMonth.csv", row.names = F, quote = F)
