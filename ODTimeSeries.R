#网格与OD点叠加分析，统计每天/每小时每个格网内的OD/O/D总数，构成OD时间序列

#读取网格shapefile（maptools）,定义投影坐标系；
#读取OD点.csv，转换成spatialpoint类，定义地理坐标系，spTransform转投影坐标系；
#根据时间戳找到某个小时段的所有OD点，用over函数统计每个格网内的OD总数
#存入 4087*（24*31）的array中
#尝试ssplot画sptial-time grids图

ODtime.series <- matrix(data = NA, nrow = 4087, ncol = 744)
j <- c(1:24)
#读取shapefile格式的网格，需定义投影坐标系
grid.data <- readShapePoly(
  fn = "Grid.shp",
  proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
)

#提取每小时的OD点
ODgrid.everyhour <- function(hour.value, HOUR.data, OD, GRID){
  subset.data <- OD[which(HOUR.data == hour.value)]
  subset.data <- cbind(subset.data$V4,subset.data$V5)
  point.data <- SpatialPoints(subset.data, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  point.data <- spTransform(point.data, CRSobj = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
  result.list <- over(GRID, point.data, returnList = T)
  return(lapply(result.list, length))
}

i <- 0
file.list <- dir(path = "/Users/xuanliu/Documents/学习/test", pattern = "*.csv", full.names = T)
system.time(
  for(fn in file.list){
  
    OD.data <-fread(
      input = fn,
      sep = ",",
      header = T,
      #encoding = "UTF-8",
      showProgress = T,
      data.table = T
    )
    #添加小时时间戳
    OD.data <- OD.data[OD.data$V9 %in% 1]
    OD.time <- parse_date_time(OD.data$V3, orders = "H! M! S!")
    HOUR <- hour(OD.time)+1
    
    temp <- lapply(j, ODgrid.everyhour, HOUR.data = HOUR, OD = OD.data, GRID = grid.data)
    temp <- t(rbindlist(temp, use.names = T))
    ODtime.series[ ,(i*24+1):((i+1)*24)] <- temp
    
    i <- i+1
  })

write.csv(ODtime.series, file = "/Users/xuanliu/Documents/学习/test/DesTimeSeriesEveryHour.csv", row.names = F,  quote = F)

#计算每个格网每天的OD总数
i <- 1
file.list <- dir(path = "/Users/xuanliu/Documents/学习/test", pattern = "*.csv", full.names = T)
system.time(
  for(fn in file.list){
    
    OD.data <-fread(
      input = fn,
      sep = ",",
      header = T,
      #encoding = "UTF-8",
      showProgress = T,
      data.table = T
    )
    subset.data <- OD.data[OD.data$V9 == 0]
    subset.data <- cbind(subset.data$V4,subset.data$V5)
    point.data <- SpatialPoints(subset.data, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    point.data <- spTransform(point.data, CRSobj = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
    result.list <- over(grid.data, point.data, returnList = T)
    temp <- lapply(result.list, length)
    temp <- t(as.data.frame(temp))
    ODtime.series[ ,i] <- temp
    
    i <- i+1
  })
write.csv(ODtime.series, file = "/Users/xuanliu/Documents/学习/test/DesTimeSeriesEveryDay.csv", row.names = F,  quote = F)





