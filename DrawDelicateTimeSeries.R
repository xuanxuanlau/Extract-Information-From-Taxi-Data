#生成时间序列交互式图形

#PART 1:每小时
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)

cluster.factor.rep <- rep(c(1:10), each = 744)
date.time <- seq.Date(as.Date("2015-07-01"), by = "day", length.out = 32)
date.time <- pretty_dates(date.time, 744)
date.time <- date.time[-745]
date.time.rep <- rep(date.time, times = 10)


OD.zscore.value <- as.vector(t(RawData))

df.pic <- data.frame(
  cluster = as.factor(cluster.factor.rep),
  datetime = date.time.rep,
  value = OD.zscore.value,
  row.names = NULL
)

picture <- nPlot(value ~ datetime, group = 'cluster', data = df.pic, type = 'lineWithFocusChart')
picture$xAxis( tickFormat="#!function(d) {return d3.time.format('%m-%d')(new Date( d * 86400000 ));}!#" )  
picture$publish("ACF距离 - 无标准化10类每小时时间序列均值曲线")  

#PART 2:每天
RawData <-fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)
cluster.factor.rep <- rep(c(1:10), each = 31)
date.time <- seq.Date(as.Date("2015-07-01"), by = "day", length.out = 32)
date.time <- date.time[-32]
date.time.rep <- rep(date.time, times = 12)
OD.zscore.value <- as.vector(t(RawData))

df.pic <- data.frame(
  cluster = as.factor(c(cluster.factor.rep, rep(c(11,12), each = 31))),
  datetime = date.time.rep,
  value = c(OD.zscore.value, z_score(T_max), z_score(T_min)),
  row.names = NULL
)

picture <- nPlot(value ~ datetime, group = 'cluster', data = df.pic, type = 'lineWithFocusChart')
picture$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %d')(new Date( d * 86400000 ));}!#" )  
picture$publish("ACF距离 - 10类每天时间序列均值曲线")  
  
  
  