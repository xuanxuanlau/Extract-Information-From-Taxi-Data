#提取出某类cluster的所有时间序列（包含格网ID），reshape成一个data.frame：[格网ID] [OD.value] [weekday.hour] [week]
#根据下雨天对应着星期几的a点至b点，提取出所有格网相应时间段的数据，
#分别计算每个格网在这个时间段内每个小时的均值，得到[格网ID] [a点] [a+1点]...[b点]，
#用下雨时段的数据减去均值，保存为[格网ID] [a点] [a+1点]...[b点]
raw.data <- fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)
cluster.mark <- fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)
grid.index <- which(cluster.mark$columm == 5)
raw.data <- raw.data[grid.index, ]
each.num <- length(grid.index)
weekday.in.month <- c(c(49:168), rep(c(1:168), times = 3), c(1:120))
week.in.month <- fun.week.order(first.day = 3, sum.days = 31)
rawdata.onecol <- as.vector(t(raw.data))

time.series <- data.frame(
  grid.id = rep(grid.index, each = 744),
  OD =  rawdata.onecol,
  week.hour = rep(weekday.in.month, times = each.num),
  week = as.factor(rep(rep(week.in.month, each = 24), times = each.num))
)

rainday.data <- time.series[time.series$week.hour %in% c(137:145), ]
rainday.mean.data <- aggregate(rainday.data[, 1:3], list(rainday.data$grid.id, rainday.data$week.hour), mean)
setorder(rainday.mean.data, Group.1, Group.2)
rainday.mean.data<- matrix(rainday.mean.data$OD, ncol = 9, byrow = T) #类别内所有格网对应下雨时段所有时段的均值
rainy <- rainday.data[rainday.data$week %in% 1, 2] %>%
  matrix(., ncol = 9, byrow = T)
rainy.minus.mean <- rainy - rainday.mean.data
minus.result.label <- rainy.minus.mean > 0
minus.result.label[which(minus.result.label == FALSE)] <- -1 
minus.result.label[which(rainy.minus.mean == 0)] <- 0
minus.result.label <- cbind(grid.index, minus.result.label)

heatmap(minus.result.label[, 2:10], Colv = NA)

cluster.result <- matrix(0, nrow = 4087)
higher.grid.index <- rowSums(minus.result.label[, 3:9])>= 4
higher.grid <- minus.result.label[higher.grid.index, ]
grid.variation <- rainy.minus.mean / rainday.mean.data * 100
higher.grid.variation.pic <- data.frame(
  grid.id = rep(higher.grid[, 1], each = 9),
  time = rep(c(1:9), times = nrow(higher.grid)),
  #time = as.factor(rep(c("SAT 5-6PM", "SAT 6-7PM", "SAT 7-8PM", "SAT 8-9PM", "SAT 9-10PM", "SAT 10-11PM", "SAT 11-12PM"), times = nrow(higher.grid))),
  variation = as.vector(t(higher.grid.variation)),
  row.names = NULL
)
picture <- nPlot(variation ~ time, group = 'grid.id', data = higher.grid.variation.pic, type = 'lineChart')
picture$yAxis(axisLabel = "OD点数/平均值 变化率")
picture$xAxis(axisLabel = "时间")
picture$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
              '<p>' + y + ' in ' + x + '</p>'
              } !#")

cluster.result[higher.grid[, 1]] <- 1
factpal <- colorFactor(c(rgb(1, 1, 1, alpha = 0), rgb(0, 0, 1, alpha = 0.5)), cluster.result)
cluster.result <- as.data.frame(cluster.result)
grid.data.attr <- SpatialPolygonsDataFrame(grid.data, cluster.result, match.ID = F)
map <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addPolygons(
    data = grid.data.attr,
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~factpal(V1),
    group = "NO.4")
map


grid.variation <- rainy.minus.mean / rainday.mean.data * 100
grid.variation.heatmap <- data.frame(
  grid.id = rep(c(1:length(grid.index)), each = 9),
  time = rep(c(137:145), times = each.num),
  OD = as.vector(t(grid.variation)),
  row.names = NULL
)
p <- ggplot(grid.variation.heatmap, aes(x = grid.id, y = time, fill = OD)) +
  geom_raster() +
  scale_fill_gradientn(colors = c("white", "yellow", "blue","purple"))
p
heatmap(grid.variation[higher.grid.index, ], Colv = NA)

grid.variation <- rainy.minus.mean / rainday.mean.data * 100
grid.variation.heatmap <- data.frame(
  grid.id = rep(c(1:length(which(higher.grid.index))), each = 9),
  time = rep(c(137:145), times = length(which(higher.grid.index))),
  OD = as.vector(t(grid.variation[higher.grid.index, ])),
  row.names = NULL
)
p <- ggplot(grid.variation.heatmap, aes(x = grid.id, y = time, fill = OD)) +
  geom_raster() +
  scale_fill_gradientn(colors = c("white", "yellow", "blue","purple"))
p


raw.data <- fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)
lat.lon <- cbind(raw.data$V4, raw.data$V5)
point.data <- SpatialPoints(lat.lon, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
point.map <- leaflet(point.data) %>% addTiles() %>% addCircleMarkers()
