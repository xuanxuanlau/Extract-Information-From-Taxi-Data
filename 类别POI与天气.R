#类别POI与天气
#从原始数据中提取出某类别POI的所有格网数据，增加字段[POI名(拼音)] [格网ID]
#reshape成ggplot数据格式：[OD] [HOUR] ([weekday]) [POI名] [格网ID] 

raw.data <- fread(
  input = file.choose(),
  sep = ",",
  header = T,
  #encoding = "UTF-8",
  showProgress = T,
  data.table = T
)
POI.grid <- c(2537, 2538, 2539, 2604, 2605, 2606, 2671, 2672, 2673,
              1175, 1241, 1242,
              1311, 1378,
              2701, 2702, 2703, 2768, 2769, 2770,
              1563, 1630,
              2159,
              3456, 3523)
POI.name <- c(rep("WuHanStation", times = 9), 
              rep("WuChangStation", times = 3),
              rep("FuJiaPoStation", times = 2),
              rep("HanKouStation", times = 6),
              rep("HanYangStation", times = 2),
              "HanKouShuiChangStation",
              rep("XinRongStation", times = 2))
POI.OD <- raw.data[POI.grid, ]
POI.df <- cbind(POI.name, POI.grid, POI.OD)
POI.df.ggplot <- data.frame(
  OD = as.vector(t(POI.OD)),
  HOUR.in.Month = rep(c(1:744), times = length(POI.grid)),
  Weekday = rep(weekday.in.month, times = length(POI.grid)),
  Week = rep(rep(as.factor(week.in.month), each = 24), times = length(POI.grid)),
  POI.Name = rep(POI.name, each = 744),
  Grid.ID = rep(as.factor(POI.grid), each = 744)
)
pdf(file = "/Users/xuanliu/Documents/学习/POI-Transport-EveryHourTimeSeries.pdf")
for(i in unique(POI.name)){
p <- ggplot(data = POI.df.ggplot[POI.df.ggplot$POI.Name %in% i, ], aes(x = Weekday, y = OD, color = Week, group = Week)) +
  annotate("rect", xmin = 1, xmax = 24, ymin = 0, ymax = 10, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 49, xmax = 72, ymin = 0, ymax = 10, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 97, xmax = 120, ymin = 0, ymax = 10, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 145, xmax = 168, ymin = 0, ymax = 10, alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = geom_vline.value, alpha = 0.5) +
  geom_line() +
  facet_grid(Grid.ID ~ ., scales = "free") +
  ggtitle(paste("POI of Transport Hub Time Series in Every Hour", i, sep = " - ")) +
  scale_color_brewer(palette = "Set1") 
ggsave("/Users/xuanliu/Documents/学习/POI-FuJiaPoStation-EveryHourTimeSeries.pdf", width = 30, height = 10, units = "cm")
print(p)
}
dev.off()

test <- data.frame(
  OD = test.data,
  Weekday = weekday.in.month, 
  Week = rep(as.factor(week.in.month), each = 24)
)
p <- ggplot(data = test, aes(x = Weekday, y = OD, color = Week, group = Week)) +
  annotate("rect", xmin = 1, xmax = 24, ymin = 0, ymax = 50, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 49, xmax = 72, ymin = 0, ymax = 50, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 97, xmax = 120, ymin = 0, ymax = 50, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 145, xmax = 168, ymin = 0, ymax = 50, alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = geom_vline.value, alpha = 0.5) +
  geom_line() +
  ggtitle(paste("POI of Transport Hub Time Series in Every Hour", i, sep = " - ")) +
  scale_color_brewer(palette = "Set1") 
ggsave("/Users/xuanliu/Documents/学习/POI-WuHanStation-EveryHourTimeSeries2.pdf", width = 30, height = 10, units = "cm")
