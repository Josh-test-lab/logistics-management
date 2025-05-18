library(readxl)
library(stringr)
library(ggplot2)
library(reshape2)
library(xlsx)

data_301 <- read.csv("/Users/wen/110運籌/109_301(返)/data_301.csv", sep = ",")

### BUS 2301
bus_2301 <- data_301[which(str_detect(data_301$路線編號, pattern = "2301", negate = FALSE)),]
stop_2301 <- c("花蓮火車站", "花蓮電影城", "商校街站", "花蓮商校", "郵政總局", "明義國小", "遠東百貨",
               "花蓮文創園區", "中華路站", "東大門夜市", "重慶市場(石藝大街)", "信義國小", "南濱公園", 
               "武聖宮(榮光社區)", "燕聲廣播", "仁安社區", "阿美麻糬", "新天堂樂園", "光華樂活創意園區", 
               "中華紙漿", "大學路口", "東華大學", "行政大樓", "原住民學院", "育成中心", "圖書館資訊大樓")
### BUS 2301 平日 -----
bus_2301_1 <- subset(bus_2301, 平日1週末0 == 1)

# 2301 依票種 圓餅圖
df_ticket_2301_1 <- data.frame(table(bus_2301_1$交易票種))
df_ticket_2301_1$per <- df_ticket_2301_1$Freq/sum(df_ticket_2301_1$Freq)*100

ggplot(df_ticket_2301_1, aes(x = "", y = per, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", color="white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, label = paste0(round(per), "%")), position = position_stack(vjust = .5)) +
  guides(fill=guide_legend(title="票種")) +
  ggtitle("301(返程平日)票種圓餅圖") +
  theme_void() + 
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 依票種 各站點直方疊圖
df_stop_ticket_2301_1 <- melt(data.frame(table(bus_2301_1$上車招呼站名稱,bus_2301_1$交易票種)))

ggplot(df_stop_ticket_2301_1, aes(x = factor(Var1, levels = stop_2301), y = value, fill = factor(Var2))) +
  geom_bar(stat="identity", position="stack",color = "white") +
  guides(fill=guide_legend(title="票種")) +
  ggtitle("301(返程平日)-各站點票種直方疊圖") +
  xlab("站點名稱") +
  ylab("數量") +
  theme_bw() + 
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 上車分鐘
bus_2301_1 <- bus_2301_1[-which(bus_2301_1$上車分鐘<100), ] # 扣除不合理上車時間資料 52筆
ggplot(bus_2301_1, aes(factor(上車招呼站名稱, levels = stop_2301), 上車分鐘)) +
  geom_point(aes(factor(上車招呼站名稱,stop_2301), color = 交易票種)) +
  theme_bw() + 
  ggtitle("301(返程平日)上車站點-上車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 下車分鐘
ggplot(bus_2301_1, aes(factor(下車招呼站名稱, levels = stop_2301), 下車分鐘)) +
  geom_point(aes(factor(下車招呼站名稱,stop_2301), color = 交易票種)) +
  theme_bw() + 
  ggtitle("301(返程平日)下車站點-下車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘
ggplot(bus_2301_1, aes(x = 乘坐分鐘 , fill = 交易票種) ) + 
  geom_histogram(bins = 50)  +
  facet_wrap(~factor(上車招呼站名稱, levels = stop_2301)) +
  ggtitle("301(返程平日)上車站點-乘坐時間長條圖") + 
  ylab("次數") +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘 盒狀圖
ggplot(bus_2301_1, aes(x = factor(上車招呼站名稱,levels=stop_2301), y = 乘坐分鐘)) + 
  geom_boxplot() + 
  ggtitle("301(返程平日)上車站點-乘坐分鐘盒狀圖") + 
  ylab("乘坐分鐘") + xlab("站點名稱") + 
  theme_bw() +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))
##########
### BUS 2301 週末 -----
bus_2301_0 <- subset(bus_2301, 平日1週末0 == 0)

# 2301 依票種 圓餅圖
df_ticket_2301_0 <- data.frame(table(bus_2301_0$交易票種))
df_ticket_2301_0$per <- df_ticket_2301_0$Freq/sum(df_ticket_2301_0$Freq)*100

ggplot(df_ticket_2301_0, aes(x = "", y = per, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", color="white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, label = paste0(round(per), "%")), position = position_stack(vjust = .5)) +
  guides(fill=guide_legend(title="票種")) +
  ggtitle("301(返程週末)票種圓餅圖") +
  theme_void() + 
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 依票種 各站點直方疊圖
df_stop_ticket_2301_0 <- melt(data.frame(table(bus_2301_0$上車招呼站名稱,bus_2301_0$交易票種)))

ggplot(df_stop_ticket_2301_0, aes(x = factor(Var1, levels = stop_2301), y = value, fill = factor(Var2))) +
  geom_bar(stat="identity", position="stack",color = "white") +
  guides(fill=guide_legend(title="票種")) +
  ggtitle("301(返程週末)-各站點票種直方疊圖") +
  xlab("站點名稱") +
  ylab("數量") +
  theme_bw() + 
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 上車分鐘
#bus_2301 <- bus_2301[-which(bus_2301$上車分鐘<100), ] # 扣除不合理上車時間資料 52筆
ggplot(bus_2301_0, aes(factor(上車招呼站名稱, levels = stop_2301), 上車分鐘)) +
  geom_point(aes(factor(上車招呼站名稱,stop_2301), color = 交易票種)) +
  theme_bw() + 
  ggtitle("301(返程週末)上車站點-上車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 下車分鐘
ggplot(bus_2301_0, aes(factor(下車招呼站名稱, levels = stop_2301), 下車分鐘)) +
  geom_point(aes(factor(下車招呼站名稱,stop_2301), color = 交易票種)) +
  theme_bw() + 
  ggtitle("301(返程週末)下車站點-下車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘
ggplot(bus_2301_0, aes(x = 乘坐分鐘 , fill = 交易票種) ) + 
  geom_histogram(bins = 50)  +
  facet_wrap(~factor(上車招呼站名稱, levels = stop_2301)) +
  ggtitle("301(返程週末)上車站點-乘坐時間長條圖") + 
  ylab("次數") +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘 盒狀圖
ggplot(bus_2301_0, aes(x = factor(上車招呼站名稱,levels=stop_2301), y = 乘坐分鐘)) + 
  geom_boxplot() + 
  ggtitle("301(返程週末)上車站點-乘坐分鐘盒狀圖") + 
  ylab("乘坐分鐘") + xlab("站點名稱") + 
  theme_bw() +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))





