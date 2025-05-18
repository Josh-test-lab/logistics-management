data_301 <- read.csv("/Users/wen/110運籌/109_301(返)/data_301.csv", sep = ",")
data_301[which(data_301$交易票種 == "學生卡"),11] <- "學生票"
colorlabel <- c("#50C878","#4D8F13","#6495ED","#FF4D00","#F8766D","#FFD000","#c6139d")

### 301 區間
bus_1301 <- data_301[c(grep("^301區間車",data_301$路線名稱)), ]
bus_1301[which(bus_1301[, 18] == 58),18] <- 0; bus_1301[which(bus_1301[, 21] == 58),21] <- 0
bus_1301[which(bus_1301[, 18] == 59),18] <- 1; bus_1301[which(bus_1301[, 21] == 59),21] <- 1
bus_1301[which(bus_1301[, 18] == 60),18] <- 2; bus_1301[which(bus_1301[, 21] == 60),21] <- 2
bus_1301[which(bus_1301[, 18] == 61),18] <- 3; bus_1301[which(bus_1301[, 21] == 61),21] <- 3
bus_1301[which(bus_1301[, 18] == 62),18] <- 4; bus_1301[which(bus_1301[, 21] == 62),21] <- 4
bus_1301[which(bus_1301[, 18] == 63),18] <- 5; bus_1301[which(bus_1301[, 21] == 63),21] <- 5
bus_1301[which(bus_1301[, 18] == 64),18] <- 6; bus_1301[which(bus_1301[, 21] == 64),21] <- 6
bus_1301[which(bus_1301[, 18] == 65),18] <- 7; bus_1301[which(bus_1301[, 21] == 65),21] <- 7
bus_1301[which(bus_1301[, 18] == 66),18] <- 8; bus_1301[which(bus_1301[, 21] == 66),21] <- 8
bus_1301[which(bus_1301[, 18] == 67),18] <- 9; bus_1301[which(bus_1301[, 21] == 67),21] <- 9
nbus_1301G <- bus_1301[which(as.numeric(bus_1301$上車招呼站代碼) - as.numeric(bus_1301$下車招呼站代碼) <= 0), ]
nbus_1301B <- bus_1301[which(as.numeric(bus_1301$上車招呼站代碼) - as.numeric(bus_1301$下車招呼站代碼) >= 0), ]

print('123')

### 301 
bus_301 <- data_301[-which(data_301$路線名稱=="301區間車"), ]
bus_301[which(bus_301[, 18] == 0),18] <-0;bus_301[which(bus_301[, 21] == 0),21] <-0
bus_301[which(bus_301[, 18] == 1),18] <-1;bus_301[which(bus_301[, 21] == 1),21] <-1
bus_301[which(bus_301[, 18] == 2),18] <-2;bus_301[which(bus_301[, 21] == 2),21] <-2
bus_301[which(bus_301[, 18] == 3),18] <-3;bus_301[which(bus_301[, 21] == 3),21] <-3
bus_301[which(bus_301[, 18] == 4),18] <-4;bus_301[which(bus_301[, 21] == 4),21] <-4
bus_301[which(bus_301[, 18] == 6),18] <-5;bus_301[which(bus_301[, 21] == 6),21] <-5
bus_301[which(bus_301[, 18] == 7),18] <-6;bus_301[which(bus_301[, 21] == 7),21] <-6
bus_301[which(bus_301[, 18] == 8),18] <-7;bus_301[which(bus_301[, 21] == 8),21] <-7
bus_301[which(bus_301[, 18] == 9),18] <-8;bus_301[which(bus_301[, 21] == 9),21] <-8
bus_301[which(bus_301[, 18] == 46),18] <-9;bus_301[which(bus_301[, 21] == 46),21] <-9
bus_301[which(bus_301[, 18] == 47),18] <-10;bus_301[which(bus_301[, 21] == 47),21] <-10
bus_301[which(bus_301[, 18] == 12),18] <-11;bus_301[which(bus_301[, 21] == 12),21] <-11
bus_301[which(bus_301[, 18] == 13),18] <-12;bus_301[which(bus_301[, 21] == 13),21] <-12
bus_301[which(bus_301[, 18] == 14),18] <-13;bus_301[which(bus_301[, 21] == 14),21] <-13
bus_301[which(bus_301[, 18] == 15),18] <-14;bus_301[which(bus_301[, 21] == 15),21] <-14
bus_301[which(bus_301[, 18] == 16),18] <-15;bus_301[which(bus_301[, 21] == 16),21] <-15
bus_301[which(bus_301[, 18] == 17),18] <-16;bus_301[which(bus_301[, 21] == 17),21] <-16
bus_301[which(bus_301[, 18] == 56),18] <-17;bus_301[which(bus_301[, 21] == 56),21] <-17
bus_301[which(bus_301[, 18] == 18),18] <-18;bus_301[which(bus_301[, 21] == 18),21] <-18
bus_301[which(bus_301[, 18] == 19),18] <-19;bus_301[which(bus_301[, 21] == 19),21] <-19
bus_301[which(bus_301[, 18] == 20),18] <-20;bus_301[which(bus_301[, 21] == 20),21] <-20
bus_301[which(bus_301[, 18] == 21),18] <-21;bus_301[which(bus_301[, 21] == 21),21] <-21
bus_301[which(bus_301[, 18] == 22),18] <-22;bus_301[which(bus_301[, 21] == 22),21] <-22 
bus_301[which(bus_301[, 18] == 24),18] <-25;bus_301[which(bus_301[, 21] == 24),21] <-25 #
bus_301[which(bus_301[, 18] == 23),18] <-24;bus_301[which(bus_301[, 21] == 23),21] <-24 #
bus_301[which(bus_301[, 18] == 57),18] <-23;bus_301[which(bus_301[, 21] == 57),21] <-23
nbus_301G <- bus_301[which(as.numeric(bus_301$上車招呼站代碼) - as.numeric(bus_301$下車招呼站代碼) <= 0), ]
nbus_301B <- bus_301[which(as.numeric(bus_301$上車招呼站代碼) - as.numeric(bus_301$下車招呼站代碼) >= 0), ]

nbus_1301B$交易票種 <- factor(nbus_1301B$交易票種, 
                          levels=c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"))
nbus_301B$交易票種 <- factor(nbus_301B$交易票種, 
                         levels=c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"))

### BUS 301區間
stop_1301 <- c("東大門夜市", "中華路", "花蓮文創園區", "遠東百貨", "明義國小", "郵政總局", "花蓮商校",
               "商校街", "花蓮火車站", "花蓮電影城")
### BUS 301區間 平日 -----
bus_1301_1 <- subset(nbus_1301B, 平日1週末0 == 1)
bus_1301_1[which(bus_1301_1$上車招呼站名稱=="花連商校"),19] <- "花蓮商校"
bus_1301_1[which(bus_1301_1$下車招呼站名稱=="花連商校"),22] <- "花蓮商校"

# 301區間 依票種 圓餅圖
df_ticket_1301_1 <- data.frame(table(bus_1301_1$交易票種))
df_ticket_1301_1$per <- df_ticket_1301_1$Freq/sum(df_ticket_1301_1$Freq)*100

p109_1 <- ggplot(df_ticket_1301_1, aes(x = "", y = per, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", color="white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, label = paste0(round(per,1), "%")), position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301區間(返程平日)票種圓餅圖") +
  theme_void() + 
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 依票種 各站點直方疊圖
df_stop_ticket_1301_1 <- melt(data.frame(table(bus_1301_1$上車招呼站名稱,bus_1301_1$交易票種)))

p109_2 <- ggplot(df_stop_ticket_1301_1, aes(x = factor(Var1, levels = stop_1301), y = value, fill = factor(Var2))) +
  geom_bar(stat="identity", position="stack",color = "white") +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301區間(返程平日)-各站點票種直方疊圖") +
  xlab("站點名稱") +
  ylab("數量") +
  theme_bw() + 
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 各站點 上車分鐘
#bus_1301_1 <- bus_1301_1[-which(bus_1301_1$上車分鐘<100), ] # 扣除不合理上車時間資料 52筆
p109_3 <- ggplot(bus_1301_1, aes(factor(上車招呼站名稱, levels = stop_1301), 上車分鐘)) +
  geom_point(aes(factor(上車招呼站名稱,stop_1301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301區間(返程平日)上車站點-上車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 各站點 下車分鐘
p109_4 <- ggplot(bus_1301_1, aes(factor(下車招呼站名稱, levels = stop_1301), 下車分鐘)) +
  geom_point(aes(factor(下車招呼站名稱,stop_1301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301區間(返程平日)下車站點-下車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 乘坐分鐘
p109_5 <- ggplot(bus_1301_1, aes(x = 乘坐分鐘 , fill = 交易票種) ) + 
  geom_histogram(bins = 50)  +
  facet_wrap(~factor(上車招呼站名稱, levels = stop_1301)) +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301區間(返程平日)上車站點-乘坐時間長條圖") + 
  ylab("次數") +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 乘坐分鐘 盒狀圖
p109_6 <- ggplot(bus_1301_1, aes(x = factor(上車招呼站名稱,levels=stop_1301), y = 乘坐分鐘)) + 
  geom_boxplot() + 
  ggtitle("109年 301區間(返程平日)上車站點-乘坐分鐘盒狀圖") + 
  ylab("乘坐分鐘") + xlab("站點名稱") + 
  theme_bw() +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))
##########
### BUS 301區間 週末 -----
bus_1301_0 <- subset(nbus_1301B, 平日1週末0 == 0)
bus_1301_0[which(bus_1301_0$上車招呼站名稱=="花連商校"),19] <- "花蓮商校"
bus_1301_0[which(bus_1301_0$下車招呼站名稱=="花連商校"),22] <- "花蓮商校"

# 301區間 依票種 圓餅圖
df_ticket_1301_0 <- data.frame(table(bus_1301_0$交易票種))
df_ticket_1301_0$per <- df_ticket_1301_0$Freq/sum(df_ticket_1301_0$Freq)*100

p109_7 <- ggplot(df_ticket_1301_0, aes(x = "", y = per, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", color="white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, label = paste0(round(per,1), "%")), position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301區間(返程週末)票種圓餅圖") +
  theme_void() + 
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 依票種 各站點直方疊圖
df_stop_ticket_1301_0 <- melt(data.frame(table(bus_1301_0$上車招呼站名稱,bus_1301_0$交易票種)))

p109_8 <- ggplot(df_stop_ticket_1301_0, aes(x = factor(Var1, levels = stop_1301), y = value, fill = factor(Var2))) +
  geom_bar(stat="identity", position="stack",color = "white") +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301區間(返程週末)-各站點票種直方疊圖") +
  xlab("站點名稱") +
  ylab("數量") +
  theme_bw() + 
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 各站點 上車分鐘
#bus_2301 <- bus_2301[-which(bus_2301$上車分鐘<100), ] # 扣除不合理上車時間資料 52筆
p109_9 <- ggplot(bus_1301_0, aes(factor(上車招呼站名稱, levels = stop_1301), 上車分鐘)) +
  geom_point(aes(factor(上車招呼站名稱,stop_1301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301區間(返程週末)上車站點-上車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 各站點 下車分鐘
p109_10 <- ggplot(bus_1301_0, aes(factor(下車招呼站名稱, levels = stop_1301), 下車分鐘)) +
  geom_point(aes(factor(下車招呼站名稱,stop_1301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301區間(返程週末)下車站點-下車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 乘坐分鐘
p109_11 <- ggplot(bus_1301_0, aes(x = 乘坐分鐘 , fill = 交易票種) ) + 
  geom_histogram(bins = 50)  +
  facet_wrap(~factor(上車招呼站名稱, levels = stop_1301)) +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301(返程週末)上車站點-乘坐時間長條圖") + 
  ylab("次數") +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 301區間 乘坐分鐘 盒狀圖
p109_12 <- ggplot(bus_1301_0, aes(x = factor(上車招呼站名稱,levels=stop_1301), y = 乘坐分鐘)) + 
  geom_boxplot() + 
  ggtitle("109年 301區間(返程週末)上車站點-乘坐分鐘盒狀圖") + 
  ylab("乘坐分鐘") + xlab("站點名稱") + 
  theme_bw() +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

### BUS 2301
stop_2301 <- c("花蓮火車站", "花蓮電影城", "商校街站", "花蓮商校", "郵政總局", "明義國小", "遠東百貨",
               "花蓮文創園區", "中華路站", "東大門夜市", "重慶市場(石藝大街)", "信義國小", "南濱公園", 
               "武聖宮(榮光社區)", "燕聲廣播", "仁安社區", "阿美麻糬", "新天堂樂園", "光華樂活創意園區", 
               "中華紙漿", "大學路口", "東華大學", "行政大樓", "原住民學院", "育成中心", "圖書館資訊大樓")
### BUS 2301 平日 -----
bus_2301_1 <- subset(nbus_301B, 平日1週末0 == 1)

# 2301 依票種 圓餅圖
df_ticket_2301_1 <- data.frame(table(bus_2301_1$交易票種))
df_ticket_2301_1$per <- df_ticket_2301_1$Freq/sum(df_ticket_2301_1$Freq)*100

p109_13 <- ggplot(df_ticket_2301_1, aes(x = "", y = per, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", color="white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, label = paste0(round(per,1), "%")), position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  guides(fill=guide_legend(title="票種")) +
  ggtitle("109年 301(返程平日)票種圓餅圖") +
  theme_void() + 
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 依票種 各站點直方疊圖
df_stop_ticket_2301_1 <- melt(data.frame(table(bus_2301_1$上車招呼站名稱,bus_2301_1$交易票種)))

p109_14 <- ggplot(df_stop_ticket_2301_1, aes(x = factor(Var1, levels = stop_2301), y = value, fill = factor(Var2))) +
  geom_bar(stat="identity", position="stack",color = "white") +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301(返程平日)-各站點票種直方疊圖") +
  xlab("站點名稱") +
  ylab("數量") +
  theme_bw() + 
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 上車分鐘
#bus_2301_1 <- bus_2301_1[-which(bus_2301_1$上車分鐘<100), ] # 扣除不合理上車時間資料 52筆
p109_15 <- ggplot(bus_2301_1, aes(factor(上車招呼站名稱, levels = stop_2301), 上車分鐘)) +
  geom_point(aes(factor(上車招呼站名稱,stop_2301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301(返程平日)上車站點-上車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 下車分鐘
p109_16 <- ggplot(bus_2301_1, aes(factor(下車招呼站名稱, levels = stop_2301), 下車分鐘)) +
  geom_point(aes(factor(下車招呼站名稱,stop_2301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301(返程平日)下車站點-下車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘
p109_17 <- ggplot(bus_2301_1, aes(x = 乘坐分鐘 , fill = 交易票種) ) + 
  geom_histogram(bins = 50)  +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  facet_wrap(~factor(上車招呼站名稱, levels = stop_2301)) +
  ggtitle("109年 301(返程平日)上車站點-乘坐時間長條圖") + 
  ylab("次數") +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘 盒狀圖
p109_18 <- ggplot(bus_2301_1, aes(x = factor(上車招呼站名稱,levels=stop_2301), y = 乘坐分鐘)) + 
  geom_boxplot() + 
  ggtitle("109年 301(返程平日)上車站點-乘坐分鐘盒狀圖") + 
  ylab("乘坐分鐘") + xlab("站點名稱") + 
  theme_bw() +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))
##########
### BUS 2301 週末 -----
bus_2301_0 <- subset(nbus_301B, 平日1週末0 == 0)

# 2301 依票種 圓餅圖
df_ticket_2301_0 <- data.frame(table(bus_2301_0$交易票種))
df_ticket_2301_0$per <- df_ticket_2301_0$Freq/sum(df_ticket_2301_0$Freq)*100

p109_19 <- ggplot(df_ticket_2301_0, aes(x = "", y = per, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", color="white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, label = paste0(round(per,1), "%")), position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301(返程週末)票種圓餅圖") +
  theme_void() + 
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 依票種 各站點直方疊圖
df_stop_ticket_2301_0 <- melt(data.frame(table(bus_2301_0$上車招呼站名稱,bus_2301_0$交易票種)))

p109_20 <- ggplot(df_stop_ticket_2301_0, aes(x = factor(Var1, levels = stop_2301), y = value, fill = factor(Var2))) +
  geom_bar(stat="identity", position="stack",color = "white") +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  ggtitle("109年 301(返程週末)-各站點票種直方疊圖") +
  xlab("站點名稱") +
  ylab("數量") +
  theme_bw() + 
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 上車分鐘
#bus_2301 <- bus_2301[-which(bus_2301$上車分鐘<100), ] # 扣除不合理上車時間資料 52筆
p109_21 <- ggplot(bus_2301_0, aes(factor(上車招呼站名稱, levels = stop_2301), 上車分鐘)) +
  geom_point(aes(factor(上車招呼站名稱,stop_2301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301(返程週末)上車站點-上車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 各站點 下車分鐘
p109_22 <- ggplot(bus_2301_0, aes(factor(下車招呼站名稱, levels = stop_2301), 下車分鐘)) +
  geom_point(aes(factor(下車招呼站名稱,stop_2301), color = 交易票種)) +
  scale_color_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  theme_bw() + 
  ggtitle("109年 301(返程週末)下車站點-下車分鐘點圖") +
  xlab("站點名稱") +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘
p109_23 <- ggplot(bus_2301_0, aes(x = 乘坐分鐘 , fill = 交易票種) ) + 
  geom_histogram(bins = 50)  +
  scale_fill_manual(name = "票種",
                    labels = c("一般票","兒童票","陪伴票","敬老票","優待票","學生票","原住民票"),
                    values=colorlabel) +
  facet_wrap(~factor(上車招呼站名稱, levels = stop_2301)) +
  ggtitle("109年 301(返程週末)上車站點-乘坐時間長條圖") + 
  ylab("次數") +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# 2301 乘坐分鐘 盒狀圖
p109_24 <- ggplot(bus_2301_0, aes(x = factor(上車招呼站名稱,levels=stop_2301), y = 乘坐分鐘)) + 
  geom_boxplot() + 
  ggtitle("109年 301(返程週末)上車站點-乘坐分鐘盒狀圖") + 
  ylab("乘坐分鐘") + xlab("站點名稱") + 
  theme_bw() +
  coord_flip() +
  theme(text=element_text(family="黑體-繁 中黑", size=14))

# heaven
h109_bus_2301_1 <- bus_2301_1[which(bus_2301_1$上車招呼站名稱=="新天堂樂園"),]
h109_bus_2301_0 <- bus_2301_0[which(bus_2301_0$上車招呼站名稱=="新天堂樂園"),]

# NDHU Library
l109_bus_2301_1 <- bus_2301_1[which(bus_2301_1$上車招呼站名稱=="圖書館資訊大樓"),]
l109_bus_2301_0 <- bus_2301_0[which(bus_2301_0$上車招呼站名稱=="圖書館資訊大樓"),]

# 東大門
h109_bus_1301_1 <- bus_1301_1[which(bus_1301_1$上車招呼站名稱=="東大門夜市"),]
h109_bus_1301_0 <- bus_1301_0[which(bus_1301_0$上車招呼站名稱=="東大門夜市"),]






