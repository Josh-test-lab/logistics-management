library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(data.table)

df11 <- fread("308 110.01-111.04 整理後.csv")
df12 <- fread("308 111.04-112.02 整理後.csv")
df1 <- rbind(df11, df12)
name   <- fread("308車序.csv")
ticket <- fread("308310全部票種.csv")

# 上車時間日期
df1$上車日期  <- as.POSIXct(df1$上車日期, format = "%Y/%m/%d") 
df1$上車年  <- format(df1$上車日期, "%Y")
df1$上車月 <- format(df1$上車日期, "%m")
df1$上車日   <- format(df1$上車日期, "%d")

# 下車時間日期
df1$下車日期  <- as.POSIXct(df1$下車日期, format = "%Y/%m/%d") 
df1$下車年  <- format(df1$下車日期, "%Y")
df1$下車月 <- format(df1$下車日期, "%m")
df1$下車日   <- format(df1$下車日期, "%d")


#df1 <- df1[,c(3:6,8,11,27,28,29,17,15,21,19)]

#unique(df1$BoardingStopName)
#unique(df1$DeboardingStopName)
#setdiff(df1$BoardingStopName,df1$DeboardingStopName)
#df1 <- df1[-which(df1$DeboardingStopName=="-99" | df1$BoardingYear=="2024"),]

colnames(df1)[c(1,14,15,16,5,7,9,11)] <- c("路線","年","月","日","上車時間","上車站名","下車時間","下車站名")

# 110
# d301 <- df1[which(df1$路線=="301" & df1$年 == "2023"),]
# d302 <- df1[which(df1$路線=="302" & df1$年 == "2023"),]
 d308 <- df1[which(df1$路線=="308" & df1$年 == "2023"),]
# d305 <- df1[which(df1$路線=="305" & df1$年 == "2023"),]

newdata <- df1
p = dim(newdata)[1]#維度 - 列
q = dim(newdata)[2]#變數 - 欄

Onhour <- matrix(0, p)
Onmin <- matrix(0, p)
Onsec <- matrix(0, p)
for(i in 1 : p){
  Onhour[i] <- substring(newdata$上車時間[i],1,2)
  Onmin[i] <- substring(newdata$上車時間[i],4,5)
  Onsec[i] <- substring(newdata$上車時間[i],7,8)
}
Onnmin <- as.data.frame (as.numeric(Onhour)*60 + as.numeric(Onmin))

#####搭車時間#####
#Ofhour <- matrix(0, p)
#Ofmin <- matrix(0, p)
#Ofsec <- matrix(0, p)
#for(i in 1 : p){
#  Ofhour[i] <- substring(newdata$下車時間[i],1,2)
#  Ofmin[i] <- substring(newdata$下車時間[i],4,5)
#  Ofsec[i] <- substring(newdata$下車時間[i],7,8)
#}
#Ofnmin <- as.data.frame (as.numeric(Ofhour)*60 + as.numeric(Ofmin))
#colnames(Onnmin) <- "Onnmin"
#colnames(Ofnmin) <- "Ofnmin"
#newdata <- cbind(newdata,Onnmin)
#newdata <- cbind(newdata,Ofnmin)
#time <- c()
#for(i in 1:p){
#  time[i] <-  abs(Onnmin[i,1] - Ofnmin[i,1])
#}
#time <- as.data.frame(time)
#newdata <- cbind(newdata,time)

# 平日：1、假日：0
install.packages("lubridate")
library(lubridate)
newdata$日期 <- with(newdata,make_date(as.numeric(年),as.numeric(月),as.numeric(日)))
newdata$周末平日 <- wday(newdata$日期, label = TRUE) %in% c("週六", "週日")
newdata$周末平日 <- ifelse(newdata$周末平日==TRUE,0,1)
newdata$周末平日 <- factor(newdata$周末平日,levels = c("1","0"))

# 交易票種
colnames(newdata)[2] <- "交易票種"
setdiff(newdata$交易票種,ticket$交易票種)
#newdata <-merge(newdata , ticket , by = "交易票種") 

# 判斷去返
nameg <- name[,1:2]
colnames(nameg) <- c("上車站名","上車站序")
nameb <- name[,1:2]
colnames(nameb) <- c("下車站名","下車站序")
newdata<-merge(newdata,nameg,by = "上車站名")
newdata<-merge(newdata,nameb,by = "下車站名")


# 更正站名
#newdata$上車站名[newdata$上車站名 == "慶修院"] <- "吉安鄉公所(慶修院)"
#newdata$下車站名[newdata$下車站名 == "慶修院"] <- "吉安鄉公所(慶修院)"
#newdata$上車站名[newdata$上車站名 == "潭南遊憩區"] <- "鯉魚潭潭南遊憩區"
#newdata$下車站名[newdata$下車站名 == "潭南遊憩區"] <- "鯉魚潭潭南遊憩區"
#newdata$上車站名[newdata$上車站名 == "豐華再現館(雲山水)"] <- "豐華再現館"
#newdata$下車站名[newdata$下車站名 == "豐華再現館(雲山水)"] <- "豐華再現館"


newdata <- newdata[,c(1,2,3,6,7,10,8,11,13,12,4,22,23,14)]
colnames(newdata) <- c("下車站名","上車站名","路線","上車日期","上車時間","下車時間","上車分鐘"
                       ,"下車分鐘","乘坐分鐘","星期","票種","上車站序","下車站序","年")#命名新標題

#newdata <- rbind(newdata,newdata1)

newdata <- relocate(newdata,上車分鐘, .after = 上車時間)
newdata <- relocate(newdata,上車站序, .after = 上車分鐘)
newdata <- relocate(newdata,上車站名, .after = 上車站序)
newdata <- relocate(newdata,下車站序, .after = 下車分鐘)
newdata <- relocate(newdata,下車站名, .after = 下車站序)
save(newdata, file = "308.Rdata")



# newdata308 <- readRDS("D:/mushroom/109運籌/data/110308new.rds")
# newdata <- merge(newdata,newdata308, all.x = TRUE)
# newdata$票種[which(is.na(newdata$TPASS)==F)] <- "月票"




# saveRDS(gnewdata,"D:/mushroom/109運籌/太魯閣客運/110圖/308/110308gnewdata.rds")
# saveRDS(bnewdata,"D:/mushroom/109運籌/太魯閣客運/110圖/308/110308bnewdata.rds")



#####分析#####
library(datasets)
library(cluster)
library(stringr)
library(ggplot2)
library(data.table)
#install.packages('R.utils')
load("308.Rdata")
setwd("112")

newdata <- newdata[which(newdata$年 == "2023"),]
gnewdata <- newdata[which(as.numeric(newdata$上車站序) - as.numeric(newdata$下車站序) <= 0), ]
bnewdata <- newdata[which(as.numeric(newdata$上車站序) - as.numeric(newdata$下車站序) >= 0), ]
table(gnewdata$票種)
table(bnewdata$票種)
table(newdata$票種)

qplot(x = 票種, data = newdata, geom = "bar", main = "112年308路線搭車票種", xlab = "票種", fill = 票種) + 
  scale_fill_manual(values=c("#00DDFF","#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + 
  geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
ggsave("112年308路線搭車票種.jpeg", width=13.79, height=8.25)
qplot(x = 票種, data = gnewdata, geom = "bar", main = "112年308路線去程搭車票種", xlab = "票種", fill = 票種) + 
  scale_fill_manual(values=c("#00DDFF","#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + 
  geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
ggsave("112年308路線去程搭車票種.jpeg", width=13.79, height=8.25)
qplot(x = 票種, data = bnewdata, geom = "bar", main = "112年308路線回程搭車票種", xlab = "票種", fill = 票種) + 
  scale_fill_manual(values=c("#00DDFF","#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + 
  geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
ggsave("112年308路線回程搭車票種.jpeg", width=13.79, height=8.25)

a1 <- name[1:33,1]
b1 <- data.frame(r308 = rev(a1$r308))

ggplot(gnewdata, aes(factor(上車站名,levels=a1$r308), 上車分鐘, color = 票種)) + 
  geom_point() + ggtitle("112年路線308(去程)上車站點-上車分鐘票種點圖") + 
  ylab("上車分鐘") + xlab("308站點") + 
  scale_color_manual(values=c("#00DDFF","#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + 
  theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
ggsave("112年路線308(去程)上車站點-上車分鐘票種點圖.jpeg", width=13.79, height=8.25)

ggplot(bnewdata, aes(factor(bnewdata$上車站名,levels=b1$r308), bnewdata$上車分鐘, color = 票種)) + 
  geom_point() + ggtitle("112年路線308(返程)上車站點-上車分鐘票種點圖") + 
  ylab("上車分鐘") + xlab("308站點") + 
  scale_color_manual(values=c("#00DDFF","#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + 
  theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
ggsave("112年路線308(返程)上車站點-上車分鐘票種點圖.jpeg", width=13.79, height=8.25)

#全部

#ggplot(newdata, aes(factor(newdata$上車站名,levels=a1), fill=newdata$票種)) + geom_bar(position="stack") + ggtitle("107年308上車站點-票種直方疊圖") + ylab("數量") + xlab("308站點") + labs(fill = "308交易票種") + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#ggplot(newdata, aes(factor(newdata$下車站名,levels=a1), fill=newdata$票種)) + geom_bar(position="stack") + ggtitle("107年308下車站點-票種直方疊圖") + ylab("數量") + xlab("308站點") + labs(fill = "308交易票種") + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#去程

#ggplot(gnewdata, aes(factor(gnewdata$上車站名,levels=a1), fill=gnewdata$票種)) + geom_bar(position="stack") + ggtitle("107年308(去程)上車站點-票種直方疊圖") + ylab("數量") + xlab("308站點") + labs(fill = "308交易票種") + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#ggplot(L350G, aes(factor(L350G$下車站名,levels=a1), fill=L350G$票種)) + geom_bar(position="stack") + ggtitle("107年350(去程)下車站點-票種直方疊圖") + ylab("數量") + xlab("350站點") + labs(fill = "350交易票種") + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#回程

#ggplot(bnewdata, aes(factor(bnewdata$上車站名,levels=b1), fill=bnewdata$票種)) + geom_bar(position="stack") + ggtitle("107年308(返程)上車站點-票種直方疊圖") + ylab("數量") + xlab("308站點") + labs(fill = "308交易票種") + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#ggplot(L350B, aes(factor(L350B$下車站名,levels=b1), fill=L350B$票種)) + geom_bar(position="stack") + ggtitle("107年350(回程)下車站點-票種直方疊圖") + ylab("數量") + xlab("350站點") + labs(fill = "350交易票種") + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#分平日假日
gnewdataa <- gnewdata[which(gnewdata$星期 == "1"),]
gnewdatab <- gnewdata[which(gnewdata$星期 == "0"),]
bnewdataa <- bnewdata[which(bnewdata$星期 == "1"),]
bnewdatab <- bnewdata[which(bnewdata$星期 == "0"),]

#平假日製圖
#qplot(x = L350a$票種, data = L350a, geom = "bar", main = "107年350路線搭車票種(平日)", xlab = "票種", fill = 票種) + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
#qplot(x = L350b$票種, data = L350b, geom = "bar", main = "107年350路線搭車票種(假日)", xlab = "票種", fill = 票種) + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
#qplot(x = L350Ga$票種, data = L350Ga, geom = "bar", main = "107年350路線去程搭車票種(平日)", xlab = "票種", fill = 票種) + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
#qplot(x = L350Gb$票種, data = L350Gb, geom = "bar", main = "107年350路線去程搭車票種(假日)", xlab = "票種", fill = 票種) + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
#qplot(x = L350Ba$票種, data = L350Ba, geom = "bar", main = "107年350路線返程搭車票種(平日)", xlab = "票種", fill = 票種) + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖
#qplot(x = L350Bb$票種, data = L350Bb, geom = "bar", main = "107年350路線返程搭車票種(假日)", xlab = "票種", fill = 票種) + scale_fill_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + geom_text(stat="count",aes(label=paste0(round(..count..*100/sum(..count..)),"%" )),vjust=-0.2) #圖形=bar 複合式的長條圖

#去程平日
#直方圖
ggplot(gnewdataa, aes(y = factor(gnewdataa$上車站名,levels=a1$r308))) + 
  geom_bar(aes(fill = 票種),colour = "black",position="stack") + ggtitle("112年路線308去程平日-各上車站點票種直方疊圖") + 
  ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + 
  scale_fill_grey() + 
  theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308去程平日-各上車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#ggplot(L350Ga, aes(y = factor(L350Ga$下車站名,levels=b1), fill=L350Ga$票種)) + geom_bar(position="stack") + ggtitle("350[台東(經大南) – 安朔去程平日]-各下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#分鐘圖
#ggplot(L350Ga, aes(factor(L350Ga$上車站名,levels=a1), L350Ga$上車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350(去程平日)上車站點-上車分鐘票種點") + ylab("上車分鐘") + xlab("350站點") + scale_color_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
#ggplot(L350Ga, aes(factor(L350Ga$下車站名,levels=a1), L350Ga$下車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350下車站名與下車分鐘(去程平日)") + ylab("下車分鐘") + xlab("350站點") + scale_color_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))

#去程假日
#直方圖
ggplot(gnewdatab, aes(y = factor(gnewdatab$上車站名,levels=a1$r308))) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308去程假日-各上車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + 
  scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308去程假日-各上車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#ggplot(L350Gb, aes(y = factor(L350Gb$下車站名,levels=b1), fill=L350Gb$票種)) + geom_bar(position="stack") + ggtitle("350[台東(經大南) – 安朔去程假日]-各下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#分鐘圖
#ggplot(L350Gb, aes(factor(L350Gb$上車站名,levels=a1), L350Gb$上車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350(去程假日)上車站點-上車分鐘票種點") + ylab("上車分鐘") + xlab("350站點") + scale_color_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
#ggplot(L350Gb, aes(factor(L350Gb$下車站名,levels=a1), L350Gb$下車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350下車站名與下車分鐘(去程假日)") + ylab("下車分鐘") + xlab("350站點") + scale_color_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))

#返程平日
#直方圖
ggplot(bnewdataa, aes(y = factor(bnewdataa$上車站名,levels=b1$r308))) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308返程平日-各上車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + 
  scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308返程平日-各上車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#ggplot(L350Ba, aes(y = factor(L350Ba$下車站名,levels=a1), fill=L350Ba$票種)) + geom_bar(position="stack") + ggtitle("350[台東(經大南) – 安朔返程平日]-各下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#分鐘圖
#ggplot(L350Ba, aes(factor(L350Ba$上車站名,levels=b1), L350Ba$上車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350(返程平日)上車站點-上車分鐘票種點") + ylab("上車分鐘") + xlab("350站點") + scale_color_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
#ggplot(L350Ba, aes(factor(L350Ba$下車站名,levels=b1), L350Ba$下車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350下車站名與下車分鐘(返程平日)") + ylab("下車分鐘") + xlab("350站點") + scale_color_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))

#返程假日
#直方圖
ggplot(bnewdatab, aes(y = factor(bnewdatab$上車站名,levels=b1$r308))) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308返程假日-各上車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + 
  scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308返程假日-各上車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#ggplot(L350Bb, aes(y = factor(L350Bb$下車站名,levels=a1), fill=L350Bb$票種)) + geom_bar(position="stack") + ggtitle("350[台東(經大南) – 安朔返程假日]-各下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
#分鐘圖
#ggplot(L350Bb, aes(factor(L350Bb$上車站名,levels=b1), L350Bb$上車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350(返程假日)上車站點-上車分鐘票種點") + ylab("上車分鐘") + xlab("350站點") + scale_color_manual(values=c("#50C878","#4d8f13","#6495ED","#FF4D00","#FFD000")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
#ggplot(L350Bb, aes(factor(L350Bb$下車站名,levels=b1), L350Bb$下車分鐘, color = 票種)) + geom_point() + ggtitle("107年路線350下車站名與下車分鐘(返程假日)") + ylab("下車分鐘") + xlab("350站點") + scale_color_manual(values=c("#FFBF00", "#DA70D6", "#50C878", "#6495ED","#FF4D00")) + theme(panel.background = element_rect(fill = 'transparent', color = "gray"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))


#熱門站點分析
#a:平日,b:假日
cityGa <- subset(gnewdataa,gnewdataa$上車站名 == "花蓮火車站"|gnewdataa$上車站名 == "花蓮商校"|gnewdataa$上車站名 == "東大門夜市")
cityGb <- subset(gnewdatab,gnewdatab$上車站名 == "花蓮火車站"|gnewdatab$上車站名 == "花蓮商校"|gnewdatab$上車站名 == "東大門夜市")
cityBa <- subset(bnewdataa,bnewdataa$上車站名 == "七星潭" | bnewdataa$上車站名 == "市老人會"| bnewdataa$上車站名 == "東大門夜市")
cityBb <- subset(bnewdatab,bnewdatab$上車站名 == "七星潭" | bnewdatab$上車站名 == "東大門夜市" | bnewdatab$上車站名 == "市老人會"| bnewdatab$上車站名 == "台灣企銀")

#直方圖
#去程平日
ggplot(cityGa, aes(y = factor(cityGa$下車站名,levels=a1$r308))) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308去程平日-熱門站點下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308去程平日-熱門站點下車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#去程假日
ggplot(cityGb, aes(y = factor(cityGb$下車站名,levels=a1$r308), fill=cityGb$票種)) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308去程假日-熱門站點下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308去程假日-熱門站點下車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#返程平日
ggplot(cityBa, aes(y = factor(cityBa$下車站名,levels=b1$r308), fill=cityBa$票種)) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308返程平日-熱門站點下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308返程平日-熱門站點下車站點票種直方疊圖.jpeg", width=13.79, height=8.25)
#返程假日
ggplot(cityBb, aes(y = factor(cityBb$下車站名,levels=b1$r308), fill=cityBb$票種)) + geom_bar(aes(fill = 票種),colour = "black",position="stack") + 
  ggtitle("112年路線308返程假日-熱門站點下車站點票種直方疊圖") + ylab("站點名稱") + xlab("數量") + labs(fill = "票種") + scale_fill_grey() + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,color = "black",size=9))
ggsave("112年路線308返程假日-熱門站點下車站點票種直方疊圖.jpeg", width=13.79, height=8.25)

#盒狀圖
ggplot(cityGa, aes(y = factor(cityGa$上車站名,levels=a1$r308), x = cityGa$乘坐分鐘)) + geom_boxplot() + 
  ggtitle("112年路線308去程平日上車站點-乘坐分鐘盒狀圖") + ylab("站點名稱") + xlab("乘坐分鐘") + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
ggsave("112年路線308去程平日上車站點-乘坐分鐘盒狀圖.jpeg", width=13.79, height=8.25)
ggplot(cityGb, aes(y = factor(cityGb$上車站名,levels=a1$r308), x = cityGb$乘坐分鐘)) + geom_boxplot() + 
  ggtitle("112年路線308去程假日上車站點-乘坐分鐘盒狀圖") + ylab("站點名稱") + xlab("乘坐分鐘") + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
ggsave("112年路線308去程假日上車站點-乘坐分鐘盒狀圖.jpeg", width=13.79, height=8.25)
ggplot(cityBa, aes(y = factor(cityBa$上車站名,levels=b1$r308), x = cityBa$乘坐分鐘)) + geom_boxplot() + 
  ggtitle("112年路線308返程平日]上車站點-乘坐分鐘盒狀圖") + ylab("站點名稱") + xlab("乘坐分鐘") + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
ggsave("112年路線308返程平日上車站點-乘坐分鐘盒狀圖.jpeg", width=13.79, height=8.25)
ggplot(cityBb, aes(y = factor(cityBb$上車站名,levels=b1$r308), x = cityBb$乘坐分鐘)) + geom_boxplot() + 
  ggtitle("112年路線308返程假日]上車站點-乘坐分鐘盒狀圖") + ylab("站點名稱") + xlab("乘坐分鐘") + theme(panel.background = element_rect(fill = "white", colour = "gray"), panel.grid.major=element_line(colour='#EDEDED'), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 9))
ggsave("112年路線308返程假日上車站點-乘坐分鐘盒狀圖.jpeg", width=13.79, height=8.25)

#分面長條圖
ggplot(cityGa, aes(x = 乘坐分鐘) ) + 
  geom_histogram(aes(fill = 票種),colour = "black",bins = 50) + 
  facet_wrap(~factor(cityGa$上車站名, levels=a1$r308),ncol = 3, scales = "free") + 
  ggtitle("112年路線308去程平日上車站點-乘坐時間長條圖") + 
  ylab("次數") + xlab("乘坐分鐘") + 
  scale_fill_grey() + 
  theme_bw() + theme(panel.background = element_rect(fill = "white", color = "gray"), panel.grid.major=element_line(colour='#EDEDED'), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 9), plot.title = element_text(colour = "black", face = "bold", size = 10))
ggsave("112年路線308去程平日上車站點-乘坐時間長條圖.jpeg", width=13.79, height=8.25)
ggplot(cityGb, aes(x = 乘坐分鐘) ) + 
  geom_histogram(aes(fill = 票種),colour = "black",bins = 50) + 
  facet_wrap(~factor(cityGb$上車站名, levels=a1$r308),ncol = 3, scales = "free") + 
  ggtitle("112年路線308去程假日上車站點-乘坐時間長條圖") + 
  ylab("次數") + xlab("乘坐分鐘") + scale_fill_grey() + theme_bw() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 9), plot.title = element_text(colour = "black", face = "bold", size = 10))
ggsave("112年路線308去程假日上車站點-乘坐時間長條圖.jpeg", width=13.79, height=8.25)
ggplot(cityBa, aes(x = 乘坐分鐘) ) + 
  geom_histogram(aes(fill = 票種),colour = "black",bins = 50) + 
  facet_wrap(~factor(cityBa$上車站名, levels=b1$r308),ncol = 3, scales = "free") + 
  ggtitle("112年路線308返程平日上車站點-乘坐時間長條圖") + 
  ylab("次數") + xlab("乘坐分鐘") + scale_fill_grey() + theme_bw() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 9), plot.title = element_text(colour = "black", face = "bold", size = 10))
ggsave("112年路線308返程平日上車站點-乘坐時間長條圖.jpeg", width=13.79, height=8.25)
ggplot(cityBb, aes(x = 乘坐分鐘) ) + 
  geom_histogram(aes(fill = 票種),colour = "black",bins = 50) + 
  facet_wrap(~factor(cityBb$上車站名, levels=b1$r308),ncol = 3, scales = "free") + 
  ggtitle("112年路線308返程假日上車站點-乘坐時間長條圖") + 
  ylab("次數") + xlab("乘坐分鐘") + 
  scale_fill_grey() + 
  theme_bw() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 9), plot.title = element_text(colour = "black", face = "bold", size = 10))
ggsave("112年路線308返程假日上車站點-乘坐時間長條圖.jpeg", width=13.79, height=8.25)



data <- subset(newdata,newdata$上車站名 == "台東火車站"| newdata$上車站名 == "台東航空站")
data1 <- subset(data,data$下車站名 == "台東火車站"| data$下車站名 == "台東航空站")
median(data1$乘坐分鐘)
