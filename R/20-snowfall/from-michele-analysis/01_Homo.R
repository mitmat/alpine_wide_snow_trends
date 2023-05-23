library(trend)
library(stringr)
library(climatol)
library(dplyr)
library(ggplot2)



meta <- read.csv("00_Data/Meta.csv", header=T, sep=";")
data <- read.csv("00_Data/HNData/Data_seasonal.csv", header=T, sep=",")
data$Date_start <- as.Date(data$Date_start)
data$Date_end <- as.Date(data$Date_end)





##### PETTITT TEST #####
### CP for each time series ###
cp <- data.frame(meta, Cp=NA, M1=NA, M2=NA, D=NA, Sign=NA)

for (i in 1:length(meta$Name)) {
  
  data.x <- subset(data, data$Name == meta$Name[i])
  data.x.NA <- na.omit(data.x)
  
  id.p.x <- pettitt.test(data.x.NA$HN)$estimate[1]
  p.x1 <- data.x.NA$Date_start[id.p.x]
  p.x2 <- data.x.NA$Date_end[id.p.x]
  p.x.char <- paste(format(p.x1, "%Y"), str_sub(format(p.x2, "%Y"),3,4), sep="-")
  
  cp$Cp[i] <- p.x.char
  cp$M1[i] <- round(mean(data.x.NA$HN[1:id.p.x]), 2)
  cp$M2[i] <- round(mean(data.x.NA$HN[(id.p.x+1):nrow(data.x.NA)]), 2)
  cp$D[i] <- cp$M2[i] - cp$M1[i]
  cp$Sign[i] <- sign(cp$D[i])
  
  png(paste0("01_Homo/Pettitt/",meta$Name[i],".png"), width=1800, height=800, res=200)

  print(ggplot(data.x.NA, aes(x=Date_start, y=HN)) +
          geom_point(size=1.2, shape=19) +
          geom_vline(xintercept=p.x1, linetype="dashed", color="red", size=0.7) +
          labs(title=paste0("Pettitt's Test: ",meta$Name[i]," [",meta$Provider[i],"]"),
               x=element_blank(),
               y="Seasonal fresh snow [cm]") +
          theme_bw() +
          theme(plot.title=element_text(size=17, hjust=0.5, face="bold", family="serif"),
                axis.text.x=element_text(size=9, family="serif"),
                axis.text.y=element_text(size=9, family="serif"),
                axis.title.x=element_text(size=12, family="serif"),
                axis.title.y=element_text(size=12, vjust=3.5, family="serif"),
                plot.margin=unit(c(1,0.5,0.5,1.5),"lines")) +
          annotate(geom="text", label=p.x.char, x=p.x1, y=max(data.x.NA$HN),
                   angle=90, hjust=0.7, vjust=1.5, size=2.5, col="red"))

  dev.off()
  
}

# write.table(cp, "01_Homo/Pettitt/CP.csv", sep=",", row.names=F)

cp.plot <- data.frame(Name=meta$Name,
                      Elevation=meta$Elevation,
                      Cp=as.Date(paste(str_sub(cp$Cp,1,4), "01", "01", sep="-")),
                      Type=factor(cp$Sign, labels=c("Negative changing points","Positive changing points")))


# cp.plot <- data.frame(cp, Shape=25, Col="red")
# cp.plot$Cp <- as.Date(paste(str_sub(cp$Cp,1,4), "01", "01", sep="-"))
# cp.plot$Shape[which(cp.plot$D > 0)] <- 24
# cp.plot$Col[which(cp.plot$D > 0)] <- "blue"



### CP vs altitude ###
pdf("01_Homo/Pettitt/CP.pdf", width=10, height=5)
ggplot(cp.plot, aes(x=Cp, y=Elevation, fill=Type, shape=Type)) +
  geom_point(col="black", size=2, stroke=0.5) +
  scale_shape_manual(name=element_blank(), values=c(25,24)) +
  scale_fill_manual(name=element_blank(), values=c("red","blue")) +
  labs(x=element_blank(), y="Elevation [m]") +
  theme_bw() +
  theme(legend.title=element_blank(), legend.position=c(0.12,0.9),
        legend.box.background=element_rect(color="grey", linewidth=1))
dev.off()


# png("01_Homo/Pettitt/CP.png", width=1800, height=800, res=200)
# ggplot(cp.plot, aes(x=Cp, y=Elevation)) +
#   geom_point(shape=cp.plot$Shape, fill=cp.plot$Col, col="black", size=2, stroke=0.7) +
#   labs(title="Pettitt's Test: Changing Points",
#        x=element_blank(),
#        y="Elevation [m]") +
#   theme_bw() +
#   theme(plot.title=element_text(size=17, hjust=0.5, face="bold", family="serif"),
#         axis.text.x=element_text(size=9, family="serif"),
#         axis.text.y=element_text(size=9, family="serif"),
#         axis.title.x=element_text(size=12, family="serif"),
#         axis.title.y=element_text(size=12, vjust=3.5, family="serif"),
#         plot.margin=unit(c(1,0.5,0.5,1.5),"lines"))
# dev.off()



### CP density ###
pdf("01_Homo/Pettitt/CP_density.pdf", width=10, height=5)
ggplot(cp.plot, aes(x=Cp)) +
  geom_density() +
  labs(x=element_blank()) +
  theme_bw()
dev.off()



### CP frequency ###
png("01_Homo/Pettitt/CP_freq.png", width=1800, height=800, res=200)
ggplot(cp.plot, aes(x=Cp)) +
  geom_histogram(bins=35) +
  labs(title="Pettitt's Test: Changing Points Frequency",
       x=element_blank(),
       y="# Number of station") +
  theme_bw() +
  theme(plot.title=element_text(size=17, hjust=0.5, face="bold", family="serif"),
        axis.text.x=element_text(size=9, family="serif"),
        axis.text.y=element_text(size=9, family="serif"),
        axis.title.x=element_text(size=12, family="serif"),
        axis.title.y=element_text(size=12, vjust=3.5, family="serif"),
        plot.margin=unit(c(1,0.5,0.5,1.5),"lines"))
dev.off()





##### CLIMATOL TEST #####
# ### Example data ###
# # Load & save the example data #
# data(Ttest)
# write(dat, "01_Homo/Climatol/Example/Ttest_1981-2000.dat")
# write.table(est.c, "01_Homo/Climatol/Example/Ttest_1981-2000.est", row.names=FALSE, col.names=FALSE)



### Data setting ###
n <- meta$Name
s <- paste(1850:2019, str_sub(1851:2020,3,4), sep="-")
data2 <- as.data.frame(matrix(nrow=length(s), ncol=length(n)+1))
data2[,1] <- s
colnames(data2) <- c("Date",n)

for (i in 1:length(n)) {
  
  data.x <- subset(data, data$Name == n[i])
  data.x$Date <- paste(format(data.x$Date_start[1],"%Y"):format(tail(data.x$Date_start,1),"%Y"),
                       str_sub(format(data.x$Date_end[1],"%Y"):format(tail(data.x$Date_end,1),"%Y"),3,4),
                       sep="-")
  data.x <- data.x[,c(1:3,5,4)]
  
  data2[,i+1] <- left_join(data2[,c(1,i+1)], data.x, by="Date")[,6]
  
}

data2 <- data2[,-1]
rownames(data2) <- s
# n.stats <- rowSums(!is.na(data2))

dat <- subset(data2, rownames(data2) >= "1865-66")
dat <- as.matrix(dat)
met <- meta[,c(3:5,1:2)]
colnames(met)[1:3] <- c("X","Y","Z")

# write(dat, "01_Homo/Climatol/HN-s_1865-2019.dat")
# write.table(met, "01_Homo/Climatol/HN-s_1865-2019.est", row.names=FALSE, col.names=FALSE)




### Climatol exploratory analysis ###
homogen("01_Homo/Climatol/HN-s", 1865, 2019, std=2, expl=TRUE)




