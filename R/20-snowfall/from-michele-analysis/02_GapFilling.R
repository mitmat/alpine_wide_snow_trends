library(stringr)
library(dplyr)
library(zoo)
library(ggplot2)





##### GAP FILLING #####
### Data loading ###
meta <- read.csv("00_Data/Meta_sel.csv", header=T, sep=";")
data.v1 <- read.csv("00_Data/HNData/Data_monthly.csv", header=T, sep=",")



### Date conversion ###
data.v1 <- cbind(as.numeric(sapply(strsplit(data.v1[,2],"-"),"[",1)),
                 as.numeric(sapply(strsplit(data.v1[,2],"-"),"[",2)),
                 as.numeric(sapply(strsplit(data.v1[,2],"-"),"[",3)),
                 data.v1)[,-c(3,5)]
colnames(data.v1)[(1:2)] <- c("Year","Month")
data.v1 <- data.v1[,c(3,1:2,4)]



### New dataset ###
n <- meta$Name
y <- rep(1920:2020, each=12)
m <- rep(1:12, 101)
data.v2 <- as.data.frame(matrix(nrow=length(y), ncol=length(n)+2))
data.v2[,1:2] <- c(y,m)
colnames(data.v2) <- c("Year","Month",n)

for (i in 1:length(n)) {
  
  data.x <- subset(data.v1, data.v1$Name == n[i])
  data.v2[,i+2] <- left_join(data.v2[,c(1:2,i+2)], data.x, by=c("Year","Month"))[,5]
  
}

# write.table(data.v2, "02_GapFilling/Data_monthly_gaped.csv", sep=",", row.names=F)



### Settings ###
N <- length(meta[,1]) # stations in data.v2 and meta need to be in the same order
mat_filled <- data.v2 # matrix were the filled series will be saved
common_years <- 10 # minimum number of years with common data between test and reference series
min.weight <- 0.00005 # minimum weight for reference station selection



### Weights matrix ###
# Computing station weights based on distance and elevation difference between station pairs #
Rt <- 6371
conv <- pi/180
dist_r <- matrix(nrow=length(meta[,1]), ncol=N)
delta_z <- matrix(nrow=length(meta[,1]), ncol=N)

for (ii in 1:N) {
  angle <- sin(conv*meta$Latitude) * sin(conv*meta$Latitude[ii]) + cos(conv*meta$Latitude) * cos(conv*meta$Latitude[ii]) * cos(abs(conv*meta$Longitude-conv*meta$Longitude[ii])) 
  angle[which(angle > 1)] <- 1
  dist_r[,ii] <- Rt*acos(angle)
  delta_z[,ii] <- abs(meta$Elevation-meta$Elevation[ii])
}

# Halving coefficients for weights: 50km for distance and 250m for elevation difference #
halv_dist <- 50
halv_elev <- 250
c_dist <- (halv_dist^2)/log(2)
c_h <- (halv_elev^2)/log(2)

wd <- exp(-(dist_r^2)/c_dist)

wh <- matrix(nrow=length(meta[,1]), ncol=N)
for (t in 1:N) {
  wh[,t] <- exp(-(delta_z[,t]^2)/c_h)
}

pesi <- wd*wh
diag(pesi) <- NA 



### Gap filling process ###
for (i in 1:N) { # iter on stations
  print(paste("station",meta$Name[i],i))
  NMAX <- 10 # max number of reference stations used for the reconstruction 
  missing <- which(is.na(data.v2[,i+2]))
  
  
  if (length(missing) > 0) {
    d <- which(pesi[,i] > min.weight) # only stations with weights above the threshold are considered 
    col_ref <- length(d)
    
    if (col_ref > 2) { # stations are sorted by decreasing weights (the reconstruction is performed only if the selected stations are more than two)
      l <- sort.int(as.numeric(pesi[d,i]), decreasing=TRUE, index.return=TRUE)
      d <- d[l$ix]
      
      if (col_ref < NMAX) {NMAX <- col_ref} # if selected stations are less than NMAX, NMAX is set to the number of stations with relevant weight
      else {NMAX <- 10} 
      
      
      for (k in 1:12) { # iter on months
        mes <- which(data.v2[,2] == k) # select data for month k
        vet_test <- data.v2[mes,i+2] # extract the values of the test series
        vet_filled <- vet_test # create a duplicate of reference series for the month k where the filled gaps will be added
        gaps <- which(is.na(vet_test)) # select the years with missing data
        
        if (length(gaps) > 0) {
          mat_ref <- data.v2[mes,d+2] # matrix of the sorted reference series for the month k
          stat <- matrix(nrow=length(gaps), ncol=NMAX) # matrix were reconstruction statistics will be stored
          filled <- numeric(length(gaps)) # vector for storing only reconstructed gaps of test series
          filled[] <- NA
          
          for (k1 in 1:length(gaps)) { # iter on gaps
            NN <- 1 
            j <- 1
            
            while (NN<=NMAX && j<=col_ref) { # cycle on reference stations until NMAX is reached or until the total number of available reference stations is exploited 
              if (!is.na(mat_ref[gaps[k1],j])) { # not missing value in the reference 
                sel <- which(!is.na(vet_test) & !is.na(mat_ref[,j])) # years with common data
                
                if (length(sel) >= common_years) {
                  sum_test <- sum(vet_test[sel])
                  sum_ref <- sum(mat_ref[sel,j])
                  ratio_sum <- sum_test/sum_ref
                  
                  if (sum_ref == 0) {ratio_sum <- 1}
                  
                  stat[k1,NN] <- round((mat_ref[gaps[k1],j]*ratio_sum),4) # reconstructed value is rescaled by the ratio
                  NN <- NN+1    
                }
              }
              j <- j+1 
            }
          }
          
          for (k2 in 1:length(gaps)) {
            filled[k2] <- mean(stat[k2,],na.rm=TRUE) # filled gap as the average of the NMAX simulations - alternatively, median/weighted mean can be considered  
          } 
          
          vet_filled[gaps] <- filled  
          mat_filled[mes,i+2] <- vet_filled # store the filled series for the month k in the data matrix
          
        }
      }
    }
  }        
}        



### Matrix conversion ###
mat_filled2 <- mat_filled
for (a in 3:ncol(mat_filled2)) {
  mat_filled2[,a][which(is.na(mat_filled2[,a]))] <- NA
  if (is.numeric(mat_filled2[,a]) == T) {mat_filled2[,a] <- round(mat_filled2[,a], digits=0)}
}



### Data gain ###
n_NA1 <- colSums(is.na(data.v2[,-c(1:2)]))/length(data.v2$Year)*100
n_NA2 <- colSums(is.na(mat_filled2[,-c(1:2)]))/length(mat_filled2$Year)*100
delta_n_NA <- n_NA1 - n_NA2
gain_m_n_NA <- mean(delta_n_NA)

# write.table(mat_filled2, "02_GapFilling/Data_monthly_filled.csv", sep=",", row.names=F)





##### PLOTTING #####
### Data loading ###
meta <- read.csv("00_Data/Meta_sel.csv", header=T, sep=";")
data.gap <- read.csv("02_GapFilling/Data_monthly_gaped.csv", header=T, sep=",")
data.fill <- read.csv("02_GapFilling/Data_monthly_filled.csv", header=T, sep=",")
colnames(data.gap) <- c("Year","Month",meta$Name)
colnames(data.fill) <- c("Year","Month",meta$Name)



### Date conversion ###
Date <- as.Date(paste(data.gap$Year,data.gap$Month,1,sep="-"), format="%Y-%m-%d")
data.gap <- cbind(Date, data.gap[,-c(1:2)])
data.fill <- cbind(Date, data.fill[,-c(1:2)])



### Plotting data ###
for (i in 1:length(meta$Name)) {
  
  data.gap.x <- data.gap[,c(1,i+1)]
  data.fill.x <- data.fill[,c(1,i+1)]
  id.NA.x <- which(is.na(data.gap.x[,2]))
  
  data.plot.x <- data.frame(Date=Date, Name=meta$Name[i], HN=data.fill.x[,2], Type="Original")
  data.plot.x$Type[id.NA.x] <- "Reconstructed"
  
  png(paste0("02_GapFilling/Plots/Monthly/",meta$Name[i],".png"), width=1600, height=800, res=100)
  print(ggplot(data.plot.x, aes(x=Date, y=HN, col=Type, fill=Type)) +
          geom_col() +
          scale_colour_manual(name=element_blank(), values=c("Original"="black",
                                                             "Reconstructed"="red")) +
          scale_fill_manual(name=element_blank(), values=c("Original"="black",
                                                           "Reconstructed"="red")) +
          labs(title=paste0(meta$Name[i]," [",meta$Provider[i],"]"),
               x=element_blank(),
               y="Monthly fresh snow [cm]") +
          theme_bw() +
          theme(plot.title=element_text(size=25, hjust=0.5, face="bold", family="serif"),
                axis.text.x=element_text(size=14, family="serif"),
                axis.text.y=element_text(size=14, family="serif"),
                axis.title.x=element_text(size=17, family="serif"),
                axis.title.y=element_text(size=17, vjust=3.5, family="serif"),
                plot.margin=unit(c(1,0.5,0.5,1.5),"lines")) +
          theme(legend.text=element_text(size=12),
                legend.key.size=unit(4, "mm"),
                legend.justification=c(0.1,0.5),
                legend.position=c(0.90,0.94)))
  dev.off()
  
}





##### SEASONAL #####
### Data ###
meta <- read.csv("00_Data/Meta_sel.csv", header=T, sep=";")
data <- read.csv("02_GapFilling/Data_monthly_filled.csv", header=T, sep=",")



### Computation ###
data.s <- as.data.frame(matrix(nrow=100, ncol=length(meta$Name)+1))
colnames(data.s) <- c("Date", meta$Name)
data.s$Date <- paste(seq(min(data$Year),max(data$Year)-1),seq(min(data$Year)+1,max(data$Year)),sep="-")

for (i in 1:length(meta$Name)) {
  
  data.x <- data[,c(1:2,i+2)]
  data.x <- data.x[which(data.x$Month == 11 | data.x$Month == 12 |
                           data.x$Month == 1 | data.x$Month == 2 |
                           data.x$Month == 3 | data.x$Month == 4 |
                           data.x$Month == 5),]
  data.x <- data.x[which(data.x$Month == 11)[1]:tail(which(data.x$Month == 5),1), ]
  rownames(data.x) <- NULL
  
  data.s[,i+1] <- rollapply(data.x[,3], width=7, FUN=sum, by=7, na.rm=F)
  
}

# write.table(data.s, "02_GapFilling/Data_seasonal_filled.csv", row.names=F, sep=",")



### Plot ###
data.s.plot <- data.s
data.s.plot$Date <- as.Date(paste(substr(data.s.plot$Date,6,10), "01", "01", sep="-"))

for (i in 1:length(meta$Name)) {
  
  data.x <- data.s.plot[,c(1,i+1)]
  colnames(data.x)[2] <- "HN"
  data.x.NA <- subset(data.x, is.na(HN))
  
  if (length(data.x.NA$HN) != 0) {
    
    data.x.NA$HN <- 0.1
    
    png(paste0("02_GapFilling/Plots/Seasonal/",meta$Name[i],".png"), width=1800, height=800, res=200)
    
    print(ggplot(data.x, aes(x=Date, y=HN)) +
            geom_point(size=1.2, shape=19) +
            geom_point(data=data.x.NA, size=0.5, shape=4, col="red") +
            labs(title=paste0(meta$Name[i]," [",meta$Provider[i],"]"),
                 x=element_blank(),
                 y="Seasonal fresh snow [cm]") +
            theme_bw() +
            theme(plot.title=element_text(size=17, hjust=0.5, face="bold", family="serif"),
                  axis.text.x=element_text(size=9, family="serif"),
                  axis.text.y=element_text(size=9, family="serif"),
                  axis.title.x=element_text(size=12, family="serif"),
                  axis.title.y=element_text(size=12, vjust=3.5, family="serif"),
                  plot.margin=unit(c(1,0.5,0.5,1.5),"lines")))
    
    dev.off()
    
  } else {
    
    png(paste0("02_GapFilling/Plots/Seasonal/",meta$Name[i],".png"), width=1800, height=800, res=200)
    
    print(ggplot(data.x, aes(x=Date, y=HN)) +
            geom_point(size=1.2, shape=19) +
            labs(title=paste0(meta$Name[i]," [",meta$Provider[i],"]"),
                 x=element_blank(),
                 y="Seasonal fresh snow [cm]") +
            theme_bw() +
            theme(plot.title=element_text(size=17, hjust=0.5, face="bold", family="serif"),
                  axis.text.x=element_text(size=9, family="serif"),
                  axis.text.y=element_text(size=9, family="serif"),
                  axis.title.x=element_text(size=12, family="serif"),
                  axis.title.y=element_text(size=12, vjust=3.5, family="serif"),
                  plot.margin=unit(c(1,0.5,0.5,1.5),"lines")))
    
    dev.off()
    
  }
  
}









