rm(list=ls()) #clears any "objects from the R workspace (computers memory)

## Required packages

library(tidyverse)#
library(magrittr)
library(plyr)#
library(signal)#
library(ggplot2)#
library(ggmatplot)
library(plotly)
library(prospectr)#
library(reshape)

## Reading spectral reflectance of crop residues
#setwd("E:/Spectrometry/Results/")
# setwd("C:/Users/h.neely/OneDrive - Washington State University (email.wsu.edu)/Spectral responses soil and residue 2022/Data")
setwd("/Users/aminnorouzi/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/Raw_Data/")

## Creating a function read_csv_filename to read all the files stored in Data.in the previous step
## skip = 26 is used to skip the first 26 lines in each of the files since they store metadata for each scan
## saving the file name as one of the column

read_csv_filename <- function(filename) {
  ret <- read_delim(filename, delim = "\t", skip = 26)
  ret$Source <- filename
  ret
}

## Reading all the scan files with .sed format in the Residue folder. We need to specify the pattern because the
## folder contain other files with .raw format which is not needed to be read

Data.in.residue <- (list.files(
  path = "Spectral Reflectance/Crop Residue",
  pattern = "*.sed", recursive = TRUE, full.names = TRUE
))

Residue.raw <- ldply(Data.in.residue, read_csv_filename)
## Separating the Source column and saving the relevant information like Sample,Scan and Crop as new columns
Residue<-separate(data=Residue.raw, col=Source, into=c(NA, "Sample", "Scan", "Crop", NA, NA, NA), sep="/")
Residue$`Reflect. %` <- as.double(Residue$`Reflect. %`)
Residue$Wvl <- as.double(Residue$Wvl)
## Reading the details about relative water content for crop residues for each scan
CropMoisture <- read.csv("CropMoisture.csv")
Residue.CropMoisture <- merge(Residue, CropMoisture, by = c("Scan", "Crop"))
## Take the median of the '4' scans (Not all samples had 4 scans but most did) 
Residue.median <- Residue.CropMoisture %>%
  group_by(Wvl, Sample, Scan, Crop) %>%
  summarise_all(.funs = c("median")) 



## Smooth median scan
Crop.names<-unique(Residue.median$Crop)
Scan.names<-unique(Residue.median$Scan)
resample.vec<-seq(500, 2450, 10)
resample.vec.names<-paste0("R", resample.vec)

Residue.median.smooth<-matrix(nrow=(length(Crop.names)*length(Scan.names)), ncol=(length(resample.vec)+4))
Residue.median.dim<-dim(Residue.median)


k<-1
kk<-1
for (i in 1:length(Crop.names)) {
  Residue.bycrop.temp<-Residue.median[which(Residue.median$Crop == Crop.names[i]),]
  rwc.temp<-unique(Residue.bycrop.temp$RWC)
  for (j in 1:length(rwc.temp)) {
    Residue.byrwc.temp<-Residue.bycrop.temp[which(Residue.bycrop.temp$RWC == rwc.temp[j]),]
    kk.dim<-dim(Residue.byrwc.temp)
    output<-sgolayfilt(Residue.byrwc.temp$`Reflect. %`, p=2, n=11, m=0, ts=1)
    output<-resample(output, Residue.byrwc.temp$Wvl, resample.vec, interpol="spline")
    
    
    Residue.median.smooth[k,]<-c(unique(Residue.byrwc.temp$Sample), unique(Residue.byrwc.temp$Crop), unique(Residue.byrwc.temp$Scan), unique(Residue.byrwc.temp$RWC), output)
    
    k<-k+1
    kk<-kk+kk.dim[1]
  }
}

RMS.dim<-dim(Residue.median.smooth)
Residue.median.smooth<-na.omit(Residue.median.smooth)
Residue.median.smooth[,5:(RMS.dim[2])]<-as.double(Residue.median.smooth[,5:(RMS.dim[2])])
Residue.median.smooth<-as.data.frame(Residue.median.smooth)
colnames(Residue.median.smooth)<-(c("Sample", "Crop", "Scan", "RWC", resample.vec.names)) 
Residue.median.smooth[,resample.vec.names]<-apply(Residue.median.smooth[,resample.vec.names,drop=F], 2,
                                                  function(x) as.numeric(as.character(x)))
Residue.median.smooth<-Residue.median.smooth[-c(1, 2, 7, 9, 12, 17, 22, 25, 31, 33, 38, 42, 48),]





##  
Residue.D <- Residue.median.smooth %>%
  group_by(Crop) %>%
  arrange(desc(RWC), .by_group=TRUE) %>%
  mutate_if(is.numeric, funs(. -first(.))) %>% 
  ungroup()

Residue.D<-Residue.D[rowSums(Residue.D[,5:(RMS.dim[2]-4)])!= 0,]
Residue.D.dim<-dim(Residue.D)



# The following is the core code for EPO
# STEP1: Singular value decomposition of difference matrix 
res1.res <- svd(Residue.D[,5:(Residue.D.dim[2])])
# STEP2: Extract eigen-vectors singular value decomposition
V1.res <- res1.res$v
# STEP3: Use the first XX of the eigen-vectors to construct Q matrix
Vs1.res <- V1.res[, 1:2]
# STEP4: Use matrix multiplication to construct Q 
Q1.res <- Vs1.res %*% t(Vs1.res)
# STEP5: P1 matrix equals identity matrix minus Q matrix
Q1.dim<-dim(Q1.res)
PROJ1.res <- diag(Q1.dim[1])-Q1.res



# To plot the P1 matrix 
#imX <- seq(500, 2450, by=5)
#imY <- seq(500, 2450, by=5)
# the selection of the break points affects the view of the P matrix
# I selected these break points based on the histogram of the image
#breaks <- c(-1, -0.06, -0.04, -0.03, -0.02, -0.01, 0, 0.0033, 0.0066, 0.01, 0.015, 0.02, 0.03, 0.05, 1)

#windows(width=7, height=7)
#image(imX, imY, PROJ1.res, breaks = breaks, col=topo.colors(length(breaks)-1), axes=F, xlab="Wavelength (nm)", ylab="Wavelength (nm)")
#axis(1, at=c(500,1000,1500,2000,2450))
#axis(2, at=c(500,1000,1500,2000,2450))
#box()

X.residue<-as.matrix(Residue.median.smooth[,5:(RMS.dim[2])])
X_star.residue<-X.residue%*%PROJ1.res
Residue.XP<-cbind(Residue.median.smooth[,1:4], X_star.residue)




Residue.XP.plot<-Residue.XP %>% pivot_longer(cols=!1:4, names_to = "Wvl", values_to = "EPO_Reflect")
colnames(Residue.median.smooth)<-(c("Sample", "Crop", "Scan", "RWC", resample.vec)) 
Residue.median.smooth.plot<- Residue.median.smooth %>% pivot_longer(cols=!1:4, names_to = "Wvl", values_to = "Reflect")
Residue.median.smooth.plot$Wvl<-as.numeric(Residue.median.smooth.plot$Wvl)

Residue.plot<-cbind(Residue.median.smooth.plot, Residue.XP.plot$EPO_Reflect)
colnames(Residue.plot)<-(c("Sample", "Crop", "Scan", "RWC", "Wvl", "Reflect", "EPO_Reflect")) 


ggplot(data = Residue.plot, mapping = aes(x = Wvl, y = EPO_Reflect, color = RWC)) +
  geom_point() + facet_wrap(~Crop) #+ theme_bw() + theme_light() + theme(legend.position="none") 


ggplot(data = filter(Residue.plot, Crop == "Weathered Wheat"), mapping = aes(x = Wvl, y = EPO_Reflect, color = RWC)) + 
  geom_point() + theme_bw() + theme_light() + scale_color_brewer(palette="Blues")


write.csv(Residue.plot, file = "Residue.csv")



#------------------------------------------------------------
Data.in.soil <- (list.files(
  path = "Spectral Reflectance/Soil",
  pattern = "*.sed", recursive = TRUE, full.names = TRUE
))

Soil <- ldply(Data.in.soil, read_csv_filename)
## Separating the Source column and saving the relevant information like Sample,Scan and Crop as new columns
Soil.raw <- separate(data = Soil, col = Source, into = c(NA, "Sample", "Scan", "Soil", NA, NA, NA), sep = "/")
Soil.raw$`Reflect. %` <- as.double(Soil.raw$`Reflect. %`)
Soil.raw$Wvl <- as.double(Soil.raw$Wvl)
## Reading the details about relative water content for soil for each scan
SoilMoisture <- read.csv("SoilMoisture.csv")
Soil.SoilMoisture <- merge(Soil.raw, SoilMoisture, by = c("Scan", "Soil"))
## Calculate median
Soil.median <- Soil.SoilMoisture %>%
  group_by(Wvl, Sample, Scan, Soil) %>%
  summarise_all(.funs = c("median"))


## Smooth median scan
Soil.names<-unique(Soil.median$Soil)
Scan.names<-unique(Soil.median$Scan)
resample.vec<-seq(500, 2450, 10) #dont' need to recreate these but I kept the place holder here
resample.vec.names<-paste0("R", resample.vec)

Soil.median.smooth<-matrix(nrow=(length(Soil.names)*length(Scan.names)), ncol=(length(resample.vec)+4))
Soil.median.dim<-dim(Soil.median)


k<-1
kk<-1
for (i in 1:length(Soil.names)) {
  Soil.bysoil.temp<-Soil.median[which(Soil.median$Soil == Soil.names[i]),]
  rwc.temp<-unique(Soil.bysoil.temp$RWC)
  for (j in 1:length(rwc.temp)) {
    Soil.byrwc.temp<-Soil.bysoil.temp[which(Soil.bysoil.temp$RWC == rwc.temp[j]),]
    kk.dim<-dim(Soil.byrwc.temp)
    output<-sgolayfilt(Soil.byrwc.temp$`Reflect. %`, p=2, n=11, m=0, ts=1)
    output<-resample(output, Soil.byrwc.temp$Wvl, resample.vec, interpol="spline")
    
    #output<-(output - mean(output))/sd(output)
    Soil.median.smooth[k,]<-c(unique(Soil.byrwc.temp$Sample), unique(Soil.byrwc.temp$Soil), unique(Soil.byrwc.temp$Scan), unique(Soil.byrwc.temp$RWC), output)
    
    k<-k+1
    kk<-kk+kk.dim[1]
  }
}

SMS.dim<-dim(Soil.median.smooth)
Soil.median.smooth<-na.omit(Soil.median.smooth)
Soil.median.smooth[,5:(SMS.dim[2])]<-as.double(Soil.median.smooth[,5:(SMS.dim[2])])
Soil.median.smooth<-as.data.frame(Soil.median.smooth)
colnames(Soil.median.smooth)<-(c("Sample", "Soil", "Scan", "RWC", resample.vec.names)) 
Soil.median.smooth[,resample.vec.names]<-apply(Soil.median.smooth[,resample.vec.names,drop=F], 2,
                                                  function(x) as.numeric(as.character(x)))
Soil.median.smooth<-Soil.median.smooth[-c(7, 16, 17, 25, 38, 43, 47, 52, 56, 62, 73, 78, 82, 87, 88, 96),]



Soil.D <- Soil.median.smooth %>%
  group_by(Soil) %>%
  arrange(desc(RWC), .by_group=TRUE) %>%
  mutate_if(is.numeric, funs(. -first(.))) 
ungroup

Soil.D<-Soil.D[rowSums(Soil.D[,5:(SMS.dim[2])])!= 0,]



# The following is the core code for EPO
# STEP1: Singular value decomposition of difference matrix 
res1.soil <- svd(Soil.D[,5:(SMS.dim[2])])
# STEP2: Extract eigen-vectors singular value decomposition
V1.soil <- res1.soil$v
# STEP3: Use the first XX of the eigen-vectors to construct Q matrix
Vs1.soil <- V1.soil[, 1:2]
# STEP4: Use matrix multiplication to construct Q 
Q1.soil <- Vs1.soil %*% t(Vs1.soil)
# STEP5: P1 matrix equals identity matrix minus Q matrix
Q1.dim<-dim(Q1.soil)
PROJ1.soil <- diag(Q1.dim[1])-Q1.soil



# To plot the P1 matrix 
#imX <- seq(500, 2450, by=5) #Change the 'by' value if using a different resampling window
#imY <- seq(500, 2450, by=5)
# the selection of the break points affects the view of the P matrix
# I selected these break points based on the histogram of the image
#breaks <- c(-1, -0.06, -0.04, -0.03, -0.02, -0.01, 0, 0.0033, 0.0066, 0.01, 0.015, 0.02, 0.03, 0.05, 1)

#windows(width=7, height=7)
#image(imX, imY, PROJ1.soil, breaks = breaks, col=topo.colors(length(breaks)-1), axes=F, xlab="Wavelength (nm)", ylab="Wavelength (nm)")
#axis(1, at=c(500,1000,1500,2000,2450))
#axis(2, at=c(500,1000,1500,2000,2450))
#box()

X.soil<-as.matrix(Soil.median.smooth[,5:(SMS.dim[2])])
X_star.soil<-X.soil%*%PROJ1.soil
Soil.XP<-cbind(Soil.median.smooth[,1:4], X_star.soil)



Soil.XP.plot<-Soil.XP %>% pivot_longer(cols=!1:4, names_to = "Wvl", values_to = "EPO_Reflect")
colnames(Soil.median.smooth)<-(c("Sample", "Soil", "Scan", "RWC", resample.vec)) 
Soil.median.smooth.plot<- Soil.median.smooth %>% pivot_longer(cols=!1:4, names_to = "Wvl", values_to = "Reflect")
Soil.median.smooth.plot$Wvl<-as.numeric(Soil.median.smooth.plot$Wvl)

Soil.plot<-cbind(Soil.median.smooth.plot, Soil.XP.plot$EPO_Reflect)
colnames(Soil.plot)<-(c("Sample", "Soil", "Scan", "RWC", "Wvl", "Reflect", "EPO_Reflect")) 


ggplot(data = Soil.plot, mapping = aes(x = Wvl, y = Reflect, color = RWC)) +
  geom_point() + facet_wrap(~Soil) + theme_bw() + theme_light() + theme(legend.position="none") 


ggplot(data = filter(Soil.plot, Soil == "Mondovi 1"), mapping = aes(x = Wvl, y = EPO_Reflect, color = RWC)) + 
  geom_point() + theme_bw() + theme_light() + scale_color_brewer(palette="Spectral")

ggplot(data = filter(Soil.plot, Soil == "Mondovi 1"), mapping = aes(x = Wvl, y = Reflect, color = RWC)) + 
  geom_point() + theme_bw() + theme_light() + scale_color_brewer(palette="Spectral")



write.csv(Soil.plot, file = "Soil.csv")

