## ---------------------------
##
## Script name: Read files generated from spectrometer
##
## Purpose of script:Reading spectral reflectance of crop residues and soil for different water content
##
## Author: Siddharth Chaudhary
##
## Date Created: 2022-08-26
##
## Copyright (c) Siddharth Chaudhary, 2022
## Email: siddharth.chaudhary@wsu.edu
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## Required packages

library(tidyverse)
library(magrittr)
library(plyr)
library(signal)#
library(ggplot2)#
library(ggmatplot)
library(plotly)
library(prospectr)#
library(reshape)

## Reading spectral refleactance of crop resideus

Data.in <- (list.files(
  path = paste0("/Users/aminnorouzi/Library/CloudStorage/",
                "OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/",
                "Projects/Soil_Residue_Spectroscopy/Data/Spectra_shift_check/",
                "Data/Spectral Reflectance/Crop Residue"),
  pattern = "*.sed", recursive = TRUE, full.names = TRUE
))

path_to_save = paste0("/Users/aminnorouzi/Library/CloudStorage/",
                      "OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/",
                      "Projects/Soil_Residue_Spectroscopy/Data/Spectra_shift_check/",
                      "Data/")

read_csv_filename <- function(filename) {
  ret <- read_delim(filename, delim = "\t", skip = 26)
  ret$Source <- filename
  ret
}

Residue <- ldply(Data.in, read_csv_filename)
Residue_ <- Residue
Residue_ <- separate(data = Residue_, col = Source, into = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,
                                                             "Sample", "Scan", "Crop", NA, NA), sep = "/")
Residue_$`Reflect. %` <- as.double(Residue_$`Reflect. %`)
Residue_$Wvl <- as.double(Residue_$Wvl)

## Details about RWC for each scan
CropMoisture <- read.csv(paste0(path_to_save, "CropMoisture.csv"))
CropMoisture$RWC <- ifelse(CropMoisture$RWC > 1, 1, CropMoisture$RWC)

Residue_ <- merge(Residue_, CropMoisture, by = c("Scan", "Crop"))

Residue_Median <- Residue_ %>%
  group_by(Wvl, Sample, Scan, Crop) %>%
  # mutate(SmoothRef = savgol((Residue$`Reflect. %`), 51, 2,0)) %>%
  summarise_all(.funs = c("median"))

write.csv(Residue_Median, paste0(path_to_save, "Residue.csv"), row.names = FALSE)

# # ###################################################
# # ###################################################
#                     # Smooth Media Scan
Residue.median <- Residue_Median
Crop.names<-unique(Residue.median$Crop)
Scan.names<-unique(Residue.median$Scan)
resample.vec<-seq(500, 2450, 5)
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



#
#
# ##
Residue.D <- Residue.median.smooth %>%
  group_by(Crop) %>%
  arrange(desc(RWC), .by_group=TRUE) %>%
  mutate_if(is.numeric, funs(. -first(.)))
ungroup

Residue.D<-Residue.D[rowSums(Residue.D[,5:(RMS.dim[2]-4)])!= 0,]
Residue.D.dim<-dim(Residue.D)




#
## Reading spectral reflectance of soil samples
Data.in <- (list.files(
  path = "E:/WSU/Spectrometry/Results/Soil",
  pattern = "*.sed", recursive = TRUE, full.names = TRUE
))

read_csv_filename <- function(filename) {
  ret <- read_delim(filename, delim = "\t", skip = 26)
  ret$Source <- filename
  ret
}
#
Soil <- ldply(Data.in, read_csv_filename)
Soil <- separate(data = Soil, col = Source, into = c(NA, NA, NA, NA, "Sample", "Scan", "Crop", NA, NA), sep = "/")
Soil$`Reflect. %` <- as.double(Soil$`Reflect. %`)
Soil$Wvl <- as.double(Soil$Wvl)

## Details about RWC for each scan
SoilMoisture <- read.csv("E:/WSU/Spectrometry/SoilMoisture.csv")
Soil <- merge(Soil, SoilMoisture, by = c("Scan", "Crop"))

Soil_Median <- Soil %>%
  group_by(Wvl, Sample, Scan, Crop) %>%
  # mutate(SmoothRef = savgol((Soil$`Reflect. %`), 51, 2,0)) %>%
  summarise_all(.funs = c("median"))

write.csv(Soil_Median, "Soil_08_18.csv", row.names = FALSE)
