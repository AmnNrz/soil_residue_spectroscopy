library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')


mixed <- read.csv(paste0(path_to_data, 
                           "mixed_spectra_dry.csv"),
                    header = TRUE, row.names = NULL)


mixed <- mixed[mixed$Wvl >= 1500, ]


select_columns_range <- function(df, start_col_name, end_col_name) {
  start_col <- which(names(df) == start_col_name)
  end_col <- which(names(df) == end_col_name)
  if (start_col == 0 || end_col == 0) {
    stop("One of the specified column names does not exist in the dataframe.")
  }
  selected_df <- df[, start_col:end_col]
  return(selected_df)
}

# mixed <- mixed %>%
#   dplyr::filter(Wvl >= 1660 | Wvl <= 2330)
# 


mix_index <- mixed %>%
  spread(Wvl, Reflect) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000) %>%
  mutate(R2220 = 2200 / 2000) %>%
  mutate(R1620 = 2200 / 2000) %>%
  mutate(RSWIR = 2200 / 2000) %>%
  mutate(ROLI = 2200 / 2000)


#CAI
mix_index$CAI <- (0.5 * (mix_index$`2030` + mix_index$`2210`) - mix_index$`2100`)

# SINDRI
mix_index$R2220_2260 <-  rowMeans(select_columns_range(mix_index, '2180', '2230'))
mix_index$R2260_2280 <-  rowMeans(select_columns_range(mix_index, '2230', '2290'))
mix_index$SINDRI <- (mix_index$R2220_2260 - mix_index$R2260_2280) / (mix_index$R2220_2260 + mix_index$R2260_2280)

# NDTI
mix_index$R1660_1690 <-  rowMeans(select_columns_range(mix_index, '1570', '1650'))
mix_index$R2220_2280 <-  rowMeans(select_columns_range(mix_index, '2110', '2290'))
mix_index$NDTI <- (mix_index$R1660_1690 - mix_index$R2220_2280) / (mix_index$R1660_1690 + mix_index$R2220_2280)

# Others
mix_index$R2220 <- mix_index$`2250`/mix_index$`2000`
mix_index$R1620 <- mix_index$`1600`/mix_index$`2000`
mix_index$RSWIR <- mix_index$`1660`/mix_index$`R2260_2280`
mix_index$ROLI <- mix_index$`1660`/mix_index$R2220_2280

desired_column <- c("Mix", "Crop", "Soil", "Fraction", "CAI", "SINDRI", "NDTI")
mix_index <- mix_index[, desired_column]

write.csv(mix_index, file = paste0(path_to_data, "mixed_index_df.csv"), row.names = FALSE)


