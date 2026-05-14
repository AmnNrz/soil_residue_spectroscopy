library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

path_to_data <- paste0('/home/amin-norouzi/OneDrive/Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')

mixed <- read.csv(paste0(path_to_data, 
                           "mixed_spectra_dry.csv"),
                    header = TRUE, row.names = NULL)

# mixed <- mixed %>%
#   rename(Reflect = Reflectance)

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
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)
  # mutate(R2220 = 2200 / 2000) %>%
  # mutate(R1620 = 2200 / 2000) %>%
  # mutate(RSWIR = 2200 / 2000) %>%
  # mutate(ROLI = 2200 / 2000)

mix_index$R2025_2035 <- rowMeans(select_columns_range(mix_index, '2025', '2035'))
mix_index$R2095_2105 <- rowMeans(select_columns_range(mix_index, '2095', '2105'))
mix_index$R2245_2255 <- rowMeans(select_columns_range(mix_index, '2245', '2255'))
mix_index$CAI <- (0.5 * (mix_index$R2025_2035 + mix_index$R2245_2255) - mix_index$R2095_2105)

mix_index$R2220_2260 <-  rowMeans(select_columns_range(mix_index, '2220', '2260'))
mix_index$R2310_2360 <-  rowMeans(select_columns_range(mix_index, '2310', '2360'))
mix_index$SINDRI <- 100 * (mix_index$R2220_2260 - mix_index$R2310_2360) / (mix_index$R2220_2260 + mix_index$R2310_2360)

mix_index$R1600_1680 <-  rowMeans(select_columns_range(mix_index, '1600', '1680'))
mix_index$R2160_2340 <-  rowMeans(select_columns_range(mix_index, '2160', '2340'))
mix_index$NDTI <- (mix_index$R1600_1680 - mix_index$R2160_2340) / (mix_index$R1600_1680 + mix_index$R2160_2340)

desired_column <- c("Mix", "Crop", "Soil", "Fraction", "CAI", "SINDRI", "NDTI")
mix_index <- mix_index[, desired_column]

write.csv(mix_index, file = paste0(path_to_data, "mixed_index_df.csv"), row.names = FALSE)


