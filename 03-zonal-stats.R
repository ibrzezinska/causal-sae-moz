# Prepare population data for causal SAE
# Zonal statistics at EA level for all census EAs (full population)
# Drop flow accumulation


# Libraries
library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyverse)

# Load population-level EA shapefile (for analysis districts)
#pop_ea <- st_read("./output/population_eas.shp")
#sample_data <- readRDS("./output/data_eligible.rds")
flood_ana <- rast("./data/flood_ana.tif")
flood_dumako <- rast("./data/flood_dumako.tif")
flood_gombe <- rast("./data/flood_gombe.tif")
cov_dir <- "./data/Geodata_processed" 
aes_census_2017 <- st_read("Z:/01 Mozambique/02 Code/moz-hh-impact-study/Data/Output/aes_2017/aes_2017.shp")


# List all WorldPop covariates
tif_files <- list.files(
  cov_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = F
)

# Stack all geodata
cov_stack <- rast(tif_files)
names(cov_stack) <- tools::file_path_sans_ext(basename(tif_files))

name_lookup <- c(
  "Matlas_Global_Motorized_Travel_Time_to_Healthcare_2019" = "travel_time_health_2019",
  "moz_buildings_count_BCB_gl_100m_v1_1" = "build_count_bcb_2022",
  "moz_built_S_GHS_U_wFGW_100m_v1_2022" = "built_area_ghs_2022",
  "moz_esalc_11_dst_2022_100m_v1" = "dist_cult_area_2022",
  "moz_esalc_130_dst_2022_100m_v1" = "dist_shrub_2022",
  "moz_esalc_140_dst_2022_100m_v1" = "dist_herbaceous_2022",
  "moz_esalc_150_dst_2022_100m_v1" = "dist_sparse_veg_2022",
  "moz_esalc_160_dst_2022_100m_v1" = "dist_aqua_veg_2022",
  "moz_esalc_190_dst_2022_100m_v1" = "dist_art_surface_2022",
  "moz_esalc_200_dst_2022_100m_v1" = "dist_bare_area_2022",
  "moz_esalc_210_dst_2022_100m_v1" = "dist_wetlands",
  "moz_esalc_40_dst_2022_100m_v1" = "dist_water_snow_ice_2022",
  "moz_highway_dist_osm_2023_100m_v1" = "dist_highway_2022",
  "moz_pop_2022_CN_100m_R2025A_v1" = "population_2022",
  "moz_ppt_2022_yravg_tc_100m_v1" = "precip_2022",
  "moz_tavg_2022_tlst_100m_v1" = "tavg_2022",
  "moz_viirs_nvf_2022_100m_v1" = "nightlights_2022",
  "urban_rural_catchment_areas" = "urban_rural_fao"
)

names(cov_stack) <- unname(name_lookup[names(cov_stack)])

names(cov_stack)

# Check CRS
crs(cov_stack)
st_crs(aes_census_2017)

# Zonal statistics - mean values of WorldPop covariates
# Define statistic for each layer
# Population layer
pop_layer <- cov_stack[["population_2022"]]

# All other layers
cov_stack_nopop <- cov_stack[[names(cov_stack) != "population_2022"]]

# Mean for all non-population covariates
zonal_means <- exact_extract(
  cov_stack_nopop,
  aes_census_2017,
  "mean",
  progress = TRUE
)

# Sum for population
zonal_pop <- exact_extract(
  pop_layer,
  aes_census_2017,
  "sum",
  progress = TRUE
)

# Combine
zonal_stats <- bind_cols(
  aes_census_2017 %>%
    st_drop_geometry() %>%
    dplyr::select(AES),
  zonal_means,
  population_2022 = zonal_pop[[1]]
)

# Name according to EA level
names(zonal_stats)[-1] <- paste0(names(zonal_stats)[-1], "_aes")

# Create a 5km buffer around EAs 
pop_ea_5km <- aes_census_2017 %>%
  st_transform(32736) %>%  # reproject to meters before calculating buffers
  st_buffer(5000) %>%
  st_transform(st_crs(aes_census_2017))

# List flood rasters
flood_rasters <- list(
  ana = flood_ana,
  dumako = flood_dumako,
  gombe = flood_gombe
)

# Calculate zonal max for each raster
zonal_max <- map_dfc(
  flood_rasters,
  exact_extract,
  y = pop_ea_5km, # 5km buffer
  fun = "max"
)

# Recode NA as 0s (means no flood)
zonal_max[is.na(zonal_max)] <- 0

# Check result
summary(zonal_max)

# Add AES identifier 
zonal_max <- bind_cols(
  pop_ea %>% st_drop_geometry() %>% select(AES),
  zonal_max
) 

# Rename so they match sample data
zonal_max <- zonal_max %>%
  rename(
    flood_ana_5km = ana,
    flood_dumako_5km = dumako,
    flood_gombe_5km = gombe
  )

# Left join zonal stats to the households
pop_ea_cov <- pop_ea %>%
  left_join(zonal_means) %>%
  left_join(zonal_sum) %>%
  left_join(zonal_max) #%>%
  #left_join(zonal_fa)

# Create binary for being affected by at least one flood 
pop_ea_cov <- pop_ea_cov %>%
  mutate(
    flood_any_5km_no_eloise = if_else(
      flood_ana_5km + flood_dumako_5km + flood_gombe_5km > 0,
      1,
      0
    )
  )


### Sample data ###

# Left join zonal stats to the households
sample_data_cov <- sample_data %>%
  select(-flow_acc) %>%
  left_join(zonal_means) %>%
  left_join(zonal_sum)# %>%
  #left_join(zonal_fa)


# Save
saveRDS(
  sample_data_cov,
  file = "./output/sample_data_cov_std.rds"
)

saveRDS(
  pop_ea_cov,
  file = "./output/pop_ea_cov_std.rds"
)

saveRDS(
  pop_ea_5km,
  file= "./output/pop_ea_5km.rds"
)