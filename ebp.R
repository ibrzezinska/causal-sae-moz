# Libraries
library(tidyverse)
library(emdi)
library(survey)
library(sf)
library(haven)


# Load population and sample data
# Load population and sample data
sample_data <- readRDS("./output/sample_data_cov_std.rds")
population_data <- readRDS("./output/pop_ea_cov_std.rds")
adm2_shp <- st_read("Z:/01 Mozambique/02 Code/descriptives-flood-poverty/results/moz_admin2_shp.shp")

# Full sample and full population


# Add District identifier. First 4 digits = District
population_data$ID_DIST <- substr(population_data$BA_ID, 1, 4)

# Check identifiers
pop_dist <- unique(population_data$ID_DIST)
sample_dist <- unique(sample_data$ID_DIST)

# 41 vs 42?? not sure why 
setdiff(pop_dist, sample_dist)
setdiff(sample_dist, pop_dist)

# "0414" district ID is in population for some reason, but not sample
# Dropping it for now - to be investigated 
population_data <- subset(
  population_data,
  ID_DIST != "0414"
)

# Get sample weights for IOF 2022
iof_2022_raw <-  read_dta("Z:/01 Mozambique/01 Data/01 Poverty/04 Household data MZ/02 WB data/iof2022_v3.dta")

# Leave only hhid and sample weights - aggregate at hh level 
hh_weights <- iof_2022_raw %>%
  dplyr::select(hhid, weight_hh) %>%
  group_by(hhid) %>%
  summarise(
    weight_hh = max(weight_hh, na.rm = TRUE),
    .groups = "drop"
  )

# Remove large IOF file
rm(iof_2022_raw)

# Attach weights to the sample data
sample_data <- left_join(sample_data, hh_weights)

# Move to the front
sample_data <- sample_data %>%
  relocate(weight_hh, .before = hhid)


# Create a survey design object, accounting for household weights, strata, and clusters 
iof_design <- svydesign(ids = ~ AES,    # Enumeration Area is the cluster
                        weights = ~ weight_hh,  # household sampling weight 
                        data = sample_data)

de <- svyby(~ consumption,
            by = ~ID_DIST,
            design = iof_design,
            svymean,
            vartype=c("se"))


# Inspect the estimates
summary(de$consumption)

# Histogram to see distribution of the direct estimates
hist(de$consumption)

# Look on a map
de_geo <- left_join(adm2_shp, de)

library(ggplot2)

p <- ggplot(de_geo) +
  geom_sf(aes(fill = consumption), colour = "grey60", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "lightgrey",
    name = "Consumption"
  ) +
  theme_minimal() +
  labs(
    title = "Direct Estimate of Consumption by District, Mozambique"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p

ggsave(
  "./plots/de_consumption_map.png",
  plot = p,
  width = 10,
  height = 8
)


sample_dist <- sample_data %>% na.omit() %>% distinct(ID_DIST) %>% pull(ID_DIST) # get codes of unique postos from survey data
pop_dist <- population_data %>% na.omit() %>% distinct(ID_DIST) %>% pull(ID_DIST) # get codes of unique postos from census data


# Variables used to model poverty
form_hh <- consumption ~ mean.buildings_count_BCB_gl_100m_v1_1 +
mean.MOZ_level0_100m_2015_2030 + mean.highway_dist_osm_2023_100m_v1 +
mean.ppt_2022_yravg_tc_100m_v1 + mean.tavg_2022_tlst_100m_v1 + 
mean.viirs_nvf_2022_100m_v1

# LASSO selection
form_lasso <- consumption ~ mean.Matlas_Global_Motorized_Travel_Time_to_Healthcare_2019 + 
  mean.esalc_11_dst_2022_100m_v1 + mean.esalc_130_dst_2022_100m_v1 + mean.esalc_200_dst_2022_100m_v1 +
  mean.esalc_210_dst_2022_100m_v1 + mean.esalc_40_dst_2022_100m_v1 + mean.tavg_2022_tlst_100m_v1 +
  mean.povmap_grdi_v1 + mean.viirs_nvf_2022_100m_v1 + mean.urban_rural_catchment_areas


# Running the EBP for household data
ebp_hh <- ebp(
  fixed = form_lasso,  # model for relationship between poverty and hh variables
  pop_data = population_data %>% filter(ID_DIST %in% pop_dist), # auxiliary data - use census for out of sample HHs (explanatory variables)
  pop_domains = "ID_DIST",  # at the level of the posto
  smp_data = sample_data %>% filter(ID_DIST %in% sample_dist),  # in-sample data 
  smp_domains = "ID_DIST", # at the level of the posto
  interval = c(-2,0),
  threshold = 61.3,
  na.rm = TRUE,
  transformation = "log",   
  MSE = TRUE,   # calculate MSE estimates 
  B = 30
  #cpus = 2,
)

ebp_hh

# Create a data frame with predictions 
ebp_pov <- ebp_hh$ind
ebp_mse <- ebp_hh$MSE

# Rename variables in ebp_mse
names(ebp_mse) <- paste(names(ebp_mse), "mse", sep="_")

# Rename domain 
ebp_mse <- ebp_mse %>%
  rename("Domain" = "Domain_mse")

# Left join
pov_est_mz <- left_join(ebp_pov, ebp_mse)

# Rename domain 
pov_est_mz <- pov_est_mz %>%
  rename("ID_DIST" = "Domain")

# Join in names of postos
pov_est_mz <- left_join(adm2_shp, pov_est_mz)

# Model summary
summary(ebp_hh)
plot(ebp_hh)

# Coefficient of variation
pov_est_mz$cv_mean <- sqrt(pov_est_mz$Mean) / pov_est_mz$Mean_mse * 100
pov_est_mz <- left_join(pov_est_mz, de)

pov_est_mz <- pov_est_mz %>%
  rename(cons_de = consumption)

pov_est_mz$ebp_min_de <- pov_est_mz$Mean - pov_est_mz$cons_de
