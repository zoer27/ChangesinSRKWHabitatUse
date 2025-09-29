#SDMTMB for presence/absence with both transients and residents
#other marine mammal sightings used as absences
#Zoe Rand
#Last Updated: 9/29/25

library(tidyverse)
library(sdmTMB)
# install.packages("remotes")
#remotes::install_github("pbs-assess/sdmTMBextra", dependencies = TRUE)
library(sdmTMBextra)
library(sf)

#read in data
KW_pa_dat<-read_csv("data/KW_Presence_Absence_TG.csv")

#read in puget sound shape file for plotting
ps_raw<-read_sf("Data/Puget Sound Detailed/pugetsounddetailed.shp")
#EPSG 5069
#transforming to equal area km 
ps<-st_transform(ps_raw, crs = "+proj=utm +zone=10 +datum=WGS84 +units=km")
ps<-ps[, c(1:8, 10:14)] #removing duplicate column
ggplot(ps) +
  geom_sf(fill = "#69b3a2", color = "white")

#quadrant key
quads<-read_csv("data/QuadChart.csv")
#filter year so it's only 1978 and on (after there was dedicated effort to track KW)
#from Ettinger paper
#ending model in 2022
KW_pa_dat<-KW_pa_dat %>% filter(Year >= 1978) %>% filter(Year <= 2022)

#convert lat and long into x and y
#remove ones without lat and long
KW_pa_dat<-KW_pa_dat %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(Longitude))

KW_pa_dat <- KW_pa_dat %>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%   
  as.data.frame()

#KW_pa_dat$Year<-as.factor(KW_pa_dat$Year)

#separating ecotypes for now
SRKW_dat<-KW_pa_dat %>% filter(Pod == "SRKW")
T_dat<- KW_pa_dat %>% filter(Pod == "T")

# SRKW model --------------------------------------------------------------

#make mesh
mesh_SRKW<-make_mesh(SRKW_dat, c("X", "Y"), cutoff = 8)
mesh_SRKW$mesh$n #number of veritces
plot(mesh_SRKW)

#adding in Puget sound barrier
barrier_mesh_SRKW<-add_barrier_mesh(
  spde_obj = mesh_SRKW,
  barrier_sf = ps,
  range_fraction = 0.1,
  plot = FALSE
)
barrier_mesh_SRKW$mesh$n #number of vertices

#plotting barrier mesh
mesh_df_water <- barrier_mesh_SRKW$mesh_sf[barrier_mesh_SRKW$normal_triangles, ]
mesh_df_land <- barrier_mesh_SRKW$mesh_sf[barrier_mesh_SRKW$barrier_triangles, ]
ggplot(ps) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 2, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 2, colour = "darkgreen")

#fit model
#cyclic smoother (bs = "cc", K ) for seasonal changes
#Factor smoother over year so seasonal changes through time
#this gives you the global seasonal effect as well as yearly deviations 
#making year explicitly a factor to help with model stability


SRKW_dat$fyear<-as.factor(SRKW_dat$Year)

K<-9 #smooth parameter, making slightly less than 12 because helps with model fit
m1_SRKW<-sdmTMB(data = SRKW_dat, 
                mesh = barrier_mesh_SRKW, 
                family = binomial(link = "logit"), 
                formula = Presence ~ -1 + fyear + s(Month, bs = "cc", k = K) + #global smooth
                  s(Month, fyear, bs = "fs", k = K), #annual smooth
                spatial = "on", 
                time = "Year", 
                spatiotemporal = "IID") #spatial fields are not autocorrelated through time


saveRDS(m1_SRKW, "results/Model1_SRKW_fit.RDS")


# Transients --------------------------------------------------------------

#make mesh
mesh_T<-make_mesh(T_dat, c("X", "Y"), cutoff = 8)
mesh_T$mesh$n #number of veritces
plot(mesh_T)

#adding in Puget sound barrier
barrier_mesh_T<-add_barrier_mesh(
  spde_obj = mesh_T,
  barrier_sf = ps,
  range_fraction = 0.1,
  plot = FALSE
)
barrier_mesh_T$mesh$n #number of vertices

#plotting barrier mesh
mesh_df_water <- barrier_mesh_T$mesh_sf[barrier_mesh_T$normal_triangles, ]
mesh_df_land <- barrier_mesh_T$mesh_sf[barrier_mesh_T$barrier_triangles, ]
ggplot(ps) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 2, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 2, colour = "darkgreen")


#models for transients
T_dat$fyear<-as.factor(T_dat$Year)
K<-9 #smooth parameter
m1_T<-sdmTMB(data = T_dat, 
             mesh = barrier_mesh_T, 
             family = binomial(link = "logit"), 
             formula = Presence ~ -1 + fyear + s(Month, bs = "cc", k = K) + #global smooth
               s(Month, fyear, bs = "fs", k = K), #annual smooth
             spatial = "on", 
             time = "Year", 
             spatiotemporal = "IID") #spatial fields are not autocorrelated through time


saveRDS(m1_T, "results/Model1_T_fit.RDS")


# SRKW Pod Specific -------------------------------------------------------
#Just for SRKW
#Absences are from other SRKW sightings of other pods
#If mixed pod included in all models

## J Pod ----


# Models without Hydrophone data ------------------------------------------
KW_dat_noH<-read_csv("data/KW_Presence_Absence_TG_nohydrophone.csv")
KW_pa_dat_noH<-KW_dat_noH %>% filter(Year >= 1978) %>% filter(Year <= 2022)

#convert lat and long into x and y
#remove ones without lat and long
KW_pa_dat_noH<-KW_pa_dat_noH %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(Longitude))

KW_pa_dat_noH <- KW_pa_dat_noH %>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%   
  as.data.frame()

SRKW_dat_noH<-KW_pa_dat_noH %>% filter(Pod == "SRKW")
T_dat_noH<- KW_pa_dat_noH %>% filter(Pod == "T")

## SRKW ----
#make mesh
mesh_SRKW_noH<-make_mesh(SRKW_dat_noH, c("X", "Y"), cutoff = 8)
mesh_SRKW_noH$mesh$n #number of veritces
plot(mesh_SRKW_noH)

#adding in Puget sound barrier
barrier_mesh_SRKW_noH<-add_barrier_mesh(
  spde_obj = mesh_SRKW_noH,
  barrier_sf = ps,
  range_fraction = 0.1,
  plot = FALSE
)
barrier_mesh_SRKW_noH$mesh$n #number of vertices

SRKW_dat_noH$fyear<-as.factor(SRKW_dat_noH$Year)

K<-9 #smooth parameter, making slightly less than 12 because helps with model fit
m3_SRKW<-sdmTMB(data = SRKW_dat_noH, 
                mesh = barrier_mesh_SRKW_noH, 
                family = binomial(link = "logit"), 
                formula = Presence ~ -1 + fyear + s(Month, bs = "cc", k = K) + #global smooth
                  s(Month, fyear, bs = "fs", k = K), #annual smooth
                spatial = "on", 
                time = "Year", 
                spatiotemporal = "IID") #spatial fields are not autocorrelated through time


saveRDS(m3_SRKW, "results/Model3_SRKW_fit_noH.RDS")

## Transients ----
#make mesh
mesh_T_noH<-make_mesh(T_dat_noH, c("X", "Y"), cutoff = 8)
mesh_T_noH$mesh$n #number of veritces
plot(mesh_T_noH)

#adding in Puget sound barrier
barrier_mesh_T_noH<-add_barrier_mesh(
  spde_obj = mesh_T_noH,
  barrier_sf = ps,
  range_fraction = 0.1,
  plot = FALSE
)
barrier_mesh_T_noH$mesh$n #number of vertices


#models for transients
T_dat_noH$fyear<-as.factor(T_dat_noH$Year)
K<-9 #smooth parameter
m3_T<-sdmTMB(data = T_dat_noH, 
             mesh = barrier_mesh_T_noH, 
             family = binomial(link = "logit"), 
             formula = Presence ~ -1 + fyear + s(Month, bs = "cc", k = K) + #global smooth
               s(Month, fyear, bs = "fs", k = K), #annual smooth
             spatial = "on", 
             time = "Year", 
             spatiotemporal = "IID") #spatial fields are not autocorrelated through time


saveRDS(m3_T, "results/Model3_T_fit_noH.RDS")