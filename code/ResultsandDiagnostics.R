#Model results and diagnostics
#Zoe Rand
#Last updated: 9/29/25
library(tidyverse)
library(sdmTMB)
library(sdmTMBextra)
library(sf)

#NOTE: Add in leave one out cross validation for model checking
# Read in Data ------------------------------------------------------------
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


# Read in model Results ---------------------------------------------------

m1_SRKW<-readRDS("results/Model1_SRKW_fit.RDS")
m1_T<-readRDS("results/Model1_T_fit.RDS")
K<-9
#without hydrophone data
m3_SRKW<-readRDS("results/Model3_SRKW_fit_noH.RDS")
m3_T<-readRDS("results/Model3_T_fit_noH.RDS")

# Diagnostics -------------------------------------------------------------
## SRKW ----
m1_SRKW
sanity(m1_SRKW)


#checking residuals
resids<-residuals(m1_SRKW, type = "mle-mvn")
qqnorm(resids)
qqline(resids)

#plotting spatial residuals

SRKW_dat$resids<-resids
ggplot(SRKW_dat, aes(X, Y, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~Year, ncol = 14) +
  coord_fixed()

#residuals over time
ggplot(SRKW_dat, aes(Year, resids)) +
  geom_point() +
  geom_smooth()

## Transients ----
m1_T
sanity(m1_T)

#residuals
resids<-residuals(m1_T, type = "mle-mvn")
qqnorm(resids)

#plotting spatial residuals

T_dat$resids<-resids
ggplot(T_dat, aes(X, Y, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~Year, ncol = 14) +
  coord_fixed()

#residuals over time
ggplot(T_dat, aes(Year, resids)) +
  geom_point() +
  geom_smooth()


# Model Result Tables -----------------------------------------------------
m1_results_SRKW_fixed<-tidy(m1_SRKW)
m1_results_SRKW_random<-tidy(m1_SRKW, effects = "ran_pars")
m1_results_SRKW<-bind_rows(m1_results_SRKW_fixed, m1_results_SRKW_random)
write_csv(m1_results_SRKW, "results/model_result_table_SRKW.csv")

m1_results_T_fixed<-tidy(m1_T)
m1_results_T_random<-tidy(m1_T, effects = "ran_pars")
m1_results_T<-bind_rows(m1_results_T_fixed, m1_results_T_random)
write_csv(m1_results_T, "results/model_result_table_T.csv")
# Predictions by quad with uncertainty ------------------------------------

## SRKW ----
#predictions for each quadrant
quads_dat<-quads %>% st_as_sf(coords=c('Long','Lat'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%   
  as.data.frame()

predict_quad<-expand_grid(
  Quad = quads_dat$Quad, 
  Year = unique(SRKW_dat$Year), 
  Month = seq(1,12, by = 1))
predict_quad$fyear<-as.factor(predict_quad$Year)
predict_quad<-predict_quad %>% left_join(quads_dat, by = "Quad") %>% as.data.frame()
sims_SRKW_quads<-predict(m1_SRKW, newdata = predict_quad, nsim = 1000)

head(sims_SRKW_quads)
colnames(sims_SRKW_quads)<-paste0("P", seq(1, 1000, by =1))

preds_quads<-bind_cols(predict_quad, sims_SRKW_quads) %>% select(-c(FishArea, UTMx, UTMy, geometry))
head(preds_quads)

#save 
saveRDS(preds_quads, "results/SRKW_quads_preds.RDS")

## Transients ----
quads_dat<-quads %>% st_as_sf(coords=c('Long','Lat'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%   
  as.data.frame()

predict_quad<-expand_grid(
  Quad = quads_dat$Quad, 
  Year = unique(T_dat$Year), 
  Month = seq(1,12, by = 1))
predict_quad$fyear<-as.factor(predict_quad$Year)
predict_quad<-predict_quad %>% left_join(quads_dat, by = "Quad") %>% as.data.frame()
sims_T_quads<-predict(m1_T, newdata = predict_quad, nsim = 1000)

head(sims_T_quads)
colnames(sims_T_quads)<-paste0("P", seq(1, 1000, by =1))

preds_quads<-bind_cols(predict_quad, sims_T_quads) %>% select(-c(FishArea, UTMx, UTMy, geometry))
head(preds_quads)
#save simulations
saveRDS(preds_quads, "results/T_quads_preds.RDS")

# Predictions continuous spatial grid---------------------
## SRKW ----
#full prediction grid
predict_full_grid<-expand_grid(
  X = seq(min(SRKW_dat$X), max(SRKW_dat$X), length = 50), 
  Y = seq(min(SRKW_dat$Y), max(SRKW_dat$Y), length = 50), 
  Year = unique(SRKW_dat$Year), 
  Month = seq(1,12, by = 1))

predict_full_grid$fyear<-as.factor(predict_full_grid$Year)
#NOTE takes a while to run
#need return_tmb_object = TRUE to run COG and stuff later
preds_SRKW<-predict(m1_SRKW, newdata = predict_full_grid, return_tmb_object = FALSE)
#sims_SRKW<-predict(m1_SRKW, newdata = predict_full_grid, nsim = 1000)

#colnames(sims_SRKW)<-paste0("P", seq(1, 1000, by =1))

#preds_SRKW_2<-bind_cols(preds_SRKW, sims_SRKW) #%>% select(-c(FishArea, UTMx, UTMy, geometry))
#head(preds_SRKW_2)

saveRDS(preds_SRKW, "results/SRKW_preds_fullgrid_unc.RDS")

## Transients ----
predict_full_grid<-expand_grid(
  X = seq(min(T_dat$X), max(T_dat$X), length = 50), 
  Y = seq(min(T_dat$Y), max(T_dat$Y), length = 50), 
  Year = unique(T_dat$Year), 
  Month = seq(1,12, by = 1))

predict_full_grid$fyear<-as.factor(predict_full_grid$Year)

preds_T<-predict(m1_T, newdata = predict_full_grid, return_tmb_object = FALSE)
#sims_T<-predict(m1_T, newdata = predict_full_grid, nsim = 1000)
#colnames(sims_T)<-paste0("P", seq(1, 1000, by =1))
#preds_T_2<-bind_cols(preds_T, sims_T)

saveRDS(preds_T, "results/T_preds_fullgrid_unc.RDS")


# Predictions without hydrophone data -------------------------------------

## SRKW ----
#predictions for each quadrant
quads_dat<-quads %>% st_as_sf(coords=c('Long','Lat'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%   
  as.data.frame()

predict_quad<-expand_grid(
  Quad = quads_dat$Quad, 
  Year = unique(SRKW_dat$Year), 
  Month = seq(1,12, by = 1))
predict_quad$fyear<-as.factor(predict_quad$Year)
predict_quad<-predict_quad %>% left_join(quads_dat, by = "Quad") %>% as.data.frame()
sims_SRKW_quads<-predict(m3_SRKW, newdata = predict_quad, nsim = 1000)

head(sims_SRKW_quads)
colnames(sims_SRKW_quads)<-paste0("P", seq(1, 1000, by =1))

preds_quads<-bind_cols(predict_quad, sims_SRKW_quads) %>% select(-c(FishArea, UTMx, UTMy, geometry))
head(preds_quads)

#save 
saveRDS(preds_quads, "results/SRKW_quads_preds_noH.RDS")

## Transients ----
quads_dat<-quads %>% st_as_sf(coords=c('Long','Lat'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%   
  as.data.frame()

predict_quad<-expand_grid(
  Quad = quads_dat$Quad, 
  Year = unique(T_dat$Year), 
  Month = seq(1,12, by = 1))
predict_quad$fyear<-as.factor(predict_quad$Year)
predict_quad<-predict_quad %>% left_join(quads_dat, by = "Quad") %>% as.data.frame()
sims_T_quads<-predict(m3_T, newdata = predict_quad, nsim = 1000)

head(sims_T_quads)
colnames(sims_T_quads)<-paste0("P", seq(1, 1000, by =1))

preds_quads<-bind_cols(predict_quad, sims_T_quads) %>% select(-c(FishArea, UTMx, UTMy, geometry))
head(preds_quads)
#save simulations
saveRDS(preds_quads, "results/T_quads_preds_noH.RDS")

## Comparison ----
m3_SRKW_results_fixed<-tidy(m3_SRKW)
m3_SRKW_results_random<-tidy(m3_SRKW, effects = "ran_pars")
m3_SRKW_results<-bind_rows(m3_SRKW_results_fixed, m3_SRKW_results_random)
m3_SRKW_results$with_H_estimate<-m1_results_SRKW$estimate
m3_SRKW_results<-m3_SRKW_results %>% mutate(percdiff = abs(with_H_estimate-estimate)/(abs(with_H_estimate + estimate)/2) * 100)
write_csv(m3_SRKW_results, "results/SRKW_noH_results_table.csv")

m3_T_results_fixed<-tidy(m3_T)
m3_T_results_random<-tidy(m3_T, effects = "ran_pars")
m3_T_results<-bind_rows(m3_T_results_fixed, m3_T_results_random)
m3_T_results$with_H_estimate<-m1_results_T$estimate
m3_T_results<-m3_T_results %>% mutate(percdiff = abs(with_H_estimate-estimate)/(abs(with_H_estimate + estimate)/2) * 100)
write_csv(m3_T_results, "results/T_noH_results_table.csv")

# Monthly and Yearly Patterns by region -----------------------------------

## SRKW ----
#Puget sound monthly
mths<-seq(1, 12, by = 1)
mths_labs<-month.abb[mths]
names(mths_labs)<-as.character(mths)


SRKW_PS_presence<-preds_quads %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 364) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/82) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

SRKW_PS_plot<-ggplot(SRKW_PS_presence) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  facet_wrap(~Month, labeller = as_labeller(mths_labs)) + geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("SRKW Puget Sound") + theme_minimal()
SRKW_PS_plot

#plotting central salish sea monthly
SRKW_SS_presence<-preds_quads %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad < 364) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/365) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

SR_SS_plot<-ggplot(SRKW_SS_presence) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  facet_wrap(~Month, labeller = as_labeller(mths_labs)) + geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("SRKW central Salish Sea") + theme_minimal()
SR_SS_plot

#Annual
#average over all simulations by year
#total
SRKW_all_presence_year<-SRKW_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

#plot
p1<-ggplot(SRKW_all_presence_year) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("SRKW all") + theme_minimal()

#just PS
SRKW_PS_presence_year<-SRKW_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 364) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(82*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

p2<-ggplot(SRKW_PS_presence_year) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("SRKW Puget Sound") + theme_minimal()

## Transients ----
#Puget sound monthly
mths<-seq(1, 12, by = 1)
mths_labs<-month.abb[mths]
names(mths_labs)<-as.character(mths)


T_PS_presence<-preds_quads %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 364) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/82) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

T_PS_plot<-ggplot(T_PS_presence) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  facet_wrap(~Month, labeller = as_labeller(mths_labs)) + geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("Transient Puget Sound") + theme_minimal()
T_PS_plot

#Central salish sea monthly
T_SS_presence<-preds_quads %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad < 364) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/365) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

T_SS_plot<-ggplot(T_SS_presence) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  facet_wrap(~Month, labeller = as_labeller(mths_labs)) + geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("Transient central Salish Sea") + theme_minimal()
T_SS_plot

#Annual
T_all_presence_year<-T_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

T_PS_presence_year<-T_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 364) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(82*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

#plot

p3<-ggplot(T_PS_presence_year) + geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("Transient Puget Sound") + theme_minimal()

p4<-ggplot(T_all_presence_year) + 
  geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  geom_line(aes(x = Year, y = med)) + 
  geom_point(aes(x = Year, y = med)) + 
  geom_hline(aes(yintercept = 0.5), color = "red") + 
  labs(y = "Average probability of presence") + ggtitle("Transient all") + theme_minimal()


# Cross Validation --------------------------------------------------------
## SRKW ----
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

SRKW_dat$fyear<-as.factor(SRKW_dat$Year)
#fit with cross validation
SRKW_fit_cv<-sdmTMB_cv(data = SRKW_dat, 
                       mesh = barrier_mesh_SRKW, 
                       family = binomial(link = "logit"), 
                       formula = Presence ~ -1 + fyear + s(Month, bs = "cc", k = K) + #global smooth
                         s(Month, fyear, bs = "fs", k = K), #annual smooth
                       spatial = "on", 
                       time = "Year", 
                       spatiotemporal = "IID", 
                       k_folds = 3)

head(SRKW_fit_cv$data$cv_predicted)

pred<-predict(m1_SRKW)
pred$p<-plogis(pred$est)
pred$pred_01<-ifelse(pred$p < 0.5, 0, 1)

tss_func<-function(pred_01, presence){
  conmat <- table(pred_01, presence)
  true_neg <- conmat[1, 1]
  false_neg <- conmat[1, 2]
  false_pos <- conmat[2, 1]
  true_pos <- conmat[2, 2]
  
  # Calculate TSS:
  true_pos_rate <- true_pos / (true_pos + false_neg)
  true_neg_rate <- true_neg / (true_neg + false_pos)
  TSS <- true_pos_rate + true_neg_rate - 1
  return(TSS)
}

tss_func(pred$pred_01, pred$Presence)

#using the different folds
fold1<-tss_func(ifelse(SRKW_fit_cv$data$cv_predicted[SRKW_fit_cv$data$cv_fold == 1] <0.5, 0, 1), SRKW_fit_cv$data$Presence[SRKW_fit_cv$data$cv_fold == 1])
fold2<-tss_func(ifelse(SRKW_fit_cv$data$cv_predicted[SRKW_fit_cv$data$cv_fold == 2] <0.5, 0, 1), SRKW_fit_cv$data$Presence[SRKW_fit_cv$data$cv_fold == 2])
fold3<-tss_func(ifelse(SRKW_fit_cv$data$cv_predicted[SRKW_fit_cv$data$cv_fold == 3] <0.5, 0, 1), SRKW_fit_cv$data$Presence[SRKW_fit_cv$data$cv_fold == 3])

fold1
fold2
fold3

(fold1+fold2+fold3)/3

## Transient ----
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

#models for transients
T_dat$fyear<-as.factor(T_dat$Year)
K<-9 #smooth parameter
T_fit_cv<-sdmTMB_cv(data = T_dat, 
             mesh = barrier_mesh_T, 
             family = binomial(link = "logit"), 
             formula = Presence ~ -1 + fyear + s(Month, bs = "cc", k = K) + #global smooth
               s(Month, fyear, bs = "fs", k = K), #annual smooth
             spatial = "on", 
             time = "Year", 
             spatiotemporal = "IID", 
             k_folds = 3) 

pred_T<-predict(m1_T)
pred_T$p<-plogis(pred_T$est)
pred_T$pred_01<-ifelse(pred_T$p < 0.5, 0, 1)

tss_func(pred_T$pred_01, pred_T$Presence)

#using the different folds
fold1<-tss_func(ifelse(T_fit_cv$data$cv_predicted[T_fit_cv$data$cv_fold == 1] <0.5, 0, 1), T_fit_cv$data$Presence[T_fit_cv$data$cv_fold == 1])
fold2<-tss_func(ifelse(T_fit_cv$data$cv_predicted[T_fit_cv$data$cv_fold == 2] <0.5, 0, 1), T_fit_cv$data$Presence[T_fit_cv$data$cv_fold == 2])
fold3<-tss_func(ifelse(T_fit_cv$data$cv_predicted[T_fit_cv$data$cv_fold == 3] <0.5, 0, 1), T_fit_cv$data$Presence[T_fit_cv$data$cv_fold == 3])

fold1
fold2
fold3

(fold1+fold2+fold3) / 3
