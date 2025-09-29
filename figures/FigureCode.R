
# Figures for Manuscript 
library(tidyverse)
library(sf)
library(patchwork)
library(rnaturalearth)
library(ggh4x) #for different colored facet labels

# Read in Data ------------------------------------------------------------
#read in data
KW_pa_dat<-read_csv("data/KW_Presence_Absence_TG.csv")

#read in puget sound shape file for plotting
ps_raw<-read_sf("Data/Puget Sound Detailed/pugetsounddetailed.shp")
#EPSG 5069
#transforming to equal area km 
ps<-st_transform(ps_raw, crs = "+proj=utm +zone=10 +datum=WGS84 +units=km")
ps<-ps[, c(1:8, 10:14)] #removing duplicate column
#new shapefile
#ps<-read_sf("data/SalishSeaShoreline/SalishSeaShoreline_polygons_UTM10N.shp") %>% 
  #filter(LandWater == "L") 
ggplot(ps) +
  geom_sf(fill = "#69b3a2", color = "white") #%>% 
  #coord_sf(xlim = c(400000, 560000), ylim = c(5000000, 5200000))



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
m1_J<-readRDS("results/Model1_J_fit.RDS")
m1_K<-readRDS("results/Model1_K_fit.RDS")
m1_L<-readRDS("results/Model1_L_fit.RDS")

#get predictions for each quadrant with uncertainty
T_quads_preds<-readRDS("results/T_quads_preds.RDS")
SRKW_quads_preds<-readRDS("results/SRKW_quads_preds.RDS")
J_quads_preds<-readRDS("results/J_quads_preds.RDS")
K_quads_preds<-readRDS("results/K_quads_preds.RDS")
L_quads_preds<-readRDS("results/L_quads_preds.RDS")
T_quads_fullgrid<-readRDS("results/T_preds_fullgrid_unc.RDS")
SRKW_quads_fullgrid<-readRDS("results/SRKW_preds_fullgrid_unc.RDS")

#models without hydrophones
m3_SRKW_noH<-readRDS("results/Model3_SRKW_fit_noH.RDS")
m3_T_noH<-readRDS("results/Model3_T_fit_noH.RDS")
SRKW_preds_noH<-readRDS("results/SRKW_quads_preds_noH.RDS")
T_preds_noH<-readRDS("results/T_quads_preds_noH.RDS")


# Annual averages ---------------------------------------------------------

SRKW_all_presence_year<-SRKW_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

T_all_presence_year<-T_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

p1<-ggplot(SRKW_all_presence_year) + 
  #geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  #geom_line(aes(x = Year, y = med)) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) +
  geom_point(aes(x = Year, y = med, color = med)) + 
  #geom_hline(aes(yintercept = 0.5), color = "red") + 
  scale_color_viridis_c(option = "magma", limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "a) SRKW") + #ggtitle("SRKW") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
p1

p2<-ggplot(T_all_presence_year) + 
  #geom_ribbon(aes(x = Year, ymin = lwr, ymax = upr), alpha = 0.5) + 
  #geom_line(aes(x = Year, y = med)) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) +
  geom_point(aes(x = Year, y = med, color = med)) + 
  #geom_hline(aes(yintercept = 0.5), color = "red") + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1)) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "b) Bigg's") + 
  #ggtitle("Transient") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
p2

p_annual<-p1+p2 + plot_layout(axis_titles = "collect", axes = "collect_y", guides = "collect")
p_annual

ggsave("Manuscript/figures/annual_average.png", dpi = 600, width = 5, height = 3, units = "in")


# Monthly Averages --------------------------------------------------------

mths<-seq(1, 12, by = 1)
mths_labs<-month.abb[mths]
names(mths_labs)<-as.character(mths)

#puget sound
SRKW_PS_presence<-SRKW_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/80) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

#central salish sea
SRKW_SS_presence<-SRKW_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad < 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/367) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

#as just probabilities

SRKW_PS_Month2<-ggplot(SRKW_PS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  #geom_line(aes(x = Year, y = med, group = Month, color = med)) + 
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        strip.text = element_blank(), 
        plot.tag.location = "panel") + 
  labs(y = "Average probability of presence", tag = "a) SRKW Puget Sound")
SRKW_PS_Month2

#plotting north of puget sound

SRKW_SS_Month2<-ggplot(SRKW_SS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  #geom_line(aes(x = Year, y = med, group = Month, color = med)) +
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        plot.tag.location = "panel") + 
  labs(y = "Average probability of presence", tag = "b) SRKW Central Salish Sea") 

SRKW_SS_Month2




SRKW_Month2<-SRKW_PS_Month2 + SRKW_SS_Month2 + plot_layout(guides = "collect", axes = "collect_y", axis_titles = "collect") & theme(plot.tag.position = c(0.45, 1.02))
SRKW_Month2


#ggsave("../Figures/SRKW_monthly_prob.png", SRKW_Month2, dpi = 600, width = 5, height  = 8, units = "in")

#Transients

T_PS_presence<-T_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/80) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

T_SS_presence<-T_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad < 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/367) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = quantile(avg_est, prob = 0.5), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))
 #as total
T_PS_Month2<-ggplot(T_PS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        plot.tag.location= "panel") + 
  labs(y = "Average probability of presence", tag = "c) Bigg's Puget Sound   ")

T_PS_Month2





T_SS_Month2<-ggplot(T_SS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        plot.tag.location= "panel") + 
  labs(y = "Average probability of presence", tag = "d) Bigg's Central Salish Sea")

T_SS_Month2

#combine with SRKW
SRKW_SS_Month2<-SRKW_SS_Month2 + theme(strip.text = element_blank())
T_PS_Month2<-T_PS_Month2 + theme(strip.text = element_blank())

month_tog<-SRKW_PS_Month2 + SRKW_SS_Month2 + T_PS_Month2 + T_SS_Month2 +
  plot_layout(ncol = 4, guides = "collect", axes = "collect_y", axis_titles = "collect") & theme(plot.tag.position = c(0.5, 1.015), 
                                                                                                 plot.tag = element_text(size = rel(0.9)))
month_tog

ggsave("Manuscript/figures/all_monthly_prob.png", month_tog, dpi = 600, width = 8, height  = 8, units = "in")


# Pod Specific ------------------------------------------------------------
#annual average
J_annual<-J_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))
K_annual<-K_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))
L_annual<-L_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

pJ<-ggplot(J_annual) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) +
  geom_point(aes(x = Year, y = med, color = med)) + 
  scale_color_viridis_c(option = "magma", limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "a) J pod") + #ggtitle("SRKW") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
pJ

pK<-ggplot(K_annual) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) +
  geom_point(aes(x = Year, y = med, color = med)) + 
  scale_color_viridis_c(option = "magma", limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "b) K pod") + #ggtitle("SRKW") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
pK

pL<-ggplot(L_annual) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) +
  geom_point(aes(x = Year, y = med, color = med)) + 
  scale_color_viridis_c(option = "magma", limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "c) L pod") + #ggtitle("SRKW") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
pL


pPods<-pJ + pK + pL + plot_layout(axes = "collect")

pPods

pPods2<-pJ / pK / pL / plot_layout(axes = "collect")

pPods2

ggsave("Manuscript/figures/annual_pods.png", pPods2, dpi = 600, width = 5, height = 6, units = "in")
#Monthly averages 
mths<-seq(1, 12, by = 1)
mths_labs<-month.abb[mths]
names(mths_labs)<-as.character(mths)

#puget sound
J_PS_presence<-J_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/80) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))


J_PS_Month<-ggplot(J_PS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  geom_line(aes(x = Year, y = med, group = Month, color = med)) + 
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  scale_x_continuous(limits = c(2015, 2022)) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        strip.text = element_blank(), 
        plot.tag.location = "panel") + 
  labs(y = "Average probability of presence", tag = "a) J Pod Puget Sound")
J_PS_Month

#puget sound
K_PS_presence<-K_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/80) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))


K_PS_Month<-ggplot(K_PS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  geom_line(aes(x = Year, y = med, group = Month, color = med)) + 
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  scale_x_continuous(limits = c(2015, 2022)) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        strip.text = element_blank(), 
        plot.tag.location = "panel") + 
  labs(y = "Average probability of presence", tag = "b) K Pod Puget Sound")

K_PS_Month

#puget sound
L_PS_presence<-L_quads_preds %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% filter(Quad >= 366) %>% group_by(Year, Month, Sim) %>% 
  summarise(avg_est = sum(est_real)/80) %>% ungroup() %>% group_by(Year, Month) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))


L_PS_Month<-ggplot(L_PS_presence) + 
  geom_linerange(aes(x = Year, ymin = lwr, ymax = upr, color = med)) + 
  geom_point(aes(x = Year, y = med, group = Month, color = med)) + 
  geom_line(aes(x = Year, y = med, group = Month, color = med)) + 
  facet_grid(Month~., labeller = as_labeller(mths_labs)) + 
  scale_color_viridis_c(option = "magma", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.00), expand = c(0, 0.06)) +
  scale_x_continuous(limits = c(2015, 2022)) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(), 
        legend.position = "none", 
        #strip.text = element_blank(), 
        plot.tag.location = "panel") + 
  labs(y = "Average probability of presence", tag = "c) L Pod Puget Sound")

L_PS_Month

#together
pod_PS<-J_PS_Month + K_PS_Month + L_PS_Month + plot_layout(guides = "collect", axes = "collect_y", axis_titles = "collect") & theme(plot.tag.position = c(0.45, 1.), 
                                                                                                                                    plot.tag = element_text(size = rel(0.9)))
pod_PS
ggsave("Manuscript/figures/pod_monthly_PS.png", pod_PS, width = 8, height = 6, units = "in", dpi = 600)



# SRKW and Transient Comparison all years ---------------------------------
#comparison
comp<-SRKW_quads_preds %>% left_join(T_quads_preds, by = c("Quad", "Year", "X", "Y", "fyear", "Month", "Lat", "Long")) %>%
  mutate_at(vars(-c("Quad", "Year", "X", "Y", "fyear", "Month", "Lat", "Long")), plogis)


# Identify column groups dynamically
original_names <- unique(unique(sub("\\.x$|\\.y$", "", names(comp)))[!(names(comp) %in% c("Quad", "Year", "X", "Y", "fyear", "Month", "Lat", "Long"))])

# Sum each duplicated column set separately
comp2<-comp

# Sum each duplicated column set separately, keep ".x" columns, and remove ".y"
for (col in original_names) {
  y_col <- paste0(col, ".y")
  x_col <- paste0(col, ".x")
  sum_col <- paste0(col, "_sum")  # New column for summed values
  prob_col<-paste0(col, "_prob") #new column for relative probabilities
  
  if (y_col %in% names(comp2)) {  # If a ".y" column exists
    comp2 <- comp2 %>%
      mutate(!!sum_col := rowSums(select(., all_of(c(x_col, y_col))), na.rm = TRUE)) %>%
      mutate(!!prob_col := ifelse(!!sym(sum_col) == 0, NA, !!sym(x_col) / !!sym(sum_col))) %>%
      select(-all_of(y_col))  # Remove only the ".y" column
  }
}


#get just relative probabilities and get estimate and confidence
comp3<- comp2 %>% select(c("Quad", "Year", "Month", ends_with("_prob"))) %>% 
  pivot_longer(ends_with("_prob"), names_to = "draw", 
               values_to = "prob") %>% group_by(Quad, Year, Month) %>%
  summarise(q50 = mean(prob), lwr = quantile(prob, 0.025), upr = quantile(prob, 0.975))


comp3<-comp3 %>% mutate(Date = mdy(paste(Month, "-01-", Year)))

#add regions
comp4<-comp3 %>% mutate(region = ifelse(Quad >= 434, "Hood canal", 
                                        ifelse(Quad >= 397, "Central basin", 
                                               ifelse(Quad >= 387, "Ad. inlet", 
                                                      ifelse(Quad >= 365, "Whidbey basin", 
                                                             ifelse(Quad >= 222, "Strait of Juan de Fuca", 
                                                                    ifelse(Quad >= 146, "San Juan islands", 
                                                                           "Strait of Georgia")))))))

comp4$region<-factor(comp4$region, levels = c("Strait of Georgia", "San Juan islands", "Strait of Juan de Fuca", 
                                              "Whidbey basin", "Ad. inlet", 
                                              "Central basin", "Hood canal"))

#write_csv(comp4, "results/T_S_Comparison.csv")
#read in data if necessary
comp4<-read_csv("results/T_S_Comparison.csv")


clrs<-c('#00E5AB','#ffffbd','#b8bcfb',"#fea77f",'#0070fe','#fe73de','#aaff00')
names(clrs)<-c("Strait of Georgia", "San Juan islands", "Strait of Juan de Fuca", 
               "Whidbey basin", "Ad. inlet", 
               "Central basin", "Hood canal")

comp4$region<-factor(comp4$region, levels = c("Strait of Georgia", "San Juan islands", "Strait of Juan de Fuca", 
                                              "Whidbey basin", "Ad. inlet", 
                                              "Central basin", "Hood canal"))
strip <- strip_themed(background_x = elem_list_rect(fill = clrs))
plot_all<-ggplot(comp4) + geom_tile(aes(x = Quad, y = Date, fill = q50)) + 
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red", 
                       midpoint = 0.5, limits = c(0, 1)) + 
  scale_x_continuous(breaks = seq(0, 500, by = 50), expand = c(0, 0)) + 
  scale_y_date(expand = c(0,0)) + 
  facet_grid2(~region, scales = "free_x", space = "free_x", 
              labeller = labeller(region = label_wrap_gen(4)), strip = strip) + 
  theme_minimal() + 
  labs(y = "Year", x = "Whale Museum Quadrant", fill = "Average relative \n SRKW probability ") + 
  theme(panel.spacing.x = unit(0, "lines"), 
        strip.background = element_rect(fill = clrs), 
        legend.position = "bottom", 
        strip.text = element_text(size = 8))

plot_all
ggsave("Manuscript/figures/T_SRKW_comparison_grid.png", dpi = 600, width = 11, height = 6)


# Comparison map in 2022 --------------------------------------------------

#Maps of 2022
SRKW_full_preds_2022<-SRKW_full_preds %>% filter(Year == 2022) %>% select(-c(est_non_rf, est_rf, omega_s, epsilon_st)) %>% mutate(est = plogis(est))
T_full_preds_2022<-T_full_preds %>% filter(Year == 2022)%>% select(-c(est_non_rf, est_rf, omega_s, epsilon_st)) %>% mutate(est = plogis(est))

comp_2022<-SRKW_full_preds_2022 %>% left_join(T_full_preds_2022, by = c("X", "Y", "Year", "Month", "fyear")) 
original_names <- "est"

comp_2022_2<-comp_2022
# Sum each duplicated column set separately, keep ".x" columns, and remove ".y"
for (col in original_names) {
  y_col <- paste0(col, ".y")
  x_col <- paste0(col, ".x")
  sum_col <- paste0(col, "_sum")  # New column for summed values
  prob_col<-paste0(col, "_prob") #new column for relative probabilities
  
  if (y_col %in% names(comp_2022_2)) {  # If a ".y" column exists
    comp_2022_2 <- comp_2022_2 %>%
      mutate(!!sum_col := rowSums(select(., all_of(c(x_col, y_col))), na.rm = TRUE)) %>%
      mutate(!!prob_col := ifelse(!!sym(sum_col) == 0, NA, !!sym(x_col) / !!sym(sum_col))) %>%
      select(-all_of(y_col))  # Remove only the ".y" column
  }
}

comp_2022_2

mths<-seq(1, 12, by = 1)
mths_labs<-month.abb[mths]
names(mths_labs)<-as.character(mths)

comp_map_2022<-ggplot(data=comp_2022_2) +
  geom_raster(aes(X, Y, fill = est_prob)) +
  geom_sf(data = ps, fill = "gray", color = "gray60") +
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red", 
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(fill = "Relative \nSRKW probability")+
  facet_wrap(~ Month, ncol = 4, labeller = as_labeller(mths_labs)) +
  coord_sf(xlim = c(400.000, 560.00), ylim = c(5215.00, 5460.00), 
           default_crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=km")) +
  theme_void() +
  theme(strip.text = element_text(face = "bold"), 
        title = element_text(face = "bold")) +
  ggtitle("2022")

comp_map_2022  

ggsave("Manuscript/figures/comparison_map_2022.png", comp_map_2022, width = 6, height = 8, units = "in")



# No hydrophone sensitivity -----------------------------------------------
#annual averages

SRKW_all_presence_year_noH<-SRKW_preds_noH %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

T_all_presence_year_noH<-T_preds_noH %>% pivot_longer(-c(Quad, Year, Month, fyear, Lat, Long, X, Y), names_to = "Sim", values_to = "est") %>% 
  mutate(est_real = plogis(est)) %>% group_by(Year, Sim) %>% 
  summarise(avg_est = sum(est_real)/(445*12)) %>% ungroup() %>% group_by(Year) %>% 
  summarise(med = mean(avg_est), lwr = quantile(avg_est, prob = 0.025), 
            upr = quantile(avg_est, prob = 0.975))

p1_noH<-ggplot() + 
  geom_linerange(data = SRKW_all_presence_year, aes(x = Year, ymin = lwr, ymax = upr), color = "gray") +
  geom_point(data = SRKW_all_presence_year, aes(x = Year, y = med), color = "gray") + 
  geom_linerange(data = SRKW_all_presence_year_noH, aes(x = Year, ymin = lwr, ymax = upr), color = "red") +
  geom_point(data = SRKW_all_presence_year_noH, aes(x = Year, y = med), color = "red", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "a) SRKW") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
p1_noH

p2_noH<-ggplot() + 
  geom_linerange(data = T_all_presence_year, aes(x = Year, ymin = lwr, ymax = upr), color = "gray") +
  geom_point(data = T_all_presence_year, aes(x = Year, y = med), color = "gray") + 
  geom_linerange(data = T_all_presence_year_noH, aes(x = Year, ymin = lwr, ymax = upr), color = "red") +
  geom_point(data = T_all_presence_year_noH, aes(x = Year, y = med), color = "red", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0.05)) +
  scale_x_continuous(expand = c(0.05,0.05)) + 
  labs(y = "Average annual probability of presence", tag = "b) Bigg's") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        plot.tag.location = "panel")
p2_noH

no_h_together<-p1_noH + p2_noH + plot_layout(axis_titles = "collect", axes = "collect_y", guides = "collect")
no_h_together

ggsave("Manuscript/figures/noH_annual_comparison.png", dpi = 600, width = 5, height = 3, units = "in")


# Maps of spatial and spatio-temporal distributions for supplement --------

plot_st_map <- function(dat, column) {
  print(ggplot(data=dat) +
    geom_raster(aes(X, Y, fill = {{ column }})) +
    geom_sf(data = ps, fill = "gray", color = "black") +
    coord_sf() + 
    facet_wrap(~Year) + 
    scale_fill_viridis_c() +
    theme_classic())
}

plot_sp_map<- function(dat, column) {
  print(ggplot(data=dat) +
    geom_raster(aes(X, Y, fill = {{ column }})) +
    geom_sf(data = ps, fill = "gray", color = "black") +
    coord_sf()+ 
    scale_fill_viridis_c() +
    theme_classic()) 
}

#SRKW
SRKW_epsilon_st<-plot_st_map(SRKW_quads_fullgrid, epsilon_st)
#SRKW_epsilon_st

SRKW_omega_s<-plot_sp_map(SRKW_quads_fullgrid, omega_s)
#SRKW_omega_s

ggsave("Manuscript/figures/SRKW_epsilonst_suppl.png", SRKW_epsilon_st, dpi = 600, width = 10, height = 10, units = "in")

ggsave("Manuscript/figures/SRKW_omegas_suppl.png", SRKW_omega_s, dpi = 600)


#Transients
T_epsilon_st<-plot_st_map(T_quads_fullgrid, epsilon_st)
#T_epsilon_st

T_omega_s<-plot_sp_map(T_quads_fullgrid, omega_s)
#T_omega_s

ggsave("Manuscript/figures/T_epsilonst_suppl.png", T_epsilon_st, dpi = 600, width = 10, height = 10, units = "in")

ggsave("Manuscript/figures/T_omegas_suppl.png", T_omega_s, dpi = 600)
