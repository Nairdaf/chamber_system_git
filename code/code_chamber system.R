# This code is to replicate analysis and figures for the paper "Comprehensive industry-relevant black soldier fly bioconversion characterisation by a novel chamber system", Fuhrmann et. al 2024
# Code developed by Adrian Fuhrmann and Moritz Gold

# Load libraries


library(tidyverse)
library(readxl)
library(hms)
library(ggplot2)
library(readr)

###load and prepare all csv data from data folder----

CO2_data <- read_csv("data/cs_pub_runs_CO2.csv")
scale_data <- read_csv("data/cs_pub_scale.csv")
manual_data <- read_csv("data/cs_pub_runs_manual.csv")
runs_data <- read_csv("data/cs_pub_runs.csv")
runs_g_data <- read_csv("data/cs_pub_runs_gather.csv")



#---

# process data----
#Controller refers to the Chamber and run to the experiment
#process manual data
# spread data

#make controller a factor
manual_data$Controller <- as.factor(manual_data$Controller)

runs_manual <-
  
 manual_data  %>% 
  
  group_by(Controller,run,duration_d,box,parameter,replicates) %>% 
  
  spread(key = parameter,value=value)


# summary performance data -----

# day 0 ----   

# calculate rearing duration per run

runs_duration <-
  
  runs_manual %>% 
  
  group_by(run) %>% 
  
  summarise(max_duration_d=max(duration_d))



# calculate parameters at day=0 that apply to all controllers

runs_manual_spread_all_day0 <- 
  
  manual_data %>% 
  
  filter(duration_d==0) %>% 
  
  filter(Controller=="all") %>% 
  
  group_by(run,parameter) %>% 
  
  summarise(mean=mean(value)) %>% 
  
  group_by(run) %>%
  
  spread(key = parameter,value=mean)

# merge all relevant data for day 0

runs_manual_spread_day0 <-
  
  manual_data %>% 
  
  filter(duration_d==0) %>% 
  
  filter(!Controller=="all") %>% 
  
  group_by(run,Controller,parameter) %>% 
  
  summarise(mean=mean(value)) %>% 
  
  group_by(run,Controller) %>% 
  
  spread(key = parameter,value=mean)   %>% 
  
  left_join(runs_manual_spread_all_day0,by="run") %>% 
  
  mutate(frassMC_perc=coalesce(frassMC_perc,wasteMC_perc)) %>% 
  
  left_join(runs_duration,by="run") 

# calculate starting metrics

runs_manual_spread_day0_calc <- 
  
  runs_manual_spread_day0 %>% 
  
  mutate(larval_density_Lcm2=number_larvae/(60*40), # larval density
         feedstock_gDM = feedstock_g*(1-frassMC_perc), # feedstock gDM
         feeding_rate_mgDMLd=(feedstock_gDM*1000)/(number_larvae*max_duration_d), # feeding rate g DM 
         larval_mass_tot_g_day0 = (larval_mass_mg*number_larvae)/1000, # larval mass beginning
         larval_mass_tot_gDM_day0 = (larval_mass_mgDM*number_larvae)/1000) %>% # larval DM beginning
  
  select(run,Controller,cocopeat_perc,feedstock_g,number_larvae,larval_mass_tot_gDM_day0,larval_mass_tot_g_day0,
         larval_density_Lcm2,feeding_rate_mgDMLd,feedstock_gDM)  

# rearing performance metrics ----

# calculate rearing performance metrics for each Experiment, Controller and box


runs_manual_gather_boxlevel <- 
  
  manual_data %>% 
  group_by(run) %>% 
  
  filter(duration_d==max(duration_d)) %>% 
  
  group_by(run,Controller,waste,parameter,box) %>% 
  
  summarise(value=mean(value,na.rm = T)) %>% 
  
  spread(key = parameter,value=value) %>% 
  
  left_join( runs_manual_spread_day0_calc,by=c("run","Controller")) %>% 
  
  mutate(frass_gDM = frass_g*(1-frassMC_perc),
         larvae_gDM = larvae_g*(1-larvaeMC_perc),  
         BCR_perc = ((larvae_g-larval_mass_tot_g_day0)/feedstock_g),
         BCR_percDM = (larvae_gDM-larval_mass_tot_gDM_day0)/feedstock_gDM,
         extBCR_percDM = (frass_gDM+larvae_gDM-larval_mass_tot_gDM_day0)/feedstock_gDM,
         loss_gDM = (feedstock_gDM-(frass_gDM+larvae_gDM-larval_mass_tot_gDM_day0)),
         WR_perc = 1-(frass_g/feedstock_g),
         WR_percDM = 1-(frass_gDM/feedstock_gDM),
         larval_number_end = larvae_g/(larval_mass_mg/1000),  
         survival_rate_perc = larval_number_end/number_larvae,
         water_in_waste_g = feedstock_g-feedstock_gDM,
         water_in_larvae_g = larvae_g-larvae_gDM,
         water_in_frass_g = frass_g-frass_gDM,
         water_removed_g = water_in_waste_g-water_in_larvae_g-water_in_frass_g)  %>% 
  
  gather(5:32,key="parameter",value="value")

# calculate rearing performance metrics for each run and controller

runs_manual_gather_controllerlevel <-
  
  runs_manual_gather_boxlevel %>% 
  
  group_by(run,Controller,waste,parameter) %>% 
  
  summarise(mean=mean(value,na.rm = T)) %>% 
  
  spread(key = "parameter",value="mean") %>% 
  
  select(run,Controller,waste,feeding_rate_mgDMLd,cocopeat_perc,larval_density_Lcm2,
         BCR_percDM,WR_percDM,frassMC_perc,survival_rate_perc,larval_mass_mgDM,extBCR_percDM, loss_gDM)



# sum up frass and larvae (dry matter and wet matter) over one controller
runs_manual_gather_controllerlevel_harvest_yield<-runs_manual_gather_boxlevel %>%
  
  group_by(run,Controller,parameter) %>% 
  
  filter(parameter=="frass_g" | parameter=="larvae_g" | parameter=="frass_gDM" | parameter=="larvae_gDM") %>% 
  
  summarise(sum = ifelse(all(is.na(value)), NA, sum(value, na.rm = TRUE)))%>%
  
  spread(key = "parameter",value="sum") 




# plot data ----
#instruction: please set under run either FW,CF or FW_het to plot the data for the respective run 

####CO2 curve -----

CO2_data %>% 
  filter(run %in% c("FW")) %>%
  
  ggplot(aes(duration_h/24, CO2_total_gh)) +
  
  geom_smooth(aes(color = Controller), method = "loess", span = 0.02, se=F) +
  
  geom_point(aes(color = Controller),size = 0.5, alpha=0.2) +
  
  #produce ribbon of confidence interval arond mean line
  # stat_summary(
  
  # aes(fill = "green"),
  #fun.data = mean_cl_normal,
  #geom = "ribbon",
  #alpha = 0.2,
  #color = NA, fill="blue",
  #) +
  
  
theme_bw(base_size = 12)+
  
  ylab(expression("CO"[2]*" production (g h"^{-1}*")")) +
  xlab("Rearing day")+
  labs(color = "Treatment") +
  
  # Increase text size for axis labels and names
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  
  # Legend settings with increased size
  theme(legend.text = element_text(size = 18)) +
  
  scale_color_discrete( name=" Food waste mixtures") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(legend.title = element_text(size = 19))



#### substrate +larvae mass loss----

scale_data %>%
  
  filter(run=="FW_het")%>%
  
  #filter(Controller %in% c("Adrian's Farm-House 2", "Adrian's Farm-House 3", "Adrian's Farm-House 4")) %>%
  ggplot(aes(duration_d, (-1)*weight_loss_kg, color=Controller)) + 
  
  #points with increased opacity
  geom_point(size=1) +
  
  
  # Theme settings
  theme_bw(base_size = 12) +
  
  
  # Axis labels
  ylab("Mass loss (kg)") +
  xlab("Rearing day") +
  
  # Increase text size for axis labels and names
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  
  # Legend settings with increased size
  theme(legend.text = element_text(size = 18)) +
  scale_color_discrete(name = "\n") 



#### larval weight ----
#make first data point plotable:

unique_controllers <- levels(manual_data$Controller)[-length(levels(manual_data$Controller))]

# Create new rows and bind to existing data frame
new_rows <- lapply(unique_controllers, function(controller) {
  subset_data <- subset(manual_data, Controller == "all")
  subset_data$Controller <- controller
  subset_data
})

new_rows_df <- do.call(rbind, new_rows)

# Combine the new rows with the original data frame
manual_data_plot <- rbind(manual_data, new_rows_df)


manual_data_plot%>%
  
  filter(run=="FW_het")%>%
  filter(Controller!="all")%>%
  filter(parameter=="larval_mass_mgDM" )%>% 
  
  ggplot(aes(x = duration_d, y = value, group = Controller)) +
  
  # Scatter plot
  geom_point(aes(color = Controller)) +
  
  # Line plot with mean
  stat_summary(aes(color = Controller), fun = "mean", geom = "line", size = 0.7) +
  
  # Error bars for mean +/- standard deviation
  stat_summary(
    data = . %>% filter(duration_d != 0),
    fun.data = "mean_sdl",
    geom = "errorbar",
    fun.args = list(mult = 1),
    size = 0.7,
    width = 0.1
  ) +
  
  # Theme settings
  theme_bw(base_size = 12) +
  
  
  # Axis labels
  ylab("Larval mass (mg DM)") +
  xlab("Rearing day") +
  
  # Increase text size for axis labels and names
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  
  # Legend settings with increased size
  theme(legend.text = element_text(size = 18)) +
  scale_color_discrete( name = "\n") 





####substrate/frass temperature----

runs_g_data%>% 
  
  filter(run=="FW_het")%>%
  
  filter(parameter== "T" )%>%
  
  filter(type== "residue" )%>%
  
  
  ggplot(aes(duration_d, value, color=Controller)) + 
  
  #points with increased opacity
  geom_point(alpha=0.02,size=0.01) +
  stat_summary(fun = "mean", geom = "line", linewidth = 2)+
  
  
  theme_bw(base_size = 12)+
  
  
  ylab("Substrate/Frass temperature (C°)") +
  xlab("Rearing day")+
  labs(color = "Reactor") +
  
  # Increase text size for axis labels and names
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  
  # Legend settings with increased size
  theme(legend.text = element_text(size = 18)) +
  scale_color_discrete( name = "\n") +
  
 
  theme(
    legend.spacing.y = unit(1, "lines"),      
    legend.text = element_text(margin = margin(r = 10)),  
    legend.title = element_text(margin = margin(b = 10)), 
    legend.box.spacing = unit(0.5, "lines")  
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 7.3) , byrow = TRUE 
  ))



