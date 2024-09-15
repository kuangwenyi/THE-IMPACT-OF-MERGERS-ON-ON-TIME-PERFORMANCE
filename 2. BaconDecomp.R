########### Bacon Decomposition to Check Early VS Late Treated Weights ##########

# load package
# install relevant packages if not installed
pkgs = c(
  'tidyverse','patchwork','fastDummies','ggthemes','did','bacondecomp',
  'kableExtra','fixest','ggplot2','readxl','readr','tidyr',
  'dplyr','stringr','lme4','RColorBrewer','broom.mixed', 'TwoWayFEWeights', 
  'DIDmultiplegt')

kwy = lapply(pkgs, library, character.only=TRUE)

# set blank plot theme for all Figures in the manuscript
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# read in balanced data for Bacon Decomposition
# bacon decomposition has to use balanced panel. dta data is a reconstructed balaned panel.
Essay3_R <- haven::read_dta("Essay3.dta") 

###### 3.1 Bacon Decomposition ------
colnames(Essay3_R)
Essay3_R$lCarrierDelay = log(Essay3_R$AIRCARRIERDELAY)

# calculate the bacon decomposition without covariates
bacon_out <- bacon(lCarrierDelay ~ Treated, # percent on time 
                   data = Essay3_R,
                   id_var = "Carriercode",
                   time_var = "Occasion") 

# get the total weight and weighted average for each group (code adapted from Baker et al. 2022)
total_weights <- bacon_out %>% 
  group_by(type) %>% 
  summarize(weight = sum(weight))

group_avg <- bacon_out %>% 
  group_by(type) %>% 
  summarize(avg = weighted.mean(estimate, weight),
            weights = sum(weight))

# Bacon Decomposition Plot (This is Figure 2 in the manuscript)
# code based on Baker et al. 2022

#### Make Bacon Decomposition Plots
# Group 1: early v late 
EvL <- bacon_out %>% 
  filter(type == "Earlier vs Later Treated") %>% 
  ggplot(aes(x = weight, y = estimate)) + 
  geom_point(size = 3, alpha = 1/2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = group_avg$avg[1], color = "#014182", linewidth = 1.5) + 
  labs(x = "Weight", y = expression(widehat(delta^'DD'))) + 
  ggtitle(paste0("Early vs Later Treated \n Total Weight = ", scales::percent(total_weights$weight[1]))) + 
  scale_y_continuous(limits = c(-2, 2)) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# Group 2: late v early
LvE <- bacon_out %>% 
  filter(type == "Later vs Earlier Treated") %>% 
  ggplot(aes(x = weight, y = estimate)) + 
  geom_point(size = 3, alpha = 1/2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = group_avg$avg[2], color = "#014182", linewidth = 1.5) + 
  labs(x = "Weight", y = expression(widehat(delta^'DD'))) + 
  scale_y_continuous(limits = c(-2, 2)) + 
  ggtitle(paste0("Later vs Early Treated \n Total Weight = ", scales::percent(total_weights$weight[2]))) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# Group 3: Treated VS Untreated
TvU <- bacon_out %>% 
  filter(type == "Treated vs Untreated") %>% 
  ggplot(aes(x = weight, y = estimate)) + 
  geom_point(size = 3, alpha = 1/2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = group_avg$avg[[3]], color = "#014182", linewidth = 1.5) + 
  labs(x = "Weight", y = expression(widehat(delta^'DD'))) + 
  scale_y_continuous(limits = c(-2, 2)) + 
  ggtitle(paste0("Treated vs Untreated \n Total Weight = ", scales::percent(total_weights$weight[3]))) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# combine the figures and save
BLL_decomp_plot <- EvL + LvE + TvU
plot(BLL_decomp_plot) # check plot before saving

# combine two plots
Bacon_decomp_plot <- EvL + LvE
Bacon_decomp_plot

# save
ggsave(Bacon_decomp_plot, 
       filename = here::here("Figs_Tables", "Delay_decomp_plot.png"),
       dpi = 500, width = 6, height = 3.5)

