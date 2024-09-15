
#### MERGER IMPACT ON ARILINE-INDUCED DELAYS ####

#### SECTION 0 Data Prep #### 
# load required packages
pkgs = c(
  'tidyverse','patchwork','fastDummies','ggthemes','did','bacondecomp',
  'kableExtra','fixest','ggplot2','readxl','readr','tidyr',
  'dplyr','stringr','lme4','RColorBrewer','broom.mixed', 'TwoWayFEWeights', 
  'DIDmultiplegt', 'here')

kwy = lapply(pkgs, library, character.only=TRUE)

# set blank plot theme for all Figures in the manuscript
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))


# read in file
Essay3_OD = read.csv("Essay3_OD_Cleaned.csv")

# aggregate to carrier-route-quarter level data # 10007 OD Pair
Essay3_OD_New = Essay3_OD %>% group_by(Year,Quarter,Reporting_Airline, OD_pair) %>%
  summarise(TotalFlights = sum(TotalFlights, na.rm = T), 
            TaxiOut = sum(TaxiOut_T, na.rm =  T), 
            TaxiIn = sum(TaxiIn_T, na.rm = T), 
            DepDelay = sum(DepDelay_T, na.rm = T), 
            DepDelayMinutes = sum(DepDelayMinutes_T,na.rm = T), 
            ArrDelay = sum(ArrDelay_T, na.rm = T), 
            ArrDelayMinutes = sum(ArrDelayMinutes_T,na.rm = T), 
            CarrierDelay = sum(CarrierDelay_T, na.rm = T), 
            WeatherDelay = sum(WeatherDelay_T, na.rm = T), 
            NASDelay = sum(NASDelay_T, na.rm = T),
            SecurityDelay = sum(SecurityDelay_T, na.rm = T),
            LateAircraftDelay = sum(LateAircraftDelay_T, na.rm = T), 
            Dist = mean(Dist))

# Read in Quarterly Data with other variables
E1 = read_excel("E1.xlsx")
E1$carriercode = NULL
E1$v1 = NULL
E1$yearq = NULL

# Rename airlines to 2 digit IATA Code to combine the two files
test = as.data.frame(unique(E1$airline))
newcol = as.data.frame (c('FL', 'OO', 'AS', 
           'WN', 'DL', 'HP', 
           'NW', 'TZ', 'HA', 
           'US', 'B6', 'UA',
           'MQ', 'CO', 'AA', 
           'XE', 'OH', 'EV', 
           'DH', 'F9', 'YV', 
           'AQ', '9E', 'VX', 
           'NK', 'YX', 'OH', 
           'G4' )
           )

test = cbind(test, newcol)
colnames(test) = c('airline', "Reporting_Airline")

E1 = E1 %>% left_join(test, by = "airline")
  
# Combine the two files
Essay3_R = Essay3_OD_New %>% 
  left_join(E1, by = c("Year" = "year", 
                       'Quarter' = 'quarter',
                       'Reporting_Airline'))

# get rid of the original airline column in the quarterly data to avoid confusion
Essay3_R$airline = NULL
Essay3_R$occasion = NULL

# create OD dummy and join 
OD_list = data.frame(unique(Essay3_R$OD_pair)) # 10007 OD Pair
OD_list$OD_Dummy = seq(1:10007)
colnames(OD_list) = c("OD_pair", "OD_Dummy")

Essay3_R = Essay3_R %>% left_join(OD_list, by= "OD_pair")

# create Carrier ID and join # 27 Carriers
arlinename = as.data.frame(sort(unique(Essay3_R$Reporting_Airline))) %>% 
  mutate(Carriercode = seq(1:n()))

colnames(arlinename) = c("Reporting_Airline", "Carriercode")

Essay3_R = merge(Essay3_R, arlinename, by = "Reporting_Airline")

# create Occasion ID
Essay3_R$YearQ = paste(Essay3_R$Year, Essay3_R$Quarter, sep = "-")
Occasion_list = data.frame(sort(unique(Essay3_R$YearQ)))

Occasion_list$OccasionDummy = seq(1:64)
colnames(Occasion_list) = c("YearQ", "OccasionDummy")

Essay3_R = merge(Essay3_R, Occasion_list, by = "YearQ")

# create merger occasions (FirstTreat occasion)
Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("AS","VX"),
                                 52,
                                 0)

Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("AA","US"),
                                 40,
                                 Essay3_R$m_com_occasion)

Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("DL","NW"),
                                 20,
                                 Essay3_R$m_com_occasion)

Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("XE","EV"),
                                 32,
                                 Essay3_R$m_com_occasion)

Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("F9","YX"),
                                 28,
                                 Essay3_R$m_com_occasion)

Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("WN","FL"),
                                 30,
                                 Essay3_R$m_com_occasion)

Essay3_R$m_com_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("UA","CO"),
                                 28,
                                 Essay3_R$m_com_occasion)


# create merger completion Treated occasion
Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("AS","VX") & 
                            Essay3_R$OccasionDummy > 52,
                          1,
                          0)

Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("AA","US") & 
                            Essay3_R$OccasionDummy > 40,
                          1,
                          Essay3_R$Treated)

Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("DL","NW")& 
                            Essay3_R$OccasionDummy > 20,
                          1,
                          Essay3_R$Treated)

Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("XE","EV")& 
                            Essay3_R$OccasionDummy > 32,
                          1,
                          Essay3_R$Treated)

Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("F9","YX")& 
                            Essay3_R$OccasionDummy > 28,
                          1,
                          Essay3_R$Treated)

Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("WN","FL")& 
                            Essay3_R$OccasionDummy > 30,
                          1,
                          Essay3_R$Treated)

Essay3_R$Treated = ifelse(Essay3_R$Reporting_Airline %in% c("UA","CO")& 
                            Essay3_R$OccasionDummy > 28,
                          1,
                          Essay3_R$Treated)

# sanity check
unique(Essay3_R$m_com_occasion)
unique(Essay3_R$Treated)

# create announcement occasions for robustness test 
Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("AS","VX"),
                                 50,
                                 0)

Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("AA","US"),
                                 37,
                                 Essay3_R$m_ann_occasion)

Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("DL","NW"),
                                 18,
                                 Essay3_R$m_ann_occasion)

Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("XE","EV"),
                                 27,
                                 Essay3_R$m_ann_occasion)

Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("F9","YX"),
                                 26,
                                 Essay3_R$m_ann_occasion)

Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("WN","FL"),
                                 27,
                                 Essay3_R$m_ann_occasion)

Essay3_R$m_ann_occasion = ifelse(Essay3_R$Reporting_Airline %in% c("UA","CO"),
                                 26,
                                 Essay3_R$m_ann_occasion)


# create merger announcement treat occasion 
Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("AS","VX") & 
                            Essay3_R$OccasionDummy > 50,
                          1,
                          0)

Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("AA","US") & 
                            Essay3_R$OccasionDummy > 37,
                          1,
                          Essay3_R$Treated_ann)

Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("DL","NW")& 
                            Essay3_R$OccasionDummy > 18,
                          1,
                          Essay3_R$Treated_ann)

Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("XE","EV")& 
                            Essay3_R$OccasionDummy > 27,
                          1,
                          Essay3_R$Treated_ann)

Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("F9","YX")& 
                            Essay3_R$OccasionDummy > 26,
                          1,
                          Essay3_R$Treated_ann)

Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("WN","FL")& 
                            Essay3_R$OccasionDummy > 27,
                          1,
                          Essay3_R$Treated_ann)

Essay3_R$Treated_ann = ifelse(Essay3_R$Reporting_Airline %in% c("UA","CO")& 
                            Essay3_R$OccasionDummy > 26,
                          1,
                          Essay3_R$Treated_ann)


# FirstTreated Occasion and other var
Essay3_R$FirstTreat = Essay3_R$m_com_occasion
Essay3_R$FirstTreat_IE = ifelse(Essay3_R$FirstTreat == Essay3_R$occasion, 1, 0)

Essay3_R$rel_occasion = Essay3_R$OccasionDummy - Essay3_R$FirstTreat

# create other variables
Essay3_R = Essay3_R %>% mutate(
  pOntime = ontime/totalrecords,
  lTotalDelay = log(totaldelay),
  lCarrierDelay = log(CarrierDelay),
  pDelay = totaldelay/totalrecords,
  lTotalComplaint = log(totalcomplaint), 
  pComp = totalcomplaint/enplanedpassengers *1000,
  ROA = net_income/assets,
  lBagComplaint = log(baggage),
  lpcpGDP = log(percpita_gdp),
  lFare = log(avg_fare),
  lYield = log(yieldpax),
  lHetero = log(heterogeneity), 
  lEMPFTE = log(empfte),
  lEnPax = log(revpaxenplaned),
  lOntime = log(ontime), 
  lTaxiIn = log(TaxiIn), 
  lTaxiOut = log(TaxiOut), 
  lTotalFlights = log(TotalFlights),
  lDist = log(Dist),
  LCC = lcc)

# Make a reference copy
Essay3_R_OR = Essay3_R
write.csv(Essay3_R_OR, file = "Essay3_R_0606.csv")

#### SECTION 1 ACQUIRER ONLY  ####

##### 1.1 Data Prep ##### 

# acquirer only
Essay3_R = Essay3_R_OR[!(Essay3_R_OR$Reporting_Airline %in% c("VX", "US","NW",
                                                    "EV",
                                                    "YX", "FL",
                                                    "CO")), ]

# function to get treat-year specific cohorts for 20 quarters
make_dt <- function(tyr) {
  Essay3_R %>% 
    filter(OccasionDummy <= 52) %>% # drop observation after everyone is treated
    filter(FirstTreat == tyr | FirstTreat > tyr + 20) %>% 
    filter(OccasionDummy %>% between(tyr - 4, tyr + 20)) %>% # pre 4 post 20
    mutate(FirstTreat = if_else(FirstTreat == tyr, FirstTreat, NA_real_),
           rel_occasion = OccasionDummy - FirstTreat) %>% 
    select(Carriercode,
           OccasionDummy, FirstTreat, Treated, 
           rel_occasion, 
           OPOR,pOntime, #DV
           lfp,lfleetutil,lhetero, lsparsity, # operational metrics
           lfueleff,lYield, lavglandfee, ltdomt_cost, # efficiency metrics
           lEnPax, lempfte,# Human Metrics
           lmisbagg,lTotalDelay, lcomplaint, # Service quality metrics
           lpcpGDP,lFare, percent_gdp, percent_chg_fare, # Marco metrics
           lMKTshare, # market power metrics
           OD_Dummy,
           lTaxiIn, lTaxiOut, lTotalFlights,
           LCC, recession,
           lCarrierDelay, # carrier specific delay
           Year, Quarter) %>% 
    mutate(dt = as.character(tyr))
}


# treats
treats <- Essay3_R %>% 
  filter(FirstTreat < max(FirstTreat)) %>% 
  pull(FirstTreat) %>% 
  unique() %>% 
  sort()

# stack the data 
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_occasion", 
             remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(Carriercode, "_", dt))

# make formula
indicatorStacked <- c(paste0("`", "rel_occasion_", c(-4:-1, 1:20), "`"))

##### 1.2 Impact on Delays - Acquirer ##### 
# estimate the model and plot
# without controls
stack_nc <- feols(lCarrierDelay ~ .[indicatorStacked] | 
                    OccasionDummy^dt + Carriercode^dt + OD_Dummy^dt + 
                    Year^dt + Quarter^dt,
                  cluster = "Carriercode", data = stacked_data)
summary(stack_nc)

# with controls
stack1_Acquirer <- feols(lCarrierDelay ~ .[indicatorStacked] + 
                     lfp + lfleetutil + lhetero + lsparsity +
                     lEnPax + lempfte + 
                     lMKTshare +
                     lTaxiIn + lTaxiOut + lTotalFlights
                    |OccasionDummy^dt + Carriercode^dt + OD_Dummy^dt + 
                    Year^dt + Quarter^dt,
                    cluster = "Carriercode",
                    data = stacked_data)

summary(stack1_Acquirer)

# unpack
coef_acquirer_dynamic = broom::tidy(stack1_Acquirer, conf.int = TRUE)
write.csv(coef_acquirer_dynamic, file= "coef_Acquirer_Dynamic.csv")

# plot (Figure XXX)
ES_stack_Acquirer
ES_stack_Acquirer <- broom::tidy(stack1_Acquirer, conf.int = TRUE)[1:24,] %>%
  # add in the relative time variable
  mutate(t = c(-4:-1, 1:20)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill = factor(group)), shape = 21) + 
  scale_fill_manual(values = c("#993441", "#0029a5"))  + 
  geom_line() + 
  ggtitle("The Dynamic Effect of Airline Mergers on  \n Carrier-Induced Delays - Acquirer") + 
  geom_errorbar(aes(ymin = conf.low + 0.03, ymax = conf.high - 0.03, color= factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Change in Carrier-Induced Delays", x = "Quarters Relative to Merger")+
  #subtitle = "Stagged Regression"#) + 
  scale_x_continuous(breaks = seq(-4, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-2, 2, by = 0.2)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

ggsave(ES_stack_Acquirer, filename = here::here("Figs_Tables", "Acquirer_Delay.png"), 
       dpi = 500, width = 6, height = 4)

##### 1.3 Static Effect of Delays - Acquirer ##### 
# create binned data pre 4 and post 20
data_dummieBin <- Essay3_R  %>% 
  dummy_cols(select_columns = "rel_occasion", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>%
  mutate(`rel_occasion_-4` = if_else(rel_occasion <= -4, -1, 0), # omit t-1 and t-2 due to balanced panel
         rel_occasion_20 = if_else(rel_occasion >= 20, 1, 0))

mod_Static_Acquirer <- feols(lCarrierDelay ~ Treated + 
                        lfp + lfleetutil + lhetero + lsparsity +
                        lEnPax + lempfte + 
                        lMKTshare + 
                        lTaxiIn + lTaxiOut + lTotalFlights |
                        OccasionDummy + Carriercode + 
                        OD_Dummy + Year + Quarter, 
                        cluster = "Carriercode", 
                        data = data_dummieBin)

summary(mod_Static_Acquirer)

#### SECTION 2 TARGET ONLY ####

##### 2.1 Data Prep ##### 
# Target only
Essay3_R = Essay3_R_OR[!Essay3_R_OR$Reporting_Airline %in% c("AS", 
                                                   "AA","US", 
                                                   "DL",
                                                   "XE",
                                                   "F9", "YX", 
                                                   "WN",
                                                   "UA"), ]

# function to get treat-year specific cohorts for 20 quarters
make_dt <- function(tyr) {
  Essay3_R %>% 
    filter(OccasionDummy <= 52) %>% # drop observation after everyone is treated
    filter(FirstTreat == tyr | FirstTreat > tyr + 20) %>% 
    filter(OccasionDummy %>% between(tyr - 4, tyr + 20)) %>% # pre 4 post 20
    mutate(FirstTreat = if_else(FirstTreat == tyr, FirstTreat, NA_real_),
           rel_occasion = OccasionDummy - FirstTreat) %>% 
    select(Carriercode,
           OccasionDummy, FirstTreat, Treated, 
           rel_occasion, 
           OPOR,pOntime, #DV
           lfp,lfleetutil,lhetero, lsparsity, # operational metrics
           lfueleff,lYield, lavglandfee, ltdomt_cost, # efficiency metrics
           lEnPax, lempfte,# Human Metrics
           lmisbagg,lTotalDelay, lcomplaint, # Service quality metrics
           lpcpGDP,lFare, percent_gdp, percent_chg_fare, # Marco metrics
           lMKTshare, # market power metrics
           OD_Dummy,
           lTaxiIn, lTaxiOut, lTotalFlights,
           LCC, recession,
           lCarrierDelay, # carrier specific delay
           Year, Quarter) %>% 
    mutate(dt = as.character(tyr))
}


# treats
treats <- Essay3_R %>% 
  filter(FirstTreat < max(FirstTreat)) %>% 
  pull(FirstTreat) %>% 
  unique() %>% 
  sort()

# stack the data 
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_occasion", 
             remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(Carriercode, "_", dt))

# make formula post 10 is the max
indicatorStacked <- c(paste0("`", "rel_occasion_", c(-4:-1, 1:10), "`"))


##### 2.2 Impact on Delays - Target ##### 
# estimate the model and plot
# without controls
stack_nc <- feols(lCarrierDelay ~ .[indicatorStacked] | OccasionDummy^dt + Carriercode^dt,
                  cluster = "Carriercode", data = stacked_data)
summary(stack_nc)

# with controls
# Occasion_Dummy taken out to avoid Col.
stack_target_Delay <- feols(lCarrierDelay ~ .[indicatorStacked] + 
                     lfp + lfleetutil + lhetero + lsparsity +
                     lEnPax + lempfte + 
                     lMKTshare + 
                     lTaxiIn + lTaxiOut + lTotalFlights
                     |Carriercode^dt  + OD_Dummy^dt + Year^dt + Quarter^dt,
                   cluster = "Carriercode",
                   data = stacked_data)

summary(stack_target_Delay)


# unpack
coef_target_delay = broom::tidy(stack_target_Delay, conf.int = TRUE)
write.csv(coef_target_delay, file= "coef_Target_Delay_0608.csv")
write.csv(stacked_data, file = "stacked_data_target.csv")

ES_stack_Target

ES_stack_Target <- broom::tidy(stack_target_Delay, conf.int = TRUE)[1:14,] %>%
  # add in the relative time variable
  mutate(t = c(-4:-1, 1:10)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill = factor(group)), shape = 21) + 
  scale_fill_manual(values = c("#993441", "#0029a5"))  + 
  geom_line() + 
  ggtitle("The Dynamic Effect of Airline Mergers on \n Carrier-Induced Delays - Target") + 
  geom_errorbar(aes(ymin = conf.low + 0.03, ymax = conf.high - 0.03, color= factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Change in Carrier-Induced Delays", x = "Quarters Relative to Merger")+
  #subtitle = "Stagged Regression"#) + 
  scale_x_continuous(breaks = seq(-4, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-4, 2, by = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

ggsave(ES_stack_Target, filename = here::here("Figs_Tables", "Target_Delay.png"), 
       dpi = 500, width = 6, height = 4)


##### 2.2 Static Effect of Delays - Target ##### 
# bin pre 4 and post 10
data_dummieBin <- Essay3_R  %>% 
  dummy_cols(select_columns = "rel_occasion", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>%
  mutate(`rel_occasion_-4` = if_else(rel_occasion <= -4, -1, 0), 
         rel_occasion_20 = if_else(rel_occasion >= 10, 1, 0))

mod_Static_Target <- feols(lCarrierDelay ~ Treated + 
                               lfp + lfleetutil + lhetero + lsparsity +
                               lEnPax + lempfte + 
                               lMKTshare + 
                               lTaxiIn + lTaxiOut + lTotalFlights | 
                               OccasionDummy + OD_Dummy + Carriercode + 
                               Year + Quarter, 
                             cluster = "Carriercode", 
                             data = stacked_data)

summary(mod_Static_Target)

#### SECTION 3 TEST PARRALLEL TREND ####
# read in data
Essay3_R <- readxl::read_excel(here('Data',"Essay_Quarterly.xlsx"))

# create first treat identifier for Instantaneous effect
Essay3_R$FirstTreat_IE = ifelse(Essay3_R$FirstTreat == Essay3_R$Occasion, 1, 0)

# create variables
Essay3_R = Essay3_R %>% mutate(pOntime = ONTIME/TOTAL.RECORDS,
                               OPOR = OP_PROFIT_LOSS/OP_REVENUES,
                               lTotalDelay = log(totaldelay),
                               pDelay = totaldelay/TOTAL.RECORDS,
                               lTotalComplaint = log(total),
                               pComp = total/enplaned.passengers *1000,
                               lCarrierDelay = log(AIR.CARRIER.DELAY),
                               ROA = NET_INCOME/ASSETS,
                               lBagComplaint = log(baggage),
                               lfuel = log(TDOMT_COST),
                               lEnPax = log(RevPaxEnplaned),
                               lEMPFTE = log(EMPFTE),
                               lFare = log(avg_fare),
                               lYield = log(yieldPAX),
                               lfleetutil = log(fleetutil),
                               lpcpGDP = log(percpita_gdp),
                               lseatmile = log(AvlSeatMiles))

Y = "lCarrierDelay"
G = "Carriercode"
T = "Occasion"
D = "FirstTreat_IE" # This variable is used to estimate using deChaiseMartin (change of treatment status from t-1 to t)
D_TWFE = "Treated" # This variable is used to demonstrate TWFE pitfalls. Refer to Lang and Bliese 2016 Coding

controls = c("lYield", "LFP","lfleetutil", "lEnPax","lEMPFTE", "lfuel")

# get rid of NA and Inf for model
Essay3_R$lYield[is.na(Essay3_R$lYield)] = 0
Essay3_R$LFP[is.na(Essay3_R$LFP)] = 0
Essay3_R$lfleetutil[is.na(Essay3_R$lfleetutil)] = 0
Essay3_R$lEnPax[is.na(Essay3_R$lEnPax)] = 0
Essay3_R$lEMPFTE[is.na(Essay3_R$lEMPFTE)] = 0
Essay3_R$lfuel[is.na(Essay3_R$lfuel)] = 0
Essay3_R$lCarrierDelay[is.na(Essay3_R$lCarrierDelay)] = 0


Essay3_R$lYield[is.infinite(Essay3_R$lYield)] = 0
Essay3_R$LFP[is.infinite(Essay3_R$LFP)] = 0
Essay3_R$lfleetutil[is.infinite(Essay3_R$lfleetutil)] = 0
Essay3_R$lEnPax[is.infinite(Essay3_R$lEnPax)] = 0
Essay3_R$lEMPFTE[is.infinite(Essay3_R$lEMPFTE)] = 0
Essay3_R$lfuel[is.infinite(Essay3_R$lfuel)] = 0
Essay3_R$lCarrierDelay[is.infinite(Essay3_R$lCarrierDelay)] = 0

# decompose weights
twowayfeweights(Essay3_R, Y, G, T, D_TWFE, cmd_type = "feTR")
twowayfeweights(Essay3_R, Y, G, T, D_TWFE, cmd_type = "feTR", controls = controls)

# estimate instantaneous effect and 4 placebo 
set.seed(1234) # set seed for exact model outputs 
# model without control for baselining. Not reported in manuscript
model_IE_nc = did_multiplegt(Essay3_R, Y, G, T, D, placebo = 4, 
                             brep = 10, 
                             parallel = TRUE,
                             cluster = "Carriercode")

model_IE = did_multiplegt(Essay3_R, Y, G, T, D, controls = controls, 
                          dynamic = 0, 
                          placebo = 4,
                          brep = 20, 
                          parallel = TRUE, 
                          cluster = 'Carriercode') # brep (bootstrap) to get CI

#### Some prep work to plot Instantaneous Effect 

# unlist and combine model outputs for plotting
total = list(model_IE_nc, model_IE)
test = as.data.frame(do.call(cbind, total))
test$Names = rownames(test)

# extract estimates and se for plot
estimates =rbind(test[grepl('^placebo_', test$Names), ], test[grepl('^effect', test$Names), ])
se = rbind(test[grepl('^se_placebo_', test$Names), ], test[grepl('^se_effect', test$Names), ])

estimates$Names = NULL
se$Names = NULL

# convert coeffients to numeric
for (i in 1:ncol(estimates)){
  estimates[[i]] <- as.numeric(estimates[[i]])}

for (j in 1:ncol(se)){
  se[[j]] <- as.numeric(se[[j]])}

# Instantaneous Effect Plot (This is Figure 1 in the manuscript)
df <- data.frame(x =-4:0,
                 F =estimates$V2,
                 L =estimates$V2 - se$V2*1.96, # 5% confidence level
                 U =estimates$V2 + se$V2*1.96)
# IE_plot
IE_plot =  df %>% 
  mutate(group = as.factor(case_when(
    x < 0~1,
    x >= 0~2 ))) %>% 
  # plot
  ggplot(aes(x = x, y = F)) + 
  geom_point(aes(fill= factor(group)), shape = 21) + geom_line() + 
  scale_fill_manual(values = c("#993441", "#0029a5")) + 
  ggtitle("Parallel Trend Assumption Test on Carrier-Induced Delay") + 
  geom_errorbar(aes(ymin = L, ymax = U, 
                    color = factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Chang in Carrier-Induced Delay", x = "Quarters before Merger",
       subtitle = "de Chaisemartin and D’Haultfoeuille (2020) Estimator") + 
  scale_x_continuous(breaks = seq(-4, 0, by = 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.2)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

IE_plot

ggsave(IE_plot, filename = here::here("Figs_Tables", "Parallel_Trend_Test.png"), 
       dpi = 500, width = 6, height = 4)


# Another tidy function to unpack models
tidy_IE = function(x, level = 0.95) {
  ests = x[grepl("^placebo_|^effect|^dynamic_", names(x))]
  ret = data.frame(
    term      = names(ests),
    estimate  = as.numeric(ests),
    std.error = as.numeric(x[grepl("^se_placebo|^se_effect|^se_dynamic", names(x))]),
    N         = as.numeric(x[grepl("^N_placebo|^N_effect|^N_dynamic", names(x))])
  ) |>
    # For CIs we'll assume standard normal distribution
    within({
      conf.low  = estimate - std.error*(qnorm(1-(1-level)/2))
      conf.high = estimate + std.error*(qnorm(1-(1-level)/2))
    })
  return(ret)
}

coef_IE = tidy_IE(model_IE)
write.csv(coef_IE, file = "coef_IE_0606.csv")

#### SECTION 4 PLACEBO MERGER ANNOUCEMENT DATE TEST ####
##### 1.1 Data Prep ##### 
# acquirer only
Essay3_R = Essay3_R_OR[!(Essay3_R_OR$Reporting_Airline %in% c("VX", "US","NW",
                                                              "EV",
                                                              "YX", "FL",
                                                              "CO")), ]

Essay3_R$FirstTreat = Essay3_R$m_ann_occasion


# function to get treat-year specific cohorts for 20 quarters
make_dt <- function(tyr) {
  Essay3_R %>% 
    filter(OccasionDummy <= 52) %>% # drop observation after everyone is treated
    filter(FirstTreat == tyr | FirstTreat > tyr + 20) %>% 
    filter(OccasionDummy %>% between(tyr - 4, tyr + 20)) %>% # pre 4 post 20
    mutate(FirstTreat = if_else(FirstTreat == tyr, FirstTreat, NA_real_),
           rel_occasion = OccasionDummy - FirstTreat) %>% 
    select(Carriercode,
           OccasionDummy, FirstTreat, Treated, 
           rel_occasion, 
           OPOR,pOntime, #DV
           lfp,lfleetutil,lhetero, lsparsity, # operational metrics
           lfueleff,lYield, lavglandfee, ltdomt_cost, # efficiency metrics
           lEnPax, lempfte,# Human Metrics
           lmisbagg,lTotalDelay, lcomplaint, # Service quality metrics
           lpcpGDP,lFare, percent_gdp, percent_chg_fare, # Marco metrics
           lMKTshare, # market power metrics
           OD_Dummy,
           lTaxiIn, lTaxiOut, lTotalFlights,
           LCC, recession,
           lCarrierDelay, # carrier specific delay
           Year, Quarter) %>% 
    mutate(dt = as.character(tyr))
}


# treats
treats <- Essay3_R %>% 
  filter(FirstTreat < max(FirstTreat)) %>% 
  pull(FirstTreat) %>% 
  unique() %>% 
  sort()

# stack the data 
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_occasion", 
             remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(Carriercode, "_", dt))

# make formula
indicatorStacked <- c(paste0("`", "rel_occasion_", c(-4:-1, 1:20), "`"))

##### 1.2 Impact on Delays Merger Announcement - Acquirer ##### 
# estimate the model and plot
# with controls
stack_Acquirer_Ann <- feols(lCarrierDelay ~ .[indicatorStacked] + 
                           lfp + lfleetutil + lhetero + lsparsity +
                           lEnPax + lempfte + lMKTshare +
                           lTaxiIn + lTaxiOut + lTotalFlights
                           |Carriercode^dt + OD_Dummy^dt +  
                            Year^dt + Quarter^dt,
                            cluster = "Carriercode",
                            data = stacked_data)

summary(stack_Acquirer_Ann)

# unpack
coef_acquirer_ann = broom::tidy(stack1_Acquirer, conf.int = TRUE)
write.csv(coef_acquirer_ann, file= "coef_Acquirer_Annoucement_0608.csv")

# plot
ES_stack_Acquirer_Ann
ES_stack_Acquirer_Ann <- broom::tidy(stack_Acquirer_Ann, conf.int = TRUE)[1:24,] %>%
  # add in the relative time variable
  mutate(t = c(-4:-1, 1:20)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill = factor(group)), shape = 21) + 
  scale_fill_manual(values = c("#993441", "#0029a5"))  + 
  geom_line() + 
  ggtitle("The Dynamic Effect of Airline Mergers on  \n Carrier-Induced Delays - Placebo Test") + 
  geom_errorbar(aes(ymin = conf.low + 0.03, ymax = conf.high - 0.03, color= factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Change in Carrier-Induced Delays", x = "Quarters Relative to Merger")+
  #subtitle = "Stagged Regression"#) + 
  scale_x_continuous(breaks = seq(-4, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-2, 2, by = 0.2)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

ggsave(ES_stack_Acquirer_Ann, filename = here::here("Figs_Tables", "Acquirer_Placebo.png"), 
       dpi = 500, width = 6, height = 4)


# Example of Stacked Regression
unique(stacked_data$dt)

test = stacked_data %>% filter(Carriercode == 21) %>% 
  select(Carriercode, OccasionDummy,rel_occasion, lCarrierDelay, OD_Dummy,dt) 
 
df26 = stacked_data %>% filter(dt == 26) %>% select(Carriercode)
unique(df26)


write.csv(test, file = "Table5.csv")
unique(test$rel_occasion)


# percent of delays
Essay3_OD$LateAircraftDelay_T[Essay3_OD$LateAircraftDelay_T == "-Inf"] == 0
Essay3_OD$LateAircraftDelay_T[Essay3_OD$LateAircraftDelay_T == "Inf"] == 0  

Essay3_OD$LateAircraftDelay_T == "-Inf"

Essay3_OD %>% select(CarrierDelay_T, WeatherDelay_T, NASDelay_T, 
                     SecurityDelay_T, LateAircraftDelay_T) %>%
  summarise(CarrierAll = sum(CarrierDelay_T, na.rm = T), 
            WeatherAll = sum(WeatherDelay_T, na.rm = T), 
            NASAll = sum(NASDelay_T, na.rm = T), 
            SecurityAll = sum(SecurityDelay_T, na.rm = T), 
            LateAirCraftAll = sum(LateAircraftDelay_T, na.rm = T)
            )

sum(Essay3_OD$LateAircraftDelay_T, na.rm = T)

Essay3_R = read_excel("E1.xlsx")
Essay3_R$air

DelayP = Essay3_R %>% select(aircarrierdelay, 
                    extremeweatherdelay, 
                    nasdelay, securitydelay,
                    latearrivingaircraftdelay) %>%
         summarise(CarrierAll = sum(aircarrierdelay, na.rm = T), 
            WeatherAll = sum(extremeweatherdelay, na.rm = T), 
            NASAll = sum(nasdelay, na.rm = T), 
            SecurityAll = sum(securitydelay, na.rm = T), 
            LateAirCraftAll = sum(latearrivingaircraftdelay, na.rm = T)) %>% 
        mutate(TotalDelay = CarrierAll + WeatherAll + NASAll + SecurityAll + LateAirCraftAll, 
               CarrierP = CarrierAll/TotalDelay, 
               WeatherP = WeatherAll/TotalDelay, 
               NASP = NASAll/TotalDelay, 
               SecurityP = SecurityAll/TotalDelay,
               LateAll = LateAirCraftAll/TotalDelay)



