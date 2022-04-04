# libraries
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(wesanderson)
library(scales)
#devtools::install_github("ropenscilabs/ochRe")
library(ochRe)
#devtools::install_github("awhstin/awtools")
library(awtools)

# read in continental data
population_dataframe <- read.csv("/Volumes/FLEUR_DATA/Baobab/tZonal_stats/continental/coverage_continental.csv")[3:25] # skip first columns indicating rownumbers
healthfacilities <- st_read("Dropbox/PhD GeoHealth - Fleur/Projects/2020-Population grids/Analyses/ssa/data/raw/health facilities/suhsharan_health_facilities/sub-saharan_health_facilities.shp")
admin0 <- st_read("Dropbox/PhD GeoHealth - Fleur/Projects/2020-Population grids/Analyses/ssa/data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
admin1 <- st_read("Dropbox/PhD GeoHealth - Fleur/Projects/2020-Population grids/Analyses/ssa/data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm1.shp")
admin2 <- st_read("Dropbox/PhD GeoHealth - Fleur/Projects/2020-Population grids/Analyses/ssa/data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm2.shp")


# to rename the time variables
time_new <- c("X30" = 30, "X60" = 60, "X90" = 90, "X120" = 120, "X150" = 150, "X180" = 180)

# start with transformation of dataframe 
# bring wide dataframe to long format
population_dataframe_long <- population_dataframe %>%
  mutate(GID_2 = ifelse(is.na(GID_2), GID_1, GID_2), 
         NAME_2 = ifelse(is.na(NAME_2), NAME_1, NAME_2)) %>%
  select(-c(adm)) %>%
  # gather("variable", "value", totpop:barrierper) %>%
  # na.omit() %>%
  # spread(variable, value) %>%
  gather("time", "popcov", c("X30", "X60", "X90", "X120", "X150", "X180")) %>%
  select(-c("X120per", "X60per", "X30per", "X90per", "X150per", "X180per", "barrierper")) %>% 
  mutate(time= recode(time, !!!time_new)) %>%
  mutate(perc_cov = (popcov/totpop)*100,
         perc_barrier = (barrier/totpop) *100,
         label = ifelse(dataset == "ghs", paste0("GHS-POP"), 
                        ifelse(dataset == "gpwv4",  paste0("GPWv4"), 
                               ifelse(dataset == "landscan",  paste0("Landscan"),
                                      ifelse(dataset == "wpcon",  paste0("WorldPop:\nconstrained"),
                                             ifelse(dataset == "wpun",  paste0("WorldPop:\nunconstrained"), 
                                                    ifelse(dataset == "facebook",  paste0("Facebook"),"")))))))


# filter out countries not captured in analysis
population_dataframe_long1 <- filter(population_dataframe_long, 
                                     !NAME_0 %in% c("Egypt", "Algeria", "Morocco", 
                                                    "Libya", "Tunisia", "Western Sahara"))
# for facebook remove South Sudan, Sudan and Ethiopia (missing data)
population_dataframe_long2 <- filter(population_dataframe_long1,
                                     !(GID_0 %in% c("SSD", "SDN", "ETH") & 
                                        dataset == "facebook"))

# create averages
population_mean <- population_dataframe_long2 %>%
  dplyr::group_by(dataset, time) %>%
  summarise(sum_cov = sum(popcov), 
            totpop = sum(totpop), 
            barrierpop = sum(barrier), 
            min = min(perc_cov), 
            max = max(perc_cov)) %>%
  mutate(perc_cov = (sum_cov/totpop)*100, 
         time = as.numeric(time), 
         perc_barrier = (barrierpop/totpop)*100,
         label = ifelse(dataset == "ghs", paste0("GHS-POP", "\nTotal population: ", format(round(totpop), big.mark=","), "\nOn barrier: ", round(perc_barrier), "% "), 
                        ifelse(dataset == "gpwv4",  paste0("GPWv4", "\nTotal population: ", format(round(totpop), big.mark=","), "\nOn barrier: ", round(perc_barrier), "% "), 
                               ifelse(dataset == "landscan",  paste0("Landscan", "\nTotal population: ", format(round(totpop), big.mark=","), "\nOn barrier: ", round(perc_barrier), "% "),
                                      ifelse(dataset == "wpcon",  paste0("WorldPop constrained", "\nTotal population: ", format(round(totpop), big.mark=","), "\nOn barrier: ", round(perc_barrier), "% "),
                                             ifelse(dataset == "wpun",  paste0("WorldPop unconstrained", "\nTotal population: ", format(round(totpop), big.mark=","), "\nOn barrier: ", round(perc_barrier), "% "),
                                                    ifelse(dataset == "facebook",  paste0("Facebook", "\nTotal population: ", format(round(totpop), big.mark=","), "\nOn barrier: ", round(perc_barrier), "% "), "")))))))

# labels for plotting
# datasets <- c("ghs", "gpwv4", "landscan", "wpcon", "wpun")
# dataset_labels <- c("GHS-POP", "GPWv4", "Landscan", "WorldPop:\nconstrained", "WorldPop:\nunconstraied")

# PLOTTING ABSOLUTE NATIONAL COVERAGE 
plot1 <- ggplot(population_mean, aes(time, sum_cov, color  = label, group = label)) +
  geom_point() +
  geom_line(size = 1) + 
  scale_color_manual(values = a_palette, name = "")  +
  scale_fill_manual(values = a_palette, name = "") +
  # scale_color_manual(labels = dataset_labels, values = wes_palette("Royal2", n = 6)) +
  # scale_color_manual(labels = dataset_labels, values = wes_palette("Royal2", n = 6), name = "Gridded\npopulation\ndataset") +
  scale_x_continuous(breaks=c(0, 30, 60, 90, 120, 150, 180)) +
  # scale_y_continuous(labels = comma, breaks = seq(0, max(population_mean$sum_cov)+100000000, 200000000), limits = c(0, max(population_mean$sum_cov) + 1000000), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, max(population_mean$sum_cov)+100000000, 200000000), limits = c(0, max(population_mean$sum_cov) + 1000000), labels = c("0", "200", "400", "600", "800", "1000"), expand = c(0,0)) +
  xlab("Travel time (minutes)") + ylab("Total population covered (millions)") +
  theme_bw() +
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title= paste0(population_mean$NAME_0))

# ochre_palettes
palette <-  a_palette

# PLOTTING RELATIVE COVERAGE
plot2 <- ggplot(population_mean, aes(time, perc_cov, color = label, group = label)) +
  #geom_ribbon(aes(ymin = min, ymax = max, fill = label), size = 0.1, alpha = 0.1) + # all lines overlap
  geom_line(size = 0.8) + 
  geom_point(size = 1.3) +
  scale_x_continuous(breaks=c(0, 30, 60, 90, 120, 150, 180)) +
  scale_y_continuous(limits =c(0, 100), expand = c(0, 0)) +
  scale_color_manual(values = a_palette, name = "")  +
  scale_fill_manual(values = a_palette, name = "") +
  # scale_color_manual(values = wes_palette("Royal2", n = 5), name = "Population dataset:")  +
  # scale_fill_manual(values = wes_palette("Royal2", n = 5), name = "Population dataset:") +
  # scale_fill_manual(values = palette, name = "Population dataset:") +
  # scale_color_manual(values = palette, name = "Population dataset:") +
  xlab("Travel time (minutes)") + ylab("Total population covered (%)") +
  theme_bw() +
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5 , "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title= paste0(population_mean$NAME_0))

# function to increase vertical spacing between legend items
# found at: https://rpubs.com/TX-YXL/707491 
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.8, "npc"),
    height = grid::unit(0.8, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

# register function throughout R session
GeomBar$draw_key = draw_key_polygon3

library(ggpubr)
ggarrange(plot1, plot2, ncol = 2, common.legend = T)  


