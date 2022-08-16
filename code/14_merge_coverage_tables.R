##' Title: Merge coverage tables
##' Description: In this R-script we merge the different national coverage tables 
##' in one overarching continental table for all datasets
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva


# libraries
library(dplyr)
library(data.table)
library(sf)
library(purrr)

# read in data sets
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
facebook <- c(list.files("tZonal_stats/dataset/facebook/", recursive = T, full.names = T))
gpwv4 <- c(list.files("tZonal_stats/dataset/gpwv4/", recursive = T, full.names = T))
ghs_pop <- c(list.files("tZonal_stats/dataset/ghs_pop/", recursive = T, full.names = T))
worldpop_unconstr <- c(list.files("tZonal_stats/dataset/worldpop_unconstr/", recursive = T, full.names = T))
worldpop_constr <- c(list.files("tZonal_stats/dataset/worldpop_constr/", recursive = T, full.names = T))
landscan <- c(list.files("tZonal_stats/dataset/landscan/", recursive = T, full.names = T))
admin0 <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")

# create an empty list to store all the data in 
list_outbound <- list()
list_national <- list()

# for loop to bind the columns of each individual country
for (i in 1:length(admin0$GID_0)) {
  
  # read the csv files into R
  fb <- read.csv(facebook[i])
  gpw <- read.csv(gpwv4[i])
  ghs <- read.csv(ghs_pop[i])
  wp_un <- read.csv(worldpop_unconstr[i])
  wp_con <- read.csv(worldpop_constr[i])
  ls <- read.csv(landscan[i])
  
  # first try a row bind
  list_df <- list(gpw, ghs, wp_un, wp_con, ls, fb) 
  
  list_df1 <- lapply(list_df, function(x) setNames(x, gsub(".*\\_", "", names(x)))) # make sure that all lists have same name structure of columns
  
  list_df1 <- list_df1[sapply(list_df1, function(x) dim(x)[1]) > 0] # remove empty list items in case country does not have population data (especially for HRSL data)
  list_df1 <- lapply(seq(list_df1), function(x) "[[<-"(list_df1[[x]], paste0("dataset"), value = x)) # create new variable for each dataframe that indicates the dataset as an integer, values represent sequential order of first list element
  
  total <- do.call(rbind, list_df1) # bind all list items together
  levels <- list("1" = "gpwv4", "2" = "ghs", "3" = "wpun", "4" = "wpcon", "5" = "landscan", "6" = "facebook") 
  total <- total %>% mutate(dataset = recode(dataset, !!!levels)) # give numerical values the dataset levels as indicated above
  colnames(total)[1:8] <- c("X", "adm", "GID_0", "NAME_0", "GID_1", "NAME_1", "GID_2", "NAME_2") # give first 8 columns their original name
  
 # summarize points that fall out of bounds
  outofbounds <- total %>%
    filter(is.na(GID_1)) %>% #V5 indicates the admin boundary, if it's NA pop falls out of boundary
    mutate(country = admin0$GID_0[i]) %>%
    group_by(country, dataset) %>%
    summarise(population = sum(totpop)) # V9 represents total population
  
  list_outbound[[i]] <- outofbounds # create a list to calc the number of people falling out of boundaries
  
  write.csv(total, paste0("tZonal_stats/national/coverage_", admin0$GID_0[i], ".csv"))

  list_national[[i]] <- total # create a list to for each country
}


continental <- do.call(rbind, list_national)
outofbounds_df <- do.call(rbind, list_outbound)

write.csv(continental, "tZonal_stats/continental/coverage_continental.csv")
write.csv(outofbounds_df, "tZonal_stats/outside_boundaries/outside_boundaries.csv")
