library(tidyverse)
library(openxlsx)

# Load the data 
# Alkanes 
alkanes <- read.xlsx("KI_upload_template.xlsx", sheet = 1) %>% 
  filter(!is.na(carbons) & !is.na(RT_seconds)) # name of the alkane can be empty - its not used in the script

# Compounds of interest
compounds_of_interest <- read.xlsx("KI_upload_template.xlsx", sheet = 2) %>% 
  filter(!is.na(compounds_of_interest) & !is.na(RT_seconds))


# Plot retention time progression of the alkane series
ggplot(alkanes,aes(x = carbons,  y = RT_seconds))+
  geom_point()+
  geom_line()


###########################################################
# Set the N-1 / N+1 points from your compound of interest #
###########################################################

# RT1 = Retention time of the first alkane before "my.rt" 
# RT2 = Retention time of the first alkane after "my.rt" 
# N = The number of carbons of the alkane that comes after "my.rt"

fun.KI <- function(my.rt){ 
  RT1 = alkanes %>% filter(RT_seconds < my.rt) %>% tail(1) %>% .$RT_seconds
  RT2= alkanes %>% filter(RT_seconds > my.rt) %>% head(1) %>% .$RT_seconds
  N = alkanes %>% filter(RT_seconds > my.rt) %>% head(1) %>% .$carbons

  KI = round((100 * N)+100*((my.rt - RT1)/(RT2 - RT1)))
  
  return(KI)
}

# Add KI for every RT in the list of compounds of interest 
compounds_of_interest_with_KI <- 
compounds_of_interest %>% 
  mutate(calculated_KI = lapply(RT_seconds, fun.KI))

# Write .xlsx file
write.xlsx(compounds_of_interest_with_KI,
           file =  "output_compounds_with_KI.xlsx")


############################################
# Extra - Plot alkenes series vs. their KI #
############################################

alkanes %>% 
  mutate(KI_of_alkane = as.numeric(lapply(RT_seconds, fun.KI))) %>%
  drop_na(KI_of_alkane) %>%
  ggplot(aes(x = carbons, y = KI_of_alkane)) +
  geom_point()+
  geom_line()
