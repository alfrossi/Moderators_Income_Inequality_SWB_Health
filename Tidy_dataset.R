
# Importing Main WVS dataset
setwd("/Users/arossisa/Documents/Task1/Data/")

load("WVS_TimeSeries_R_v1_5.rdata")

wvs_data = WVS_TimeSeries_R_v1_5
wvs_data$S001=3

# Wave 7 (forthcoming 2020)
# Wave 6 (2010-2014)
# Wave 5 (2005-2009)
# Wave 4 (1999-2004)
# Wave 3 (1995-1998)
# Wave 2 (1990-1994)
# Wave 1 (1981-1984)

# Removing the old dataset to open memory space
rm(WVS_TimeSeries_R_v1_5)

# Importing EVS old waves and current wave
library(misty)

evs2017 = read.sav("/Users/arossisa/Documents/ZA7505_v1-0-0.sav")
evs2017$S001=evs2017$study
correct_base=evs2017$study

# library(haven)
# evs2017 <- read_sav("/Users/arossisa/Documents/ZA7500_v4-0-0.sav")

evs_old = read.sav("/Users/arossisa/Documents/ZA4804_v3-1-0.sav")
evs_old$S001=2
table(evs_old$s002vs)

# Wave 7 (forthcoming 2020)
# Wave 6 (2010-2014)
# Wave 5 (2005-2009)
# Wave 4 (1999-2004)
# Wave 3 (1995-1998)
# Wave 2 (1990-1994)
# Wave 1 (1981-1984)

setwd("/Users/arossisa/Documents/Task1/Data/")
dict_evs = read.csv("countries_codes_and_coordinates.csv")
dict_evs = dict_evs[,1:2]
names(dict_evs)[1] = "Label"
dict_evs$Alpha.2.code = gsub(" \\\"|\\\"","",dict_evs$Alpha.2.code)

library(tidyverse)
evs_old2 = evs_old %>% group_by(s002vs,S009) %>% tally() 
evs_old2 = evs_old2 %>% arrange(S009,s002vs)
  
evs_old2 = right_join(dict_evs,evs_old2, by = c("Alpha.2.code" = "S009"))

evs20172 = evs2017 %>% group_by(wave,cntry_AN) %>% tally()
evs20172 = evs20172 %>% arrange(cntry_AN)

evs_old2 = right_join(dict_evs,evs_old, by = c("Alpha.2.code" = "S009"))

evs20172 = right_join(dict_evs,evs20172, by = c("Alpha.2.code" = "cntry_AN"))

setwd("/Users/arossisa/Documents/Task1/Results/")

##################################
#### Merging EVS 2017 and WVS ####
##################################

# Importing dictionary and changing evs2017 variables names to WVS 7wave standard
library(readxl)
dict_evs2017 <- read_excel("~/Documents/Task1/Data/ZA7505_VariableCorrespondence.xlsx", 
                           sheet = "EVS_WVS_Variable_Correspondence")

column_evs = as.vector(dict_evs2017[,7])

names(column_evs) = "vector_evs"
vector_evs = dplyr::pull(column_evs, vector_evs)

names(evs2017) = vector_evs

# Importing dictionary and changing WVS and evs old variables names to WVS 7wave standard

dict_wvs <- read_excel("~/Documents/Task1/Data/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", 
                       skip = 1)

column_wvs = as.vector(dict_wvs[,7])

names(column_wvs) = "vector_wvs"
vector_wvs = dplyr::pull(column_wvs, vector_wvs)

column_wvs_evs = as.vector(dict_wvs[,2])

names(column_wvs_evs) = "vector_wvs_evs"
vector_wvs_evs = dplyr::pull(column_wvs_evs, vector_wvs_evs)

final_dict = data.frame(Wave7 =vector_wvs ,Variable = vector_wvs_evs)

for (i in 1:ncol(evs2017)) {
  if(sum(which(names(evs2017)[i]==final_dict[,1]))>0){
  names(evs2017)[i] = final_dict[which(names(evs2017)[i]==final_dict[,1]),2]
  }
}

evs2017$S002 = 7
evs2017$S001 = correct_base


library(tidyverse)

wvs_evs2017 = bind_rows(wvs_data,evs2017)

table(wvs_evs2017$S001,wvs_evs2017$S002)

#################################
#### Merging EVS old and WVS ####
#################################

library(plyr)

wvs_evs2017$s002vs = wvs_evs2017$S002

#cd ~
#touch .Renviron
#open .Renviron
#R_MAX_VSIZE=100Gb 

wvs_evs = bind_rows(wvs_evs2017,evs_old)

table(wvs_evs$S001,wvs_evs$s002vs)

#############################
#### Removing Duplicates ####
#############################

wvs_evs$country_year = paste(wvs_evs$s002vs,wvs_evs$S003)

wvs_evs$S002 = wvs_evs$s002vs

dt = wvs_evs[with(wvs_evs,ave(S001,country_year,FUN = max)==S001),]

dt$Year = as.factor(dt$S002)
levels(dt$Year) = c("Wave 1 (1981-1984)",
                    "Wave 2 (1990-1994)",
                    "Wave 3 (1995-1998)",
                    "Wave 4 (1999-2004)",
                    "Wave 5 (2005-2009)",
                    "Wave 6 (2010-2014)",
                    "Wave 7 (2017-2021)")


table(dt$S001,dt$S002)

table(dt$S001,dt$country_year)

###############################

setwd("/Users/arossisa/Documents/Task1/Data/")
dict_evs = read.csv("countries_codes_and_coordinates.csv")
dict_evs = dict_evs[,c(1,4)]
dict_evs$Numeric.code = as.numeric(gsub(" \\\"|\\\"","",dict_evs$Numeric.code))

names(dict_evs)[1] = "Label"

library(tidyverse)

dt2 = dt %>% group_by(S003,Year) %>% tally()
dt2 = dt2 %>% arrange(S003,Year)

dt2 = right_join(dict_evs,dt2, by = c("Numeric.code" = "S003"))

setwd("/Users/arossisa/Documents/Task1/Results/")


write_csv(dt,"database.csv")
saveRDS(dt, file = "database.rds")

##############################################################
####### Merging the names of the country and exporting ######
##############################################################

library(readxl)
contry <- na.omit(read_excel("~/Documents/Task1/Data/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", 
                             sheet = "S003 ISO Codes"))
names(contry) = c("S003","Country")
contry$S003 = as.integer(contry$S003)
contry = na.omit(contry)

control = data.frame(WAVE = dt$s002vs,EVS_WVS = dt$S001,S003 = dt$S003)

country_wave_WVS_EVS = right_join(contry,control, by= "S003")
unique_rows <- !duplicated(country_wave_WVS_EVS[c("S003","WAVE","EVS_WVS")])

country_wave_WVS_EVS <- country_wave_WVS_EVS[unique_rows,]

write_csv(country_wave_WVS_EVS,"country_wave_WVS_EVS.csv")

#################################
###### Running empty model ######
#################################

summary(lmer(A008 ~ 1 + (1 | country_year), data = dt))


#############################################################

library(readxl)
dict <- read_excel("~/Documents/Task1/Data/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", 
                   sheet = "S021 Country - wave - study - s", 
                   skip = 1)

names(dict)[1] = "S021"
aux = right_join(dict,dt,by="S021")

aux = aux[!is.na(aux$Label),]

aux2 = aux[aux$Label == "Brazil [1991]",]





