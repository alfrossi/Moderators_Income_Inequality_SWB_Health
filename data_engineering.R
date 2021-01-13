
# Importing dataset
setwd("/Users/arossisa/Documents/Task1/Results/")
dt = readRDS("database.rds")


##########

library(tidyverse)

non_missing_data_participants = dt %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(!is.na(.))))

hist(t(non_missing_data_participants), xlab = "Participants", main = "")

non_missing_data_country = NULL

for (i in 1:ncol(dt)) {
  aux = na.omit(dt[,c(which(names(dt)=="S003"), i)])
  non_missing_data_country = c(non_missing_data_country,
                              length(unique(aux[,1])))
  print(i)
}
hist(non_missing_data_country, xlab = "Country", main = "")

non_missing_data_country_year = NULL

for (i in 1:ncol(dt)) {
  aux = na.omit(dt[,c(which(names(dt)=="country_year"), i)])
  non_missing_data_country_year = c(non_missing_data_country_year,
                              length(unique(aux[,1])))
  print(i)
}

hist(non_missing_data_country_year, xlab = "Country_Year", main = "")


non_missing_data_country_year2 = NULL

for (i in 1:ncol(dt)) {
  aux = na.omit(dt[,c(which(names(dt)=="country_year"), 
                      which(names(dt)=="S003"), i)])
  aux2 = unique(aux[c(1, 2)])
  aux3 = aux2[duplicated(aux2$S003),]
  aux4 = length(unique(aux3$S003))

  non_missing_data_country_year2 = c(non_missing_data_country_year2,
                                   aux4)
  print(i)
}


hist(non_missing_data_country_year2, xlab = ">= 2 waves", main = "")

###############

data = cbind(add_rownames(as.data.frame(t(non_missing_data_participants)),"Variable"),
             non_missing_data_country_year,non_missing_data_country,
             non_missing_data_country_year2)
names(data) = c("Variable","Participants", "country_Year","Country",">= 2 waves")

data = data %>% mutate(Participants_per_Country = Participants / Country,
                       Participants_per_CountryYear = Participants / country_Year)

write_csv(data,"Variables_CountLevels.csv")


#####################
###### Topic D ###### 
#####################

library(readxl)
dict <- read_excel("~/Documents/Task1/Data/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", 
                   skip = 1)

dict <- dict[,c(1:6)]

data2 = data[,1:5]
names(data2) = c("Variable","N","K","L","M")

dict <- left_join(dict,data2,by = "Variable")

dict2 = dict

dict2$Categories_old = dict$Categories
dict2$Categories_new = dict$Categories

dict2$Categories_new = gsub("-[0-9] |See Annex|Inappropiate|Not have done|Unknown|Not available|  Missing; Unknown\r\n-4  Not asked in survey\r\n-3  Not applicable\r\n-2  No answer\r\n-1  Don´t know\r\n","",dict2$Categories_new)
dict2$Categories_new = gsub("\\r\\n|na (survey break-off)|Not asked|Missing|Don´t know|Missing; Unknown|Don't know|No answer|Not applicable|Not asked in survey","",dict2$Categories_new)
dict2$Categories_new = gsub("-10 multiple answers Mail|no follow-up|follow-up|non response|matrix not applied|na \\(survey break-off\\)","",dict2$Categories_new)
dict2$Categories_new = gsub("\\s+"," ",dict2$Categories_new)
dict2$Categories_new = gsub(";|\\{|\\}|No spouse|\\/partner|No formal education|Not occupied|-99 missing|-99","",dict2$Categories_new)
dict2$Categories_new = gsub("\\s+"," ",dict2$Categories_new)


library(fBasics)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

dt2 = NULL
for (i in 1033:ncol(dt)) {
  dt[,i] = as.character(dt[,i])
}

for (i in 1:ncol(dt)) {
  aux = data.frame(Variable = names(dt)[i],
                   QTT_dif_responses = length(unique(dt[,i])),
                   Possible_responses = paste(sort(unique(dt[,i])),collapse = "-"),
                   Mean=mean(dt[,i],na.rm=T),
                   SD=sd(dt[,i],na.rm=T),
                   Q1=as.numeric(summary(dt[,i],na.rm=T)[2]),
                   Median=median(dt[,i],na.rm=T),
                   Q3=as.numeric(summary(dt[,i],na.rm=T)[5]),
                   Mode_ = Mode(dt[,i]),
                   Kurtosis=kurtosis(dt[,i],na.rm=T),
                   Skewness=skewness(dt[,i],na.rm=T)
                  )
  dt2 = rbind(dt2,aux)
  print(i)
}

dt2$Possible_responses[dt2$QTT_dif_responses>150] = NA
  
dt3 = dt2

dict_partial <- left_join(dict2,dt2,by = "Variable")

library(WriteXLS)
WriteXLS(dict_partial,"dictionary_partial.xlsx")

dict_partial_others <- left_join(data,dt2,by = "Variable")
WriteXLS(dict_partial_others,"dictionary_partial_others.xlsx")



###################################
############# Topic F #############
##### Merging WVS/EVS with WB #####
###################################

library(readxl)
setwd("~/Documents/Task1/Data/")
WB_data <- read_excel("WB_data.xlsx")
WB_data <- WB_data[1:217,]
#View(WB_data)

key_merge = read_excel("countries_codes_and_coordinates2.xlsx")
key_merge = key_merge[,3:4]
#key_merge$Numeric.code = as.numeric(gsub(" \\\"|\\\"","",key_merge$Numeric.code))
#key_merge$`Country Code` = gsub(" \\\"|\\\"","",key_merge$Alpha.3.code)

WB_data2 = left_join(WB_data,key_merge,by = c("Country Code"="Alpha3"))

sum(is.na(WB_data2$Numeric.code))
# Confirming that every country have a number

WB_data2 = WB_data2[,5:ncol(WB_data2)]

WB_data3 = WB_data2 %>%
            pivot_longer(!Numeric, names_to = "Year", values_to = "GDP")

WB_data3$Year = as.numeric(str_remove(WB_data3$Year, " \\[.*"))

dt = left_join(dt,WB_data3, by= c("S003"="Numeric","S020"="Year"))

setwd("~/Documents/Task1/Results/")
write_csv(dt,"database.csv")
saveRDS(dt, file = "database.rds")

#######################################
############### Topic G ###############
##### Specification curve analysis ####
#######################################


