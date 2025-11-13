#Analysing the relationship between heat and violence:


#VACS countries code:

#load the libraries 
library(here)
library(tidyverse)
library(data.table)
library(patchwork)
library(survey)
library(dplyr)
library(terra)
library(sf)
library(raster)
library(readxl)
library(stringr)

#-------------------First: preparing shapefiles-----------#


#1-LESOTHO
#load the dfLsoset
dfLso <- read_dta("Data/Lesotho/Lesotho VACS Public Use Data Package/lesotho_pud_rurban.dta")

#renaming the district variable in dfLso to region:
dfLso <- dfLso %>%
  rename(region = District)
dfLso$region

#create a country column in dfLso
dfLso$country <- "Lesotho"
table(dfLso$country)

#load the Lesotho shapefile
Lesotho_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Lesotho/shps/sdr_subnational_boundaries.shp")

#harmonise region variable across shapefile and dataset
Lesotho_shape <- Lesotho_shape %>%
  rename(region = DHSREGEN)

# # #rename country var in lesotho_shape
Lesotho_shape <- Lesotho_shape %>%
  rename(country = CNTRYNAMEE)

#change Butha_Butha name to match dfLso Botha-Botha:
Lesotho_shape <- Lesotho_shape %>%
  mutate(region = if_else(region == "Butha-Buthe", "Botha-Botha", region))

Lesotho_shape$region
table(dfLso$region)
#merge dfLso with Lesotho Shape:
merged_lso <- left_join(Lesotho_shape, dfLso, by = "region", "country")

#urban rural variable PERIURBAN was converted to Rural.
dfLso <- dfLso %>%
  rename(rural_urban = sett)

dfLso$rural_urban <- ifelse(dfLso$rural_urban %in% c(2, 3), "2",
                            ifelse(dfLso$rural_urban == 1, "1", NA))

table(dfLso$rural_urban, useNA = "always")

#check regional identifiers in dfLso and Lesotho_shape
table(Lesotho_shape$region)
table(dfLso$region)

#2-NAMIBIA
#load the dataset
dfNmb <- read_dta("Data/Namibia/namibia_pubuse_092923 (1).dta")

#Load the Namibia shapefile
Namibia_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Namibia/shps/sdr_subnational_boundaries.shp")

# Create a lookup table for the replacements
region_replacements <- c(
  "Zambezi - Rural"    = "caprivi",
  "Zambezi - Urban"    = "caprivi",
  "Oshikoto - Rural"   = "oshikoto",
  "Oshikoto - Urban"   = "oshikoto",
  "Oshana - Urban"     = "oshana",
  "Oshana - Rural"     = "oshana",
  "Omusati - Rural"    = "omusati",
  "Omusati - Urban"    = "omusati",
  "Ohangwena - Rural"  = "ohangwena",
  "Ohangwena - Urban"  = "ohangwena",
  "Kunene - Urban"     = "kunene",
  "Kunene - Rural"     = "kunene",
  "Khomas - Urban"     = "khomas",
  "Khomas - Rural"     = "khomas",
  "Kavango West - Rural" = "kavango",
  "Kavango East - Rural" = "kavango",
  "Kavango West - Urban" = "kavango",
  "Kavango East - Urban" = "kavango",
  "Hardap - Rural"     = "hardap",
  "Hardap - Urban"     = "hardap",
  "Erongo - Urban"     = "erongo",
  "Erongo - Rural"     = "erongo",
  "!Karas - Rural"     = "karas",
  "Otjozondjupa - Rural" = "otjozondjupa",
  "Otjozondjupa - Urban" = "otjozondjupa",
  "Omaheke - Urban"    = "omaheke",
  "Omaheke - Rural"   =  "omaheke",
  "!Karas - Urban"     = "karas"
)

# Replace the values in the 'region' column based on the lookup table
# dfNmb$region <- recode(dfNmb$strata, !!!region_replacements)
dfNmb$strata <- region_replacements[dfNmb$strata]
table(dfNmb$strata)

#create a new column called "region" from the strata column
dfNmb$region <- dfNmb$strata

table(dfNmb$strata)
table(dfNmb$region)
#-----change column name in namibia_shape to match dfNmb
Namibia_shape <- Namibia_shape %>%
  rename (region = REGNAME)
# # #rename country var in namibia_shape
Namibia_shape <- Namibia_shape %>%
  rename(country = CNTRYNAMEE)

#Merge the dfNmb and Namibia_shape together by region column 
# merged_Nmb <- left_join(dfNmb, Namibia_shape, by = "region")
table(Namibia_shape$region)
table(dfNmb$strata)
#Now the dfNmb and Namibia_shape are joined by common identifier "region"

#introduce rural urban variables to dfNmb:

#1 load rural urban variables:
ruralurban1 <- read_xlsx("Data/Namibia/Namibia EAs.xlsx")

#2 harmonise the name of the region column in rural urban dataframe as well as dfNmb dataframe:
ruralurban1 <- ruralurban1 %>%
  rename(ea = EA)
#3 merge rural and urban variable with region variable:
dfNmb <- left_join(dfNmb, ruralurban1, by = "ea")

#now a new column in dfNmb called Type was created showing whether participant is residing in rural or urban
print(dfNmb$Type)

#rename Type.x to ruralurban variable:
dfNmb <- dfNmb %>%
  rename (rural_urban = Type)
table(dfNmb$rural_urban)
#create a rural_urban variable from Type column 
dfNmb$rural_urban <- ifelse(dfNmb$rural_urban == "Rural", 2,
                            ifelse(dfNmb$rural_urban == "Urban", 1, NA))

table(dfNmb$rural_urban, useNA = "always")

#check regional identifiers in dfNmb and Namibia_shape
table(dfNmb$region)
table(Namibia_shape$region)

#3-ZIMBABWE

#load the dataset
dfZmb <- read_dta("Data/Zimbabwe/Zimbabwe VACS Public Use Data Package/Zimbabwe VACS Public Use Data Package/Zimbabwe VACS Public Use Data Package/zimbabwe_pud_rurban.dta")

#load the shapefile
Zimbabwe_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Zimbabwe/shps/sdr_subnational_boundaries.shp")

#change dfZmb province name to region
dfZmb <-  dfZmb %>%
  rename(region = province)

#change DHSREGEN in Zimbabwe_shape to region
# and harmonise region names 
Zimbabwe_shape <- Zimbabwe_shape %>%
  rename(region = DHSREGEN) %>%
  mutate(region = case_when(
    region == "Mashonaland Central" ~ "Mash Central",
    region == "Mashonaland East" ~ "Mash East",
    region == "Mashonaland West" ~ "Mash West",
    region == "Matabeleland North" ~ "Mat North",
    region == "Matabeleland South" ~ "Mat South",
    region == "Harare Chitungwiza" ~ "Harare",
    TRUE ~ region
  ))

Zimbabwe_shape <- Zimbabwe_shape %>%
  rename(country = CNTRYNAMEE)

#check if changed and correct:
table(Zimbabwe_shape$region)
table(dfZmb$region)
#merge zimbabwe_shape and dfZmb by region
merged_zmb <- left_join(Zimbabwe_shape, dfZmb, by = "region")
table(merged_zmb$region)

#rural urban variable 
dfZmb <- dfZmb %>%
  mutate(rural_urban1 = ifelse(region %in% c("Harare", "Bulawayo"), "Urban", "Rural"))
table(dfZmb$rural_urban1)

#recode rural_urban to 1 and 2 
dfZmb <- dfZmb %>%
  mutate(rural_urban = ifelse(rural_urban1 == "Urban", 1, 2))
table(dfZmb$rural_urban)

#check regional identifiers in dfZmb and Zimbabwe_shape
table(dfZmb$region)
table(Zimbabwe_shape$region)

table(dfZmb$rural_urban, useNA = "always")
#----------------------------
#4-MOZAMBIQUE
#load the dataset:
dfMoz <- read_dta("Data/Mozambique/Mozambique VACS Public Use Dataset/mozambique_pud_rurban.dta", encoding = "latin1")

#Load the Mozambique shapefile
Mozambique_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Mozambique/shps/sdr_subnational_boundaries.shp")

#-----change prov column name in dfMoz to region
dfMoz <- dfMoz %>%
  rename(region = prov)
print(dfMoz$region)

#-----change column name in Mozambique_shape to match dfMoz
Mozambique_shape <- Mozambique_shape %>%
  rename (region = DHSREGEN)
Mozambique_shape <- Mozambique_shape %>%
  rename(country = CNTRYNAMEE)

# Define a vector of district names in the desired order
district_names <- c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica",
                    "Inhambane", "Gaza", "Maputo Provincia", "Maputo Cidade")

# Rename the numerical values in the 'prov' column
dfMoz$region <- factor(dfMoz$region, levels = 1:10, labels = district_names)
table(dfMoz$region, useNA = "always")

#Merge the dfMoz and Mozambique_shape by region
# merged_moz <- left_join(dfMoz, Mozambique_shape, by = "region")

print(dfMoz$region)

#renaming rural_urban variable: 
dfMoz <- dfMoz %>%
  rename(rural_urban = Type_Name)
table(dfMoz$rural_urban)

#create a rural_urban variable from Type column 
dfMoz$rural_urban <- ifelse(dfMoz$rural_urban == "Rural", 2,
                            ifelse(dfMoz$rural_urban == "Urban", 1, NA))

table(dfMoz$rural_urban, useNA = "always")

#check any inconcsistencies with the region names
table(Mozambique_shape$region)
table(dfMoz$region)

#harminise the different names:
# Rename "Maputo Cidade" to "Maputo City" in the region column
library(dplyr)

dfMoz <- dfMoz %>%
  mutate(region = dplyr::recode(region, "Maputo Cidade" = "Maputo City"))

# Rename "Maputo Provincia" to "Maputo" in the region column
dfMoz <- dfMoz %>%
  mutate(region = dplyr::recode(region, "Maputo Provincia" = "Maputo"))

Mozambique_shape <- Mozambique_shape %>%
  dplyr::filter(!region %in% c("Sofala", "Inhambane"))

dfMoz <- dfMoz %>%
  dplyr::filter(!region == "Inhambane")

#check any inconcsistencies with the region names
table(Mozambique_shape$region)
table(dfMoz$region)

#----------------------------
# #5-MALAWI
dfMlw <- haven::read_dta(here("/Users/bothainaeltigani/Dropbox/2023_Violence against children/Data/Malawi/Malawi VACS Public Use Data Package/Malawi Public Use Data Files/malawi_pubuse_Female_04072020.dta"))
dfMlw2 <- haven::read_dta(here("/Users/bothainaeltigani/Dropbox/2023_Violence against children/Data/Malawi/Malawi VACS Public Use Data Package/Malawi Public Use Data Files/malawi_pubuse_male_04072020.dta"))

# #Load the Malawi shapefile
Malawi_shape <- read_sf("/Users/bothainaeltigani/Dropbox/2023_Violence against children/Data/Shapefiles/Malawi regional/shps/sdr_subnational_boundaries.shp")
# 
#change name of DHSREGEN in Malawi_shaoe to region
Malawi_shape <- Malawi_shape %>%
  rename(region = DHSREGEN)

#create country column in malawi shape:
Malawi_shape <- Malawi_shape %>%
  mutate(country = "Malawi")

# #change name of dist variable in Malawi_shape to region
dfMlw <- dfMlw %>%
  rename(region = Region_Name)

# #change name of dist variable in Malawi_shape to region
dfMlw2 <- dfMlw2 %>%
  rename(region = Region_Name)


# # Rename the numerical values in the 'dist' column in dfMlw
# dfMlw$region <- factor(dfMlw$region, levels = 1:3, labels = district_names_mlw)

print(dfMlw$region)

#create rural_urban variable
dfMlw <- dfMlw %>%
  mutate(rural_urban = ifelse(Urbanization == "Urban", 1,
                              ifelse(Urbanization =="Rural", 2, NA)))
table(dfMlw$rural_urban)

dfMlw2 <- dfMlw2 %>%
  mutate(rural_urban = ifelse(Urbanization == "Urban", 1,
                              ifelse(Urbanization =="Rural", 2, NA)))
table(dfMlw2$rural_urban)

#merge the dfmlw and Malawi_shape by region
mlw_df_shape <- left_join(dfMlw, Malawi_shape, by = "region")

# #----------------------------
#6-ZAMBIA

#load the dataset
dfZam <- read_dta("Data/Zambia/Zambia VACS Public Use Data Package/zambiaf_pud_rurban.dta")

#Load the Zambia shapefile
Zambia_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Zambia/shps/sdr_subnational_boundaries.shp")

#change region column in dfZam to rural_urban
dfZam <- dfZam %>%
  rename(rural_urban = region)
dfZam <- dfZam %>%
  mutate(rural_urban = ifelse(rural_urban == 1, "Urban", "Rural"))

table(dfZam$rural_urban, useNA = "always")
#create a rural_urban variable from Type column 
dfZam$rural_urban <- ifelse(dfZam$rural_urban == "Rural", 2,
                            ifelse(dfZam$rural_urban == "Urban", 1, NA))

table(dfZam$rural_urban, useNA = "always")

#-----change prov column name in dfZam to region
dfZam <- dfZam %>%
  rename(region = prov)
table(dfZam$region)

#checking the type of the region column
column_type <- class(dfZam$region)
print(column_type)

# Define a vector of district names in the desired order
district_names <- c("Central", "Copperbelt", "Eastern", "Luapula", "Lusaka", "Muchinga",
                    "North Western", "Northern", "Southern", "Western")

# Rename the numerical values in the 'prov' column
dfZam$region <- factor(dfZam$region, levels = 1:10, labels = district_names)
table(dfZam$region)

# Convert 'region' column from numeric to integer because 'region' is numeric 
dfZam$region <- as.character(dfZam$region)

#checking the type of the edited 'region' column
column_type1 <- class(dfZam$region)
print(column_type1) #successfully changed to character from numeric

#-----change column name in zambia_shape to match dfZam
Zambia_shape <- Zambia_shape %>%
  rename (region = DHSREGEN)
table(Zambia_shape$region)

Zambia_shape <- Zambia_shape %>%
  rename(country = CNTRYNAMEE)

#REPEAT THE SAME ABOVE FOR dfZam2 (MALE DATASET)

#Load the data
dfZam2 <- read_dta("Data/Zambia/Zambia VACS Public Use Data Package/zambiam_pud_rurban.dta")
dfZam2$country <- "Zambia"

#change region column in dfZam2 to rural_urban
dfZam2 <- dfZam2 %>%
  rename(rural_urban = region)

dfZam2 <- dfZam2 %>%
  mutate(rural_urban = ifelse(rural_urban == 1, "Urban", "Rural"))

table(dfZam$rural_urban)

#create a rural_urban variable from Type column 
dfZam2$rural_urban <- ifelse(dfZam2$rural_urban == "Rural", 2,
                             ifelse(dfZam2$rural_urban == "Urban", 1, NA))

table(dfZam2$rural_urban, useNA = "always")

# #-----change prov column name in dfZam2 to region
dfZam2 <- dfZam2 %>%
  rename(region = prov)
table(dfZam2$region)

#checking the type of the region column
column_type <- class(dfZam2$region)
print(column_type)

# Define a vector of district names in the desired order
district_names <- c("Central", "Copperbelt", "Eastern", "Luapula", "Lusaka", "Muchinga",
                    "North Western", "Northern", "Southern", "Western")

# Rename the numerical values in the 'prov' column
dfZam2$region <- factor(dfZam2$region, levels = 1:10, labels = district_names)
table(dfZam2$region)

# Convert 'region' column from numeric to integer because 'region' is numeric 
dfZam2$region <- as.character(dfZam2$region)

#checking the type of the edited 'region' column
column_type1 <- class(dfZam2$region)
print(column_type1) #successfully changed to character from numeric

#-----------------------------------------------------

#7-KENYA

#load the dataset
dfKen <- read_dta("Data/Kenya/Kenya 2 VACS Public Use Data Package/Kenya 2 VACS Public Use Data Package/kenya_pud_rurban.dta")
table(dfKen_ins$county_name)
#load the shapefile
Kenya_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Kenya/shps/sdr_subnational_boundaries.shp")
Kenya_shape <- Kenya_shape %>%
  rename (region = DHSREGEN)
print(Kenya_shape$region)


#load the rural urban data
ruralurban2 <- read_xlsx("Data/Shapefiles/Rural urban/Kenya clusters_2019.xlsx")

#harmonise 'clust' in dfKen and ruralurban2
ruralurban2 <- ruralurban2 %>%
  rename(clust = Clust)

dfKen <- dfKen %>%
  mutate(rural_urban = ifelse(rural_urban == "Urban", 1, 2))

#Merge ruralurban 2 with dfKen by clust variable
dfKen <- left_join(dfKen, ruralurban2, by = "clust")
table(dfKen$Rural_Urban, useNA = "always")

#harmonise rural_urban variable in dfKen:
dfKen <- dfKen %>%
  rename(rural_urban = Rural_Urban)     ##788 MISSING
table(dfKen$rural_urban)

#Regional variable 
table(dfKen$county_name)
dfKen <- dfKen %>%
  mutate(region = county_name)

#rename country column in shapefile  
Kenya_shape <- Kenya_shape %>%
  mutate(country = CNTRYNAMEE)

#steps:
#transforming all region names in dfKen to first letter uppercase and the rest is loswercase
dfKen$region <- str_to_title(dfKen$region)

# Print the transformed region names
print(table(dfKen$region))
library(dplyr)

dfKen <- dfKen %>%
  mutate(region = case_when(
    region == "Elgeyo Marakwet" ~ "Elgeyo-Marakwet",
    TRUE ~ region
  ))

# Recode "Muranga" to "Murang'a"
dfKen <- dfKen %>%
  mutate(region = case_when(
    region == "Muranga" ~ "Murang'a",
    TRUE ~ region
  ))

# Recode "Tharaka" to "Tharaka-Nithi"
dfKen <- dfKen %>%
  mutate(region = case_when(
    region == "Tharaka" ~ "Tharaka-Nithi",
    TRUE ~ region
  ))

# Recode "Trans-Nzoia" to "Trans Nzoia"
dfKen <- dfKen %>%
  mutate(region = case_when(
    region == "Trans-Nzoia" ~ "Trans Nzoia",
    TRUE ~ region
  ))

#check if harmonised
table(dfKen$region)
table(Kenya_shape$region)


#------------------Second: preparing the VACS surveys: LESOTHO-------------------#

#create a WI variable with the observations of n
dfLso <- dfLso %>%
  mutate(WI = row_number())

#renaming the weights variables:
dfLso <- dfLso %>%
  rename(
    cluster = psu,
    sampleweight = individual_weight1)

#Apply weights to WI using the individual weight variable
sum_w <- sum(dfLso$WI * dfLso$sampleweight)  # Calculate the sum of weighted numbers
dfLso$WI_1 <- sum_w  # Replace WI column with the sum
table(dfLso$WI_1)

#create the strata variable from the region variable:
dfLso$strata <- dfLso$region

# Create a survey design object using svydesign
svy_design <- svydesign(
  ids = ~cluster,   # Clustering variable
  strata = ~strata,   # Stratification variable
  weights = ~sampleweight, # Survey weight variable
  nest = TRUE, 
  data = dfLso,
)

#create a country variable
dfLso$country <- "Lesotho"

# Extracting year, month, and day information
dfLso$HDATE_VF <- paste(substr(dfLso$HDATE_VF,1,4), substr(dfLso$HDATE_VF,5,6), substr(dfLso$HDATE_VF,7,8), sep = "-")

dfLso <- dfLso %>% 
  mutate(
    year = as.integer(substr(HDATE_VF, 1, 4)),
    month = as.integer(substr(HDATE_VF, 6, 7)),
    day = as.integer(substr(HDATE_VF, 9, 10)))

table(dfLso$year, useNA = "always")   #7 NAs
table(dfLso$month, useNA = "always")

#--------------------DEMOGRAPHIC VARIABLES-----------------
#Sex of head of household:
dfLso <- dfLso %>%
  rename(HHSex = H2)

table(dfLso$HHSex)
attr(dfLso$HHSex, "label") <- "Head of household sex"

table(dfLso$HHSex, useNA = "always")

#Regional variable 
table(dfLso$region)

#urban rural variable
table(dfLso$rural_urban)

#examine the gender variable (sex):
dfLso$sex

#Marital status
# Initialize the variable with NA
dfLso$married <- 0
dfLso$married[dfLso$Q38 %in% c(1, 2)] <- 1
dfLso$married[dfLso$Q36 == 3] <- 0
# Labeling
attr(dfLso$married, "label") <- "Currently married"
table(dfLso$married, useNA = "always")

#child marriage
dfLso <- dfLso %>%
  mutate(childmar = ifelse( Q37 %in% c (13:17),1,0))
table(dfLso$childmar, dfLso$sex, useNA = "always")
# Labeling
attr(dfLso$childmar, "label") <- "Married before the age of 18"
table(dfLso$childmar, dfLso$sex, useNA = "always")

#food insecurity:
dfLso$food_insec <- 0
dfLso$food_insec[dfLso$H53 == 1 | dfLso$H52 == 1 | dfLso$Q9A == 1] <- 1
table(dfLso$food_insec, useNA = "always") #1697 NAs

# Labeling
attr(dfLso$food_insec, "label") <- "Food insecure"

#----Age variable
# Age group:
dfLso$AgeGroup <- 0
dfLso$AgeGroup[dfLso$Q2 >= 13 & dfLso$Q2 <= 17] <- 1
dfLso$AgeGroup[dfLso$Q2 >= 18 & dfLso$Q2 <= 24] <- 2

# Labeling variables
attr(dfLso$AgeGroup, "label") <- "age group of girls"
label_values <- c("13-17", "18-24")
attr(dfLso$AgeGroup, "labels") <- label_values

# Displaying Q2 and AgeGroup for the first 30 rows
dfLso[1:30, c("Q2", "AgeGroup")]

#Tabulating Q2 and AgeGroup:
table(dfLso$Q2, dfLso$AgeGroup)

#--------------- Education attainment

# Check missing values in each variable
table(!is.na(dfLso$Q3)) # no missing values issue (only 1 DNK and 3 DECLINED)
table(!is.na(dfLso$Q4)) # 141 missing, 1 DNK and 1 DECLINED
table(!is.na(dfLso$Q6)) # 5,214 missing dfLso (more than 60%), 4 DNK
table(!is.na(dfLso$Q7)) # 3,497 missing dfLso, 1 DNK and 4 DECLINED

# Recode values 98 and 99 as missing (.)
dfLso$Q3_ <- ifelse(dfLso$Q3 %in% c(98, 99), NA, dfLso$Q3)
dfLso$Q4_ <- ifelse(dfLso$Q4 %in% c(98, 99), NA, dfLso$Q4)
dfLso$Q6_ <- ifelse(dfLso$Q6 %in% c(98, 99), NA, dfLso$Q6)
dfLso$Q7_ <- ifelse(dfLso$Q7 %in% c(98, 99), NA, dfLso$Q7)


# Cross-tabulation for imputation
cross_table <- table(dfLso$Q3_, dfLso$Q6_, useNA = "always")
print(cross_table)

# Recode Q6_ and Q7_ to regroup into 1. primary or less, 2. secondary, 3. higher than secondary
dfLso$Q6_ <- ifelse(dfLso$Q3_ == 2, 0, dfLso$Q6_)
dfLso$Q6_ <- ifelse(dfLso$Q6_ %in% c(0, 1, 2), 1, dfLso$Q6_)
dfLso$Q6_ <- ifelse(dfLso$Q6_ %in% c(3, 4), 2, dfLso$Q6_)
dfLso$Q6_ <- ifelse(dfLso$Q6_ == 5, 3, dfLso$Q6_)

dfLso$Q7_ <- ifelse(dfLso$Q3_ == 2, 0, dfLso$Q7_)
dfLso$Q7_ <- ifelse(dfLso$Q7_ %in% c(0, 1, 2), 1, dfLso$Q7_)
dfLso$Q7_ <- ifelse(dfLso$Q7_ %in% c(3, 4), 2, dfLso$Q7_)
dfLso$Q7_ <- ifelse(dfLso$Q7_ == 5, 3, dfLso$Q7_)

# Replace missing values in Q6_ for those with the value of primary or less in Q7_
dfLso$Q6_[dfLso$Q7_ == 1 & is.na(dfLso$Q6_)] <- 1
dfLso$Q6_[dfLso$Q7_ == 2 & is.na(dfLso$Q6_)] <- 2
dfLso$Q6_[dfLso$Q7_ == 3 & is.na(dfLso$Q6_)] <- 3

# Create a new variable "EducationEver"
dfLso$EducationEver <- dfLso$Q6_

#label the Education variable:
label(dfLso$EducationEver) <- "Education attainment"

# Define value labels for EducationEver
value_labels <- c("Completed primary school or less", "Completed secondary school", "Higher than secondary")

# Apply value labels to EducationEver
dfLso$EducationEver <- factor(dfLso$EducationEver, levels = c(1, 2, 3), labels = value_labels)
table(dfLso$EducationEver, useNA = "always")

# Create a new variable "EducationEver2" with two categories
dfLso$EducationEver2 <- ifelse(dfLso$EducationEver %in% c("Completed secondary school", "Higher than secondary"), 1, 0)

# Drop the original EducationEver variable
dfLso <- dfLso %>% dplyr::select(-EducationEver)            

# Rename EducationEver2 to EducationEver
dfLso <- dfLso %>% rename(EducationEver = EducationEver2)
table(dfLso$EducationEver, useNA = "always")

# label the EducationEver variable:
attr(dfLso$EducationEver, "label") <- "Completed secondary or higher level of education"
table(dfLso$EducationEver, useNA = "always")

#--Current school enrollment
dfLso$School_enrol <- ifelse(dfLso$Q3_ == 2 & is.na(dfLso$Q4_), 2, dfLso$Q4_)
dfLso$School_enrol[dfLso$School_enrol == 2] <- 0

# Labeling variable
attr(dfLso$School_enrol, "label") <- "School enrollemnt"
table(dfLso$School_enrol)
# view_df(dfLso)

#---------Orphanhood variables
# Maternal orphanhood
dfLso$OrphMater <- 0
dfLso$OrphMater[dfLso$Q15 == 1] <- 0
dfLso$OrphMater[dfLso$Q15 == 2] <- 1
attr(dfLso$OrphMater, "label") <- "Maternal orphanhood"
table(dfLso$OrphMater, useNA = "always")

# Paternal orphanhood
dfLso$OrphPater <- 0
dfLso$OrphPater[dfLso$Q25 == 1] <- 0
dfLso$OrphPater[dfLso$Q25 == 2] <- 1
attr(dfLso$OrphPater, "label") <- "Paternal orphanhood"
table(dfLso$OrphPater, useNA = "always")

# Orphanhood variable
dfLso$Orphan <- 0
dfLso$Orphan[dfLso$OrphMater == 1 | dfLso$OrphPater == 1] <- 1
dfLso$Orphan[dfLso$OrphMater == 0 & dfLso$OrphPater == 0] <- 0
table(dfLso$Orphan, useNA = "always")

#tabulations
table(dfLso$OrphMater, useNA = "always")
table(dfLso$OrphPater, useNA = "always")
table(dfLso$Orphan, useNA = "always")

#-----self reported HIV positive
dfLso$hivpos

#INFORMAL HOUSING VARIABLE (old code)
dfLso <- dfLso %>%
  mutate(inf_housing = ifelse(H7A == 2, 1,   #no electricity
                              # ifelse(H6 ==1, 1,    #sharing of toilet with other households
                              ifelse(H10 %in% c(1:6), 1, 0))) #roof material
table(dfLso$inf_housing)

#main problem with NAs
table(dfLso$inf_housing, useNA = "always")
table(dfLso$H10, useNA = "always")
table(dfLso$rural_urban, useNA = "always")

#--------Violence variables----------------
#-------------Life time sexual violence variables --------------

# Unwanted sexual touching in a lifetime:
dfLso <- dfLso %>% 
  mutate(SV_Q600 = ifelse(Q600==1,1,
                          ifelse(Q600 == 2, 0,NA)))

# Label the variable 'SV_Q600':
attr(dfLso$SV_Q600, "label") <- "Unwanted sexual touching in a lifetime"
table(dfLso$SV_Q600, useNA = "always")

#--------------Attempted forced sex in a lifetime:

# Recode variables Q700A and Q700B
dfLso <- dfLso %>%
  mutate(SV_Q700A = ifelse(Q700A == 1, 1, ifelse(Q700A == 2, 0, NA)),
         SV_Q700B = ifelse(Q700B == 1, 1, ifelse(Q700B == 2, 0, NA)))

# Calculate row totals for SV_Q700A and SV_Q700B
dfLso$SV_AttRape <- rowSums(dfLso[, c("SV_Q700A", "SV_Q700B")], na.rm = TRUE)
table(dfLso$SV_AttRape, useNA = "always")

# Replace missing values and recode values greater than or equal to 1 to 1, and others to 0
dfLso$SV_AttRape <- ifelse(is.na(dfLso$SV_AttRape), NA,
                           ifelse(dfLso$SV_AttRape >= 1, 1, 0))

# Label the variable SV_AttRape
attr(dfLso$SV_AttRape, "label") <- "Attempted forced Sex"
table(dfLso$SV_AttRape, useNA = "always")

#------------Physically forced sex in a lifetime:
dfLso <- dfLso %>%
  mutate(SV_800A = ifelse(Q800A == 1, 1, ifelse(Q800A == 2, 0, NA)),
         SV_800B = ifelse(Q800B == 1, 1, ifelse(Q800B == 2, 0, NA)))

#Calculate row totals for SV_800A and SV_800B
dfLso$SV_Rape <- rowSums(dfLso[, c("SV_800A", "SV_800B")], na.rm = TRUE)

dfLso$SV_Rape <- ifelse(is.na(dfLso$SV_Rape), NA,
                        ifelse(dfLso$SV_Rape >=1, 1, 0))          
attr(dfLso$SV_Rape, "label") <- "Physically forced sex"
table(dfLso$SV_Rape, useNA = "always")

#-----------Pressured Sex--------------------------:

dfLso <- dfLso %>% 
  mutate(SV_900A = ifelse(Q900A == 1, 1, ifelse(Q900A == 2, 0, NA)),
         SV_900B = ifelse(Q900B == 1, 1, ifelse(Q900B == 2, 0, NA)))

dfLso$SV_PreSex <- rowSums(dfLso[, c("SV_900A","SV_900B")], na.rm = TRUE)

dfLso$SV_PreSex <- ifelse(is.na(dfLso$SV_PreSex), NA,
                          ifelse(dfLso$SV_PreSex >=1, 1, 0))
attr(dfLso$SV_PreSex, "label") <- "Pressured Sex"

table(dfLso$SV_PreSex, useNA = "always")

# #-----------Alcohol facilitated Sex--------------------------:
dfLso <- dfLso %>% 
  mutate(SV_1000A = ifelse(Q1000A ==1,1, ifelse(Q1000A ==2,0,NA)),
         SV_1000B = ifelse(Q1000B ==1,1, ifelse(Q1000B ==2,0,NA)))

dfLso$SV_RapeAlcohol <- rowSums(dfLso[,c("SV_1000A", "SV_1000B")], na.rm = TRUE)

dfLso$SV_RapeAlcohol <- ifelse(is.na(dfLso$SV_RapeAlcohol), NA,
                               ifelse(dfLso$SV_RapeAlcohol >=1,1,0))

attr(dfLso$SV_RapeAlcohol, "label") <- "Alcohol facilitated Sex"
table(dfLso$SV_RapeAlcohol, useNA = "always")

#-----------Any lifetime sexual violence-------------------:

dfLso$Any_SV_lifetime <- rowSums(dfLso[,c("SV_Q600","SV_AttRape", "SV_Rape", "SV_PreSex")],
                                 na.rm =TRUE)
dfLso$Any_SV_lifetime <- ifelse(is.na(dfLso$Any_SV_lifetime), NA,
                                ifelse(dfLso$Any_SV_lifetime >=1, 1, 0))
attr(dfLso$Any_SV_lifetime, "label") <- "Any Lifetime sexual violence"
table(dfLso$Any_SV_lifetime, useNA = "always")

#-----------Sexual violence variables in the PAST 12 MONTHS--------------

# Unwanted sexual touching BY ANYONE in the last 12 months:
dfLso <- dfLso %>%
  mutate(
    SV_1_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 2 | Q609 == 2)) ~ 0,  # No recent unwanted touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 1 | Q609 == 1)) ~1,
      TRUE ~ NA_real_  # All other cases to NA
    )
  )
# Adding a label to the column
attr(dfLso$SV_1_12m, "label") <- "Unwanted Sexual Touching by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfLso$SV_1_12m, useNA = "always")

# Unwanted attempted sex by ANYONE in the last 12 months:
# Recode SV_2_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_2_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q709 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q709 == 1)) ~ 1,
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_2_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfLso$SV_2_12m, useNA = "always")

# Unwanted forced sex BY ANYONE in the last 12 months:
# Recode SV_3_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_3_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 2 | Q810 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 1 | Q810 == 1)) ~ 1,
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfLso$SV_3_12m, "label") <- "Physically forced sex by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfLso$SV_3_12m, useNA = "always")

# Pressured sex BY ANYONE in the last 12 months:
# Recode SV_4_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_4_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 2 | Q909 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 1 | Q909 == 1)) ~ 1,
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_4_12m, "label") <- "Pressured Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfLso$SV_4_12m, useNA = "always")

# # Alcohol facilitated forced sex BY ANYONE in the last 12 months:
# Recode SV_5_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_5_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1009 == 2)) ~ 0,  # No recent alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1009 == 1)) ~ 1, 
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_5_12m, "label") <- "Alcohol facilitated forced Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfLso$SV_5_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfLso$SV_Any <- rowSums(dfLso[,c("SV_1_12m","SV_2_12m", "SV_3_12m","SV_4_12m", "SV_5_12m")], na.rm =TRUE)
dfLso$SV_Any<- ifelse(is.na(dfLso$SV_Any), NA,
                      ifelse(dfLso$SV_Any >=1, 1, 0))

attr(dfLso$SV_Any, "label") <- "Any sexual violence by anyone in the last 12 months"
table(dfLso$SV_Any, useNA = "always")

#----------SEXUAL VIOLENCE IN THE PAST 12 MONTHS BY NON-PARTNERS AND PARTNERS:

# Unwanted sexual touching BY NON-PARTNERS in the last 12 months:
dfLso <- dfLso %>%
  mutate(
    SV_1any_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 2 | Q609 == 2)) ~ 0,  # No recent unwanted touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 1 | Q609 == 1)) &
        (Q605 %in% c(5:16, 21:32) | Q612 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_1any_12m, "label") <- "Unwanted Sexual Touching by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfLso$SV_1any_12m, useNA = "always")

#### Unwanted sexual touching by an intimate partner in the last 12 months:
dfLso <- dfLso %>%
  mutate(
    SV_1IPV_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 2 | Q609 == 2)) ~ 0,  # No recent unwanted touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 1 | Q609 == 1)) & 
        (Q605 %in% c(1:4,17:20) | Q612 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_1IPV_12m, "label") <- "Unwanted Sexual Touching by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfLso$SV_1IPV_12m, useNA = "always")

# Unwanted attempted sex by ANYONE in the last 12 months:
# Recode SV_2_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_2any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q709 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q709 == 1)) &
        (Q705 %in% c(5:16, 21:32) | Q712 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_2any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfLso$SV_2any_12m, useNA = "always")

# Unwanted attempted sex by INTIMATE PARTNER in the last 12 months:
# Recode SV_2_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_2IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q709 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q709 == 1)) &
        (Q705 %in% c(1:4,17:20) | Q712 %in% c(1:4, 17:20)) ~ 1, 
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_2IPV_12m, "label") <- "Unwanted attempted sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable
table(dfLso$SV_2IPV_12m, useNA = "always")

# Unwanted forced sex BY ANYONE in the last 12 months:
# Recode SV_3_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_3any_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 2 | Q810 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 1 | Q810 == 1)) &
        (Q805 %in% c(5:16, 21:32) | Q813 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfLso$SV_3any_12m, "label") <- "Physically forced sex by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfLso$SV_3any_12m, useNA = "always")

# Unwanted forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_3_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_3IPV_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 2 | Q810 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 1 | Q810 == 1)) &
        (Q805 %in% c(1:4,17:20) | Q813 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfLso$SV_3IPV_12m, "label") <- "Physically forced sex by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfLso$SV_3IPV_12m, useNA = "always")

# Pressured sex BY ANYONE in the last 12 months:
# Recode SV_4_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_4any_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 2 | Q909 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 1 | Q909 == 1)) &
        (Q905 %in% c(5:16, 21:32) | Q912 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_4any_12m, "label") <- "Pressured Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfLso$SV_4any_12m, useNA = "always")

# Pressured sex AN INTIMATE PARTNER in the last 12 months:
# Recode SV_4_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_4IPV_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 2 | Q909 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 1 | Q909 == 1)) &
        (Q905 %in% c(1:4,17:20) | Q912 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_4IPV_12m, "label") <- "Pressured Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable 
table(dfLso$SV_4IPV_12m, useNA = "always")

# # Alcohol facilitated forced sex BY ANYONE in the last 12 months:
# Recode SV_5_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_5any_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1009 == 2)) ~ 0,  # No recent alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1009 == 1)) &
        (Q1005 %in% c(5:16, 21:32) | Q1012 %in% c(5:16, 21:32)) ~ 1,   # Recent alcohol-facilitated forced sex
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_5any_12m, "label") <- "Alcohol facilitated forced Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfLso$SV_5any_12m, useNA = "always")

# # Alcohol facilitated forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_5_12m based on conditions
dfLso <- dfLso %>%
  mutate(
    SV_5IPV_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1009 == 2)) ~ 0,  # No recent alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1009 == 1)) &
        (Q1005 %in% c(1:4,17:20) | Q1012 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfLso$SV_5IPV_12m, "label") <- "Alcohol facilitated forced Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfLso$SV_5IPV_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfLso$Any_SV_12m <- rowSums(dfLso[,c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m", "SV_5any_12m")], na.rm =TRUE)
dfLso$Any_SV_12m <- ifelse(is.na(dfLso$Any_SV_12m), NA,
                           ifelse(dfLso$Any_SV_12m >=1, 1, 0))

attr(dfLso$Any_SV_12m, "label") <- "Any sexual violence by anyone in the last 12 months"
table(dfLso$Any_SV_12m, useNA = "always")

#  sexual violence BY AN INTIMATE PARTNER in the past 12 months:
dfLso$SV_IPV_12m <- rowSums(dfLso[,c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m", "SV_5IPV_12m")], na.rm =TRUE)
dfLso$SV_IPV_12m <- ifelse(is.na(dfLso$SV_IPV_12m), NA,
                           ifelse(dfLso$SV_IPV_12m >=1, 1, 0))

attr(dfLso$SV_IPV_12m, "label") <- "Any sexual violence by an intimate partner in the last 12 months"
table(dfLso$SV_IPV_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfLso$Any_SV <- rowSums(dfLso[,c("SV_IPV_12m","Any_SV_12m")], na.rm =TRUE)
dfLso$Any_SV <- ifelse(is.na(dfLso$Any_SV), NA,
                       ifelse(dfLso$Any_SV >=1, 1, 0))

attr(dfLso$Any_SV, "label") <- "Any sexual violence by any perpetrator in the last 12 months"
table(dfLso$Any_SV, useNA = "always")

#-------------Life time Physical violence variables --------------
# Physical IPV in a lifetime
vars <- c("Q100A", "Q100B", "Q100C", "Q100D")
for (var in vars) {
  dfLso[[paste0("IPV_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$PV_IPV_lt <- rowSums(dfLso[, grep("^IPV_", colnames(dfLso))], na.rm = TRUE)
dfLso$PV_IPV_lt <- ifelse(is.na(dfLso$PV_IPV_lt), NA, ifelse(dfLso$PV_IPV_lt >= 1, 1, 0))

attr(dfLso$PV_IPV_lt, "label") <- "Lifetime physical IPV"
table(dfLso$PV_IPV_lt, useNA = "always")

# Physical peer violence in a lifetime
vars_peer <- c("Q110A", "Q110B", "Q110C", "Q110D")
for (var in vars_peer) {
  dfLso[[paste0("ViolPeer_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$PV_Peers_lt <- rowSums(dfLso[, grep("^ViolPeer_", colnames(dfLso))], na.rm = TRUE)
dfLso$PV_Peers_lt <- ifelse(is.na(dfLso$PV_Peers_lt), NA, ifelse(dfLso$PV_Peers_lt >= 1, 1, 0))

attr(dfLso$PV_Peers_lt, "label") <- "Lifetime peer physical violence"
table(dfLso$PV_Peers_lt, useNA = "always")

# Physical family violence
vars_fam <- c("Q120A", "Q120B", "Q120C", "Q120D")
for (var in vars_fam) {
  dfLso[[paste0("ViolF_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$PV_Fam_lt <- rowSums(dfLso[, grep("^ViolF_", colnames(dfLso))], na.rm = TRUE)
dfLso$PV_Fam_lt <- ifelse(is.na(dfLso$PV_Fam_lt), NA, ifelse(dfLso$PV_Fam_lt >= 1, 1, 0))

attr(dfLso$PV_Fam_lt, "label") <- "Lifetime family physical violence"
table(dfLso$PV_Fam_lt, useNA = "always")

# Community Physical Violence (adults in neighbourhood)
vars_com <- c("Q132A", "Q132B", "Q132C", "Q132D")
for (var in vars_com) {
  dfLso[[paste0("ViolC_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$PV_Com_lt <- rowSums(dfLso[, grep("^ViolC_", colnames(dfLso))], na.rm = TRUE)
dfLso$PV_Com_lt <- ifelse(is.na(dfLso$PV_Com_lt), NA, ifelse(dfLso$PV_Com_lt >= 1, 1, 0))

attr(dfLso$PV_Com_lt, "label") <- "Lifetime community physical violence"
table(dfLso$PV_Com_lt, useNA = "always")


# Any physical violence in a lifetime:
dfLso$Any_PV_lt <- rowSums(dfLso[,c("PV_IPV_lt", "PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfLso$Any_PV_lt <- ifelse(is.na(dfLso$Any_PV_lt), NA,
                          ifelse(dfLso$Any_PV_lt >=1, 1, 0))

attr(dfLso$Any_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfLso$Any_PV_lt, useNA = "always")

####.  Create JUST child physical violence variable: ####
# Child physical violence in a lifetime:
dfLso$child_PV_lt <- rowSums(dfLso[,c("PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfLso$child_PV_lt <- ifelse(is.na(dfLso$child_PV_lt), NA,
                            ifelse(dfLso$child_PV_lt >=1, 1, 0))

attr(dfLso$child_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfLso$child_PV_lt, useNA = "always")

####.  Create JUST community physical violence variable: ####
dfLso$community_PV_lt <- rowSums(dfLso[,c("PV_Peers_lt", "PV_Com_lt")], na.rm = TRUE)
dfLso$community_PV_lt <- ifelse(is.na(dfLso$community_PV_lt), NA,
                                ifelse(dfLso$community_PV_lt >=1, 1, 0))

attr(dfLso$community_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfLso$community_PV_lt, useNA = "always")

#------------Physical violence variables in the last 12 months------------

# Physical IPV in the last 12 months:
vars_ipv <- c("Q100A1", "Q100B1", "Q100C1", "Q100D1")
for (var in vars_ipv) {
  dfLso[[paste0(var, "1")]] <- ifelse(dfLso[[var]] == 2, 2, ifelse(dfLso[[var]] == 1, 1, NA))
  dfLso[[paste0("IPV_", var, "1")]] <- ifelse(dfLso[[paste0(var, "1")]] == 1, 1, ifelse(dfLso[[paste0(var, "1")]] == 2, 0, NA))
}

dfLso$PV_IPV_12m <- rowSums(dfLso[, grep("^IPV_", colnames(dfLso))], na.rm = TRUE)
dfLso$PV_IPV_12m <- ifelse(is.na(dfLso$PV_IPV_12m), NA, ifelse(dfLso$PV_IPV_12m >= 1, 1, 0))

attr(dfLso$PV_IPV_12m, "label") <- "Physical IPV in the past 12 months"

# Physical peer violence in the last 12 months:
dfLso$PV_Peers_12m <- NA
dfLso$PV_Peers_12m[dfLso$PV_Peers_lt == 0 | 
                     (dfLso$PV_Peers_lt == 1 & 
                        (dfLso$Q111 >= 1 & 
                           (dfLso$Q112 == 2 | dfLso$Q115 == 2)))] <- 0
dfLso$PV_Peers_12m[dfLso$PV_Peers_lt == 1 & 
                     (dfLso$Q111 >= 1 & 
                        (dfLso$Q112 == 1 | dfLso$Q115 == 1))] <- 1
attr(dfLso$PV_Peers_12m, "label") <- "Peers physical violence in the past 12 months"
table(dfLso$PV_Peers_12m, useNA = "always")

# Generate a frequency table with missing values included
table(dfLso$PV_Peers_12m, useNA = "always")

# Physical family violence in the last 12 months:
dfLso$PV_Fam_12m <- NA
dfLso$PV_Fam_12m[dfLso$PV_Fam_lt == 0 | 
                   (dfLso$PV_Fam_lt == 1 & (dfLso$Q121 >= 1 & 
                                              (dfLso$Q122 == 2 | dfLso$Q126 == 2)))] <- 0
dfLso$PV_Fam_12m[dfLso$PV_Fam_lt == 1 & (dfLso$Q121 >= 1 & 
                                           (dfLso$Q122 == 1 | dfLso$Q126 == 1))] <- 1
# Label the variable
attr(dfLso$PV_Fam_12m, "label") <- "Family physical violence in the past 12 months"
table(dfLso$PV_Fam_12m, useNA = "always")

# Physical violence in the community in the last 12 months:
dfLso$PV_Com_12m <- NA
dfLso$PV_Com_12m[dfLso$PV_Com_lt == 0 | 
                   (dfLso$PV_Com_lt == 1 & 
                      (dfLso$Q133 >= 1 & 
                         (dfLso$Q134 == 2 | dfLso$Q137 == 2)))] <- 0
dfLso$PV_Com_12m[dfLso$PV_Com_lt == 1 & 
                   (dfLso$Q133 >= 1 & 
                      (dfLso$Q134 == 1 | dfLso$Q137 == 1))] <- 1

# Add a descriptive label to the variable
attr(dfLso$PV_Com_12m, "label") <- "Physical violence in the community in the past 12 months"

# Generate a frequency table with missing values included
table(dfLso$PV_Com_12m, useNA = "always")

# Any past-year physical violence BY EVERYONE in the last 12 months:
dfLso$PV_Any <- rowSums(dfLso[, c("PV_IPV_12m", "PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfLso$PV_Any <- ifelse(is.na(dfLso$PV_Any), NA, ifelse(dfLso$PV_Any >= 1, 1, 0))

attr(dfLso$PV_Any, "label") <- "Any physical violence in the past 12 months"
table(dfLso$PV_Any, useNA = "always")

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfLso$child_PV_12m <- rowSums(dfLso[, c("PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfLso$child_PV_12m <- ifelse(is.na(dfLso$child_PV_12m), NA, ifelse(dfLso$child_PV_12m >= 1, 1, 0))

attr(dfLso$child_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfLso$child_PV_12m, useNA = "always")

#### creating just community physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfLso$community_PV_12m <- rowSums(dfLso[, c("PV_Peers_12m", "PV_Com_12m")], na.rm = TRUE)
dfLso$community_PV_12m <- ifelse(is.na(dfLso$community_PV_12m), NA, ifelse(dfLso$community_PV_12m >= 1, 1, 0))

attr(dfLso$community_PV_12m, "label") <- "Any community physical violence in the past 12 months"
table(dfLso$community_PV_12m, useNA = "always")

# Any physical violence perpetrated by anyone in the last 12 months:
dfLso$child_PV_12m <- rowSums(dfLso[, c("child_PV_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfLso$child_PV_12m <- ifelse(is.na(dfLso$child_PV_12m), NA, ifelse(dfLso$child_PV_12m >= 1, 1, 0))

attr(dfLso$child_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfLso$child_PV_12m, useNA = "always")

#-------------Life time Emotional violence variables --------------
#Q300 + 301(1==1/2)  + 306 (1==1)(lifetime) from : PARENT, ADULT CAREGIVER OT OTHER ADULT RELATIVE
#Q310 (A,B,C,D,==1)
#Q315 (lifetime) PEER EMOTIONAL VIOLENCE
#Q300==0 | Q300==1 & (Q301== >=1 | (Q302==2 & Q306==2)  

#-----------Emotional violence in the LAST 12 MONTHS:
#------ Any lifetime emotional violence by parent, adult caregiver or other adult relative
vars_EV_fam_lt <- c("Q300A", "Q300B", "Q300C")
for (var in vars_EV_fam_lt) {
  dfLso[[paste0("EViolF_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$EV_Fam_lt <- rowSums(dfLso[, grep("^EViolF_", colnames(dfLso))], na.rm = TRUE)
dfLso$EV_Fam_lt <- ifelse(is.na(dfLso$EV_Fam_lt), NA, ifelse(dfLso$EV_Fam_lt >= 1, 1, 0))

attr(dfLso$EV_Fam_lt, "label") <- "Any lifetime emotional violence by parent, adult caregiver or other adult relative"
table(dfLso$EV_Fam_lt, useNA = "always")

#---- Any lifetime emotional violence by INTIMATE PARTNER:
vars_EV_IPV_lt <- c("Q310A", "Q310B", "Q310C", "Q310D", "Q310E")
for (var in vars_EV_IPV_lt) {
  dfLso[[paste0("EIPV_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$EV_IPV_lt <- rowSums(dfLso[, grep("^EIPV_", colnames(dfLso))], na.rm = TRUE)
dfLso$EV_IPV_lt <- ifelse(is.na(dfLso$EV_IPV_lt), NA, ifelse(dfLso$EV_IPV_lt >= 1, 1, 0))

attr(dfLso$EV_IPV_lt, "label") <- "Lifetime emotional IPV"
table(dfLso$EV_IPV_lt, useNA = "always")

#------ Any lifetime emotional violence BY ANYONE
dfLso$EV_Any_lt <- rowSums(dfLso[, c("EV_IPV_lt", "EV_Fam_lt")], na.rm = TRUE)
dfLso$EV_Any_lt <- ifelse(is.na(dfLso$EV_Any_lt), NA, ifelse(dfLso$EV_Any_lt >= 1, 1, 0))

attr(dfLso$EV_Any_lt, "label") <- "Any lifetime emotional violence"
table(dfLso$EV_Any_lt, useNA = "always")

#---------------------Emotional violence variables in the LAST 12 MONTHS:
#------ Emotional violence by parent, adult caregiver, or other adult relative in the past 12 months
dfLso$EV_Fam_12m <- NA
dfLso$EV_Fam_12m[dfLso$EV_Fam_lt == 0 | 
                   (dfLso$EV_Fam_lt == 1 & (dfLso$Q301 >= 1 & 
                                              (dfLso$Q302 == 2 | dfLso$Q306 == 2)))] <- 0
dfLso$EV_Fam_12m[dfLso$EV_Fam_lt == 1 & (dfLso$Q301 >= 1 & 
                                           (dfLso$Q302 == 1 | dfLso$Q306 == 1))] <- 1

attr(dfLso$EV_Fam_12m, "label") <- "Any emotional violence by parent, adult caregiver, or other adult relative in the past 12 months"
table(dfLso$EV_Fam_12m, useNA = "always")
#------------Emotional violence by INTIMATE PARTNER in the LAST 12 MONTHS:
# Initialize the variable with NA
dfLso$EV_IPV_12m <- 0
dfLso$EV_IPV_12m[dfLso$EV_IPV_lt == 0 | 
                   (dfLso$EV_IPV_lt == 1 & 
                      (dfLso$Q311 >= 1 & 
                         dfLso$Q312 == 2))] <- 0
dfLso$EV_IPV_12m[dfLso$EV_IPV_lt == 1 & 
                   (dfLso$Q311 >= 1 & 
                      dfLso$Q312 == 1)] <- 1
attr(dfLso$EV_IPV_12m, "label") <- "Emotional violence by intimate partner in the past 12 months"
table(dfLso$EV_IPV_12m, useNA = "always")

#------------Emotional violence by PEER in the LAST 12 MONTHS:
vars_EV_peer_12m <- c("Q315A", "Q315B", "Q315C")
for (var in vars_EV_peer_12m) {
  dfLso[[paste0("EViolPeer_", var)]] <- ifelse(dfLso[[var]] == 1, 1, ifelse(dfLso[[var]] == 2, 0, NA))
}

dfLso$EV_Peer_12m <- rowSums(dfLso[, grep("^EViolPeer_", colnames(dfLso))], na.rm = TRUE)
dfLso$EV_Peer_12m <- ifelse(is.na(dfLso$EV_Peer_12m), NA, ifelse(dfLso$EV_Peer_12m >= 1, 1, 0))

attr(dfLso$EV_Peer_12m, "label") <- "Lifetime emotional violence by peers"
table(dfLso$EV_Peer_12m, useNA = "always")


#------ Any emotional violence BY NON-PARTNERS in the past 12 months
dfLso$EV_Any_12m <- rowSums(dfLso[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfLso$EV_Any_12m <- ifelse(is.na(dfLso$EV_Any_12m), NA, ifelse(dfLso$EV_Any_12m >= 1, 1, 0))

attr(dfLso$EV_Any_12m, "label") <- "emotional violence by ANYONE in the past 12 months"
table(dfLso$EV_Any_12m, useNA = "always")

#------ Any emotional violence BY ANYONE in the past 12 months
dfLso$EV_Any <- rowSums(dfLso[, c("EV_IPV_12m", "EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfLso$EV_Any <- ifelse(is.na(dfLso$EV_Any), NA, ifelse(dfLso$EV_Any >= 1, 1, 0))

attr(dfLso$EV_Any, "label") <- "emotional violence by ANYONE in the past 12 months"
table(dfLso$EV_Any, useNA = "always")


#Select key variables of interest
dfLso1 <- dplyr::select(dfLso,
                        Q2,
                        HHSex,
                        inf_housing,
                        strata,
                        cluster,
                        sampleweight,
                        HDATE_VF,
                        month,
                        year,
                        region,
                        country,
                        AgeGroup,
                        sex,
                        married,
                        food_insec,
                        rural_urban,
                        EducationEver,
                        School_enrol,
                        Orphan,
                        Any_SV_12m,
                        EV_Any_12m,
                        child_PV_12m,
                        community_PV_12m,
                        PV_Fam_12m,
                        PV_IPV_12m,
                        SV_IPV_12m,
                        EV_IPV_12m,
                        SV_Any,
                        EV_Any,
                        PV_Any)


#---------Namibia------:

#rename survey weights variables
dfNmb <- dfNmb %>%
  rename(cluster = ea)

# Create a survey design object using svydesign
svy_design <- svydesign(
  ids = ~cluster,   # Clustering variable
  strata = ~strata,   # Stratification variable
  weights = ~sampleweight, # Survey weight variable
  nest = TRUE, 
  data = dfNmb,
)

#create a WI variable with the observations of n
dfNmb <- dfNmb %>%
  mutate(WI = row_number())

#Apply weights to WI using the individual weight variable
sum_w <- sum(dfNmb$WI * dfNmb$sampleweight)  # Calculate the sum of weighted numbers
dfNmb$WI_1 <- sum_w  # Replace WI column with the sum
table(dfNmb$WI_1)

dfNmb$country <- "Namibia"

#create interview date variable:
dfNmb <- dfNmb %>%
  rename(HDATE_VF = HDATE_F)
table(dfNmb$HDATE_VF)

# Extract the date part (year, month, day) from the time stamp values
dfNmb$HDATE_VF <- sub("T.*", "", dfNmb$HDATE_VF)

# Extracting year, month, and day information
dfNmb <- dfNmb %>% 
  mutate(
    year = as.integer(substr(HDATE_VF, 1, 4)),
    month = as.integer(substr(HDATE_VF, 6,7)),
    day = as.integer(substr(HDATE_VF, 9, 10))
  )

table(dfNmb$month, useNA = "always")
table(dfNmb$year, useNA = "always")

#--------------------DEMOGRAPHIC VARIABLES-----------------
dfNmb <- dfNmb %>%
  rename(HHSex = H2)

table(dfNmb$HHSex)
attr(dfNmb$HHSex, "label") <- "Head of household sex"

#rural urban variable
table(dfNmb$rural_urban)

#Regional variable in dfNmb:
table(dfNmb$region)

#gender
table(dfNmb$sex)
attr(dfNmb$sex, "label") <- "Gender"

#Marital status
# Initialize the variable with NA
dfNmb$married <- 0
dfNmb$married[dfNmb$Q38 %in% c(1, 2) | dfNmb$Q40 == 1] <- 1
dfNmb$married[dfNmb$Q36 == 3] <- 0
table(dfNmb$married, useNA = "always")

# Display the table
table(dfNmb$married, useNA = "always")
# Labeling
attr(dfNmb$married, "label") <- "Currently married"

#child marriage
dfNmb <- dfNmb %>%
  mutate(childmar = ifelse( Q37 %in% c (13:17),1,0))
table(dfNmb$childmar, useNA = "always")
# Labeling
attr(dfNmb$childmar, "label") <- "Married before the age of 18"
table(dfNmb$childmar, dfNmb$sex, useNA = "always")

#food insecurity:
dfNmb$food_insec <- 0
dfNmb$food_insec[dfNmb$H53 == 1 | dfNmb$H52 == 1 | dfNmb$Q9A == 1] <- 1

# Generate a frequency table with missing values included
table(dfNmb$food_insec, useNA = "always")

#------Age variable
# Age group:

dfNmb$AgeGroup <- 0
dfNmb$AgeGroup[dfNmb$Q2 >= 13 & dfNmb$Q2 <= 17] <- 1
dfNmb$AgeGroup[dfNmb$Q2 >= 18 & dfNmb$Q2 <= 24] <- 2

# Labeling variables
attr(dfNmb$AgeGroup, "label") <- "age group of girls"
label_values <- c("13-17", "18-24")
attr(dfNmb$AgeGroup, "labels") <- label_values

# Displaying Q2 and AgeGroup for the first 30 rows
dfNmb[1:30, c("Q2", "AgeGroup")]

#Tabulating Q2 and AgeGroup:
table(dfNmb$Q2, dfNmb$AgeGroup)

# Education attainment
library(dplyr)

# Check missing values in each variable
table(!is.na(dfNmb$Q3)) # 11 missing values
table(!is.na(dfNmb$Q4)) # 137 missing values
table(!is.na(dfNmb$Q6)) # 3480 missing values
table(!is.na(dfNmb$Q7)) # 1849 missing values

# Recode values 98 and 99 as missing (.)
dfNmb$Q3_ <- ifelse(dfNmb$Q3 %in% c(98, 99), NA, dfNmb$Q3)
dfNmb$Q4_ <- ifelse(dfNmb$Q4 %in% c(98, 99), NA, dfNmb$Q4)
dfNmb$Q6_ <- ifelse(dfNmb$Q6 %in% c(98, 99), NA, dfNmb$Q6)
dfNmb$Q7_ <- ifelse(dfNmb$Q7 %in% c(98, 99), NA, dfNmb$Q7)

# Cross-tabulation for imputation
table(dfNmb$Q3_, dfNmb$Q7_, useNA = "always")
table(dfNmb$Q3_, dfNmb$Q6_, useNA = "always")
table(dfNmb$Q6_)
# Recode Q6_ and Q7_ to regroup into 1. primary or less, 2. secondary, 3. higher than secondary
dfNmb$Q6_ <- ifelse(dfNmb$Q3_ == 0, 1, dfNmb$Q6_)
dfNmb$Q6_ <- ifelse(dfNmb$Q6_ %in% c(0, 1, 2), 1, dfNmb$Q6_)
dfNmb$Q6_ <- ifelse(dfNmb$Q6_ %in% c(3, 4), 2, dfNmb$Q6_)
dfNmb$Q6_ <- ifelse(dfNmb$Q6_ == 5, 3, dfNmb$Q6_)

dfNmb$Q7_ <- ifelse(dfNmb$Q3_ == 0, 1, dfNmb$Q7_)
dfNmb$Q7_ <- ifelse(dfNmb$Q7_ %in% c(0, 1, 2), 1, dfNmb$Q7_)
dfNmb$Q7_ <- ifelse(dfNmb$Q7_ %in% c(3, 4), 2, dfNmb$Q7_)
dfNmb$Q7_ <- ifelse(dfNmb$Q7_ == 5, 3, dfNmb$Q7_)

# Replace missing values in Q6_ for those with the value of primary or less in Q7_
dfNmb$Q6_[dfNmb$Q7_ == 1 & is.na(dfNmb$Q6_)] <- 1
dfNmb$Q7_[dfNmb$Q6_ == 2 & is.na(dfNmb$Q7_)] <- 2
dfNmb$Q7_[dfNmb$Q6_ == 3 & is.na(dfNmb$Q7_)] <- 3

# Create a new variable "EducationEver"
dfNmb$EducationEver <- dfNmb$Q6_

# Define value labels for EducationEver
value_labels <- c("Completed primary school or less", "Completed secondary school", "Higher than secondary")
# Apply value labels to EducationEver
dfNmb$EducationEver <- factor(dfNmb$EducationEver, levels = c(1, 2, 3), labels = value_labels)

# Create a new variable "EducationEver2" with two categories from EducationEver
dfNmb$EducationEver2 <- ifelse(dfNmb$EducationEver %in% c("Completed secondary school", "Higher than secondary"), 1, 0)

# Drop the original EducationEver variable
dfNmb <- dfNmb %>% dplyr::select(-EducationEver)            

# Rename EducationEver2 to EducationEver
dfNmb <- dfNmb %>% rename(EducationEver = EducationEver2)
attr(dfNmb$EducationEver, "label") <- "Education attainment"
table(dfNmb$EducationEver, useNA = "always")

#--Current school enrollment
dfNmb$School_enrol <- ifelse(dfNmb$Q3_ == 2 & is.na(dfNmb$Q4_), 2, dfNmb$Q4_)
dfNmb$School_enrol[dfNmb$School_enrol == 2] <- 0
table(dfNmb$School_enrol, useNA = "always")

# Labeling variable
attr(dfNmb$School_enrol, "label") <- "School enrollemnt"

#---------Orphanhood variables

# Maternal orphanhood
dfNmb <- dfNmb %>%
  mutate(OrphMater = ifelse(Q15 == 1, 0, ifelse(Q15 == 2, 1,NA)))
# Label the variable
attr(dfNmb$OrphMater, "label") <- "Maternal orphanhood"
table(dfNmb$OrphMater, useNA = "always")  #missing values = 23

# Paternal Orphanhood
dfNmb <- dfNmb %>%
  mutate(OrphPater = ifelse(Q25 == 1, 0, ifelse(Q25 == 2, 1, NA)))
# Labeling variable
attr(dfNmb$OrphPater, "label") <- "Paternal orphanhood"
table(dfNmb$OrphPater, useNA = "always")  #missing values = 90
table(dfNmb$OrphMater, dfNmb$OrphPater, useNA = "always")

# Orphanhood variable
dfNmb$Orphan[dfNmb$OrphMater == 1 | dfNmb$OrphPater == 1] <- 1
dfNmb$Orphan[dfNmb$OrphMater == 1 & dfNmb$OrphPater == 1] <- 1
dfNmb$Orphan[dfNmb$OrphMater == 0 & dfNmb$OrphPater == 0] <- 0
dfNmb$Orphan[is.na(dfNmb$Orphan)] <- NA
table(dfNmb$Orphan, useNA = "always")  #missing values = 78 
#lost one parent = 1345 #lost both= 241 none= 3591 (if both needed replace 1 with 2 in orphanhood var second line)

# Labeling Orphan variable
attr(dfNmb$Orphan, "labels") <- "Orphanhood by one parent or both"

#self reported HIV positive
table(dfNmb$hivpositive)

#INFORMAL HOUSING VARIABLE:
dfNmb <- dfNmb %>%
  mutate(inf_housing = ifelse(H7A == 2, 1,  #electricity
                              # ifelse(H6 ==1, 1,   #sharing toilet 
                              ifelse(H10 %in% c(1:7), 1, 0)))  #roof material

table(dfNmb$inf_housing)

#--------Violence variables----------------
#-------------Life time sexual violence variables --------------
# Unwanted sexual touching
dfNmb$SV_Q600 <- ifelse(dfNmb$Q600 == 1, 1,
                        ifelse(dfNmb$Q600 == 2, 0, NA))

# Label the variable 'SV_Q600':
attr(dfNmb$SV_Q600, "label") <- "Unwanted sexual touching in a lifetime"
table(dfNmb$SV_Q600, useNA = "always")

#--------------Attempted forced sex in a lifetime:

# Recode variables Q700A and Q700B
dfNmb <- dfNmb %>%
  mutate(SV_Q700A = ifelse(Q700A == 1, 1, ifelse(Q700A == 2, 0, NA)),
         SV_Q700B = ifelse(Q700B == 1, 1, ifelse(Q700B == 2, 0, NA)))

# Calculate row totals for SV_Q700A and SV_Q700B
dfNmb$SV_AttRape <- rowSums(dfNmb[, c("SV_Q700A", "SV_Q700B")], na.rm = TRUE)
table(dfNmb$SV_Q700A, useNA = "always")
# Replace missing values and recode values greater than or equal to 1 to 1, and others to 0
dfNmb$SV_AttRape <- ifelse(is.na(dfNmb$SV_AttRape), NA,
                           ifelse(dfNmb$SV_AttRape >= 1, 1, 0))

# Label the variable SV_AttRape
attr(dfNmb$SV_AttRape, "label") <- "Attempted forced Sex"
table(dfNmb$SV_AttRape, useNA = "always")

#------------Physically forced sex in a lifetime:
dfNmb <- dfNmb %>%
  mutate(SV_800A = ifelse(Q800A == 1, 1, ifelse(Q800A == 2, 0, NA)),
         SV_800B = ifelse(Q800B == 1, 1, ifelse(Q800B == 2, 0, NA)))
#Calculate row totals for SV_800A and SV_800B
dfNmb$SV_Rape <- rowSums(dfNmb[, c("SV_800A", "SV_800B")], na.rm = TRUE)

dfNmb$SV_Rape <- ifelse(is.na(dfNmb$SV_Rape), NA,
                        ifelse(dfNmb$SV_Rape >=1, 1, 0))          
attr(dfNmb$SV_Rape, "label") <- "Physically forced sex"
table(dfNmb$SV_Rape, useNA = "always")

#-----------Pressured Sex in a lifetime--------------------------:

dfNmb <- dfNmb %>% 
  mutate(SV_900A = ifelse(Q900A == 1, 1, ifelse(Q900A == 2, 0, NA)),
         SV_900B = ifelse(Q900B == 1, 1, ifelse(Q900B == 2, 0, NA)))
table(dfNmb$SV_900B, useNA = "always")

dfNmb$SV_PreSex <- rowSums(dfNmb[, c("SV_900A","SV_900B")], na.rm = TRUE)
table(dfNmb$SV_PreSex, useNA = "always")

dfNmb$SV_PreSex <- ifelse(is.na(dfNmb$SV_PreSex), NA,
                          ifelse(dfNmb$SV_PreSex >=1, 1, 0))
table(dfNmb$SV_PreSex, useNA = "always")

# #-----------Alcohol facilitated Sex--------------------------:
dfNmb <- dfNmb %>% 
  mutate(SV_1000A = ifelse(Q1000A ==1,1, ifelse(Q1000A ==2,0,NA)),
         SV_1000B = ifelse(Q1000B ==1,1, ifelse(Q1000B ==2,0,NA)))
table(dfNmb$SV_1000B, useNA = "always")
dfNmb$SV_RapeAlcohol <- rowSums(dfNmb[,c("SV_1000A", "SV_1000B")], na.rm = TRUE)
table(dfNmb$SV_RapeAlcohol)
dfNmb$SV_RapeAlcohol <- ifelse(is.na(dfNmb$SV_RapeAlcohol), NA,
                               ifelse(dfNmb$SV_RapeAlcohol >=1,1,0))
table(dfNmb$SV_RapeAlcohol, useNA = "always") 
#-----------Any lifetime sexual violence-------------------:

dfNmb$Any_SV_lifetime <- rowSums(dfNmb[,c("SV_Q600","SV_AttRape", "SV_Rape", "SV_PreSex", "SV_RapeAlcohol")],
                                 na.rm =TRUE)
dfNmb$Any_SV_lifetime <- ifelse(is.na(dfNmb$Any_SV_lifetime), NA,
                                ifelse(dfNmb$Any_SV_lifetime >=1, 1, 0))
attr(dfNmb$Any_SV_lifetime, "label") <- "Any Lifetime sexual violence"
table(dfNmb$Any_SV_lifetime, useNA = "always") 

#-----------Sexual violence variables BY ANYONE in the PAST 12 MONTHS--------------

# Unwanted sexual touching in the last 12 months:
dfNmb$SV_1_12m <- NA
dfNmb$SV_1_12m[dfNmb$SV_Q600 == 0 | 
                 (dfNmb$SV_Q600 == 1 & 
                    (dfNmb$Q601 >= 1 & 
                       (dfNmb$Q602 == 2 | dfNmb$Q609 == 2)))] <- 0
dfNmb$SV_1_12m[dfNmb$SV_Q600 == 1 & 
                 (dfNmb$Q601 >= 1 & 
                    (dfNmb$Q602 == 1 | dfNmb$Q609 == 1))] <- 1

# Add a label to the column
attr(dfNmb$SV_1_12m, "label") <- "Unwanted Sexual Touching in the past 12 months"

# Generate a frequency table with missing values included
table(dfNmb$SV_1_12m, useNA = "always")

# Unwanted attempted sex in the last 12 months:
# Initialize the variable with NA
dfNmb$SV_2_12m <- NA
dfNmb$SV_2_12m[dfNmb$SV_AttRape == 0 | 
                 (dfNmb$SV_AttRape == 1 & 
                    (dfNmb$Q701 >= 1 & 
                       (dfNmb$Q702 == 2 | dfNmb$Q709 == 2)))] <- 0
dfNmb$SV_2_12m[dfNmb$SV_AttRape == 1 & 
                 (dfNmb$Q701 >= 1 & 
                    (dfNmb$Q702 == 1 | dfNmb$Q709 == 1))] <- 1
attr(dfNmb$SV_2_12m, "label") <- "Unwanted attempted sex in the last 12 months"
table(dfNmb$SV_2_12m, useNA = "always")

# Unwanted forced sex in the last 12 months:
# Initialize the variable with NA
dfNmb$SV_3_12m <- NA
dfNmb$SV_3_12m[dfNmb$SV_Rape == 0 | 
                 (dfNmb$SV_Rape == 1 & 
                    (dfNmb$Q801 >= 1 & 
                       (dfNmb$Q802 == 2 | dfNmb$Q810 == 2)))] <- 0
dfNmb$SV_3_12m[dfNmb$SV_Rape == 1 & 
                 (dfNmb$Q801 >= 1 & 
                    (dfNmb$Q802 == 1 | dfNmb$Q809 == 1))] <- 1
attr(dfNmb$SV_3_12m, "label") <- "Physically forced sex in the past 12 months"
table(dfNmb$SV_3_12m, useNA = "always")

# Pressured sex in the last 12 months:
dfNmb$SV_4_12m <- NA
dfNmb$SV_4_12m[dfNmb$SV_PreSex == 0 | 
                 (dfNmb$SV_PreSex == 1 & 
                    (dfNmb$Q901 >= 1 & 
                       (dfNmb$Q902 == 2 | dfNmb$Q909 == 2)))] <- 0
dfNmb$SV_4_12m[dfNmb$SV_PreSex == 1 & 
                 (dfNmb$Q901 >= 1 & 
                    (dfNmb$Q902 == 1 | dfNmb$Q909 == 1))] <- 1
attr(dfNmb$SV_4_12m, "label") <- "Pressured Sex in the last 12 months"
# Generate a frequency table with missing values included
table(dfNmb$SV_4_12m, useNA = "always")

# # Alcohol facilitated forced sex in the last 12 months:
dfNmb$SV_5_12m <- NA
dfNmb$SV_5_12m[dfNmb$SV_RapeAlcohol == 0 | 
                 (dfNmb$SV_RapeAlcohol == 1 & 
                    (dfNmb$Q1001 >= 1 & 
                       (dfNmb$Q1002 == 2 | dfNmb$Q1009 == 2)))] <- 0
dfNmb$SV_5_12m[dfNmb$SV_RapeAlcohol == 1 & 
                 (dfNmb$Q1001 >= 1 & 
                    (dfNmb$Q1002 == 1 | dfNmb$Q1009 == 1))] <- 1
attr(dfNmb$SV_5_12m, "label") <- "Alcohol facilitated forced Sex in the last 12 months"
table(dfNmb$SV_5_12m, useNA = "always")

# Any sexual violence in the past 12 months:
dfNmb$SV_Any <- rowSums(dfNmb[,c("SV_1_12m","SV_2_12m","SV_3_12m","SV_4_12m")], na.rm =TRUE)
dfNmb$SV_Any <- ifelse(is.na(dfNmb$SV_Any), NA,
                       ifelse(dfNmb$SV_Any >=1, 1, 0))

attr(dfNmb$SV_Any, "label") <- "Any sexual violence in the last 12 months"
table(dfNmb$SV_Any, useNA = "always")


#-----------Sexual violence variables BY NON-PARTNER AND PARTNER in the PAST 12 MONTHS--------------
# Unwanted sexual touching BY ANYONE in the last 12 months:
dfNmb <- dfNmb %>%
  mutate(
    SV_1any_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 2 | Q609 == 2)) ~ 0,  # No recent unwanted touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 1 | Q609 == 1)) &
        (Q605 %in% c(5:16, 21:32) | Q612 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases to NA
    )
  )

# Add a label to the column
attr(dfNmb$SV_1_12m, "label") <- "Unwanted Sexual Touching in the past 12 months"

# Generate a frequency table with missing values included
table(dfNmb$SV_1_12m, useNA = "always")

#### Unwanted sexual touching by an intimate partner in the last 12 months:
dfNmb <- dfNmb %>%
  mutate(
    SV_1IPV_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 2 | Q609 == 2)) ~ 0,  # No recent unwanted touching
      SV_Q600 == 1 & (Q601 >= 1 & (Q602 == 1 | Q609 == 1)) & 
        (Q605 %in% c(1: 4,17:20) | Q612 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_1IPV_12m, "label") <- "Unwanted Sexual Touching by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfNmb$SV_1IPV_12m, useNA = "always")

# Unwanted attempted sex by ANYONE in the last 12 months:
# Recode SV_2_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_2any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q709 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q709 == 1)) &
        (Q705 %in% c(5:16, 21:32) | Q712 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_2any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfNmb$SV_2any_12m, useNA = "always")

# Unwanted attempted sex by INTIMATE PARTNER in the last 12 months:
# Recode SV_2_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_2IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q709 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q709 == 1)) &
        (Q705 %in% c(1:4,17:20) | Q712 %in% c(1:4, 17:20)) ~ 1, 
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_2IPV_12m, "label") <- "Unwanted attempted sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable
table(dfNmb$SV_2IPV_12m, useNA = "always")

# Unwanted forced sex BY ANYONE in the last 12 months:
# Recode SV_3_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_3any_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 2 | Q810 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 1 | Q810 == 1)) &
        (Q805 %in% c(5:16, 21:32) | Q813 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfNmb$SV_3any_12m, "label") <- "Physically forced sex by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfNmb$SV_3any_12m, useNA = "always")

# Unwanted forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_3_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_3IPV_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 2 | Q810 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q801 >= 1 & (Q802 == 1 | Q810 == 1)) &
        (Q805 %in% c(1:4,17:20) | Q813 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfNmb$SV_3IPV_12m, "label") <- "Physically forced sex by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfNmb$SV_3IPV_12m, useNA = "always")

# Pressured sex BY ANYONE in the last 12 months:
# Recode SV_4_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_4any_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 2 | Q909 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 1 | Q909 == 1)) &
        (Q905 %in% c(5:16, 21:32) | Q912 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_4any_12m, "label") <- "Pressured Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfNmb$SV_4any_12m, useNA = "always")

# Pressured sex AN INTIMATE PARTNER in the last 12 months:
# Recode SV_4_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_4IPV_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 2 | Q909 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q901 >= 1 & (Q902 == 1 | Q909 == 1)) &
        (Q905 %in% c(1:4,17:20) | Q912 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_4IPV_12m, "label") <- "Pressured Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable 
table(dfNmb$SV_4IPV_12m, useNA = "always")

# # Alcohol facilitated forced sex BY ANYONE in the last 12 months:
# Recode SV_5_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_5any_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1009 == 2)) ~ 0,  # No recent alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1009 == 1)) &
        (Q1005 %in% c(5:16, 21:32) | Q1012 %in% c(5:16, 21:32)) ~ 1,   # Recent alcohol-facilitated forced sex
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_5any_12m, "label") <- "Alcohol facilitated forced Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfNmb$SV_5any_12m, useNA = "always")

# # Alcohol facilitated forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_5_12m based on conditions
dfNmb <- dfNmb %>%
  mutate(
    SV_5IPV_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1009 == 2)) ~ 0,  # No recent alcohol-facilitated forced sex
      SV_RapeAlcohol == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1009 == 1)) &
        (Q1005 %in% c(1:4,17:20) | Q1012 %in% c(1:4, 17:20)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfNmb$SV_5IPV_12m, "label") <- "Alcohol facilitated forced Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfNmb$SV_5IPV_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfNmb$Any_SV_12m <- rowSums(dfNmb[,c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m", "SV_5any_12m")], na.rm =TRUE)
dfNmb$Any_SV_12m <- ifelse(is.na(dfNmb$Any_SV_12m), NA,
                           ifelse(dfNmb$Any_SV_12m >=1, 1, 0))

attr(dfNmb$Any_SV_12m, "label") <- "Any sexual violence by anyone in the last 12 months"
table(dfNmb$Any_SV_12m, useNA = "always")

#  sexual violence BY AN INTIMATE PARTNER in the past 12 months:
dfNmb$SV_IPV_12m <- rowSums(dfNmb[,c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m", "SV_5IPV_12m")], na.rm =TRUE)
dfNmb$SV_IPV_12m <- ifelse(is.na(dfNmb$SV_IPV_12m), NA,
                           ifelse(dfNmb$SV_IPV_12m >=1, 1, 0))

attr(dfNmb$SV_IPV_12m, "label") <- "Any sexual violence by an intimate partner in the last 12 months"
table(dfNmb$SV_IPV_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfNmb$Any_SV <- rowSums(dfNmb[,c("SV_IPV_12m","Any_SV_12m")], na.rm =TRUE)
dfNmb$Any_SV <- ifelse(is.na(dfNmb$Any_SV), NA,
                       ifelse(dfNmb$Any_SV >=1, 1, 0))

attr(dfNmb$Any_SV, "label") <- "Any sexual violence by any perpetrator in the last 12 months"
table(dfNmb$Any_SV, useNA = "always")

#-------------Life time Physical violence variables --------------
# Physical IPV in a lifetime
vars <- c("Q100A", "Q100B", "Q100C", "Q100D")
for (var in vars) {
  dfNmb[[paste0("IPV_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$PV_IPV_lt <- rowSums(dfNmb[, grep("^IPV_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$PV_IPV_lt <- ifelse(is.na(dfNmb$PV_IPV_lt), NA, ifelse(dfNmb$PV_IPV_lt >= 1, 1, 0))

attr(dfNmb$PV_IPV_lt, "label") <- "Lifetime physical IPV"
table(dfNmb$PV_IPV_lt, useNA = "always")

# Physical peer violence in a lifetime
vars_peer <- c("Q110A", "Q110B", "Q110C", "Q110D")
for (var in vars_peer) {
  dfNmb[[paste0("ViolPeer_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$PV_Peers_lt <- rowSums(dfNmb[, grep("^ViolPeer_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$PV_Peers_lt <- ifelse(is.na(dfNmb$PV_Peers_lt), NA, ifelse(dfNmb$PV_Peers_lt >= 1, 1, 0))

attr(dfNmb$PV_Peers_lt, "label") <- "Lifetime peer physical violence"
table(dfNmb$PV_IPV_lt, useNA = "always")

# Physical family violence
vars_fam <- c("Q120A", "Q120B", "Q120C", "Q120D")
for (var in vars_fam) {
  dfNmb[[paste0("ViolF_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$PV_Fam_lt <- rowSums(dfNmb[, grep("^ViolF_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$PV_Fam_lt <- ifelse(is.na(dfNmb$PV_Fam_lt), NA, ifelse(dfNmb$PV_Fam_lt >= 1, 1, 0))

attr(dfNmb$PV_Fam_lt, "label") <- "Lifetime family physical violence"
table(dfNmb$PV_Fam_lt, useNA = "always")

# Community Physical Violence (adults in neighbourhood)
vars_com <- c("Q132A", "Q132B", "Q132C", "Q132D")
for (var in vars_com) {
  dfNmb[[paste0("ViolC_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$PV_Com_lt <- rowSums(dfNmb[, grep("^ViolC_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$PV_Com_lt <- ifelse(is.na(dfNmb$PV_Com_lt), NA, ifelse(dfNmb$PV_Com_lt >= 1, 1, 0))

attr(dfNmb$PV_Com_lt, "label") <- "Lifetime community physical violence"
table(dfNmb$PV_Com_lt, useNA = "always")

# Any physical violence in a lifetime:
dfNmb$Any_PV_lt <- rowSums(dfNmb[,c("PV_IPV_lt", "PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfNmb$Any_PV_lt <- ifelse(is.na(dfNmb$Any_PV_lt), NA,
                          ifelse(dfNmb$Any_PV_lt >=1, 1, 0))

attr(dfNmb$Any_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfNmb$Any_PV_lt, useNA = "always")

####.  Create JUST child physical violence variable: ####
# Child physical violence in a lifetime:
dfNmb$child_PV_lt <- rowSums(dfNmb[,c("PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfNmb$child_PV_lt <- ifelse(is.na(dfNmb$child_PV_lt), NA,
                            ifelse(dfNmb$child_PV_lt >=1, 1, 0))

attr(dfNmb$child_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfNmb$child_PV_lt, useNA = "always")

####.  Create JUST community physical violence variable: ####
# Child physical violence in a lifetime:
dfNmb$community_PV_lt <- rowSums(dfNmb[,c("PV_Peers_lt", "PV_Com_lt")], na.rm = TRUE)
dfNmb$community_PV_lt <- ifelse(is.na(dfNmb$community_PV_lt), NA,
                                ifelse(dfNmb$community_PV_lt >=1, 1, 0))

attr(dfNmb$community_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfNmb$community_PV_lt, useNA = "always")


#------------Physical violence variables in the last 12 months------------

# Physical IPV in the last 12 months:
vars_ipv <- c("Q100A1", "Q100B1", "Q100C1", "Q100D1")
for (var in vars_ipv) {
  dfNmb[[paste0(var, "1")]] <- ifelse(dfNmb[[var]] == 2, 2, ifelse(dfNmb[[var]] == 1, 1, NA))
  dfNmb[[paste0("IPV_", var, "1")]] <- ifelse(dfNmb[[paste0(var, "1")]] == 1, 1, ifelse(dfNmb[[paste0(var, "1")]] == 2, 0, NA))
}

dfNmb$PV_IPV_12m <- rowSums(dfNmb[, grep("^IPV_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$PV_IPV_12m <- ifelse(is.na(dfNmb$PV_IPV_12m), NA, ifelse(dfNmb$PV_IPV_12m >= 1, 1, 0))

attr(dfNmb$PV_IPV_12m, "label") <- "Physical IPV in the past 12 months"
table(dfNmb$PV_IPV_12m, useNA = "always")

# Physical peer violence in the last 12 months:
dfNmb$PV_Peers_12m <- NA
dfNmb$PV_Peers_12m[dfNmb$PV_Peers_lt == 0 | 
                     (dfNmb$PV_Peers_lt == 1 & 
                        (dfNmb$Q111 >= 1 & 
                           (dfNmb$Q112 == 2 | dfNmb$Q115 == 2)))] <- 0
dfNmb$PV_Peers_12m[dfNmb$PV_Peers_lt == 1 & 
                     (dfNmb$Q111 >= 1 & 
                        (dfNmb$Q112 == 1 | dfNmb$Q115 == 1))] <- 1
attr(dfNmb$PV_Peers_12m, "label") <- "Peers physical violence in the past 12 months"
table(dfNmb$PV_Peers_12m, useNA = "always")

# Physical family violence in the last 12 months:
dfNmb$PV_Fam_12m <- NA
dfNmb$PV_Fam_12m[dfNmb$PV_Fam_lt == 0 | 
                   (dfNmb$PV_Fam_lt == 1 & 
                      (dfNmb$Q121 >= 1 & 
                         (dfNmb$Q122 == 2 | dfNmb$Q126 == 2)))] <- 0
dfNmb$PV_Fam_12m[dfNmb$PV_Fam_lt == 1 & 
                   (dfNmb$Q121 >= 1 & 
                      (dfNmb$Q122 == 1 | dfNmb$Q126 == 1))] <- 1

# Add a descriptive label to the variable
attr(dfNmb$PV_Fam_12m, "label") <- "Family physical violence in the past 12 months"
table(dfNmb$PV_Fam_12m, useNA = "always")

# Generate a frequency table with missing values included
table(dfNmb$PV_Fam_12m, useNA = "always")

# Physical violence in the community in the last 12 months:
# Initialize the variable with NA
dfNmb$PV_Com_12m <- NA
dfNmb$PV_Com_12m[dfNmb$PV_Com_lt == 0 | 
                   (dfNmb$PV_Com_lt == 1 & 
                      (dfNmb$Q133 >= 1 & 
                         (dfNmb$Q134 == 2 | dfNmb$Q137 == 2)))] <- 0
dfNmb$PV_Com_12m[dfNmb$PV_Com_lt == 1 & 
                   (dfNmb$Q133 >= 1 & 
                      (dfNmb$Q134 == 1 | dfNmb$Q137 == 1))] <- 1
attr(dfNmb$PV_Com_12m, "label") <- "Physical violence in the community in the past 12 months"
table(dfNmb$PV_Com_12m, useNA = "always")

# Any past-year physical violence in the last 12 months:
dfNmb$PV_Any <- rowSums(dfNmb[, c("PV_IPV_12m", "PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfNmb$PV_Any <- ifelse(is.na(dfNmb$PV_Any), NA, ifelse(dfNmb$PV_Any >= 1, 1, 0))

attr(dfNmb$PV_Any, "label") <- "Any physical violence in the past 12 months"
table(dfNmb$PV_Any, useNA = "always")

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfNmb$child_PV_12m <- rowSums(dfNmb[, c("PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfNmb$child_PV_12m <- ifelse(is.na(dfNmb$child_PV_12m), NA, ifelse(dfNmb$child_PV_12m >= 1, 1, 0))

attr(dfNmb$child_PV_12m, "label") <- "Any physical violence in the past 12 months"
table(dfNmb$child_PV_12m, useNA = "always")

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfNmb$community_PV_12m <- rowSums(dfNmb[, c("PV_Peers_12m", "PV_Com_12m")], na.rm = TRUE)
dfNmb$community_PV_12m <- ifelse(is.na(dfNmb$community_PV_12m), NA, ifelse(dfNmb$community_PV_12m >= 1, 1, 0))

attr(dfNmb$community_PV_12m, "label") <- "Any physical violence in the past 12 months"
table(dfNmb$community_PV_12m, useNA = "always")

#-------------Life time Emotional violence variables --------------

#------ Any lifetime emotional violence by parent, adult caregiver or other adult relative
vars_EV_fam_lt <- c("Q300A", "Q300B", "Q300C")
for (var in vars_EV_fam_lt) {
  dfNmb[[paste0("EViolF_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$EV_Fam_lt <- rowSums(dfNmb[, grep("^EViolF_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$EV_Fam_lt <- ifelse(is.na(dfNmb$EV_Fam_lt), NA, ifelse(dfNmb$EV_Fam_lt >= 1, 1, 0))

attr(dfNmb$EV_Fam_lt, "label") <- "Any lifetime emotional violence by parent, adult caregiver or other adult relative"
table(dfNmb$EV_Fam_lt, useNA = "always")

#---- Any lifetime emotional violence by INTIMATE PARTNER:
vars_EV_IPV_lt <- c("Q310A", "Q310B", "Q310C", "Q310D", "Q310E")
for (var in vars_EV_IPV_lt) {
  dfNmb[[paste0("EIPV_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$EV_IPV_lt <- rowSums(dfNmb[, grep("^EIPV_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$EV_IPV_lt <- ifelse(is.na(dfNmb$EV_IPV_lt), NA, ifelse(dfNmb$EV_IPV_lt >= 1, 1, 0))

attr(dfNmb$EV_IPV_lt, "label") <- "Lifetime emotional IPV"
table(dfNmb$EV_IPV_lt, useNA = "always")

#------ Any lifetime emotional violence BY ANYONE
dfNmb$EV_Any_lt <- rowSums(dfNmb[, c("EV_IPV_lt", "EV_Fam_lt")], na.rm = TRUE)
dfNmb$EV_Any_lt <- ifelse(is.na(dfNmb$EV_Any_lt), NA, ifelse(dfNmb$EV_Any_lt >= 1, 1, 0))

attr(dfNmb$EV_Any_lt, "label") <- "Any lifetime emotional violence"
table(dfNmb$EV_Any_lt, useNA = "always")

#---------------------Emotional violence variables in the LAST 12 MONTHS:
#------ Emotional violence by parent, adult caregiver, or other adult relative in the past 12 months
# Initialize the variable with NA
dfNmb$EV_Fam_12m <- NA
dfNmb$EV_Fam_12m[dfNmb$EV_Fam_lt == 0 | 
                   (dfNmb$EV_Fam_lt == 1 & 
                      (dfNmb$Q301 >= 1 & 
                         (dfNmb$Q302 == 2 | dfNmb$Q306 == 2)))] <- 0
dfNmb$EV_Fam_12m[dfNmb$EV_Fam_lt == 1 & 
                   (dfNmb$Q301 >= 1 & 
                      (dfNmb$Q302 == 1 | dfNmb$Q306 == 1))] <- 1
attr(dfNmb$EV_Fam_12m, "label") <- "Family emotional violence in the past 12 months"
table(dfNmb$EV_Fam_12m, useNA = "always")

#------------Emotional violence by INTIMATE PARTNER in the LAST 12 MONTHS:
dfNmb$EV_IPV_12m <- NA
dfNmb$EV_IPV_12m[dfNmb$EV_IPV_lt == 0 | 
                   (dfNmb$EV_IPV_lt == 1 & 
                      (dfNmb$Q311 >= 1 & 
                         dfNmb$Q312 == 2))] <- 0
dfNmb$EV_IPV_12m[dfNmb$EV_IPV_lt == 1 & 
                   (dfNmb$Q311 >= 1 & 
                      dfNmb$Q312 == 1)] <- 1
attr(dfNmb$EV_IPV_12m, "label") <- "Any emotional violence by parent, adult caregiver, or other adult relative in the past 12 months"
table(dfNmb$EV_IPV_12m, useNA = "always")

#------------Emotional violence by PEER in the LAST 12 MONTHS:
vars_EV_peer_12m <- c("Q315A", "Q315B", "Q315C")
for (var in vars_EV_peer_12m) {
  dfNmb[[paste0("EViolPeer_", var)]] <- ifelse(dfNmb[[var]] == 1, 1, ifelse(dfNmb[[var]] == 2, 0, NA))
}

dfNmb$EV_Peer_12m <- rowSums(dfNmb[, grep("^EViolPeer_", colnames(dfNmb))], na.rm = TRUE)
dfNmb$EV_Peer_12m <- ifelse(is.na(dfNmb$EV_Peer_12m), NA, ifelse(dfNmb$EV_Peer_12m >= 1, 1, 0))

attr(dfNmb$EV_Peer_12m, "label") <- "Lifetime emotional violence by peers"
table(dfNmb$EV_Peer_12m, useNA = "always")

#------ Any emotional violence BY EVRYONE in the past 12 months
dfNmb$EV_Any <- rowSums(dfNmb[, c("EV_IPV_12m", "EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfNmb$EV_Any <- ifelse(is.na(dfNmb$EV_Any), NA, ifelse(dfNmb$EV_Any >= 1, 1, 0))

attr(dfNmb$EV_Any, "label") <- "Any emotional violence in the past 12 months"
table(dfNmb$EV_Any, useNA = "always")

#------ Any emotional violence BY ANYONE in the past 12 months
dfNmb$EV_Any_12m <- rowSums(dfNmb[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfNmb$EV_Any_12m <- ifelse(is.na(dfNmb$EV_Any_12m), NA, ifelse(dfNmb$EV_Any_12m >= 1, 1, 0))

attr(dfNmb$EV_Any_12m, "label") <- "Any emotional violence in the past 12 months"
table(dfNmb$EV_Any_12m, useNA = "always")


#Select the key variables of interest
dfNmb1 <- dplyr::select(dfNmb,
                        Q2,
                        HHSex,
                        inf_housing,
                        strata,
                        cluster,
                        sampleweight,
                        HDATE_VF,
                        month,
                        year,
                        region,
                        country,
                        AgeGroup,
                        sex,
                        married,
                        food_insec,
                        rural_urban,
                        EducationEver,
                        School_enrol,
                        Orphan,
                        Any_SV_12m,
                        EV_Any_12m,
                        child_PV_12m,
                        community_PV_12m,
                        PV_Fam_12m,
                        PV_IPV_12m,
                        SV_IPV_12m,
                        EV_IPV_12m,
                        SV_Any,
                        EV_Any,
                        PV_Any)

#############-----------------------Mozambique------------------------#########

#rename survey weights:
dfMoz <- dfMoz %>%
  rename( strata = Strata,
          cluster = Cluster
  )

dfMoz <- dfMoz[complete.cases(dfMoz$sampleweight), ]
table(dfMoz$sampleweight, useNA = "always")

# Create a survey design object using svydesign
svy_design <- svydesign(
  ids = ~ cluster,   # Clustering variable
  strata = ~strata,   # Stratification variable
  weights = ~sampleweight, # Survey weight variable
  nest = TRUE, 
  data = dfMoz
)

#create a WI variable with the observations of n
dfMoz <- dfMoz %>%
  mutate(WI = row_number())

#Apply weights to WI using the individual weight variable
sum_w <- sum(dfMoz$WI * dfMoz$sampleweight)  # Calculate the sum of weighted numbers
dfMoz$WI_1 <- sum_w  # Replace WI column with the sum
table(dfMoz$WI_1)

#create country variable
dfMoz$country <- "Mozambique"

#Examining new variables in the dataset: rural urban + interview date:
dfMoz <- dfMoz %>%
  rename(HDATE_VF = HDATE_F)
table(dfMoz$HDATE_VF)

# Extracting year, month, and day information
dfMoz <- dfMoz %>% 
  mutate(
    year = as.integer(substr(HDATE_VF, 1, 4)),
    month = as.integer(substr(HDATE_VF, 6, 7)),
    day = as.integer(substr(HDATE_VF, 9, 10))
  )

table(dfLso$year, useNA = "always")   #7 NAs
table(dfLso$month, useNA = "always")

#--------------------DEMOGRAPHIC VARIABLES-----------------
#head of household sex
dfMoz <- dfMoz %>%
  rename(HHSex = H2)

table(dfMoz$HHSex)
attr(dfMoz$HHSex, "label") <- "Head of household sex"

#Gender
dfMoz <- dfMoz %>%
  rename(sex = Sex)
attr(dfMoz$sex, "label") <- "Gender"

# Marital status
dfMoz$married <- 0
dfMoz$married[dfMoz$marnow %in% c(1, 2)] <- 1
dfMoz$married[dfMoz$marnow %in% c(3, 4)] <- 0

# Display the table
table(dfMoz$married, useNA = "always")

# Labeling
attr(dfMoz$married, "label") <- "Currently married or living with someone as if married"

#child marriage
dfMoz <- dfMoz %>%
  mutate(childmar = ifelse(marage %in% c(13:17), 1, 0))

#Labeling
attr(dfMoz$childmar, "label") <- "married or living with someone as if married before the age of 18"
table(dfMoz$childmar, useNA = "always")

#food insecurity:
# Initialize the variable with 0
dfMoz$food_insec <- 0
dfMoz$food_insec[dfMoz$H52 == 1 | dfMoz$H53 == 1] <- 1
table(dfMoz$food_insec, useNA = "always")

#Rural urban variable
table(dfMoz$rural_urban, useNA = "always")

#Regional variable 
table(dfMoz$region)

#----Age variable
# Age group:
dfMoz$AgeGroup <- 0
dfMoz$AgeGroup[dfMoz$age >= 13 & dfMoz$age <= 17] <- 1
dfMoz$AgeGroup[dfMoz$age >= 18 & dfMoz$age <= 24] <- 2

# Labeling variables
attr(dfMoz$AgeGroup, "label") <- "age group of girls"
label_values <- c("13-17", "18-24")
attr(dfMoz$AgeGroup, "labels") <- label_values

# Displaying age and AgeGroup for the first 30 rows
dfMoz[1:30, c("age", "AgeGroup")]

#Tabulating age and AgeGroup:
table(dfMoz$age, dfMoz$AgeGroup)

#Rename age variable
dfMoz <- dfMoz %>% 
  rename(Q2 = age)

# Check the current levels of AgeGroup
table(dfMoz$AgeGroup, useNA = "always")

# Modify AgeGroup to have only 1 and 2 categories with 2 in 0 as NAs
dfMoz <- dfMoz %>%
  mutate(AgeGroup = ifelse(AgeGroup == 0, NA, AgeGroup))

# Check the new levels of AgeGroup
table(dfMoz$AgeGroup, useNA = "always")

#--------------- Education attainment

# Check missing values in each variable
table(!is.na(dfMoz$eversch)) # no missing values issue (only 1 DNK and 3 DECLINED)
table(!is.na(dfMoz$currsch)) # 141 missing, 1 DNK and 1 DECLINED
table(!is.na(dfMoz$schhl)) # 5,214 missing dfMoz (more than 60%), 4 DNK
table(!is.na(dfMoz$schcl)) # 3,497 missing dfMoz, 1 DNK and 4 DECLINED

# Recode values 98 and 99 as missing (.)
dfMoz$eversch_ <- ifelse(dfMoz$eversch %in% c(98, 99), NA, dfMoz$eversch)
dfMoz$currsch_ <- ifelse(dfMoz$currsch %in% c(98, 99), NA, dfMoz$currsch)
dfMoz$schhl_ <- ifelse(dfMoz$schhl %in% c(98, 99), NA, dfMoz$schhl)
dfMoz$schcl_ <- ifelse(dfMoz$schcl %in% c(98, 99), NA, dfMoz$schcl)


# Cross-tabulation for imputation
cross_table <- table(dfMoz$eversch_, dfMoz$schhl_, useNA = "always")
print(cross_table)

# Recode schhl_ and schcl_ to regroup into 1. primary or less, 2. secondary, 3. higher than secondary
dfMoz$schhl_ <- ifelse(dfMoz$eversch_ == 2, 0, dfMoz$schhl_)
dfMoz$schhl_ <- ifelse(dfMoz$schhl_ %in% c(0, 1, 2), 1, dfMoz$schhl_)
dfMoz$schhl_ <- ifelse(dfMoz$schhl_ %in% c(3, 4), 2, dfMoz$schhl_)
dfMoz$schhl_ <- ifelse(dfMoz$schhl_ == 5, 3, dfMoz$schhl_)

dfMoz$schcl_ <- ifelse(dfMoz$eversch == 2, 0, dfMoz$schcl_)
dfMoz$schcl_ <- ifelse(dfMoz$schcl_ %in% c(0, 1, 2), 1, dfMoz$schcl_)
dfMoz$schcl_ <- ifelse(dfMoz$schcl_ %in% c(3, 4), 2, dfMoz$schcl_)
dfMoz$schcl_ <- ifelse(dfMoz$schcl_ == 5, 3, dfMoz$schcl_)

# Replace missing values in schhl_ for those with the value of primary or less in schcl_
dfMoz$schhl_[dfMoz$schcl_ == 1 & is.na(dfMoz$schhl_)] <- 1
dfMoz$schhl_[dfMoz$schcl_ == 2 & is.na(dfMoz$schhl_)] <- 2
dfMoz$schhl_[dfMoz$schcl_ == 3 & is.na(dfMoz$schhl_)] <- 3

# Create a new variable "EducationEver"
dfMoz$EducationEver <- dfMoz$schhl_

# Define value labels for EducationEver
value_labels <- c("Completed primary school or less", "Completed secondary school", "Higher than secondary")
names(value_labels) <- c(1, 2, 3)

# Apply value labels to EducationEver
dfMoz$EducationEver <- factor(dfMoz$EducationEver, levels = c(1, 2, 3), labels = value_labels)

# Create a new variable "EducationEver2" with two categories
dfMoz$EducationEver2 <- ifelse(dfMoz$EducationEver %in% c("Completed secondary school", "Higher than secondary"), 1, 0)

# Drop the original EducationEver variable
dfMoz <- dfMoz %>% dplyr::select(-EducationEver)            

# Rename EducationEver2 to EducationEver
dfMoz <- dfMoz %>% rename(EducationEver = EducationEver2)
table(dfMoz$EducationEver, useNA = "always")

#--Current school enrollment
dfMoz$School_enrol <- ifelse(dfMoz$eversch == 2 & is.na(dfMoz$currsch), 2, dfMoz$currsch)
dfMoz$School_enrol[dfMoz$School_enrol == 2] <- 0
# Labeling variable
attr(dfMoz$School_enrol, "label") <- "School enrollemnt"
table(dfMoz$School_enrol, useNA = "always")

#---------Orphanhood variables
#Maternal orphanhood:
dfMoz$OrphMater <- 0
dfMoz$OrphMater[dfMoz$mlive == 2] <- 1

# Labeling variable
attr(dfMoz$OrphMater, "label") <- "Maternal orphanhood"

# Paternal Orphanhood
dfMoz$OrphPater <- 0
dfMoz$OrphPater[dfMoz$flive == 2] <- 1

# Labeling variable
attr(dfMoz$OrphPater, "label") <- "Paternal orphanhood"

# Orphanhood variable
# Orphanhood labels
labels <- c("None", "Lost one parent", "Lost both parents")

# Orphanhood variable
dfMoz$Orphan <- 0
dfMoz$Orphan[dfMoz$OrphMater == 1 | dfMoz$OrphPater == 1] <- 1
dfMoz$Orphan[dfMoz$OrphMater == 1 & dfMoz$OrphPater == 1] <- 1

# Labeling variable
levels(dfMoz$Orphan) <- labels
attr(dfMoz$Orphan,"label") <- "Orphanhood"

#--self reported HIV positive
table(dfMoz$hivpos, useNA = "always")

#informal housing:
dfMoz <- dfMoz %>%
  mutate(inf_housing = ifelse(H7A == 2, 1,  #electricity
                              ifelse(H10 %in% c(1:3), 1, 0)))   #roof material

table(dfMoz$inf_housing, useNA = "always")
table(dfMoz$H7A)

#--------Violence variables----------------
#-------------Life time sexual violence variables --------------

# Unwanted sexual touching in a lifetime:
dfMoz$SV_Q600 <- ifelse(dfMoz$svtch == 1, 1, ifelse(dfMoz$svtch == 2, 0, NA))
# Label the variable 'SV_Q600':
attr(dfMoz$SV_Q600, "label") <- "Unwanted sexual touching in a lifetime"

table(dfMoz$SV_Q600, useNA = "always")
#--------------Attempted forced sex in a lifetime:

# Recode variables Q700A and Q700B
dfMoz <- dfMoz %>%
  mutate(SV_Q700A = ifelse(svaffsx == 1, 1, ifelse(svaffsx == 2, 0, NA)),
         SV_Q700B = ifelse(svafsany == 1, 1, ifelse(svafsany == 2, 0, NA)))

# Calculate row totals for SV_Q700A and SV_Q700B
dfMoz$SV_AttRape <- rowSums(dfMoz[, c("SV_Q700A", "SV_Q700B")], na.rm = TRUE)

# Replace missing values and recode values greater than or equal to 1 to 1, and others to 0
dfMoz$SV_AttRape <- ifelse(is.na(dfMoz$SV_AttRape), NA,
                           ifelse(dfMoz$SV_AttRape >= 1, 1, 0))

# Label the variable SV_AttRape
attr(dfMoz$SV_AttRape, "label") <- "Attempted forced Sex"
table(dfMoz$SV_AttRape, useNA = "always")
#------------Physically forced sex in a lifetime:
dfMoz <- dfMoz %>%
  mutate(SV_800A = ifelse(svpfssx == 1, 1, ifelse(svpfssx == 2, 0, NA)),
         SV_800B = ifelse(svpfsany == 1, 1, ifelse(svpfsany == 2, 0, NA)))

#Calculate row totals for SV_800A and SV_800B
dfMoz$SV_Rape <- rowSums(dfMoz[, c("SV_800A", "SV_800B")], na.rm = TRUE)

dfMoz$SV_Rape <- ifelse(is.na(dfMoz$SV_Rape), NA,
                        ifelse(dfMoz$SV_Rape >=1, 1, 0))          
attr(dfMoz$SV_Rape, "label") <- "Physically forced sex"
table(dfMoz$SV_Rape, useNA = "always")
#-----------Pressured Sex--------------------------:

dfMoz <- dfMoz %>% 
  mutate(SV_900A = ifelse(svprssx == 1, 1, ifelse(svprssx == 2, 0, NA)),
         SV_900B = ifelse(svprsany == 1, 1, ifelse(svprsany == 2, 0, NA)))

dfMoz$SV_PreSex <- rowSums(dfMoz[, c("SV_900A","SV_900B")], na.rm = TRUE)

dfMoz$SV_PreSex <- ifelse(is.na(dfMoz$SV_PreSex), NA,
                          ifelse(dfMoz$SV_PreSex >=1, 1, 0))

table(dfMoz$SV_PreSex, useNA = "always")
#-----------Alcohol facilitated Sex--------------------------:
dfMoz <- dfMoz %>% 
  mutate(SV_1000A = ifelse(svaffsx ==1,1, ifelse(svafssx ==2,0,NA)),
         SV_1000B = ifelse(svafsxany ==1,1, ifelse(svafsxany ==2,0,NA)))

dfMoz$SV_RapeAlcohol <- rowSums(dfMoz[,c("SV_1000A", "SV_1000B")], na.rm = TRUE)

dfMoz$SV_RapeAlcohol <- ifelse(is.na(dfMoz$SV_RapeAlcohol), NA,
                               ifelse(dfMoz$SV_RapeAlcohol >=1,1,0))
table(dfMoz$SV_RapeAlcohol, useNA = "always")

#-----------Any lifetime sexual violence-------------------:

dfMoz$Any_SV_lifetime <- rowSums(dfMoz[,c("SV_Q600","SV_AttRape", "SV_Rape", "SV_PreSex", "SV_RapeAlcohol")],
                                 na.rm =TRUE)
dfMoz$Any_SV_lifetime <- ifelse(is.na(dfMoz$Any_SV_lifetime), NA,
                                ifelse(dfMoz$Any_SV_lifetime >=1, 1, 0))
attr(dfMoz$Any_SV_lifetime, "label") <- "Any Lifetime sexual violence"
table(dfMoz$Any_SV_lifetime, useNA = "always")

#-----------Sexual violence variables in the PAST 12 MONTHS--------------

# Unwanted sexual touching in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_1_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (SV1TME >= 1 & (SV112M == 2 | SV1FT12 == 2)) ~ 0,  # No recent unwanted sexual touching
      SV_Q600 == 1 & (SV1TME >= 1 & (SV112M == 1 | SV1FT12 == 1)) ~ 1,  # Recent unwanted sexual touching
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_1_12m, "label") <- "Unwanted Sexual Touching in the past 12 months"
table(dfMoz$SV_1_12m, useNA = "always")
# Unwanted attempted sex in the last 12 months:
# Recode SV_2_12m based on conditions
dfMoz <- dfMoz %>%
  mutate(
    SV_2_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No unwanted attempted sex
      SV_AttRape == 1 & (SV2TME >= 1 & (SV212M == 2 | SV2FT12 == 2)) ~ 0,  # No recent unwanted attempted sex
      SV_AttRape == 1 & (SV2TME >= 1 & (SV212M == 1 | SV2FT12 == 1)) ~ 1,  # Recent unwanted attempted sex
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_2_12m, "label") <- "Unwanted attempted sex in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_2_12m, useNA = "always")

# Unwanted forced sex in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_3_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (SV3TME >= 1 & (SV312M == 2 | SV3FT12 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (SV3TME >= 1 & (SV312M == 1 | SV3FT12 == 1)) ~ 1,  # Recent physically forced sex
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_3_12m, "label") <- "Physically forced sex in the past 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_3_12m, useNA = "always")

# Pressured sex in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_4_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (SV4TME >= 1 & (SV412M == 2 | SV4FT12 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (SV4TME >= 1 & (SV412M == 1 | SV4FT12 == 1)) ~ 1,  # Recent pressured sex
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_4_12m, "label") <- "Pressured Sex in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_4_12m, useNA = "always")

# Alcohol facilitated sex in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_5_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No pressured sex
      SV_RapeAlcohol == 1 & (SV5TME >= 1 & (SV512M == 2 | SV5FT12 == 2)) ~ 0,  # No recent pressured sex
      SV_RapeAlcohol == 1 & (SV5TME >= 1 & (SV512M == 1 | SV5FT12 == 1)) ~ 1,  # Recent pressured sex
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_5_12m, "label") <- "Pressured Sex in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_5_12m, useNA = "always")

# Any sexual violence in the past 12 months:
dfMoz$SV_Any <- rowSums(dfMoz[,c("SV_1_12m","SV_2_12m", "SV_3_12m","SV_4_12m", "SV_5_12m")], na.rm =TRUE)
dfMoz$SV_Any <- ifelse(is.na(dfMoz$SV_Any), NA,
                       ifelse(dfMoz$SV_Any >=1, 1, 0))

attr(dfMoz$SV_Any, "label") <- "Any sexual violence in the last 12 months"
table(dfMoz$SV_Any, useNA = "always")


###SEXUAL VIOLENCE VARIABLES BY PARTNER AND NON-PARTNER:

# Unwanted sexual touching BY ANYONE in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_1any_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted sexual touching
      SV_Q600 == 1 & (SV1TME >= 1 & (SV112M == 2 | SV1FT12 == 2)) ~ 0,  # No recent unwanted sexual touching
      SV_Q600 == 1 & (SV1TME >= 1 & (SV112M == 1 | SV1FT12 == 1)) &  # Recent unwanted sexual touching
        (SV1WHO %in% c(5:20, 25:40) | SV1FTR %in% c(5:20, 25:40)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_1any_12m, "label") <- "Unwanted Sexual Touching in the past 12 months"
table(dfMoz$SV_1any_12m, useNA = "always")

# Unwanted sexual touching by an intimate partner in the last 12 months:
# Recode SV_2_12m based on conditions
dfMoz <- dfMoz %>%
  mutate(
    SV_1IPV_12m = case_when(
      SV_Q600 == 0 ~ 0,  # No unwanted attempted sex
      SV_Q600 == 1 & (SV1TME >= 1 & (SV112M == 2 | SV1FT12 == 2)) ~ 0,  # No recent unwanted attempted sex
      SV_Q600 == 1 & (SV1TME >= 1 & (SV112M == 1 | SV1FT12 == 1)) &  # Recent unwanted attempted sex
        (SV1WHO %in% c(1:4, 21:24) | SV1FTR %in% c(1:4, 21:24)) ~ 1,  # Recent unwanted touching by an intimate partner  # Recent unwanted touching
      TRUE ~ NA_real_  # All other cases to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_1IPV_12m, "label") <- "Unwanted Sexual Touching by an intimate partner in the past 12 months"
table(dfMoz$SV_1IPV_12m, useNA = "always")

# Unwanted attempted sex BY ANYONE in the last 12 months:
# Recode SV_2_12m based on conditions
dfMoz <- dfMoz %>%
  mutate(
    SV_2any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No unwanted attempted sex
      SV_AttRape == 1 & (SV2TME >= 1 & (SV212M == 2 | SV2FT12 == 2)) ~ 0,  # No recent unwanted attempted sex
      SV_AttRape == 1 & (SV2TME >= 1 & (SV212M == 1 | SV2FT12 == 1)) &  # Recent unwanted attempted sex
        (SV2WHO %in% c(5:20, 25:40) | SV2FTR %in% c(5:20, 25:40)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_2any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_2any_12m, useNA = "always")

# Unwanted attempted sex by an intimate partner in the last 12 months:
# Recode SV_2_12m based on conditions
dfMoz <- dfMoz %>%
  mutate(
    SV_2IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No unwanted attempted sex
      SV_AttRape == 1 & (SV2TME >= 1 & (SV212M == 2 | SV2FT12 == 2)) ~ 0,  # No recent unwanted attempted sex
      SV_AttRape == 1 & (SV2TME >= 1 & (SV212M == 1 | SV2FT12 == 1)) &  # Recent unwanted attempted sex
        (SV2WHO %in% c(1:4, 21:24) | SV2FTR %in% c(1:4, 21:24)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_2IPV_12m, "label") <- "Unwanted attempted sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_2IPV_12m, useNA = "always")

# Unwanted physically forced sex BY ANYONE in the last 12 months:
# Recode SV_3_12m based on conditions
dfMoz <- dfMoz %>%
  mutate(
    SV_3any_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (SV3TME >= 1 & (SV312M == 2 | SV3FT12 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (SV3TME >= 1 & (SV312M == 1 | SV3FT12 == 1)) &  # Recent physically forced sex
        (SV3WHO %in% c(5:20, 25:40) | SV3FTR %in% c(5:20, 25:40)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_3any_12m, "label") <- "Physically forced sex BY ANYONE in the past 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_3any_12m, useNA = "always")

# Unwanted physically forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_3_12m based on conditions
dfMoz <- dfMoz %>%
  mutate(
    SV_3IPV_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (SV3TME >= 1 & (SV312M == 2 | SV3FT12 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (SV3TME >= 1 & (SV312M == 1 | SV3FT12 == 1)) & # Recent physically forced sex
        (SV3WHO %in% c(1:4, 21:24) | SV3FTR %in% c(1:4, 21:24)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_3IPV_12m, "label") <- "Physically forced sex by an intimate partner in the past 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_3IPV_12m, useNA = "always")

# Pressured sex BY ANYONE in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_4any_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (SV4TME >= 1 & (SV412M == 2 | SV4FT12 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (SV4TME >= 1 & (SV412M == 1 | SV4FT12 == 1)) & # Recent pressured sex
        (SV4WHO %in% c(5:20, 25:40) | SV4FTR %in% c(5:20, 25:40)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_4any_12m, "label") <- "Pressured Sex in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_4any_12m, useNA = "always")

# Pressured sex BY AN INTIMATE PARTNER in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_4IPV_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (SV4TME >= 1 & (SV412M == 2 | SV4FT12 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (SV4TME >= 1 & (SV412M == 1 | SV4FT12 == 1)) & # Recent pressured sex
        (SV4WHO %in% c(1:4, 21:24) | SV4FTR %in% c(1:4, 21:24)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_4IPV_12m, "label") <- "Pressured Sex by any intimate partner in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_4IPV_12m, useNA = "always")

# Alcohol facilitated forced sex BY ANYONE in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_5any_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No pressured sex
      SV_RapeAlcohol == 1 & (SV5TME >= 1 & (SV512M == 2 | SV5FT12 == 2)) ~ 0,  # No recent pressured sex
      SV_RapeAlcohol == 1 & (SV5TME >= 1 & (SV512M == 1 | SV5FT12 == 1)) & # Recent pressured sex
        (SV5WHO %in% c(5:20, 25:40) | SV5FTR %in% c(5:20, 25:40)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_5any_12m, "label") <- "Pressured Sex in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_5any_12m, useNA = "always")

# Alcohol facilitated forced sex BY an intimate partner in the last 12 months:
dfMoz <- dfMoz %>%
  mutate(
    SV_5IPV_12m = case_when(
      SV_RapeAlcohol == 0 ~ 0,  # No pressured sex
      SV_RapeAlcohol == 1 & (SV5TME >= 1 & (SV512M == 2 | SV5FT12 == 2)) ~ 0,  # No recent pressured sex
      SV_RapeAlcohol == 1 & (SV5TME >= 1 & (SV512M == 1 | SV5FT12 == 1)) & # Recent pressured sex
        ( SV5WHO %in% c(1:4, 21:24) | SV5FTR %in% c(1:4, 21:24)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfMoz$SV_5IPV_12m, "label") <- "Pressured Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfMoz$SV_5IPV_12m, useNA = "always")

# Any sexual violence by ANYONE in the past 12 months:
dfMoz$Any_SV_12m <- rowSums(dfMoz[,c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m", "SV_5any_12m")], na.rm =TRUE)
dfMoz$Any_SV_12m <- ifelse(is.na(dfMoz$Any_SV_12m), NA,
                           ifelse(dfMoz$Any_SV_12m >=1, 1, 0))

attr(dfMoz$Any_SV_12m, "label") <- "Any sexual violence by anyone in the last 12 months"
table(dfMoz$Any_SV_12m, useNA = "always")

# Any sexual violence by AN INTIMATE PARTNER in the past 12 months:
dfMoz$SV_IPV_12m <- rowSums(dfMoz[,c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m", "SV_5IPV_12m")], na.rm =TRUE)
dfMoz$SV_IPV_12m <- ifelse(is.na(dfMoz$SV_IPV_12m), NA,
                           ifelse(dfMoz$SV_IPV_12m >=1, 1, 0))

attr(dfMoz$SV_IPV_12m, "label") <- "Any sexual violence by an intimate partner in the last 12 months"
table(dfMoz$SV_IPV_12m, useNA = "always")

#-------------Life time Physical violence variables --------------
# Physical IPV in a lifetime
vars <- c("PV1HRT", "PV1OBJ", "PV1CSD", "PV1WPN")
for (var in vars) {
  dfMoz[[paste0("IPV_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$PV_IPV_lt <- rowSums(dfMoz[, grep("^IPV_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$PV_IPV_lt <- ifelse(is.na(dfMoz$PV_IPV_lt), NA, ifelse(dfMoz$PV_IPV_lt >= 1, 1, 0))

attr(dfMoz$PV_IPV_lt, "label") <- "Lifetime physical IPV"
table(dfMoz$PV_IPV_lt, useNA = "always")

# Physical peer violence in a lifetime
vars_peer <- c("PV2HRT", "PV2OBJ", "PV2CSD", "PV2WPN")
for (var in vars_peer) {
  dfMoz[[paste0("ViolPeer_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$PV_Peers_lt <- rowSums(dfMoz[, grep("^ViolPeer_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$PV_Peers_lt <- ifelse(is.na(dfMoz$PV_Peers_lt), NA, ifelse(dfMoz$PV_Peers_lt >= 1, 1, 0))

attr(dfMoz$PV_Peers_lt, "label") <- "Lifetime peer physical violence"
table(dfMoz$PV_Peers_lt, useNA = "always")

# Physical family violence
vars_fam <- c("PV3HRT", "PV3OBJ", "PV3CSD", "PV3WPN")
for (var in vars_fam) {
  dfMoz[[paste0("ViolF_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$PV_Fam_lt <- rowSums(dfMoz[, grep("^ViolF_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$PV_Fam_lt <- ifelse(is.na(dfMoz$PV_Fam_lt), NA, ifelse(dfMoz$PV_Fam_lt >= 1, 1, 0))

attr(dfMoz$PV_Fam_lt, "label") <- "Lifetime family physical violence"
table(dfMoz$PV_Fam_lt , useNA = "always")
# Community Physical Violence (adults in neighbourhood)
vars_com <- c("PV4HRT", "PV4OBJ", "PV4CSD", "PV4WPN")
for (var in vars_com) {
  dfMoz[[paste0("ViolC_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$PV_Com_lt <- rowSums(dfMoz[, grep("^ViolC_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$PV_Com_lt <- ifelse(is.na(dfMoz$PV_Com_lt), NA, ifelse(dfMoz$PV_Com_lt >= 1, 1, 0))

attr(dfMoz$PV_Com_lt, "label") <- "Lifetime community physical violence"
table(dfMoz$PV_Com_lt , useNA = "always")

# Any physical violence in a lifetime:
dfMoz$Any_PV_lt <- rowSums(dfMoz[,c("PV_IPV_lt", "PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfMoz$Any_PV_lt <- ifelse(is.na(dfMoz$Any_PV_lt), NA,
                          ifelse(dfMoz$Any_PV_lt >=1, 1, 0))

attr(dfMoz$Any_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfMoz$Any_PV_lt , useNA = "always")

####  Create JUST child physical violence variable: ####
# Child physical violence in a lifetime:
dfMoz$child_PV_lt <- rowSums(dfMoz[,c("PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfMoz$child_PV_lt <- ifelse(is.na(dfMoz$child_PV_lt), NA,
                            ifelse(dfMoz$child_PV_lt >=1, 1, 0))

attr(dfMoz$child_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfMoz$child_PV_lt, useNA = "always")

####  Create JUST communtiy physical violence variable: ####
# Child physical violence in a lifetime:
dfMoz$community_PV_lt <- rowSums(dfMoz[,c("PV_Peers_lt", "PV_Com_lt")], na.rm = TRUE)
dfMoz$community_PV_lt <- ifelse(is.na(dfMoz$community_PV_lt), NA,
                                ifelse(dfMoz$community_PV_lt >=1, 1, 0))

attr(dfMoz$community_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfMoz$community_PV_lt, useNA = "always")

#------------Physical violence variables in the last 12 months------------

# Physical IPV in the last 12 months:
vars_ipv <- c("PV1HRT12", "PV1OBJ12", "PV1CSD12", "PV1WPN12")
for (var in vars_ipv) {
  dfMoz[[paste0(var, "1")]] <- ifelse(dfMoz[[var]] == 2, 2, ifelse(dfMoz[[var]] == 1, 1, NA))
  dfMoz[[paste0("IPV_", var, "1")]] <- ifelse(dfMoz[[paste0(var, "1")]] == 1, 1, ifelse(dfMoz[[paste0(var, "1")]] == 2, 0, NA))
}

dfMoz$PV_IPV_12m <- rowSums(dfMoz[, grep("^IPV_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$PV_IPV_12m <- ifelse(is.na(dfMoz$PV_IPV_12m), NA, ifelse(dfMoz$PV_IPV_12m >= 1, 1, 0))

attr(dfMoz$PV_IPV_12m, "label") <- "Physical IPV in the past 12 months"
table(dfMoz$PV_IPV_12m, useNA = "always")

# Physical peer violence in the last 12 months:
dfMoz$PV_Peers_12m <- NA
dfMoz$PV_Peers_12m[dfMoz$PV_Peers_lt == 0 | 
                     (dfMoz$PV_Peers_lt == 1 & 
                        (dfMoz$PV2TME >= 1 & 
                           (dfMoz$PV212 == 2 | dfMoz$PV2FT == 2)))] <- 0
dfMoz$PV_Peers_12m[dfMoz$PV_Peers_lt == 1 & 
                     (dfMoz$PV2TME >= 1 & 
                        (dfMoz$PV212 == 1 | dfMoz$PV2FT == 1))] <- 1

# Add a descriptive label to the variable
attr(dfMoz$PV_Peers_12m, "label") <- "Peers physical violence in the past 12 months"

# Generate a frequency table with missing values included
table(dfMoz$PV_Peers_12m, useNA = "always")

# Physical family violence in the last 12 months:
dfMoz$PV_Fam_12m <- NA
dfMoz$PV_Fam_12m[dfMoz$PV_Fam_lt == 0 | 
                   (dfMoz$PV_Fam_lt == 1 & 
                      (dfMoz$PV3TME >= 1 & 
                         (dfMoz$PV312 == 2 | dfMoz$PV3FT == 2)))] <- 0
dfMoz$PV_Fam_12m[dfMoz$PV_Fam_lt == 1 & 
                   (dfMoz$PV3TME >= 1 & 
                      (dfMoz$PV312 == 1 | dfMoz$PV3FT == 1))] <- 1

# Add a descriptive label to the variable
attr(dfMoz$PV_Fam_12m, "label") <- "Family physical violence in the past 12 months"
table(dfMoz$PV_Fam_12m, useNA = "always")

# Physical violence in the community in the last 12 months:
# Initialize the variable with NA
dfMoz$PV_Com_12m <- NA
dfMoz$PV_Com_12m[dfMoz$PV_Com_lt == 0 | 
                   (dfMoz$PV_Com_lt == 1 & 
                      (dfMoz$PV4TME >= 1 & 
                         (dfMoz$PV412 == 2 | dfMoz$PV4FT == 2)))] <- 0
dfMoz$PV_Com_12m[dfMoz$PV_Com_lt == 1 & 
                   (dfMoz$PV4TME >= 1 & 
                      (dfMoz$PV412 == 1 | dfMoz$PV4FT == 1))] <- 1

# Add a descriptive label to the variable
attr(dfMoz$PV_Com_12m, "label") <- "Physical violence in the community in the past 12 months"

# Generate a frequency table with missing values included
table(dfMoz$PV_Com_12m, useNA = "always")

# Any past-year physical violence BY EVERYONE in the last 12 months:
dfMoz$PV_Any <- rowSums(dfMoz[, c("PV_IPV_12m", "PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfMoz$PV_Any <- ifelse(is.na(dfMoz$PV_Any), NA, ifelse(dfMoz$PV_Any >= 1, 1, 0))

attr(dfMoz$PV_Any, "label") <- "Any physical violence in the past 12 months"
table(dfMoz$PV_Any, useNA = "always")    #428 NAs

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfMoz$child_PV_12m <- rowSums(dfMoz[, c("PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfMoz$child_PV_12m <- ifelse(is.na(dfMoz$child_PV_12m), NA, ifelse(dfMoz$child_PV_12m >= 1, 1, 0))

attr(dfMoz$child_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfMoz$child_PV_12m, useNA = "always")

#### creating just community physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfMoz$community_PV_12m <- rowSums(dfMoz[, c("PV_Peers_12m", "PV_Com_12m")], na.rm = TRUE)
dfMoz$community_PV_12m <- ifelse(is.na(dfMoz$community_PV_12m), NA, ifelse(dfMoz$community_PV_12m >= 1, 1, 0))

attr(dfMoz$community_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfMoz$community_PV_12m, useNA = "always")

#-------------Life time Emotional violence variables --------------

#------ Any lifetime emotional violence by parent, adult caregiver or other adult relative
vars_EV_fam_lt <- c("EV1LOVE", "EV1DEAD", "EV1DOWN")
for (var in vars_EV_fam_lt) {
  dfMoz[[paste0("EViolF_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$EV_Fam_lt <- rowSums(dfMoz[, grep("^EViolF_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$EV_Fam_lt <- ifelse(is.na(dfMoz$EV_Fam_lt), NA, ifelse(dfMoz$EV_Fam_lt >= 1, 1, 0))

attr(dfMoz$EV_Fam_lt, "label") <- "Any lifetime emotional violence by parent, adult caregiver or other adult relative"
table(dfMoz$EV_Fam_lt, useNA = "always")

#---- Any lifetime emotional violence by INTIMATE PARTNER:
vars_EV_IPV_lt <- c("EV2HUM", "EV2MON", "EV2TLK", "EV2KNW", "EV2HRM")
for (var in vars_EV_IPV_lt) {
  dfMoz[[paste0("EIPV_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$EV_IPV_lt <- rowSums(dfMoz[, grep("^EIPV_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$EV_IPV_lt <- ifelse(is.na(dfMoz$EV_IPV_lt), NA, ifelse(dfMoz$EV_IPV_lt >= 1, 1, 0))

attr(dfMoz$EV_IPV_lt, "label") <- "Lifetime emotional IPV"
table(dfMoz$EV_IPV_lt, useNA = "always")

#------ Any lifetime emotional violence BY ANYONE
dfMoz$EV_Any_lt <- rowSums(dfMoz[, c("EV_IPV_lt", "EV_Fam_lt")], na.rm = TRUE)
dfMoz$EV_Any_lt <- ifelse(is.na(dfMoz$EV_Any_lt), NA, ifelse(dfMoz$EV_Any_lt >= 1, 1, 0))

attr(dfMoz$EV_Any_lt, "label") <- "Any lifetime emotional violence"
table(dfMoz$EV_Any_lt, useNA = "always")

#---------------------Emotional violence variables in the LAST 12 MONTHS:
#------ Emotional violence by parent, adult caregiver, or other adult relative in the past 12 months
dfMoz$EV_Fam_12m <- NA
dfMoz$EV_Fam_12m[dfMoz$EV_Fam_lt == 0 | 
                   (dfMoz$EV_Fam_lt == 1 & 
                      (dfMoz$EV1TME >= 1 & 
                         (dfMoz$EV112M == 2 | dfMoz$EV1FT == 2)))] <- 0
dfMoz$EV_Fam_12m[dfMoz$EV_Fam_lt == 1 & 
                   (dfMoz$EV1TME >= 1 & 
                      (dfMoz$EV112M == 1 | dfMoz$EV1FT == 1))] <- 1
attr(dfMoz$EV_Fam_12m, "label") <- "Any emotional violence by parent, adult caregiver, or other adult relative in the past 12 months"
table(dfMoz$EV_Fam_12m, useNA = "always")

#------------Emotional violence by INTIMATE PARTNER in the LAST 12 MONTHS:
dfMoz$EV_IPV_12m <- NA
dfMoz$EV_IPV_12m[dfMoz$EV_IPV_lt == 0 | 
                   (dfMoz$EV_IPV_lt == 1 & 
                      (dfMoz$EV2TME >= 1 & 
                         dfMoz$EV212M == 2))] <- 0
dfMoz$EV_IPV_12m[dfMoz$EV_IPV_lt == 1 & 
                   (dfMoz$EV2TME >= 1 & 
                      dfMoz$EV212M == 1)] <- 1
attr(dfMoz$EV_IPV_12m, "label") <- "Emotional violence by intimate partner in the past 12 months"
table(dfMoz$EV_IPV_12m, useNA = "always")

#------------Emotional violence by PEER in the LAST 12 MONTHS:
vars_EV_peer_12m <- c("EV3TEASE", "EV3LIE", "EV3EXCL")
for (var in vars_EV_peer_12m) {
  dfMoz[[paste0("EViolPeer_", var)]] <- ifelse(dfMoz[[var]] == 1, 1, ifelse(dfMoz[[var]] == 2, 0, NA))
}

dfMoz$EV_Peer_12m <- rowSums(dfMoz[, grep("^EViolPeer_", colnames(dfMoz))], na.rm = TRUE)
dfMoz$EV_Peer_12m <- ifelse(is.na(dfMoz$EV_Peer_12m), NA, ifelse(dfMoz$EV_Peer_12m >= 1, 1, 0))

attr(dfMoz$EV_Peer_12m, "label") <- "Lifetime emotional violence by peers"
table(dfMoz$EV_Peer_12m, useNA = "always")

# #------ Any emotional violence BY ANYONE in the past 12 months
dfMoz$EV_Any <- rowSums(dfMoz[, c("EV_IPV_12m", "EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfMoz$EV_Any <- ifelse(is.na(dfMoz$EV_Any), NA, ifelse(dfMoz$EV_Any >= 1, 1, 0))

attr(dfMoz$EV_Any, "label") <- "Any emotional violence in the past 12 months"
table(dfMoz$EV_Any, useNA = "always")


#------ Any emotional violence BY ANYONE in the past 12 months
dfMoz$EV_Any_12m <- rowSums(dfMoz[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfMoz$EV_Any_12m <- ifelse(is.na(dfMoz$EV_Any_12m), NA, ifelse(dfMoz$EV_Any_12m >= 1, 1, 0))

attr(dfMoz$EV_Any_12m, "label") <- "Any emotional violence by anyone in the past 12 months"
table(dfMoz$EV_Any_12m, useNA = "always")


#select key variables of interest:
dfMoz2 <- dplyr::select(dfMoz1,
                        Q2,
                        HHSex,
                        inf_housing,
                        strata,
                        cluster,
                        sampleweight,
                        HDATE_VF,
                        month,
                        year,
                        region,
                        country,
                        AgeGroup,
                        sex,
                        married,
                        food_insec,
                        rural_urban,
                        EducationEver,
                        School_enrol,
                        Orphan,
                        Any_SV_12m,
                        EV_Any_12m,
                        child_PV_12m,
                        community_PV_12m,
                        PV_Fam_12m,
                        PV_IPV_12m,
                        SV_IPV_12m,
                        EV_IPV_12m,
                        SV_Any,
                        EV_Any,
                        PV_Any)


#-------------------------ZAMBIA FEMALE--------------:

#create a strata variable from the region variable:
dfZam$strata <- dfZam$region

#apply survey weights:
dfZam <- dfZam %>%
  rename( 
    cluster = psu,
    sampleweight = Finalwgt)

# Create a survey design object using svydesign
svy_design <- svydesign(
  ids = ~cluster,   # Clustering variable
  strata = ~strata,   # Stratification variable
  weights = ~sampleweight, # Survey weight variable
  nest = TRUE, 
  data = dfZam,
)

#create a WI variable with the observations of n
dfZam <- dfZam %>%
  mutate(WI = row_number())

# #Apply weights to WI using the individual weight variable
sum_w <- sum(dfZam$WI * dfZam$sampleweight)  # Calculate the sum of weighted numbers
dfZam$WI_1 <- sum_w  # Replace WI column with the sum
table(dfZam$WI_1)

#Create country column
dfZam$country <- "Zambia"

#specify all sex observations 
dfZam$sex <- 2

#times tamp information (interview date):
dfZam$HDATE_VF <- paste(dfZam$HYR_VF, dfZam$HMTH_VF, dfZam$HDAY_VF)
table(dfZam$HDATE_VF)

# Extracting year, month, and day information
dfZam <- dfZam %>% 
  mutate(
    year = as.integer(substr(HDATE_VF, 1, 4)),
    month = as.integer(substr(HDATE_VF, 6,7)),
    day = as.integer(substr(HDATE_VF, 9, 10))
  )

table(dfZam$day, useNA = "always")
table(dfZam$month, useNA = "always")
table(dfZam$year, useNA = "always")

#--------------------DEMOGRAPHIC VARIABLES-----------------
#Sex of head of household
dfZam <- dfZam %>%
  rename(HHSex = H2)

table(dfZam$HHSex)
attr(dfZam$HHSex, "label") <- "Head of household sex"

#Regional variable: 
table(dfZam$region)

#inspecting the rural urban variable
table(dfZam$rural_urban)

# Marital status
dfZam$married <- 0
dfZam$married[dfZam$Q27 == 1 | dfZam$Q28 == 1] <- 1
dfZam$married[dfZam$Q25 == 2] <- 0

# Display the table for Q25 and married
table(dfZam$married, useNA = "always")

#child marriage
dfZam <- dfZam %>%
  mutate(childmar = ifelse( Q26 %in% c (13:17),1,0))
table(dfZam$childmar, useNA = "always")
# Labeling
attr(dfZam$childmar, "label") <- "Married before the age of 18"

#Food insecurity:
dfZam$food_insec <- 0
dfZam$food_insec[dfZam$H20 == 1 | dfZam$H21 == 1] <- 1
table(dfZam$food_insec, useNA = "always")

#------Age variable
# Age group
dfZam$AgeGroup <- 0
dfZam$AgeGroup[dfZam$Q2 >= 13 & dfZam$Q2 <= 17] <- 1
dfZam$AgeGroup[dfZam$Q2 >= 18 & dfZam$Q2 <= 24] <- 2
dfZam$AgeGroup
# Labeling variables
attr(dfZam$AgeGroup, "label") <- "age group of girls"
label_values <- c("13-17", "18-24")
attr(dfZam$AgeGroup, "labels") <- label_values

# Displaying Q2 and AgeGroup for the first 30 rows
dfZam[1:30, c("Q2", "AgeGroup")]

# Tabulating Q2 and AgeGroup
table(dfZam$Q2, dfZam$AgeGroup)

#--------------- Education attainment
# Education attainment
table(dfZam$Q3, useNA = "always")  # Check for missing values in Q3 column
table(dfZam$Q4, useNA = "always")  # Check for missing values in Q4 column
table(dfZam$Q5, useNA = "always")  # Check for missing values in Q5 column
table(dfZam$Q6, useNA = "always")  # Check for missing values in Q6 column

# Recode missing values as NAs and create new variables
dfZam$Q3_ <- ifelse(dfZam$Q3 %in% c(98, 99), NA, dfZam$Q3)
dfZam$Q4_ <- ifelse(dfZam$Q4 %in% c(98, 99), NA, dfZam$Q4)
dfZam$Q5_ <- ifelse(dfZam$Q5 %in% c(98, 99), NA, dfZam$Q5)
dfZam$Q6_ <- ifelse(dfZam$Q6 %in% c(98, 99), NA, dfZam$Q6)

# Cross-tabulation to help with imputation
table(dfZam$Q3_, dfZam$Q5_, useNA = "always")  # Cross-tabulation of Q3_ and Q5_
table(dfZam$Q3_, dfZam$Q6_, useNA = "always")  # Cross-tabulation of Q3_ and Q6_

# Recode Q5_ and Q6_ into categories: primary or less, secondary, higher than secondary than secondary But first, replace NA by 1 if Q3_ == 2 "never attended school"
# First, replace missing values with 0 if Q3_ is eQual to 2
dfZam <- dfZam %>%
  mutate(Q5_ = ifelse(Q3_ == 2, 0, Q5_),
         Q6_ = ifelse(Q3_ == 2, 0, Q6_))

#If your current highest level is primary or less, then you highest ever level is even =<. So we can replace missing in Q5_ for those with the value of primary or less in Q6_
dfZam$Q5_ <- ifelse(dfZam$Q6_ == 1 & is.na(dfZam$Q5_), 1, dfZam$Q5_)
dfZam$Q5_ <- ifelse(dfZam$Q6_ == 2 & is.na(dfZam$Q5_), 2, dfZam$Q5_)
dfZam$Q5_ <- ifelse(dfZam$Q6_ == 3 & is.na(dfZam$Q5_), 3, dfZam$Q5_)

# Clone EducationEver variable
dfZam$EducationEver <- dfZam$Q5_

#label the Education variable:
label(dfZam$EducationEver) <- "Education attainment"

#Define the value lables of EducationEver:
value_labels <- c("Completed primary school or less", 
                  "Completed secondary school", "Higher than secondary")
dfZam$EducationEver <- factor(dfZam$EducationEver,
                              levels = c(1,2,3),
                              labels= value_labels)
table(dfZam$EducationEver)
levels(dfZam$EducationEver)

# Recode EducationEver into two categories
dfZam$EducationEver2 <- ifelse(dfZam$EducationEver %in% c("Completed secondary school", "Higher than secondary"), 1, 0)

table(dfZam$EducationEver, useNA = "always")

# Drop the original EducationEver variable
dfZam <- dfZam %>% dplyr::select(-EducationEver)            

# Rename EducationEver2 to EducationEver
dfZam <- dfZam %>% rename(EducationEver = EducationEver2)
table(dfZam$EducationEver, useNA = "always")  #0 NAs

# label the EducationEver variable:
attr(dfZam$EducationEver, "label") <- "Completed secondary or higher level of education"

# Current school enrollment
dfZam$School_enrol <- ifelse(dfZam$Q3_ == 2 & is.na(dfZam$Q4_), 2, dfZam$Q4_)
dfZam$School_enrol[dfZam$School_enrol == 2] <- 0

# Set value labels for School_enrol variable
attr(dfZam$School_enrol, "label") <- "School enrol"
# view_df(dfZam)

#--------------------Orphanhood
# Maternal orphanhood
dfZam <- dfZam %>%
  mutate(OrphMater = ifelse(Q13 == 1 | (Q13 == 2 & Q16 == 1) , 0,
                            ifelse(Q13 == 1 | Q16 == 2 | Q15 == 1, 1, NA)))
table(dfZam$OrphMater, useNA = "always")

attr(dfZam$OrphMater, "label") <- "Maternal orphanhood"

# Paternal orphanhood
dfZam <- dfZam %>%
  mutate(OrphPater = ifelse(Q19 == 1 | (Q19 == 2 & Q22 == 1), 0,
                            ifelse(Q22 == 2 | Q21 == 1, 1, NA)))

table(dfZam$OrphPater, useNA = "always")

attr(dfZam$OrphPater, "label") <- "Paternal orphanhood"

# Orphanhood
# Orphanhood
library(dplyr)

dfZam <- dfZam %>%
  mutate(Orphan = case_when(
    OrphMater == 0 & OrphPater == 0 ~ 0,
    OrphMater == 1 | OrphPater == 1 ~ 1,
    OrphMater == 1 & OrphPater == 1 ~ 1,
    OrphMater == 0 & OrphPater == 1 ~ 1,
    OrphMater == 1 & OrphPater == 0 ~ 1,
    TRUE ~ 0
  ))

#THERE is only 1 value of double orphanhood that's why we have 288 NAs
#so I converted missing values to 0 using case_when
#Orphan is any orphanhood maternal, paternal or both.

attr(dfZam$Orphan, "label") <- "Orphanhood"
table(dfZam$Orphan, useNA = "always")


#-----self reported HIV positive
#need to find HIV variable

#INFORMAL HOUSING VARIABLE:
dfZam <- dfZam %>%
  mutate(inf_housing = ifelse(H7A == 2, 1,  #electricity
                              # ifelse(H6 ==1, 1,   #sharing toilet 
                              ifelse(H10 %in% c(1:3), 1, 0)))   #roof material

table(dfZam$inf_housing)

#--------Violence variables----------------
#-------------Life time sexual violence variables --------------
# Unwanted sexual touching
dfZam$SV_Q600 <- ifelse(dfZam$Q700 == 1, 1, ifelse(dfZam$Q700 == 2, 0, NA))
label(dfZam$SV_Q600) <- "Unwanted sex touching"
table(dfZam$SV_Q600, useNA = "always")

# Attempted forced sex
dfZam$SV_AttRape <- ifelse(dfZam$Q800 == 1, 1, ifelse(dfZam$Q800 == 2, 0, NA))
label(dfZam$SV_AttRape) <- "Attempted forced sex"
table(dfZam$SV_AttRape, useNA = "always")

# Physically forced sex
dfZam$SV_Rape <- ifelse(dfZam$Q900 == 1, 1, ifelse(dfZam$Q900 == 2, 0, NA))
label(dfZam$SV_Rape) <- "Physically forced sex"
table(dfZam$SV_Rape, useNA = "always")

# Pressured sex
dfZam$SV_PreSex <- ifelse(dfZam$Q1000 == 1, 1, ifelse(dfZam$Q1000 == 2, 0, NA))
label(dfZam$SV_PreSex) <- "Pressured sex"
table(dfZam$SV_PreSex, useNA = "always")

# Any lifetime sexual violence
dfZam$Any_SV_lifetime <- rowSums(dfZam[, c("SV_Q600", "SV_AttRape", "SV_Rape", "SV_PreSex")], na.rm = TRUE)
dfZam$Any_SV_lifetime <- ifelse(is.na(dfZam$Any_SV_lifetime), NA, ifelse(dfZam$Any_SV_lifetime >= 1, 1, 0))
label(dfZam$Any_SV_lifetime) <- "Any lifetime sexual violence"

table(dfZam$Any_SV_lifetime, useNA = "always")

#------------------- Sexual violence in the past 12 months 
# Unwanted sexual touching in the last 12 months:
dfZam$SV_1_12m <- NA
dfZam$SV_1_12m[dfZam$SV_Q600 == 0 | 
                 (dfZam$SV_Q600 == 1 & 
                    (dfZam$Q701 >= 1 & 
                       (dfZam$Q702 == 2 | dfZam$Q712 == 2)))] <- 0
dfZam$SV_1_12m[dfZam$SV_Q600 == 1 & 
                 (dfZam$Q701 >= 1 & 
                    (dfZam$Q702 == 1 | dfZam$Q712 == 1))] <- 1
table(dfZam$SV_1_12m, useNA = "always")

# Unwanted attempted sex in the last 12 months:
# Initialize the variable with NA
dfZam$SV_2_12m <- NA
dfZam$SV_2_12m[dfZam$SV_AttRape == 0 | 
                 (dfZam$SV_AttRape == 1 & 
                    (dfZam$Q801 >= 1 & 
                       (dfZam$Q802 == 2 | dfZam$Q812 == 2)))] <- 0
dfZam$SV_2_12m[dfZam$SV_AttRape == 1 & 
                 (dfZam$Q801 >= 1 & 
                    (dfZam$Q802 == 1 | dfZam$Q812 == 1))] <- 1

# Generate a frequency table with missing values included
table(dfZam$SV_2_12m, useNA = "always")

# Physically forced sex in the last 12 months:
dfZam$SV_3_12m <- NA
dfZam$SV_3_12m[dfZam$SV_Rape == 0 | 
                 (dfZam$SV_Rape == 1 & 
                    (dfZam$Q901 >= 1 & 
                       (dfZam$Q902 == 2 | dfZam$Q915 == 2)))] <- 0
dfZam$SV_3_12m[dfZam$SV_Rape == 1 & 
                 (dfZam$Q901 >= 1 & 
                    (dfZam$Q902 == 1 | dfZam$Q915 == 1))] <- 1
table(dfZam$SV_3_12m, useNA = "always")

# Pressured sex in the last 12 months:
dfZam$SV_4_12m <- NA
dfZam$SV_4_12m[dfZam$SV_PreSex == 0 | 
                 (dfZam$SV_PreSex == 1 & 
                    (dfZam$Q1001 >= 1 & 
                       (dfZam$Q1002 == 2 | dfZam$Q1014 == 2)))] <- 0
dfZam$SV_4_12m[dfZam$SV_PreSex == 1 & 
                 (dfZam$Q1001 >= 1 & 
                    (dfZam$Q1002 == 1 | dfZam$Q1014 == 1))] <- 1
table(dfZam$SV_4_12m, useNA = "always")

# Any sexual violence in the past 12 months:
dfZam$SV_Any <- ifelse(rowSums(dfZam[, c("SV_1_12m", "SV_2_12m", "SV_3_12m", "SV_4_12m")]) >= 1, 1, 0)

table(dfZam$SV_Any, useNA = "always")
label(dfZam$SV_Any) <- "Any sexual violence in the last 12m"

#------------------- Sexual violence BY PARTNERS AND NON-PARTNERS in the past 12 months 

# Unwanted sexual touching in the last 12 months:
dfZam <- dfZam %>%
  mutate(
    SV_1any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q712 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q712 == 1)) &
        (Q707 %in% c(5:16, 21:32) | Q717 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam$SV_1any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfZam$SV_1any_12m, useNA = "always")

#### Unwanted sexual touching by an intimate partner in the last 12 months:
# Unwanted sexual touching in the last 12 months:
dfZam <- dfZam %>%
  mutate(
    SV_1IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q712 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q712 == 1)) &
        (Q707 %in% c(1: 4,17:20) | Q717 %in% c(1: 4,17:20)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam$SV_1IPV_12m, "label") <- "Unwanted Sexual Touching by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfZam$SV_1IPV_12m, useNA = "always")

# Unwanted attempted sex by ANYONE in the last 12 months:
# Recode SV_2_12m based on conditions
dfZam <- dfZam %>%
  mutate(
    SV_2any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 2 | Q812 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 1 | Q812 == 1)) &
        (Q807 %in% c(5:16, 21:32) | Q817 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam$SV_2any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfZam$SV_2any_12m, useNA = "always")

# Unwanted attempted sex by INTIMATE PARTNER in the last 12 months:
# Recode SV_2_12m based on conditions
dfZam <- dfZam %>%
  mutate(
    SV_2IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 2 | Q812 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 1 | Q812 == 1)) &
        (Q807 %in% c(1:4,17:20) | Q817 %in% c(1:4,17:20)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam$SV_2IPV_12m, "label") <- "Unwanted attempted sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable
table(dfZam$SV_2IPV_12m, useNA = "always")

# Unwanted forced sex BY ANYONE in the last 12 months:
# Recode SV_3_12m based on conditions
dfZam <- dfZam %>%
  mutate(
    SV_3any_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 2 | Q915 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 1 | Q915 == 1)) &
        (Q907 %in% c(5:16, 21:32) | Q920 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam$SV_3any_12m, "label") <- "Physically forced sex by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfZam$SV_3any_12m, useNA = "always")

# Unwanted forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_3_12m based on conditions
dfZam <- dfZam %>%
  mutate(
    SV_3IPV_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 2 | Q915 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 1 | Q915 == 1)) &
        (Q907 %in% c(1:4,17:20) | Q920 %in% c(1:4,17:20)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam$SV_3IPV_12m, "label") <- "Physically forced sex by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfZam$SV_3IPV_12m, useNA = "always")

# Pressured sex BY ANYONE in the last 12 months:
# Recode SV_4_12m based on conditions
dfZam <- dfZam %>%
  mutate(
    SV_4any_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1014 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1014 == 1)) &
        (Q1007 %in% c(5:16, 21:32) | Q1019 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam$SV_4any_12m, "label") <- "Pressured Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfZam$SV_4any_12m, useNA = "always")

# Pressured sex AN INTIMATE PARTNER in the last 12 months:
# Recode SV_4_12m based on conditions
dfZam <- dfZam %>%
  mutate(
    SV_4IPV_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1014 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1014 == 1)) &
        (Q1007 %in% c(1:4,17:20)  | Q1019 %in% c(1:4,17:20)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam$SV_4IPV_12m, "label") <- "Pressured Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable 
table(dfZam$SV_4IPV_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfZam$Any_SV_12m <- rowSums(dfZam[,c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m")], na.rm =TRUE)
dfZam$Any_SV_12m <- ifelse(is.na(dfZam$Any_SV_12m), NA,
                           ifelse(dfZam$Any_SV_12m >=1, 1, 0))

attr(dfZam$Any_SV_12m, "label") <- "Any sexual violence by anyone in the last 12 months"
table(dfZam$Any_SV_12m, useNA = "always")

#  sexual violence BY AN INTIMATE PARTNER in the past 12 months:
dfZam$SV_IPV_12m <- rowSums(dfZam[,c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m")], na.rm =TRUE)
dfZam$SV_IPV_12m <- ifelse(is.na(dfZam$SV_IPV_12m), NA,
                           ifelse(dfZam$SV_IPV_12m >=1, 1, 0))

attr(dfZam$SV_IPV_12m, "label") <- "Any sexual violence by an intimate partner in the last 12 months"
table(dfZam$SV_IPV_12m, useNA = "always")

#------------------- Physical violence in a life time:
# Physical IPV in a lifetime
vars <- c("Q100A", "Q100B", "Q100C")
for (var in vars) {
  dfZam[[paste0("IPV_", var)]] <- ifelse(dfZam[[var]] == 1, 1, ifelse(dfZam[[var]] == 2, 0, NA))
}

dfZam$PV_IPV_lt <- rowSums(dfZam[, grep("^IPV_", colnames(dfZam))], na.rm = TRUE)
dfZam$PV_IPV_lt <- ifelse(is.na(dfZam$PV_IPV_lt), NA, ifelse(dfZam$PV_IPV_lt >= 1, 1, 0))
attr(dfZam$PV_IPV_lt, "label") <- "Lifetime physical IPV"
table(dfZam$PV_IPV_lt, useNA = "always")
#------------------ Physical peer violence in a lifetime
vars_peer <- c("Q116A", "Q116B", "Q116C")
for (var in vars_peer) {
  dfZam[[paste0("ViolPeer_", var)]] <- ifelse(dfZam[[var]] == 1, 1, ifelse(dfZam[[var]] == 2, 0, NA))
}

dfZam$PV_Peers_lt <- rowSums(dfZam[, grep("^ViolPeer_", colnames(dfZam))], na.rm = TRUE)
dfZam$PV_Peers_lt <- ifelse(is.na(dfZam$PV_Peers_lt), NA, ifelse(dfZam$PV_Peers_lt >= 1, 1, 0))

attr(dfZam$PV_Peers_lt, "label") <- "Lifetime peer physical violence"
table(dfZam$PV_Peers_lt, useNA = "always")

#----------------- Physical family violence
vars_fam <- c("Q128A", "Q128B", "Q128C")
for (var in vars_fam) {
  dfZam[[paste0("ViolF_", var)]] <- ifelse(dfZam[[var]] == 1, 1, ifelse(dfZam[[var]] == 2, 0, NA))
}

dfZam$PV_Fam_lt <- rowSums(dfZam[, grep("^ViolF_", colnames(dfZam))], na.rm = TRUE)
dfZam$PV_Fam_lt <- ifelse(is.na(dfZam$PV_Fam_lt), NA, ifelse(dfZam$PV_Fam_lt >= 1, 1, 0))

attr(dfZam$PV_Fam_lt, "label") <- "Lifetime family physical violence"
table(dfZam$PV_Fam_lt, useNA = "always")

#---------------- Community Physical Violence (adults in neighbourhood)
vars_com <- c("Q142A", "Q142B", "Q142C")
for (var in vars_com) {
  dfZam[[paste0("ViolC_", var)]] <- ifelse(dfZam[[var]] == 1, 1, ifelse(dfZam[[var]] == 2, 0, NA))
}

dfZam$PV_Com_lt <- rowSums(dfZam[, grep("^ViolC_", colnames(dfZam))], na.rm = TRUE)
dfZam$PV_Com_lt <- ifelse(is.na(dfZam$PV_Com_lt), NA, ifelse(dfZam$PV_Com_lt >= 1, 1, 0))

attr(dfZam$PV_Com_lt, "label") <- "Lifetime community physical violence"
table(dfZam$PV_Com_lt, useNA = "always")

#-------------- Any physical violence in a lifetime:
dfZam$Any_PV_lt <- rowSums(dfZam[,c("PV_IPV_lt", "PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfZam$Any_PV_lt <- ifelse(is.na(dfZam$Any_PV_lt), NA,
                          ifelse(dfZam$Any_PV_lt >=1, 1, 0))

attr(dfZam$Any_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfZam$Any_PV_lt, useNA = "always")

####.  Create JUST child physical violence variable: ####
# Child physical violence in a lifetime:
dfZam$child_PV_lt <- rowSums(dfZam[,c("PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfZam$child_PV_lt <- ifelse(is.na(dfZam$child_PV_lt), NA,
                            ifelse(dfZam$child_PV_lt >=1, 1, 0))

attr(dfZam$child_PV_lt, "label") <- "Any child lifetime physical violence"
table(dfZam$child_PV_lt, useNA = "always")

####.  Create JUST child physical violence variable: ####
# Child physical violence in a lifetime:
dfZam$community_PV_lt <- rowSums(dfZam[,c("PV_Peers_lt", "PV_Com_lt")], na.rm = TRUE)
dfZam$community_PV_lt <- ifelse(is.na(dfZam$community_PV_lt), NA,
                                ifelse(dfZam$community_PV_lt >=1, 1, 0))

attr(dfZam$community_PV_lt, "label") <- "Any child lifetime physical violence"
table(dfZam$community_PV_lt, useNA = "always")

#------------ Physical violence variables IN THE LAST 12 MONTHS:-----------

# Physical IPV in the last 12 months:
dfZam$PV_IPV_12m <- NA
dfZam$PV_IPV_12m[dfZam$PV_IPV_lt == 0 | 
                   (dfZam$PV_IPV_lt == 1 & 
                      (dfZam$Q101 >= 1 & 
                         (dfZam$Q102 == 2 | dfZam$Q109 == 2)))] <- 0
dfZam$PV_IPV_12m[dfZam$PV_IPV_lt == 1 & 
                   (dfZam$Q101 >= 1 & 
                      (dfZam$Q102 == 1 | dfZam$Q109 == 1))] <- 1
attr(dfZam$PV_IPV_12m, "label") <- "Intimate Partner Violence in the past 12 months"

# Physical peer violence in the last 12 months:
dfZam$PV_Peers_12m <- NA
dfZam$PV_Peers_12m[dfZam$PV_Peers_lt == 0 | 
                     (dfZam$PV_Peers_lt == 1 & 
                        (dfZam$Q117 >= 1 & 
                           (dfZam$Q118 == 2 | dfZam$Q123 == 2)))] <- 0

dfZam$PV_Peers_12m[dfZam$PV_Peers_lt == 1 & 
                     (dfZam$Q117 >= 1 & 
                        (dfZam$Q118 == 1 | dfZam$Q123 == 1))] <- 1

attr(dfZam$PV_Peers_12m, "label") <- "Peers physical violence in the past 12 months"

table(dfZam$PV_Peers_12m, useNA = "always")
# Physical family violence in the last 12 months:
dfZam$PV_Fam_12m <- NA
dfZam$PV_Fam_12m[dfZam$PV_Fam_lt == 0 | 
                   (dfZam$PV_Fam_lt == 1 & 
                      (dfZam$Q129 >= 1 & 
                         (dfZam$Q130 == 2 | dfZam$Q136 == 2)))] <- 0
dfZam$PV_Fam_12m[dfZam$PV_Fam_lt == 1 & 
                   (dfZam$Q129 >= 1 & 
                      (dfZam$Q130 == 1 | dfZam$Q136 == 1))] <- 1
attr(dfZam$PV_Fam_12m, "label") <- "Family physical violence in the past 12 months"
table(dfZam$PV_Fam_12m, useNA = "always")

# Physical violence in the community in the last 12 months:
dfZam$PV_Com_12m <- NA
dfZam$PV_Com_12m[dfZam$PV_Com_lt == 0 | 
                   (dfZam$PV_Com_lt == 1 & 
                      (dfZam$Q143 >= 1 & 
                         (dfZam$Q144 == 2 | dfZam$Q149 == 2)))] <- 0
dfZam$PV_Com_12m[dfZam$PV_Com_lt == 1 & 
                   (dfZam$Q143 >= 1 & 
                      (dfZam$Q144 == 1 | dfZam$Q149 == 1))] <- 1

attr(dfZam$PV_Com_12m, "label") <- "Physical violence in the community in the past 12 months"
table(dfZam$PV_Com_12m, useNA = "always")

# Any physical violence in the past 12 months:
dfZam$PV_Any <- rowSums(dfZam[, c("PV_IPV_12m", "PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfZam$PV_Any <- ifelse(is.na(dfZam$PV_Any), NA, ifelse(dfZam$PV_Any >= 1, 1, 0))

attr(dfZam$PV_Any, "label") <- "Any physical violence by everyone in the past 12 months"
table(dfZam$PV_Any, useNA = "always")

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfZam$child_PV_12m <- rowSums(dfZam[, c("PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfZam$child_PV_12m <- ifelse(is.na(dfZam$child_PV_12m), NA, ifelse(dfZam$child_PV_12m >= 1, 1, 0))

attr(dfZam$child_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfZam$child_PV_12m, useNA = "always")

#### creating just community physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfZam$community_PV_12m <- rowSums(dfZam[, c("PV_Peers_12m", "PV_Com_12m")], na.rm = TRUE)
dfZam$community_PV_12m <- ifelse(is.na(dfZam$community_PV_12m), NA, ifelse(dfZam$community_PV_12m >= 1, 1, 0))

attr(dfZam$community_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfZam$community_PV_12m, useNA = "always")

#----------------------Emotional violence in a lifetime

# Any lifetime emotional violence by parent, adult caregiver, or other adult relative
vars <- c("Q300A", "Q300B", "Q300C")
for (var in vars) {
  dfZam[[paste0("EViolF_", var)]] <- ifelse(dfZam[[var]] == 1, 1, ifelse(dfZam[[var]] == 2, 0, NA))
}

dfZam$EV_Fam_lt <- rowSums(dfZam[, grep("^EViolF_", colnames(dfZam))], na.rm = TRUE)
dfZam$EV_Fam_lt <- ifelse(is.na(dfZam$EV_Fam_lt), NA, ifelse(dfZam$EV_Fam_lt >= 1, 1, 0))

attr(dfZam$EV_Fam_lt, "label") <- "Any lifetime emotional violence by parent, adult caregiver, or other adult relative"
table(dfZam$EV_Fam_lt, useNA = "always")

#------Any lifetime emotional violence by PEERS 
vars_EV_peer_lt <- c("Q310A", "Q310B", "Q310C")
for (var in vars_EV_peer_lt) {
  dfZam[[paste0("EViolPeer_", var)]] <- ifelse(dfZam[[var]] == 1, 1, ifelse(dfZam[[var]] == 2, 0, NA))
}

dfZam$EV_Peer_lt <- rowSums(dfZam[, grep("^EViolPeer_", colnames(dfZam))], na.rm = TRUE)
dfZam$EV_Peer_lt <- ifelse(is.na(dfZam$EV_Peer_lt), NA, ifelse(dfZam$EV_Peer_lt >= 1, 1, 0))

attr(dfZam$EV_Peer_lt, "label") <- "Lifetime emotional violence by peers"
table(dfZam$EV_Peer_lt, useNA = "always")

#----------------------Emotional violence in the last 12 months
# Emotional violence by parent, adult caregiver, or other adult relative in the past 12 months
dfZam$EV_Fam_12m <- NA
dfZam$EV_Fam_12m[dfZam$EV_Fam_lt == 0 | 
                   (dfZam$EV_Fam_lt == 1 & 
                      (dfZam$Q301 == 2 | dfZam$Q301 == 3 & 
                         (dfZam$Q302 == 2 | dfZam$Q306 == 2)))] <- 0
dfZam$EV_Fam_12m[dfZam$EV_Fam_lt == 1 & 
                   (dfZam$Q301 == 2 | dfZam$Q301 == 3 & 
                      (dfZam$Q302 == 1 | dfZam$Q306 == 1))] <- 1
attr(dfZam$EV_Fam_12m, "label") <- "Any emotional violence by parent, adult caregiver, or other adult relative in the past 12 months"
table(dfZam$EV_Fam_12m, useNA = "always")

# Emotional violence by Peers in the past 12 months
dfZam$EV_Peer_12m <- NA
dfZam$EV_Peer_12m[dfZam$EV_Peer_lt == 0 | 
                    (dfZam$EV_Peer_lt == 1 & 
                       (dfZam$Q312 == 2 | dfZam$Q319 == 2))] <- 0
dfZam$EV_Peer_12m[dfZam$EV_Peer_lt == 1 & 
                    (dfZam$Q311 == 1 |dfZam$Q311 == 2 | dfZam$Q311 == 3 & 
                       (dfZam$Q312 == 1 | dfZam$Q319 == 1))] <- 1
attr(dfZam$EV_Peer_12m, "label") <- "Any emotional violence by peers in the last 12 months"
table(dfZam$EV_Peer_12m, useNA = "always")

#------Any emotional violence by ANYONE in the past 12 months: 
dfZam$EV_Any_12m <- rowSums(dfZam[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfZam$EV_Any_12m <- ifelse(is.na(dfZam$EV_Any_12m), NA, ifelse(dfZam$EV_Any_12m >= 1, 1, 0))

attr(dfZam$EV_Any_12m, "label") <- "Any Emotional violence in the past 12 months"
table(dfZam$EV_Any_12m, useNA = "always")

#------Any emotional violence by ANYONE in the past 12 months: 
dfZam$EV_Any <- rowSums(dfZam[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfZam$EV_Any <- ifelse(is.na(dfZam$EV_Any_12m), NA, ifelse(dfZam$EV_Any_12m >= 1, 1, 0))

attr(dfZam$EV_Any, "label") <- "Any Emotional violence in the past 12 months"
table(dfZam$EV_Any, useNA = "always")

# create an Emotional IPV variable with NAs for merging purposes
dfZam$EV_IPV_12m <- NA

dfZam1 <- dplyr::select(dfZam,
                        Q2,
                        HHSex,
                        inf_housing,
                        strata,
                        cluster,
                        sampleweight,
                        HDATE_VF,
                        month,
                        year,
                        region,
                        country,
                        AgeGroup,
                        sex,
                        married,
                        food_insec,
                        rural_urban,
                        EducationEver,
                        School_enrol,
                        Orphan,
                        Any_SV_12m,
                        EV_Any_12m,
                        child_PV_12m,
                        community_PV_12m,
                        PV_Fam_12m,
                        PV_IPV_12m,
                        SV_IPV_12m,
                        EV_IPV_12m,
                        SV_Any,
                        EV_Any,
                        PV_Any)


#------------- ZAMBIA MALE:
#create a strata variable from the region variable
dfZam2$strata <- dfZam2$region

dfZam2 <- dfZam2 %>%
  rename( 
    cluster = psu,
    sampleweight = Finalwgt)
dfZam2$strata

# Create a survey design object using svydesign
svy_design <- svydesign(
  ids = ~cluster,   # Clustering variable
  strata = ~strata,   # Stratification variable
  weights = ~sampleweight, # Survey weight variable
  nest = TRUE, 
  data = dfZam2,
)

#create a WI variable with the observations of n
dfZam2 <- dfZam2 %>%
  mutate(WI = row_number())

sum_w <- sum(dfZam2$WI * dfZam2$sampleweight)  # Calculate the sum of weighted numbers
dfZam2$WI_1 <- sum_w  # Replace WI column with the sum
table(dfZam2$WI_1)

#-------------------------- Demographic variables
#Sex of head of household
dfZam2 <- dfZam2 %>%
  rename(HHSex = H2)

table(dfZam2$HHSex)
attr(dfZam2$HHSex, "label") <- "Head of household sex"

#timestamp information:
dfZam2$HDATE_VF <- paste(dfZam2$HYR_VF, dfZam2$HMTH_VF, dfZam2$HDAY_VF)
table(dfZam2$HDATE_VF)
# Extracting year, month, and day information
dfZam2 <- dfZam2 %>% 
  mutate(
    year = as.integer(substr(HDATE_VF, 1, 4)),
    month = as.integer(substr(HDATE_VF, 6,7)),
    day = as.integer(substr(HDATE_VF, 9, 10))
  )

table(dfZam2$day, useNA = "always")
table(dfZam2$month, useNA = "always")
table(dfZam2$year, useNA = "always")

#--------------------DEMOGRAPHIC VARIABLES-----------------
#Regional variable: 
table(dfZam2$region)

#creating a gender column
dfZam2$sex <- 1

#creating country variable
dfZam2$country <- "Zambia"

#inspecting the rural urban variable
table(dfZam2$rural_urban)

# Marital status
dfZam2$married <- 0
dfZam2$married[dfZam2$Q27 == 1 | dfZam2$Q28 == 1] <- 1
dfZam2$married[dfZam2$married != 1] <- 0

# Display the table
table(dfZam2$married, useNA = "always")

#Food insecurity:
dfZam2$food_insec <- 0
dfZam2$food_insec[dfZam2$H20 == 1 | dfZam2$H21 == 1] <- 1
table(dfZam2$food_insec, useNA = "always")

#child marriage
dfZam2 <- dfZam2 %>%
  mutate(childmar = ifelse( Q26 %in% c (13:17),1,0))
table(dfZam2$childmar, useNA = "always")
# Labeling
attr(dfZam2$childmar, "label") <- "Married before the age of 18"

#------Age variable
# Age group
dfZam2$AgeGroup <- 0
dfZam2$AgeGroup[dfZam2$Q2 >= 13 & dfZam2$Q2 <= 17] <- 1
dfZam2$AgeGroup[dfZam2$Q2 >= 18 & dfZam2$Q2 <= 24] <- 2
dfZam2$AgeGroup
# Labeling variables
attr(dfZam2$AgeGroup, "label") <- "age group of girls"
label_values <- c("13-17", "18-24")
attr(dfZam2$AgeGroup, "labels") <- label_values

# Displaying Q2 and AgeGroup for the first 30 rows
dfZam2[1:30, c("Q2", "AgeGroup")]

# Tabulating Q2 and AgeGroup
table(dfZam2$Q2, dfZam2$AgeGroup)

#--------------- Education attainment
# Education attainment
table(dfZam2$Q3, useNA = "always")  # Check for missing values in Q3 column
table(dfZam2$Q4, useNA = "always")  # Check for missing values in Q4 column
table(dfZam2$Q5, useNA = "always")  # Check for missing values in Q5 column
table(dfZam2$Q6, useNA = "always")  # Check for missing values in Q6 column

# Recode missing values as NAs and create new variables
dfZam2$Q3_ <- ifelse(dfZam2$Q3 %in% c(98, 99), NA, dfZam2$Q3)
dfZam2$Q4_ <- ifelse(dfZam2$Q4 %in% c(98, 99), NA, dfZam2$Q4)
dfZam2$Q5_ <- ifelse(dfZam2$Q5 %in% c(98, 99), NA, dfZam2$Q5)
dfZam2$Q6_ <- ifelse(dfZam2$Q6 %in% c(98, 99), NA, dfZam2$Q6)

# Cross-tabulation to help with imputation
table(dfZam2$Q3_, dfZam2$Q5_, useNA = "always")  # Cross-tabulation of Q3_ and Q5_
table(dfZam2$Q3_, dfZam2$Q6_, useNA = "always")  # Cross-tabulation of Q3_ and Q6_

# Recode Q5_ and Q6_ into categories: primary or less, secondary, higher than secondary than secondary But first, replace NA by 1 if Q3_ == 2 "never attended school"
# First, replace missing values with 0 if Q3_ is eQual to 2
dfZam2 <- dfZam2 %>%
  mutate(Q5_ = ifelse(Q3_ == 2, 0, Q5_),
         Q6_ = ifelse(Q3_ == 2, 0, Q6_))

#If your current highest level is primary or less, then you highest ever level is even =<. So we can replace missing in Q5_ for those with the value of primary or less in Q6_
dfZam2$Q5_ <- ifelse(dfZam2$Q6_ == 1 & is.na(dfZam2$Q5_), 1, dfZam2$Q5_)
dfZam2$Q5_ <- ifelse(dfZam2$Q6_ == 2 & is.na(dfZam2$Q5_), 2, dfZam2$Q5_)
dfZam2$Q5_ <- ifelse(dfZam2$Q6_ == 3 & is.na(dfZam2$Q5_), 3, dfZam2$Q5_)

# Clone EducationEver variable
dfZam2$EducationEver <- dfZam2$Q5_

#label the Education variable:
label(dfZam2$EducationEver) <- "Education attainment"

#Define the value lables of EducationEver:
value_labels <- c("Completed primary school or less", 
                  "Completed secondary school", "Higher than secondary")
dfZam2$EducationEver <- factor(dfZam2$EducationEver,
                               levels = c(1,2,3),
                               labels= value_labels)
table(dfZam2$EducationEver)
levels(dfZam2$EducationEver)

# Recode EducationEver into two categories
dfZam2$EducationEver2 <- ifelse(dfZam2$EducationEver %in% c("Completed secondary school", "Higher than secondary"), 1, 0)

table(dfZam2$EducationEver, useNA = "always")

# Drop the original EducationEver variable
dfZam2 <- dfZam2 %>% dplyr::select(-EducationEver)            

# Rename EducationEver2 to EducationEver
dfZam2 <- dfZam2 %>% rename(EducationEver = EducationEver2)
table(dfZam2$EducationEver, useNA = "always")  #0 NAs

# label the EducationEver variable:
attr(dfZam2$EducationEver, "label") <- "Completed secondary or higher level of education"

# Current school enrollment
dfZam2$School_enrol <- ifelse(dfZam2$Q3_ == 2 & is.na(dfZam2$Q4_), 2, dfZam2$Q4_)
dfZam2$School_enrol[dfZam2$School_enrol == 2] <- 0

# Set value labels for School_enrol variable
attr(dfZam2$School_enrol, "label") <- "School enrol"

#--------------------Orphanhood
# Maternal orphanhood
dfZam2 <- dfZam2 %>%
  mutate(OrphMater = ifelse(Q13 == 1 | (Q13 == 2 & Q16 == 1) , 0,
                            ifelse(Q13 == 1 | Q16 == 2 | Q15 == 1, 1, NA)))
table(dfZam2$OrphMater, useNA = "always")

attr(dfZam2$OrphMater, "label") <- "Maternal orphanhood"

# Paternal orphanhood
dfZam2 <- dfZam2 %>%
  mutate(OrphPater = ifelse(Q19 == 1 | (Q19 == 2 & Q22 == 1), 0,
                            ifelse(Q22 == 2 | Q21 == 1, 1, NA)))

table(dfZam2$OrphPater, useNA = "always")

attr(dfZam2$OrphPater, "label") <- "Paternal orphanhood"

# Orphanhood
dfZam2 <- dfZam2 %>%
  mutate(Orphan = case_when(
    OrphMater == 0 & OrphPater == 0 ~ 0,
    OrphMater == 1 | OrphPater == 1 ~ 1,
    OrphMater == 1 & OrphPater == 1 ~ 1,
    OrphMater == 0 & OrphPater == 1 ~ 1,
    OrphMater == 1 & OrphPater == 0 ~ 1,
    TRUE ~ 0
  ))

attr(dfZam2$Orphan, "label") <- "Orphanhood"
table(dfZam2$Orphan, useNA = "always")

#-----self reported HIV positive
#need to find HIV variable

#INFORMAL HOUSING VARIABLE:
dfZam2 <- dfZam2 %>%
  mutate(inf_housing = ifelse(H7A == 2, 1,  #electricity
                              # ifelse(H6 ==1, 1,   #sharing toilet 
                              ifelse(H10 %in% c(1:3), 1, 0)))   #roof material

table(dfZam2$inf_housing)


#--------Violence variables----------------
#-------------Life time sexual violence variables --------------
# Unwanted sexual touching
dfZam2$SV_Q600 <- ifelse(dfZam2$Q700 == 1, 1, ifelse(dfZam2$Q700 == 2, 0, NA))
label(dfZam2$SV_Q600) <- "Unwanted sex touching"
table(dfZam2$SV_Q600, useNA = "always")

# Attempted forced sex
dfZam2$SV_AttRape <- ifelse(dfZam2$Q800 == 1, 1, ifelse(dfZam2$Q800 == 2, 0, NA))
label(dfZam2$SV_AttRape) <- "Attempted forced sex"
table(dfZam2$SV_AttRape, useNA = "always")

# Physically forced sex
dfZam2$SV_Rape <- ifelse(dfZam2$Q900 == 1, 1, ifelse(dfZam2$Q900 == 2, 0, NA))
label(dfZam2$SV_Rape) <- "Physically forced sex"
table(dfZam2$SV_Rape, useNA = "always")

# Pressured sex
dfZam2$SV_PreSex <- ifelse(dfZam2$Q1000 == 1, 1, ifelse(dfZam2$Q1000 == 2, 0, NA))
label(dfZam2$SV_PreSex) <- "Pressured sex"
table(dfZam2$SV_PreSex, useNA = "always")

# Any lifetime sexual violence
dfZam2$Any_SV_lifetime <- rowSums(dfZam2[, c("SV_Q600", "SV_AttRape", "SV_Rape", "SV_PreSex")], na.rm = TRUE)
dfZam2$Any_SV_lifetime <- ifelse(is.na(dfZam2$Any_SV_lifetime), NA, ifelse(dfZam2$Any_SV_lifetime >= 1, 1, 0))
label(dfZam2$Any_SV_lifetime) <- "Any lifetime sexual violence"

#------------------- Sexual violence in the past 12 months 

# Unwanted sexual touching in the last 12 months:
dfZam2$SV_1_12m <- ifelse(dfZam2$SV_Q600 == 0 | (dfZam2$SV_Q600 == 1 & (dfZam2$Q701 >= 1 & (dfZam2$Q702 == 2 | dfZam2$Q712 == 2))), 0, 1)
dfZam2$SV_1_12m <- ifelse(dfZam2$SV_Q600 == 1 & (dfZam2$Q701 >= 1 & (dfZam2$Q702 == 1 | dfZam2$Q712 == 1)), 1, dfZam2$SV_1_12m)
table(dfZam2$SV_1_12m, useNA = "always")

# Unwanted attempted sex in the last 12 months:
dfZam2$SV_2_12m <- ifelse(dfZam2$SV_AttRape == 0 | (dfZam2$SV_AttRape == 1 & (dfZam2$Q801 >= 1 & (dfZam2$Q802 == 2 | dfZam2$Q812 == 2))), 0, 1)
dfZam2$SV_2_12m <- ifelse(dfZam2$SV_AttRape == 1 & (dfZam2$Q801 >= 1 & (dfZam2$Q802 == 1 | dfZam2$Q812 == 1)), 1, dfZam2$SV_2_12m)
table(dfZam2$SV_1_12m, useNA = "always")

# Physically forced sex in the last 12 months:
dfZam2$SV_3_12m <- ifelse(dfZam2$SV_Rape == 0 | (dfZam2$SV_Rape == 1 & (dfZam2$Q901 >= 1 & (dfZam2$Q902 == 2 | dfZam2$Q915 == 2))), 0, 1)
dfZam2$SV_3_12m <- ifelse(dfZam2$SV_Rape == 1 & (dfZam2$Q901 >= 1 & (dfZam2$Q902 == 1 | dfZam2$Q915 == 1)), 1, dfZam2$SV_3_12m)
table(dfZam2$SV_1_12m, useNA = "always")

# Pressured sex in the last 12 months:
dfZam2$SV_4_12m <- ifelse(dfZam2$SV_PreSex == 0 | (dfZam2$SV_PreSex == 1 & (dfZam2$Q1001 >= 1 & (dfZam2$Q1002 == 2 | dfZam2$Q1014 == 2))), 0, 1)
dfZam2$SV_4_12m <- ifelse(dfZam2$SV_PreSex == 1 & (dfZam2$Q1001 >= 1 & (dfZam2$Q1002 == 1 | dfZam2$Q1014 == 1)), 1, dfZam2$SV_4_12m)
table(dfZam2$SV_1_12m, useNA = "always")

# Any sexual violence in the past 12 months
dfZam2$SV_Any <- rowSums(dfZam2[,c("SV_1_12m","SV_2_12m", "SV_3_12m","SV_4_12m")], na.rm =TRUE)
dfZam2$SV_Any <- ifelse(rowSums(dfZam2[, c("SV_1_12m", "SV_2_12m", "SV_3_12m", "SV_4_12m")]) >= 1, 1, 0)
table(dfZam2$SV_Any, useNA = "always")
label(dfZam2$SV_Any) <- "Any sexual violence by everyone in the last 12m"

#------------------- Sexual violence BY PARTNERS AND NON-PARTNERS in the past 12 months 

# Unwanted sexual touching in the last 12 months:
dfZam2 <- dfZam2 %>%
  mutate(
    SV_1any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q712 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q712 == 1)) &
        (Q707 %in% c(5:16, 21:32) | Q717 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam2$SV_1any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfZam2$SV_1any_12m, useNA = "always")

#### Unwanted sexual touching by an intimate partner in the last 12 months:
# Unwanted sexual touching in the last 12 months:
dfZam2 <- dfZam2 %>%
  mutate(
    SV_1IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 2 | Q712 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q701 >= 1 & (Q702 == 1 | Q712 == 1)) &
        (Q707 %in% c(1: 4,17:20) | Q717 %in% c(1: 4,17:20)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam2$SV_1IPV_12m, "label") <- "Unwanted Sexual Touching by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfZam2$SV_1IPV_12m, useNA = "always")

# Unwanted attempted sex by ANYONE in the last 12 months:
# Recode SV_2_12m based on conditions
dfZam2 <- dfZam2 %>%
  mutate(
    SV_2any_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 2 | Q812 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 1 | Q812 == 1)) &
        (Q807 %in% c(5:16, 21:32) | Q817 %in% c(5:16, 21:32)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam2$SV_2any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"

# Check the distribution of the new variable
table(dfZam2$SV_2any_12m, useNA = "always")

# Unwanted attempted sex by INTIMATE PARTNER in the last 12 months:
# Recode SV_2_12m based on conditions
dfZam2 <- dfZam2 %>%
  mutate(
    SV_2IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,  # No attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 2 | Q812 == 2)) ~ 0,  # No recent attempted forced sex
      SV_AttRape == 1 & (Q801 >= 1 & (Q802 == 1 | Q812 == 1)) &
        (Q807 %in% c(1:4,17:20) | Q817 %in% c(1:4,17:20)) ~ 1,  # Recent unwanted touching by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam2$SV_2IPV_12m, "label") <- "Unwanted attempted sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable
table(dfZam2$SV_2IPV_12m, useNA = "always")

# Unwanted forced sex BY ANYONE in the last 12 months:
# Recode SV_3_12m based on conditions
dfZam2 <- dfZam2 %>%
  mutate(
    SV_3any_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 2 | Q915 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 1 | Q915 == 1)) &
        (Q907 %in% c(5:16, 21:32) | Q920 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam2$SV_3any_12m, "label") <- "Physically forced sex by anyone in the past 12 months"

# Check the distribution of the new variable
table(dfZam2$SV_3any_12m, useNA = "always")

# Unwanted forced sex BY AN INTIMATE PARTNER in the last 12 months:
# Recode SV_3_12m based on conditions
dfZam2 <- dfZam2 %>%
  mutate(
    SV_3IPV_12m = case_when(
      SV_Rape == 0 ~ 0,  # No physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 2 | Q915 == 2)) ~ 0,  # No recent physically forced sex
      SV_Rape == 1 & (Q901 >= 1 & (Q902 == 1 | Q915 == 1)) &
        (Q907 %in% c(1:4,17:20) | Q920 %in% c(1:4,17:20)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )
# Adding a label to the column
attr(dfZam2$SV_3IPV_12m, "label") <- "Physically forced sex by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfZam2$SV_3IPV_12m, useNA = "always")

# Pressured sex BY ANYONE in the last 12 months:
# Recode SV_4_12m based on conditions
dfZam2 <- dfZam2 %>%
  mutate(
    SV_4any_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1014 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1014 == 1)) &
        (Q1007 %in% c(5:16, 21:32) | Q1019 %in% c(5:16, 21:32)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam2$SV_4any_12m, "label") <- "Pressured Sex by anyone in the last 12 months"

# Check the distribution of the new variable (optional)
table(dfZam2$SV_4any_12m, useNA = "always")

# Pressured sex AN INTIMATE PARTNER in the last 12 months:
# Recode SV_4_12m based on conditions
dfZam2 <- dfZam2 %>%
  mutate(
    SV_4IPV_12m = case_when(
      SV_PreSex == 0 ~ 0,  # No pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 2 | Q1014 == 2)) ~ 0,  # No recent pressured sex
      SV_PreSex == 1 & (Q1001 >= 1 & (Q1002 == 1 | Q1014 == 1)) &
        (Q1007 %in% c(1:4,17:20)  | Q1019 %in% c(1:4,17:20)) ~ 1,  # Recent physically forced sex by a non-intimate partner
      TRUE ~ NA_real_  # All other cases set to NA
    )
  )

# Adding a label to the column
attr(dfZam2$SV_4IPV_12m, "label") <- "Pressured Sex by an intimate partner in the last 12 months"

# Check the distribution of the new variable 
table(dfZam2$SV_4IPV_12m, useNA = "always")

# Any sexual violence BY ANYONE in the past 12 months:
dfZam2$Any_SV_12m <- rowSums(dfZam2[,c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m")], na.rm =TRUE)
dfZam2$Any_SV_12m <- ifelse(is.na(dfZam2$Any_SV_12m), NA,
                            ifelse(dfZam2$Any_SV_12m >=1, 1, 0))

attr(dfZam2$Any_SV_12m, "label") <- "Any sexual violence by anyone in the last 12 months"
table(dfZam2$Any_SV_12m, useNA = "always")

#  sexual violence BY AN INTIMATE PARTNER in the past 12 months:
dfZam2$SV_IPV_12m <- rowSums(dfZam2[,c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m")], na.rm =TRUE)
dfZam2$SV_IPV_12m <- ifelse(is.na(dfZam2$SV_IPV_12m), NA,
                            ifelse(dfZam2$SV_IPV_12m >=1, 1, 0))

attr(dfZam2$SV_IPV_12m, "label") <- "Any sexual violence by an intimate partner in the last 12 months"
table(dfZam2$SV_IPV_12m, useNA = "always")

#------------------- Physical Intimate partner violence in a life time:
# Physical IPV in a lifetime
vars <- c("Q100A", "Q100B", "Q100C")
for (var in vars) {
  dfZam2[[paste0("IPV_", var)]] <- ifelse(dfZam2[[var]] == 1, 1, ifelse(dfZam2[[var]] == 2, 0, NA))
}

dfZam2$PV_IPV_lt <- rowSums(dfZam2[, grep("^IPV_", colnames(dfZam2))], na.rm = TRUE)
dfZam2$PV_IPV_lt <- ifelse(is.na(dfZam2$PV_IPV_lt), NA, ifelse(dfZam2$PV_IPV_lt >= 1, 1, 0))

attr(dfZam2$PV_IPV_lt, "label") <- "Lifetime physical IPV"
table(dfZam2$PV_IPV_lt, useNA = "always")

#------------------ Physical peer violence in a lifetime
vars_peer <- c("Q116A", "Q116B", "Q116C")
for (var in vars_peer) {
  dfZam2[[paste0("ViolPeer_", var)]] <- ifelse(dfZam2[[var]] == 1, 1, ifelse(dfZam2[[var]] == 2, 0, NA))
}

dfZam2$PV_Peers_lt <- rowSums(dfZam2[, grep("^ViolPeer_", colnames(dfZam2))], na.rm = TRUE)
dfZam2$PV_Peers_lt <- ifelse(is.na(dfZam2$PV_Peers_lt), NA, ifelse(dfZam2$PV_Peers_lt >= 1, 1, 0))

attr(dfZam2$PV_Peers_lt, "label") <- "Lifetime peer physical violence"
table(dfZam2$PV_Peers_lt, useNA = "always")

#----------------- Physical family violence
vars_fam <- c("Q128A", "Q128B", "Q128C")
for (var in vars_fam) {
  dfZam2[[paste0("ViolF_", var)]] <- ifelse(dfZam2[[var]] == 1, 1, ifelse(dfZam2[[var]] == 2, 0, NA))
}

dfZam2$PV_Fam_lt <- rowSums(dfZam2[, grep("^ViolF_", colnames(dfZam2))], na.rm = TRUE)
dfZam2$PV_Fam_lt <- ifelse(is.na(dfZam2$PV_Fam_lt), NA, ifelse(dfZam2$PV_Fam_lt >= 1, 1, 0))

attr(dfZam2$PV_Fam_lt, "label") <- "Lifetime family physical violence"
table(dfZam2$PV_Fam_lt, useNA = "always")

#---------------- Community Physical Violence (adults in neighbourhood)
vars_com <- c("Q142A", "Q142B", "Q142C")
for (var in vars_com) {
  dfZam2[[paste0("ViolC_", var)]] <- ifelse(dfZam2[[var]] == 1, 1, ifelse(dfZam2[[var]] == 2, 0, NA))
}

dfZam2$PV_Com_lt <- rowSums(dfZam2[, grep("^ViolC_", colnames(dfZam2))], na.rm = TRUE)
dfZam2$PV_Com_lt <- ifelse(is.na(dfZam2$PV_Com_lt), NA, ifelse(dfZam2$PV_Com_lt >= 1, 1, 0))

attr(dfZam2$PV_Com_lt, "label") <- "Lifetime community physical violence"
table(dfZam2$PV_Com_lt, useNA = "always")

#-------------- Any physical violence in a lifetime:
dfZam2$Any_PV_lt <- rowSums(dfZam2[,c("PV_IPV_lt", "PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfZam2$Any_PV_lt <- ifelse(is.na(dfZam2$Any_PV_lt), NA,
                           ifelse(dfZam2$Any_PV_lt >=1, 1, 0))

attr(dfZam2$Any_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfZam2$PV_Com_lt, useNA = "always")

####  Create JUST child physical violence variable: ####
# Child physical violence in a lifetime:
dfZam2$child_PV_lt <- rowSums(dfZam2[,c("PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfZam2$child_PV_lt <- ifelse(is.na(dfZam2$child_PV_lt), NA,
                             ifelse(dfZam2$child_PV_lt >=1, 1, 0))

attr(dfZam2$child_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfZam2$child_PV_lt, useNA = "always")

#------------ Physical violence variables IN THE LAST 12 MONTHS:-----------

# Physical IPV in the last 12 months:
dfZam2$PV_IPV_12m <- NA
dfZam2$PV_IPV_12m[dfZam2$PV_IPV_lt == 0 | 
                    (dfZam2$PV_IPV_lt == 1 & 
                       (dfZam2$Q101 >= 1 & 
                          (dfZam2$Q102 == 2 | dfZam2$Q109 == 2)))] <- 0
dfZam2$PV_IPV_12m[dfZam2$PV_IPV_lt == 1 & 
                    (dfZam2$Q101 >= 1 & 
                       (dfZam2$Q102 == 1 | dfZam2$Q109 == 1))] <- 1
attr(dfZam2$PV_IPV_12m, "label") <- "Intimate partner physical violence in the past 12 months"
table(dfZam2$PV_IPV_12m, useNA = "always")

# Physical peer violence in the last 12 months:
# Initialize the variable with NA
dfZam2$PV_Peers_12m <- NA
dfZam2$PV_Peers_12m[dfZam2$PV_Peers_lt == 0 | 
                      (dfZam2$PV_Peers_lt == 1 & 
                         (dfZam2$Q117 >= 1 & 
                            (dfZam2$Q118 == 2 | dfZam2$Q123 == 2)))] <- 0
dfZam2$PV_Peers_12m[dfZam2$PV_Peers_lt == 1 & 
                      (dfZam2$Q117 >= 1 & 
                         (dfZam2$Q118 == 1 | dfZam2$Q123 == 1))] <- 1
attr(dfZam2$PV_Peers_12m, "label") <- "Peers physical violence in the past 12 months"
table(dfZam2$PV_Peers_12m, useNA = "always")

# Physical family violence in the last 12 months:
dfZam2$PV_Fam_12m <- NA
dfZam2$PV_Fam_12m[dfZam2$PV_Fam_lt == 0 | 
                    (dfZam2$PV_Fam_lt == 1 & 
                       (dfZam2$Q129 >= 1 & 
                          (dfZam2$Q130 == 2 | dfZam2$Q136 == 2)))] <- 0
dfZam2$PV_Fam_12m[dfZam2$PV_Fam_lt == 1 & 
                    (dfZam2$Q129 >= 1 & 
                       (dfZam2$Q130 == 1 | dfZam2$Q136 == 1))] <- 1
attr(dfZam2$PV_Fam_12m, "label") <- "Family physical violence in the past 12 months"
table(dfZam2$PV_Fam_12m, useNA = "always")

# Physical violence in the community in the last 12 months:
dfZam2$PV_Com_12m <- NA
dfZam2$PV_Com_12m[dfZam2$PV_Com_lt == 0 | 
                    (dfZam2$PV_Com_lt == 1 & 
                       (dfZam2$Q143 >= 1 & 
                          (dfZam2$Q144 == 2 | dfZam2$Q149 == 2)))] <- 0
dfZam2$PV_Com_12m[dfZam2$PV_Com_lt == 1 & 
                    (dfZam2$Q143 >= 1 & 
                       (dfZam2$Q144 == 1 | dfZam2$Q149 == 1))] <- 1

attr(dfZam2$PV_Com_12m, "label") <- "Physical violence in the community in the past 12 months"
table(dfZam2$PV_Com_12m, useNA = "always")

# Any physical violence in the past 12 months:
dfZam2$PV_Any <- rowSums(dfZam2[, c("PV_IPV_12m", "PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfZam2$PV_Any <- ifelse(is.na(dfZam2$PV_Any), NA, ifelse(dfZam2$PV_Any >= 1, 1, 0))

attr(dfZam2$PV_Any, "label") <- "Any physical violence in the past 12 months"
table(dfZam2$PV_Any, useNA = "always")

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfZam2$child_PV_12m <- rowSums(dfZam2[, c("PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfZam2$child_PV_12m <- ifelse(is.na(dfZam2$child_PV_12m), NA, ifelse(dfZam2$child_PV_12m >= 1, 1, 0))

attr(dfZam2$child_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfZam2$child_PV_12m, useNA = "always")

#### creating just Child physical violence variable:
# Any child past-year physical violence in the last 12 months:
dfZam2$community_PV_12m <- rowSums(dfZam2[, c("PV_Peers_12m", "PV_Com_12m")], na.rm = TRUE)
dfZam2$community_PV_12m <- ifelse(is.na(dfZam2$community_PV_12m), NA, ifelse(dfZam2$community_PV_12m >= 1, 1, 0))

attr(dfZam2$community_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfZam2$community_PV_12m, useNA = "always")

#----------------------Emotional violence in a lifetime

# Any lifetime emotional violence by parent, adult caregiver, or other adult relative
vars <- c("Q300A", "Q300B", "Q300C")
for (var in vars) {
  dfZam2[[paste0("EViolF_", var)]] <- ifelse(dfZam2[[var]] == 1, 1, ifelse(dfZam2[[var]] == 2, 0, NA))
}

dfZam2$EV_Fam_lt <- rowSums(dfZam2[, grep("^EViolF_", colnames(dfZam2))], na.rm = TRUE)
dfZam2$EV_Fam_lt <- ifelse(is.na(dfZam2$EV_Fam_lt), NA, ifelse(dfZam2$EV_Fam_lt >= 1, 1, 0))

attr(dfZam2$EV_Fam_lt, "label") <- "Any lifetime emotional violence by parent, adult caregiver, or other adult relative"
table(dfZam2$EV_Fam_lt, useNA = "always")

#------Any lifetime emotional violence by PEERS 
vars_EV_peer_lt <- c("Q310A", "Q310B", "Q310C")
for (var in vars_EV_peer_lt) {
  dfZam2[[paste0("EViolPeer_", var)]] <- ifelse(dfZam2[[var]] == 1, 1, ifelse(dfZam2[[var]] == 2, 0, NA))
}

dfZam2$EV_Peer_lt <- rowSums(dfZam2[, grep("^EViolPeer_", colnames(dfZam2))], na.rm = TRUE)
dfZam2$EV_Peer_lt <- ifelse(is.na(dfZam2$EV_Peer_lt), NA, ifelse(dfZam2$EV_Peer_lt >= 1, 1, 0))

attr(dfZam2$EV_Peer_lt, "label") <- "Lifetime emotional violence by peers"
table(dfZam2$EV_Peer_lt, useNA = "always")

#----------------------Emotional violence in the last 12 months
# Emotional violence by parent, adult caregiver, or other adult relative in the past 12 months
dfZam2$EV_Fam_12m <- NA
dfZam2$EV_Fam_12m[dfZam2$EV_Fam_lt == 0 | 
                    (dfZam2$EV_Fam_lt == 1 & 
                       (dfZam2$Q301 == 2 | dfZam2$Q301 == 3 & 
                          (dfZam2$Q302 == 2 | dfZam2$Q306 == 2)))] <- 0
dfZam2$EV_Fam_12m[dfZam2$EV_Fam_lt == 1 & 
                    (dfZam2$Q301 == 2 | dfZam2$Q301 == 3 & 
                       (dfZam2$Q302 == 1 | dfZam2$Q306 == 1))] <- 1
attr(dfZam2$EV_Fam_12m, "label") <- "Any emotional violence by parent, adult caregiver, or other adult relative in the past 12 months"
table(dfZam2$EV_Fam_12m, useNA = "always")

# Emotional violence by Peers in the past 12 months
dfZam2$EV_Peer_12m <- NA
dfZam2$EV_Peer_12m[dfZam2$EV_Peer_lt == 0 | 
                     (dfZam2$EV_Peer_lt == 1 & 
                        (dfZam2$Q312 == 2 | dfZam2$Q319 == 2))] <- 0
dfZam2$EV_Peer_12m[dfZam2$EV_Peer_lt == 1 & 
                     (dfZam2$Q311 == 1 | dfZam2$Q311 == 2 | dfZam2$Q311 == 3 & 
                        (dfZam2$Q312 == 1 | dfZam2$Q319 == 1))] <- 1
attr(dfZam2$EV_Peer_12m, "label") <- "Any emotional violence by peers in the last 12 months"
table(dfZam2$EV_Peer_12m, useNA = "always")

#------Any emotional violence by ANYONE in the past 12 months: 
dfZam2$EV_Any_12m <- rowSums(dfZam2[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfZam2$EV_Any_12m <- ifelse(is.na(dfZam2$EV_Any_12m), NA, ifelse(dfZam2$EV_Any_12m >= 1, 1, 0))

attr(dfZam2$EV_Any_12m, "label") <- "Any Emotional violence in the past 12 months"
table(dfZam2$EV_Any_12m, useNA = "always")

#------Any emotional violence by ANYONE in the past 12 months: 
dfZam2$EV_Any_12m <- rowSums(dfZam2[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfZam2$EV_Any_12m <- ifelse(is.na(dfZam2$EV_Any_12m), NA, ifelse(dfZam2$EV_Any_12m >= 1, 1, 0))

attr(dfZam2$EV_Any_12m, "label") <- "Any Emotional violence in the past 12 months"
table(dfZam2$EV_Any_12m, useNA = "always")

#------Any emotional violence by ANYONE in the past 12 months: 
dfZam2$EV_Any <- rowSums(dfZam2[, c("EV_Peer_12m", "EV_Fam_12m")], na.rm = TRUE)
dfZam2$EV_Any <- ifelse(is.na(dfZam2$EV_Any), NA, ifelse(dfZam2$EV_Any >= 1, 1, 0))

attr(dfZam2$EV_Any, "label") <- "Any Emotional violence in the past 12 months"
table(dfZam2$EV_Any, useNA = "always")


#selecting the needed vars:
dfZam2$EV_IPV_12m <- NA
dfZam2 <- dplyr::select(dfZam2,
                        Q2,
                        HHSex,
                        inf_housing,
                        strata,
                        cluster,
                        sampleweight,
                        HDATE_VF,
                        month,
                        year,
                        region,
                        country,
                        AgeGroup,
                        sex,
                        married,
                        food_insec,
                        TransSex,
                        rural_urban,
                        EducationEver,
                        School_enrol,
                        Orphan,
                        Any_SV_12m,
                        EV_Any_12m,
                        InconsCondom_12m,
                        sex_multiple_12m,
                        ADS5Y,
                        child_PV_12m,
                        community_PV_12m,
                        PV_Fam_12m,
                        PV_IPV_12m,
                        SV_IPV_12m,
                        EV_IPV_12m,
                        SV_Any,
                        EV_Any,
                        PV_Any)


#apply survey weights:


#load the dataset
dfZmb <- read_dta("Data/Zimbabwe/Zimbabwe VACS Public Use Data Package/Zimbabwe VACS Public Use Data Package/Zimbabwe VACS Public Use Data Package/zimbabwe_pud_rurban.dta")

#load the shapefile
Zimbabwe_shape <- read_sf("Data/Shapefiles/Shapefile.shp/Zimbabwe/shps/sdr_subnational_boundaries.shp")

#change dfZmb province column to region (harmonising column names)
dfZmb <-  dfZmb %>%
  rename(region = province)

#change Zimbabwe_shape CNTRYNAMEE to country (harmonising column names)
Zimbabwe_shape <- Zimbabwe_shape %>%
  rename(country = CNTRYNAMEE)

#change Zimbabwe_shape DHSREGEN to region and harmonise region names to match dfZmb

Zimbabwe_shape <- Zimbabwe_shape %>%
  rename(region = DHSREGEN) %>%
  mutate(region = case_when(
    region == "Mashonaland Central" ~ "Mash Central",
    region == "Mashonaland East" ~ "Mash East",
    region == "Mashonaland West" ~ "Mash West",
    region == "Matabeleland North" ~ "Mat North",
    region == "Matabeleland South" ~ "Mat South",
    region == "Harare Chitungwiza" ~ "Harare",
    TRUE ~ region
  ))

#check if changed and correct:
table(Zimbabwe_shape$region)
table(dfZmb$region)

#rural urban variable: code Harare to Urban and Bulawayo to Rural
dfZmb <- dfZmb %>%
  mutate(rural_urban1 = ifelse(region %in% c("Harare", "Bulawayo"), "Urban", "Rural"))
table(dfZmb$rural_urban1)

#recode rural_urban column to 1 and 2 
dfZmb <- dfZmb %>%
  mutate(rural_urban = ifelse(rural_urban1 == "Urban", 1, 2))
table(dfZmb$rural_urban)

#check regional identifiers in dfZmb and Zimbabwe_shape
table(dfZmb$region)
table(Zimbabwe_shape$region)

#PREPARING THE VACS SURVEY DATA:

#code survey weights:
#Create a survey design object using svydesign
dfZmb <- dfZmb %>%
  rename(cluster = GeoCode,
         strata = district1,
         sampleweight = finalwt)


#create a WI variable with the observations of n
dfZmb <- dfZmb %>%
  mutate(WI = row_number())

#Apply weights to WI using the individual weight variable
sum_w <- sum(dfZmb$WI * dfZmb$sampleweight)  # Calculate the sum of weighted numbers
dfZmb$WI_1 <- sum_w  # Replace WI column with the sum
table(dfZmb$WI_1)

#create country column
dfZmb$country <- "Zimbabwe"

#double check the rural urban variable
table(dfZmb$rural_urban)

#timing of interview
dfZmb <- dfZmb %>%
  rename(HDATE_VF = hdate_vf)
table(dfZmb$HDATE_VF)

# Extracting year, month, and day information
dfZmb <- dfZmb %>% 
  mutate(
    year = as.integer(substr(HDATE_VF, 1, 4)),
    month = as.integer(substr(HDATE_VF, 5, 6))
  )

#checking the coded variables
table(dfZmb$year, useNA = "always")
table(dfZmb$month, useNA = "always")

#Timestamp information is ready:
table(dfZmb$HDATE_VF)

#convert the timestamp variable to day-month-year format
dfZmb$HDATE_VF <- paste(substr(dfZmb$HDATE_VF,1,4), substr(dfZmb$HDATE_VF,5,6), substr(dfZmb$HDATE_VF,7,8), sep = "-")
table(dfZmb$HDATE_VF)

#--------------------DEMOGRAPHIC VARIABLES-----------------

#Sex of head of household
dfZmb <- dfZmb %>%
  rename(HHSex = h2)

table(dfZmb$HHSex)
attr(dfZmb$HHSex, "label") <- "Head of household sex"

#Regional variable 
table(dfZmb$region)

#gender
dfZmb$sex

# Marital status: any participant married or cohabiting as if married
dfZmb <- dfZmb %>%
  mutate(married = ifelse((q31_rc == 1 | q33 == 1 | q34 == 1), 1 , 0)) %>%
  mutate(married = replace(married, is.na(married), 0))
table(dfZmb$married, useNA = "always")

# Labeling
attr(dfZmb$married, "label") <- "Currently married or cohabiting as if married"

#Food insecurity:
dfZmb$food_insec <- 0
dfZmb$food_insec[dfZmb$h36 == 1 | dfZmb$h37 == 1] <- 1
table(dfZmb$food_insec, useNA = "always")

#Age variable
# Age group
dfZmb$AgeGroup <- 0
dfZmb$AgeGroup[dfZmb$q2 >= 13 & dfZmb$q2 <= 17] <- 1
dfZmb$AgeGroup[dfZmb$q2 >= 18 & dfZmb$q2 <= 24] <- 2

# Labeling variables 
attr(dfZmb$AgeGroup, "label") <- "age group of girls"
label_values <- c("13-17", "18-24")
attr(dfZmb$AgeGroup, "labels") <- label_values

# Displaying q2 and AgeGroup for the first 30 rows
dfZmb[1:30, c("q2", "AgeGroup")]

# Tabulating q2 and AgeGroup
table(dfZmb$q2, dfZmb$AgeGroup)

#Rename age variable to match the other datasets
dfZmb <- dfZmb %>% 
  rename(Q2 = q2)

#--------------- Education attainment

# Check missing values in each variable
table(!is.na(dfZmb$q3)) # no missing values issue (only 1 DNK and 3 DECLINED)
table(!is.na(dfZmb$q4)) # 141 missing, 1 DNK and 1 DECLINED
table(!is.na(dfZmb$q6)) # 5,214 missing dfZmb (more than 60%), 4 DNK
table(!is.na(dfZmb$q7)) # 3,497 missing dfZmb, 1 DNK and 4 DECLINED

# Recode values 98 and 99 as missing (.)
dfZmb$Q3_ <- ifelse(dfZmb$q3 %in% c(98, 99), NA, dfZmb$q3)
dfZmb$Q4_ <- ifelse(dfZmb$q4 %in% c(98, 99), NA, dfZmb$q4)
dfZmb$Q5_ <- ifelse(dfZmb$q5 %in% c(98, 99), NA, dfZmb$q5)
dfZmb$Q6_ <- ifelse(dfZmb$q6 %in% c(98, 99), NA, dfZmb$q6)


# Cross-tabulation for imputation
cross_table <- table(dfZmb$Q3_, dfZmb$Q5_, useNA = "always")
print(cross_table)

# Recode Q6_ and Q7_ to regroup into 1. primary or less, 2. secondary, 3. higher than secondary
dfZmb$Q5_ <- ifelse(dfZmb$Q3_ == 2, 0, dfZmb$Q5_)
dfZmb$Q5_ <- ifelse(dfZmb$Q5_ %in% c(0, 1, 2), 1, dfZmb$Q5_)
dfZmb$Q5_ <- ifelse(dfZmb$Q5_ ==3, 2, dfZmb$Q5_)
dfZmb$Q5_ <- ifelse(dfZmb$Q5_ == 4, 3, dfZmb$Q5_)

dfZmb$Q6_ <- ifelse(dfZmb$Q3_ == 2, 0, dfZmb$Q6_)
dfZmb$Q6_ <- ifelse(dfZmb$Q6_ %in% c(0, 1, 2), 1, dfZmb$Q6_)
dfZmb$Q6_ <- ifelse(dfZmb$Q6_ ==3, 2, dfZmb$Q6_)
dfZmb$Q6_ <- ifelse(dfZmb$Q6_ == 4, 3, dfZmb$Q6_)

# Replace missing values in Q5_ for those with the value of primary or less in Q6_
dfZmb$Q5_[dfZmb$Q6_ == 1 & is.na(dfZmb$Q5_)] <- 1
dfZmb$Q5_[dfZmb$Q6_ == 2 & is.na(dfZmb$Q5_)] <- 2
dfZmb$Q5_[dfZmb$Q6_ == 3 & is.na(dfZmb$Q5_)] <- 3

# Create a new variable "EducationEver"
dfZmb$EducationEver <- dfZmb$Q5_

#label the Education variable:
label(dfZmb$EducationEver) <- "Education attainment"

# Define value labels for EducationEver
value_labels <- c("Completed primary school or less", "Completed secondary school", "Higher than secondary")

# Apply value labels to EducationEver
dfZmb$EducationEver <- factor(dfZmb$EducationEver, levels = c(1, 2, 3), labels = value_labels)
table(dfZmb$EducationEver, useNA = "always")

# Create a new variable "EducationEver2" with two categories
dfZmb$EducationEver2 <- ifelse(dfZmb$EducationEver %in% c("Completed secondary school", "Higher than secondary"), 1, 0)

# Drop the original EducationEver variable
dfZmb <- dfZmb %>% dplyr::select(-EducationEver)            

# Rename EducationEver2 to EducationEver
dfZmb <- dfZmb %>% rename(EducationEver = EducationEver2)
table(dfZmb$EducationEver, useNA = "always")

# label the EducationEver variable:
attr(dfZmb$EducationEver, "label") <- "Completed secondary or higher level of education"
table(dfZmb$EducationEver, useNA = "always")

#--Current school enrollment
dfZmb$School_enrol <- ifelse(dfZmb$Q3_ == 2 & is.na(dfZmb$Q4_), 2, dfZmb$Q4_)
dfZmb$School_enrol[dfZmb$School_enrol == 2] <- 0

# Labeling variable
attr(dfZmb$School_enrol, "label") <- "School enrollemnt"
table(dfZmb$School_enrol)

#--------Informal housing variable: based on no electricity and weak roof material
dfZmb <- dfZmb %>%
  mutate(inf_housing = ifelse(h18a == 2, 1,  #electricity
                              ifelse(h29 %in% c(1:7), 1, 0)))   #roof material

table(dfZmb$inf_housing, useNA = "always")

#--------Poverty or wealth quintile:
# Load necessary libraries
library(dplyr)
library(haven)
library(psych)

# Drinking Water supply
dfZmb <- dfZmb %>%
  mutate(
    wpiped = ifelse(h4 == 1:3, 1, 0), #piped water
    wbore = ifelse(h4 == 4, 1, 0), #bore
    wwell = ifelse(h4 %in% c(5, 6), 1, 0), #well
    wsurf = ifelse(h4 %in% 7:12, 1, 0),  #surface
    woth = ifelse(h4 >= 13 & h4 <= 99, 1, 0)
  )

# Other purposes Water supply
dfZmb <- dfZmb %>%
  mutate(
    wpiped1 = ifelse(h4 == 1:3, 1, 0), #piped water
    wbore1 = ifelse(h4 == 4, 1, 0), #bore
    wwell1 = ifelse(h4 %in% c(5, 6), 1, 0), #well
    wsurf1 = ifelse(h4 %in% 7:12, 1, 0),  #surface
    woth1 = ifelse(h4 >= 13 & h4 <= 99, 1, 0)
  )

# Type of toilet
dfZmb_clean$tlatop <- 0
dfZmb_clean$tnonoth <- 0

dfZmb<- dfZmb %>%
  mutate(
    tflush = ifelse(h12 %in% 1:3, 1,0),   #flush system: hygienic 
    tlatimp = ifelse(h12 %in% 5:6, 1, 0),  #ventilated and covered
    tlatop = ifelse(h12 %in% 7:10, 1, 0),   #open no hygiene measures
    tnonoth = ifelse(h12 %in% c(4, 11, 98,99), 1, 0)
  )

# Whether toilet is shared
dfZmb <- dfZmb %>% mutate(fshare = ifelse(h14 == 1, 1, 0))
table(dfZmb$fshare, useNA = "always")

# Cooking fuel
dfZmb <- dfZmb %>%
  mutate(
    clpg = ifelse(h22 %in% 1:3, 1, 0),    #modern
    cchar = ifelse(h22 %in% 4:6, 1, 0),  #charcoal
    cwood = ifelse(h22 %in% 7:9, 1, 0),     #wood and natural sources
    cnone = ifelse(h22 >= 10 & h22 <= 99, 1, 0)
  )

# Floor material
dfZmb <- dfZmb %>%
  mutate(
    fnat = ifelse(h27 %in% 1:2, 1, 0),  #natural
    fnat = ifelse(h27 %in% 3:5, 1, 0),  #rudimentary
    ffin = ifelse(h27 %in% 6:10, 1, 0),  #finished
    foth = ifelse(h27 >= 88 & h27 <= 99, 1, 0)
  )

# Roof material
dfZmb <- dfZmb %>%
  mutate(
    rnat = ifelse(h29 %in% c(2:5, 7), 1, 0),    #rudimentary
    rmetal = ifelse(h29 == 6, 1, 0),            #metal roofing 
    rfin = ifelse(h29 %in% 8:11, 1, 0),         #finished
    roth = ifelse(h29 %in% c(1, 6, 88,98,99), 1, 0)   #no roof others
  )

# Wall material
dfZmb <- dfZmb %>%
  mutate(
    wallnat = ifelse(h30 %in% 2:4, 1, 0),            #natural
    wallrud = ifelse(h30 %in% 5:8, 1, 0),            #rudimentary
    wallcem = ifelse(h30 %in% c(9, 10, 13), 1, 0),   #cement
    wallfin = ifelse(h30 %in% c(11:12, 14), 1, 0),   #finished
    walloth = ifelse(h30 %in% c(1, 88, 99), 1, 0)
  )

# Asset ownership
dfZmb <- dfZmb %>%
  mutate(
    h18ef = ifelse(h18e == 1 | h18f == 1, 1, 2),
    h18c = ifelse(h18c == 1, 1, 2)
  )

asset_vars <- c("h18a", "h18d", "h18ef", "h18g", "h18h", "h18i","h18j","h18k","h18l"
                ,"h19b","h19c", "h19e" )

dfZmb <- dfZmb %>%
  mutate(across(all_of(asset_vars), ~ ifelse(. == 1, 1, 0), .names = "own{col}"))

# PCA
pca_results <- principal(dplyr::select(dfZmb, wpiped:woth, wpiped1:woth1, tflush:tnonoth, fshare,
                                       clpg:cnone, fnat:foth, rnat:roth, wallnat:walloth, 
                                       starts_with("own")), nfactors = 1, 
                         scores = TRUE, weight = dfZmb$sampleweight)

dfZmb$wealthscore <- pca_results$scores

# Wealth quintiles
dfZmb <- dfZmb %>% mutate(quintile = ntile(wealthscore, 5))

# Labeling quintiles
dfZmb <- dfZmb %>%
  mutate(
    quintile = factor(quintile, labels = c("Poorest", "Second", "Third", "Fourth", "Wealthiest"))
  )

# Recode quintile into poor/non-poor
dfZmb <- dfZmb %>%
  mutate(
    poor = ifelse(quintile == "Poorest", 1, 0))


table(dfZmb$poor)

#--------------------Orphanhood

# Maternal orphanhood
dfZmb <- dfZmb %>%
  mutate(OrphMater = ifelse(q17 == 2 & (q19 == 1 | q20 == 2), 1,
                            ifelse(q17 == 1 | (q17 == 2 & q20 == 1), 0, NA)))
table(dfZmb$OrphMater, useNA = "always")
attr(dfZmb$OrphMater, "label") <- "Maternal orphanhood"

# Paternal orphanhood
dfZmb <- dfZmb %>%
  mutate(OrphPater = ifelse(q23 == 2 & (q26 == 2 | q25 == 1), 1,
                            ifelse(q23 == 1 | (q23 == 2 & q26 == 2), 0, NA)))
table(dfZmb$OrphPater, useNA = "always")
attr(dfZmb$OrphPater, "label") <- "Paternal orphanhood"
table(dfZmb$OrphPater, dfZmb$OrphMater, useNA = "always")

# Orphanhood:
dfZmb <- dfZmb %>%
  mutate(Orphan = case_when(
    OrphMater == 0 & OrphPater == 0 ~ 0,
    OrphMater == 1 | OrphPater == 1 ~ 1,
    OrphMater == 1 & OrphPater == 1 ~ 1,
    OrphMater == 0 & OrphPater == 1 ~ 1,
    OrphMater == 1 & OrphPater == 0 ~ 1,
    TRUE ~ 0
  ))

#Labeling variable
attr(dfZmb$Orphan,"label") <- "Orphanhood"
table(dfZmb$Orphan, useNA = "always")

#--------Violence variables----------------
#-------------Life time sexual violence variables --------------
# Unwanted sexual touching
dfZmb$SV_Q600 <- ifelse(dfZmb$q600 == 1, 1, ifelse(dfZmb$q600 == 2, 0, NA))
label(dfZmb$SV_Q600) <- "Unwanted sex touching"

table(dfZmb$SV_Q600, useNA = "always")

# Attempted forced sex
dfZmb$SV_AttRape <- ifelse(dfZmb$q700 == 1, 1, ifelse(dfZmb$q700 == 2, 0, NA))
label(dfZmb$SV_AttRape) <- "Attempted forced sex"
table(dfZmb$SV_AttRape, useNA = "always")

# Physically forced sex
dfZmb$SV_Rape <- ifelse(dfZmb$q800 == 1, 1, ifelse(dfZmb$q800 == 2, 0, NA))
label(dfZmb$SV_Rape) <- "Physically forced sex"
table(dfZmb$SV_Rape, useNA = "always")

# Pressured sex
dfZmb$SV_PreSex <- ifelse(dfZmb$q900 == 1, 1, ifelse(dfZmb$q900 == 2, 0, NA))
label(dfZmb$SV_PreSex) <- "Pressured sex"

table(dfZmb$SV_PreSex, useNA = "always")

# Any lifetime sexual violence
dfZmb$Any_SV_lifetime <- rowSums(dfZmb[, c("SV_Q600", "SV_AttRape", "SV_Rape", "SV_PreSex")], na.rm = TRUE)
dfZmb$Any_SV_lifetime <- ifelse(is.na(dfZmb$Any_SV_lifetime), NA, ifelse(dfZmb$Any_SV_lifetime >= 1, 1, 0))
label(dfZmb$Any_SV_lifetime) <- "Any lifetime sexual violence"
table(dfZmb$Any_SV_lifetime, useNA = "always")

# #------------------- Sexual violence in the past 12 months BY ANYONE
# # Unwanted sexual touching in the last 12 months:
dfZmb$SV_1_12m <- 0
dfZmb$SV_1_12m[dfZmb$SV_Q600 == 0 | 
                 (dfZmb$SV_Q600 == 1 & 
                    (dfZmb$q601 >= 1 & 
                       (dfZmb$q602 == 2 | dfZmb$q612 == 2)))] <- 0

# Assign 1 to cases meeting the second condition
dfZmb$SV_1_12m[dfZmb$SV_Q600 == 1 & 
                 (dfZmb$q601 >= 1 & 
                    (dfZmb$q602 == 1 | dfZmb$q612 == 1))] <- 1

table(dfZmb$SV_1_12m, useNA = "always")

# # Unwanted attempted sex in the last 12 months:
dfZmb$SV_2_12m <- 0
dfZmb$SV_2_12m[dfZmb$SV_AttRape == 0 | 
                 (dfZmb$SV_AttRape == 1 & 
                    (dfZmb$q701 >= 1 & 
                       (dfZmb$q702 == 2 | dfZmb$q712 == 2)))] <- 0
dfZmb$SV_2_12m[dfZmb$SV_AttRape == 1 & 
                 (dfZmb$q701 >= 1 & 
                    (dfZmb$q702 == 1 | dfZmb$q712 == 1))] <- 1
table(dfZmb$SV_2_12m, useNA = "always")

# Adding a label to the column
attr(dfZmb$SV_2_12m, "label") <- "Unwanted attempted sex in the last 12 months"

# Physically forced sex in the last 12 months:
dfZmb$SV_3_12m <- 0
dfZmb$SV_3_12m[dfZmb$SV_Rape == 0 | 
                 (dfZmb$SV_Rape == 1 & 
                    (dfZmb$q801 >= 1 & 
                       (dfZmb$q802 == 2 | dfZmb$q815 == 2)))] <- 0
dfZmb$SV_3_12m[dfZmb$SV_Rape == 1 & 
                 (dfZmb$q801 >= 1 & 
                    (dfZmb$q802 == 1 | dfZmb$q815 == 1))] <- 1
table(dfZmb$SV_3_12m, useNA = "always")

# Adding a label to the column
attr(dfZmb$SV_3_12m, "label") <- "Physically forced sex in the past 12 months"

# Pressured sex in the last 12 months:
dfZmb$SV_4_12m <- 0
dfZmb$SV_4_12m[dfZmb$SV_PreSex == 0 | 
                 (dfZmb$SV_PreSex == 1 & 
                    (dfZmb$q901 >= 1 & 
                       (dfZmb$q902 == 2 | dfZmb$q914 == 2)))] <- 0
dfZmb$SV_4_12m[dfZmb$SV_PreSex == 1 & 
                 (dfZmb$q901 >= 1 & 
                    (dfZmb$q902 == 1 | dfZmb$q914 == 1))] <- 1

# Generate a frequency table with missing values included
table(dfZmb$SV_4_12m, useNA = "always")

# Adding a label to the column
attr(dfZmb$SV_4_12m, "label") <- "Pressured Sex in the last 12 months" 

# Any sexual violence BY ANYONE in the past 12 months
dfZmb$SV_Any <- rowSums(dfZmb[,c("SV_1_12m","SV_2_12m", "SV_3_12m","SV_4_12m")], na.rm =TRUE)

dfZmb <- dfZmb %>%
  mutate(SV_Any = ifelse(rowSums(dfZmb[, c("SV_1_12m", "SV_2_12m", "SV_3_12m", "SV_4_12m")]) >= 1, 1, 0))
label(dfZmb$SV_Any) <- "Any sexual violence in the last 12m"
table(dfZmb$SV_Any, useNA = "always")

#------------------- Sexual violence in the past 12 months BY NON-PARTNERS AND PARTNERS:
# Unwanted sexual touching BY NON-PARTNERS in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_1any_12m = case_when(
      SV_Q600 == 0 ~ 0,  
      SV_Q600 == 1 &  (q601 >= 1 & (q602 == 2 | q612 == 2)) ~ 0,
      SV_Q600 == 1 &  (q601 >= 1 & (q602 == 1 | q612 == 1) & 
                         (q607 %in% c(5:16, 21:32) | q617 %in% c(5:16, 21:32))) ~ 1,
      TRUE ~ 0
    )
  )

# Check the distribution of the new variable
table(dfZmb$SV_1any_12m, useNA = "always")

label(dfZmb$SV_1any_12m) <- "sexual violence by anyone in the past 12 months"

# Unwanted sexual touching AN INTIMATE PARTNER in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_1IPV_12m = case_when(
      SV_Q600 == 0 ~ 0,  
      SV_Q600 == 1 &  (q601 >= 1 & (q602 == 2 | q612 == 2)) ~ 0,
      SV_Q600 == 1 &  (q601 >= 1 & (q602 == 1 | q612 == 1) & 
                         (q607 %in% c(1:4, 17:20) | q617 %in% c(1:4, 17:20))) ~ 1,
      TRUE ~ 0
    )
  )

label(dfZmb$SV_1IPV_12m) <- "sexual violence by an intimate partner in the past 12 months"

# Check the distribution of the new variable
table(dfZmb$SV_1IPV_12m, useNA = "always")

# Unwanted attempted sex BY NON-PARTNERS in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_2any_12m = case_when(
      SV_AttRape == 0 ~ 0,
      SV_AttRape == 1 & (q701 >= 1 & (q702 == 2 | q712 == 2)) ~ 0,
      SV_AttRape == 1 & (q701 >= 1 & (q702 == 1 | q712 == 1)  &
                           (q707 %in% c(5:16, 21:32) | q717 %in% c(5:16, 21:32))) ~ 1,
      TRUE ~ 0
    ))


# Adding a label to the column
attr(dfZmb$SV_2any_12m, "label") <- "Unwanted attempted sex by anyone in the last 12 months"
table(dfZmb$SV_2any_12m, useNA = "always")

# Unwanted attempted sex BY AN INTIMATE PARTNER in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_2IPV_12m = case_when(
      SV_AttRape == 0 ~ 0,
      SV_AttRape == 1 & (q701 >= 1 & (q702 == 2 | q712 == 2)) ~ 0,
      SV_AttRape == 1 & (q701 >= 1 & (q702 == 1 | q712 == 1)  &
                           (q707 %in% c(1:4, 16:20) | q717 %in% c(1:4, 16:20))) ~ 1,
      TRUE ~ 0
    ))

# Adding a label to the column
attr(dfZmb$SV_2IPV_12m, "label") <- "Unwanted attempted sex by an intimate partner in the last 12 months"
table(dfZmb$SV_2IPV_12m, useNA = "always")

# Physically forced sex BY NON-PARTNERS in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_3any_12m = case_when(
      SV_Rape == 0 ~ 0,
      SV_Rape == 1 & (q801 >= 1 & (q802 == 2 | q815 == 2)) ~ 0,
      SV_Rape == 1 & (q801 >= 1 & (q802 == 1 | q815 == 1) &
                        (q807 %in% c(5:16, 21:32) | q820 %in% c(5:16, 21:32))) ~ 1,
      TRUE ~ 0
    )
  )

# Adding a label to the column
attr(dfZmb$SV_3any_12m, "label") <- "Physically forced sex by anyone in the past 12 months"
table(dfZmb$SV_3any_12m, useNA = "always")

# Physically forced sex BY AN INTIMATE PARTNER in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_3IPV_12m = case_when(
      SV_Rape == 0 ~ 0,
      SV_Rape == 1 & (q801 >= 1 & (q802 == 2 | q815 == 2)) ~ 0,
      SV_Rape == 1 & (q801 >= 1 & (q802 == 1 | q815 == 1) &
                        (q807 %in% c(1:4, 16:20) | q820 %in% c(1:4, 16:20))) ~ 1,
      TRUE ~ 0
    )
  )

# Adding a label to the column
attr(dfZmb$SV_3IPV_12m, "label") <- "Physically forced sex by an intimate partner in the past 12 months"
table(dfZmb$SV_3IPV_12m, useNA = "always")

# Pressured sex BY NON-PARTNERS in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_4any_12m = case_when(
      SV_PreSex == 0 ~ 0,
      SV_PreSex == 1 & (q901 >= 1 & (q902 == 2 | q914 == 2)) ~ 0,
      SV_PreSex == 1 & (q901 >= 1 & (q902 == 1 | q914 == 1) &
                          (q907 %in% c(5:16, 21:32) | q919 %in% c(5:16, 21:32))) ~ 1,
      TRUE ~ 0
    )
  )

table(dfZmb$SV_4any_12m, useNA = "always")
# Adding a label to the column
attr(dfZmb$SV_4any_12m, "label") <- "Pressured Sex by anyone in the last 12 months"

# Pressured sex BY AN INTIMATE PARTNER in the last 12 months:
dfZmb <- dfZmb %>%
  mutate(
    SV_4IPV_12m = case_when(
      SV_PreSex == 0 ~ 0,
      SV_PreSex == 1 & (q901 >= 1 & (q902 == 2 | q914 == 2)) ~ 0,
      SV_PreSex == 1 & (q901 >= 1 & (q902 == 1 | q914 == 1) &
                          (q907 %in% c(1:4, 16:20) | q919 %in% c(1:4, 16:20))) ~ 1,
      TRUE ~ 0
    )
  )

table(dfZmb$SV_4IPV_12m, useNA = "always")
# Adding a label to the column
attr(dfZmb$SV_4IPV_12m, "label") <- "Pressured Sex by an intimate partner in the last 12 months"

# Any sexual violence BY NON-PARTNERS in the past 12 months
dfZmb$Any_SV_12m <- rowSums(dfZmb[,c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m")], na.rm =TRUE)

dfZmb <- dfZmb %>%
  mutate(Any_SV_12m = ifelse(rowSums(dfZmb[, c("SV_1any_12m","SV_2any_12m", "SV_3any_12m","SV_4any_12m")]) >= 1, 1, 0))
label(dfZmb$Any_SV_12m) <- "Any sexual violence in the last 12m"
table(dfZmb$Any_SV_12m, useNA = "always")

# Any sexual violence BY INTIMATE PARTNER in the past 12 months
dfZmb$SV_IPV_12m <- rowSums(dfZmb[,c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m")], na.rm =TRUE)

dfZmb <- dfZmb %>%
  mutate(SV_IPV_12m = ifelse(rowSums(dfZmb[, c("SV_1IPV_12m","SV_2IPV_12m", "SV_3IPV_12m","SV_4IPV_12m")]) >= 1, 1, 0))
label(dfZmb$SV_IPV_12m) <- "Any sexual violence by an intimate partner in the last 12m"
table(dfZmb$SV_IPV_12m, useNA = "always")

#------------------- Physical violence in a life time:
# Physical IPV in a lifetime
vars <- c("q100a", "q100b", "q100c")
for (var in vars) {
  dfZmb[[paste0("IPV_", var)]] <- ifelse(dfZmb[[var]] == 1, 1, ifelse(dfZmb[[var]] == 2, 0, NA))
}

dfZmb$PV_IPV_lt <- rowSums(dfZmb[, grep("^IPV_", colnames(dfZmb))], na.rm = TRUE)
dfZmb$PV_IPV_lt <- ifelse(is.na(dfZmb$PV_IPV_lt), NA, ifelse(dfZmb$PV_IPV_lt >= 1, 1, 0))

attr(dfZmb$PV_IPV_lt, "label") <- "Lifetime physical IPV"
table(dfZmb$PV_IPV_lt, useNA = "always")

#------------------ Physical peer violence in a lifetime
vars_peer <- c("q116a", "q116b", "q116c")
for (var in vars_peer) {
  dfZmb[[paste0("ViolPeer_", var)]] <- ifelse(dfZmb[[var]] == 1, 1, ifelse(dfZmb[[var]] == 2, 0, NA))
}

dfZmb$PV_Peers_lt <- rowSums(dfZmb[, grep("^ViolPeer_", colnames(dfZmb))], na.rm = TRUE)
dfZmb$PV_Peers_lt <- ifelse(is.na(dfZmb$PV_Peers_lt), NA, ifelse(dfZmb$PV_Peers_lt >= 1, 1, 0))

attr(dfZmb$PV_Peers_lt, "label") <- "Lifetime peer physical violence"
table(dfZmb$PV_Peers_lt, useNA = "always")

#----------------- Physical family violence
vars_fam <- c("q128a", "q128b", "q128b")
for (var in vars_fam) {
  dfZmb[[paste0("ViolF_", var)]] <- ifelse(dfZmb[[var]] == 1, 1, ifelse(dfZmb[[var]] == 2, 0, NA))
}

dfZmb$PV_Fam_lt <- rowSums(dfZmb[, grep("^ViolF_", colnames(dfZmb))], na.rm = TRUE)
dfZmb$PV_Fam_lt <- ifelse(is.na(dfZmb$PV_Fam_lt), NA, ifelse(dfZmb$PV_Fam_lt >= 1, 1, 0))

attr(dfZmb$PV_Fam_lt, "label") <- "Lifetime family physical violence"
table(dfZmb$PV_Fam_lt, useNA = "always")

#---------------- Community Physical Violence (adults in neighbourhood)
vars_com <- c("q142a", "q142b", "q142c")
for (var in vars_com) {
  dfZmb[[paste0("ViolC_", var)]] <- ifelse(dfZmb[[var]] == 1, 1, ifelse(dfZmb[[var]] == 2, 0, NA))
}

dfZmb$PV_Com_lt <- rowSums(dfZmb[, grep("^ViolC_", colnames(dfZmb))], na.rm = TRUE)
dfZmb$PV_Com_lt <- ifelse(is.na(dfZmb$PV_Com_lt), NA, ifelse(dfZmb$PV_Com_lt >= 1, 1, 0))

attr(dfZmb$PV_Com_lt, "label") <- "Lifetime community physical violence"
table(dfZmb$PV_Com_lt, useNA = "always")

#-------------- Any physical violence in a lifetime:
dfZmb$Any_PV_lt <- rowSums(dfZmb[,c("PV_IPV_lt", "PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfZmb$Any_PV_lt <- ifelse(is.na(dfZmb$Any_PV_lt), NA,
                          ifelse(dfZmb$Any_PV_lt >=1, 1, 0))

attr(dfZmb$Any_PV_lt, "label") <- "Any lifetime Physical violence"
table(dfZmb$Any_PV_lt, useNA = "always")

####  Create JUST child physical violence = NON-PARTNER physical violence: ####
# Child physical violence in a lifetime:
dfZmb$child_PV_lt <- rowSums(dfZmb[,c("PV_Peers_lt", "PV_Fam_lt", "PV_Com_lt")], na.rm = TRUE)
dfZmb$child_PV_lt <- ifelse(is.na(dfZmb$child_PV_lt), NA,
                            ifelse(dfZmb$child_PV_lt >=1, 1, 0))

attr(dfZmb$child_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfZmb$child_PV_lt, useNA = "always")

####.  Create JUST community physical violence variable: ####
# Child physical violence in a lifetime:
dfZmb$community_PV_lt <- rowSums(dfZmb[,c("PV_Peers_lt", "PV_Com_lt")], na.rm = TRUE)
dfZmb$community_PV_lt <- ifelse(is.na(dfZmb$community_PV_lt), NA,
                                ifelse(dfZmb$community_PV_lt >=1, 1, 0))

attr(dfZmb$community_PV_lt, "label") <- "Any child lifetime Physical violence"
table(dfZmb$community_PV_lt, useNA = "always")

#------------ Physical violence variables IN THE LAST 12 MONTHS:-----------

# Physical IPV in the last 12 months
dfZmb$PV_IPV_12m <- NA  
dfZmb$PV_IPV_12m[dfZmb$PV_IPV_lt == 0 | 
                   (dfZmb$PV_IPV_lt == 1 & 
                      (dfZmb$q101 >= 1 & 
                         (dfZmb$q102 == 2 | dfZmb$q109 == 2)))] <- 0 
dfZmb$PV_IPV_12m[dfZmb$PV_IPV_lt == 1 & 
                   (dfZmb$q101 >= 1 & 
                      (dfZmb$q102 == 1 | dfZmb$q109 == 1))] <- 1  

attr(dfZmb$PV_IPV_12m, "label") <- "Physical IPV in the past 12 months"

# Display a summary table
table(dfZmb$PV_IPV_12m, useNA = "always")

# Physical peer violence in the last 12 months:
dfZmb$PV_Peers_12m <- NA  
dfZmb$PV_Peers_12m[dfZmb$PV_Peers_lt == 0 | 
                     (dfZmb$PV_Peers_lt == 1 & 
                        (dfZmb$q117 >= 1 & 
                           (dfZmb$q118 == 2 | dfZmb$q119 == 2)))] <- 0  # Assign 0 for no recent physical violence
dfZmb$PV_Peers_12m[dfZmb$PV_Peers_lt == 1 & 
                     (dfZmb$q117 >= 1 & 
                        (dfZmb$q118 == 1 | dfZmb$q119 == 1))] <- 1  # Assign 1 for recent physical violence

# Add a label to the variable
attr(dfZmb$PV_Peers_12m, "label") <- "Peers physical violence in the past 12 months"

# Display a summary table
table(dfZmb$PV_Peers_12m, useNA = "always")

# Physical family violence in the last 12 months:
# Initialize the variable with NA
dfZmb$PV_Fam_12m <- NA
dfZmb$PV_Fam_12m[dfZmb$PV_Fam_lt == 0 | 
                   (dfZmb$PV_Fam_lt == 1 & 
                      (dfZmb$q129 >= 1 & 
                         (dfZmb$q130 == 2 | dfZmb$q136 == 2)))] <- 0
dfZmb$PV_Fam_12m[dfZmb$PV_Fam_lt == 1 & 
                   (dfZmb$q129 >= 1 & 
                      (dfZmb$q130 == 1 | dfZmb$q136 == 1))] <- 1
attr(dfZmb$PV_Fam_12m, "label") <- "Family physical violence in the past 12 months"
table(dfZmb$PV_Fam_12m, useNA = "always")

# Physical violence in the community in the last 12 months:
dfZmb$PV_Com_12m <- NA
dfZmb$PV_Com_12m[dfZmb$PV_Com_lt == 0 | 
                   (dfZmb$PV_Com_lt == 1 & 
                      (dfZmb$q143 >= 1 & 
                         (dfZmb$q144 == 2 | dfZmb$q149 == 2)))] <- 0
dfZmb$PV_Com_12m[dfZmb$PV_Com_lt == 1 & 
                   (dfZmb$q143 >= 1 & 
                      (dfZmb$q144 == 1 | dfZmb$q149 == 1))] <- 1
attr(dfZmb$PV_Com_12m, "label") <- "Physical violence in the community in the past 12 months"
table(dfZmb$PV_Com_12m, useNA = "always")

# # Any physical violence BY EVERYONE in the past 12 months:
dfZmb$PV_Any <- rowSums(dfZmb[, c("PV_IPV_12m", "PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfZmb$PV_Any <- ifelse(is.na(dfZmb$PV_Any), NA, ifelse(dfZmb$PV_Any >= 1, 1, 0))
table(dfZmb$PV_Any)

attr(dfZmb$PV_Any, "label") <- "Any physical violence in the past 12 months"
table(dfZmb$PV_Any, useNA = "always")

#### creating Child physical (NON-PARTNER) violence variable:
# Any child past-year physical violence in the last 12 months:
dfZmb$child_PV_12m <- rowSums(dfZmb[, c("PV_Peers_12m", "PV_Fam_12m", "PV_Com_12m")], na.rm = TRUE)
dfZmb$child_PV_12m <- ifelse(is.na(dfZmb$child_PV_12m), NA, ifelse(dfZmb$child_PV_12m >= 1, 1, 0))

attr(dfZmb$child_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfZmb$child_PV_12m, useNA = "always")

#### creating just community physical violence variable:
# community child past-year physical violence in the last 12 months:
dfZmb$community_PV_12m <- rowSums(dfZmb[, c("PV_Peers_12m", "PV_Com_12m")], na.rm = TRUE)
dfZmb$community_PV_12m <- ifelse(is.na(dfZmb$community_PV_12m), NA, ifelse(dfZmb$community_PV_12m >= 1, 1, 0))

attr(dfZmb$community_PV_12m, "label") <- "Any child physical violence in the past 12 months"
table(dfZmb$community_PV_12m, useNA = "always")

#----------------------Emotional violence in a lifetime (ONLY FAMILY VIOLENCE AVAILABLE)

# Any lifetime emotional violence by parent, adult caregiver, or other adult relative
vars <- c("q300a", "q300b", "q300c")
for (var in vars) {
  dfZmb[[paste0("EViolF_", var)]] <- ifelse(dfZmb[[var]] == 1, 1, ifelse(dfZmb[[var]] == 2, 0, NA))
}

dfZmb$EV_Fam_lt <- rowSums(dfZmb[, grep("^EViolF_", colnames(dfZmb))], na.rm = TRUE)
dfZmb$EV_Fam_lt <- ifelse(is.na(dfZmb$EV_Fam_lt), NA, ifelse(dfZmb$EV_Fam_lt >= 1, 1, 0))

attr(dfZmb$EV_Fam_lt, "label") <- "Any lifetime emotional violence by parent, adult caregiver, or other adult relative"
table(dfZmb$EV_Fam_lt, useNA = "always")

#----------------------Emotional violence in the last 12 months (ONLY FAMILY VIOLENCE AVAILABLE)

# Emotional violence by parent, adult caregiver, or other adult relative in the past 12 months

dfZmb$EV_Any_12m <- 0
dfZmb$EV_Any_12m[dfZmb$EV_Fam_lt == 0 | 
                   (dfZmb$EV_Fam_lt == 1 & 
                      (dfZmb$q302 == 2 | dfZmb$q306 == 2))] <- 0
dfZmb$EV_Any_12m[dfZmb$EV_Fam_lt == 1 & 
                   (dfZmb$q301 %in% c(1, 3) & 
                      (dfZmb$q302 == 1 | dfZmb$q306 == 1))] <- 1

dfZmb$EV_Any <- 0
dfZmb$EV_Any[dfZmb$EV_Fam_lt == 0 | 
               (dfZmb$EV_Fam_lt == 1 & 
                  (dfZmb$q302 == 2 | dfZmb$q306 == 2))] <- 0
dfZmb$EV_Any[dfZmb$EV_Fam_lt == 1 & 
               (dfZmb$q301 %in% c(1, 3) & 
                  (dfZmb$q302 == 1 | dfZmb$q306 == 1))] <- 1

#selecting the key variables of interest:
dfZmb$EV_IPV_12m <- NA
dfZmb1 <- dplyr::select(dfZmb,
                        Q2,
                        HHSex,
                        inf_housing,
                        strata,
                        cluster,
                        sampleweight,
                        HDATE_VF,
                        month,
                        year,
                        region,
                        country,
                        AgeGroup,
                        sex,
                        married,
                        food_insec,
                        TransSex,
                        rural_urban,
                        EducationEver,
                        School_enrol,
                        Orphan,
                        Any_SV_12m,
                        EV_Any_12m,
                        InconsCondom_12m,
                        sex_multiple_12m,
                        ADS5Y,
                        child_PV_12m,
                        community_PV_12m,
                        PV_Fam_12m,
                        PV_IPV_12m,
                        SV_IPV_12m,
                        EV_IPV_12m,
                        SV_Any,
                        EV_Any,
                        PV_Any)

#merge all the survey dataframes together
df_vacs <- rbind(dfLso1, dfNmb1, dfZmb1, dfZam_merged, dfMoz1)

#=========Third: working on the heat data ============:

#===========Working on the heat data: 

## Uploading the Heat data ------------------------------------------------
## 

#2 loading heat data
#here you can load the heat data I sent you: for now we'll call it heat_data
heat_data

#3 (if you're using admin level-1) make sure the admin-1 areas are spelled the same across
#the two datasets (heat and survey)

#4 also, all common identifier variables must be harmonised: country, region, month, year 
#rename admin-1 column to region  var in heatwave data:
heat_data <- heat_data %>%
  rename(country = Country,
         month = Month,
         region = Admin_1,
         year = Year)

#Harmonise heatwave data region names with shapefiles and survey data region names:
#you will need to check if Eswatini and Kenya need harmonisation as well

sort(unique(heat_data$region))
sort(unique(sfdf$region))

heat_data <- heat_data %>%
  mutate(region = case_when(
    region == "Mashonaland Central" ~ "Mash Central",
    region == "Matabeleland North" ~ "Mat North",
    region == "Mashonaland East" ~ "Mash East",
    region == "Mashonaland West" ~ "Mash West",
    region == "Matabeleland South" ~ "Mat South",
    region == "Butha Buthe" ~ "Botha-Botha",
    region == "Thaba Tseka" ~ "Thaba-Tseka",
    region == "Caprivi" ~ "caprivi",
    region == "Erongo" ~ "erongo",
    region == "Hardap" ~ "hardap",
    region == "Karas" ~ "karas",
    region == "Kavango" ~ "kavango",
    region == "Khomas" ~ "khomas",
    region == "Kunene" ~ "kunene",
    region == "Ohangwena" ~ "ohangwena",
    region == "Omaheke" ~ "omaheke",
    region == "Omusati" ~ "omusati",
    region == "Oshana" ~ "oshana",
    region == "Oshikoto" ~ "oshikoto",
    region == "Otjozondjupa" ~ "otjozondjupa",
    region == "North-Western" ~ "North Western",
    region == "Lago niassa" ~ "Niassa",
    TRUE ~ region  # Keeps other values unchanged
  ))

#5 to link the heatdata with survey data we need spatial boundaries called Shapefiles
# These Shapefiles need to be in both: the survey data + heat data so they can be linked together

#6 We need to include Shapefiles in the heat dataset (we already harmonised the columns and observations above)

heat_sfdf_df <- heat_data %>%
  left_join(sfdf, by =c("region","country"))

# remove regions with 0 observations in either datasets
heat_sfdf_df <- heat_sfdf_df %>%
  dplyr::filter(!region %in% c("Sofala", "Inhambane", "Maputo City", "Muchinga"))

#reduce the size of the heat_sfdf_df by including the needed columns only
heat_sfdf_df <- heat_sfdf_df %>%
  dplyr::select(Heat, month, year, region, country, geometry)

#remove the duplicates in the heat_sfdf_final 
heat_sfdf_final <- heat_sfdf_df %>%
  distinct(country, region, year, month, .keep_all = TRUE)

# examine duplicates in X data heat_sfdf_final
heat_sfdf_final %>% count(country, region, year, month) %>% filter(n > 1)

#8 Now the survey datasets are ready to be joined with spatial data (heat data + shapefiles)
df_heat_vacs <- df_vacs %>% 
  left_join(heat_sfdf_final, by = join_by("country","region", "year", "month"))

#---------------STEP 2: PREPARING HEAT DATA INDICATORS -----------------------------------

#creating heat indicators:
library(dplyr)
library(zoo)

#convert Heat variable to numeric
df_heat_vacs$Heat <- as.numeric(df_heat_vacs$Heat)

#create heat indicators:
df_heat_vacs <- df_heat_vacs %>% 
  filter(year > 2013) %>%
  group_by(country, region) %>%
  arrange(country, region, year, month) %>% 
  mutate(
    heatwave_mean = roll_meanr(Heat, n = 12, fill = NA, na.rm = TRUE), # 12-month rolling average of heat 
                                                                      #this step provides the rolling averages of heat
                                                                      #observations for each 12 months before the interview date
    Heat_binary = ifelse(heatwave_mean >= 1, 1, 0), # a whole region experiencing a heatwave
    Heatwave_sum_m12 = roll_sumr(Heat_binary, n = 12, fill = NA, na.rm = TRUE), # Sum of months with heatwave > 0.5
    heatwave_frequency_m12 = roll_sumr(ifelse(Heat >= 0.5, 1, 0), n = 12, fill = NA, na.rm = TRUE)
  )
table(merged_df_south_siya$heatwave_mean)

#creating a heat severity variable using heatwave_mean var
df_heat_vacs <- df_heat_vacs %>% 
  mutate(heatwave_severity = cut(
    heatwave_mean,  # Assuming heatwave_mean is already in merged_df_south
    breaks = c(-Inf, 0, 1 , Inf),  # Define the intervals
    labels = c("No Heatwave" ,"Mild and Moderate heatwave", "Severe heatwave"),  # Assign labels to intervals
    right = TRUE  # Exclude the upper limit of each interval
  ))

#explore the presence of heatwave in each country      
heat_eachcountry <- merged_df_south_all %>%
  group_by(country) %>%
  count(heatwave_severity)
heat_eachcountry


#running the regression analyses:
# NON-WEIGHTED BASELINE regression MODELS HEAT AND VIOLENCE by anyone and partners

#The same models can be used for IPV variables e.g (SV_IPV_12m) + violence by anyone variables separately e.g (Any_SV_12m)

#---------------# Heat and violence models

model_all_1 <- feglm(SV_Any ~ heatwave_mean + AgeGroup + sex + married + rural_urban + HHSex + inf_housing
                     + EducationEver | as.factor(country) ,
                     se = "cluster", cluster = c("cluster"), 
                     data = df_heat_vacs, family = quasibinomial())
summary(model_all_1)

model_all_2 <- feglm(EV_Any ~ heatwave_mean+ AgeGroup+ sex +married+rural_urban + HHSex + inf_housing
                     + EducationEver |  as.factor(country),
                     se = "cluster", cluster = c("cluster"), 
                     data = df_heat_vacs, family = quasibinomial())
summary(model_all_2)

model_all_3 <- feglm(PV_Any ~ heatwave_mean + AgeGroup + sex + married + rural_urban + HHSex + inf_housing
                     + EducationEver | as.factor(country),
                     se = "cluster", cluster = c("cluster"), 
                     data = df_heat_vacs, family = quasibinomial())
summary(model_all_3)

#EXPORT MODEL RESULTS:
models_all <- list( model_all_1,model_all_2, model_all_3)
msummary(models_all,
         estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
         exp = TRUE, p.values = TRUE)


#======== End of masterfile =======#

