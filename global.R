library(shiny)
library(htmlwidgets)
# library(pivottabler)
library(leaflet)
# library(RColorBrewer)
# library(scales)
# library(lattice)
# library(dplyr)
library(DT)
library(highcharter)
# library(rsconnect)
# library(shinythemes)
# library(rgdal)
# library(metricsgraphics)
# library(sf)
library(rlist)

# install.packages(c("RColorBrewer", "scales", "lattice", "dplyr", "DT", "rsconnect", "shinythemes", "rgdal", "metricsgraphics", "sf", "RSQLite"))

time_series <- readRDS(file = "./data/time_series.rds")

#pwd <- readRDS(file = "pwd_data.rds")

basic <- readRDS(file = "./basic_data_v2.rds")

methodology <- readRDS(file = "./data/methodology.rds")

# basic <- basic %>% distinct(school_id, .keep_all = TRUE)

# basic <- read.csv("./data/pwd_gender_basic.csv")

# saveRDS(basic, "./data/pwd_gender_basic.rds")

# basic <- readRDS("./data/pwd_gender_basic.rds")



# all_data <- read.csv("./data/this_one.csv")
# saveRDS(all_data, "./data/this_one.rds")
# all_data <- readRDS("./data/this_one.rds")


# all_data$latitude <- as.numeric(as.character(all_data$latitude))

# saveRDS(all_data, "./data/this_one.rds")
all_data <- readRDS("./data/this_one.rds")

all_data$numeric <- as.character(all_data$school_year)
all_data$region <- as.character(all_data$region)

all_data$original_electricity_boolean[all_data$original_electricity_boolean == 1] <- "Yes"
all_data$original_electricity_boolean[all_data$original_electricity_boolean == 0] <- "No"

all_data$original_water_boolean[all_data$original_water_boolean == 1] <- "Yes"
all_data$original_water_boolean[all_data$original_water_boolean == 0] <- "No"

all_data$original_internet_boolean[all_data$original_internet_boolean == 1] <- "Yes"
all_data$original_internet_boolean[all_data$original_internet_boolean == 0] <- "No"



# kMeans <- classInt::classIntervals(all_data$remoteness_index, 4, style = "kmeans", rtimes = 3)
all_data$remoteness_cluster <- NA
all_data$remoteness_cluster[all_data$remoteness_index >= -805 & all_data$remoteness_index < -400] <- 'Extremely Accessible'
all_data$remoteness_cluster[all_data$remoteness_index >= -400 & all_data$remoteness_index < 980] <- 'Accessible'
all_data$remoteness_cluster[all_data$remoteness_index >= 980 & all_data$remoteness_index < 5107] <- 'Remote'
all_data$remoteness_cluster[all_data$remoteness_index >= 5107 & all_data$remoteness_index < 28419] <- 'Extremely Remote'



# time_series$Latitude <-as.numeric(as.character(time_series$Latitude))

# Input Variables ---------------------------------------------------------
pwd_vars <- c(
  'Difficulty Seeing' = 'ds_total',
  'Cerebral Palsy' = 'cp_total',
  'Difficulty Communicating' = 'dcm_total',
  'Difficulty Remembering, Concentrating, Paying Attention and Understanding Based On Manifestation' = 'drcpau_total',
  'Special Health Problem' = 'shp_total',
  'Autism' = 'autism_total',
  'Difficulty Walking, Climbing and Grasping' = 'wcg_total',
  'Difficulty Hearing' = 'dh_total',
  'Emotional-Behavioral Disability' = 'eb_total',
  'Hearing Impairment' = 'hi_total')

vars <- c(
  "School Neediness Score" = 'shi_score',
  #'Pareto Frontier' = 'pareto_frontier',
  'Remoteness Index' = 'remoteness_index',
  #'Remoteness Cluster' = 'cluster',
  "Percentage of Students Recieving CCT's" = 'cct_percentage',
  'Student Teacher Ratio' = 'Student_Teacher_Ratio',
  'Student Classroom Ratio' = 'Student_Classroom_Ratio',
  'Student Teacher Ratio' = 'Student_Teacher_Ratio',
  'Access to Water' = 'Water_Access',
  'Access to Internet' = 'Internet_Access',
  'Access to Electricity' = 'Electricity_Access'
)

shp_vars <- c(
  "School Hardship Score" = 'shi_score',
  'Remoteness Index' = 'remoteness',
  "Percentage of Students Recieving CCT's" = 'cct_percentage',
  'Student Teacher Ratio' = 'Student_Teacher_Ratio',
  'Student Classroom Ratio' = 'Student_Classroom_Ratio',
  'Student Teacher Ratio' = 'Student_Teacher_Ratio',
  'Access to Water' = 'Water_Access',
  'Access to Internet' = 'Internet_Access',
  'Access to Electricity' = 'Electricity_Access')

profile_vars <- c(
  'School_Name_y',
  "School_ID",
  "Region_Name",
  "Division",
  "District",
  'shi_score',
  'remoteness_index',
  'cct_percentage',
  'Student_Teacher_Ratio',
  'Student_Classroom_Ratio',
  'Water_Access',
  'Internet_Access',
  'Electricity_Access')

area <- c("Region", "District", "Division")

size_vars <- c(4,5,6,7,8)




ts_clean <- c(
  "School_Year",
  "School_ID",
  "School_Name",
  "Region_Name",
  "Division_Name",
  "District_Name",
  "remoteness_index",
  "cct_percentage",
  "Original_Water_Boolean",
  "Original_Internet_Boolean",
  "Original_Electricity_Boolean",
  "Student_Classroom_Ratio",
  "Student_Teacher_Ratio",
  "Accessibility",
  "Amenities",
  "Conditions",
  "shi_score",
  "comments"
)

ts_clean <- time_series[ts_clean]

ts_clean <- setNames(ts_clean, c(
  "School Year",
  "School ID",
  "School Name",
  "Region Name",
  "Division Name",
  "District Name",
  "Remoteness Index",
  "CCT Percentage",
  "Original Water Boolean",
  "Original Internet Boolean",
  "Original Electricity Boolean",
  "Student Classroom Ratio",
  "Student Teacher Ratio",
  "Accessibility",
  "Amenities",
  "Conditions",
  "School Neediness Index Score",
  "Comments"
))




#basic <- basic %>% distinct(School_ID, keep_all = TRUE)

# ts_shp <- sf::read_sf("./data/shp/time_series.csv.shp")









## Percentile and Cluster Calculations
all_data$remoteness_cluster <- NA
all_data$remoteness_cluster[all_data$remoteness_index >= -805 & all_data$remoteness_index < -400] <- 'Extremely Accessible'
all_data$remoteness_cluster[all_data$remoteness_index >= -400 & all_data$remoteness_index < 980] <- 'Accessible'
all_data$remoteness_cluster[all_data$remoteness_index >= 980 & all_data$remoteness_index < 5107] <- 'Remote'
all_data$remoteness_cluster[all_data$remoteness_index >= 5107 & all_data$remoteness_index < 28419] <- 'Extremely Remote'


no_na <- all_data[all_data$shi_score != -999,]

quantile(no_na$shi_score, probs = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)) # quartile

all_data$remoteness_percentile_text <- NA
all_data$remoteness_percentile_numeric <- NA

all_data$remoteness_percentile_text[all_data$remoteness_index < -770.30] <- '10% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -770.30 & all_data$remoteness_index < -745.95] <- '20% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -745.95 & all_data$remoteness_index < -725.40] <- '30% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -725.40 & all_data$remoteness_index < -698.25] <- '40% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -698.25 & all_data$remoteness_index < -670.70] <- '50% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -670.70 & all_data$remoteness_index < -631.50] <- '60% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -631.50 & all_data$remoteness_index < -586.80] <- '70% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -586.80 & all_data$remoteness_index < -526.60] <- '80% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -526.60 & all_data$remoteness_index < -392.70] <- '90% Percentile'
all_data$remoteness_percentile_text[all_data$remoteness_index >= -392.70 & all_data$remoteness_index < 28419.00] <- '100% Percentile'


all_data$remoteness_percentile_numeric[all_data$remoteness_index < -770.30] <- .10
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -770.30 & all_data$remoteness_index < -745.95] <- .20
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -745.95 & all_data$remoteness_index < -725.40] <- .30
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -725.40 & all_data$remoteness_index < -698.25] <- .40
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -698.25 & all_data$remoteness_index < -670.70] <- .50
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -670.70 & all_data$remoteness_index < -631.50] <- .60
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -631.50 & all_data$remoteness_index < -586.80] <- .70
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -586.80 & all_data$remoteness_index < -526.60] <- .80
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -526.60 & all_data$remoteness_index < -392.70] <- .90
all_data$remoteness_percentile_numeric[all_data$remoteness_index >= -392.70 & all_data$remoteness_index < 28419.00] <- 1



all_data$sni_percentile_text <- NA
all_data$sni_percentile_numeric <- NA

all_data$sni_percentile_numeric[all_data$shi_score < 1.003583] <- .10
all_data$sni_percentile_numeric[all_data$shi_score >= 1.003583 & all_data$shi_score < 1.067679] <- .20
all_data$sni_percentile_numeric[all_data$shi_score >= 1.067679 & all_data$shi_score < 1.141679] <- .30
all_data$sni_percentile_numeric[all_data$shi_score >= 1.141679 & all_data$shi_score < 1.982536] <- .40
all_data$sni_percentile_numeric[all_data$shi_score >= 1.982536 & all_data$shi_score < 2.000917] <- .50
all_data$sni_percentile_numeric[all_data$shi_score >= 2.000917 & all_data$shi_score < 2.066655] <- .60
all_data$sni_percentile_numeric[all_data$shi_score >= 2.066655 & all_data$shi_score < 2.101114] <- .70
all_data$sni_percentile_numeric[all_data$shi_score >= 2.101114 & all_data$shi_score < 2.146512] <- .80
all_data$sni_percentile_numeric[all_data$shi_score >= 2.146512 & all_data$shi_score < 2.986154] <- .90
all_data$sni_percentile_numeric[all_data$shi_score >= 2.986154 & all_data$shi_score < 4.940997] <- 1

all_data$sni_percentile_text[all_data$shi_score < 1.003583] <- '10% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 1.003583 & all_data$shi_score < 1.067679] <- '20% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 1.067679 & all_data$shi_score < 1.141679] <- '30% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 1.141679 & all_data$shi_score < 1.982536] <- '40% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 1.982536 & all_data$shi_score < 2.000917] <- '50% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 2.000917 & all_data$shi_score < 2.066655] <- '60% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 2.066655 & all_data$shi_score < 2.101114] <- '70% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 2.101114 & all_data$shi_score < 2.146512] <- '80% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 2.146512 & all_data$shi_score < 2.986154] <- '90% Percentile'
all_data$sni_percentile_text[all_data$shi_score >= 2.986154 & all_data$shi_score < 4.940997] <- '100% Percentile'



