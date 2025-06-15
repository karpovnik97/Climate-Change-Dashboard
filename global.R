
# Required libraries
library(tidyverse)
library(plotly)
library(readxl)
library(readr)
library(httr) 
library(httr2)
library(DT)
library(viridis)



### Species_USA ####
print("Species USA")

Species_USA <- read_csv("https://ecos.fws.gov/ecp/pullreports/catalog/species/report/species/export?format=csv&columns=%2Fspecies%40sn%2Cstatus%2Clisting_date%2Cgn%3B%2Fspecies%2Fcurrent_range_county%40state_name&sort=%2Fspecies%40sn%20asc", 
                        col_types = cols(`Scientific Name_url` = col_skip()))

#Formating Species_USA data 

# Testing for missing values
# anyNA(Species_USA)
# Species_USA[complete.cases(Species_USA) == FALSE, ]
# colSums(is.na(Species_USA[colSums(is.na(Species_USA)) > 0]))

# Formatting ESA Listing Status column
Species_USA$`ESA Listing Status` <- factor(Species_USA$`ESA Listing Status`, 
                                           levels = c("Endangered","Threatened","Species of Concern",
                                                      "Proposed Endangered","Proposed Threatened",
                                                      "Resolved Taxon","Under Review","Candidate",
                                                      "Status Undefined","Recovery","Not Listed"), 
                                           ordered = TRUE)

# Formatting ESA Listing Date column to Year only
Species_USA$`ESA Listing Date` <- as.Date(Species_USA$`ESA Listing Date`, "%m-%d-%Y")
Species_USA$`ESA Listing Date` <- format(Species_USA$`ESA Listing Date`, "%Y")
Species_USA$`ESA Listing Date` <- as.numeric(Species_USA$`ESA Listing Date`)

# Replacing NAs with more meaningful values
Species_USA$`ESA Listing Status` <- if_else(is.na(Species_USA$`ESA Listing Status`), "Else", Species_USA$`ESA Listing Status`)
Species_USA$`State Name` <- if_else(is.na(Species_USA$`State Name`), "State absent", Species_USA$`State Name`)
Species_USA$`Species Group` <- as.factor(Species_USA$`Species Group`)

# Sorting (optional)
Choices_USA <- c("Endangered","Threatened","Species of Concern",
                 "Resolved Taxon","Under Review","Candidate",
                 "Status Undefined","Recovery","Not Listed")

# Visualization: Bar plot of species by ESA Listing Status

# ggplotly(
#   ggplot(Species_USA) +
#     aes(y = `ESA Listing Status`) +
#     geom_bar()
# )
# 
# 




### Sea Levels #### 
print("Sea levels")

fetch_page <- function(offset) {
  req <- request("https://services9.arcgis.com/weJ1QsnbMYJlCHdG/arcgis/rest/services/Indicator_3_3_melted_new/FeatureServer/0/query") |>
    req_url_query(
      where = "1=1",
      outFields = "Measure,Date,Value,ISO2",
      outSR = "4326",
      f = "json",
      resultRecordCount = 2000,
      resultOffset = offset
    )
  resp <- req_perform(req)
  result <- resp_body_json(resp)
  result$features
}

# Get all pages
offsets <- seq(0, 42000, by = 2000)
all_results <- map(offsets, fetch_page) |> flatten()

# Manual field extraction
Measure <- lapply(all_results, \(x) x$attributes$Measure) |> unlist()
Date    <- lapply(all_results, \(x) x$attributes$Date)    |> unlist()
Value   <- lapply(all_results, \(x) x$attributes$Value)   |> unlist() |> as.numeric()

Sea_Levels <- tibble(Measure, Date, Value)


# Formatting dataframe columns 
Sea_Levels$Date <- sub("^D", "", Sea_Levels$Date)
Sea_Levels$Date <- as.Date(Sea_Levels$Date,format="%m/%d/%Y")
Year <- format(Sea_Levels$Date, "%Y")
Sea_Levels <- cbind(Sea_Levels,Year)

# Cehcking for NAs 

anyNA(Sea_Levels)

# Adding manually calculated world average 

Sea_Levels <- bind_rows(
  Sea_Levels,
  Sea_Levels %>%
    group_by(Date) %>%
    reframe(
      Measure = "World2",
      Value = mean(Value),
      Year = Year
    ) %>%
    relocate(Measure)
)

# Trend in mean sea levels in different regions


# How is global sea levels changing? 

# ggplot(Sea_Levels %>% filter(Measure %in% c("World","World2")))+
#   aes(y=Value,x=Date,colour=Measure)+
#   geom_line(linewidth=1.5)

# Trend apprears to be seasonal
# last observation indicates that sea levels are stable at 0 which is off the 
# seasonal trend while world trend calculated by me does not show such extreme
# Observation

# ggplot(Sea_Levels %>% filter(Measure %in% c("World","World2"),Year>2015))+
#   aes(y=Value,x=Date,colour=Measure)+
#   geom_smooth(linewidth=1.5)

# smoothed line gives more insight and indicates increasing global sea levels trend





### Forest Cover ####  
print("Forest Cover")

# Units - 1000 HA, Index, Million tonnes, Percent
Forest_Area <- read_csv("https://opendata.arcgis.com/datasets/66dad9817da847b385d3b2323ce1be57_0.csv", col_types = cols(
  ObjectId = col_skip(), Unit = col_skip(), ISO3=col_skip(), ISO2 = col_skip(), 
  Source = col_skip(),CTS_Code = col_skip(), CTS_Name = col_skip(),CTS_Full_Descriptor = col_skip()))

# Fixing column names
colnames(Forest_Area) <- c("Country", "Indicator", as.character(1992:2022))
Forest_Area <- Forest_Area %>% pivot_longer(c(`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`,`2022`),names_to="Year",values_to="Value")

# Formatting dataframe columns
Forest_Area <- Forest_Area %>% filter(Indicator=="Forest area"|Indicator=="Share of forest area")
Forest_Area$Value <- as.integer(Forest_Area$Value)
Forest_Area$Year <- as.integer(Forest_Area$Year)
Forest_Area$Indicator <- as.factor(Forest_Area$Indicator)

# Exploring and testing data, Forest area trend in countries 

# ggplot(Forest_Area %>% filter(Indicator=="Forest area",Country %in% c("Azerbaijan, Rep. of","Georgia")))+
#   aes(x=Year,y=Value/10000,col=Country,group=Country)+
#   geom_point()+
#   geom_line()

# For example area that forest takes is increasing in Azerbaijan while its stable in Georgia




### Natural Disasters ####
print("Natural Disasters")


library(tidyverse)
library(httr2)

# Natural Disaster Frequency

req <- request(
  "https://services9.arcgis.com/weJ1QsnbMYJlCHdG/arcgis/rest/services/Indicator_11_1_Physical_Risks_Climate_related_disasters_frequency/FeatureServer/0/query?where=1%3D1&outFields=Country,Indicator,F1980,F1981,F1982,F1983,F1984,F1985,F1986,F1987,F1988,F1989,F1990,F1991,F1992,F1993,F1994,F1995,F1996,F1997,F1998,F1999,F2000,F2001,F2002,F2003,F2004,F2005,F2006,F2007,F2008,F2009,F2010,F2011,F2012,F2013,F2014,F2015,F2016,F2017,F2018,F2019,F2020,F2021,F2022,F2023,F2024&outSR=4326&f=json")
req_dry_run(req)
response <- req_perform(req)
resp_status(response)
resp_status_desc(response)

result <- resp_body_json(response)

features <- result[[5]]

attrs_list <- lapply(features, `[[`, "attributes")

all_names <- unique(unlist(lapply(attrs_list, names)))

attrs_filled <- lapply(attrs_list, function(x) {
  x <- lapply(x, function(v) if (is.null(v)) NA else v)
  missing <- setdiff(all_names, names(x))
  if (length(missing)) x[missing] <- NA
  x[all_names]
})

Disasters <- do.call(
  rbind,
  lapply(attrs_filled, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  })
)
rownames(Disasters) <- NULL

# Fixing column names

names(Disasters) <- gsub("^F", "", names(Disasters))

last_year <- as.integer(colnames(Disasters)[length(Disasters)])

Disasters <- Disasters %>%
  pivot_longer(
    cols = 3:ncol(Disasters),
    names_to = "Year",
    values_to = "Value"
  )

# Shortening indicators for convenience 

Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: Drought"] <- "Drought"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: Extreme temperature"] <- "Extreme temperature"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: Flood"] <- "Flood"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: Landslide"] <- "Landslide"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: Storm"] <- "Storm"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: TOTAL"] <- "Total"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of Disasters: Wildfire"] <- "Wildfire"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: Drought"] <- "Drought (People Affected)"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: Extreme temperature"] <- "Extreme temperature (People Affected)"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: Flood"] <- "Flood (People Affected)"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: Landslide"] <- "Landslide (People Affected)"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: Storm"] <- "Storm (People Affected)"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: TOTAL"] <- "Total (People Affected)"
Disasters$Indicator[Disasters$Indicator == "Climate related disasters frequency, Number of People Affected: Wildfire"] <- "Wildfire (People Affected)"

# Formatting dataframe columns dataframe columns
Disasters$Year <- as.numeric(Disasters$Year)
Disasters$Indicator <- as.factor(Disasters$Indicator)

# Exploring Data

DTable <- drop_na(Disasters) %>%
  group_by(Country) %>%
  filter(Indicator=="Total") %>%
  summarise(Frequency=sum(Value)) %>%
  arrange(desc(Frequency))


DTable2 <- drop_na(Disasters) %>%
  group_by(Indicator) %>%
  filter(Indicator!="Total") %>%
  summarise(Frequency=sum(Value)) %>%
  arrange(desc(Frequency))


# Comparing countries and testing fill argument

# ggplot(Disasters %>% filter(Country %in% c("United States","India","Philippines")))+
#   aes(x=Country,y=Value,fill=Indicator)+
#   geom_bar(stat="identity")





### Air pollution Data ####
print("Air Pollution")

temp_file <- tempfile(fileext = ".xls")  # temporary file
curl::curl_download("https://api.worldbank.org/v2/en/indicator/EN.ATM.PM25.MC.M3?downloadformat=excel", temp_file)
Air_Pollution <- read_excel(temp_file, skip = 2)

unlink(temp_file)

# removing unneeded columns
Air_Pollution <- Air_Pollution[,-c(3,4)]

# Transforming dataframe
Air_Pollution <- Air_Pollution %>%
  pivot_longer(
    cols = 3:ncol(Air_Pollution), 
    names_to = "Year",
    values_to = "Value"
  )

# Fixing column names
colnames(Air_Pollution) <- c("Country","Country Code","Year","Value")

# Formatting
Air_Pollution$Year <- as.numeric(Air_Pollution$Year)
#str(Air_Pollution)


# Calculating mean and median that will be used for plotting
Meadina_Air <- na.omit(Air_Pollution) %>% group_by(Year) %>% summarise(Median=median(Value))
Mean_Air <- na.omit(Air_Pollution) %>% group_by(Year) %>% summarise(Mean=mean(Value))

# Testing and Exploring data

# Determining countries with worst average pollution, data will be used in server funtion for plotting 

# Country with highest average pollution 
# 
# AP_First <- 
#   na.omit(Air_Pollution) %>%
#   group_by(Country) %>% 
#   summarise(Mean=mean(Value)) %>%
#   arrange(desc(Mean)) %>%
#   select(Country) %>%
#   head(1)

# Country with lowest average pollution 

# AP_Lowest <- 
#   na.omit(Air_Pollution) %>%
#   group_by(Country) %>% 
#   summarise(Mean=mean(Value)) %>%
#   arrange(desc(Mean)) %>%
#   select(Country) %>%
#   tail(1)

# Countries with high pollution

# ggplotly(ggplot(Air_Pollution %>% filter(Value>70))+
#            aes(y=Value,x=Year,fill=Country,col=Country)+
#            geom_line())




### Temperature_Change ####
print("Temperature Change")

Temperature_Change <- read_csv("https://opendata.arcgis.com/datasets/4063314923d74187be9596f10d034914_0.csv",col_types = cols(
  ObjectId = col_skip(),ISO3 = col_skip(), Source = col_skip(), `CTS Code` =col_skip(),
  ISO2 = col_skip(), Indicator = col_skip(),Unit = col_skip(), `CTS Name` = col_skip(),
  `CTS Full Descriptor` = col_skip()))

Temperature_Change <- tibble(Temperature_Change)

# Fixing column names
last_year <- as.integer(colnames(Temperature_Change)[length(Temperature_Change)])
first_year <- as.integer(colnames(Temperature_Change)[2])

colnames(Temperature_Change) <- c("Country", as.character(first_year:last_year))


# transforming dataset

Temperature_Change <- Temperature_Change %>%
  pivot_longer(
    cols = 2:ncol(Temperature_Change), 
    names_to = "Year",
    values_to = "Value"
  )


# Formatting dataframe columns
Temperature_Change$Year <- as.numeric(Temperature_Change$Year)

# Exploring 

# is average global temperature increasing?

# ggplot(Temperature_Change %>% filter(Country=="World"))+
#   aes(x=Year,y=Value)+
#   geom_area(fill="red",col="black")

# it is

# ggplotly(
#   ggplot(Temperature_Change %>% filter(Country %in% c("Georgia","Albania")))+
#     aes(x=Year,y=Value,fill=Country)+
#     geom_area()+
#     theme_bw())

# temperature tends to increase in both Georgia and Albania





# n <- 1
# clist <- unique(Temperature_Change$Country)
# repeat{
# CClist <- clist[c(1:n)]
# print(
# ggplot(Temperature_Change %>% filter(Country %in% CClist) %>% na.omit())+
#  aes(x=Year,y=Value, group=Country,fill=Country,col=Country)+
# geom_smooth()+
# theme_classic())
# Sys.sleep(3.5)
# n <- n+1
# if (n>30) {break}
# }
# 




###  CO2 Concentrations ####
print("CO2 Concentrations")

CO2_Concentrations <- read_csv("https://opendata.arcgis.com/datasets/9c3764c0efcc4c71934ab3988f219e0e_0.csv", col_types = cols(
  Date = col_datetime(format = "%YM%m"),ISO2 = col_skip(), ObjectId = col_skip(),Indicator = col_skip(),
  Source = col_skip(),ISO3 =col_skip(),`CTS Code` = col_skip(), `CTS Name` = col_skip(), 
  `CTS Full Descriptor` = col_skip(), Country = col_skip(),Value = col_number()))


# Formatting data frame columns
CO2_Concentrations <- CO2_Concentrations %>% filter(Unit=="Parts Per Million")
CO2_Concentrations$Date <- as.Date(CO2_Concentrations$Date,format="%Y-%m-%d")
Year <- format(CO2_Concentrations$Date, "%Y")
CO2_Concentrations <- cbind(CO2_Concentrations,Year)


# removing error

# CO2_Concentrations <- CO2_Concentrations[791,]

#CO2_Concentrations$Date <- format(CO2_Concentrations$Date, "%Y")
#CO2_Concentrations$Date <- as.character(CO2_Concentrations$Date)

# Exploring

# what global trend? is CO2 level really increasing?

# ggplot(CO2_Concentrations)+
#   aes(x=Date,y=Value)+
#   geom_line()

# CO2 levels are increasing 




### CO2_Emissions ####
print("CO2 Emissions")

tempfile <- tempfile(fileext = ".xls")
curl::curl_download("https://api.worldbank.org/v2/en/indicator/EN.ATM.CO2E.PC?downloadformat=excel", tempfile)
CO2_Emissions <- read_excel(tempfile, skip = 3)

unlink(tempfile) # Remove temporal file'


# Cheking NAs and removing years/columns with no value

# is.na(CO2_Emissions)

colSums(is.na(CO2_Emissions[colSums(is.na(CO2_Emissions)) > 0]))

CO2_Emissions <- CO2_Emissions[,-c(3,4)] 

CO2_Emissions <- CO2_Emissions[,-c(2:32,64,65,66)] 

# transforming data frame 

CO2_Emissions <- CO2_Emissions %>%
  pivot_longer(
    cols = 3:ncol(CO2_Emissions), 
    names_to = "Year",
    values_to = "Value"
  )


# Formatting dataframe columns
CO2_Emissions$Year <- as.integer(CO2_Emissions$Year)

# Exploring

# Is CO2 emissions increasing over time in United States?

# ggplot(CO2_Emissions %>% filter(`Country Name`=="United States"))+
#   aes(x=Year,y=Value/1000)+
#   geom_bar(stat="identity")







### Threatened_Species in EU ####
print("Species EU")

# Downloading temporary zip file and exacting needed csv file 
csv_filepath <- tempfile(fileext = ".csv")
response <- GET("https://www.eea.europa.eu/data-and-maps/data/article-17-database-habitats-directive-92-43-eec-2/article-17-2020-dataset/article-17-2020-data-csv-format/at_download/file")
writeBin(content(response, "raw"), csv_filepath)
zip_files <- unzip(csv_filepath, list = TRUE)
csv_entry <- zip_files[grep("Article17_2020_data_pressures_threats.csv", zip_files$Name, fixed = TRUE), ]
unzip(csv_filepath, files = csv_entry$Name, exdir = tempdir())

Threatened_Species <- read_csv(file.path(tempdir(), csv_entry$Name), show_col_types = FALSE)

unlink(csv_filepath) # Remove temp file

# Remove columns 
Threatened_Species <- Threatened_Species[,-c(3,4,9)]

# Formatting dataframe columns 
#Threatened_Species$country <- as.factor(Threatened_Species$country)

# testing 
# unique(Threatened_Species$pressurecode)

# extract first letter of each pressure code, first letter identifies pressure code group
temp <- substr (Threatened_Species$pressurecode, 1, 1)

# Assign presurecodes their group title 
# N - Natural processes is the o

`Threat Group` <- case_when(
  temp == "A" ~ "Agriculture",
  temp == "B" ~ "Forestry",
  temp == "C" ~ "Extraction of resources",
  temp == "D" ~ "Energy production processes",
  temp == "E" ~ "Development & operation of transport systems",
  temp == "F" ~ "Using land for settlement",
  temp == "G" ~ "Extraction of biological resources",
  temp == "H" ~ "Military action",
  temp == "I" ~ "Alien and problematic species",
  temp == "J" ~ "Mixed source pollution",
  temp == "K" ~ "Human-induced changes in water",
  temp == "L" ~ "Natural processes",
  temp == "M" ~ "Natural processes",
  temp == "N" ~ "Climate change",
  temp == "X" ~ "Unknown"
)

Threatened_Species <- cbind(Threatened_Species,`Threat Group`)

Threatened_Species <- Threatened_Species %>% select(featuretype,country,`Threat Group`)

# Function that converts ISO2 county codes to Full country names (for convenience)

iso2_to_full <- function(iso2_names) {
  
  full_names <- case_when(
    iso2_names == "AT" ~ "Austria",   iso2_names == "BE" ~ "Belgium", iso2_names == "BG" ~ "Bulgaria",
    iso2_names == "HR" ~ "Croatia",   iso2_names == "CY" ~ "Cyprus",  iso2_names == "CZ" ~ "Czech Republic",
    iso2_names == "DK" ~ "Denmark",   iso2_names == "EE" ~ "Estonia", iso2_names == "FI" ~ "Finland",
    iso2_names == "FR" ~ "France",    iso2_names == "DE" ~ "Germany", iso2_names == "GR" ~ "Greece",
    iso2_names == "HU" ~ "Hungary",   iso2_names == "IE" ~ "Ireland", iso2_names == "SK" ~ "Slovakia",
    iso2_names == "IT" ~ "Italy",     iso2_names == "LV" ~ "Latvia",  iso2_names == "LT" ~ "Lithuania",
    iso2_names == "LU" ~ "Luxembourg",iso2_names == "MT" ~ "Malta",   iso2_names == "NL" ~ "Netherlands",
    iso2_names == "PL" ~ "Poland",    iso2_names == "PT" ~ "Portugal",iso2_names == "RO" ~ "Romania",
    iso2_names == "SI" ~ "Slovenia",  iso2_names == "ES" ~ "Spain",   iso2_names == "UK" ~ "United Kindgdom",
    iso2_names == "SE" ~ "Sweden", TRUE ~ NA_character_
  )
}

Threatened_Species <- mutate(Threatened_Species,`Country Name`=iso2_to_full(Threatened_Species$country))

# cheeking for NAs

# anyNA(Threatened_Species)
# Threatened_Species$country[complete.cases(Threatened_Species)==FALSE]

# Exploring

# What threatens species in EU?

# ggplot(Threatened_Species)+
#   aes(fill=`Threat Group`, y=featuretype)+
#   geom_bar()

# Mostly agriculture



print("End")

# Data for Valueboxes
# Number of values, observations and variables anaized
info1 <- count(Air_Pollution)+count(Species_USA)+count(Disasters)+count(Sea_Levels)+count(Temperature_Change)+count(Threatened_Species)+count(Forest_Area)+count(CO2_Concentrations)+count(CO2_Emissions)
info2 <- length(Air_Pollution)+length(Species_USA)+length(Disasters)+length(Sea_Levels)+length(Temperature_Change)+length(Threatened_Species)+length(Forest_Area)+length(CO2_Concentrations)+length(CO2_Emissions)
info3 <- length(Species_USA)*count(Species_USA)+length(Air_Pollution)*count(Air_Pollution)+length(Disasters)*count(Disasters)+length(Sea_Levels)*count(Sea_Levels)+length(Temperature_Change)*length(Temperature_Change)+length(Threatened_Species)*count(Threatened_Species)+length(Forest_Area)*count(Forest_Area)+count(CO2_Concentrations)*length(CO2_Concentrations)+count(CO2_Emissions)*length(CO2_Emissions)

#removing temporary data
rm(all_names, Measure,offsets, Date, Value,temp,Year,`Threat Group`,zip_files,csv_entry,csv_filepath,iso2_to_full,tempfile,temp_file,response)
