# Data Wrangling - Protection Programs.

library(tidyverse)
library(haven)
library(readxl)
library(lubridate)
library(plm)
library(stargazer)
library(modelsummary)
library(lmtest)
library(naniar)
library(skimr)

ppp <- read_xlsx("Protection Programs Data.xlsx")

pp <- ppp %>% 
  select(Data_Entrevista, provincia, P_INAS14, P_INAS014_Valor) %>% 
  rename(Date = `Data_Entrevista`,
         Province = `provincia`, 
         PASD = `P_INAS14`,
         PASD_Value = `P_INAS014_Valor`) %>% 
  filter(PASD == 1) %>% 
  mutate(Date = lubridate::ymd_hms(Date),
         Year = lubridate::year(Date))   

view(pp1)

pp1 <- pp %>% 
  select(Year, Province, PASD, PASD_Value)

pp2 <-  pp %>% 
  select(Year, Province, PASD, PASD_Value) %>% 
  group_by(Province) %>% 
  summarise(
    PASD_Count = n(),
    PASD_Value = mean(PASD_Value),
    .groups = "drop"
  )

view(pp2)

duplicated(pp2)

write.csv(pp1,"Protection_final.csv")

pp$Date <- as.Date(pp$Date)
pp$Date <- format(pp$Date, "%d/%m/%Y")
  



#######################################################################

# Data wrangling - flood data

flood <- read.csv("flood_data.csv")

view(flood)

fd <- flood %>% 
  select(NAME_1, NAME_2, BeginDate, 
         NumberOfFatalities, NumberOfDisplaced,
         MainCause,Severity, 
         FloodImpactIndex, Duration) %>% 
  
  rename(Province = `NAME_1`,
         District = `NAME_2`, 
         Date = `BeginDate`) %>% 
  
  # Extracting Year component from Date
  
  mutate(Date = lubridate::mdy(Date),
         Year = lubridate::year(Date))%>% 
  
  filter(Year %in% c(1996:2023)) %>% 
  
  group_by(Province, District, Year) %>% 
  summarise(
    Severity = sum(Severity, na.rm = TRUE),
    Fatalities = sum(NumberOfFatalities, na.rm = TRUE),
    Displaced = sum(NumberOfDisplaced, na.rm = TRUE),
    Duration = sum(Duration, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(Year)

fd <- fd %>% 
  mutate(Log_Displaced = log1p(Displaced))

view(fd)

colSums(is.na(fd))

########################################################################

# Data Wrangling for conflicts.

conf <- read_xlsx("conflict_data.xlsx")

view(conf)

cf <- conf %>% 
  select(event_date, event_type, admin1, admin2) %>% 
  rename(District = `admin2`,  Date = `event_date`, 
         Event_type = `event_type`,
         Province = `admin1`
         ) %>% 
  
  # Extracting Year component from the Date
  mutate(Year = year(Date)) %>% 
  
  # Conflict = Riots + Violence against Civilians + Battles
  
  filter(
    Event_type %in% c("Riots", "Violence against civilians", "Battles"),
    Year %in% c(1997:2025)
    ) %>% 
  
  # Aggregating to District-Year
  
  group_by(District, Year) %>% 
  summarise(
    Riots = sum(Event_type == "Riots"),
    Violence = sum(Event_type == "Violence against civilians"),
    Battles = sum(Event_type == "Battles"),
    Conflict = sum(Event_type %in% c("Riots", "Violence against civilians", "Battles"),
                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  arrange(Year)

# cf$Date <- as.Date(cf$Date, format = "%Y-%m-%d")
# cf$Date <- format(cf$Date, "%d/%m/%Y")

view(cf)

#########################################################################

# Merging Conflict and flood data by Year and District.

p.data <- fd %>% 
  left_join(cf, by = c("District", "Year")) %>% 
  
  # Filling NAs with zeros (0)
  
  mutate(
    Conflict = replace_na(Conflict, 0),
    Riots = replace_na(Riots, 0),
    Violence = replace_na(Violence, 0),
    Battles = replace_na(Battles, 0)
  ) %>% 
  
  # Removing Cabo Delgado Province due to insurgency (Confounder) &
  # filtering years above 1997 since ACLED started to record conflict events
  
  filter( Province!= "Cabo Delgado" & Year >= 1997)
  # filter(Year >= 1997)

view(p.data)

  # Creating a unique ID per district to remove duplicates
  # as some district names appear the same for different Provinces. 
  

panel <- p.data %>%
  mutate(
    Province = recode(Province,                          
                      "Maputo City" = "Cidade de Maputo",
                      "Nassa" = "Niassa"),
    District_Id = paste(Province, District, sep = "_")  
  )

view(panel)

any(duplicated(panel[,c("District_Id", "Year")]))

write.csv(panel, "flood_and_conf.csv")

panel$Province
summary(panel)
str(panel)
panel$Year

  # Declaring a panel model

paneld <- pdata.frame(panel, index = c("District_Id", "Year"))

view(paneld)

summary(paneld)

is.data.frame(paneld)
is.pbalanced(paneld)

 # Creating regional dummies, with Cabo Delgado included.

panel <- panel %>% 
  mutate(Region = case_when(
    Province %in% c("Niassa", "Nampula", "Cabo Delgado") ~ "North",
    Province %in% c("Zambezia", "Tete", "Manica", "Sofala") ~ "Centre",
    Province %in% c("Inhambane", "Gaza", "Maputo", "Cidade de Maputo") ~ "South"
  ),
  Region = case_when(
    Region == "North" ~ 1,
    Region == "Centre"~ 2,              # Creating regional dummies
    Region == "South" ~ 3
  ))


panel1 <- panel %>%       
  mutate(
    Year         = as.integer(as.character(Year)),
    Province     = as.factor(Province),
    District     = as.factor(District),
    District_Id  = as.character(District_Id),
    Region       = as.factor(Region),
    Conflict     = as.integer(Conflict),
    Riots        = as.integer(Riots),
    Violence     = as.integer(Violence),
    Battles      = as.integer(Battles),
    Fatalities   = as.integer(Fatalities),
    Duration     = as.integer(Duration)
  )

panel2 <- pdata.frame(panel1, index = c("District_Id", "Year"))

skim(panel1)  # check on panel1 not panel2

view(panel1)
view(panel2)

write.csv(panel1, "final_panel.csv")        


 








 






 
 
















