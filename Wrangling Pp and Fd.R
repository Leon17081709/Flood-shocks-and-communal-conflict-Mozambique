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
  
 # filter( Province!= "Cabo Delgado" & Year >= 1997)
  filter(Year >= 1997)

view(p.data)

  # Creating a unique ID per district to remove duplicates
  # as some district names appear the same for different Provinces. 
  

panel <- p.data %>%
  mutate(
    Province = recode(Province,                          # recode FIRST
                      "Maputo City" = "Cidade de Maputo",
                      "Nassa" = "Niassa"),
    District_Id = paste(Province, District, sep = "_")  # then create ID with clean names
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


  # FIXED MODEL : ANSWER TO QUESTION NUMBER ONE.

fe.model <- plm(Conflict ~ Severity + Log_Displaced,
                model = "within",effect = "twoways", data = panel2)
summary(fe.model)

modelsummary(fe.model, stars=T, statistic = c("std.error", "p.value"))

  # Checking for Heteroscedasticity
resi <- fe.model$residuals
ggplot(data = paneld, aes(y = resi, x = Severity)) + geom_point(col = "blue") +
  geom_abline(slope = 0)

bptest(fe.model)
 # H0: Homoscedasticity
 # H1: Heteroscedasticity
 # Fail to reject the null: Homoscedasticity (Constant variance of the error term)


 # RANDOM MODEL
ra.model <- plm(Conflict ~ Severity + Log_Displaced,
                model = "random", effect = "twoways", data = panel1)
summary(ra.model)

modelsummary(list("FE Estimator" = fe.model, "RE Estimator" =ra.model), stars=T,
                  statistic = c("std.error", "p.value"))


 # Running the Hausman Test
 # H0: RE Estimator is consistent 
 # H1: FE Estimator is consistent

phtest(ra.model, fe.model)


 # Running Mundlak test (RE or FE ) Needs to be fixed.

paneld_mundlak <- panel1 %>%
  as.data.frame() %>%
  group_by(District_Id) %>%
  mutate(
    mean_severity = mean(Severity, na.rm = TRUE),
    mean_displaced = mean(log1p(Displaced), na.rm = TRUE)
  ) %>%
  ungroup()

paneld_mundlak <- pdata.frame(paneld_mundlak, index = c("District_Id", "Year"))

mundlak.model <- plm(
  Conflict ~ Severity + log1p(Displaced) + mean_severity + mean_displaced,
  data = paneld_mundlak,
  model = "random",
  effect = "individual"
)

summary(mundlak.model)

 # ANSWER TO QUESTION NUMBER TWO.
 # Understanding where exactly the flood-conflict nexus is strong
 
panel2 <- as.data.frame(panel1) 

view(panel2)

table(panel2$Region, useNA = "always")

head(panel2[, c("District_Id", "Year", "Region", "Conflict", "Severity")])

 # Regression in the North Region
model_north <- plm(Conflict ~ Severity + Log_Displaced,
                    data = filter(panel2, Region == 1),
                    effect = "individual",
                    model = "within",
                    index = c("District_Id","Year"))
summary(model_north)
 
 # Regression model for the Centre Region
model_centre <- plm(Conflict ~ Severity + Log_Displaced,
                     data = filter(panel2, Region == 2),
                     effect = "individual",
                     model = "within",
                     index = c("District_Id","Year"))
summary(model_centre)

 # Regression model for the South Region
model_south <- plm(Conflict ~ Severity + Log_Displaced,
                    data = filter(panel2, Region == 3),
                    effect = "individual",
                    model = "within",
                    index = c("District_Id", "Year"))
summary(model_south)

modelsummary(list("North" = model_north, "Centre" = model_centre,
                  "South" = model_south), stars=T,
             statistic = c("std.error", "p.value"))

write.csv(panel2, "conflict_flood.csv")


 # ANSWER TO THIRD RESEARCH QUESTION.














