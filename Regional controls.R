
 # Regional controls:

install.packages("geodata")
options(timeout = 300)
install.packages("terra")
install.packages("exactextractr")
install.packages("kableExtra")

library(terra)
library(sf)
library(dplyr)
library(geodata)
library(exactextractr)
library(modelsummary)

base_path <- "C:/Users/hp/Desktop/moz/Popurbc"

base_path <- "D:/Popurbc"

list.files(base_path)

 # Load Mozambique shapefiles

Moz_districts <- st_read("gadm41_MOZ_2.shp")

names(Moz_districts)

 # Construct a variable Year with respect to Panel1's Years.

Year <- c(1998:2005, 2007:2010, 2014, 2016:2023)

base_path <- "D:/Popurbc"

# Check what base_path should be
list.files("D:/Popurbc")

# Check what's actually inside one folder
list.files("D:/Popurbc/2023AD_pop")

hyde_list <- list()

for (yr in Year) {
  cat("processing:", yr, "\n")
  
  folder <- file.path(base_path, paste0(yr, "AD_pop"))

 # File path for popd and urbc

 popd_file <- file.path(folder, paste0("popd_", yr, "AD.asc"))
 urbc_file <- file.path(folder, paste0("urbc_", yr, "AD.asc"))

 # Confirming if files are not missing
 
 if(!file.exists(popd_file) | !file.exists(urbc_file)){
   cat("Missing file for Year:", yr, "\n")
   next
 }
 
 # Loading rasters
 
 popd_rast <- rast(popd_file)
 urbc_rast <- rast(urbc_file)
 
 popd_rast <- project(popd_rast, crs(Moz_districts))
 urbc_rast <- project(urbc_rast, crs(Moz_districts))
 
 popd_vals <- exact_extract(popd_rast, Moz_districts, fun = "mean")
 urbc_vals <- exact_extract(urbc_rast, Moz_districts, fun = "mean")
 
 hyde_list[[as.character(yr)]] <- data.frame(
   District    = Moz_districts$NAME_2,
   Province    = Moz_districts$NAME_1,
   Year        = yr,
   Pop_density = popd_vals,
   Urban_pop   = urbc_vals
 )

}

 # Combine all years
hyde_panel <- bind_rows(hyde_list)

 #Check
head(hyde_panel)
dim(hyde_panel)
summary(hyde_panel)

 #Reconstruct District_Id to match my panel
hyde_panel <- hyde_panel %>%
  mutate(Province = recode(Province,
                           "Maputo City" = "Cidade de Maputo",
                           "Nassa" = "Niassa"),
    District_Id = paste(Province, District, sep = "_")
    )

# When running robustness exercise, the code below is neglected.

# hyde_panel <- hyde_panel %>% 
# filter(Province != "Cabo Delgado")

view(hyde_panel)

 # Merge to main panel
panel1 <- panel1 %>%
  left_join(hyde_panel, by = c("District_Id", "Year"))

 # Check merge
dim(panel1)
colSums(is.na(panel1))

view(panel1)

all(panel1$District.x == panel1$District.y, na.rm = TRUE)

all(panel1$Province.x == panel1$Province.y, na.rm = TRUE)

 # Since both returned TRUE, I discard one

panel1 <- panel1 %>% 
  rename(
    District = District.x,
    Province = Province.x
  ) %>% 
  select(-District.y, -Province.y)

names(panel1)

view(panel1)

write.csv(panel1, "real_panel.csv")

 # Very nice.

 
 # Wrangling of nightlight data has been done using Python - google colab
 # saved in moz file as nightlight_panel



# Merging real_panel with nightlight_panel --------------------------------


nightlight <- read.csv("nightlight_panel.csv")

rpanel <- read.csv("real_panel.csv") 

nightlight <- nightlight %>% 
  mutate(
    Province = recode(Province,
                      "Maputo City" = "Cidade de Maputo",
                      "Nassa" = "Niassa"),
    District_Id = paste(Province, District, sep = "_")
  ) # %>% 
   
  # Neglect the code below during robustness check
  
  # filter(Province != "Cabo Delgado")

 
view(nightlight) 
view(rpanel)

nightlight$Province
nightlight$Year 

  # Joining rpanel with nightlight

full_panel <- rpanel %>% 
  left_join(nightlight, by = c("District_Id", "Year"))

 # Checking whether Province, District (x,y) are the same

view(full_panel)

names(full_panel)

all(full_panel$District.x == full_panel$District.y, na.rm = TRUE)

all(full_panel$Province.x == full_panel$Province.y, na.rm = TRUE)

all(full_panel$Pop_density.x == full_panel$Pop_density.y, na.rm = TRUE)

all(full_panel$Urban_pop.x == full_panel$Urban_pop.y, na.rm = TRUE)

full_panel <- full_panel %>%  # I need this code for normal analysis
 rename(
    District = District.x,
    Province = Province.x
  ) %>% 
  select(-District.y, -Province.y) 

# full_panel <- full_panel %>% 
#  rename(
#    Urban_pop = Urban_pop.x,
#    Pop_density = Pop_density.x
#  ) %>% 
#  select(-District.y, -Province.y,
#         -Urban_pop.y, -Pop_density.y,
#         -District.x, -Province.x)      # Discarding .y District and Province
 
names(full_panel) 
  
view(full_panel)

write.csv(full_panel, "full_panel_cgado.csv")
 

# WELLDONE SO FAR ---------------------------------------------------------


# Creating a descriptive statistics table ---------------------------------

# OPTION 1

library(kableExtra)
library(dplyr)

# Build stats manually
stats_table <- full_panel %>%
  as.data.frame() %>%
  select(
    "Flood Severity"             = Severity,
    "log(Displaced+1)"           = Log_Displaced,
    "Displaced Persons"          = Displaced,
    "Conflict Events"            = Conflict,
    "Riots"                      = Riots,
    "Violence Against Civilians" = Violence,
    "Battles"                    = Battles,
    "Fatalities"                 = Fatalities,
    "Population density"         = Pop_density,
    "Urbanization"               = Urban_pop,
    "Nighttime light"            = Nightlight,
  ) %>%
  summarise(across(everything(), list(
    Mean = ~round(mean(., na.rm=TRUE), 2),
    SD   = ~round(sd(., na.rm=TRUE), 2),
    Min  = ~round(min(., na.rm=TRUE), 2),
    Max  = ~round(max(., na.rm=TRUE), 2)
  ))) %>%
  tidyr::pivot_longer(everything(),
                      names_to = c("Variable", ".value"),
                      names_pattern = "(.+)_(Mean|SD|Min|Max)"
  )

# Render table
stats_table %>%
  kable(
    format = "html",
    caption = "Table 1: Descriptive Statistics",
    col.names = c("Variable", "Mean", "Std. Dev.", "Min", "Max"),
    align = c("l", "r", "r", "r", "r"),
    digits = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#1B6B3A", color = "white") %>%
  footnote(
    general = "Note: 637 district-year observations. Cabo Delgado province excluded due to structurally distinct Islamist insurgency dynamics.",
    general_title = "",
    footnote_as_chunk = TRUE
  )


# OPTION 2

library(stargazer)

my_data2 %>%
  as.data.frame() %>%
  select(Severity, Displaced, 
         Conflict, Riots, Violence, Battles) %>%
  stargazer(
    type = "text",  # change to "latex" for thesis
    title = "Table 1: Descriptive Statistics",
    digits = 2,
    summary.stat = c("Mean", "SD", "Min", "Max", "N"),
    covariate.labels = c(
      "Flood Severity",
      "log(Displaced+1)",
      "Displaced Persons",
      "Conflict Events",
      "Riots",
      "Violence Against Civilians",
      "Battles",
      "Fatalities"
    ),
    notes = "Note: 580 district-year observations. Cabo Delgado excluded.",
    notes.align = "l"
  )

# OPTION 3

library(modelsummary)
library(kableExtra)

datasummary(
  Severity + Log_Displaced + Conflict + Riots + Violence + Battles + Fatalities + Displaced ~
    Mean + SD + Min + Max + N,
  data = full_panel,
  title = "Table 1: Descriptive Statistics",
  fmt = 2,  # 2 decimal places
  output = "kableExtra"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  add_header_above(c(" " = 1, "Summary Statistics" = 5)) %>%
  footnote(
    general = "Note: Statistics computed for 580 district-year observations. Cabo Delgado excluded.",
    general_title = ""
  )

# OPTION 4

library(kableExtra)
library(dplyr)

stats_table <- full_panel %>%
  as.data.frame() %>%
  select(
    "Flood Severity"             = Severity,
    "log(Displaced+1)"           = Log_Displaced,
    "Displaced Persons"          = Displaced,
    "Conflict Events"            = Conflict,
    "Riots"                      = Riots,
    "Violence Against Civilians" = Violence,
    "Battles"                    = Battles,
    "Fatalities"                 = Fatalities,
    "Population Density"         = Pop_density,
    "Urbanization count"         = Urban_pop,
    "Nightlight Intensity"       = Nightlight
  ) %>%
  summarise(across(everything(), list(
    Mean = ~round(mean(., na.rm=TRUE), 2),
    SD   = ~round(sd(., na.rm=TRUE), 2),
    Min  = ~round(min(., na.rm=TRUE), 2),
    Max  = ~round(max(., na.rm=TRUE), 2)
  ))) %>%
  tidyr::pivot_longer(
    everything(),
    names_to  = c("Variable", ".value"),
    names_pattern = "(.+)_(Mean|SD|Min|Max)"
  )

stats_table %>%
  kable(
    format    = "html",
    caption   = "Table 1: Descriptive Statistics",
    col.names = c("Variable", "Mean", "Std. Dev.", "Min", "Max"),
    align     = c("l", "r", "r", "r", "r"),
    digits    = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    font_size         = 13,
    position          = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white") %>%
  pack_rows("Flood Variables",    1, 3)  %>%
  pack_rows("Conflict Variables", 4, 8)  %>%
  pack_rows("Regional Controls",  9, 11) %>%
  footnote(
    general        = "Note: 580 district-year observations. Cabo Delgado province excluded due to structurally distinct Islamist insurgency dynamics.",
    general_title  = "",
    footnote_as_chunk = TRUE
  )
 

library(modelsummary)
library(kableExtra)

datasummary(
  Severity + Log_Displaced + Displaced + 
    Conflict + Riots + Violence + Battles + Fatalities +
    Pop_density + Urban_pop + Nightlight ~
    Mean + SD + Min + Max,
  data = full_panel,
  title = "Table 1: Descriptive Statistics",
  fmt = 2,
  output = "kableExtra"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  add_header_above(c(" " = 1, "Summary Statistics" = 4)) %>%
  pack_rows("Conflict Variables", 4, 8) %>%
  pack_rows("Flood Variables", 1, 3) %>%
  pack_rows("Regional Controls", 9, 11) %>%
  footnote(
    general = "Note: 637 district-year observations. Cabo Delgado excluded.",
    general_title = ""
  ) 
 
 


library(kableExtra)
library(dplyr)

# Build stats manually
my_data2 %>% as.data.frame()

calc_stats <- function(x) {
  c(
    Mean = round(mean(x, na.rm=TRUE), 2),
    SD   = round(sd(x, na.rm=TRUE), 2),
    Min  = round(min(x, na.rm=TRUE), 2),
    Max  = round(max(x, na.rm=TRUE), 2),
    N    = sum(!is.na(x))
  )
}

# Build each row manually
rows <- rbind(
  calc_stats(my_data2$Conflict),
  calc_stats(my_data2$Violence),
  calc_stats(my_data2$Riots),
  calc_stats(my_data2$Battles),
  calc_stats(my_data2$Severity),
  calc_stats(my_data2$Displaced),
  calc_stats(my_data2$Displaced_dummy),
  calc_stats(my_data2$Pop_density),
  calc_stats(my_data2$Urban_pop),
  calc_stats(my_data2$Nightlight)
)

# Variable labels
row_labels <- c(
  "Conflict events",
  "Violence against civilians",
  "Riots",
  "Battles",
  "Flood severity",
  "Displaced persons",
  "Displaced dummy",
  "Population density",
  "Urbanization",
  "Nightlight intensity"
)

# Combine into dataframe
stats_df <- data.frame(
  Variable = row_labels,
  as.data.frame(rows),
  row.names = NULL
)

# Render table
stats_df %>%
  kable(
    format    = "html",
    caption   = "Table 1: Descriptive statistics for District-Year Panel, Mozambique 1998–2023.",
    col.names = c("", "Mean", "Std. Dev.", "Min", "Max", "N"),
    align     = c("l", "r", "r", "r", "r", "r"),
    digits    = 2
  ) %>%
  kable_styling(
    full_width        = FALSE,
    font_size         = 11,
    bootstrap_options = "none"
  ) %>%
  pack_rows("Dependent variable",      1, 1,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Conflict disaggregated",  2, 4,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Flood variables (Independent variables)", 5, 7,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Regional controls",       8, 10, bold = FALSE, italic = TRUE) %>%
  row_spec(0.5, bold = TRUE) %>%
  footnote(
    general       = "Cabo Delgado province excluded due to  insurgency dynamics unrelated to flood exposure. The years 2006, 2011, 2012, 2013 and 2015 are excluded too",
    general_title = "Source: DFO, ACLED, HYDE 3.3, Chen et al. (2024).",
    footnote_as_chunk = FALSE
  ) 


library(kableExtra)

latex_table <- stats_df %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Descriptive statistics for District-Year Panel, Mozambique 1998--2023.",
    col.names = c("", "Mean", "Std. Dev.", "Min", "Max", "N"),
    align     = c("l", "r", "r", "r", "r", "r"),
    digits    = 2
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 11
  ) %>%
  pack_rows("Dependent variable",      1, 1,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Conflict disaggregated",  2, 4,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Flood variables (Independent variables)", 5, 8,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Regional controls",       9, 11, bold = FALSE, italic = TRUE) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general = "Cabo Delgado province excluded due to insurgency dynamics unrelated to flood exposure. The years 2006, 2011, 2012, 2013 and 2015 are excluded too",
    general_title     = "Source: DFO, ACLED, HYDE 3.3 and Chen et al. (2024).",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

# Print the LaTeX code to copy
cat(latex_table)
 



# DESCRIPTION OF VARIABLES ------------------------------------------------

library(kableExtra)

# Build variable description dataframe
var_table <- data.frame(
  Variable = c(
    # Dependent
    "Conflict",
    "Violence",
    "Riots", 
    "Battles",
    # Independent
    "Fatalities",
    "Severity",
    "Displaced",
    "log(Displaced+1)",
    # Regional controls
    "Pop\\_density",
    "Urban\\_pop",
    "Nightlight",
    # Administrative variables
    "District\\_Id",
    "Province",
    "Region"
  ),
  Type = c(
    "Count",
    "Count",
    "Count",
    "Count",
    "Count",
    "Ordinal",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Categorical",
    "Categorical",
    "Dummy"
  ),
  Description = c(
    "Count of conflict events per district-year (Violence, Riots and Battles)",
    "ACLED violence against civilian events per district-year",
    "ACLED riot events per district-year",
    "ACLED battle events between organized armed groups per district-year",
    "Total fatalities recorded per flood event per district-year",
    "DFO flood severity index (1.0--4.5); higher values indicate more severe floods",
    "Number of persons displaced by flood event per district-year",
    "Natural log transformation of displaced persons; log(Displaced + 1)",
    "Mean population density per km\\textsuperscript{2}",
    "Mean Urban population count per district-year",
    "Mean nightlight radiance per district-year (proxy for economic activity)",
    "Unique district identifier",
    "Administrative province (10 provinces); Cabo Delgado excluded",
    "Geographical region: 1 = North, 2 = Centre, 3 = South"
  )
)

# Generate LaTeX
latex_var_table <- var_table %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Variable Descriptions",
    col.names = c("Variable", "Type", "Description"),
    align     = c("l", "l", "p{9cm}"),
    label     = "tab:variables",
    escape    = FALSE
  ) %>%
  kable_styling(
    latex_options = "hold_position",
  ) %>%
  pack_rows("Dependent variable",      1, 1,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Conflict disaggregated",  2, 5,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Independent variables",   6, 8,  bold = FALSE, italic = TRUE) %>%
  pack_rows("Regional controls",       9, 11, bold = FALSE, italic = TRUE) %>%
  pack_rows("Panel structure",        12, 14, bold = FALSE, italic = TRUE) %>%
  footnote(
    general           = "DFO = Dartmouth Flood Observatory. ACLED = Armed Conflict Location and Event Data. HYDE = History Database of the Global Environment.",
    general_title     = "Sources:",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

cat(latex_var_table)


