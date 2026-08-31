
# Descriptive Statistics for Mozambique  ----------------------------------
# Using panel1

install.packages("naniar")
install.packages("psych")
install.packages("skimr")
library(naniar)                     
library(ggplot2)
library(AER)
library(ggpubr)
library(patchwork)
library(moments)
library(psych)
library(skimr)
library(corrplot)

# Missing Variable
miss_var_summary(panel1)
vis_miss(panel1)

# Checking for Outliers

panel1 %>%
  as.data.frame() %>% 
  ggplot(aes(x = reorder(Province, Severity), y = Severity, fill = Province)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 18, outlier.size = 3) + 
  geom_jitter(alpha = 0.6) +
  labs(
    title = "Flood severity by Province",
    x = "Province",
    y = "Flood severity"
  ) + 
  theme(plot.title = element_text(
    hjust = 0.5, face = "plain"
  )) + theme(legend.position = "none") +
  coord_flip() + theme_minimal() + theme(legend.position = "none")

panel1 %>%
  as.data.frame() %>% 
  ggplot(aes(x = Region, y = log1p(Displaced), fill = factor(Region))) +
  geom_boxplot(outlier.colour = "red", width = 0.7, 
               outlier.shape = 18, outlier.size = 3) + 
  geom_jitter(alpha = 0.6) +
  coord_cartesian(ylim = c(0,NA)) +
  labs(
    title = "Log of Displaced by Region",
    x = "Region",
    y = "Log(Number of people displaced + 1)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  theme(legend.position = "none") + theme_minimal() 


# Correlation between conflict and severity

p1 <- panel1 %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Severity, y = Conflict)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(
    title = "Conflict vs. Flood Severity",
    x = "Flood severity",
    y = "Number of conflict events"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  facet_wrap(~factor(Region)) + theme_minimal()
  
p2 <- panel1 %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Severity, y = Conflict)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(
    title = "Conflict vs. Flood Severity",
    x = "Flood severity",
    y = "Number of conflict events"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "plain")) + theme_minimal()

p1
p2

(p1 | p2)

# b) Correlation between Conflict and Displaced

panel1 %>%
  as.data.frame() %>%
  ggplot(aes(x = log1p(Displaced), y = Conflict)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", col = "firebrick", se = F) +
  facet_wrap(~factor(Region)) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(
    title = "Relationship between log(Displaced) vs.conflict by Region",
    x = "log(Displaced + 1)",
    y = "Number of conflict events"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  theme_minimal()
  

panel1 %>%
  as.data.frame() %>%
  ggplot(aes(x = log1p(Displaced), y = Conflict)) +
  geom_point(alpha = 0.8, color= "blue") +
  geom_smooth(method = "lm", col = "firebrick", se = FALSE)+
  coord_cartesian(ylim = c(0, 8)) +
  labs(
    title = "log(Displaced) vs. Conflict Incidence by Region",
    x = "log(Displaced + 1)",
    y = "Conflict Events") + theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  theme_minimal()


# A Correlation Matrix

vars <- panel1 %>% 
  as.data.frame() %>% 
  select(Severity, Conflict, Log_Displaced, Year)

corr_matrix <- cor(vars, use = "complete.obs")

corrplot(
  corr_matrix, method = "color", type = "upper",
  addCoef.col = "black",
  tl.col = "black",        
  number.cex = 0.8,
  tl.srt = 45
  )
  

 # Distribution of the data (Density & Density)

df <- as.data.frame(panel1)

Plot1 <- ggplot(data = df, aes(x = Severity)) +
  geom_histogram(aes(y = after_stat(density)), fill = "steelblue", bins = 50, alpha = 0.6) +
  geom_density(color = "darkblue", linewidth = 1) +
  labs(
    title = "Distribution plot for Severity",
    x = "Flood severity",
    y = "Density"    # after_stat(density) tells R to use same scale for y-axis
  ) + theme_minimal()


Plot2 <- ggplot(data = df, aes(x = Displaced)) +
  geom_histogram(aes(y = after_stat(density)), fill = "coral", bins = 50, alpha = 0.6) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(
    title = "Distribution plot for Displaced",
    x = "Number of Displaced People",
    y = "Density"      # after_stat(density) tells R to use same scale for y-axis
  ) + theme_minimal()


Plot3 <- ggplot(data = df, aes(x = log1p(Displaced))) +
  geom_histogram(aes(y = after_stat(density)), fill = "coral", bins = 50, alpha = 0.6) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(
    title = "Distribution plot for Log of Displaced",
    x = "Log of Displaced People",
    y = "Density"     # after_stat(density) tells R to use same scale for y-axis
  ) + theme_minimal()

Plot4 <- ggplot(data = df, aes(x = Conflict)) +
  geom_histogram(aes(y = after_stat(density)), fill = "darkgreen", bins = 50, alpha = 0.6) +
  geom_density(color = "darkgreen", linewidth = 1) +
  labs(
    title = "Distribution plot for Conflict",
    x = "Number of conflict events",
    y = "Density"     # after_stat(density) tells R to use same scale for y-axis
  ) + theme_minimal()

Plot1
Plot3
Plot4

(Plot1 | Plot2) / (Plot3 | Plot4)


#  Four moments of statistics.

df <- panel1 %>% as.data.frame()

view(df)

data.frame(
  variable = c("Severity", "Displaced", "Conflict"),
  mean     = c(mean(df$Severity, na.rm=TRUE), mean(df$Displaced, na.rm=TRUE), mean(df$Conflict, na.rm=TRUE)),
  median   = c(median(df$Severity, na.rm=TRUE), median(df$Displaced, na.rm=TRUE), median(df$Conflict, na.rm=TRUE)),
  sd       = c(sd(df$Severity, na.rm=TRUE), sd(df$Displaced, na.rm=TRUE), sd(df$Conflict, na.rm=TRUE)),
  skewness = c(skewness(df$Severity, na.rm=TRUE), skewness(df$Displaced, na.rm=TRUE), skewness(df$Conflict, na.rm=TRUE)),
  kurtosis = c(kurtosis(df$Severity, na.rm=TRUE), kurtosis(df$Displaced, na.rm=TRUE), kurtosis(df$Conflict, na.rm=TRUE))
)

datasummary(
  Severity + Displaced + log1p(Displaced) + Conflict ~
    Mean + SD + Min + Max + N + Var,
  data = panel1
)


datasummary_skim(panel1)

describe(panel1)

skim(panel1) # Very useful to understand variable types factor;numeric;etc)

datasummary(
  Severity + log1p(Displaced) + Conflict ~
     (Min + Max + Mean + Median + SD + kurtosis + skewness + N),
  data = panel1
)

# Justifying log1p transformation
library(patchwork)

p1 <- ggplot(panel1, aes(x = Displaced)) +
  geom_histogram(bins = 50) +
  labs(title = "Displaced (raw)")

p2 <- ggplot(panel1, aes(x = log1p(Displaced))) +
  geom_histogram(bins = 50) +
  labs(title = "Displaced (log1p)")

p1 + p2



panel2 <- panel1 %>%
  group_by(District_Id) %>%
  mutate(
    mean_severity = mean(Severity, na.rm = TRUE),
    mean_displaced = mean(log1p(Displaced), na.rm = TRUE)
  ) %>%
  ungroup()




# UNDERSTANDING THE STRUCTURE OF MISSING DATA -----------------------------


# Create a missing indicator
p.data <- p.data %>%
  mutate(missing_conflict = as.integer(is.na(Conflict)))

# Is missingness predicted by observable characteristics?
summary(glm(missing_conflict ~ Severity + Displaced + 
              Province + Year,
            data = p.data,
            family = binomial))



vis_miss(p.data)

all(p.data$Conflict == floor(p.data$Conflict)) # A count variable

all(p.data$Severity == floor(p.data$Severity))


ggplot(p.data, aes(x = Conflict)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Conflict")



 # Hint: Year >= 1997
p.data <- p.data %>%
  mutate(missing_conflict = as.integer(is.na(Conflict)))

 # Is missingness predicted by observable characteristics?

summary(glm(missing_conflict ~ Severity + Displaced + Province + Year, data = p.data,
                            family = binomial))


# Inner_join

p.data0 <- fd %>% 
  inner_join(cf, by = c("District", "Year")) %>% 
  filter(Year >= 1997 & Province!= "Cabo Delgado")

view(p.data0)
summary(p.data0)


