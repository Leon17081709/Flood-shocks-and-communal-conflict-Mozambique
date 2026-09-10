
# FE POISSON REGRESSION AND ROBUSTNESS CHECKS ------------------------------

install.packages("fixest")
install.packages("pscl")
install.packages("glmmTMB")
library(AER)
library(fixest)
library(tidyverse)
library(xlsx)
library(plm)
library(pscl)
library(modelsummary)
library(patchwork)
library(glmmTMB)


my_data <- read.csv("full_panel.csv")

view(my_data)
str(my_data)
glimpse(my_data)

mean(my_data$Conflict)
var(my_data$Conflict)

my_data$Region
hist(my_data$Conflict, breaks = 30)

Ols <- lm(Conflict ~ Severity + Log_Displaced, data = my_data)
summary(Ols)

# Test for heteroscedasticity

res <- residuals(Ols)

a <- ggplot(data = my_data, aes(x = Severity, y = res)) + geom_point(col = "blue") +
     geom_abline(slope = 0) + theme_minimal() +
  labs(title = "Residuals against Severity")

b <- ggplot(data = my_data, aes(x = Log_Displaced, y = res)) + geom_point(col = "blue") +
  geom_abline(slope = 0) + theme_minimal() +
  labs(title = "Residuals against Log_Displaced")

# Residual plots;

a + b

bptest(Ols)

# The test confirms that there is homoskedasticity.


ggplot(my_data, aes(x = factor(Severity), y = Conflict)) +
  geom_violin(fill = "skyblue") +
  labs(x = "Flood Severity", y = "Conflict") + theme_minimal()

ggplot(my_data, aes(x = factor(Severity), y = Conflict)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +
  labs(x = "Flood Severity", y = "Conflict") + theme_minimal()



# Regression plot between Conflict vs. Severity.

c <- ggplot(my_data, aes(x = Severity, y = Conflict)) +
     geom_point(col = "blue") +
     geom_smooth(method = "lm", se = FALSE, col = "firebrick") + 
     labs(title = "Conflict vs. severity relationship (A)") + theme_minimal()

d <- ggplot(my_data, aes(x = Log_Displaced, y = Conflict)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE, col = "firebrick") +
  labs(title = "Conflict vs. Log_Displaced relationship (B)") + theme_minimal()

c
d
c + d





# First research question -------------------------------------------------

# Population density has been removed due to collinearity (Main Specification)

# Creating a dummy for displacement variable.

my_data2 <- my_data %>% 
  mutate(Displaced_dummy = ifelse(Displaced > 0, 1, 0)
  )

view(my_data2)

FEC <- fepois(Conflict ~ Severity + 
                Nightlight + Urban_pop | District_Id + Year, data = my_data2)

FED <- fepois(Conflict ~ Severity + 
                Nightlight + Urban_pop | District_Id + Year,
              data = my_data2, vcov = ~ District_Id)

modelsummary(list("(1)" = FEC, "(2)" = FED), stars = T, fmt = 3,
             statistic = c("p.value", "std.error"))


# Second research question.

FEG <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Nightlight + Urban_pop |
                District_Id + Year, data = my_data2)

FEF <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Nightlight + Urban_pop |
                District_Id + Year, data = my_data2, vcov = ~ District_Id)

modelsummary(list("Without clustering" = FEG, "Clustered SE" = FEF),
             stars = TRUE, fmt = 3, statistic = c("p.value", "std.error"),
             coef_rename = c(
               "Severity"                       = "Flood Severity",
               "Nightlight"                     = "Nightlight",
               "Urban_pop"                      = "Urbanization",
               "Severity:factor(Region)2"       = "Severity × Region 2 (Centre)",
               "Severity:factor(Region)3"       = "Severity × Region 3 (South)"))

# Joint Wald test for regional heterogeneity

wald(FEF, keep = "Severity.*factor\\(Region\\)")





# ROBUSTNESS CHECKS WITH POPULATION DENSITY -------------------------------

# Without CS Errors -------------------------------------------------------

view(my_data2)

FE1 <- fepois(Conflict ~ Severity, data = my_data2)

FE2 <- fepois(Conflict ~ Severity + Log_Displaced, data = my_data2)

FE3 <- fepois(Conflict ~ Severity + Log_Displaced + Pop_density +
                Nightlight + Urban_pop, data = my_data2)

FE4 <- fepois(Conflict ~ Severity + Log_Displaced + Pop_density +
                Nightlight + Urban_pop | District_Id + Year,
              data = my_data2)


modelsummary(list("(1)" = FE1, "(2)" = FE2, "(3)" = FE3, "(4)" = FE4), stars = T, fmt = 3,
             statistic = c("p.value", "std.error"))



# With CS Errors ----------------------------------------------------------


FE6 <- fepois(Conflict ~ Severity  + Pop_density +
                Nightlight + Urban_pop | District_Id + Year, data = my_data2)

FE7 <- fepois(Conflict ~ Severity + Pop_density +
                Nightlight + Urban_pop | District_Id + Year,
              data = my_data2, vcov = ~ District_Id)


modelsummary(list("(1)" = FE6, "(2)" = FE7), stars = T, fmt = 3,  
             statistic = c("p.value", "std.error"))


# Second research question  -----------------------------------------------------


cor(my_data2[, c("Pop_density", "Urban_pop", "Nightlight")], use = "complete.obs")


FE8 <- fepois(Conflict ~ Severity + Severity:factor(Region) + 
              Pop_density + Nightlight + 
              Urban_pop | District_Id + Year, data = my_data2)

FE9 <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Pop_density + Nightlight + 
                Urban_pop | District_Id + Year, data = my_data2, cluster = ~District_Id)


modelsummary(list("(1)" = FE8, "(2)" = FE9),
             stars = TRUE, fmt = 3, statistic = c("p.value", "std.error"))



modelsummary(
  list("Without Clustering" = FE8, "Clustered SE" = FE9),
  stars = TRUE,
  fmt = 3,
  statistic = c("p.value", "std.error"),
  coef_rename = c(
    "Severity"                       = "Flood Severity",
    "Pop_density"                    = "Population Density",
    "Nightlight"                     = "Nightlight",
    "Urban_pop"                      = "Urbanization",
    "Severity:factor(Region)2"       = "Severity × Region 2 (Centre)",
    "Severity:factor(Region)3"       = "Severity × Region 3 (South)"
  ))



# ROBUSTNESS CHECKS WITH DISTRICT AND YEAR FE ONLY -------------------------------------------------------

# Answer to first research question. ----------------------------------------

FE6 <- fepois(Conflict ~ Severity +
                Nightlight + Urban_pop | District_Id + Year, data = my_data2)

# Use FE6 in this part.

summary(FE6)

FE7 <- fepois(Conflict ~ Severity +
              Nightlight + Urban_pop | District_Id + Year,
              data = my_data2, vcov = ~ District_Id)


modelsummary(list("(1)" = FE6, "(2)" = FE7), stars = T, fmt = 3,  
             statistic = c("p.value", "std.error"))


# Answer to second research question --------------------------------------

FEG <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Nightlight + Urban_pop |
                District_Id + Year, data = my_data2)

modelsummary(("(1)"= FEG), stars = T, fmt = 3,
             statistic = c("p.value", "std.error"))

# Final results are sensitive to clustering Standard Errors.



# ROBUSTNESS WITHOUT DISPLACEMENT VARIABLE --------------------------------

FEK <- fepois(Conflict ~ Severity + Displaced + Displaced_dummy +
                Nightlight + Urban_pop | District_Id + Year, data = my_data2)

FEL <- fepois(Conflict ~ Severity + Nightlight + Urban_pop | District_Id + Year,
              data = my_data2, vcov = ~ District_Id)

modelsummary(list("(1)" = FEK, "(2)" = FEL), stars = T, fmt = 3,
             statistic = c("p.value", "std.error"))


# Second research question.

FE0 <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Displaced + Displaced:factor(Region) +
                Displaced_dummy + Displaced_dummy:factor(Region)+
                Nightlight + Urban_pop |
                District_Id + Year, data = my_data2)

FEP <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Nightlight + Urban_pop |
                District_Id + Year, data = my_data2, vcov = ~ District_Id)

modelsummary(("(1)"= FEP), stars = TRUE,
              fmt = 3, statistic = c("p.value", "std.error"))

wald(FEP, keep = "Severity:factor\\(Region\\)")



# CLUSTERING AT PROVINCE LEVEL (FE) ---------------------------------------

# First research question

FEQ <- fepois(Conflict ~ Severity + 
                Nightlight + Urban_pop | District_Id + Year, data = my_data2)

FER <- fepois(Conflict ~ Severity + 
                Nightlight + Urban_pop | Province + Year,
              data = my_data2, vcov = ~ Province)

summary(FER)

modelsummary(list("(1)" = FEQ, "(2)" = FER), stars = T, fmt = 3,
             statistic = c("p.value", "std.error"))


# Second research question.

FES <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Nightlight + Urban_pop |
                District_Id + Year, data = my_data2)

FET <- fepois(Conflict ~ Severity + Severity:factor(Region) +
                Nightlight + Urban_pop |
                Province + Year, data = my_data2, vcov = ~ Province)

summary(FET)

modelsummary(("(1)" = FET), stars = T, fmt = 3, statistic = c("p.value", 
             "std.error"))

wald(FET, keep = "Severity:factor\\(Region\\)")


# DISPLACEMENT AS DEPENDENT VARIABLE (FE POISSON MODEL) ----------

# This approach is to understand the mechanism, perhaps displacement is affected by flood severity.

class(my_data2$Displaced)
is.integer(my_data2$Displaced)

all(my_data2$Displaced == round(my_data2$Displaced), na.rm = TRUE)

FEE <- fepois(Displaced ~ Severity + 
                       Urban_pop + Nightlight | District_Id + Year,
                       data = my_data2, vcov = ~ District_Id)

summary(FEE)

modelsummary(("FE Extension" = FEE),
             stars = TRUE, fmt = 3, statistic = c("p.value", "std.error"))

# Flood severity is associated with increase in expected number of people
# displaced by (0.715, equals to 104.4%)



# DISPLACEMENT DUMMY AS D.V (LINEAR PROB. MODEL) --------------------------

library(fixest)
view(my_data2)

Lpm <- feols(Displaced_dummy ~ Severity +
               Urban_pop + Nightlight | District_Id + Year,
             data = my_data2, vcov = ~ District_Id)

modelsummary(("Linear Prob. Model" = Lpm),
             stars = TRUE, fmt = 3, statistic = c("p.value", "std.error"))

sum(predict(Lpm) < 0)
sum(predict(Lpm) > 1)

Prob_hat <- predict(Lpm)
range(Prob_hat)

sum(Prob_hat < 0)
sum(Prob_hat > 1)

sum(Prob_hat < 0 | Prob_hat > 1)

mean(Prob_hat < 0 | Prob_hat > 1) * 100


Prob_hat <- predict(Lpm, sample = "original")

my_data2$Prob_hat <- Prob_hat

Obs <- my_data2[!is.na(my_data2$Prob_hat) & 
                  (my_data2$Prob_hat < 0 |my_data2$Prob_hat > 1),]


nrow(Obs)

Obs[, c(
  "District_Id", "Year", "Severity", "Displaced",
  "Displaced_dummy", "Nightlight", "Urban_pop", "Prob_hat"
)]


below_zero <- my_data2[
  !is.na(my_data2$Prob_hat) & my_data2$Prob_hat < 0,
]

above_one <- my_data2[
  !is.na(my_data2$Prob_hat) & my_data2$Prob_hat > 1,
]


range(below_zero$Prob_hat)
range(above_one$Prob_hat)

my_data2$Out_of_range <- my_data2$Prob_hat < 0 | my_data2$Prob_hat > 1

summary(Out_of_range)

table(my_data2$Out_of_range)


Lpm_robust <- feols(
  Displaced_dummy ~ Severity + Nightlight + Urban_pop | District_Id + Year,
  data = my_data2[my_data2$Out_of_range == FALSE,], cluster = ~ District_Id
)

Lpm_robust

modelsummary(
  ("(1)" = Lpm_robust), star = T,fmt = 3, 
  statistic = c("p.value", "std.error")
  )


# Plotting predicted probs vs. deviation from mean severity ---------------

my_data2$Sev_dev <- my_data2$Severity - 
  ave(
    my_data2$Severity,
    my_data2$District_Id,
    FUN = function(x) mean(x, na.rm = TRUE)
  )

my_data2 <- my_data2 %>% 
  mutate(Displaced_dummy2 = ifelse(Displaced_dummy > 0,
                            "Actual displacement", "Zero displacement"))
                                 

ggplot(my_data2, aes(x = Sev_dev, y = Prob_hat)) +
  
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey50") +
  
  geom_point(aes(color = Displaced_dummy2), size = 1.5, alpha = 0.6) +
  
  geom_rug(aes(color = Displaced_dummy2), sides = "b", alpha = 0.7, length = unit(0.03, "npc")) +
  
  scale_color_manual(values = c("Actual displacement" = "#E76F51", 
                                "Zero displacement" = "#2A9D8F")) +
  labs(
    x = "Deviation from Mean Flood Severity",
    y = "Predicted Probability",
    color = NULL,
    title = ""
  ) +
  
  scale_y_continuous(limits = c(-0.2, 1.2),
                     breaks = seq(-0.2, 1.2, by = 0.1)) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.text = element_text(size = 9)
  )



# DIAGNOSTIC TESTS --------------------------------------------------------

library(car)
library(knitr)
library(kableExtra)

# OLS on final control set — VIF is computed the same way
# regardless of the actual estimator, since it's purely about regressor collinearity

view(my_data2)

# With Population density

Ols_model <- lm(Conflict ~ Severity +
                Pop_density + Nightlight + Urban_pop, data = my_data2)

vif_vals <- vif(Ols_model)
vif_table <- data.frame(Variable = names(vif_vals), VIF = round(vif_vals, 2))
vif_table <- rbind(vif_table, data.frame(Variable = "Mean VIF", VIF = round(mean(vif_vals), 2)))

kable(vif_table, caption = "VIF Test for Multicollinearity", align = "lr") %>%
  kable_styling(full_width = FALSE)

# Without Population density

Ols_model <- lm(Conflict ~ Severity  +
                  Nightlight + Urban_pop, data = my_data2)

vif_vals <- vif(Ols_model)
vif_table <- data.frame(Variable = names(vif_vals), VIF = round(vif_vals, 2))
vif_table <- rbind(vif_table, data.frame(Variable = "Mean VIF", VIF = round(mean(vif_vals), 2)))

kable(vif_table, caption = "VIF Test for Multicollinearity", align = "lr") %>%
  kable_styling(full_width = FALSE)





