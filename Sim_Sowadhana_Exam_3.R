##################################################
# ECON 418-518 Exam 3
# Sowadhana Sim
# The University of Arizona
# sowadhana@arizona.edu 
# 15 December 2024
###################################################


#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Install data.table package if not already installed
if (!require(data.table)) install.packages("data.table")

# Load the data.table package
library(data.table)

# Load data as data table
library(readr)
dt <- read_csv("Downloads/ECON_418-518_Exam_3_Data.csv")
dt <- as.data.table(dt)

####################
# Problem 3
#####################


#################
# Question (ii)
#################

# Code

# Add columns
dt[, Nov := ifelse(time_period == "Nov", 1, 0)]
dt[, NJ := ifelse(state == 1, 1, 0)]

# Calculate mean total employment
mean_emp <- dt[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)),
               by = .(state = ifelse(NJ == 1, "New Jersey", "Pennsylvania"),
                      time_period = time_period)]
print(mean_emp)

#################
# Question (iii)
#################

# Code

# Extract the means for each group
mean_NJ_Nov <- mean_emp[state == "New Jersey" & time_period == "Nov", mean_total_emp]
mean_NJ_Feb <- mean_emp[state == "New Jersey" & time_period == "Feb", mean_total_emp]
mean_PA_Nov <- mean_emp[state == "Pennsylvania" & time_period == "Nov", mean_total_emp]
mean_PA_Feb <- mean_emp[state == "Pennsylvania" & time_period == "Feb", mean_total_emp]

# Compute within-state changes
change_NJ <- mean_NJ_Nov - mean_NJ_Feb
change_PA <- mean_PA_Nov - mean_PA_Feb

# Compute the DiD estimate
DiD_estimate <- change_NJ - change_PA

# Display results
list(
  mean_total_emp = mean_emp,
  Change_NJ = change_NJ,
  Change_PA = change_PA,
  DiD_Estimate = DiD_estimate
)

# Create a simple data frame for visualization
library(ggplot2)
plot_data <- data.table(
  State = rep(c("New Jersey", "Pennsylvania"), each = 2),
  Time_Period = rep(c("Feb", "Nov"), times = 2),
  Mean_Total_Employment = c(mean_NJ_Feb, mean_NJ_Nov, mean_PA_Feb, mean_PA_Nov)
)

# Plot
ggplot(plot_data, aes(x = Time_Period, y = Mean_Total_Employment, group = State, color = State)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Difference-in-Differences Visualization",
       x = "Time Period",
       y = "Mean Total Employment") +
  theme_minimal()

#################
# Question (iv)
#################

# Code

# Create variables for the model
dt[, Post := ifelse(time_period == "Nov", 1, 0)]
dt[, NJ := ifelse(state == 1, 1, 0)]
dt[, Interaction := NJ * Post]

# Estimate the DiD model using lm()
reg <- lm(total_emp ~ NJ + Post + Interaction, data = dt)

summary(reg)

# Extract coefficients and standard errors
coef_est <- coef(reg)["Interaction"]
se <- summary(reg)$coefficients["Interaction", "Std. Error"]

# 95% Confidence Interval (by hand)
z_value <- 1.96
CI_lower <- coef_est - z_value * se
CI_upper <- coef_est + z_value * se

# Display confidence interval
confidence_interval <- c(CI_lower, CI_upper)
cat("95% Confidence Interval: [", CI_lower, ",", CI_upper, "]\n")

# Confidence interval using R
confint(reg, level = 0.95)

# Hypothesis Testing:
# Null hypothesis: ATT = 5
t_stat_5 <- (coef_est - 5) / se
p_value_5 <- 2 * (1 - pnorm(abs(t_stat_5)))

# Null hypothesis: ATT = 0
t_stat_0 <- (coef_est - 0) / se
p_value_0 <- 2 * (1 - pnorm(abs(t_stat_0))) # Two-sided test for H0: ATT = 0

list(
  Coefficient = coef_est,
  Standard_Error = se,
  Confidence_Interval = confidence_interval,
  Hypothesis_Test_ATT_5 = list(t_stat = t_stat_5, p_value = p_value_5),
  Hypothesis_Test_ATT_0 = list(t_stat = t_stat_0, p_value = p_value_0)
)

#################
# Question (vii)
#################

# Code

# Estimate the DiD model with restaurant fixed-effects
reg_fe <- lm(total_emp ~ NJ + Post + Interaction + factor(restaurant_id), data = dt)

# Summary of the model
summary(reg_fe)
