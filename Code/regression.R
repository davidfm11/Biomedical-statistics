### Regression ###

##### Linear regression ####

install.packages("lgrdata")

# You should first install this package to load the data that it contains
library(lgrdata) # load the package
library(tidyverse) 

data(howell) # load the dataset
str(howell) # see the structure

ggplot(howell, aes(x=weight, y=height)) +
  geom_point()

# Run a linear model
linear_model <- lm(height ~ weight, data = howell)
summary(linear_model)

# Run a correlation test
cor.test(howell$weight, howell$height, method = "pearson")

# Make an interpretation
res <- residuals(linear_model)
predicted <- fitted(linear_model)

ggplot(howell, aes(x = weight, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(x = weight, xend = weight, y = predicted, yend = height),
               color = "red", linetype = "dotted") +
  geom_hline(yintercept = 76)

##### Logistic regression ####

# Read the myeloma dataset and categorize TP53 expression
library(readxl)
myeloma <- read_excel("myeloma.xlsx") # remember to change your path
mean(myeloma$TP53)
myeloma <- myeloma %>% 
  mutate(low_tp53 = ifelse(TP53<=1500, 1, 0))
table(myeloma$low_tp53)

# Run a glm to explain/predict low TP53 expression
log_reg <- glm(low_tp53 ~ molecular_group, data = myeloma, family = binomial)
exp(coef(log_reg)) # convert the coefficients to odds ratio (OR)

# Discrimination test
install.packages("pROC")
library(pROC)

# Calculate predicted probabilities
prob <- predict(log_reg, type = "response")

# Generate a ROC object
roc_obj <- roc(myeloma$low_tp53, prob)
ci(roc_obj) # confidence interval
plot(roc_obj, col = "blue", main = "ROC Curve", print.auc = TRUE)

# Use a customized plot from ggplot
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "red", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  xlab("1 - Specificity") + 
  ylab("Sensitivity")
##### Cox regression ####

# Install the following packages
install.packages("survival")
install.packages("survminer") 
install.packages("ggsurvfit")

# Load the packages
library(survival)
library(survminer)
library(ggsurvfit)

# Obtain the primary biliary cirrhosis (pbc)
# status (1=liver transplant, 2=death, 0=alive)
# time: time to last follow-up
data(pbc)
head(pbc) # this will get part of the dataset

# Fit a cox regression model and interpret it
cox_stage1 <- coxph(Surv(time, status == 2) ~ stage, data = pbc)
summary(cox_stage1)

# Fit Cox regression model
cox_stage2 <- coxph(Surv(time, status == 2) ~ factor(stage), data = pbc)

# Estimate survival curves for plotting
survival <- survfit2(Surv(time, status == 2) ~ 1, data = pbc)
ggsurvfit(survival) +
  labs(x = "Days", y = "Progression free survival probability")

# What if I need to calculate the time?
# The package lubridate handles dates easily
install.packages("lubridate")
library(lubridate)
library(tidyverse)

event_data <- read_xlsx("dates_file.xlsx")

event_data <- event_data %>% 
  mutate(pfs_yrs = as.duration(dx_date %--% event_date) / dyears(1))

# Also try
# mutate(pfs_yrs = time_length(dx_date %--% event_date, unit = "years"))
# mutate(pfs_yrs = time_length(dx_date %--% event_date, unit = "months"))
# mutate(pfs_yrs = time_length(dx_date %--% event_date, unit = "days"))

survival_data <- survfit2(Surv(pfs_yrs, status==1) ~ protein1, data = event_data) 

ggsurvfit(survival_data) +
  labs(x = "Years", y = "Progression free survival probability")

# Obtain the log-rank test using survdiff (from survival package)
survdiff(Surv(pfs_yrs, status==1) ~ protein1, data = event_data) 
##### Multivariable regression ####

# You can add more variables
# Try using cox regression or linear regression with the same format
glm(low_tp53 ~ molecular_group + chr1q21_status, data = myeloma, family = binomial) %>% 
  summary()
