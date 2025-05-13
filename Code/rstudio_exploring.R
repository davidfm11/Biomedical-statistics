

#### Explore RStudio ####

# Import a database in R using the readxl package and get an overview
library(readxl)
myeloma <- read_excel("myeloma.xlsx") # change to your directory
str(myeloma)

# Examine columns or variables. Use the tab key to quickly see columns
myeloma$Patient # myeloma[1] or myeloma["Patient"]
myeloma$molecular_group # myeloma[2] or myeloma["molecular_group"]

# Examine columns and rows
# dataframe[row,column]
myeloma[3,2] # myeloma[3,"molecular_group"]

# Entire rows
myeloma[3,] # entire 3rd row

# If rownames have actual names, do the same as a column
# I do not recommend it
myeloma["GSM51000",]

# Categorical data
class(myeloma$chr1q21_status) # Which type of variable is this?
table(myeloma$chr1q21_status) # Categories
levels(myeloma$chr1q21_status) # Categories
levels(as.factor(myeloma$chr1q21_status)) # Categories

# Numerical data
class(myeloma$CCND1)
mean(myeloma$CCND1)
median(myeloma$CCND1)

#### Introduction to the tidyverse ####

# Install tidyverse
# install.packages("tidyverse)
library(tidyverse)
myeloma %>% select(molecular_group, treatment) # Select columns
myeloma %>% filter(molecular_group == "MMSET") # Select rows matching a condition
myeloma %>% filter(molecular_group %in% c("MMSET", "Cyclin D-1")) 
myeloma %>% filter(molecular_group == "MMSET" & treatment == "TT2")
myeloma %>% filter(molecular_group == "MMSET" & TP53 < 1000)

MMSET_lowTP53 <- myeloma %>% filter(molecular_group == "MMSET" & TP53 < 1000)

# Perform calculations
myeloma %>% summarise(mean(TP53)) # mean(myeloma$TP53)
myeloma %>% summarise(mean(TP53)/100) # divide by 100
myeloma %>% group_by(molecular_group) %>% summarise(mean(TP53))

# Create a new column with "mutate"
myeloma %>% mutate(TP53_expression = "low")

# Create a column based on a condition
# ifelse(expression, TRUE, FALSE)
myeloma <- myeloma %>% mutate(TP53_expression = ifelse(TP53 < 1500, "low", "high"))





#### Comparisons ####

# T-test
t.test(myeloma$TP53 ~ myeloma$event)

# Normality test: Try doing it in each event subgroup
# shapiro.test(myeloma$TP53)

# ANOVA
aov(myeloma$TP53 ~ myeloma$molecular_group) %>% summary()

# Non-parametric
wilcox.test(myeloma$TP53 ~ myeloma$event)
kruskal.test(myeloma$TP53 ~ myeloma$molecular_group)


ggplot(database, aes(x, y)) +
  geom_boxplot  # you can change the geometry to geom_scatter, geom_bar, etc

# factor(event) will categorize the event variable 
ggplot(myeloma, aes(x=factor(event), y=TP53)) +
  geom_boxplot()

# Event vs. molecular_group
# First check the proportions
table(myeloma$event) # single variable
table(myeloma$event, myeloma$TP53_expression) # two categories
prop.table(table(myeloma$event, myeloma$TP53_expression)) # in % or real proportions
prop.table(table(myeloma$event, myeloma$TP53_expression), margin=1) # row sum
prop.table(table(myeloma$event, myeloma$TP53_expression), margin=2) # column sum
chisq.test(myeloma$event, myeloma$TP53_expression) # chi-square

# Reproduce the following plots
ggplot(myeloma, aes(x=TP53_expression))+
  geom_bar()

ggplot(myeloma, aes(x=TP53_expression, fill = factor(event)))+
  geom_bar()

ggplot(myeloma, aes(x=TP53_expression, fill = factor(event)))+
  geom_bar(position = "fill")

# Correlation
cor.test(myeloma$TP53, myeloma$CCND1, method = "pearson") # or spearman
lm(myeloma$TP53 ~ myeloma$CCND1) # %>% summary()

ggplot(myeloma, aes(x=TP53, y=CCND1, color=factor(event)))+
  geom_point()
