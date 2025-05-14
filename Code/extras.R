
### Extras ###

##### Multiple dataframes ####
# Work with the myeloma data and add PRDM1 mutations data
library(readxl)
library(tidyverse)
myeloma <- read_excel("myeloma.xlsx")
prdm1 <- read_excel("myeloma_prdm1.xlsx")
myeloma_2 <- read_excel("myeloma_2.xlsx")

# Check the joining
left <- left_join(myeloma, prdm1, by = "Patient")
right <- right_join(myeloma, prdm1, by = "Patient")
inner <- inner_join(myeloma, myeloma_2, by = "Patient")
full <- full_join(myeloma, myeloma_2, by = "Patient")

##### Modify values ####
library(tidyverse)
# Keep working with the myeloma database
myeloma %>% 
  rename(chr1 = chr1q21_status, 
         Patient_Id = Patient)

myeloma %>% 
  mutate(molecular_group_new = ifelse(molecular_group == "Hyperdiploid", 1, 0))

myeloma %>% 
  mutate(molecular_group_new = case_when(molecular_group == "Hyperdiploid" ~ 1,
                                         TRUE ~ 2))
myeloma %>% 
  mutate(molecular_group_new = case_when(molecular_group == "Hyperdiploid" ~ 1,
                                         molecular_group == "MMSET" ~ 3,
                                         TRUE ~ 2))
myeloma %>% 
  mutate(molecular_group = case_when(molecular_group == "Hyperdiploid" ~ 1,
                                     TRUE ~ 2))

# Work with the "diagnoses" data
library(irr)
data("diagnoses")

diagnoses %>% 
  select(rater1)

diagnoses %>% 
  select(matches("rater"))

gsub("rater", "physician", colnames(diagnoses))

diagnoses %>%
  rename_with(~ gsub("rater", "physician", .))

# Substitutions
# Work with the both "diagnoses" and myeloma data
gsub("^", "test_", colnames(diagnoses)) # beginning
gsub("$", "_test", colnames(diagnoses)) # end
gsub("[1-9]", "x", colnames(diagnoses)) # any number
gsub("[a-z]", "A", colnames(diagnoses)) # each character
gsub("\\s", "_", myeloma$chr1q21_status) # empty space
gsub("\\s", "_", myeloma$molecular_group) # empty space
sub("\\s", "_", myeloma$molecular_group) # empty space

