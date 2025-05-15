



# Import excel files: I recommend to install the package "readxl"
# install.packages("readxl")  

library(readxl)
myeloma <- read_excel("/Users/dfmoreno/Dropbox/Lectures/myeloma.xlsx") # change your path

# Check the first lines 
head(myeloma)

# Check the last lines
tail(myeloma)

# See the "structure" of the database. 
# Gives you all the columns and what "class" or type of variables they contain:
# chr --> character 
# num --> numeric (decimals)
# int --> integer (no decimals)
# factor --> factor (positive OR negative)
str(myeloma)

# It is good practice to include the separator (sep)
# read.csv can read multiple formats, not only csv files
myeloma_commas <- read.csv("/Users/dfmoreno/Dropbox/Lectures/myeloma_commas.csv", sep=",") # change your path
myeloma_tabs <- read.csv("/Users/dfmoreno/Dropbox/Lectures/myeloma_tabs.tsv", sep="\t") # change your path

# Explore the dataframe
head(myeloma_commas)
head(myeloma_tabs)
