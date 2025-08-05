getwd()
#[1] "C:/Users/PALLABI/OneDrive/Desktop/AI_Omics_Internship_2025/Module_I"

#creating the following sub folder inside the project directory
dir.create("raw_data")     
dir.create("clean_data") 
dir.create("scripts")
dir.create("results")  
dir.create("plots")

#Download "patient_info.csv" data set from GitHub repository
# load the data set into your R environment

data= read.csv("patient_info.csv")
View(data)

# Inspect the structure of the data set using appropriate R functions
attributes(data)

class(data)#data.frame

str(data)
#$ patient_id: chr  "P001" "P002" "P003" "P004" ...
#$ age       : int  34 28 45 39 50 30 41 36 55 29 ...
#$ gender    : chr  "Male" "Female" "Female" "Male" ...
#$ diagnosis : chr  "Cancer" "Normal" "Cancer" "Normal" ...
#$ bmi       : num  22.5 20.3 26.7 23.8 27.1 21.9 25.4 24.2 28.6 19.8 ...
#$ smoker    : chr  "Yes" "No" "Yes" "No"

nrow(data)#20
ncol(data)#6


# Identify variables with incorrect or inconsistent data types.
# Convert variables to appropriate data types where needed


##so here gender,diagnosis and smoker have incorrect data types.
data$gender <- as.factor(data$gender)
data$diagnosis <- as.factor(data$diagnosis)
data$smoker <- as.factor(data$smoker)

summary(data$gender)
#Female   Male 
#11      9 

summary(data$diagnosis)
#Cancer Normal 
#11      9 

summary(data$smoker)
#No Yes 
#9  11 

class(data$gender)#"factor"
str(data)
#$ patient_id       : chr  "P001" "P002" "P003" "P004" ...
#$ age              : int  34 28 45 39 50 30 41 36 55 29 ...
#$ gender           : Factor w/ 2 levels "Female","Male": 2 1 1 2 1 2 1 1 2 1 ...
#$ diagnosis        : Factor w/ 2 levels "Cancer","Normal": 1 2 1 2 1 2 1 2 1 2 ...
#$ bmi              : num  22.5 20.3 26.7 23.8 27.1 21.9 25.4 24.2 28.6 19.8 ...
#$ smoker           : Factor w/ 2 levels "No","Yes": 2 1 2 1 2 1 2 1 2 1 ...



# Create a new variable for smoking status as a binary factor:# 1 for "Yes", 0 for "No"

data$smoker_binary_var= factor(ifelse(data$smoker == "Yes",1,0), c(0,1))
class(data$smoker_binary_var)#factor
levels(data$smoker_binary_var)#"0" "1"

# Save the cleaned data set in your clean_data folder with the name patient_info_clean.csv
# Save your R script in your script folder with name "class_Ib"

write.csv(data,"clean_data/patient_info_clean.csv")
