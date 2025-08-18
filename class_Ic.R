# 1. Check Cholesterol level (using if) 
# Write an If statement to check cholesterol level is greater than 240, 
# if true, it will prints “High Cholesterol”


cholestrol=230
if(cholestrol>240){
  print("High Cholestrol")
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. Blood Pressure Status (using if...else)
# Write an if…else statement to check if blood pressure is normal.
# If it’s less than 120, print: “Blood Pressure is normal”
# If false then print: “Blood Pressure is high”

systolic_bp=130
if(systolic_bp<120){
  print("Blood Pressure is normal")
}else{
  print("Blood Pressure is high")
}

#output= [1] "Blood Pressure is high"

#----------------------------------------------------------------------------------------------------------------------------------------

# 3. Automating Data Type Conversion with for loop

# Use patient_info.csv data and metadata.csv
# Perform the following steps separately on each dataset (patient_info.csv data and metadata.csv)
# Create a copy of the dataset to work on.
# Identify all columns that should be converted to factor type.
# Store their names in a variable (factor_cols).

# Example: factor_cols <- c("gender", "smoking_status")

# Use a for loop to convert all the columns in factor_cols to factor type.
# Pass factor_cols to the loop as a vector.

# Hint:
# for (col in factor_cols) {
#   data[[col]] <- as.factor(data[[col]])  # Replace 'data' with the name of your dataset
# }

patient_info=read.csv(choose.files())
meta_data=read.csv(choose.files())

copy_patient_data=patient_info
copy_metadata=meta_data

str(copy_patient_data)
#here we see gender,diagnosis and smoker are  wrong data type they should be factors.

factor_cols=c("gender","diagnosis","smoker")
factor_cols

for (col in factor_cols) {
  copy_patient_data[[col]] <- as.factor(copy_patient_data[[col]])
}


str(copy_patient_data)


#################
str(copy_metadata)
##here we see gender and height are  wrong data type they should be factors.
factor_col_metadata=c("gender","height")

for( col in factor_col_metadata){
  copy_metadata[[col]]=as.factor(copy_metadata[[col]])
  }
str(copy_metadata)

#----------------------------------------------------------------------------------------------------------------------------------------------------------

# 4. Converting Factors to Numeric Codes

# Choose one or more factor columns (e.g., smoking_status).
# Convert "Yes" to 1 and "No" to 0 using a for loop.

  
binary_cols <- c("gender", "smoker")   

# Loop over each column and convert Yes/No or Female/Male to 1/0
for (col in binary_cols) {
  copy_patient_data[[col]] <- ifelse(copy_patient_data[[col]] %in% c("Female", "Yes"), 1, 0)
}

str(copy_patient_data)
head(copy_patient_data)

str(patient_info)
head(patient_info)
