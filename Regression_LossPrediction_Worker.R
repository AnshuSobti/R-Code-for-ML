#Importing the libraries
#install.packages("dplyr")
#install.packages("ggplot2")
library("dplyr")
library("ggplot2")

#Setting the working sirectory
getwd()
setwd("C:\\Users\\Anshu\\Downloads\\Regression")
getwd()

#Import the dataset
data = read.csv("Loss_prediction_worker_compensation_train - Regression Dataset.csv", na.strings = c("NA", "N/A", "missing", "NaN", "NULL",""," "))
#Data exploration
head(data)
tail(data)
#str(data)
#summary(data)

#To calculate the summary of the dataframe

calculate_summary_all = function(data) {
  # Create an empty data frame to store the summary results
  summary_df_char <- data.frame()
  summary_df_num <- data.frame()
  
  # Loop through each column in the data frame
  for (col_name in names(data)) {
    col <- data[[col_name]]
    
    # Calculate the summary statistics for each column
    if(class(col)=="character"){
    col_summary_char <- data.frame(
      name = col_name,
      rows = length(col),
      missing = sum(is.na(col)),
      type = class(col),
      distinct_values = n_distinct(col),
      first_10_distinct = substr(paste(head(unique(col), 10), collapse = ", "),1,50)
    )
    summary_df_char <- rbind(summary_df_char, col_summary_char)
    }else{
      min_value = min(col, na.rm = TRUE)
      q1_value = quantile(col, 0.25, na.rm = TRUE,names=FALSE)
      mean_value = mean(col, na.rm = TRUE)
      median_value = median(col, na.rm = TRUE)
      q3_value = quantile(col, 0.75, na.rm = TRUE,names=FALSE)
      max_value = max(col, na.rm = TRUE)
      skewness = (mean_value - median_value) / (q3_value - q1_value)
      skewed = ifelse(skewness < 0, "Left-tailed", "Right-tailed")
      
      col_summary_num <- data.frame(
        name = col_name,
        rows = length(col),
        missing = sum(is.na(col)),
        type = class(col),
        minimum=min_value,
        q1 = q1_value,
        mean=mean_value,
        median=median_value,
        q3=q3_value,
        max=max_value,
        skewness_value = skewness,
        left_right = skewed
      ) 
      summary_df_num <- rbind(summary_df_num, col_summary_num)
    }
    # Append the column summary to the results data frame
  }
  cat("\n")
  print("Summary for all the charcter columns in the dataframe")
  cat("\n")  
  print(summary_df_char)
  cat("\n")
  cat("\n")
  print("Summary for all the numeric columns in the dataframe")
  cat("\n")
  print(summary_df_num)
}

# Usage: Call the function with your data frame
calculate_summary_all(data)


