## Task - 1 : Manipulation

# Loading the `readxl` package
library(readxl)
# Loading the data from the Excel sheet
data = read_excel("EurostatCrime2021.xlsx", skip = 6)
#Viewing the first few rows by using `head()` function
head(data)

# Finding the size of the data set
size = dim(data)
# `dim()` function sets the dimension of the given data frame
# Printing the number of rows
print(paste("The number of rows in the dataset:", size[1]))
# Printing the number of columns
print(paste("The number of colummns in the dataset:", size[2]))

# Displaying the structure of the data set
str(data)
#`str()` displays the structures of objects or the summary of the output produced.


# Using `subset()` function to perform the above operation on the data
new_data = subset(data, select = -c(`Fraud`, `Money laundering`))
# We can use `subset()` function to select parts of the matrix, vector or data frame.
print(new_data)

new_data1 = subset(new_data, select = -c(
  Theft,
  `Theft of a motorized vehicle or parts thereof`,
  Burglary,
  `Burglary of private residential premises`
))
print(new_data1)
head(new_data1)

# Using the set `new_data1` to add columns to the overall record of offences
# Ensuring that all the columns contain numeric data and so converting the columns to numeric using `as.numeric()` function
new_data1[, -1] = lapply(new_data1[, -1], as.numeric)
new_data1$Record = rowSums(new_data1[, -1], na.rm=TRUE)
new_data1$Record


new_data1$Country[apply(is.na(new_data1), 1, any)]


# Using `na.omit()` function 
# This function removes rows with missing values from the data set
updated_data = na.omit(new_data1)
updated_data


# Using the `updated_data` to find out observations and variables
updated_size = dim(updated_data)

# Printing number of rows
print(paste("The number of rows in the new data set are:", updated_size[1]))

# Printing number of columns
print(paste("The number of columns in the new data set are:", updated_size[2]))


## Task - 2: Analysis

# Working with the data set produced at the end of Task 1

new_data3 = updated_data[order(updated_data$Record, decreasing = TRUE), ]

# Printing the data of country with highest record of offences in 2021 (per hundred thousand inhabitant)
print(paste("The country with highest record of offences in 2021 is : ",new_data3$Country[1]))


# Creating the table to produce list of countries and their proportion of the overall crimes due to acts against computer system.
updated_data$`Acts against computer systems` = round(updated_data$`Acts against computer systems` / updated_data$Record, 3)

# Creating the table using data frames
crimes_table = data.frame(Country = updated_data$Country, Proportion = updated_data$`Acts against computer systems`)

# sorting the rows and displaying the first three decimal digits.
sorted_crimes_table = crimes_table[order(crimes_table$Proportion), ]

# Printing the sorted table with three decimal digits
sorted_crimes_table


# Loading the package "ggplot2"
library(ggplot2)

# Plotting the graph to display the relationship
ggplot(updated_data, aes(x = Robbery, 
                         y = `Unlawful acts involving controlled drugs or precursors`, label = Country)) +
  geom_point(size = 2) +
  geom_text(hjust = 0.5, vjust = -0.6) +
  labs(x = "Robbery", y = "Controlled Drugs or Precursors") +
  ggtitle("Country wise Robbery vs Controlled Drugs/Precursors")


## Task 3 Creativity


# Loading the required package `ggplot2`
library(ggplot2)

# Comparing the `Sexual Exploitation` data of all the countries present in the given data set and plotting a bar graph
ggplot(updated_data, aes(x = reorder(Country, `Sexual exploitation`), y = `Sexual exploitation`, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Sexual Exploitation Data") +
  theme_minimal() +
  ggtitle("Sexual Exploitation Data of all countries")

updated_data


# For the second plot let us look at the highest assault rates and highlight the countries
# Loading the required package
library(ggplot2)

# Sorting the data by Sexual Assault rate in descending order
sorted_data = updated_data[order(updated_data$`Sexual assault`, decreasing = TRUE), ]

# Creating a bar plot for countries with the highest Sexual Assault rates
countries = head(sorted_data, 15)
ggplot(countries, aes(x = reorder(Country, -`Sexual assault`), y = `Sexual assault`)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Sexual assault") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Countries with the Highest Assault Rates")

updated_data


#For the above bar graph, we have used the cleaned and updated version of the data set. We have compared 
#the `Sexual Assault` between countries from the data set and we can conclude that `Finland` has the highest 
#amount of `Sexual Assault` and again `Greece` has the lowest rates in both `Sexual Assault` as well as `Sexual Exploitation`.

