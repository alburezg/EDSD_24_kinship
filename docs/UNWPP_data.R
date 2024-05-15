library(readr)
library(dplyr)

# Load and filter data based on country and a range of years

UNWPP_data <- function(country, start_year, end_year, sex) {
  
  # Read the data files
  WPP2022_f <- read_csv("unwppdata/WPP2022_Fertility_by_Age1.csv")  # Fertility data for females
  WPP2022_male_lifetable <- read_csv("unwppdata/WPP2022_Life_Table_Complete_Medium_Male_1950-2021.csv") # Male mortality data
  WPP2022_female_lifetable <- read_csv("unwppdata/WPP2022_Life_Table_Complete_Medium_Female_1950-2021.csv") # Female mortality data
  
  # Combine male and female life table data
  WPP2022_lifetable <- rbind(WPP2022_female_lifetable, WPP2022_male_lifetable)
  
  # px values by country, year range, and sex
  px <- WPP2022_lifetable %>%
    select(Location, sex_col = Sex, year = Time, age = AgeGrpStart, px) %>%
    filter(Location == country, year >= start_year, year <= end_year, sex_col == sex)
  
  # Initialize the final data as the px values if sex is Male
  if (sex == "Male") {
    data <- px
  } else {
    # If sex is Female, process fertility data
    # fx values
    asfr <- WPP2022_f %>%
      select(Location, year = Time, age = AgeGrpStart, fx = ASFR) %>%
      mutate(fx = fx / 1000) %>%
      filter(Location == country, year >= start_year, year <= end_year)
    
    # Join and finalize the data with fertility rates
    data <- left_join(px, asfr, by = c("Location", "year", "age")) %>%
      mutate(fx = replace(fx, is.na(fx), 0))
  }
  
  return(data)
}



