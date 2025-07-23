library(tableone)
library(MatchIt)
library(tidyverse)
library(dplyr)



data <- read.csv('/Users/adelaidemadary/Desktop/Thesis/Data/Political Parties/2018/filtered_2018.csv', header = TRUE)


clean_data <- data |> dplyr::select(COMUNE_file1, sprar = Sprar2017, 
                                    Center_Right, Center_Left, M5S)

clean_data$sprar[is.na(clean_data$sprar)] <- 0

t.test(M5S~sprar, data = clean_data)
t.test(Center_Right~sprar, data = clean_data)
t.test(Center_Left~sprar, data = clean_data)

clean_data |> 
  group_by(sprar) |> 
  summarise(
    mean_M5S_Win = mean(M5S, na.rm = TRUE),
    sd_M5S_Win = sd(M5S, na.rm = TRUE),
    n = n()
  )
clean_data |> 
  group_by(sprar) |> 
  summarise(
    mean_Center_Right = mean(Center_Right, na.rm = TRUE),
    sd_Center_Right = sd(Center_Right, na.rm = TRUE),
    n = n()
  )
clean_data |> 
  group_by(sprar) |> 
  summarise(
    mean_Center_Left = mean(Center_Left, na.rm = TRUE),
    sd_Center_Left = sd(Center_Left, na.rm = TRUE),
    n = n()
  )
