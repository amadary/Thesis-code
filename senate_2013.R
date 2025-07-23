library(tableone)
library(MatchIt)
library(tidyverse)
library(dplyr)



data <- read.csv('/Users/adelaidemadary/Desktop/Thesis/Data/Political Parties/merged.csv', header = TRUE)

clean_data <- data |> dplyr::select(COMUNE, sprar = sprar.present.at.any.time.2012.16., 
                                    Left_Wing_Win, Right_Wing_Win, M5S_Win)


t.test(M5S_Win~sprar, data = clean_data)

clean_data |> 
  group_by(sprar) |> 
  summarise(
    mean_M5S_Win = mean(M5S_Win, na.rm = TRUE),
    sd_M5S_Win = sd(M5S_Win, na.rm = TRUE),
    n = n()
  )
