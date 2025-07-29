
install.packages("readxl")

# Load it
library(readxl)

# Read the Excel file
df <- read_excel("/Users/adelaidemadary/Desktop/Thesis/Data/Mayors/Mayors_Calabria.xlsx")

t.test(gov_diss_dummy~SprarPresent, data = df)


df |> 
  group_by(SprarPresent) |> 
  summarise(
    mean_gov_diss_dummy = mean(gov_diss_dummy, na.rm = TRUE),
    sd_gov_diss_dummy = sd(gov_diss_dummy, na.rm = TRUE),
    n = n()
  )
