# install.packages("tableone")
#install.packages("tableone")
#install.packages("kableExtra")
#install.packages("ggplot2")
#install.packages("tidyverse")
library(tableone)
library(MatchIt)
library(tidyverse)
library(dplyr)



data <- read.csv('/Users/adelaidemadary/Desktop/Thesis/Data/Municipal_charateristics/Calabria_municipal_data.csv', header = TRUE)

clean_data <- data |> dplyr::select(comune_nome, sprar = sprar.present.at.any.time.2012.16., 
                                    perc_disoccupati_2011, 
                                    Pop1_1_12,
                                    turnout2008,
                                    PCT_stranieri_2011,
                                    Dpop11_01,
                                    perc_addetti_agri2011,
                                    reddito_medio, old_pct_70)
clean_data <- clean_data |> filter( 
                                   !is.na(perc_addetti_agri2011),
                                   !is.na(sprar))

clean_data$log_population <- log(clean_data$Pop1_1_12)


t.test(log_population~sprar, data = clean_data)

clean_data |> 
  group_by(sprar) |> 
  summarise(
    mean_log_population = mean(log_population, na.rm = TRUE),
    sd_log_population = sd(log_population, na.rm = TRUE),
    n = n()
  )
 



