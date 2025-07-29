library(MatchIt)
library(dplyr)


data <- read.csv('/Users/adelaidemadary/Desktop/Thesis/Data/Municipal_charateristics/Calabria_municipal_data.csv', header = TRUE)

data_nas <- data %>%
  select(
    comune_nome,
    sprar = sprar.present.at.any.time.2012.16.,
    perc_disoccupati_2011,
    Pop1_1_12,
    turnout2008,
    reddito_medio,
    Dpop11_01,
    old_pct_70,
    coalwin,
    PCT_stranieri_2011
  ) %>%
  # Filter: remove missing data
  filter(
    comune_nome == ""|
    is.na(sprar)|
    is.na(perc_disoccupati_2011)|
    is.na(Pop1_1_12)|
    is.na(turnout2008)|
    is.na(reddito_medio)|
    is.na(Dpop11_01)|
    is.na(old_pct_70)|
    is.na(coalwin)|
    is.na(PCT_stranieri_2011)
  ) %>%
  # Filter: population under 2000
  filter(Pop1_1_12 < 2000)
# Clean and select relevant columns
clean_data <- data %>%
  select(
    comune_nome,
    sprar = sprar.present.at.any.time.2012.16.,
    perc_disoccupati_2011,
    Pop1_1_12,
    turnout2008,
    reddito_medio,
    Dpop11_01,
    old_pct_70,
    coalwin,
    PCT_stranieri_2011
  ) %>%
  # Filter: remove missing data
  filter(
    comune_nome != "",
    !is.na(sprar),
    !is.na(perc_disoccupati_2011),
    !is.na(Pop1_1_12),
    !is.na(turnout2008),
    !is.na(reddito_medio),
    !is.na(Dpop11_01),
    !is.na(old_pct_70),
    !is.na(coalwin),
    !is.na(PCT_stranieri_2011)
  ) %>%
  # Filter: population under 2000
  filter(Pop1_1_12 < 2000)

# Perform Nearest Neighbor Matching
match_model <- matchit(
  sprar ~ perc_disoccupati_2011 + Pop1_1_12 + turnout2008 +
    reddito_medio + Dpop11_01 + old_pct_70 + coalwin + PCT_stranieri_2011,
  data = clean_data,
  method = "nearest",
  ratio = 4, # 1 treated : 6 controls
  caplier = 0.2,
  exact = "coalwin"
  
)

# Get matched data
matched_data <- match.data(match_model)
c <- matched_data|> group_by(subclass)|>select(coalwin)

# Reorder by matched groups
reordered_data <- matched_data[order(matched_data$subclass), ]

# Save to CSV
write.csv(reordered_data, "/Users/adelaidemadary/Desktop/Thesis/Data/Municipal_charateristics/nearest_neighbor/matched_municipalities_11NN.csv", row.names = FALSE)
