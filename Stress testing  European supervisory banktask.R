library("dplyr")

#1
bank_data <- read.csv("https://www.eba.europa.eu/assets/st21/full_database/TRA_CRE_IRB.csv")

#2
bank_data_filtered <- filter(bank_data, Scenario == 1)

bank_data_filtered <- filter(bank_data_filtered, Country != 0)

bank_data_filtered <- filter(bank_data_filtered, Portfolio %in% c(1, 2))

bank_data_filtered <- filter(bank_data_filtered, Exposure != 0)

bank_data_filtered <- filter(bank_data_filtered, IFRS9_Stages %in% c(1, 2, 3))

bank_data_filtered <- filter(bank_data_filtered, CR_guarantees == 0)

bank_data_filtered <- filter(bank_data_filtered, CR_exp_moratoria == 0)

bank_data_filtered <- filter(bank_data_filtered, Amount != "0")

#3
typeof(bank_data_filtered$Amount)

bank_data_filtered

#4

bank_data_filtered$Amount <- as.numeric(bank_data_filtered$Amount)

typeof(bank_data_filtered$Amount)

#5

is.na(bank_data_filtered$Amount)

# We can see that we have some NAs

bank_data_filtered <- na.omit(bank_data_filtered)

#6
bank_data_filtered$Amount <- 1000000 * bank_data_filtered$Amount
# We turn them back to character

bank_data_filtered$Amount <- as.character(bank_data_filtered$Amount)

typeof(bank_data_filtered$Amount)

#7
bank_data_filtered$LD <- substr(bank_data_filtered$Amount, 1, 1)

table_of_freq <- as.data.frame(table(bank_data_filtered$LD))

table_of_freq <-  mutate(table_of_freq, probabilities = table_of_freq$Freq / sum(table_of_freq$Freq))

table_of_freq$LD <- table_of_freq$Var1

#8

table_of_freq <- table_of_freq[2:10, 1:3]

Benfords_law <-
  c(
    0.30340656,
    0.18911175,
    0.11907036,
    0.09805794,
    0.06781280,
    0.06685769,
    0.05794333,
    0.04839223,
    0.04934734
  )

table_of_freq$Ben_law <- Benfords_law
table_of_freq$Deviation <-  abs(table_of_freq$probabilities - table_of_freq$Ben_law)

View(table_of_freq)

#Our empirical frequencies do not vary much from the Benford's Law frequencies
