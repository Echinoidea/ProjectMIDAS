# read old test data
df <- read.csv("./data/dummy_midas_data_1000.csv")

# Add generated MIDAS risk score
df$midasRisk <- sample(1:50, nrow(df), replace = TRUE)

write.csv(df, "./data/dummy_midas_data_1000.csv")
