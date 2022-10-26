# read old test data
df <- read.csv("./data/dummy_midas_data_1000.csv")
names <- read.delim("./data/Names.txt", sep = "\n", header = FALSE)

# Add generated MIDAS risk score
df$midasRisk <- sample(1:50, nrow(df), replace = TRUE)
df$homeroomName <- rep(names[sample(1:20), ], 50)
df$homeroomScore <- sample(1:100, prob = 1:100, nrow(df), replace = TRUE)
df$subjectRoomName <- rep(names[sample(11:30), ], 50)
df$subjectRoomScore <- sample(1:100, prob = 1:100, nrow(df), replace = TRUE)
df$selfReportScore <- sample(1:100, prob = 1:100, nrow(df), replace = TRUE)


write.csv(df, "./data/dummy_midas_data_1000.csv")

#'generate homeroomname, homeroomscore,
# subjectroomname, subjectscore, selfreportscore