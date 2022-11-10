# read old test data
df <- read.csv("./data/dummy_midas_data_1000.csv")


# Add generated MIDAS risk score
df$midasRisk <- sample(1:50, nrow(df), replace = TRUE)

df$homeroomId <-rep(sample(1:100), 10)
df$homeroomTeacherId <- rep(sample(1:100), 10)
df$homeroomScore <- sample(1:100, prob = 1:100, nrow(df), replace = TRUE)

df$subjectRoomId <- rep(sample(1:100), 10)
df$subjectRoomTeacherId <- rep(sample(1:100), 10)
df$subjectRoomScore <- sample(1:100, prob = 1:100, nrow(df), replace = TRUE)

df$selfReportScore <- sample(1:100, prob = 1:100, nrow(df), replace = TRUE)
df$subjectName <- rep(sample(c("Math", "English", "Science", "Programming", "Art"), replace = FALSE), 200)


write.csv(df, "./data/dummy_midas_data_1000.csv")

#'generate homeroomname, homeroomscore,
# subjectroomname, subjectscore, selfreportscore