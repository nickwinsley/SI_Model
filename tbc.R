library(TNC)
library(dplyr)

adjMats = list()

df <- read.csv("Card Data Cleaned.csv", stringsAsFactors = FALSE)

times = numeric(nrow(df))

for (i in 1:nrow(df)){
  row <- unlist(as.vector(df[i, ]))
  date <- row[4]
  observing <- as.integer(row[2])
  observed <- as.integer(row[3])
  
  date <- as.Date(date, format = "%d/%m/%Y %H:%M")
  
  if (is.na(date)) {
    date <- row[4]
    date <- as.Date(date, format = "%d/%m/%Y")
  }
  
  time <- as.integer(date)
  
  times[i] = time
}

df$time = times

df <- arrange(df, time)


identity = diag(1, nrow = 751, ncol = 751)


prev = 0

adjMat = identity

for (i in 1:nrow(df)) {
  row <- unlist(as.vector(df[i, ]))
  date <- row[4]
  observing <- as.integer(row[2])
  observed <- as.integer(row[3])
  
  date <- as.Date(date, format = "%d/%m/%Y %H:%M")
  
  if (is.na(date)) {
    date <- row[4]
    date <- as.Date(date, format = "%d/%m/%Y")
  }
  
  time <- as.integer(date)
  
  if (time != prev){
    prev = time
    adjMats[[length(adjMats) + 1]] = adjMat
    adjMat = identity
  }
  
  adjMat[observing, observed] = 1
  adjMat[observed, observing] = 1
}

tbc_final <- tbc(adjMats, type = "M")

output <- data.frame(Node = 1:751, tbc = tbc_final)

write.csv(output, "tbc.csv", row.names = FALSE)