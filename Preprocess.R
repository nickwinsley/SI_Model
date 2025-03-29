library(lubridate)
library(dplyr)
setwd("C:\\Users\\nickw\\Downloads\\SI_Model")

cc.data = read.csv("covidcard/nz_latest_extract.csv", stringsAsFactors = FALSE)
cc.time = read.csv("covidcard/date_time.csv", stringsAsFactors = FALSE)

cc.data$ExposureDateTime = as_datetime(cc.time$ExposureDateTime)

real.ids = read.csv("covidcard/card_ids_for_analysis.csv", stringsAsFactors = FALSE)

cc.data$date_stamp = as.Date(cc.data$ExposureDateTime)

trial_days <- c("2020-11-09","2020-11-10","2020-11-11", "2020-11-12","2020-11-13","2020-11-14", "2020-11-15")

cc.data$date_stamp_char <- as.character(cc.data$date_stamp)


cc.data$UploadDateTime <- as_datetime(cc.time$UploadDateTime)
cc.data$upload_date <- as.Date(cc.data$UploadDateTime)
cc.data$upload_date_char <- as.character(cc.data$upload_date)
print(colnames(cc.data))

cc.data.sub = cc.data %>% filter(date_stamp_char %in% trial_days)
class(cc.data.sub)
cc.data.sub <- cc.data.sub %>% filter(!(upload_date_char == "2020-11-15" & date_stamp_char == "2020-11-15"))

cc.data.sub <- cc.data.sub %>% filter((ObservingQRCode %in% real.ids$card_id) & (ObservedQRCode %in% real.ids$card_id))

IDs = sort(unique(c(cc.data.sub$ObservingQRCode, cc.data.sub$ObservedQRCode)))

n = length(IDs)

senders = match(cc.data.sub$ObservingQRCode, IDs)

receivers = match(cc.data.sub$ObservedQRCode, IDs)

for (i in 1:n) {
  cc.data.sub <- mutate(cc.data.sub, ObservingQRCode = ifelse(senders == i, i, ObservingQRCode))
  cc.data.sub <- mutate(cc.data.sub, ObservedQRCode = ifelse(receivers == i, i, ObservedQRCode))
}

write.csv(cc.data.sub, file = "Card Data.csv", row.names = FALSE)



