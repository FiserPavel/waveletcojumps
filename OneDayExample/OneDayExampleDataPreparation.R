# preparing data data
library(dplyr)

BN_5min <- readRDS("BN_5min_07_17_int.rds")
BX_5min <- readRDS("BX_5min_07_17_int.rds")

BN_1min <- readRDS("BN_1min_07_17_int.rds")
BX_1min <- readRDS("BX_1min_07_17_int.rds")

BN_1min$lClose <- as.vector(BN_1min$lClose)
BN_1min$dlClose <- as.vector(BN_1min$dlClose)
BX_1min$lClose <- as.vector(BX_1min$lClose)
BX_1min$dlClose <- as.vector(BX_1min$dlClose)

BN_5min$lClose <- as.vector(BN_5min$lClose)
BN_5min$dlClose <- as.vector(BN_5min$dlClose)
BX_5min$lClose <- as.vector(BX_5min$lClose)
BX_5min$dlClose <- as.vector(BX_5min$dlClose)

intradayIndex <- 113
BNday_5min <- BN_5min %>% filter(intraday == intradayIndex)
BXday_5min <- BX_5min %>% filter(intraday == intradayIndex)
BNday_1min <- BN_1min %>% filter(intraday == intradayIndex)
BXday_1min <- BX_1min %>% filter(intraday == intradayIndex)

vars <- c("Hour","Min","Close","lClose","dlClose")

BNday_5min <- BNday_5min %>% dplyr::select(vars)
BXday_5min <- BXday_5min %>% dplyr::select(vars)

BNday_1min <- BNday_1min %>% dplyr::select(vars)
BXday_1min <- BXday_1min %>% dplyr::select(vars)

saveRDS(BNday_5min, "BNdayExample_5min.rds")
saveRDS(BXday_5min, "BXdayExample_5min.rds")
saveRDS(BNday_1min, "BNdayExample_1min.rds")
saveRDS(BXday_1min, "BXdayExample_1min.rds")
