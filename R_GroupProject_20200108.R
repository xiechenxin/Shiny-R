#import packages
library(dplyr)
library(tidyr)
library(haven)
library(tidyverse)
library(lubridate)

#read in tables
daily <- read_sas("C:\\Users\\rsharma3\\Documents\\GitHub\\RGroupProject\\RawDataIIUserDailyAggregation.sas7bdat")
pokerchip <- read_sas("C:\\Users\\rsharma3\\Documents\\GitHub\\RGroupProject\\RawDataIIIPokerChipConversions.sas7bdat")
demograph <- read_sas("C:\\Users\\rsharma3\\Documents\\GitHub\\RGroupProject\\RawDataIDemographics.sas7bdat")
internet <- read_sas("C:\\Users\\rsharma3\\Documents\\GitHub\\RGroupProject\\AnalyticDataInternetGambling.sas7bdat")

nrow(internet)
nrow(demograph)
nrow(pokerchip)
nrow(daily)

############################ modify internet table ##################################

#replace gender value
internet$GENDER[internet$GENDER == 0] <- "Female"
internet$GENDER[internet$GENDER == 1] <- "Male"
#replacing missing value with one of the options
internet$GENDER[which(is.na(internet$GENDER))] <- "Male"

#replace country code with the country name
unique(internet$COUNTRY)
internet$COUNTRY[internet$COUNTRY == 8] <- "Albania"
internet$COUNTRY[internet$COUNTRY == 20] <- "Andorra"
internet$COUNTRY[internet$COUNTRY == 32] <- "Argentina"
internet$COUNTRY[internet$COUNTRY == 36] <- "Australia"
internet$COUNTRY[internet$COUNTRY == 40] <- "Austria"
internet$COUNTRY[internet$COUNTRY == 56] <- "Belgium"
internet$COUNTRY[internet$COUNTRY == 60] <- "Bermuda"
internet$COUNTRY[internet$COUNTRY == 70] <- "Bosnia and Herzegovina"
internet$COUNTRY[internet$COUNTRY == 100] <- "Bulgaria"
internet$COUNTRY[internet$COUNTRY == 112] <- "Belarus"
internet$COUNTRY[internet$COUNTRY == 124] <- "Canada"
internet$COUNTRY[internet$COUNTRY == 156] <- "China"
internet$COUNTRY[internet$COUNTRY == 158] <- "Taiwan"
internet$COUNTRY[internet$COUNTRY == 170] <- "Colombia"
internet$COUNTRY[internet$COUNTRY == 188] <- "Costa Rica"
internet$COUNTRY[internet$COUNTRY == 191] <- "Croatia"
internet$COUNTRY[internet$COUNTRY == 196] <- "Cyprus"
internet$COUNTRY[internet$COUNTRY == 203] <- "Czech Republic"
internet$COUNTRY[internet$COUNTRY == 208] <- "Denmark"
internet$COUNTRY[internet$COUNTRY == 233] <- "Estonia"
internet$COUNTRY[internet$COUNTRY == 234] <- "Faroe Islands"
internet$COUNTRY[internet$COUNTRY == 246] <- "Finland"
internet$COUNTRY[internet$COUNTRY == 250] <- "France"
internet$COUNTRY[internet$COUNTRY == 268] <- "Georgia"
internet$COUNTRY[internet$COUNTRY == 276] <- "Germany"
internet$COUNTRY[internet$COUNTRY == 292] <- "Gibraltar"
internet$COUNTRY[internet$COUNTRY == 300] <- "Greece"
internet$COUNTRY[internet$COUNTRY == 304] <- "Greenland"
internet$COUNTRY[internet$COUNTRY == 348] <- "Hungary"
internet$COUNTRY[internet$COUNTRY == 352] <- "Iceland"
internet$COUNTRY[internet$COUNTRY == 372] <- "Ireland"
internet$COUNTRY[internet$COUNTRY == 376] <- "Israel"
internet$COUNTRY[internet$COUNTRY == 380] <- "Italy"
internet$COUNTRY[internet$COUNTRY == 392] <- "Japan"
internet$COUNTRY[internet$COUNTRY == 422] <- "Lebanon"
internet$COUNTRY[internet$COUNTRY == 428] <- "Latvia"
internet$COUNTRY[internet$COUNTRY == 438] <- "Liechtenstein"
internet$COUNTRY[internet$COUNTRY == 440] <- "Lithuania"
internet$COUNTRY[internet$COUNTRY == 442] <- "Luxembourg"
internet$COUNTRY[internet$COUNTRY == 470] <- "Malta"
internet$COUNTRY[internet$COUNTRY == 474] <- "Martinique"
internet$COUNTRY[internet$COUNTRY == 480] <- "Mauritius"
internet$COUNTRY[internet$COUNTRY == 484] <- "Mexico"
internet$COUNTRY[internet$COUNTRY == 492] <- "Monaco"
internet$COUNTRY[internet$COUNTRY == 496] <- "Mongolia"
internet$COUNTRY[internet$COUNTRY == 498] <- "Moldavia"
internet$COUNTRY[internet$COUNTRY == 504] <- "Morocco"
internet$COUNTRY[internet$COUNTRY == 528] <- "Holland"
internet$COUNTRY[internet$COUNTRY == 530] <- "Netherlands Antilles"
internet$COUNTRY[internet$COUNTRY == 554] <- "NewZealand"
internet$COUNTRY[internet$COUNTRY == 566] <- "Nigeria"
internet$COUNTRY[internet$COUNTRY == 574] <- "Norfolk Island"
internet$COUNTRY[internet$COUNTRY == 578] <- "Norway"
internet$COUNTRY[internet$COUNTRY == 612] <- "Pitcairn"
internet$COUNTRY[internet$COUNTRY == 616] <- "Poland"
internet$COUNTRY[internet$COUNTRY == 620] <- "Portugal"
internet$COUNTRY[internet$COUNTRY == 630] <- "Puerto Rico"
internet$COUNTRY[internet$COUNTRY == 642] <- "Romania"
internet$COUNTRY[internet$COUNTRY == 643] <- "Russian Federation"
internet$COUNTRY[internet$COUNTRY == 702] <- "Singapore"
internet$COUNTRY[internet$COUNTRY == 703] <- "Slovakia"
internet$COUNTRY[internet$COUNTRY == 704] <- "Vietnam"
internet$COUNTRY[internet$COUNTRY == 705] <- "Slovenia"
internet$COUNTRY[internet$COUNTRY == 710] <- "South Africa"
internet$COUNTRY[internet$COUNTRY == 724] <- "Spain"
internet$COUNTRY[internet$COUNTRY == 752] <- "Sweden"
internet$COUNTRY[internet$COUNTRY == 756] <- "Switzerland"
internet$COUNTRY[internet$COUNTRY == 760] <- "Syria"
internet$COUNTRY[internet$COUNTRY == 764] <- "Thailand"
internet$COUNTRY[internet$COUNTRY == 784] <- "United Arab Emirates"
internet$COUNTRY[internet$COUNTRY == 788] <- "Tunesia"
internet$COUNTRY[internet$COUNTRY == 792] <- "Turkey"
internet$COUNTRY[internet$COUNTRY == 804] <- "Ukraine"
internet$COUNTRY[internet$COUNTRY == 807] <- "FYR Macedonia"
internet$COUNTRY[internet$COUNTRY == 826] <- "United Kingdom"
internet$COUNTRY[internet$COUNTRY == 840] <- "USA"
internet$COUNTRY[internet$COUNTRY == 850] <- "United States Virgin Islands"
internet$COUNTRY[internet$COUNTRY == 858] <- "Uruguay"
internet$COUNTRY[internet$COUNTRY == 891] <- "Serbia and Montenegro"
internet$COUNTRY[internet$COUNTRY == 895] <- "Diego Garcia"
#replace the language code with language name
unique(internet$LANGUAGE)
internet$LANGUAGE[internet$LANGUAGE == 1] <- "English"
internet$LANGUAGE[internet$LANGUAGE == 2] <- "German"
internet$LANGUAGE[internet$LANGUAGE == 3] <- "Italian"
internet$LANGUAGE[internet$LANGUAGE == 4] <- "Spanish"
internet$LANGUAGE[internet$LANGUAGE == 5] <- "Swedish"
internet$LANGUAGE[internet$LANGUAGE == 6] <- "French"
internet$LANGUAGE[internet$LANGUAGE == 7] <- "Turkish"
internet$LANGUAGE[internet$LANGUAGE == 8] <- "Greek"
internet$LANGUAGE[internet$LANGUAGE == 9] <- "Polish"
internet$LANGUAGE[internet$LANGUAGE == 10] <- "Norwegian"
internet$LANGUAGE[internet$LANGUAGE == 11] <- "Danish"
internet$LANGUAGE[internet$LANGUAGE == 12] <- "Catalan"
internet$LANGUAGE[internet$LANGUAGE == 13] <- "Czech"
internet$LANGUAGE[internet$LANGUAGE == 14] <- "Hungarian"
internet$LANGUAGE[internet$LANGUAGE == 15] <- "Dutch"
internet$LANGUAGE[internet$LANGUAGE == 16] <- "Portuguese"
internet$LANGUAGE[internet$LANGUAGE == 17] <- "Russian"

#create new variables
internet$FOBetSize <- internet$FOTotalStakes/internet$FOTotalBets
internet$FOProfits <- internet$FOTotalStakes-internet$FOTotalWinnings
internet$LABetSize <- internet$LATotalStakes/internet$LATotalBets
internet$LAProfits <- internet$LATotalStakes-internet$LATotalWinnings
internet$FOFirstActiveWeekday <- weekdays(as.Date(internet$FOFirstActiveDate))
internet$FOLastActiveWeekday <- weekdays(as.Date(internet$FOLastActiveDate))
internet$LAFirstActiveWeekday <- weekdays(as.Date(internet$LAFirstActiveDate))
internet$LALastActiveWeekday <- weekdays(as.Date(internet$LALastActiveDate))
internet$ActiveInLast30Days <- (difftime(as.Date('2005-09-30') ,internet$FOLastActiveDate, units = c("days")) >=30) & (difftime(as.Date('2005-09-30') ,internet$LALastActiveDate, units = c("days")) >=30)


#replace missing values
names(which(sapply(internet, anyNA)))
sapply(internet, function(x)all(any(is.na(x))))
internet$FOTotalStakes[is.na(internet$FOTotalStakes)] <- 0
internet$FOTotalWinnings[is.na(internet$FOTotalWinnings)] <- 0
internet$FOTotalBets[is.na(internet$FOTotalBets)] <- 0
internet$FOTotalDaysActive[is.na(internet$FOTotalDaysActive)] <- 0
internet$LATotalStakes[is.na(internet$LATotalStakes)] <- 0
internet$LATotalWinnings[is.na(internet$LATotalWinnings)] <- 0
internet$LATotalBets[is.na(internet$LATotalBets)] <- 0
internet$LATotalDaysActive[is.na(internet$LATotalDaysActive)] <- 0
internet$FOBetSize[is.na(internet$FOBetSize)] <- 0
internet$FOProfits[is.na(internet$FOProfits)] <- 0
internet$LABetSize[is.na(internet$LABetSize)] <- 0
internet$LAProfits[is.na(internet$LAProfits)] <- 0
internet$FOFirstActiveWeekday[is.na(internet$FOFirstActiveWeekday)] <- 0
internet$FOLastActiveWeekday[is.na(internet$FOLastActiveWeekday)] <- 0
internet$LAFirstActiveWeekday[is.na(internet$LAFirstActiveWeekday)] <- 0
internet$LALastActiveWeekday[is.na(internet$LALastActiveWeekday)] <- 0
internet$ActiveInLast30Days[is.na(internet$ActiveInLast30Days)] <- FALSE

#new variable to track the most active users
internet$maxActiveDays <- pmax(internet$FOTotalDaysActive, internet$LATotalDaysActive)

names(which(sapply(internet, anyNA)))

######################## modify the dempgraph table ##############################

#remove dupicate columns
demograph$Country <- demograph$Language <- demograph$Gender <- NULL

#replace application ID
unique(demograph$ApplicationID)
demograph$ApplicationID[demograph$ApplicationID == 1] <- "BETANDWIN.COM"
demograph$ApplicationID[demograph$ApplicationID == 3] <- "BETANDWIN.DE"
demograph$ApplicationID[demograph$ApplicationID == 4] <- "WAP.BETANDWIN.COM"
demograph$ApplicationID[demograph$ApplicationID == 8] <- "BALLS_OF_FIRE"
demograph$ApplicationID[demograph$ApplicationID == 9] <- "BETEUROPE.COM"
demograph$ApplicationID[demograph$ApplicationID == 11] <- "CASINO.BETEUROPE.COM"
demograph$ApplicationID[demograph$ApplicationID == 14] <- "BETOTO.COM"
demograph$ApplicationID[demograph$ApplicationID == 15] <- "PLAYIT.COM"
demograph$ApplicationID[demograph$ApplicationID == 16] <- "CASINO.PLAYIT.COM"
demograph$ApplicationID[demograph$ApplicationID == 19] <- "WAP.BETANDWIN.DE"
demograph$ApplicationID[demograph$ApplicationID == 21] <- "BOF.PLAYIT.COM"
demograph$ApplicationID[demograph$ApplicationID == 22] <- "BOF.BETEUROPE.COM"
demograph$ApplicationID[demograph$ApplicationID == 23] <- "BETANDWIN_POKER"
demograph$ApplicationID[demograph$ApplicationID == 24] <- "BETANDWIN_CASINO"
demograph$ApplicationID[demograph$ApplicationID == 27] <- "LOTTERY.BETOTO.COM"
demograph$ApplicationID[demograph$ApplicationID == 30] <- "PLAYIT_POKER"
demograph$ApplicationID[demograph$ApplicationID == 31] <- "BETEUROPE_POKER"
demograph$ApplicationID[demograph$ApplicationID == 32] <- "BETOTO_POKER"
demograph$ApplicationID[demograph$ApplicationID == 33] <- "BETANDWIN_GAMES"
demograph$ApplicationID[demograph$ApplicationID == 36] <- "BETOTO_CASINO"
demograph$ApplicationID[demograph$ApplicationID == 38] <- "BETEUROPE_GAMES"
demograph$ApplicationID[demograph$ApplicationID == 42] <- "PLAYIT_GAMES"
names(demograph)[names(demograph) == "ApplicationID"] <- "Application"

#modify the date format
demograph$FirstPay <- as.Date(demograph$FirstPay, format = "%Y %m %d")
demograph$FirstAct <- as.Date(demograph$FirstAct, format = "%Y %m %d")
demograph$FirstSp <- as.Date(demograph$FirstSp, format = "%Y %m %d")
demograph$FirstCa <- as.Date(demograph$FirstCa, format = "%Y %m %d")
demograph$FirstGa <- as.Date(demograph$FirstGa, format = "%Y %m %d")
demograph$FirstPo <- as.Date(demograph$FirstPo, format = "%Y %m %d")

#create new variables
demograph$daydiff_FistPay_RegDate <- difftime(demograph$FirstPay ,demograph$RegDate, units = c("days"))
demograph$daydiff_FistPay_RegDate <- round(demograph$daydiff_FistPay_RegDate,digits = 0)
demograph$daydiff_FistAct_FirstPay <- difftime(demograph$FirstAct ,demograph$FirstPay, units = c("days"))
demograph$FirstPayWeekday <- weekdays(as.Date(demograph$FirstPay))
demograph$FirstActWeekday <- weekdays(as.Date(demograph$FirstAct))
demograph$FirstSpWeekday <- weekdays(as.Date(demograph$FirstSp))
demograph$FirstCaWeekday <- weekdays(as.Date(demograph$FirstCa))
demograph$FirstGaWeekday <- weekdays(as.Date(demograph$FirstGa))
demograph$FirstPoWeekday <- weekdays(as.Date(demograph$FirstPo))
demograph$RegDate <- NULL

#create a new variable calculating the games that each user played
demograph_games <- demograph[, c("UserID", "FirstSp", "FirstCa", "FirstGa", "FirstPo")]
demograph_games$FirstSp <- as.numeric(as.Date(demograph_games$FirstSp, origin = "1900-01-01"))
demograph_games$FirstCa <- as.numeric(as.Date(demograph_games$FirstCa, origin = "1900-01-01"))
demograph_games$FirstGa <- as.numeric(as.Date(demograph_games$FirstGa, origin = "1900-01-01"))
demograph_games$FirstPo <- as.numeric(as.Date(demograph_games$FirstPo, origin = "1900-01-01"))
demograph_games$FirstSp[!is.na(demograph_games$FirstSp)] <- 1
demograph_games$FirstCa[!is.na(demograph_games$FirstCa)] <- 1
demograph_games$FirstGa[!is.na(demograph_games$FirstGa)] <- 1
demograph_games$FirstPo[!is.na(demograph_games$FirstPo)] <- 1
demograph_games <- demograph_games %>% group_by(UserID) %>% summarise_all(sum)
demograph_games$num_games <- rowSums(demograph_games[, -1], na.rm = TRUE)
demograph_games <- demograph_games[, c("UserID", "num_games")]

demograph <- merge(demograph, demograph_games, by = "UserID",all.x = TRUE)

######################### modify daily table ####################################

#replace the productID with product name
daily$ProductID[daily$ProductID == 1] <- "Sports_book_fixed_odd"
daily$ProductID[daily$ProductID == 2] <- "Sports_book_live_action"
daily$ProductID[daily$ProductID == 3] <- "Poker_BossMedia"
daily$ProductID[daily$ProductID == 4] <- "Casino_BossMedia"
daily$ProductID[daily$ProductID == 5] <- "Supertoto"
daily$ProductID[daily$ProductID == 6] <- "Games_VS"
daily$ProductID[daily$ProductID == 7] <- "Games_bwin"
daily$ProductID[daily$ProductID == 8] <- "Casino_Chartwell"

#switch negative value to absolute
daily$Stakes <- abs(daily$Stakes)
daily$Winnings <- abs(daily$Winnings)
daily$Bets <- abs(daily$Bets)

#create aggregative tables
daily_sum <- daily
daily_sum$Date <- NULL
daily_sum$ProductID <- NULL
daily_sum <- aggregate(daily_sum,list(userid=daily_sum$UserID),sum)
daily_sum$UserID <- NULL
names(daily_sum)[names(daily_sum) == "Stakes"] <- "Daily_Stakes_SUM"
names(daily_sum)[names(daily_sum) == "Winnings"] <- "Daily_Winnings_SUM"
names(daily_sum)[names(daily_sum) == "Bets"] <- "Daily_Bets_SUM"

#add variables
daily_sum$Daily_Profits_SUM <- daily_sum$Daily_Stakes_SUM - daily_sum$Daily_Winnings_SUM
daily_sum$Daily_Stakes_size <- round(daily_sum$Daily_Stakes_SUM/daily_sum$Daily_Bets_SUM, 3)
daily_sum$Daily_Winnings_size <- round(daily_sum$Daily_Winnings_SUM/daily_sum$Daily_Bets_SUM, 3)
daily_sum$Daily_Profits_size <- round(daily_sum$Daily_Profits_SUM/daily_sum$Daily_Bets_SUM, 3)


####################### modify pokerchip table ###################################

#spread the transaction type
pokerchip$TransType[pokerchip$TransType == 124] <- "Pokerchip_Sell"
pokerchip$TransType[pokerchip$TransType == 24] <- "Pokerchip_Buy"
pokerchip_1 <- pokerchip %>%
  group_by(UserID) %>%   
  mutate(row_id=1:n()) %>% ungroup() %>%  
  spread(key=TransType, value=TransAmount) %>%
  select(-row_id)

#replace missing values
pokerchip_1$Pokerchip_Buy[is.na(pokerchip_1$Pokerchip_Buy)] <- 0
pokerchip_1$Pokerchip_Sell[is.na(pokerchip_1$Pokerchip_Sell)] <- 0
#round the numbers
pokerchip_1$Pokerchip_Buy <- round(pokerchip_1$Pokerchip_Buy, digits = 3)
pokerchip_1$Pokerchip_Sell <- round(pokerchip_1$Pokerchip_Sell, digits = 3)

#create aggregate table
pokerchip_2 <- pokerchip_1
pokerchip_2$TransDateTime <- NULL
pokerchip_3 <-  pokerchip_2 %>% group_by(UserID) %>% summarise_all(sum)
names(pokerchip_3)[names(pokerchip_3) == "Pokerchip_Sell"] <- "Pokerchip_Sell_SUM"
names(pokerchip_3)[names(pokerchip_3) == "Pokerchip_Buy"] <- "Pokerchip_Buy_SUM"

#create new variables
pokerchip_1$Pokerchip_Month <- months(as.Date(pokerchip_1$TransDateTime))
pokerchip_1$Pokerchip_Weekday <- weekdays(as.Date(pokerchip_1$TransDateTime))
pokerchip_1$Pokerchip_Time <- format(as.POSIXct(pokerchip_1$TransDateTime,format="%Y-%m-%d %H:%M:%S"),"%H")

#create new aggregate table
pokerchip_4 <-  pokerchip_1 %>% group_by(UserID) %>% summarise_all(max)
names(pokerchip_4)[names(pokerchip_4) == "Pokerchip_Sell"] <- "Pokerchip_Sell_MAX"
names(pokerchip_4)[names(pokerchip_4) == "Pokerchip_Buy"] <- "Pokerchip_Buy_MAX"
names(pokerchip_4)[names(pokerchip_4) == "Pokerchip_Month"] <- "Pokerchip_Month_MAX"
names(pokerchip_4)[names(pokerchip_4) == "Pokerchip_Weekday"] <- "Pokerchip_Weekday_MAX"
names(pokerchip_4)[names(pokerchip_4) == "Pokerchip_Time"] <- "Pokerchip_Time_MAX"

#merge the two aggregated tables
pokerchip_agg <- merge(pokerchip_3, pokerchip_4, 
                       by.x = "UserID", by.y = "UserID",
                       all = TRUE)
pokerchip_agg$TransDateTime <- NULL


#################### merge the four main table together ##########################
gambling_1 <- merge(internet, demograph, 
                    by.x = "USERID", by.y = "UserID",
                    all.x = TRUE)
gambling_2 <- merge(gambling_1, daily_sum, 
                    by.x = "USERID", by.y = "userid",
                    all.x = TRUE)
gambling_Final <- merge(gambling_2, pokerchip_agg, 
                        by.x = "USERID", by.y = "UserID",
                        all.x = TRUE)
nrow(gambling_Final)

#export datamart
write_csv(gambling_Final, "C:\\Users\\rsharma3\\Documents\\GitHub\\RGroupProject\\Marketing_Datamart_3.csv")
