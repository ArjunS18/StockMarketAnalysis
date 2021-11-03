library(dplyr) 

getwd()

#read and view csv file in df
df <- read.csv("CRSP_2010_2021.csv")

df['market_cap'] <- abs(df['PRC']) * df['SHROUT']
View(df)

#clean and filter  data
vecpri <- c('A','N','Q')
vecshrcd <- c(10, 11)
fil <- filter(df, SHRCD %in% vecshrcd & PRIMEXCH %in% vecpri)
fil['market_cap'] <- abs(fil['PRC']) * fil['SHROUT']
View(fil)

#remove scientific notation
options(scipen = 999)

#filter date
datedf <- filter(fil, date %in% 20201231)
View(datedf)

#Sort data in descending order and get top 20 companies
descdate <- datedf %>% arrange(desc(market_cap))
top20 <- head(descdate, 20)
write.csv(top20,'top20.csv', row.names = TRUE)

#check and replace NA values
any(is.na(fil))
fil[is.na(fil)] <- 0
fil[fil==''] = 0
fil[fil=='C'] = 0
fil<- na.omit(fil)
View(fil)

#check type and change to numeric
typeof(fil['RET'])
fil['RET'] <- as.numeric(unlist(fil['RET']))

#group by date and find the mean of RET column
Ret_avg <- 
  fil %>% 
  group_by(date) %>% 
  summarise(RET.ew = mean(RET))
Ret_avg$RET.ew = Ret_avg$RET.ew * 100

#write the data to csv file in working directory
write.csv(Ret_avg,"Ret_ew.csv")
View(Ret_avg)

#lag the market cap by 1 month for each firm
me_lag_df <- 
  fil %>%
  group_by(TICKER) %>%
  mutate('ME.lag' = dplyr::lag(market_cap, n = 1, default = 0))
View(me_lag_df)


#check and change data type to numeric to perform calculations
typeof(data$RET)
me_lag_df$RET <-as.numeric(unlist(me_lag_df$RET))
Ret_vw_df <- 
  me_lag_df %>% 
  group_by(date) %>% 
  summarise(RET.vw = weighted.mean(RET, ME.lag))

Ret_vw_df$RET.vw = Ret_vw_df$RET.vw * 100
View(Ret_vw_df)
#truncate last 2 digits in date and make it a yyyymm format dataframe
Ret_vw_df$date <-as.numeric(unlist(Ret_vw_df['date']/100))
Ret_vw_df$date <- signif(Ret_vw_df$date,digits=6)

french_data_df <- read.csv("MKT french data.csv", skip = 3)
french_data_df['Mkt'] = french_data_df['Mkt.RF'] + french_data_df['RF']
#filter data based on date
fil_french <- french_data_df %>% filter(Date>=201001)
View(fil_french)
names(fil_french)[1] <- 'date' 
Res_df <- merge(fil_french, Ret_vw_df, by='date')

#write data to csv file
write.csv(fil_french, 'french_data_out.csv', row.names = TRUE)

#compute mean and correlation among stocks
mean(as.numeric(unlist(fil_french$Mkt)))
mean(as.numeric(unlist(Ret_vw_df$RET.vw)))
cor(fil_french$Mkt, Ret_vw_df$RET.vw)
 
show(getwd())
write.csv(Ret_vw_df,"Ret_vw.csv", row.names = TRUE)


#debug and delete unnecessary data frames
rm(fil)

rlang::last_error()
rlang::last_trace()
warnings()

