df <- as.data.table(df)
df[,Time:=as.POSIXct(Time,tz = 'UTC')+dhours(2)]
df %>% 
  summarise(min(Time),max(Time))
df %>%
  #arrange(MAC) %>% 
  slice(1:100) %>% 
  select(MAC,Time) %>% 
  mutate(hour_vol=floor_date(Time,unit="hours"),
         n_hours=n_distinct(hour_vol))


df_devices <- df %>% 
  mutate(hour_vol=floor_date(Time,unit="hours"),
         n_hours=n_distinct(hour_vol)) %>%
  group_by(hour_vol,n_hours) %>%
  distinct(MAC,.keep_all = TRUE) %>% 
  ungroup() %>% 
  count(MAC,n_hours,sort = TRUE) %>%
  filter(n >(n_hours*0.9)) 

oui <- read_csv("D:\\BlockbyBlock\\Block_by_block\\oui.csv")


vendors <- oui %>% 
  rename(name_vendor=vendor,id_oui=OUI) %>% 
  mutate(description="") %>% 
  distinct(id_oui,.keep_all = TRUE) %>% 
  select(name_vendor,description,id_oui)

stationary_devices <- toupper(c("Intel","TPLINK","NETGEAR","Cisco","D-Link","DLink","Sonos","AzureWave","Belkin","Jensen","TomTom","XEROX"))



df_small <- df %>%  
  filter(!MAC %in% df_devices$MAC) %>%   
  filter(str_length(MAC)== 17) %>% 
  get_oui() %>% 
  left_join(vendors,by=c('oui'='id_oui')) %>%
  mutate(name_vendor=ifelse(is.na(name_vendor),"unknown",name_vendor)) %>% 
  mutate(name_vendor=str_replace(name_vendor,"D-Link.*","DLink")) %>% 
  mutate(vendor_name=word(name_vendor,1,sep = "[^[:alpha:]]+"),
         vendor_name=str_replace(toupper(vendor_name),"[:punct:]","")) %>%
  filter(!vendor_name %in% stationary_devices)


df_small_min <- df_small %>% 
  mutate(min_vol=floor_date(Time,unit = 'mins')) %>%
  distinct(MAC,min_vol,.keep_all = TRUE) %>% 
  global_local() %>% 
  arrange(MAC,Time) %>% 
  as.data.table()

df_small_min[,Time := Time + dhours(2)]

df_small_min[,dates := as_date(Time)]

df_small_min[,.(min(Time),max(Time))]

df_small_min %>% names()

df_small_min <- df_small_min[dates %between% c('2019-06-14','2019-06-20')]

df_small_min %>% 
  distinct(MAC,.keep_all = TRUE) %>% 
  #sample_frac(size = 0.001) %>% 
  global_local() %>% 
  count(gl_loc)



df_small_min <- df_small_min %>% 
  global_local() %>% 
  filter(gl_loc == 'global') %>% 
  as.data.table()

setorder(df_small_min,MAC,Time)

df_small_min[,`:=`(Time_lag1=shift(Time, type = "lag", n = 1)),by='MAC']

df_small_min[,Time_300 := Time_lag1+dseconds(300)] 

df_small_min %>% glimpse()

df_small_min[,`:=`(missing_300=Time_300<Time),by=MAC]

df_small_min[is.na(missing_300),missing_300 := FALSE]

df_small_min[,visits := cumsum(missing_300),by=c('MAC','dates')]

df_small_min[,stay := difftime(max(Time),min(Time),units = 'secs'),
             by=.(MAC,dates,visits)]

df_small_min[,stay := as.numeric(round(stay,0))]

df_small_min[,timepart := format(as.POSIXct(Time),format='%T')][order(MAC,Time)]


df_small_min[,timepart := hms::parse_hms(timepart)]

df_small_min[,days_seen := n_distinct(dates),by='MAC']

df_dt <- df_small_min %>% 
  day_split() %>% 
  as.data.table()

workday_s <- hms::parse_hms("06:00:00")

workday_e <- hms::parse_hms("20:00:00")

df_dt[,activity := 0L]

df_dt[is.na(stay),'stay'] 

df_dt[,activity := if_else(
  stay > 18000 & days_seen >= 5 & !timepart %between% c(workday_s,workday_e),1L,#live
  if_else(stay > 18000 & days_seen >= 3 & timepart %between% c(workday_s,workday_e),2L,#work
          if_else(stay <= 60 & days_seen >1,3L,#commuters
                  if_else(stay <=60 & days_seen == 1, 4L,#passer-by
                          if_else(activity != 5L,5L,activity#leisure
                          ))))),by='MAC'] 

df_dt[timepart %between% c(workday_s,workday_e),.N]

df_dt[str_detect(SSID,'Emmery.*'),.N,by=.(activity)]

df_dt %>% 
  filter(str_detect(SSID,'Emmery.*')) %>% 
  group_by(activity) %>% 
  summarise(n_distinct(MAC))

df_dt %>% 
  distinct(MAC,.keep_all = TRUE) %>% 
  global_local() %>% 
  count(gl_loc)

collected_data_test <- df_dt %>%
  #sample_frac(size = 0.0001,replace = FALSE) %>% 
  mutate(#id_coll_data="",#vector("integer",length =1L),
    #id_location=r_sample_integer(,x=2:3, name = "id_location"),
    id_oui=str_sub(MAC,  start = 1L,end = 8L),
    id_oui=toupper(str_replace_all(id_oui,":","")),
    date_created=as_date(Time),
    time_created=hms::hms(as.numeric(round_date(Time,"sec") - floor_date(round_date(Time,"sec"), "1 day"),unit="secs")),
    id_day=wday(as_date(Time)-1),
    src_resolved=str_extract(Source,'.*(?=_)')) %>%
  mutate(dest1=str_sub(Destination,start = 1L,end = 8L),
         dest2=str_sub(Destination,start = 10L,end = 17L)) %>%
  mutate(dest1hex=hex2dec(tolower(str_replace_all(dest1,":",""))),
         dest2hex=hex2dec(tolower(str_replace_all(dest2,":",""))),
         dest=str_c(dest1hex,dest2hex)) %>%
  mutate(mac1=str_sub(MAC,start = 1L,end = 8L),
         mac2=str_sub(MAC,start = 10L,end = 17L)) %>% 
  mutate(mac1hex=hex2dec(tolower(str_replace_all(mac1,":",""))),
         mac2hex=hex2dec(tolower(str_replace_all(mac2,":",""))),
         mac=str_c(mac1hex,mac2hex)) %>% 
  
  # select(-c(mac1,mac2,MAC,AP,ap1,ap2)) %>% 
  # mutate(mac1=dec2hex(mac1hex),
  #        mac2=dec2hex(mac2hex)) %>%
  rename(data_created='Time',dst='Destination',sn='SN',
         src='mac',ssid='SSID',id_out_activity= "activity") %>%
  select(data_created,
         date_created,time_created,id_day,id_period_day,src,src_resolved,dst,
         subtype,sn,ssid,id_oui,days_seen,stay,id_out_activity) 


df_dt %>% 
  summarise(min(Time),max(Time))
collected_data_test %>% 
 summarise(min(data_created),max(data_created))


collected_data_test$id_day <- as.integer(collected_data_test$id_day)

collected_data_test$id_location <- 1L


collected_data_test$id_in_activity <- 0L

collected_data_test$id_service <- 2L

collected_data_test$returning_customer <- 0L

collected_data_test$id_in_day_period <- 0L

collected_data_test$id_out_day_period <- collected_data_test$id_period_day

names(collected_data_test)

collected_data_test$id_out_activity %>% table()

collected_data_test$hours <-  hour(collected_data_test$time_created)

collected_data_test$stay <- as.numeric(collected_data_test$stay)

collected_data_test <- collected_data_test %>% 
  mutate(id_day_type= as.integer(if_else(id_day %in% c(6,7),2,1)))

collected_data <- collected_data_test %>% 
  select(id_location,
         data_created,
         date_created,
         time_created,
         id_day,
         id_in_day_period,
         id_out_day_period,
         src,
         src_resolved,
         dst,
         subtype,
         sn,
         ssid,
         id_oui,
         stay,
         id_in_activity,
         id_out_activity,
         hours,
         id_day_type,
         id_service,
         returning_customer)

names(collected_data)

collected_data %>% 
  summarise_at(vars(1:21),~class(.)[1]) %>% 
  gather(vars,value) %>% 
  knitr::kable() %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)

collected_data %>% 
  summarise(min(data_created),max(data_created))

# Write csv file emmery_out---------------------
write_csv(collected_data,'emmery_out.csv')
# Create df with global MACs only----------------

df_small %>% class()
df_dt_gl %>% 
  summarise(min(Time),max(Time))

df_gl <- df_small %>% 
  global_local() %>% 
  filter(gl_loc == 'global')

df_gl %>% glimpse()

df_dt_gl <- as.data.table(df_gl[,-c(15,16)])

df_dt_gl[,Time := Time + dhours(2)]

df_dt_gl[,dates := as_date(Time)]

df_dt_gl[,hour_vol := floor_date(Time,unit = 'hours')]

df_dt_gl[,weekday := wday(as_date(Time)-1)]

df_dt_gl[,.N,by="dates"]



df_dt_gl <- df_dt_gl[,timepart := format(as.POSIXct(Time),format='%T')][order(MAC,Time)]

df_dt_gl[,timepart := hms::parse_hms(timepart)]



workday_st <- hms::parse_hms("07:30:00")
workday_end <- hms::parse_hms("18:00:00")
workday_st_sat <- hms::parse_hms('08:00:00')
workday_end_sat <- hms::parse_hms('17:00:00')
workday_st_sun <- hms::parse_hms('08:00:00')
workday_end_sun <- hms::parse_hms('16:00:00')

df_dt_gl$emmery_w_h <- 'n'



df_dt_gl[,.(min(Time),max(Time))]

df_dt_x <- df_dt_gl[dates %between% c('2019-06-14','2019-06-20')] %>% 
  group_by(dates) %>% 
  mutate(emmery_w_h=if_else(weekday %in% c(1:5) & between(timepart,workday_st,workday_end),'y',
                            if_else(weekday == 6 & between(timepart,workday_st_sat,workday_end_sat),'y',
                                    if_else(weekday == 7 & between(timepart,workday_st_sun,workday_end_sun),'y',
                                            if_else(emmery_w_h != 'y','n',emmery_w_h))))) %>% 
  filter(emmery_w_h == 'y') %>%
  ungroup() %>% 
  select(-emmery_w_h) %>% 
  arrange(MAC) %>%
  as.data.table()

df_dt_x[,.(min(Time),max(Time)),by=.(dates)]

df_dt_unique <- df_dt_x %>%
  arrange(MAC,Time) %>% 
  group_by(MAC,hour_vol) %>% 
  distinct(MAC,.keep_all = TRUE) %>% 
  as.data.table()



df_dt_unique[,.N,by='dates']

df_dt_unique[,cons_hour := c(NA,diff(hour_vol)==1),by=c('MAC','dates')]

df_dt_unique[is.na(cons_hour),cons_hour := TRUE] 


df_dt_unique[,h_seq := sequence(rle(cons_hour)$lengths),by='MAC']

df_dt_unique[,seq_id := cumsum(c(1, diff(h_seq)) != 1),by=.(MAC,dates)]

# Define df with stay longer than 4 hours
df_grater_4 <- df_dt_unique %>% 
  group_by(MAC,dates,seq_id) %>% 
  mutate(duration=length(seq_id)) %>% 
  filter(duration > 4) %>% 
  ungroup()

df_grater_4 %>% 
  ggplot(aes(duration)) +
  geom_histogram(binwidth = 1)

# Filter out the stay longer than 4 hours
df_dt_less_4 <- df_dt_x %>% 
  anti_join(df_grater_4,by=c('MAC','hour_vol')) %>% 
  as.data.table()


setorder(df_dt_less_4,MAC,Time)

df_dt_less_4[,`:=`(Time_lag1=shift(Time, type = "lag", n = 1)),by='MAC']

df_dt_less_4[,Time_300 := Time_lag1+dseconds(300)] 

df_dt_less_4 %>% glimpse()

df_dt_less_4[,`:=`(missing_300=Time_300<Time),by=MAC]

df_dt_less_4[is.na(missing_300),missing_300 := FALSE]

df_dt_less_4[,visits := cumsum(missing_300),by=c('MAC','dates')]

df_dt_v <- df_dt_less_4[,`:=`(visit= any(.SD$Time >= Time[1]+dseconds(60) &
                                           .SD$Time < Time[1]+dseconds(300)),
                              end_t=max(Time),start_t=min(Time)),
                        by=.(MAC,dates,visits)][visit == TRUE,]

df_dt_v[,n_distinct(MAC),by=.(hour_vol)]

df_dt_v[,stay := difftime(end_t,start_t,units = 'secs'),by=.(MAC,dates,visits)]

df_dt_v[,stay := as.numeric(round(stay,0))]

df_dt_v[stay %between% c(0,300),n_distinct(MAC),by='hour_vol']

df_dt_v[stay < 300,.(cust=n_distinct(MAC)),by='hour_vol'] %>% 
  arrange(hour_vol) %>% 
  ggplot(aes(hour_vol,cust)) +
  geom_col() +
  facet_wrap(~ as_date(hour_vol),scales = 'free_x')


df_v <- df_dt_v %>% 
  group_by(MAC) %>% 
  distinct(MAC,start_t,end_t,.keep_all = TRUE) %>% 
  ungroup() %>% 
  as.data.table()

df_dt_v[,n_distinct(MAC)]

df_v %>% 
  summarise(n_distinct(MAC))

df_v %>% class()

df_v[,`:=`(days_seen=n_distinct(dates),n_visits=n_distinct(visits)),by=.(MAC)]

df_v[,n_visits_daily := n_distinct(visits),by=.(MAC,dates)]

df_v[,.N,by=.(n_visits_daily)]

df_v[,loyal_cust := n_distinct(dates),by=.(MAC)]

df_v <- df_v[n_visits < 11 & n_visits_daily < 4,]

df_v[,.N,by=.(days_seen)]

df_v %>% 
  filter(days_seen != 1 & n_visits != 1) %>%
  group_by(hour_vol) %>% 
  distinct(MAC,.keep_all = TRUE) %>% 
  count(dates,hour_vol) %>% View()

df_v[,returning_customer := if_else(days_seen == 1,1L,2L)]

df_v[returning_customer == 2,n_distinct(MAC),by=.(dates)]



midday_s <- hms::parse_hms('11:00:00')

midday_end <- hms::parse_hms('14:00:00')

df_v <- as.data.table(df_v)

df_v[,id_in_day_period := if_else(
  timepart < midday_s,1L,#morning
  if_else(timepart %between% c(midday_s,midday_end),2L,#Midday
          3L,#Afternoon
  )),by='MAC'] 

df_v[, hours := hour(timepart)]

df_v[,n_distinct(MAC),by='id_in_day_period']  

df_v[,id_out_day_period := 0L]

df_v[,id_out_activity := 0L]

df_v[,id_day_type := as.integer(if_else(weekday %in% c(6,7),2L,1L))]

df_v[,id_service := 1L]

df_v[,id_location := 1L]

df_v[,.(),by='MAC']

df_v[,.(n_visits=n_distinct(visits)),by=.(MAC,dates)][,hist(n_visits)]

df_v[,.N,by='dates']

df_v %>% 
  count(dates,MAC)

df_v$n_visits %>% table()

df_v$n_visits_daily %>% table()

df_v[,id_in_activity := if_else(
  stay > 600,2L,#Sit-down
  if_else(stay < 300,3L,#Take-away & Transaction 1-5min
          4L)),# Transaction 5-10min
  by='MAC'] 

df_v[,n_distinct(MAC),by=.(id_in_activity)]

df_v$timepart[1]

emmery_in <-df_v %>%
  #sample_frac(size = 0.0001,replace = FALSE) %>% 
  mutate(#id_coll_data="",#vector("integer",length =1L),
    #id_location=r_sample_integer(,x=2:3, name = "id_location"),
    id_oui=str_sub(MAC,  start = 1L,end = 8L),
    id_oui=toupper(str_replace_all(id_oui,":","")),
    date_created=as_date(Time),
    time_created= timepart,#hms::hms(as.numeric(round_date(Time,"sec") - floor_date(round_date(Time,"sec"), "1 day"),unit="secs")),
    id_day=as.integer(wday(as_date(Time)-1)),
    src_resolved=str_extract(Source,'.*(?=_)')) %>%
  mutate(dest1=str_sub(Destination,start = 1L,end = 8L),
         dest2=str_sub(Destination,start = 10L,end = 17L)) %>%
  mutate(dest1hex=hex2dec(tolower(str_replace_all(dest1,":",""))),
         dest2hex=hex2dec(tolower(str_replace_all(dest2,":",""))),
         dest=str_c(dest1hex,dest2hex)) %>%
  mutate(mac1=str_sub(MAC,start = 1L,end = 8L),
         mac2=str_sub(MAC,start = 10L,end = 17L)) %>% 
  mutate(mac1hex=hex2dec(tolower(str_replace_all(mac1,":",""))),
         mac2hex=hex2dec(tolower(str_replace_all(mac2,":",""))),
         mac=str_c(mac1hex,mac2hex)) %>% 
  
  # select(-c(mac1,mac2,MAC,AP,ap1,ap2)) %>% 
  # mutate(mac1=dec2hex(mac1hex),
  #        mac2=dec2hex(mac2hex)) %>%
  rename(data_created='Time',dst='Destination',sn='SN',
         src='mac',ssid='SSID') %>%
  select(id_location,data_created,date_created,time_created,
         id_day,id_in_day_period,id_out_day_period,src,src_resolved,
         dst,subtype,sn,ssid,id_oui,stay,id_in_activity,id_out_activity,hours,id_day_type,
         id_service,returning_customer)

# Write csv file emmery_in----------------
write_csv(emmery_in,'emmery_in.csv')

emmery_in <- read_csv('emmery_in.csv')

emmery_in %>% 
  count(id_in_day_period,date_created,returning_customer) %>% 
  ggplot(aes(factor(id_in_day_period),n,fill=factor(returning_customer))) +
  geom_col()

emmery_in %>% 
  distinct(src,.keep_all = TRUE) %>% 
  ggplot(aes(factor(returning_customer))) +
  geom_bar() +
  theme_economist()

emmery_in %>% 
  count(id_location)
  
emmery_in %>% 
  filter(returning_customer == 2) %>% 
  count(date_created)

df_v %>% 
  filter(returning_customer == 2) %>% 
  count(dates)
