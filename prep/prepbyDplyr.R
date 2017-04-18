library(dplyr)
library(tidyr)
library(data.table)

fare <- read.csv("/Users/jingyu/Downloads/t_f.csv",stringsAsFactors=FALSE)
data <- read.csv("/Users/jingyu/Downloads/t_d.csv",stringsAsFactors=FALSE)
data.dt <- data.table(data)
fare.dt <- data.table(fare)
# How many persons
cnt_persion.dt <- data.dt %>% 
  filter(!is.na(passenger_count)) %>% 
  group_by(passenger_count) %>% 
  summarise(n = n())

# Payment_type
cnt_payment_type.dt <- fare.dt %>% 
  filter(!is.na(payment_type)) %>% 
  group_by(payment_type) %>% 
  summarise(n = n())

# fare_amount
cnt_payment_type.dt <- fare.dt %>% 
  filter(!is.na(fare_amount)) %>% 
  group_by(fare_amount) %>% 
  summarise(n = n())

# tip_amount
cnt_tip_amount.dt <- fare.dt %>% 
  filter(!is.na(tip_amount)) %>% 
  group_by(tip_amount) %>% 
  summarise(n = n())

# total_amount
cnt_total_amount.dt <- fare.dt %>% 
  filter(!is.na(total_amount)) %>% 
  group_by(total_amount) %>% 
  summarise(n = n())

# top 5 busiest hours 
cnt_busiest_hour.dt <- data.dt %>% 
  filter(!is.na(pickup_datetime)) %>% 
  mutate(hour_of_day = hour(as.POSIXct(strptime(pickup_datetime, "%Y-%m-%d/ %H:%M:%S"))))  %>% 
  top_n(50) %>%  group_by(hour_of_day) %>% 
  summarise(n = n()) 

poa_longlat<-as.data.frame(poa_longlat_vec)
zipcodedbf <- zipcode_shp@data
zipcode <- zipcodedbf[1:2513,1]




#tidyr united
#test %>% unite(lonlat,lon, lat, sep = ",", remove = FALSE)

output = apply(expand.grid(poa_longlat_vec[1:10,],poa_longlat_vec[1:10,]),1, function(x1,x2) gcd.slc(x1[1],x1[2]))

lonlat1 = as.data.frame(poa_longlat_vec[1:3,])
lonlat2 = as.data.frame(poa_longlat_vec[1:3,])
colnames(lonlat1) = c("lon1","lat1")
colnames(lonlat2) = c("lon2","lat2")
dis_df = merge(test,test1,all=TRUE)
distance = apply(dis_calulation, 1, function(x1,x2,y1,y2) gcd.slc(x1[1],x1[2],x1[3], x1[4]))
result = cbind(dis_df, distance)
#testlyr <- cross_join(test,test) %>%  select(lon, lat)
#output = apply(expand.grid(poa_longlat[1:10,],poa_longlat_vec[1:10,]),1, function(x1,x2) gcd.slc(x1[1],x1[2]))

zip_matrix<- rdist.earth.vec(poa_longlat, poa_longlat, miles = FALSE, R = NULL)
diag(zip_matrix) <- 0
zipcode_geo = cbind(as.character(zipcode), zip_matrix)
zip.dt<-data.table(zipcode_geo)
setnames(zip.dt, 2:2514, as.vector(zipcode))
write.csv(zip.dt, file = "/Users/jingyu/Downloads/1270055003_poa_2011_aust_shape/MyData.csv",row.names=TRUE, col.names = TRUE)
