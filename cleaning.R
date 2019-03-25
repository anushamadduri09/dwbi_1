#Read the file1 into a dataframe
mmt<-read.csv(file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\makemytrip_com-travel_sample.csv",stringsAsFactors=FALSE,header=TRUE,sep=",")
#Required columns from makemytrip file
mmt_col<-mmt[,c('city','country','hotel_overview','hotel_star_rating','latitude','longitude','mmt_review_score','property_name','query_time_stamp')]
#extracting date from timestamp column
mmt_col$query_time_stamp<-substr(mmt_col$query_time_stamp,1,10)
mmt_col$query_time_stamp<-as.Date(mmt_col$query_time_stamp, format="%Y-%m-%d")
#Removing the duplicates according to recent date
mmt_col<-mmt_col%>%group_by(property_name,latitude,longitude) %>% filter(query_time_stamp == max(query_time_stamp))
#write the file into csv
write.csv(mmt_col,file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\mmt_remdup1.csv")
#Read the file2 into a dataframe
goibibo<-read.csv(file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\goibibo_com-travel_sample.csv",header=TRUE,sep=",")
#Required columns from goibibo file
goibibo_col<-goibibo[,c('additional_info','city','country','latitude','longitude','point_of_interest','property_name','property_type','room_count','room_type','site_review_rating','qts','state')]
#extracting date from timestamp column
goibibo_col$qts<-substr(goibibo_col$qts,1,10)
goibibo_col$qts<-as.Date(goibibo_col$qts,format="%Y-%m-%d")
#Removing the duplicates according to recent date
goibibo_col<-goibibo_col%>%group_by(property_name,latitude,longitude) %>% filter(qts == max(qts))
#write the file into csv
write.csv(goibibo_col,file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\goibibo1.csv")
#Merging two datasets with common columns
mmt_goibibo<-merge(goibibo_col,mmt_col,by.x=c("property_name","city"),by.y=c("property_name","city"))
#Removing Duplicate columns
drops <- c("latitude.y","longitude.y","country.y","qts","query_time_stamp")
mmt_goibibo<-mmt_goibibo[ , !(names(mmt_goibibo) %in% drops)]
#Removing Duplicate rows
mmt_goibibo<-distinct(mmt_goibibo,property_name,city,.keep_all = TRUE)
#Predicting values with median for rating
mediangoibibo<-mmt_goibibo$site_review_rating
mediangoibibo[is.na(mediangoibibo)] <- median(mediangoibibo, na.rm = TRUE)
mmt_goibibo$site_review_rating<-mediangoibibo
#sapply(df, function(x) sd(x, na.rm=T))
medianmmt<-mmt_goibibo$mmt_review_score
medianmmt[is.na(medianmmt)] <- median(medianmmt, na.rm = TRUE)
mmt_goibibo$mmt_review_score<-medianmmt
#Converting point of interest column into number of point of interest
#mmt_goibibo<-read.csv(file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\mmt_goibibo.csv",stringsAsFactors=FALSE,header=TRUE,sep=",")
pi<-as.character(mmt_goibibo$point_of_interest)
pi<-lengths(strsplit(pi,"[|]")) 
mmt_goibibo$point_of_interest<-pi
#Checking about airport in hotel overview column
mmt_goibibo$hotel_overview<-as.character(mmt_goibibo$hotel_overview)
mmt_goibibo$hotel_overview<-grepl("airport|Airport",mmt_goibibo$hotel_overview)
mmt_goibibo$hotel_overview<-as.integer(as.logical(mmt_goibibo$hotel_overview))
#colnames(mmt_goibibo)[14]<-"Existence_of_Airport"
#setting default facility for null values in hotel facilities
mmt_goibibo$additional_info[mmt_goibibo$additional_info==""] <- "Room Service"
#uniform values in hotelstar rating column
mmt_goibibo$hotel_star_rating<-substr(mmt_goibibo$hotel_star_rating,1,1)
#Cleaning Facilities column
data<-mmt_goibibo$additional_info
data<-gsub("  "," ",data)
data<-gsub("[|]",",",data)
data<-gsub("/",",",data)
write.csv(data,file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\data.csv")
data<-read.csv(file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\data.csv",stringsAsFactors=FALSE,header=TRUE,sep=",")
data$Free_Internet<-grepl("Free Internet",data$x)
data$Free_Internet<-as.integer(as.logical(data$Free_Internet))
data$Gym<-grepl("Gym",data$x)
data$Gym<-as.integer(as.logical(data$Gym))
data$Internet_Access<-grepl("Internet Access",data$x)
data$Internet_Access<-as.integer(as.logical(data$Internet_Access))
data$Restaurant<-grepl("Restaurant",data$x)
data$Restaurant<-as.integer(as.logical(data$Restaurant))
data$Room_Service<-grepl("Room Service",data$x)
data$Room_Service<-as.integer(as.logical(data$Room_Service))
data$Spa<-grepl("Spa|spa",data$x)
data$Spa<-as.integer(as.logical(data$Spa))
data$Swimming_Pool<-grepl("Swimming Pool",data$x)
data$Swimming_Pool<-as.integer(as.logical(data$Swimming_Pool))
data<-data[,c('Free_Internet','Gym','Internet_Access','Restaurant','Room_Service','Spa','Swimming_Pool')]
write.csv(data,file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\data.csv")
data<-read.csv(file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\data.csv")
mmt_goibibo<-qpcR:::cbind.na(mmt_goibibo,data)
#write the file into csv
write.csv(mmt_goibibo,file="C:\\Users\\MOLAP\\Desktop\\Project_Hotel\\mmt_goibibo.csv")


