
# cleaning stage
tsdat2 <-readRDS("data/produced/data.rds") # TOMs rds file
tsdat2<-tsdat2[[2]]


# gives all names of places with mobiltyd data 
mobdata<-tsdat2 %>% group_by(name) %>% filter(!is.na(park_reg)) %>%
  summarise(dtcount = n())

#1 ) 
tsdatafiltered <- filter(tsdat2,name %in% mobdata$name)


# 2) add case mark
tsdatafiltered<-tsdatafiltered %>% group_by(name) %>%
  mutate(case2=ifelse(case==0,0,1),
         logcase =log10(case+0.1))

# get rid of NAs above 180
tsdatafiltered<-mutate(tsdatafiltered, logcase=ifelse(date >= 180,NA,logcase))
tsdatafiltered<-mutate(tsdatafiltered, case=ifelse(date >= 180,NA,case))



adjusteddataframe<- NULL
for (j in unique(tsdatafiltered$name)[-c(60,161,162,175,216,267,277,295)]) {
  
  # select location
  # j <- 23
  thisdat<- filter(tsdatafiltered,name== j )
  
  consec <- c()
  for(i in 1:(nrow(thisdat)-6)){
    
    dt<-thisdat[i:(i+6),]
    conday<-sum(dt$case2)
    consec<-c(consec,conday)
  }
  
  simpcon<-ifelse(consec > 5,1,0)
  ifelse(any(simpcon[match(1,simpcon):length(simpcon)] ==0),
         # I'm sorry
         thisdat2<-thisdat[match(1,simpcon):(match(1,simpcon)+match(0,simpcon[match(1,simpcon):length(simpcon)])),],
         thisdat2<-thisdat[match(1,simpcon):length(simpcon),])
  
  #  plot(thisdat2$date,thisdat2$logcase)        
  
  # filter any extra NAs
  thisdat2<- filter(thisdat2,!is.na(park_la) )%>%
    filter(!is.na(logcase)) 
  
  adjusteddataframe<-rbind(adjusteddataframe,thisdat2)
  
}

# data match names 
jnamematch<-read.csv(file.choose())
names(jnamematch)[c(2,7)] <- c("lname","name")
jnamematch<-jnamematch[,c(2,7)]

adjusteddataframe<- left_join(adjusteddataframe,jnamematch,by="name")

write.csv(adjusteddataframe,file="daat/produced/tsfits3.csv")



