apresanalysis <- NULL
fullout<-NULL

adjusteddataframe <- read.csv("data/produced/ts3fits.csv") 


dtpoints<-adjusteddataframe %>% group_by(lname) %>%
  summarise(n=n())

enoughdata<-filter(dtpoints, n > 50)

adjusteddataframe2 <- filter(adjusteddataframe ,lname %in% enoughdata$lname)
ns2<-c()
for (i in unique(adjusteddataframe2$lname)){
  
  thisdata <- filter(adjusteddataframe2, lname==i )
  
  thisdata<-mutate(thisdata, laglog= lag(logcase))
  
  lmod<-lm(logcase ~date + laglog  ,data=thisdata)
  
  smod<-segmented(lmod,~date)
  keyout<-summary(smod)
  
  a<-slope(smod)
  ns<-a$date[2,1]
  
  ns2<-c(ns2,ns) # gets other slope
  key1<-smod
  coefs<-key1$coefficients
  breakdate<-key1$psi[2]
  pdate<-keyout$coefficients[2,4]
  r2<-keyout$r.squared
  
  outs<-c(coefs,pdate,breakdate,r2)
  
  # plot in folder
  png(paste0("plots/",i,"fit",".png"))
  plot(smod,ylim=c(0,3))
  points(thisdata$date,thisdata$logcase)
  dev.off()
  
  thisdata<-cbind.data.frame(thisdata, outs[2],outs[4],outs[6],outs[7],outs[8])
  names(thisdata)[36:40]<-c("part1coef","diffpart2coef","pval","breakpoint","r2")
  
  apresanalysis<-rbind(apresanalysis,thisdata)
  fullout<-rbind(fullout,outs)
  
  
}


write.csv(apresanalysis,"data/produced/brokensticksheet.csv")    


## now fit the averages  ## 

apresanalysis<-group_by(apresanalysis,lname) %>% mutate(loc = ifelse(date > ceiling(breakpoint),1,0))
apresanalysis<-ungroup(apresanalysis)

apresanalysis<-apresanalysis %>% rowwise() %>% mutate(Avg=mean(c(ret_la,food_la,tran_la,work_la,resid_la), na.rm=T),
                                                      Avg2=mean(c(ret_la,food_la,tran_la,work_la,resid_la,park_la),na.rm=T)) 
apresanalysis <- mutate(apresanalysis, relativpark = park_la)
parktable<-apresanalysis %>% group_by(lname,loc) %>%
  summarise(meanrelativeparkuse = mean(relativpark),
            meanrelativemob =mean(Avg2),
            meanrelativenotpark =mean(Avg),
            meantempperiod =mean(mean_temperature),
            meanrainperiod = mean(total_rainfall))

parktable<-cbind.data.frame(parktable,ns2)
z1<-dplyr::select(parktable, lname,meanrelativeparkuse,meanrelativemob,meanrelativenotpark,loc)

z11<- filter(z1,loc==0)
z22<-  filter(z1,loc==1)
names(z11)<-c("lname","meanrelpark1","meanrelmob1","meanrelnotpark1","loc")
names(z22)<-c("lname","meanrelpark2","meanrelmob2","meanrelnotpark2","loc")


z11<-z11[,1:4]
z22<-z22[,1:4]


## now tidyver was loaded in from Toms sheet

pb2<- filter(prebreak, lname %in% tidyver$name)
tidyver$lname <- pb2$lname # remove trouble row
td.1<-left_join(tidyver,z11,by="lname")
td.2<-left_join(td.1,z22,by="lname")
tidyver2<-td.2

# join slope
tidyver2<-dplyr::select(tidyver2, -slope2) # may have to remove old slope2
z3<- dplyr::select(parktable,lname,ns2)
names(z3)<-c("lname","slope2")
tidyver2<-left_join(tidyver2,z3,by="lname")

##########

# addition of totals
parktable2<-apresanalysis %>% group_by(lname) %>%
  summarise(meanrelparktotal = mean(relativpark),
            meanrelmobtotal =mean(Avg2),
            meanrelnotparktotal =mean(Avg))

parktable2<-mutate(parktable2,parkpreftotal = meanrelparktotal-meanrelnotparktotal)

names(parktable2)[1] <-c("name")

tidyver2<-left_join(tidyver2,parktable2)

tidyver2<-mutate(tidyver2, parkpref1 = meanrelpark1-meanrelnotpark1,
                 parkpref2= meanrelpark2-meanrelnotpark2)
write.csv("data/produced/tidyver2.csv")