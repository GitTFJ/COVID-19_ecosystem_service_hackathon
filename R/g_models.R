join = read.csv("data/produced/tidyver2.csv")


ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

# remove bad fits with R2<0.5
join<-filter(join,r2 >=0.5)

#######################################################################################
#### Model 1: Rate of increase in cases 
m1 = lm(ihs(slope1*100) ~
          meanrelmob1 + 
          parkpref1+
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpref1:green_km_pp+
          parkpref1:frag+
          parkpref1:address_garden_perc+
          green_km_pp:frag +
          meantempperiod+
          meanrainperiod+
          lt +
          ln ,data = join)
stp_m1 = stepAIC(m1, direction = "both", 
                 trace = FALSE)
summary(stp_m1)
vif(stp_m1)
densityplot(resid(stp_m1))

## save results
results_table <- data.frame(summary(stp_m1)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m1.csv", row.names=TRUE)

############ PLOTS

## 1 - meanrelmob1 (Relative mobility for each LA averaged across TP)
mydf <- ggpredict(stp_m1, terms = "meanrelmob1")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_1_rel_mob <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Relative mobility", breaks=seq(-40,0,10)) +
  scale_y_continuous(name="Rate of increase in new COVID-19 cases", breaks=seq(0.01,0.04,0.005)) +
  theme_classic() +
  theme(text = element_text(size=14))
cases_1_rel_mob

ggsave(file="plots/cases_1_relmob.png", cases_1_rel_mob, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

# 2 - interaction between park use and garden amount 
# good effect of park if there are not many gardens
mydf <- ggpredict(stp_m1, terms = c("parkpref1","address_garden_perc"))
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases1_park_garden <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(aes(color=group), lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=group), alpha = .1) +
  scale_x_continuous(name="Preference of parks") +
  scale_y_continuous(name="Rate of increase in new COVID-19 cases") +
  theme_classic() +
  theme(text = element_text(size=8)) +
  scale_color_discrete(name = "Addresses with\n gardens (%)", labels = c("Low", "Medium", "High")) +
  scale_fill_discrete(guide=FALSE) +
  theme(legend.position="bottom")
cases1_park_garden
ggsave(file="plots/cases_1_park_garden.png", cases1_park_garden, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

# 3 - interaction between park use and green space 
# good effect of park if there are not many gardens
mydf <- ggpredict(stp_m1, terms = c("parkpref1","green_km_pp"))
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_1_park_green <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(aes(color=group), lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=group), alpha = .1) +
  scale_x_continuous(name="Preference of parks") +
  scale_y_continuous(name="Rate of increase in new COVID-19 cases") +
  theme_classic() +
  theme(text = element_text(size=14)) +
  scale_color_discrete(name = expression("Green space"~(km^2) * " per person"), labels = c("Low", "Medium", "High")) +
  scale_fill_discrete(guide=FALSE) +
  theme(legend.position="bottom")
cases_1_park_green
ggsave(file="plots/cases_1_park_green.png", cases_1_park_green, width = 230, height = 120, dpi = 600, units = "mm", device='png') 

## 4 - green space 
mydf <- ggpredict(stp_m1, terms = "green_km_pp")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_1_green <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name=expression("Green space"~(km^2) * " per person"), breaks=seq(0,0.015,0.005)) +
  scale_y_continuous(name="Rate of increase in new COVID-19 cases", breaks=seq(0.01,0.04,0.005)) +
  theme_classic() +
  theme(text = element_text(size=14))
cases_1_green

ggsave(file="plots/cases1_green.png", cases_1_green, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

## 5 - parkpref1
mydf <- ggpredict(stp_m1, terms = "parkpref1")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_1_park <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Preference of parks", breaks=seq(-25,75,25)) +
  scale_y_continuous(name="Rate of increase in new COVID-19 cases", breaks=seq(0.01,0.04,0.005)) +
  theme_classic() +
  theme(text = element_text(size=8))
cases_1_park

ggsave(file="plots/cases1_park.png", cases_1_park, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

## 6 - garden
mydf <- ggpredict(stp_m1, terms = "address_garden_perc")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_1_garden <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Addresses with gardens (%)", breaks=seq(-25,75,25)) +
  scale_y_continuous(name="Rate of increase in new COVID-19 cases", breaks=seq(0.01,0.04,0.005)) +
  theme_classic() +
  theme(text = element_text(size=8))
cases_1_garden
ggsave(file="plots/cases1_garden.png", cases_1_garden, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

############# slope of interaction between gardens and parkpref
pcase<-filter(join, name=="Birmingham")
p1<-c(-30,-10,10,30,50,70,90) # park
p3<-c(70,80,90) # garden 

parms<-expand.grid(p1,p3)

samp<-do.call("rbind", replicate(nrow(parms), pcase, simplify = FALSE))

samp$parkpref1<-parms$Var1
samp$address_garden_perc<-parms$Var2

spred<-predict(stp_m1,samp)

slopes<-cbind(sinh(spred/100),parms)
names(slopes) <- c("slope","park","garden")
slopes
log10(130)

slopes$garden[slopes$garden == 70] <- "Low"
slopes$garden[slopes$garden == 80] <- "Medium"
slopes$garden[slopes$garden == 90] <- "High"
slopes$garden<- factor(slopes$garden,levels = c("Low", "Medium", "High"))

garden_park <- ggplot()+
  geom_abline(data=slopes, aes(intercept=0, slope=slope,color=park),size=1.5)+
  xlim(0,65)+
  ylim(0,3)+
  facet_wrap(~garden) +
  theme_classic()+
  ylab("Log10 case number increase")+
  theme(text = element_text(size=20))+
  scale_color_continuous(name = "Preference \nof parks", low = "black", high = "white") +
  xlab("Days") +
  ggtitle("Addresses with gardens (%)")
garden_park
ggsave(file="plots/cases1_garden_park_slope.png", garden_park, width = 400, height = 200, dpi = 600, units = "mm", device='png') 

############# slope of parkpref
tcase<-filter(join, name=="Birmingham")
t2<-rbind(tcase,tcase,tcase,tcase,tcase,tcase,tcase)
t2$parkpref1 <- c(-30,-10,10,30,50,70,90)
j<-predict(stp_m1,t2)

angchang<-cbind.data.frame(sinh(j/100),c(-30,-10,10,30,50,70,90))
names(angchang) <-c("slope","park")

log10(130)
parkpref_slope <- ggplot()+
  geom_abline(data=angchang, aes(intercept=0, slope=slope,color=park),size=1.5)+
  xlim(0,65)+
  ylim(0,3)+
  theme_classic()+
  ylab("Log10 case number increase")+
  theme(text = element_text(size=16))+
  scale_color_continuous(name = "Preference \nof parks", low = "black", high = "white") +
  xlab("Day since first case")
parkpref_slope
ggsave(file="plots/cases1_park_slope.png", parkpref_slope, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

############# slope of mobility
ycase<-filter(join, name=="Croydon")
y2<-rbind(ycase,ycase,ycase,ycase,ycase,ycase)
y2$meanrelmob1 <- c(-50,-40,-30,-20,-10,0)
y<-predict(stp_m1,y2)

angchang2<-cbind.data.frame(sinh(y/100),c(-50,-40,-30,-20,-10,0))
names(angchang2) <-c("slope","mobility")

log10(80)
relmob_slope <- ggplot()+
  geom_abline(data=angchang2, aes(intercept=0, slope=slope,color=mobility),size=1.5)+
  xlim(0,65)+
  ylim(0,3)+
  theme_classic()+
  ylab("Log10 case number increase")+
  theme(text = element_text(size=16))+
  scale_color_continuous(name = "Relative mobility", low = "black", high = "white") +
  xlab("Day since first case")
relmob_slope
ggsave(file="plots/relmob_slope.png", relmob_slope, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

#######################################################################################
#### Model 2: Rate of decrease in cases 
m2 = lm(ihs(slope2*100) ~
          meanrelmob2 + 
          parkpref2+
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpref2:green_km_pp+
          parkpref2:frag+
          parkpref2:address_garden_perc+
          green_km_pp:frag +
          meantempperiod2+
          meanrainperiod2+
          lt +
          ln ,
        data = join)
stp_m2 = stepAIC(m2, direction = "both", 
                 trace = FALSE)
summary(stp_m2)
vif(stp_m2)
densityplot(resid(stp_m2))

## save results
results_table <- data.frame(summary(stp_m2)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m2.csv", row.names=TRUE)

############ PLOTS

## 1 - meanrelmob2 (Relative mobility for each LA averaged across TP)
mydf <- ggpredict(stp_m2, terms = "meanrelmob2")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_2_relmob <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Relative mobility", breaks=seq(-60,-10,10)) +
  scale_y_continuous(name="Rate of decrease in new COVID-19 cases", breaks=seq(-0.03,0.02,0.005)) +
  theme_classic() +
  theme(text = element_text(size=16))
cases_2_relmob

ggsave(file="plots/cases_2_relmob.png", cases_2_relmob, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

# 2 - interaction between park use and garden amount 
# good effect of park if there are not many gardens
mydf <- ggpredict(stp_m2, terms = c("parkpref2","address_garden_perc"))
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases2_park_garden <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(aes(color=group), lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=group), alpha = .1) +
  scale_x_continuous(name="Preference of parks") +
  scale_y_continuous(name="Rate of decrease in new COVID-19 cases") +
  theme_classic() +
  theme(text = element_text(size=8)) +
  scale_color_discrete(name = "Addresses with\n gardens (%)", labels = c("Low", "Medium", "High")) +
  scale_fill_discrete(guide=FALSE) +
  theme(
    legend.position = c(.368,1.02),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6)
  )
cases2_park_garden
ggsave(file="plots/cases2_park_garden.png", cases2_park_garden, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

# 3 - interaction between green space and fragmentation 
mydf <- ggpredict(stp_m2, terms = c("green_km_pp","frag"))
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases2_green_frag <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(aes(color=group), lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=group), alpha = .1) +
  scale_x_continuous(name=expression("Green space"~(km^2) * " per person")) +
  scale_y_continuous(name="Rate of decrease in new COVID-19 cases") +
  theme_classic() +
  theme(text = element_text(size=14)) +
  scale_color_discrete(name = "Fragmentation", labels = c("Low", "Medium", "High")) +
  scale_fill_discrete(guide=FALSE) +
  theme(
    legend.position = c(.1,.1),
    legend.justification = c("left", "bottom"),
    legend.box.just = "left",
    #legend.margin = margin(6, 6, 6, 6)
  )
cases2_green_frag
ggsave(file="plots/cases2_green_frag.png", cases2_green_frag, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

## 4 - green space 
mydf <- ggpredict(stp_m2, terms = "green_km_pp")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_2_green <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name=expression("Green space"~(km^2) * " per person"), breaks=seq(0,0.015,0.005)) +
  scale_y_continuous(name="Rate of decrease in new COVID-19 cases", breaks=seq(-0.06,-0.01,0.01)) +
  theme_classic() +
  theme(text = element_text(size=14))
cases_2_green

ggsave(file="plots/cases2_green.png", cases_2_green, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

## 5 - parkpref1
mydf <- ggpredict(stp_m2, terms = "parkpref2")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_2_park <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Preference of parks", breaks=seq(-25,100,25)) +
  scale_y_continuous(name="Rate of decrease in new COVID-19 cases", breaks=seq(-0.02,-0.014,0.002)) +
  theme_classic() +
  theme(text = element_text(size=8))
cases_2_park

ggsave(file="plots/cases2_park.png", cases_2_park, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

## 6 - garden
mydf <- ggpredict(stp_m2, terms = "address_garden_perc")
mydf$predicted_trans <- sinh(mydf$predicted/100)
mydf$conf.low <- sinh(mydf$conf.low/100)
mydf$conf.high <- sinh(mydf$conf.high/100)

cases_2_garden <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Addresses with gardens (%)", breaks=seq(60,100,10)) +
  scale_y_continuous(name="Rate of decrease in new COVID-19 cases", breaks=seq(-0.04,-0.01,0.002)) +
  theme_classic() +
  theme(text = element_text(size=8))
cases_2_garden

ggsave(file="plots/cases2_garden.png", cases_2_garden, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

#######################################################################################
#### Model 3: Peak of cases 
m3 = lm(sqrt(peak) ~
          meanrelmobtotal + 
          parkpreftotal +
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpreftotal:green_km_pp+
          parkpreftotal:frag+
          parkpreftotal:address_garden_perc+
          green_km_pp:frag +
          meantempperiod_total +
          meanrainperiod_total +
          lt +
          ln ,
        data = join)
stp_m3 = stepAIC(m3, direction = "both", 
                 trace = FALSE)
summary(stp_m3)
vif(stp_m3)
densityplot(resid(stp_m3))

## save results
results_table <- data.frame(summary(stp_m3)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m3.csv", row.names=TRUE)

############ PLOTS

## 1 - meanrelmobtotal (overall mobility for each LA averaged across TP)
mydf <- ggpredict(stp_m3, terms = "meanrelmobtotal")
mydf$predicted_trans <- mydf$predicted^2
mydf$conf.low <- mydf$conf.low^2
mydf$conf.high <- mydf$conf.high^2

peak_relmob <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Relative mobility", breaks=seq(-50,-10,10)) +
  scale_y_continuous(name="Peak of new COVID-19 cases", breaks=seq(20,60,10)) +
  theme_classic() +
  theme(text = element_text(size=16))
peak_relmob
ggsave(file="plots/peak_relmob.png", peak_relmob, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

#######################################################################################
#### Model 4: Cumulative cases 
m4 = lm(log10(cum) ~
          meanrelmobtotal + 
          parkpreftotal +
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpreftotal:green_km_pp+
          parkpreftotal:frag+
          parkpreftotal:address_garden_perc+
          green_km_pp:frag +
          meantempperiod_total +
          meanrainperiod_total +
          lt +
          ln ,
        data = join)
stp_m4 = stepAIC(m4, direction = "both", 
                 trace = FALSE)
summary(stp_m4)
vif(stp_m4)
densityplot(resid(stp_m4))

## save results
results_table <- data.frame(summary(stp_m4)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m4.csv", row.names=TRUE)

############ PLOTS

## 1 - address_garden_perc
mydf <- ggpredict(stp_m4, terms = "address_garden_perc")
mydf$predicted_trans <- 10^mydf$predicted
mydf$conf.low <- 10^mydf$conf.low
mydf$conf.high <- 10^mydf$conf.high

cum_garden <- ggplot(mydf, aes(x, predicted_trans)) +
  geom_line(lwd=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(name="Addresses with gardens (%)", breaks=seq(60,100,10)) +
  scale_y_continuous(name="Cumulative COVID-19 cases", breaks=seq(400,2000,200)) +
  theme_classic() +
  theme(text = element_text(size=16))
cum_garden
ggsave(file="plots/cum_garden.png", cum_garden, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

###### put some plots together
library(cowplot)
library(gridExtra)
library(ggpubr)

cases_slope1 <- plot_grid(cases_1_rel_mob, cases_1_park_green, cases1_park_garden,
                          labels=c("(a)", "(b)", "(c)"), ncol = 3, scale=1, nrow = 1) 
cases_slope1
ggsave(file="plots/Cases_slope1.png", cases_slope1, width = 350, height = 120, dpi = 600, units = "mm", device='png') 

cases_slope2 <- plot_grid(cases_2_relmob, cases2_green_frag, cases2_park_garden,
                          labels=c("(a)", "(b)", "(c)"), ncol = 3, scale=1, nrow = 1) 
cases_slope2
ggsave(file="plots/Cases_slope2.png", cases_slope2, width = 200, height = 120, dpi = 600, units = "mm", device='png') 

pg <- arrangeGrob(cases_1_garden, cases1_park_garden, garden_park,
                  layout_matrix=cbind(c(1,3), c(2,3)), nrow=2)
park_garden <- as_ggplot(pg) +                                # transform to a ggplot
  draw_plot_label(label = c("(a)", "(b)", "(c)"), size = 10)
park_garden

ggsave(file="plots/green_garden.png", green_garden, width = 200, height = 150, dpi = 600, units = "mm", device='png') 

########################################################################################################################

## run models with scaled covariates to produce forest plot
## scale all covariates
join[c(7:17,19:20,22:23,27,30,32:33,37,39)] <- scale(join[c(7:17,19:20,22:23,27,30,32:33,37,39)])
## run models again & save results
m1 = lm(ihs(slope1*100) ~
          meanrelmob1 + 
          parkpref1+
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpref1:green_km_pp+
          parkpref1:frag+
          parkpref1:address_garden_perc+
          green_km_pp:frag +
          meantempperiod+
          meanrainperiod+
          lt +
          ln ,data = join)
stp_m1 = stepAIC(m1, direction = "both", 
                 trace = FALSE)
summary(stp_m1)
## save results
results_table <- data.frame(summary(stp_m1)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m1.csv", row.names=TRUE)
###
m2 = lm(ihs(slope2*100) ~
          meanrelmob2 + 
          parkpref2+
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpref2:green_km_pp+
          parkpref2:frag+
          parkpref2:address_garden_perc+
          green_km_pp:frag +
          meantempperiod2+
          meanrainperiod2+
          lt +
          ln ,
        data = join)
stp_m2 = stepAIC(m2, direction = "both", 
                 trace = FALSE)
summary(stp_m2)
## save results
results_table <- data.frame(summary(stp_m2)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m2.csv", row.names=TRUE)
###
m3 = lm(sqrt(peak) ~
          meanrelmobtotal + 
          parkpreftotal +
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpreftotal:green_km_pp+
          parkpreftotal:frag+
          parkpreftotal:address_garden_perc+
          green_km_pp:frag +
          meantempperiod_total +
          meanrainperiod_total +
          lt +
          ln ,
        data = join)
stp_m3 = stepAIC(m3, direction = "both", 
                 trace = FALSE)
summary(stp_m3)
## save results
results_table <- data.frame(summary(stp_m3)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m3.csv", row.names=TRUE)
###
m4 = lm(log10(cum) ~
          meanrelmobtotal + 
          parkpreftotal +
          pop_density_km + 
          pop_white_perc +
          unemploy_perc +
          address_garden_perc +
          green_km_pp +
          urban_km_pp + 
          frag + 
          parkpreftotal:green_km_pp+
          parkpreftotal:frag+
          parkpreftotal:address_garden_perc+
          green_km_pp:frag +
          meantempperiod_total +
          meanrainperiod_total +
          lt +
          ln ,
        data = join)
stp_m4 = stepAIC(m4, direction = "both", 
                 trace = FALSE)
summary(stp_m4)
## save results
results_table <- data.frame(summary(stp_m4)$coefficients[,1:4])
write.csv(results_table, file = "plots/stp_m4.csv", row.names=TRUE)

## Forest plots
## read in data
stp_m1 <- read.csv("plots/stp_m1.csv", header=TRUE)
stp_m2 <- read.csv("plots/stp_m2.csv", header=TRUE)
stp_m3 <- read.csv("plots/stp_m3.csv", header=TRUE)
stp_m4 <- read.csv("plots/stp_m4.csv", header=TRUE)

## calculate CI for each
stp_m1$CI <- stp_m1$Std..Error*1.96
stp_m2$CI <- stp_m2$Std..Error*1.96
stp_m3$CI <- stp_m3$Std..Error*1.96
stp_m4$CI <- stp_m4$Std..Error*1.96

## remove intercept from each dataset
stp_m1 <- stp_m1[-1,]
stp_m2 <- stp_m2[-1,]
stp_m3 <- stp_m3[-1,]
stp_m4 <- stp_m4[-1,]

## create unique response code for each dataset
stp_m1$response <- "Pre-peak case rate"
stp_m2$response <- "Post-peak case rate"
stp_m3$response <- "Peak cases"
stp_m4$response <- "Cumulative cases"

## bind together
models <- rbind(stp_m1, stp_m2, stp_m3, stp_m4)
#models <- models[- grep(":", models$X),] ## remove interactions
row.names(models) <- 1:nrow(models) ## change rownames

## Change variable names
lookup <- c("parkpref1"="Preferece\n of parks", "parkpref2"="Preferece\n of parks", "parkpreftotal"="Preferece\n of parks",
            "meanrelmob1"="Relative\n mobility", "meanrelmob2"="Relative\n mobility", "meanrelmobtotal"="Relative\n mobility", "lt"="Latitude",
            "green_km_pp"="Green space\n (km2) per person", "address_garden_perc"= "Addresses with \ngardens (%)","unemploy_perc"="Unemployment (%)", "frag"="Fragmentation", 
            "ln"="Longitude", "meantempperiod" = "Mean \ntemperature", "meantempperiod2" = "Mean \ntemperature",
            "meantempperiod_total" = "Mean \ntemperature", "meanrainperiod" = "Mean \nrainfall", "meanrainperiod2" = "Mean \nrainfall",
            "meanrainperiod_total" = "Mean \nrainfall", "urban_km_pp" = "Urban space \n(km2) per person", "pop_density_km" = "Population density\n per km",
            "parkpref1:green_km_pp" = "Preference of parks:\nGreen space (km2) per person", "parkpref1:address_garden_perc" = 
              "Preference of parks: \nAddress with gardens (%)", "parkpref2:address_garden_perc" = "Preference of parks: \nAddress with gardens (%)",
            "green_km_pp:frag" = "Green space (km2) per person: \nFragmentation")
models <- models %>% mutate(X=lookup[as.character(X)])

## Forest plot
forest_plot <- ggplot(data=models,aes(x=X,y=Estimate, group=response), position=position_dodge(width=0.5)) +
  geom_point(aes(color=response), size=4,position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=Estimate-CI,ymax=Estimate+CI, color=response), position=position_dodge(width=0.5),width=0.5) +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_y_continuous(breaks=seq(-1,2,0.5)) +
  labs(x = "Fixed effects", y = "Standardised coefficient") +
  scale_color_discrete(name = "COVID-19 metrics") +
  theme_classic() +
  theme(text = element_text(size=16))
forest_plot
## save plot
ggsave(file="plots/forest_plot_4metrics.png", forest_plot, width = 400, height = 150, dpi = 600, units = "mm", device='png') 

## remove peak and cumulative
models2<-models[!(models$response=="Peak cases" | models$response=="Cumulative cases"),]
models2$X<- factor(models2$X,levels = c("Relative\n mobility", "Preferece\n of parks", "Preference of parks:\nGreen space (km2) per person",
                                        "Preference of parks: \nAddress with gardens (%)", "Addresses with \ngardens (%)", "Green space\n (km2) per person",
                                        "Fragmentation", "Green space (km2) per person: \nFragmentation", "Urban space \n(km2) per person",
                                        "Mean \ntemperature", "Mean \nrainfall", "Unemployment (%)",
                                        "Population density\n per km", "Longitude"))
models2$response <- factor(models2$response,levels = c("Pre-peak case rate", "Post-peak case rate"))
## Forest plot
forest_plot2 <- ggplot(data=models2,aes(x=X,y=Estimate, group=response), position=position_dodge(width=0.5)) +
  geom_point(aes(color=response), size=4,position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=Estimate-CI,ymax=Estimate+CI, color=response), position=position_dodge(width=0.5),width=0.5) +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_y_continuous(breaks=seq(-1,2,0.5)) +
  labs(x = "Fixed effects", y = "Standardised coefficient") +
  scale_color_discrete(name = "COVID-19 metrics") +
  theme_classic() +
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
forest_plot2
ggsave(file="plots/forest_plot_2metrics.png", forest_plot2, width = 380, height = 250, dpi = 600, units = "mm", device='png') 


## mean and SD of slope 1 and slope 2
mean(join$slope1) ## 0.092
sd(join$slope1) ## 0.2
mean(join$slope2) ## -0.02
sd(join$slope2) ## 0.008
