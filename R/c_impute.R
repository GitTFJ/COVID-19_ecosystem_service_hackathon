cmb = read.csv("data/produced/data.csv")
val = cmb
val$pop_white_perc = (val$pop_white_thou/val$pop_thou)*100
val$date = as.numeric(val$date)
val$case = ifelse(val$date < 180 & is.na(val$case), 0, val$case)
val$nat_case = ifelse(val$date < 180 & is.na(val$nat_case), 0, val$nat_case)
val = val %>%
  group_by(name) %>%
  filter(sum(case > 0, na.rm = T) > 0)
val$region = as.numeric(val$region)
val$name = as.numeric(val$name)
val$country = 1


dat = as.data.frame(val[,c(3,2,7:20, 22:36)])

# empty mice imputation
imp0 <- mice(dat, maxit=0)
predM <- imp0$predictorMatrix
predM[,"name"] = c(0, rep(-2,30))
tmp <- mice(dat, m = 5, predictorMatrix = predM, 
            method =  "2l.pan", maxit=5, paniter=200)
tmp2 = complete(tmp,1)

datalist = list()
datalist[["data_missing"]] = dat
datalist[["data_imp1"]] = complete(tmp,1)
datalist[["data_imp2"]] = complete(tmp,2)
datalist[["data_imp3"]] = complete(tmp,3)
datalist[["data_imp4"]] = complete(tmp,4)
datalist[["data_imp5"]] = complete(tmp,5)

saveRDS(datalist, "data/produced/data.rds")

dat_join = rbind(datalist[[2]], datalist[[3]], datalist[[4]], datalist[[5]], datalist[[6]])
dat_join = dat_join %>%
  group_by(name, date) %>%
  dplyr::summarise(
    ret_m = mean(ret_la),
    ret_l = mean(ret_la) - (1.96*sd(ret_la)/sqrt(length(ret_la))),
    ret_u = mean(ret_la) + (1.96*sd(ret_la)/sqrt(length(ret_la))),
    food_m = mean(food_la),
    food_l = mean(food_la) - (1.96*sd(food_la)/sqrt(length(food_la))),
    food_u = mean(food_la) + (1.96*sd(food_la)/sqrt(length(food_la))),
    park_m = mean(park_la),
    park_l = mean(park_la) - (1.96*sd(park_la)/sqrt(length(park_la))),
    park_u = mean(park_la) + (1.96*sd(park_la)/sqrt(length(park_la))),
    tran_m = mean(tran_la),
    tran_l = mean(tran_la) - (1.96*sd(tran_la)/sqrt(length(tran_la))),
    tran_u = mean(tran_la) + (1.96*sd(tran_la)/sqrt(length(tran_la))),
    work_m = mean(work_la),
    work_l = mean(work_la) - (1.96*sd(work_la)/sqrt(length(work_la))),
    work_u = mean(work_la) + (1.96*sd(work_la)/sqrt(length(work_la))),
    resid_m = mean(resid_la),
    resid_l = mean(resid_la) - (1.96*sd(resid_la)/sqrt(length(resid_la))),
    resid_u = mean(resid_la) + (1.96*sd(resid_la)/sqrt(length(resid_la)))
  )

park = ggplot(dat_join[which(dat_join$date < 180),]) +
  geom_vline(aes(xintercept = 31), colour = "dark blue", size = 3, alpha = 0.35) +
  geom_vline(aes(xintercept = 81), colour = "dark red", size = 3, alpha = 0.35) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(aes(x = date, y = park_m, group = name), alpha = 0.01) +
  geom_line(aes(x = date, y = park_l, group = name), alpha = 0.01) +
  geom_line(aes(x = date, y = park_u, group = name), alpha = 0.01) +
  coord_cartesian(ylim = c(100, -100)) +
  scale_x_continuous(breaks = c(0,90,180), labels = c("January 1st", "April 1st", "July 1st")) +
  labs(x = "", y = "Parks") +
  theme_classic()


groc = ggplot() +
  geom_vline(aes(xintercept = 31), colour = "dark blue", size = 3, alpha = 0.35) +
  geom_vline(aes(xintercept = 81), colour = "dark red", size = 3, alpha = 0.35) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = food_m, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = food_l, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = food_m, group = name), alpha = 0.01) +
  coord_cartesian(ylim = c(100, -100)) +
  scale_x_continuous(breaks = c(0,90,180), labels = c("January 1st", "April 1st", "July 1st")) +
  labs(x = "", y = "Grocery & Pharmacy Stores") +
  theme_classic()


ret = ggplot() +
  geom_vline(aes(xintercept = 31), colour = "dark blue", size = 3, alpha = 0.35) +
  geom_vline(aes(xintercept = 81), colour = "dark red", size = 3, alpha = 0.35) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = ret_m, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = ret_l, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = ret_u, group = name), alpha = 0.01) +
  coord_cartesian(ylim = c(100, -100)) +
  scale_x_continuous(breaks = c(0,90,180), labels = c("January 1st", "April 1st", "July 1st")) +
  labs(x = "", y = "Retail & Recreation") +
  theme_classic()

tran = ggplot() +
  geom_vline(aes(xintercept = 31), colour = "dark blue", size = 3, alpha = 0.35) +
  geom_vline(aes(xintercept = 81), colour = "dark red", size = 3, alpha = 0.35) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = tran_m, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = tran_l, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = tran_u, group = name), alpha = 0.01) +
  coord_cartesian(ylim = c(100, -100)) +
  scale_x_continuous(breaks = c(0,90,180), labels = c("January 1st", "April 1st", "July 1st")) +
  labs(x = "", y = "Transport") +
  theme_classic()

work = ggplot() +
  geom_vline(aes(xintercept = 31), colour = "dark blue", size = 3, alpha = 0.35) +
  geom_vline(aes(xintercept = 81), colour = "dark red", size = 3, alpha = 0.35) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = work_m, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = work_l, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = work_u, group = name), alpha = 0.01) +
  coord_cartesian(ylim = c(100, -100)) +
  scale_x_continuous(breaks = c(0,90,180), labels = c("January 1st", "April 1st", "July 1st")) +
  labs(x = "", y = "Workplaces") +
  theme_classic()

resid = ggplot() +
  geom_vline(aes(xintercept = 31), colour = "dark blue", size = 3, alpha = 0.35) +
  geom_vline(aes(xintercept = 81), colour = "dark red", size = 3, alpha = 0.35) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = resid_m, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = resid_l, group = name), alpha = 0.01) +
  geom_line(data = dat_join[which(dat_join$date < 180),],
            aes(x = date, y = resid_u, group = name), alpha = 0.01) +
  coord_cartesian(ylim = c(100, -100)) +
  scale_x_continuous(breaks = c(0,90,180), labels = c("January 1st", "April 1st", "July 1st")) +
  labs(x = "", y = "Residential") +
  theme_classic()

ggarrange(park, groc, ret, tran, work, resid, ncol = 3, nrow = 2)
ggsave("plots/changes_in mobility.png", width = 10, height = 5.5, units = "in")
