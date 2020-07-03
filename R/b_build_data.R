#Load data
cases = read.csv("data/local_authority_cases.csv")
mobil = read.csv("data/mobility.csv")
bame = read.csv("data/bame.csv")
gard = read.csv("data/garden.csv")
emply = read.csv("data/unemployed.csv")
size = read.csv("data/local_authority.csv")

#Extract heirarchal information from gard
hei = gard[,c(3,4,5,6)]

#Produce time series for every local authority from 1st of Jan
cmb = NULL
for(a in unique(hei$LAD.name)){
  print(a)
  tmp_df = data.frame(
    date = seq(as.Date("2020/01/01"), by = "day", length.out = 200),
    Name = a)
  cmb = rbind(cmb, tmp_df)
}
cmb = subset(cmb, Name != "")
cmb = left_join(cmb, hei, by = c("Name" = "LAD.name"))



#Clean dates for cases
cases$date = as.Date(paste(
  substr(as.character(cases$Specimen_Date), 7, 10),
  substr(as.character(cases$Specimen_Date), 4, 5),
  substr(as.character(cases$Specimen_Date), 1, 2), 
  sep = "-"))

cmb = left_join(cmb, cases[,c(1,4,5)], by = c("Name", "date"))

#Clean dates for mobility
mobil$date = as.Date(paste(
  substr(as.character(mobil$date), 7, 10),
  substr(as.character(mobil$date), 4, 5),
  substr(as.character(mobil$date), 1, 2), 
  sep = "-"))
cmb = left_join(cmb, mobil[c(2,8:14)], by = c("LAD.code" = "Code", "date"))
cmb = left_join(cmb, mobil[c(2,8:14)], by = c("Region.code" = "Code", "date"))
colnames(cmb) = c(
  "date",
  "name",
  "region_code",
  "region",
  "la_code",
  "case",
  "ret_la",
  "food_la",
  "park_la",
  "tran_la",
  "work_la",
  "resid_la",
  "ret_reg",
  "food_reg",
  "park_reg",
  "tran_reg",
  "work_reg",
  "resid_reg")
bame$Area.Name = trimws(as.character(bame$Area.Name))
cmb = left_join(cmb, bame[,c(1,3,4)], by = c("name" = "Area.Name"))
cmb$Total = as.numeric(as.character(gsub(",","",cmb$Total)))

cmb = left_join(cmb, emply, by = c("name" = "la"))

cmb = left_join(cmb, gard[,6:11], by = c("name" = "LAD.name"))

grn = read.csv("data/produced/green_sum.csv")
frg = read.csv("data/prodcued/frag.csv")

#Claculate key greenenss, urban anfd fragmentation metrics
sum_grn = grn %>%
  group_by(lad17nm) %>%
  dplyr::summarise(green_med = median(green), urb_med = median(urban))
sum_grn$green_perc = (sum_grn$green_med/40000)*100
sum_grn$urb_perc = (sum_grn$urb_med/40000)*100

sum_frg = frg[which(frg$metric == "lsi"),] %>%
  group_by(lad17nm) %>%
  dplyr::summarise(frg = median(value, na.rm = T))

cmb = left_join(cmb, sum_grn[,c(1,4,5)], by = c("name" = "lad17nm"))
cmb = left_join(cmb, sum_frg, by = c("name" = "lad17nm"))
cmb = left_join(cmb, size[,2:5], by = c("name" = "lad17nm"))

colnames(cmb)[19:26] = c(
  "pop_thou",
  "pop_white_thou",
  "unmply_perc",
  "addresses",
  "addresses_garden",
  "total_garden_m",
  "perc_private_space",
  "av_garden_m")
cmb$addresses = as.numeric(as.character(gsub(",","",cmb$addresses)))
cmb$addresses_garden = as.numeric(as.character(gsub(",","",cmb$addresses_garden)))
cmb$total_garden_m = as.numeric(as.character(gsub(",","",cmb$total_garden_m)))
cmb$perc_private_space = as.numeric(as.character(gsub("%","",cmb$perc_private_space)))
cmb$av_garden_m = as.numeric(as.character(gsub(",","",cmb$av_garden_m)))

#Add national cases data
nat_case = read.csv("data/national_cases.csv")
nat_case = subset(nat_case, Area.name == "England")
nat_case$date = as.Date(paste(
  substr(as.character(nat_case$Specimen.date), 7, 10),
  substr(as.character(nat_case$Specimen.date), 4, 5),
  substr(as.character(nat_case$Specimen.date), 1, 2), 
  sep = "-"))
nat_case$nat_case = nat_case$Daily.lab.confirmed.cases
cmb = left_join(cmb, nat_case[,c(12,13)], by = "date")

#Add rainfall data
temp = read.csv("data/temp_rainfall.csv")
temp = temp[-153,]
temp$date = as.Date(paste(
  substr(as.character(temp$date), 7, 10),
  substr(as.character(temp$date), 4, 5),
  substr(as.character(temp$date), 1, 2), 
  sep = "-"))
cmb = left_join(cmb, temp)


write.csv(cmb, "data/produced/data.csv")
