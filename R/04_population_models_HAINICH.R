source("R/setup.R")

falter_complete <- st_read("data/aurinia/aurinia_observations_complete.shp")

falter_complete <- falter_complete %>% filter(site == "nlp")

# variable: number of neighbour plots
plots <- st_read("data/study_sites/sampling_plots.shp") %>% st_transform(crs=25832) %>%
  rename(plot_id = id, cluster = Cluster)%>%
  mutate(plot_id = as.character(plot_id))

plot_centroids <- st_centroid(plots)

plot_buf <- st_buffer(plot_centroids, dist=250)

plot_buf2 <- st_intersection(plot_buf , plot_centroids) %>%
  st_drop_geometry() %>% filter(plot_id != plot_id.1) %>% 
  group_by(plot_id) %>% 
  summarise(neighbourplots = uniqueN(plot_id.1))

# variable: weather data
start = 9 # Startzeit Wetterdaten
end = 17 # Endzeit Wetterdaten

sun <- fread("data/clim/produkt_zehn_min_sd_20200101_20231231_07368.txt") %>% 
  mutate(timestamp = ymd_hm(MESS_DATUM),
         date = date(timestamp),
         hour = hour(timestamp)) %>% 
  filter(date %in% falter_complete$timestamp &
           hour %in% seq(start,end,1)) %>%
  group_by(date) %>% 
  summarise(sundur = sum(SD_10)) 

temp <- fread("data/clim/produkt_zehn_min_tu_20200101_20231231_07368.txt")%>% 
  mutate(timestamp = ymd_hm(MESS_DATUM),
         date = date(timestamp),
         hour = hour(timestamp)) %>% 
  filter(date %in% falter_complete$timestamp &
           hour %in% seq(start,end,1)) %>%
  group_by(date) %>% 
  summarise(temp = mean(TT_10))


wind <- fread("data/clim/produkt_zehn_min_ff_20200101_20231231_07368.txt")%>% 
  mutate(timestamp = ymd_hm(MESS_DATUM),
         date = date(timestamp),
         hour = hour(timestamp)) %>% 
  filter(date %in% falter_complete$timestamp &
           hour %in% seq(start,end,1)) %>%
  group_by(date) %>% 
  summarise(wind = ((mean(FF_10)*60*60)/1000))

weather <- cbind(sun, temp[,2], wind[,2])

fwrite(weather, "data/clim/weather_data_DWD_Eisenach.csv")


##### HAINICH #####
#capture history
capt.hist<-falter_complete %>% st_drop_geometry() %>% 
  mutate(site = substr(obs_id, start=1, stop=3))

capt.hist <- capt.hist %>% ungroup %>% 
  mutate(event = dense_rank(date(timestamp)),
         detect = 1) %>% 
  select(id, sex, event, detect) %>% unique()

capt.hist<-spread(capt.hist,event,detect,fill = 0)

capt.hist.gof <- capt.hist 

capt.hist<-group_by(capt.hist,id)

capt.hist<-unite(capt.hist,"ch",3:14,sep = "")



#capture history tabelle umformatieren: ID weg, Spalte "frequency" erstellen
Popt<-capt.hist
Popt<-Popt[-1]
Popt$frequency<-nchar(1)


dates <- sort(unique(falter_complete$date))
dates
timeints <- difftime(dates[2:length(dates)],dates[1:length(dates)-1]) %>% as.numeric()
timeints

#Process data
Popt.pr <- process.data(Popt, begin.time = 1, model = "POPAN", 
                        time.intervals = timeints %>% as.numeric())
Popt.ddl=make.design.data(Popt.pr)
Popt.ddl

df=data.frame(time=cumsum(c(1, timeints)),
              temp=weather$temp[weather$date %in% dates],
              sundur = weather$sundur[weather$date %in% dates],
              wind = weather$wind[weather$date %in% dates])


Popt.ddl$p=merge_design.covariates(Popt.ddl$p,df)

str(Popt.ddl$p)

# Model parameters
p.sun=list(formula= ~sundur)
p.wind=list(formula= ~wind)
p.temp=list(formula= ~temp)
p.tempwind=list(formula= ~temp+wind)
p.sunwind=list(formula= ~sundur+wind)
p.tempsun=list(formula= ~temp+sundur)
p.tempwindsun=list(formula= ~temp+wind+sundur)

Phi.dot = list(formula =  ~ 1)
Phi.time = list(formula = ~ Time)
Phi.timesq = list(formula =  ~ Time + I(Time^2))
p.dot = list(formula = ~ 1)
p.time = list(formula = ~ Time)
p.timesq = list(formula = ~ Time + I(Time^2))
pent.dot = list(formula = ~ 1)
pent.time = list(formula = ~ Time)
pent.timesq = list(formula = ~ Time + I(Time^2))
N.dot = list(formula = ~ 1)

# create all combinations of model parameters
models <- create.model.list("POPAN")

# run all models
models_output <- mark.wrapper.parallel(models, data = Popt.pr, ddl=Popt.ddl, delete=T)

# extract model table
model.table <- models_output$model.table

# save model output as xlsx
fwrite(model.table, "data/population_models/models_output_hainich.csv")
writexl::write_xlsx(model.table,"data/population_models/models_output_hainich.xlsx")

# empty data frame to be filled in loop
all <- data.frame()

for(i in 1:nrow(model.table))
{
mymod <- models_output[[i]]

para <- strsplit(rownames(mymod$results$real), split=" ") %>% as.data.frame()
para <- para[1,] %>% t()

mymod.Phi <- mymod$results$real[para == "Phi", ]
mymod.p <- mymod$results$real[para == "p", ]
mymod.pent <- mymod$results$real[para == "pent", ]
mymod.N <- mymod$results$real[para == "N", ]

dat <- rbind(round(colMeans(mymod.Phi[,1:4]),5),
      round(colMeans(mymod.p[,1:4]),5),
      round(colMeans(mymod.pent[,1:4]),5),
      round(colMeans(mymod.N[,1:4]),0)) %>% as.data.frame()

dat$parameter <- c("Phi", "p", "pent", "N")
dat$model = mymod$model.name

all <- rbind(all, dat)

}

all$estimate_CI <- paste0(round(all$estimate, 2), " [", round(all$lcl, 3), "-", round(all$ucl, 3), "]")

summary_table <- all %>% group_by(model) %>% pivot_wider(id_cols=c("model"), 
                                                         names_from="parameter",
                                                         values_from="estimate_CI") 

summary <- summary_table %>% left_join(model.table[5:length(model.table)], by="model") %>% arrange(AICc)

summary$weightcum <- cumsum(summary$weight)

N <- all %>% filter(parameter == "N")

# Population density
summary$ind_per_ha <- paste0(round(N$estimate/uniqueN(falter_complete$plot_id), digits=0), " [", round(N$lcl/uniqueN(falter_complete$plot_id), 0), "-", round(N$ucl/uniqueN(falter_complete$plot_id), 0), "]")

fwrite(summary %>% select(!weightcum),"data/population_models/models_summary_hainich.csv")
writexl::write_xlsx(summary %>% select(!weightcum),"data/population_models/models_summary_hainich.xlsx")
writexl::write_xlsx(summary %>% filter(weightcum <= 0.95) %>% select(!weightcum),"data/population_models/models_summary_hainich_weight95.xlsx")


# GOF test

library(R2ucare)

hist <- capt.hist.gof[3:length(capt.hist.gof)] %>% as.matrix()
capt.hist.gof$freq <- 1
freq <- capt.hist.gof$freq

overall_CJS(X=hist,freq=freq,rounding = 3) 
