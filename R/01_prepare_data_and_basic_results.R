source("R/setup.R")

##### import study geodata #####
# load study sites and plots
ug <- st_read("data/study_sites/study_sites.shp") %>% st_transform(crs=25832)

# sampling plots
plots <- st_read("data/study_sites/sampling_plots.shp") %>% st_transform(crs=25832) %>%
  rename(plot_id = id, cluster = Cluster)%>%
  mutate(plot_id = as.character(plot_id))

# calculate centroids of sampling plots
plot_centroids <- st_centroid(plots)

##### import observations of E. aurinia #####

### NLP Hainich
falter1 <- st_read("data/aurinia/aurinia_observations_hainich.shp") %>% st_transform(crs=25832) %>% 
  rename(id = Falter_ID, timestamp = Datum, plot_id = GridID,
         sex = Geschlecht) %>%
  mutate(timestamp = as_datetime(dmy_hm(timestamp)),
         date = date(timestamp)) %>%  arrange(timestamp, id) %>%
         mutate(obs_id = paste0("nlp_",seq(1000,length(id)+999,1)),
                site_name = "Hainich")

### Kriegberg
falter2 <- st_read("data/aurinia/aurinia_observations_kriegberg.shp") %>% st_transform(crs=25832) %>% 
  rename(id = Falter_ID, timestamp = Datum, plot_id = GridID,
         sex = Geschlecht,
         X = x, Y = y) %>%
  mutate(timestamp = as_datetime(dmy_hm(timestamp)),
         date = date(timestamp)) %>%  arrange(timestamp, id) %>%
  mutate(obs_id = paste0("krb_",seq(1000,length(id)+999,1)),
         site_name = "Kriegberg")

### Jonastal
falter3 <- st_read("data/aurinia/aurinia_observations_jonastal.shp") %>% st_transform(crs=25832) %>% 
  rename(id = Falter_ID, timestamp = Datum, plot_id = GridID,
         sex = Geschlecht) %>%
  mutate(timestamp = as_datetime(dmy_hm(timestamp)),
         date = date(timestamp)) %>%  arrange(timestamp, id) %>%
  mutate(obs_id = paste0("jon_",seq(1000,length(id)+999,1)),
         site_name = "Jonastal")

# combine observations from all sites
falter <- rbind(falter1, falter2, falter3) %>% rename(plot_id_orig = plot_id) %>%
  mutate(site_name = factor(site_name, levels=c("Hainich", "Kriegberg", "Jonastal")))

# renaming german to english
falter$sex <- falter$sex %>% gsub(pattern="Männlich", replacement="male")
falter$sex <- falter$sex %>% gsub(pattern="Weiblich", replacement="female")
falter$sex <- falter$sex %>% gsub(pattern="Unbekannt", replacement="unknown")

# add plot_id to observations
falter_inplot <- falter %>% st_intersection(plots %>% select(plot_id, geometry))

falter <- rbind(falter_inplot, falter %>% filter(!obs_id %in% falter_inplot$obs_id) %>%
                  mutate(plot_id = plot_id_orig))

# remove recaptures in the same plot the same day
falter <- falter %>% group_by(id, date, plot_id) %>% 
  mutate(rank = dense_rank(timestamp)) %>% filter(rank == 1) %>%
  select(!rank)

# add observations that were recorded little bit outside the plots but during 
# the sampling period (10 min), as they result from GPS inaccuracies
surveys <- falter %>% st_drop_geometry() %>% 
  group_by(plot_id, date) %>% 
  summarise(n_obs = length(id), from = min(timestamp),to = max(timestamp)) %>%
  mutate(plot_id = as.character(plot_id))

# filter observations without assigned plot_id
lostfalter <- falter %>% filter(plot_id == "-")

# snap observations to next plot, with a tolerance of half plot distance
lostfalter <- st_snap(lostfalter %>% 
                        select(!plot_id), plots, tolerance=75/2) %>% 
                      st_intersection(plots)

# filter for observations spatially outside plots but temporally within sampling 10-min-period
lostfalter <- lostfalter %>% left_join(surveys, by=c("date", "plot_id")) %>% 
  filter(!is.na(from) & timestamp >= from & timestamp <= to)

# combine observations from inside plots with 
falter_complete <- rbind(falter %>% select(id, sex, timestamp, plot_id, obs_id, geometry) %>% filter(!plot_id == "-"), 
                         lostfalter %>% select(id, sex, timestamp, plot_id, obs_id, geometry) ) %>%
  merge(plots %>% st_drop_geometry() %>% select(plot_id, cluster), by="plot_id")


# Sort points by ID and timestamp and add capture-event_id
falter_complete <- falter_complete %>%
  arrange(id, timestamp) %>%
  group_by(id) %>%
  mutate(event_id = paste0("event", dense_rank(timestamp)),
         site = substr(obs_id, start=1, stop=3),
         date=date(timestamp)) 

falter_complete <- st_write(falter_complete, "data/aurinia/aurinia_observations_complete.shp", append=F)

# calculate numbers for manuscript

uniqueN(falter_complete$id)

nlp <- falter_complete %>% filter(site == "nlp")
uniqueN(nlp$id)
uniqueN(nlp$id[nlp$event_id != "event1"])
uniqueN(nlp$id[nlp$event_id %in% c("event3")])

krb <- falter_complete %>% filter(site == "krb")
uniqueN(krb$id)
uniqueN(krb$id[krb$event_id != "event1"])

jon <- falter_complete %>% filter(site == "jon")
uniqueN(jon$id)
uniqueN(jon$id[jon$event_id != "event1"])

##### table recapture data #####
recapt_total <- falter_complete %>% st_drop_geometry() %>% 
  group_by(site, event_id) %>%
  count() %>% arrange(site, event_id) %>% mutate(sex = "total") %>% 
  pivot_wider(id_cols=c("site", "sex"), names_from="event_id", values_from = "n")

recaptures <- falter_complete %>% st_drop_geometry() %>%
  group_by(site, sex, event_id) %>%
  count() %>% arrange(site, sex, event_id) %>% 
  pivot_wider(id_cols=c("site", "sex"), names_from="event_id", values_from = "n")

recaptures <- rbind(recaptures, recapt_total)

recaptures[is.na(recaptures)] <- 0

recaptures$sex <- factor(recaptures$sex, levels=c("female", "male", "unknown", "total"))

recaptures <- recaptures %>% arrange(site, sex)

fwrite(recaptures, "data/table_recaptures.csv")

##### figure time-series #####

falter_ts <- falter_complete %>% st_drop_geometry %>% 
  group_by(date, site, sex) %>% summarise(n_obs = length(id))

falter_ts_all <- falter_complete %>% st_drop_geometry %>% 
  group_by(date, site) %>% summarise(n_obs = length(id)) %>%
  mutate(sex = "total")

falter_ts <- rbind(falter_ts, falter_ts_all)

ts_all <- expand.grid(date = unique(falter_ts$date), 
                      site = unique(falter_ts$site),
                      sex = unique(falter_ts$sex))

falter_ts <- falter_ts %>% merge(ts_all, by=c("site", 
                                              "date", "sex"),all.y=T)

falter_ts$n_obs[is.na(falter_ts$n_obs)] <- 0

falter_ts$site_name <- factor(falter_ts$site, levels=c("nlp", "krb", "jon"),
                              labels=c("Hainich", "Kriegberg", "Jonastal"))



ts_plot <- ggplot(falter_ts %>% filter(sex != "unknown"), 
                  aes(x=date, y=n_obs, col=sex)) + 
  geom_line(linewidth=1) +
  geom_point(size=3, pch=1) +
  theme_bw() +
  theme(legend.position = "bottom")+
  scale_x_date(breaks=seq(ymd("2020-05-16"), ymd("2020-06-09"), 4),
               date_labels = "%b %d")+
  xlab("Date") +
  ylab("Captured individuals") + facet_wrap(~site_name, ncol=1,
                                            scales="free_y")+
  scale_colour_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))


ts_plot

png("figures/figure1_captures_per_day.png", width=2500*2, height=1800*2, res=600)
ts_plot
dev.off()

pdf("figures/figure1_captures_per_day.pdf", width=8, height=7)
ts_plot
dev.off()
