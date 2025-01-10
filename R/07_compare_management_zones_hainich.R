source("R/setup.R")

##### import study geodata #####
# load study sites and plots
ug <- st_read("data/study_sites/study_sites.shp") %>% st_transform(crs=25832)

# border of Hainich nationalpark
nlp_auﬂen <- st_read("data/study_sites/nlp_shapes/nationalpark_hainich.shp") %>% st_transform(crs=25832)
nlp_schutz <- st_read("data/study_sites/nlp_shapes/hainich_zones.shp") %>% st_transform(crs=25832)

# sampling plots
plots <- st_read("data/study_sites/sampling_plots.shp") %>% st_transform(crs=25832) %>%
  rename(plot_id = id, cluster = Cluster)%>%
  mutate(plot_id = as.character(plot_id)) %>%
  filter(!plot_id %in% c(107:110))

# calculate centroids of sampling plots
plot_centroids <- st_centroid(plots)


falter_complete <- st_read("data/aurinia/aurinia_observations_complete.shp")

falter_recapt_cent <- st_read("data/aurinia/aurinia_observations_recaptures.shp")

falter_lines <- st_read("data/aurinia/aurinia_recaptures_lines.shp")

# # crop orthophoto hainich
# ortho_nlp <- terra::rast("data/orthophotos/dop_hainich_2020.tif") 
# mybox_nlp <- st_buffer(plots %>% filter(cluster <6), dist=250) %>% st_transform(crs=25832)
# ortho_nlp <- crop(ortho_nlp, mybox_nlp)
# 
# # crop orthophoto kriegberg
# ortho_krb <- terra::rast("data/orthophotos/dop_kriegberg_2020.tif") 
# mybox_krb <- st_buffer(plots %>% filter(cluster == 6), dist=250) %>% st_transform(crs=25832)
# ortho_krb <- crop(ortho_krb, mybox_krb)
# 
# # crop orthophoto kriegberg
# ortho_jon <- terra::rast("data/orthophotos/dop_jonastal_2020.tif") 
# mybox_jon <- st_buffer(plots %>% filter(cluster == 7), dist=250) %>% st_transform(crs=25832)
# ortho_jon <- crop(ortho_jon, mybox_jon)
# 


##### calculations #####


falter_network1 <- falter_recapt_cent %>%  
  st_intersection(plots %>% select(plot_id)) %>%
  st_drop_geometry() %>%
  arrange(id, timestamp) %>% pivot_wider(id_cols=c("id"), 
                                         names_from="event_id",
                                         values_from="plot_id") %>% 
  rename(from = event1,
         to = event2)

falter_network3 <- falter_network1 %>% filter(!is.na(event3)) %>%
  mutate(from = to, 
         to = event3)

falter_network4 <- falter_network3 %>% filter(!is.na(event4)) %>%
  mutate(from = to, 
         to = event4)

falter_network <- rbind(falter_network1, falter_network3, falter_network4) %>% 
  select(id, from, to)                                              

falter_network <- falter_network %>% group_by(from, to) %>% 
  summarise(connections = length(id)) %>%
  ungroup() %>%
  mutate(connect_id = paste0("c",seq(1,length(from),1))) #%>%

uniqueN(falter_network %>% select(from, to))


###### network

falter_connect <- falter_lines[0,0] 

falter_recapt_cent1 <- falter_recapt_cent %>% filter(event_id %in% c("event1", "event2"))%>% 
  group_by(id) %>% mutate(n = length(id)) %>% filter(n == 2) %>% select(!n)

for(i in unique(falter_recapt_cent1$id))
{
  mydat <- falter_recapt_cent1 %>% filter(id == i) %>% group_by(id) %>% arrange(timestamp)  
  pl <- sort(c(mydat$plot_id[1],mydat$plot_id[2]))
  
  myline <- mydat %>% group_by(id) %>% st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>% 
    mutate(id = i,
           sex = mydat$sex[1],
           connect_id = paste0(pl[1],"_",pl[2]))
  
  myline$minlength = st_length(myline) %>% as.numeric()
  
  #falter_lines <- ifelse(a == 1, myline, rbind(myline, falter_lines))
  
  falter_connect <- rbind(myline, falter_connect)
  
}

falter_recapt_cent2 <- falter_recapt_cent %>% filter(event_id == "event3")

falter_recapt_cent2x <- falter_recapt_cent %>% filter(event_id == "event2" &
                                                        id %in% falter_recapt_cent2$id)

falter_recapt_cent2 <- rbind(falter_recapt_cent2, falter_recapt_cent2x)

for(i in unique(falter_recapt_cent2$id))
{
  mydat <- falter_recapt_cent2 %>% filter(id == i) %>% group_by(id) %>% arrange(timestamp)
  
  pl <- sort(c(mydat$plot_id[1],mydat$plot_id[2]))
  
  myline <- mydat %>% group_by(id) %>% st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>% 
    mutate(id = i,
           sex = mydat$sex[1],
           connect_id = paste0(pl[1],"_",pl[2]))
  
  myline$minlength = st_length(myline) %>% as.numeric()
  
  #falter_lines <- ifelse(a == 1, myline, rbind(myline, falter_lines))
  
  falter_connect <- rbind(myline, falter_connect)
  
}

falter_recapt_cent3 <- falter_recapt_cent %>% filter(event_id == "event4")

falter_recapt_cent3x <- falter_recapt_cent %>% filter(event_id == "event3" &
                                                        id %in% falter_recapt_cent2$id)

falter_recapt_cent3 <- rbind(falter_recapt_cent3, falter_recapt_cent3x)

for(i in unique(falter_recapt_cent3$id))
{
  mydat <- falter_recapt_cent3 %>% filter(id == i) %>% group_by(id)
  
  pl <- sort(c(mydat$plot_id[1],mydat$plot_id[2]))
  
  myline <- mydat %>% group_by(id) %>% st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>% 
    mutate(id = i,
           sex = mydat$sex[1],
           connect_id = paste0(pl[1],"_",pl[2]))
  
  myline$minlength = st_length(myline) %>% as.numeric()
  
  #falter_lines <- ifelse(a == 1, myline, rbind(myline, falter_lines))
  
  falter_connect <- rbind(myline, falter_connect)
  
}

cons <- falter_connect %>% st_drop_geometry() %>% group_by(connect_id) %>% count()

falter_cons <- falter_connect %>% select(connect_id, x, minlength) %>% 
  unique() %>% merge(cons, by="connect_id") %>% st_as_sf() %>% filter(minlength > 0)

falter_intraplot <- falter_connect %>% select(connect_id, x, minlength) %>% 
  unique() %>% merge(cons, by="connect_id") %>% st_as_sf() %>% filter(minlength == 0) %>%
  st_drop_geometry() %>% separate(col="connect_id", sep="_", into=c("plot_id"))


falter_complete2 <- falter_complete %>% st_transform(crs=4326) %>% st_coordinates()%>%
  as.data.frame()

# calculate captures per plot

plots_captures <- falter_complete %>% st_drop_geometry() %>% 
  group_by(plot_id) %>%
  summarise(ncap = length(id)) %>% merge(plots, by="plot_id",
                                         all.y=T)

plots_captures$ncap[is.na(plots_captures$ncap)] <- 0

plots_captures <- plots_captures %>% st_as_sf() %>% st_transform(crs=4326) %>% 
  mutate(ncap_rel = ncap/(12*(10/60))) %>% filter(cluster < 6)


plots2 <-  plots %>% st_transform(crs=4326) %>% 
  merge(falter_intraplot, by="plot_id", all.x=T)

falter_cons_buf <- st_buffer(falter_cons, dist=(falter_cons$n)*10) %>%
  arrange(n)

falter_cons_hainich <- falter_cons_buf %>% st_intersection(ug[ug$Name == "Hainich",]) 



falter_plots_hainich_cent <- plots_captures %>% st_transform(crs=25832) %>% 
  st_centroid()

falter_plots_hainich_schutz <- plots_captures %>% st_transform(crs=25832) %>% 
  st_centroid() %>% st_intersection(nlp_schutz) %>% as.data.frame() %>%
  mutate(zone = 1)

falter_plots_hainich_cent <- falter_plots_hainich_cent %>% 
  left_join(falter_plots_hainich_schutz %>% select(plot_id, zone), by="plot_id") %>%#
  mutate(zone = ifelse(is.na(zone), "Management", "Schutzzone"))

density <- falter_plots_hainich_cent %>% group_by(zone) %>% st_drop_geometry() %>%
  summarise(mean_ncap = mean(ncap_rel),
            sd_ncap = sd(ncap_rel)) %>%
  as.data.frame()

density

test <- t.test(falter_plots_hainich_cent$ncap_rel ~ falter_plots_hainich_cent$zone)

pvalue <- ifelse(round(test$p.value, digits=3) == 0, "<0.001", round(test$p.value, digits=3))

statistic <- test$statistic

png("figures/figure_boxplots_hainich_zonen.png", width=3000, height=2700, res=300)
ggplot(falter_plots_hainich_cent, aes(x=zone, y=ncap_rel, fill=zone)) + 
  geom_boxplot() + 
  stat_summary(geom = "point", pch=21, fill="white", size=5, col="black", alpha=0.7) +
  theme_bw() + 
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ylab("Gefangene Individuen pro ha und Stunde") +
  xlab("Nationalpark-Zone") + 
  scale_fill_manual(values=c("#1F77B4", "#2CA02C")) +
  annotate("rect", xmin = 0.5, xmax = 1.2, ymin = 32, ymax = 46,
           alpha = .6, colour="black", fill="white") +
  annotate(x=0.8, y=43, geom="text", label='atop(bold("Mittelwerte Falter-Dichte:"))', parse = TRUE) +
  annotate(x=0.8, y=42, geom="text", label=paste0("Management-Zone: ", round(density$mean_ncap[density$zone == "Management"], digits=1))) +
  annotate(x=0.75, y=40, geom="text", label=paste0("Schutz-Zone: ", round(density$mean_ncap[density$zone == "Schutzzone"], digits=1))) +
  annotate(x=0.78, y=36, geom="text", label='atop(bold("t-Test des Unterschieds"))', parse = TRUE) +
  annotate(x=0.8, y=35, geom="text", label=paste0("T= ", round(statistic, digits=2), ", p= ", pvalue, "****")) +
  labs(title="Dichte gefangener Individuen - Euphydryas aurinia",
       subtitle="Nationalpark Hainich 2020",
       caption="Boxplots: waagerechter Strich = Median, zentraler Punkt = Mittelwert, \n kleine Punkte = Ausreiﬂer. Boxen decken den Interquartilsbereich ab.
       Grafik: David Singer")

dev.off()

density <- falter_plots_hainich_cent %>% group_by(zone) %>% st_drop_geometry() %>%
  summarise(mean_ncap = mean(ncap_rel),
            sd_ncap = sd(ncap_rel)) %>%
  as.data.frame()
  
density

plot(falter_plots_hainich_cent)
