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

# crop orthophoto hainich
ortho_nlp <- terra::rast("data/orthophotos/dop_hainich_2020.tif") 
mybox_nlp <- st_buffer(plots %>% filter(cluster <6), dist=250) %>% st_transform(crs=25832)
ortho_nlp <- crop(ortho_nlp, mybox_nlp)

# crop orthophoto kriegberg
ortho_krb <- terra::rast("data/orthophotos/dop_kriegberg_2020.tif") 
mybox_krb <- st_buffer(plots %>% filter(cluster == 6), dist=250) %>% st_transform(crs=25832)
ortho_krb <- crop(ortho_krb, mybox_krb)

# crop orthophoto kriegberg
ortho_jon <- terra::rast("data/orthophotos/dop_jonastal_2020.tif") 
mybox_jon <- st_buffer(plots %>% filter(cluster == 7), dist=250) %>% st_transform(crs=25832)
ortho_jon <- crop(ortho_jon, mybox_jon)



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
  mutate(ncap_rel = ncap/(12*(10/60)))


plots2 <-  plots %>% st_transform(crs=4326) %>% 
  merge(falter_intraplot, by="plot_id", all.x=T)

falter_cons_buf <- st_buffer(falter_cons, dist=(falter_cons$n)*10) %>%
  arrange(n)

falter_cons_hainich <- falter_cons_buf %>% st_intersection(ug[ug$Name == "Hainich",])
falter_cons_jon <- falter_cons_buf %>% st_intersection(ug[ug$Name == "Jonastal",])
falter_cons_krb <- falter_cons_buf %>% st_intersection(ug[ug$Name == "Kriegberg",])

falter_plots_hainich <- plots2 %>% filter(cluster < 6 & !is.na(n))
falter_plots_krb <- plots2 %>% filter(cluster == 6 & !is.na(n))
falter_plots_jon <- plots2 %>% filter(cluster == 7 & !is.na(n))



##### captures maps #####

plots_captures_buf <- st_buffer(plots_captures, dist=0.5)

map_nlp_capt <- ggplot() + 
  geom_spatraster_rgb(data = ortho_nlp, alpha=0.8) +
  geom_sf(data=nlp_auﬂen %>% st_crop(st_bbox(mybox_nlp)), 
          linewidth=1, lty=1, col="black", fill=NA)+
  geom_sf(data=nlp_schutz %>% st_crop(st_bbox(mybox_nlp)), 
          linewidth=0.7, lty=2, col="black", , fill=NA)+
  geom_sf(data=plots_captures_buf %>% filter(cluster < 6 & ncap > 0), 
          aes(col=ncap_rel, fill=ncap_rel), linewidth=0.6, size=1, alpha=0.4)+
  geom_sf(data=plots_captures_buf %>% filter(cluster < 6 & ncap == 0), 
          linewidth=0.6, fill="grey75", alpha=0.4)+
  theme_bw()+
  #scale_fill_viridis_c(option = "magma", trans="reverse")+
  scale_color_distiller(breaks=c(0.5, seq(10, 50, 10)),
                        palette="YlOrRd", direction=1,
                        limits = c(min(plots_captures$ncap_rel), max(plots_captures$ncap_rel)))+
  scale_fill_distiller(breaks=c(0.5, seq(10, 50, 10)),
                       palette="YlOrRd", direction=1,
                       limits = c(min(plots_captures$ncap_rel), max(plots_captures$ncap_rel)))+
  geom_sf(data=falter_complete %>% filter(cluster < 6), fill="black", size= 0.1, alpha=0.25)+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        # legend.direction = "horizontal",
        legend.position.inside = c(1.14, 0.12),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"),
        #legend.background = element_blank(),
        legend.background = element_rect(fill="white", colour ="white"),
        legend.spacing.x = unit(0.15, 'cm'))+
  #ylab("Latitude") + xlab("Longitude") +
  annotation_scale(line_width = 1, style="ticks", location="bl",
                   pad_x = unit(0.75, "cm"),
                   pad_y = unit(0.2, "cm"),)+
  annotation_north_arrow(height=unit(0.65, "cm"),
                         width=unit(0.4, "cm"),
                         location="bl",
                         pad_x = unit(0.16, "cm"),
                         pad_y = unit(0.2, "cm"),
                         style=north_arrow_orienteering(
                           line_width = 1,
                           fill = c("black", "black"),
                           text_size = 8,
                           text_angle = 0
                         ))+
  labs(col="captured individuals \n per ha and hour",
       caption="")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  guides(fill = FALSE)

map_nlp_capt

map_krb_capt <- ggplot() + 
  geom_spatraster_rgb(data = ortho_krb, alpha=0.8) +
  geom_sf(data=plots_captures_buf %>% filter(cluster == 6 & ncap > 0), 
          aes(col=ncap_rel, fill=ncap_rel), linewidth=0.6, size=1, alpha=0.4)+
  geom_sf(data=plots_captures_buf %>% filter(cluster == 6 & ncap == 0), 
          linewidth=0.6, fill="grey75", alpha=0.4)+
  theme_bw()+
  geom_sf(data=falter_complete %>% filter(cluster == 6), 
          fill="black", size= 1, alpha=0.25)+
  #scale_fill_viridis_c(option = "magma", trans="reverse")+
  scale_color_distiller(breaks=c(0.5, seq(10, 50, 10)),
                        palette="YlOrRd", direction=1,
                        limits = c(min(plots_captures$ncap_rel), max(plots_captures$ncap_rel)))+
  scale_fill_distiller(breaks=c(0.5, seq(10, 50, 10)),
                       palette="YlOrRd", direction=1,
                       limits = c(min(plots_captures$ncap_rel), max(plots_captures$ncap_rel)))+
  #scale_fill_continuous(trans = 'reverse', type = "viridis", name="number of connections")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position.inside = c(0.16, 0.1),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.45, "cm"),
        legend.background = element_rect(fill=NA),
        legend.spacing.x = unit(0.15, 'cm'))+
  annotation_scale(line_width = 1, style="ticks", location="br",
                   pad_x = unit(0.33, "cm"),
                   pad_y = unit(1, "cm"),)+
  annotation_north_arrow(height=unit(0.45, "cm"),
                         width=unit(0.3, "cm"),
                         location="br",
                         pad_x = unit(0.33, "cm"),
                         pad_y = unit(0.33, "cm"),
                         style=north_arrow_orienteering(
                           line_width = 1,
                           fill = c("black", "black"),
                           text_size = 8,
                           text_angle = 0
                         ))+
  #ylab("Latitude") + xlab("Longitude") +
  theme(legend.position = "none")+ 
  scale_y_continuous(breaks = c(50.956, 50.96, 50.964, 50.968), expand=c(0,0))+
  scale_x_continuous(breaks = c( 10.638, 10.646), expand=c(0,0))

map_krb_capt


map_jon_capt <- ggplot() + 
  geom_spatraster_rgb(data = ortho_jon, alpha=0.8) +
  geom_sf(data=plots_captures_buf %>% filter(cluster == 7 & ncap > 0), 
          aes(col=ncap_rel, fill=ncap_rel), linewidth=0.6, size=1, alpha=0.4)+
  geom_sf(data=plots_captures_buf %>% filter(cluster == 7 & ncap == 0), 
          linewidth=0.6, fill="grey75", alpha=0.4)+
  theme_bw()+
  geom_sf(data=falter_complete %>% filter(cluster == 7), 
          fill="black", size= 1, alpha=0.25)+
  #scale_fill_viridis_c(option = "magma", trans="reverse")+
  scale_color_distiller(breaks=c(0.5, seq(10, 50, 10)),
                        palette="YlOrRd", direction=1,
                        limits = c(min(plots_captures$ncap_rel), max(plots_captures$ncap_rel)))+
  scale_fill_distiller(breaks=c(0.5, seq(10, 50, 10)),
                       palette="YlOrRd", direction=1,
                       limits = c(min(plots_captures$ncap_rel), max(plots_captures$ncap_rel)))+
  #scale_fill_continuous(trans = 'reverse', type = "viridis", name="number of connections")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position.inside = c(0.16, 0.1),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.45, "cm"),
        legend.background = element_rect(fill=NA),
        legend.spacing.x = unit(0.15, 'cm'))+
  annotation_scale(line_width = 1, style="ticks", location="br",
                   pad_x = unit(0.33, "cm"),
                   pad_y = unit(0.33, "cm"),)+
  annotation_north_arrow(height=unit(0.45, "cm"),
                         width=unit(0.3, "cm"),
                         location="br",
                         pad_x = unit(0.33, "cm"),
                         pad_y = unit(0.9, "cm"),
                         style=north_arrow_orienteering(
                           line_width = 1,
                           fill = c("black", "black"),
                           text_size = 8,
                           text_angle = 0
                         ))+
  #ylab("Latitude") + xlab("Longitude") +
  theme(legend.position = "none")+ 
  scale_y_continuous(breaks = c(50.804, 50.807, 50.810, 50.813), expand=c(0,0))+
  scale_x_continuous(breaks = c(10.835, 10.840,10.845, 10.850), expand=c(0,0))

map_jon_capt



##### network maps #####


### recapture map hainich ###

map_nlp <- ggplot() + 
  geom_spatraster_rgb(data = ortho_nlp, alpha=0.8) +
  geom_sf(data=nlp_auﬂen %>% st_crop(st_bbox(mybox_nlp)), 
          linewidth=1, lty=1, col="black", fill=NA)+
  geom_sf(data=nlp_schutz %>% st_crop(st_bbox(mybox_nlp)), 
          linewidth=0.7, lty=2, col="black", , fill=NA)+
  #annotation_map_tile(type="osm", zoom=14, zoomin=-2, alpha=0.7)+
  geom_sf(data=plots2 %>% filter(cluster <6), alpha=0.3, linewidth=0.5)+
  geom_sf(data=falter_cons_hainich %>% st_transform(crs=4326) , 
          aes(fill=n), col=NA, alpha=1, linewidth=0)+
  geom_sf(data=falter_plots_hainich, aes(fill=n)) + 
  theme_bw()+
  scale_fill_viridis_c(trans = 'reverse', limits = c(6, 1))+
  #scale_fill_fermenter(palette="viridis", n.breaks=6, limits=c(1,6))+
  #scale_fill_continuous(trans = 'reverse', type = "viridis", name="number of connections")+
  
  theme(
        axis.text.y=element_blank(),
        axis.text=element_text(size=6),
        axis.title=element_text(size=6),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        legend.position.inside = c(1.14, 0.12),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"),
        #legend.background = element_blank(),
        legend.background = element_rect(fill="white", colour ="white"),
        legend.spacing.x = unit(0.15, 'cm'))+
  #ylab("Latitude") + xlab("Longitude") +
  guides(fill = guide_legend(
    theme(legend.title.position = "number of\nrecaptures")
  )) + 
  annotation_scale(line_width = 1, style="ticks", location="bl",
                   pad_x = unit(0.75, "cm"),
                   pad_y = unit(0.2, "cm"),)+
  annotation_north_arrow(height=unit(0.65, "cm"),
                         width=unit(0.4, "cm"),
                         location="bl",
                         pad_x = unit(0.16, "cm"),
                         pad_y = unit(0.2, "cm"),
                         style=north_arrow_orienteering(
                           line_width = 1,
                           fill = c("black", "black"),
                           text_size = 8,
                           text_angle = 0
                         ))+
  labs(#title = "Hainich", 
    caption = "Orthophotos March 2020: (c) GDI-TH")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))

map_nlp

### recapture map kriegberg ###

map_krb <- ggplot() + 
  geom_spatraster_rgb(data = ortho_krb, alpha=0.8) +
  
  #annotation_map_tile(type="osm", zoom=16, zoomin=-2, alpha=0.7)+
  geom_sf(data=plots2 %>% filter(cluster == 6), alpha=0.3, linewidth=0.5)+
  geom_sf(data=falter_cons_krb %>% st_transform(crs=4326) , 
          aes(fill=n), col=NA, alpha=1, linewidth=0)+
  geom_sf(data=falter_plots_krb, aes(fill=n)) + 
  theme_bw()+
  scale_fill_viridis_c(trans = 'reverse', limits = c(6, 1))+
  #scale_fill_continuous(trans = 'reverse', type = "viridis", name="number of connections")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "inside",
        legend.direction = "vertical",
        legend.position.inside = c(0.16, 0.1),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.45, "cm"),
        legend.background = element_rect(fill=NA),
        legend.spacing.x = unit(0.15, 'cm'))+
  annotation_scale(line_width = 1, style="ticks", location="br",
                   pad_x = unit(0.33, "cm"),
                   pad_y = unit(1, "cm"),)+
  annotation_north_arrow(height=unit(0.45, "cm"),
                         width=unit(0.3, "cm"),
                         location="br",
                         pad_x = unit(0.33, "cm"),
                         pad_y = unit(0.33, "cm"),
                         style=north_arrow_orienteering(
                           line_width = 1,
                           fill = c("black", "black"),
                           text_size = 8,
                           text_angle = 0
                         ))+
  #ylab("Latitude") + xlab("Longitude") +
  theme(legend.position = "none",
        axis.text.y=element_blank())+ 
  scale_y_continuous(breaks = c(50.956, 50.96, 50.964, 50.968), expand=c(0,0))+
  scale_x_continuous(breaks = c(10.638, 10.646), expand=c(0,0))

map_krb


### recapture map jonastal ###

map_jon <- ggplot() + 
  geom_spatraster_rgb(data = ortho_jon, alpha=0.8) +
  
  #annotation_map_tile(type="osm", zoom=16, zoomin=-2, alpha=0.7)+
  geom_sf(data=plots2 %>% filter(cluster == 7), alpha=0.3, linewidth=0.5)+
  geom_sf(data=falter_cons_jon %>% st_transform(crs=4326) , 
          aes(fill=n), col=NA, alpha=1, linewidth=0)+
  geom_sf(data=falter_plots_jon, aes(fill=n)) + 
  theme_bw()+
  scale_fill_viridis_c(trans = 'reverse', limits = c(6, 1))+
  #scale_fill_continuous(trans = 'reverse', type = "viridis", name="number of connections")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "inside",
        legend.direction = "vertical",
        legend.position.inside = c(0.16, 0.1),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.45, "cm"),
        legend.background = element_rect(fill=NA),
        legend.spacing.x = unit(0.15, 'cm'))+
  annotation_scale(line_width = 1, style="ticks", location="br",
                   pad_x = unit(0.33, "cm"),
                   pad_y = unit(0.33, "cm"),)+
  annotation_north_arrow(height=unit(0.45, "cm"),
                         width=unit(0.3, "cm"),
                         location="br",
                         pad_x = unit(0.33, "cm"),
                         pad_y = unit(0.9, "cm"),
                         style=north_arrow_orienteering(
                           line_width = 1,
                           fill = c("black", "black"),
                           text_size = 8,
                           text_angle = 0
                         ))+
  #ylab("Latitude") + xlab("Longitude") +
  theme(legend.position = "none")+ 
  scale_y_continuous(breaks = c(50.804, 50.807, 50.810, 50.813), expand=c(0,0))+
  scale_x_continuous(breaks = c(10.835, 10.840,10.845, 10.850), expand=c(0,0))

map_jon


##### combine all plots #####

png("figures/figure3_maps_jon_krb_nlp.png", width=4500, height=5200, res=600)
  ggarrange(
    ggarrange(NULL, 
      map_jon_capt, 
      NULL,
    ggarrange(
      NULL, map_krb_capt, map_krb, nrow=1, widths = c(0.1, 0.5, 0.5), align="v"),
    widths=c(0.05, 0.5, 0.05, 1.1), nrow=1, labels=c("", "A", " ", "B")),
    NULL,
    ggarrange( 
      map_nlp_capt, NULL, map_nlp, nrow=1, 
      widths = c(1, -0.015, 1), align="v"),
    nrow=3, heights=c(0.55,-0.02,1.4), labels = c("", "", "C"))
dev.off()


