source("R/setup.R")

##### import study geodata #####

falter_complete <- st_read("data/aurinia/aurinia_observations_complete.shp")

# filter for individuals that have been recaptured (within regular sampling periods at plots)
falter_recapt <- falter_complete %>% group_by(id) %>%  st_cast("MULTIPOINT") %>% 
  filter(length(id) > 1) %>% st_transform(crs=25832) 

# simplify geometries of observations for plot-network visualisation (location = plot centroid)
falter_recapt <- falter_recapt %>% st_drop_geometry %>%
  merge(plot_centroids %>% select(plot_id, geometry), by="plot_id") %>% 
  st_as_sf() %>% arrange(id, timestamp)

st_write(falter_recapt, "data/aurinia/aurinia_observations_recaptures.shp")

##### calculate flight lines and distances #####

# empty data frame
falter_lines <- falter_complete[0,0]

# filter first recaptures
falter_recapt1 <- falter_recapt %>% filter(event_id %in% c("event1", "event2")) %>% 
  group_by(id) %>% mutate(n = length(id)) %>% filter(n == 2) %>% select(!n)

for(i in unique(falter_recapt1$id))
{
  mydat <- falter_recapt1 %>% filter(id == i) %>% group_by(id)
  
  myline <- mydat %>% group_by(id) %>% st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>%
    mutate(id = i,
           sex = mydat$sex[1]) 
  
  myline$minlength = st_length(myline) %>% as.numeric()
  
  myline$diffdays = as.numeric(date(mydat$timestamp[2]) - date(mydat$timestamp[1]))
  
  myline$event = "r1"
  
  myline_cent <- plot_centroids %>% filter(plot_id %in% c(mydat$plot_id)) %>% 
    st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>%
    mutate(id = i,
           sex = mydat$sex[1]) 
  
  myline_cent$centlength = st_length(myline_cent) %>% as.numeric()
  
  myline$interplot = ifelse(mydat$plot_id[1] == mydat$plot_id[2], 0, 1)
  myline$centlength = myline_cent$centlength
  #falter_lines <- ifelse(a == 1, myline, rbind(myline, falter_lines))
  
  falter_lines <- rbind(myline, falter_lines)
  
}

# filter second recaptures
falter_recapt2 <- falter_recapt %>% filter(event_id %in% c("event2", "event3")) %>% 
  group_by(id) %>% mutate(n = length(id)) %>% filter(n == 2) %>% select(!n)


for(i in unique(falter_recapt2$id))
{
  mydat <- falter_recapt2 %>% filter(id == i) %>% group_by(id)
  
  myline <- mydat %>% group_by(id) %>% st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>%
    mutate(id = i,
           sex = mydat$sex[1]) 
  
  myline$minlength = st_length(myline) %>% as.numeric()
  
  myline$diffdays = as.numeric(date(mydat$timestamp[2]) - date(mydat$timestamp[1]))
  
  myline$event = "r2"
  
  myline_cent <- plot_centroids %>% filter(plot_id %in% c(mydat$plot_id)) %>% 
    st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>%
    mutate(id = i,
           sex = mydat$sex[1]) 
  
  myline_cent$centlength = st_length(myline_cent) %>% as.numeric()
  
  myline$interplot = ifelse(mydat$plot_id[1] == mydat$plot_id[2], 0, 1)
  myline$centlength = myline_cent$centlength
  #falter_lines <- ifelse(a == 1, myline, rbind(myline, falter_lines))
  
  falter_lines <- rbind(myline, falter_lines)
  
}

# filter third recaptures
falter_recapt3 <- falter_recapt %>% filter(event_id %in% c("event3", "event4")) %>% 
  group_by(id) %>% mutate(n = length(id)) %>% filter(n == 2) %>% select(!n)

for(i in unique(falter_recapt3$id))
{
  mydat <- falter_recapt3 %>% filter(id == i) %>% group_by(id)
  
  myline <- mydat %>% group_by(id) %>% st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>%
    mutate(id = i,
           sex = mydat$sex[1]) 
  
  myline$minlength = st_length(myline) %>% as.numeric()
  
  myline$diffdays = as.numeric(date(mydat$timestamp[2]) - date(mydat$timestamp[1]))
  
  myline$event = "r3"
  
  myline_cent <- plot_centroids %>% filter(plot_id %in% c(mydat$plot_id)) %>% 
    st_union() %>% 
    st_cast("MULTIPOINT") %>% st_cast("LINESTRING") %>%
    st_as_sf() %>% st_transform(crs= 25832) %>%
    mutate(id = i,
           sex = mydat$sex[1]) 
  
  myline_cent$centlength = st_length(myline_cent) %>% as.numeric()
  
  myline$interplot = ifelse(mydat$plot_id[1] == mydat$plot_id[2], 0, 1)
  myline$centlength = myline_cent$centlength
  #falter_lines <- ifelse(a == 1, myline, rbind(myline, falter_lines))
  
  falter_lines <- rbind(myline, falter_lines)
  
}

st_write(falter_lines, "data/aurinia/aurinia_recaptures_lines.shp", append=F)


##### figure flight distances #####

nrow(falter_lines)

nrow(falter_lines[falter_lines$interplot == 1,])

test <- falter_lines[falter_lines$interplot == 1 & 
                       falter_lines$sex != "unknown",]

wilcox.test(test$centlength ~ test$sex)

uniqueN(falter_lines$id)

length(falter_lines$centlength[falter_lines$interplot == 0])

mean(falter_lines$centlength[falter_lines$interplot == 1])
max(falter_lines$centlength[falter_lines$interplot == 1])
sd(falter_lines$centlength[falter_lines$interplot == 1])

mean(falter_lines$centlength[falter_lines$sex == "male" & falter_lines$interplot == 1])
sd(falter_lines$centlength[falter_lines$sex == "male" & falter_lines$interplot == 1])

mean(falter_lines$centlength[falter_lines$sex == "female"&falter_lines$interplot == 1])
sd(falter_lines$centlength[falter_lines$sex == "female"&falter_lines$interplot == 1])

max(falter_lines$centlength)

falter_lines_all <- falter_lines
falter_lines_all$sex <- "total"

male_interplot <- falter_lines %>% filter(sex == "male" & interplot == 1) %>% nrow()
female_interplot <- falter_lines %>% filter(sex == "female" & interplot == 1) %>% nrow()
male_intraplot <- falter_lines %>% filter(sex == "male" & interplot == 0) %>% nrow()
female_intraplot <- falter_lines %>% filter(sex == "female" & interplot ==0) %>% nrow()

recapt_mat <- matrix(c(male_intraplot, male_interplot, 
                       female_intraplot, female_interplot), nrow = 2, byrow = TRUE)

# Chi-Squared Test
rownames(recapt_mat) <- c("male", "demale")
colnames(recapt_mat) <- c("intra-plot-recaptures", "intra-plot-recaptures")

result <- chisq.test(recapt_mat)
print(result)

p_male <- round(male_interplot/(male_interplot+male_intraplot), digits=2)*100
p_female <- round(female_interplot/(female_interplot+female_intraplot), digits=2)*100


hist <- ggplot(falter_lines %>% filter(sex != "unknown" ), 
               aes(x=centlength, fill=sex)) + 
  geom_histogram(breaks=c(-25,25, 173, seq(174, 1400, 175)),
                 col="black", position="stack")+
  xlab("Minimum flight distance (m)") +
  ylab("Number of individuals")+
  theme_bw() +
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks=seq(0, 1300,175),
                     limits=c(-25, 1400))+
  geom_text(x=50, y=20, label="Intra-plot-recaptures", size=3, angle=90)+
  geom_text(x=125, y=20, label="Inter-plot-recaptures", size=3, angle=90)+
  geom_vline(xintercept=94, lty=2)+
  scale_fill_manual(values=c("#2CA02C","#1F77B4"))

hist

violin <- ggplot(falter_lines  %>% filter(sex != "unknown" & 
                                            centlength > 0), 
                 aes(y=centlength, x=sex, fill=sex)) + 
  geom_violin()+
  geom_jitter(data= falter_lines  %>% filter(sex != "unknown"),
              width=0.3, height=5, alpha=0.5) +
  stat_mean(pch=24, size=2.5, fill="grey75")+
  theme(legend.position = "none") +
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(breaks=c(0, seq(175, 1400,175)),
                     limits=c(-25, 1400))+
  geom_hline(yintercept=94, lty=2)+
  geom_text(x=1, y=45, label=paste0(100-p_female, " %"), size=2.5)+
  geom_text(x=2, y=45, label=paste0(100-p_male, " %"), size=2.5)+
  geom_text(x=1, y=135, label=paste0(p_female, " %"), size=2.5)+
  geom_text(x=2, y=135, label=paste0(p_male, " %"), size=2.5)+
  coord_flip()+
  scale_fill_manual(values=c( "#2CA02C","#1F77B4"))
violin


png("figures/figure2_flight_distances.png", res=300, width=2500, height=1800)
ggarrange(violin, ggplot() + theme_void(), hist, align = c("hv"), nrow=3, 
          heights=c(0.9,-0.25,1.2), 
          labels=c("A", "", "B"))
dev.off()

