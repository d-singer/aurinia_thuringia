library(tidyverse)
library(data.table)


all <- fread("data/hainich_daily_estimates.csv") %>% mutate(sex = "total")
females <- fread("data/hainich_daily_estimates_females.csv")%>% mutate(sex = "female")
males <- fread("data/hainich_daily_estimates_males.csv")%>% mutate(sex = "male")


df <- rbind(males, females, all)


a <- ggplot(data=df, 
            aes(x=date, y=p, group=sex)) + 
  
  
  theme_bw() +
  geom_line(lwd=1, aes(col=sex)) +
  
  geom_ribbon(aes(ymin = p.lcl, ymax = p.ucl, fill=sex), col=NA, alpha=0.15) +
  geom_point(size=2, fill="white", aes(col=sex, shape=sex)) +
  scale_fill_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_color_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_shape_manual(values=c(25,24,21))+
  theme(legend.position = "none")

a


c <- ggplot(data=df, 
            aes(x=date, y=Phi, group=sex)) + 
 
 
  theme_bw() +
  geom_line(lwd=1, aes(col=sex)) +
  
  geom_ribbon(aes(ymin = Phi.lcl, ymax = Phi.ucl, fill=sex), col=NA, alpha=0.15) +
  geom_point(size=2, fill="white", aes(col=sex, shape=sex)) +
  scale_fill_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_color_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_shape_manual(values=c(25,24,21))+
  theme(legend.position = "none")
c

d <- ggplot(data=df, 
            aes(x=date, y=N, group=sex)) + 
  theme_bw() +
  geom_line(lwd=1, aes(col=sex)) + 
  
  geom_ribbon(aes(ymin = N.lcl, ymax = N.ucl, fill=sex), col=NA, alpha=0.15) +
  geom_point(size=2, fill="white", aes(col=sex, shape=sex)) +
  scale_fill_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_color_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_shape_manual(values=c(25,24,21))+
  theme(legend.position = "bottom")
d

all <- fread("data/kriegberg_daily_estimates.csv") %>% mutate(sex = "total")
females <- fread("data/kriegberg_daily_estimates_females.csv")%>% mutate(sex = "female")
males <- fread("data/kriegberg_daily_estimates_males.csv")%>% mutate(sex = "male")


df <- rbind(males, females, all)


e <- ggplot(data=df, 
            aes(x=date, y=p, group=sex)) + 
  
  
  theme_bw() +
  geom_line(lwd=1, aes(col=sex)) +
  
  geom_ribbon(aes(ymin = p.lcl, ymax = p.ucl, fill=sex), col=NA, alpha=0.15) +
  geom_point(size=2, fill="white", aes(col=sex, shape=sex)) +
  scale_fill_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_color_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_shape_manual(values=c(25,24,21))+
  theme(legend.position = "bottom")+
  xlim(c(ymd(20200517), ymd(20200609)))

e

f <- ggplot(data=df, 
            aes(x=date, y=Phi, group=sex)) + 
  
  
  theme_bw() +
  geom_line(lwd=1, aes(col=sex)) +
  
  geom_ribbon(aes(ymin = Phi.lcl, ymax = Phi.ucl, fill=sex), col=NA, alpha=0.15) +
  geom_point(size=2, fill="white", aes(col=sex, shape=sex)) +
  scale_fill_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_color_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_shape_manual(values=c(25,24,21))+
  theme(legend.position = "bottom")+
  xlim(c(ymd(20200517), ymd(20200609)))
f

g <- ggplot(data=df, 
            aes(x=date, y=N, group=sex)) + 
  theme_bw() +
  geom_line(lwd=1, aes(col=sex)) + 
  
  geom_ribbon(aes(ymin = N.lcl, ymax = N.ucl, fill=sex), col=NA, alpha=0.15) +
  geom_point(size=2, fill="white", aes(col=sex, shape=sex)) +
  scale_fill_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_color_manual(values=c( "#2CA02C","#1F77B4", "#D55E00"))+
  scale_shape_manual(values=c(25,24,21))+
  theme(legend.position = "none")+
  xlim(c(ymd(20200517), ymd(20200609)))
g



png("figures/figureX_daily_estimates.png", width=5000, height=3500, res=600)
ggpubr::ggarrange(a,c, d, e,f, g, align="hv", ncol=3, nrow=2, labels=c("A", "", "", "B", ""))
dev.off()

png("figures/figure_daily_estimates.png", width=5000, height=3500, res=600)
ggpubr::ggarrange(a, d, e, g, align="v", ncol=2, nrow=2, labels=c("A", "", "B", ""))
dev.off()

png("figures/figure_daily_estimates_hainich.png", width=4700, height=2000, res=600)
ggpubr::ggarrange(a, d, align="hv", ncol=2, nrow=1)
dev.off()

