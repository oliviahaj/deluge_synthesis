####### setup ########
library(knitr)
library(tidyverse)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)
library(lmerTest)
library(lme4)
library(RColorBrewer)
library(rstatix)
library(tidyr)
library(ggpubr)
library(lubridate)
library(ggtext)
library(ggh4x)
library(ggpattern)
library(data.table)

plot_theme<-theme_classic()+
  theme(
    strip.background=element_blank(),
    strip.text.x=element_text(size=20,
                              hjust=0,
                              face="bold",colour="black"),
    strip.text.y=element_text(size=20,
                              hjust=0,
                              face="bold",colour="black"),
    panel.background=element_rect(fill="transparent"),
    panel.border=element_rect(fill="transparent",color="black",
                              linewidth=2),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",
                                   linewidth=0.5),
    legend.position="none",
    # legend.title=element_text(size=30,face="bold"),
    legend.text=element_text(size=16),
    axis.line.x=element_line(linewidth=1,color="black"),
    axis.line.y=element_blank(),
    axis.ticks=element_line(linewidth=1,color="black"),
    axis.title = element_text(size=20,
                              colour="black",face="bold"),
    axis.text=element_text(size=16,colour="black"),
    axis.text.x=element_text(angle=45,hjust=1,colour="black"),
    axis.text.y=element_text(colour="black"),
    plot.title=element_text(size=20),
    strip.placement="outside")

#### Daily precip data ####
daily.precip<-read.csv("/Users/Kathy/Desktop/Complete Project Files/Condon and Knapp 2026 - Subtle differences in spring soil moisture - Ecosphere/Condon-Knapp_2026_Ecosphere/ms_data_analysis/SGS_Precip_Combined.csv")

daily.precip<-daily.precip%>%
  mutate(new.Date=as.Date(new.Date,format="%m/%d/%y"))%>%
  mutate(ymd.date=ymd(new.Date))%>%
  group_by(yr=year(ymd.date),mon=month(ymd.date))

## Daily to monthly
daily.to.monthly<-daily.precip%>%
  filter(yr %in% c(1990:2024))%>%
  group_by(yr,mon)%>%
  dplyr::summarise(
    n.days=n(),
    noaa_LTA=sum(NOAA_mm2),
    usda_annual=sum(usda_20to21))%>%
  mutate(mon=as.character(mon))

str(daily.to.monthly)

monthly<-daily.to.monthly%>%
  mutate(mon=factor(mon,levels=c("1","2","3","4","5","6","7","8","9","10","11","12")))%>%
  mutate(USDA_20=case_when(yr == 2020 ~ usda_annual))%>%
  mutate(USDA_21=case_when(yr == 2021 ~ usda_annual))

str(monthly)

#### Averages ####
origin.df<-data.frame(
  data=c("web_mm","noaa_LTA","USDA_20","USDA_21"),
  mon=0,
  csum=0)%>%
  mutate(mon=factor(mon))

monthly.avs<-monthly%>%
  pivot_longer(cols=c("noaa_LTA","USDA_20","USDA_21"),names_to="data",values_to="precip_mm")%>%
  group_by(data,mon)%>%
  drop_na("precip_mm")%>%
  dplyr::summarise(
    n.yrs=n(),
    monthly.av=mean(precip_mm),
    monthly.sd=sd(precip_mm),
    monthly.SE=monthly.sd/sqrt(n.yrs))%>%
  mutate(data=factor(data))%>%
  mutate(csum=cumsum(monthly.av))%>%
  mutate(data=fct_relevel(data,"noaa_LTA","USDA_20","USDA_21"))

# view(monthly.avs)

monthly.avs.merge<-bind_rows(origin.df,monthly.avs,)

an.tot<-daily.precip%>%
  mutate(ymd.date=ymd(new.Date))%>%
  group_by(year=year(ymd.date))%>%
  summarise(
    NOAA_ap=sum(NOAA_mm2),
    USDA_ap=sum(usda_20to21))
view(an.tot)

#### Daily GS ####
gs.all.yrs<-daily.precip%>%
  mutate(ymd.date=ymd(new.Date))%>%
  group_by(year=year(ymd.date),mon=month(ymd.date))%>%
  filter(ymd.date %within%
           interval (ymd(paste(first(year),
                               "04-01",
                               sep="-")),
                     ymd(paste(first(year),
                               "09-15",
                               sep="-"))))%>%
  summarise(
    NOAA_gs=sum(NOAA_mm2),
    USDA_gs=sum(usda_20to21))%>%
  pivot_longer(cols=c("NOAA_gs","USDA_gs"),names_to="data",values_to="mm")%>%
  drop_na("mm")%>%
  mutate(groups=case_when(
    year == 2021 & data == "USDA_gs" ~ "USDA_21",
    year == 2020 & data == "USDA_gs" ~ "USDA_20",
    TRUE ~ data))

# view(gs.all.yrs)

# gs.tot<-daily.precip%>%
#   mutate(ymd.date=ymd(new.Date))%>%
#   group_by(year=year(ymd.date))%>%
#   filter(ymd.date %within%
#            interval (ymd(paste(first(year),
#                                "04-01",
#                                sep="-")),
#                      ymd(paste(first(year),
#                                "09-15",
#                                sep="-"))))%>%
#   summarise(NOAA_gs=sum(NOAA_mm2),
#             USDA_gs=sum(usda_20to21))

gs.avs<-gs.all.yrs%>%
  group_by(groups,mon)%>%
  dplyr::summarise(
    n=n(),
    gs.av=mean(mm),
    gs.sd=sd(mm),
    gs.SE=gs.sd/sqrt(n))%>%
  mutate(csum=cumsum(gs.av))%>%
  rename(data=groups)%>%
  mutate(data=factor(data,levels=c("NOAA_gs","USDA_20","USDA_21")))%>%
  mutate(mon=factor(mon))

gs.avs.merge<-bind_rows(origin.df,gs.avs)


########## precip plotting ##############
#### Plotting monthly bars & cumu line ####
gr_dat<-monthly.avs%>%
  filter(!is.na(mon))%>%
  filter(data %in% c("noaa_LTA","USDA_21"))%>%
  rename(col=data)

monthly.precip.bars<-ggplot(data=gr_dat,color=col,group=col)+
  geom_line(data=gr_dat,
            aes(x=mon,
                y=csum,
                color=col,
                group=col,
                linetype=col),
            linewidth=0.75,
            inherit.aes=FALSE)+
  scale_linetype_manual(name="",
                        values=c("dashed","solid"),
                        labels=c("35-yr average","2021"))+
  geom_bar(aes(x=mon,y=monthly.av,fill=col,color=col),
           position=position_dodge(0.75),
           stat="identity",
           width=0.75,linewidth=0.75)+
  geom_errorbar(aes(x=mon,color=col,ymin=monthly.av-monthly.SE,ymax=monthly.av+monthly.SE),
                position=position_dodge(0.75),width=0,
                linewidth=0.75)+
  scale_fill_discrete(name="",
                      type=c("gray70","#98c1d9"),
                      # type=c("#3d5a80","#ee6c4d"),
                      # type=c("#98c1d9","#3d5a80","#ee6c4d"),
                      # labels=c("A","D"),
                      labels=c("35-yr average","2021"),
                      # labels=c("A","A+D","D+D")
  )+
  scale_color_discrete(name="",
                       type=c("gray30","#698696"),
                       # type=c("#1f2d40","#ab4e37")
                       # type=c("#698696","#1f2d40","#ab4e37"),
                       labels=c("35-yr average","2021")
  )+
  labs(y=expression(paste(bold("Precipitation (mm)"))),x="Date")+
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),expand=c(0,0))+
  scale_y_continuous(
    limits=c(0,415),
    expand=c(0,0)
  )+
  plot_theme+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position="inside",
        legend.position.inside=c(0.14,0.9),
        title=element_text(face="bold"))+
  force_panelsizes(rows=unit(4,"in"),
                   cols=unit(8,"in"))

monthly.precip.bars

# ggsave(monthly.precip.bars,filename="ms_fig_panels/MS_2021_Fig1_Precip_bars.pdf",bg="transparent",width=9,height=5)

#### precip deluge distributions ####
precip_20plus<-daily.precip%>%
  mutate(ymd.date=ymd(new.Date))%>%
  filter(yr %in% c(1990:2024))%>%
  group_by(year=year(ymd.date),mon=month(ymd.date))%>%
  filter(ymd.date %within%
           interval (ymd(paste(first(year),
                               "04-01",
                               sep="-")),
                     ymd(paste(first(year),
                               "09-15",
                               sep="-"))))%>%
  mutate(event_size = case_when(
    yr %in% c(1990:2019) ~ NOAA_mm2,
    yr %in% c(2020:2021) ~ usda_20to21,
    yr %in% c(2022:2024) ~ NOAA_mm2))%>%
  mutate(rel_events = case_when(
    event_size <= 2 ~ 0,
    event_size > 2 ~ event_size))

gr_dat<-precip_20plus%>%
  ungroup()%>%
  mutate(group=rleid(rel_events > 0))%>%
  group_by(group)%>%
  summarise(n.days=n(),
            start_date=min(new.Date),
            end_date=max(new.Date),
            total_precip=sum(rel_events))%>%
  filter(total_precip>0)%>%
  filter(total_precip>20)

# view(gr_dat)

hist<-ggplot(aes(x=total_precip),data=gr_dat)+
  geom_histogram(binwidth=10,fill="gray70",color="gray30",
                 linewidth=0.75)+
  labs(y="# Events",x="Event size (mm)",title="")+
  theme_classic()+
  plot_theme+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position="inside",
        legend.position.inside=c(0.1,0.9),
        title=element_text(face="bold"))+
  scale_x_continuous(expand=c(0.05,0.05),
                     breaks=c(0,20,40,60,80,100,120))+
  scale_y_continuous(limits=c(0,36),
                     breaks=c(0,5,10,15,20,25,30,35),
                     expand=c(0,0))+
  force_panelsizes(rows=unit(4,"in"),
                   cols=unit(4.5,"in"))

hist

# ggsave(hist,filename="ms_fig_panels/MS_2021_Fig1_Deluge_hist.pdf",bg="transparent",width=5.5,height=5)

#### calc percentiles ####
quant.dat<-daily.precip%>%
  mutate(ymd.date=ymd(new.Date))%>%
  group_by(year=year(ymd.date),mon=month(ymd.date))%>%
  filter(ymd.date %within%
           interval (ymd(paste(first(year),
                               "04-01",
                               sep="-")),
                     ymd(paste(first(year),
                               "09-15",
                               sep="-"))))%>%
  mutate(event_size = case_when(
    yr %in% c(1990:2019) ~ NOAA_mm2,
    yr %in% c(2020:2021) ~ usda_20to21,
    yr %in% c(2022:2024) ~ NOAA_mm2))%>%
  filter(event_size>=2)

vals<-ecdf(quant.dat$event_size)
vals(60)

quant.dat<-quant.dat%>%
  mutate(event_quant=vals(event_size))

quantile(quant.dat$event_size,c(.25,.5,.75,.98,.99))
