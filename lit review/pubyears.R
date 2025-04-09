rm(list=ls())
lapply(c("ggthemes","tidyverse","viridis",
         "ggplot2","cowplot","lme4",
         "grid","ggrepel","multcomp","emmeans"),require,character.only=T)

f=read_csv("~/Documents/Science/facil-wos.csv"); head(f)
f = f %>% rename_at(vars(c("Count")), ~ c("Fcount"))

c=read_csv("~/Documents/Science/comp-wos.csv"); head(c)
c = c %>% rename_at(vars(c("Count")), ~ c("Ccount"))

j=full_join(f, c); head(j)
j =j%>% pivot_longer(cols = c("Fcount","Ccount"),
                     names_to = "group",
                     values_to = "count")

head(j)
j = j %>% filter(PublicationYears !=2025)
ggplot(j, aes(x=PublicationYears, y=count, color=group))+
  geom_point()+geom_line()+theme_bw()+
  geom_vline(xintercept=1994)+
  theme(legend.position = "none")

je = j %>% filter(PublicationYears <= 1975)
head(je)
ggplot(je, aes(x=PublicationYears, y=count, color=group))+
  geom_point()+geom_line()+theme_bw()

jl = j %>% filter(PublicationYears >= 1975)
head(jl)
ggplot(jl, aes(x=PublicationYears, y=count, color=group))+
  geom_point()+geom_line()+theme_bw()
