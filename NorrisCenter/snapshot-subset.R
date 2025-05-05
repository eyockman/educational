# look at specific species if I wanted
# run snapshot code to line 249
dat2$modified2 = as.Date(dat2$modified)
fallqtr = as.Date("2024-09-26") # make object called fallqtr and assign it a date value (first day of fall qtr)
winqtr = as.Date("2025-01-06") # same with winter quarter
sprqtr= as.Date("2025-03-31") # and with spring!


# define periods to highlight on a graph 
highlight_periods = data.frame( # make a dataset called highlight_periods and use the data.frame() function to start making our dataframe
  start = fallqtr, # set miniumum date to the fallqtr value (already in Date form)
  end = as.Date("2025-05-01"), # this is the last day Al got his data
  lower = -Inf, # negative infinity, we want our highlight to just keep going
  upper = Inf, # positive infinity 
  highlightcolor ="red") # fill

ggplot(dat2, aes(x=modified2))+ # histogram?
  geom_rect(data = highlight_periods, # highlight part of the graph using my highlight_periods data fram
            aes(xmin = start, xmax = end, # use the start and end days for highlight
                ymin = lower, ymax = upper, # put upper and lower bounds
                fill=highlightcolor), # fill
            alpha = 0.3, inherit.aes = FALSE)+
  scale_fill_manual(values=c("red"))+ # red is how long we've been going for
  geom_histogram(fill="darkgreen", bins=80)+
  theme_few()+
  theme(legend.position = "none")+
  labs(x="When Occurence last updated (errors)", y="Number of Specimens")+
  scale_y_continuous(labels = comma)+
  xlim(as.Date("2016-01-01"), as.Date("2025-11-01"))

# what if I want to look just at Danthonia
colnames(dat2)
fav = dat2 %>% filter(genus=="Spergularia")
N=nrow(fav)
print(fav %>% select(catalogNumber, genus, specificEpithet, recordedBy, stateProvince, locality),n=15)
unique(fav$specificEpithet)

fav2 = fav %>% group_by(genus, specificEpithet) %>% summarize(n=n()); head(fav2)

ggplot(fav2, aes(x=specificEpithet, y=n, fill=specificEpithet))+
  geom_col()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=6),
        axis.ticks.x=element_blank(),
        legend.position = "none")

users = c("eyockman","abedelst@ucsc.edu","abcwrigh","benskizzle","tchennes","scheim","jlhester","sophia_chu","rinataylor","alabucay","jlmoes","Moiraceae","LiNemHoo","elfaulkn","MaxOWSchroeder","malcolmwang05","harrisonbrand","ananyaj","MOOSE","alysha731","owengoldberg","mjlima","imparker","misslittlepie")

interns = dat2 %>% filter(recordEnteredBy %in% users)
nrow(interns) # 1020 records by us!
unique(interns$recordEnteredBy)

ggplot(interns, aes(x=modified2))+
  geom_histogram(fill="purple")+
  theme_bw()

interns2 = interns %>% group_by(family, genus) %>% summarize(n=n()); head(interns2)
ggplot(interns2, aes(x=family, y=n, fill=genus))+
  geom_col()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=6),
        axis.ticks.x=element_blank(),
        legend.position = "none")

interns3 = interns %>% group_by(recordEnteredBy) %>% summarize(n=n()); head(interns3)
ggplot(interns3, aes(x=recordEnteredBy, y=n, fill=recordEnteredBy))+
  geom_col()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=6),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual(values=c("scheim" = "red",
                              "benskizzle" = "blue",
                              "abcwrigh" = "darkgreen",
                              "tchennes" = "purple"))

interns4 = interns %>% group_by(modified2) %>% summarize(n=n())
head(interns4,22)
interns4$n2[interns4$modified2=="2024-11-21"] = interns4$n[interns4$modified2=="2024-11-21"]
interns4$n2[interns4$modified2=="2025-02-12"] = interns4$n[interns4$modified2=="2025-02-12"] +interns4$n2[interns4$modified2=="2024-11-21"]
interns4$n2[interns4$modified2=="2025-02-13"] = interns4$n[interns4$modified2=="2025-02-13"] +interns4$n2[interns4$modified2=="2025-02-12"]
interns4$n2[interns4$modified2=="2025-02-14"] = interns4$n[interns4$modified2=="2025-02-14"] +interns4$n2[interns4$modified2=="2025-02-13"]
interns4$n2[interns4$modified2=="2025-02-18"] = interns4$n[interns4$modified2=="2025-02-18"] +interns4$n2[interns4$modified2=="2025-02-14"]
interns4$n2[interns4$modified2=="2025-02-19"] = interns4$n[interns4$modified2=="2025-02-19"] +interns4$n2[interns4$modified2=="2025-02-18"]
interns4$n2[interns4$modified2=="2025-02-20"] = interns4$n[interns4$modified2=="2025-02-20"] +interns4$n2[interns4$modified2=="2025-02-19"]
interns4$n2[interns4$modified2=="2025-02-21"] = interns4$n[interns4$modified2=="2025-02-21"] +interns4$n2[interns4$modified2=="2025-02-20"]
interns4$n2[interns4$modified2=="2025-02-23"] = interns4$n[interns4$modified2=="2025-02-23"] +interns4$n2[interns4$modified2=="2025-02-21"]
interns4$n2[interns4$modified2=="2025-02-25"] = interns4$n[interns4$modified2=="2025-02-25"] +interns4$n2[interns4$modified2=="2025-02-23"]
interns4$n2[interns4$modified2=="2025-03-04"] = interns4$n[interns4$modified2=="2025-03-04"] +interns4$n2[interns4$modified2=="2025-02-25"]
interns4$n2[interns4$modified2=="2025-03-05"] = interns4$n[interns4$modified2=="2025-03-05"] +interns4$n2[interns4$modified2=="2025-03-04"]
interns4$n2[interns4$modified2=="2025-03-06"] = interns4$n[interns4$modified2=="2025-03-06"] +interns4$n2[interns4$modified2=="2025-03-05"]
interns4$n2[interns4$modified2=="2025-03-07"] = interns4$n[interns4$modified2=="2025-03-07"] +interns4$n2[interns4$modified2=="2025-03-06"]
interns4$n2[interns4$modified2=="2025-03-11"] = interns4$n[interns4$modified2=="2025-03-11"] +interns4$n2[interns4$modified2=="2025-03-07"]
interns4$n2[interns4$modified2=="2025-03-12"] = interns4$n[interns4$modified2=="2025-03-12"] +interns4$n2[interns4$modified2=="2025-03-11"]
interns4$n2[interns4$modified2=="2025-03-13"] = interns4$n[interns4$modified2=="2025-03-13"] +interns4$n2[interns4$modified2=="2025-03-12"]
interns4$n2[interns4$modified2=="2025-03-14"] = interns4$n[interns4$modified2=="2025-03-14"] +interns4$n2[interns4$modified2=="2025-03-13"]
interns4$n2[interns4$modified2=="2025-03-20"] = interns4$n[interns4$modified2=="2025-03-20"] +interns4$n2[interns4$modified2=="2025-03-14"]
interns4$n2[interns4$modified2=="2025-03-21"] = interns4$n[interns4$modified2=="2025-03-21"] +interns4$n2[interns4$modified2=="2025-03-20"]
interns4$n2[interns4$modified2=="2025-04-06"] = interns4$n[interns4$modified2=="2025-04-06"] +interns4$n2[interns4$modified2=="2025-03-21"]
interns4$n2[interns4$modified2=="2025-04-07"] = interns4$n[interns4$modified2=="2025-04-07"] +interns4$n2[interns4$modified2=="2025-04-06"]

ggplot(interns4, aes(x=modified2, y=n2))+
  geom_line()
