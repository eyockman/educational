rm(list=ls()) # clear R's brain
lapply(c("ggthemes","tidyverse","viridis",
         "ggplot2","cowplot","lme4",
         "grid","ggrepel","multcomp","emmeans"),require,character.only=T) # load in packages all in one function!

f=read_csv("~/Documents/Science/facil-wos.csv"); head(f) # read in facilitation pubs dataset using file path - the ";" runs that line, then runs head(f) next
f = f %>% rename_at(vars(c("Count")), ~ c("Fcount")) # rename a column

c=read_csv("~/Documents/Science/comp-wos.csv"); head(c) # read in competition pubs dataset using file path - the ";" runs that line, then runs head(c) next
c = c %>% rename_at(vars(c("Count")), ~ c("Ccount")) # rename a column

j=full_join(f, c); head(j) # join two datasets together!
j =j%>% pivot_longer(cols = c("Fcount","Ccount"), # transform data, taking the columns Fcount and Ccount
                     names_to = "group", # putting column names into a new column called "group"
                     values_to = "count") # putting values from those columns into a new column called values = this is the number publications per year for each "group"

head(j)
j = j %>% filter(PublicationYears !=2025) # filter to exclude year 2025 (because the year isn't over yet, so data not complete)
ggplot(data=j, aes(x=PublicationYears, y=count, color=group))+ # make a plot using dataset j, set x as years, y as count, and color as group
  geom_point()+ # draw points on the plot using the above aesthetics
  geom_line()+ # draw lines on the plot using the above aesthetics
  theme_bw()+ # use a cuter theme than the basic one
  geom_vline(xintercept=1994)+ # add a vertical line at 1994 to show the year that Bertness & Callaway's facilitation paper came out
  theme(legend.position = "none") # get rid of legend bc ugly; red will be competition and blue facil i think (can remove this line if you need to check)

je = j %>% filter(PublicationYears <= 1975) # make a new dataframe called je with only years 1975 or after
head(je) # show first few lines of this data frame to check to see if its ok
ggplot(je, aes(x=PublicationYears, y=count, color=group))+  # make a plot using dataset je, set x as years, y as count, and color as group
  geom_point()+ # draw points on the plot using the above aesthetics
  geom_line()+ # draw lines on the plot using the above aesthetics
  theme_bw() # use a cuter theme than the basic one

jl = j %>% filter(PublicationYears >= 1975) # make a new dataframe called jl with only years 1975 or after
head(jl) # show first few lines of this data frame to check to see if its ok
ggplot(jl, aes(x=PublicationYears, y=count, color=group))+ # make a plot using dataset jl, set x as years, y as count, and color as group
  geom_point()+ # draw points on the plot using the above aesthetics
  geom_line()+ # draw lines on the plot using the above aesthetics
  theme_bw() # use a cuter theme than the basic one
