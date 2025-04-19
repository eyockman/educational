# Read Al's data | Emma Yockman
# 17 Apr 2025
rm(list=ls()) # clear R's brain

library(tidyverse)
library(readxl)
library(ggplot2) # load in necessary packages. the ";" is a way to have more than one line run in the same line

setwd("~/Documents/Coding/educational/NorrisCenter") # set folder I want to call files from

al = read_excel("Al_data.xlsx") # call my file, name it "al"

head(al) # look at the first few rows
colnames(al) # look at column names

# plot my data ----

ggplot(data=al, aes(x=DATE, y=DATA_RECORDS))+ # make a blank plot using al data, with DATE as x axis and DATA_RECORDS as y axis
  geom_line() # draw a line using the x and y stated previously

ggplot(data=al, (aes(x=DATE, y=IMAGE_LINKED))) + # make a blank plot using al data, with DATE as x axis and IMAGE_LINKED as y axis
  geom_line() # draw a line

# show all types of data on the same graph ----

al2 = al %>% # %>% is the pipe function, it's saying I want to use the select() function within my al dataset.
  select(DATE, BARCODES_VISIBLE_IN_CCH2, DATA_RECORDS, DATA_RECORDS_W_ACCESSION_NO, IMAGE_LINKED, TOTAL_RECORDS, TOTAL_ALLSOURCES) #I am making a new dataset called al2 that only has these columns


al_long = pivot_longer(data=al2, # make a new dataset called al_long that does something called "pivot_longer" I can explain this more in depth, but basically it converts my columns to rows (kind of)
                       cols= c("BARCODES_VISIBLE_IN_CCH2", "DATA_RECORDS","DATA_RECORDS_W_ACCESSION_NO", "IMAGE_LINKED", "TOTAL_RECORDS", "TOTAL_ALLSOURCES"), # do it to these columns
                       names_to = "count_type", # the column that will now store the converted old columns will be called "count_type"
                       values_to = "counts") # the column that will now store the values that each old column had will be called "counts"

head(al_long) # look at the first few rows of my pivoted dataset


ggplot(data=al_long, aes(x=DATE, y=counts, colour=count_type)) + # make a new plot using al_long where DATE is on the x axis and counts are on the y, but color code by count_type
  geom_line() # draw line

# make a more complicated graph ----

library(ggthemes) # load in ggthemes
str(al) # look at the types of data that are in "al" - you can see that DATE is something called POSIXct, I'd rather have it as just date
al$DATE = as.Date(al$DATE) # convert the column DATE in al to date using the as.Date() function
str(al) # check the data types to see if it worked

fallqtr = as.Date("2024-09-26") # make object called fallqtr and assign it a date value (first day of fall qtr)
winqtr = as.Date("2025-01-06") # same with winter quarter
sprqtr= as.Date("2025-03-31") # and with spring!


# define periods to highlight on a graph 
highlight_periods = data.frame( # make a dataset called highlight_periods and use the data.frame() function to start making our dataframe
  start = fallqtr, # set miniumum date to the fallqtr value (already in Date form)
  end = as.Date("2025-04-17"), # this is the last day Al got his data
  lower = -Inf, # negative infinity, we want our highlight to just keep going
  upper = Inf, # positive infinity 
  highlightcolor ="lightgreen") # fill

ggplot(data=al, aes(x=DATE, y=BARCODES_VISIBLE_IN_CCH2))+ # make plot using al data and set DATE as x axis and BARCODES VISIBLE... as y axis
  geom_line()+ # draw line
  ylim(0,16000)+ # set y axis limits manually
  xlim(as.Date("2020-01-01"),highlight_periods$end)+ # set x axis limits manually (otherwise it chooses its own)
  geom_vline(xintercept=winqtr, linetype="solid", color="blue", linewidth=0.5)+ # draw a line at the day winter quarter started
  geom_rect(data = highlight_periods, # highlight part of the graph using my highlight_periods data fram
            aes(xmin = start, xmax = end, # use the start and end days for highlight
                ymin = lower, ymax = upper, # put upper and lower bounds
                fill=highlightcolor), # fill
          alpha = 0.3, inherit.aes = FALSE)+
  scale_fill_manual(values=c("lightgreen"))+ # make the highlight section green
  theme_minimal()+ # use a cuter theme
  theme(legend.position = "none") #get rid of legend because I thought it was ugly


# where did I learn all these things that ggplot can do?
# https://lile.duke.edu/wp-content/uploads/2020/07/R_ggplot2_cheatsheet.pdf
# or run:
??ggplot2
