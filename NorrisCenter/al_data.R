# Read Al's data | Emma Yockman
# 17 Apr 2025
rm(list=ls())
library(tidyverse); library(readxl)
setwd("~/Documents/Coding/educational/NorrisCenter")
al = read_excel("Al_data.xlsx")
head(al); colnames(al)

ggplot(al, aes(x=DATE, y=DATA_RECORDS))+
  geom_line()

ggplot(al, (aes(x=DATE, y=IMAGE_LINKED))) +
  geom_line()

al2 = al %>% select(DATE, BARCODES_VISIBLE_IN_CCH2, DATA_RECORDS, DATA_RECORDS_W_ACCESSION_NO, IMAGE_LINKED, TOTAL_RECORDS, TOTAL_ALLSOURCES)

al_long = pivot_longer(al2, cols= c("BARCODES_VISIBLE_IN_CCH2", "DATA_RECORDS","DATA_RECORDS_W_ACCESSION_NO", "IMAGE_LINKED", "TOTAL_RECORDS", "TOTAL_ALLSOURCES"),
                       names_to = "count_type",
                       values_to = "counts")
head(al_long)

ggplot(al_long, (aes(x=DATE, y=counts, colour=count_type))) +
  geom_line()

library(ggthemes)
str(al)
fallqtr = as.Date("2024-09-26")
winqtr = as.Date("2025-01-06")
sprqtr= as.Date("2025-03-31")
al$DATE = as.Date(al$DATE)
str(al)
colnames(al)

# Define periods to highlight
highlight_periods <- data.frame(
  xmin = as.Date("2024-09-26"),
  xmax = as.Date("2025-04-17"),
  ymin = -Inf,
  ymax = Inf,
  fill = c("lightgreen"))

ggplot(al, aes(x=DATE, y=BARCODES_VISIBLE_IN_CCH2))+
  geom_line()+
  ylim(0,20000)+ 
  #geom_vline(xintercept=fallqtr, linetype="solid", color="darkgreen", linewidth=0.5)+
  geom_rect(data = highlight_periods, 
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax, 
                fill = fill), 
          alpha = 0.3, inherit.aes = FALSE)+
  scale_fill_manual(values=c("lightgreen"))+
  theme_minimal()+theme(legend.position = "none")
