#  program:  mortal-pain-trends-nhis.R
#  task:     trends on pain and hopelessness
#  input:    NHIS estimates
#  output:   
#  project:  MORTAL conference talk
#  author:   sam harper \ 2024-06-08

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### 
lb <- read_csv(here("data", "lbpain_m.csv"), col_names=TRUE) %>%
  mutate(source = 1)

np <- read_csv(here("data", "npain_m.csv"), col_names=TRUE) %>%
  mutate(source = 2)

fp <- read_csv(here("data", "fpain_m.csv"), col_names=TRUE) %>%
  mutate(source = 3)

ap <- read_csv(here("data", "anypain_m.csv"), col_names=TRUE) %>%
  mutate(source = 4)

pain <- bind_rows(lb, np, fp, ap) %>%
  mutate_at(vars(c(estimate, stderr, 
    min95, max95)), ~ . * 100) %>%
  mutate(gender = recode_factor(male, `0` = "Women", `1` = "Men"),
    educ = recode_factor(univ, `0` = "University or more", 
      `1` = "<University"),
    time = recode_factor(year, `0` = "2001-03", `1` = "2004-06",
      `2` = "2007-09", `3` = "2010-12", 
      `4` = "2013-15", `5` = "2016-18"),
    source = recode_factor(source, `1` = "Lower back", 
      `2` = "Neck", `3` = "Facial", `4` = "Any"))

ht <- read_csv(here("data", "hopeless_m.csv"), col_names=TRUE) %>%
  mutate_at(vars(c(estimate, stderr, 
    min95, max95)), ~ . * 100) %>%
  mutate(gender = recode_factor(male, `0` = "Women", `1` = "Men"),
    educ = recode_factor(univ, `0` = "University or more", 
      `1` = "<University"),
    time = recode_factor(year, `0` = "2001-3", `1` = "2004-6",
      `2` = "2007-9", `3` = "2010-2", 
      `4` = "2013-5", `5` = "2016-8"))


## set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

m <- ggplot(subset(pain, gender=="Men"), 
  aes(x = year, y = estimate, colour=educ)) +
  geom_ribbon(aes(ymin = min95, ymax = max95, 
    fill=educ), alpha = 0.25, linewidth=0) +
  geom_line(show.legend=F, linewidth=1.5) + 
  facet_wrap(~ source, nrow=1) + 
  scale_y_continuous(limits=c(0,50), 
                     breaks=c(0,25,50)) +
  scale_x_continuous(breaks=c(1,3,5), 
    labels=c("0" = "2001-\n03", "2" = "'07-\n09",
      "5" = "'16-\n18")) +
  scale_color_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) +
  scale_fill_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) + 
  labs(y = "", x = "") + ggtitle("Men") + stheme +
  theme(panel.spacing.x = unit(2, "lines"))

w <- ggplot(subset(pain, gender=="Women"), 
       aes(x = year, y = estimate, colour=educ)) +
  geom_ribbon(aes(ymin = min95, ymax = max95, 
    fill=educ), alpha = 0.25, linewidth=0) +
  geom_line(show.legend=F, linewidth=1.5) + 
  facet_wrap(~ source, nrow=1) + 
  scale_y_continuous(limits=c(0,50), breaks=c(0,25,50)) +
  scale_x_continuous(breaks=c(1,3,5), 
    labels=c("0" = "2001-\n03", "2" = "'07-\n09",
      "5" = "'16-\n18")) +
  scale_color_manual(name="Education", 
    values=c("#4daf4a","#377eb8")) + 
  scale_fill_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) + 
  geom_text(data=subset(pain, gender=="Women" & source=="Lower back"), aes(y = estimate), label = "No BA", x = 1, y = 40, size = 4, hjust=0, colour = "#377eb8") +
  geom_text(data=subset(pain, gender=="Women" & source=="Lower back"), aes(y = estimate), label = "BA", x = 1, y = 20, size = 4, hjust=0, colour = "#4daf4a") +
  labs(y = "", x = "") + ggtitle("Women") + stheme +
  theme(panel.spacing.x = unit(2, "lines"))

p <- w / m + plot_layout(guides = "collect") & theme(legend.position = 'none', legend.text = element_text(size=12)) 

p2 <- p + plot_annotation(
  title = '% adults ages 25+ reporting pain lasting a day or more, 2001-03 to 2016-18', 
  theme = theme(plot.title = element_text(size = 18,
    color="grey60")))
p2

ggsave(here("images", "pain-trends-educ.png"), plot=p2, width=11, height=7)


mh <- ggplot(subset(ht, male==1), 
  aes(x = year, y = estimate, colour=educ)) +
  geom_ribbon(aes(ymin = min95, ymax = max95, 
    fill=educ), alpha = 0.25, linewidth=0) +
  geom_line(show.legend=F, linewidth=1.5) + 
  scale_y_continuous(limits=c(0,3), 
                     breaks=c(0,1,2,3)) +
  scale_x_continuous(breaks=c(0:5), 
    labels=c("0" = "2001-03", "1" = "'04-06", 
    "2" = "'08-10", "3" = "'10-12", "4" = "'14-16",
    "5" = "'16-18")) +
  scale_color_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) +
  scale_fill_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) + 
  labs(y = "", x = "") + ggtitle("Men") + 
  stheme 

wh <- ggplot(subset(ht, male==0), 
  aes(x = year, y = estimate, colour=educ)) +
  geom_ribbon(aes(ymin = min95, ymax = max95, 
    fill=educ), alpha = 0.25, linewidth=0) +
  geom_line(show.legend=F, linewidth=1.5) + 
  scale_y_continuous(limits=c(0,3), 
                     breaks=c(0,1,2,3)) +
  scale_x_continuous(breaks=c(0:5), 
    labels=c("0" = "2001-03", "1" = "'04-06", 
    "2" = "'08-10", "3" = "'10-12", "4" = "'14-16",
    "5" = "'16-18")) +
  scale_color_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) +
  scale_fill_manual(name="Education", 
                     values=c("#4daf4a","#377eb8")) + 
  labs(y = "", x = "") + ggtitle("Women") + 
  annotate("text", label = "No BA", x = 1, 
    y = 2, size = 6, hjust=0, colour = "#377eb8") +
  annotate("text", label = "BA", x = 1, 
    y = 0.7, size = 6, hjust=0, colour = "#4daf4a") +
  stheme 
  
mh 

ph <- wh + mh

ph2 <- ph + plot_annotation(
  title = '% adults ages 25+ reporting feeling hopeless most or all of the time, past month', 
  theme = theme(plot.title = element_text(size = 20,
    color="grey60")))
ph2

ggsave(here("images", "hope-trends-educ.png"), plot=ph2, width=11, height=7)

# Unemployment trends
ut <- read_csv(here("data", "fredgraph.csv"), col_names = TRUE)
ggplot(ut, aes(x = DATE, y = LNS14027660)) + 
  geom_rect(xmin=as.Date("2001-03-01"), xmax=as.Date("2001-11-01"), 
    ymin=0, ymax=15, fill="grey90", alpha=0.3, col="grey90") +
  geom_rect(xmin=as.Date("2007-01-01"), xmax=as.Date("2009-01-01"), 
    ymin=0, ymax=15, fill="grey90", alpha=0.3, col="grey90") +
  geom_line(color = "#377eb8") + 
  geom_line(aes(y= CGBD25O), color = "#4daf4a") +
  annotate("text", label = "% Unemployed", 
    x = as.Date("1999-01-01"), y = 17, size = 6, 
    hjust=0, colour = "grey60") +
  annotate("text", label = "No BA", x = as.Date("2010-01-01"), 
    y = 12, size = 4, hjust=0, colour = "#377eb8") +
  annotate("text", label = "BA", x = as.Date("2010-01-01"), 
    y = 7, size = 4, hjust=0, colour = "#4daf4a") +
  labs(x = "", y = " ") +
  stheme

2001-03-01, 2001-11-01


    