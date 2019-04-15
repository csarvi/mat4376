# do some data exploration on the raw dataset ----
masterRaw <- readRDS("data/masterRaw.RDS")

# load libraries
library(magrittr)
library(data.table)
library(ggplot2)

# theme 
source("project_analysis/figures/projectTheme.R")

# create autoimmune disease variable ----
cols <- c("CASSTILL", "HAYF1", "HAYF2", "RALLG1", "RALLG2", "DALLG1", "DALLG2", "SALLG1", "SALLG2")
masterRaw[, AUTOIMM := dplyr::if_else(masterRaw[, Reduce(`|`, lapply(.SD, `==`, 1L)), .SDcols = cols], 1L, 0L, 0L)]
masterRaw[, AUTOIMM := factor(AUTOIMM, levels = c(0,1), labels=c("No autoimmune", "Has autoimmune"))]
rm(cols)

cols <- c("P_ASSMEV", "P_DIBTYPE", "P_AHAYFYR")
masterRaw[, P_AUTOIMM := dplyr::if_else(masterRaw[, Reduce(`|`, lapply(.SD, `==`, 1L)), .SDcols = cols], 1L, 0L, 0L)]
masterRaw[, P_AUTOIMM := factor(P_AUTOIMM, levels = c(0,1), labels=c("No autoimmune", "Has autoimmune"))]
rm(cols)


# IMPUTATION FIGS ----

fig <- ggplot(data=masterRaw[BWTGRM_P < 10000]) + 
  geom_density(aes(x=BWTGRM_P), fill="grey45", color="grey45", alpha=0.5) +
  labs(x="Birthweight (in grams)") + 
  theme_minimal()

saveRDS(fig, "project_analysis/figures/birthweight_freq.RDS")
rm(fig)


# autoimmune by age ----
data <- masterRaw[, .N, by=.(AGE_P, AUTOIMM)] %>% 
  .[, PROP := N/sum(N, na.rm=T), by=.(AGE_P)] %>%
  .[AUTOIMM=="Has autoimmune", .(AGE_P, PROP)] %>% 
  .[order(AGE_P)]

fig <- ggplot(data=data) +
  geom_bar(aes(x=AGE_P,y=PROP), stat = "identity") + 
  geom_text(aes(x=AGE_P,y=PROP, 
                label=scales::percent(PROP)), nudge_y = 0.01, size=3) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = data[, AGE_P]) + 
  labs(x="Age",y=NULL) + 
  theme_void() + 
  theme(axis.text.x = element_text(size = 10, margin = margin(-15,0,0,0)),
        axis.title.x = element_text(margin=margin(5,0,10,0)),
        axis.title = element_text(size=11))

saveRDS(fig, "project_analysis/figures/age_autoimmune.RDS")
rm(data,fig)

# are kids from parents with autoimmune disease more likley to have this condition? ----
data <- masterRaw[, .N, by = c("P_AUTOIMM", "AUTOIMM")] %>% 
  .[, PROP := N/sum(N, na.rm=T), by="P_AUTOIMM"] %>% 
  .[AUTOIMM == "Has autoimmune", ]

fig <- ggplot(data=data) +
  geom_bar(aes(x=P_AUTOIMM,y=PROP), stat = "identity") + 
  geom_text(aes(x=P_AUTOIMM,y=PROP, label=scales::percent(PROP)), nudge_y = -0.015, size=7, color="white") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x=NULL,y=NULL) + 
  scale_x_discrete(labels=c("No autoimmune disease", "Has autoimmune disease")) +
  projectTheme() + 
  theme(axis.text = element_text(size = 15, margin = margin(-15,0,5,0)),
        axis.title = element_text(size=11), 
        axis.text.x = element_text(size = 15, margin = margin(-15,0,5,0)), 
        axis.text.y = element_blank())

saveRDS(fig, "project_analysis/figures/parent_autoimmune.RDS")
rm(data, fig)

# FAMILY SIZE AND AUTOIMMUNE DISEASE ----
data <- masterRaw[, .(KIDS = ifelse(FM_KIDS > 3, 4, FM_KIDS), AUTOIMM)] %>% 
  .[, KIDS := factor(KIDS, levels = c(1:4),labels =  as.character(c(c(1:3), "4+")), ordered = T)] %>% 
  .[, .N, by = c("KIDS", "AUTOIMM")] %>% 
  .[, PROP := N/sum(N, na.rm=T), by=c("KIDS")] %>%
  .[AUTOIMM == "Has autoimmune"] %>% 
  .[order(KIDS)]

fig <- ggplot(data=data, aes(x=as.numeric(KIDS), y=PROP)) + 
  geom_line(size=1.25, alpha=0.5) + 
  geom_point(size=5, color="white", 
             fill="white") + 
  geom_point(size=3, alpha=0.5, 
             color="black") + 
  geom_text(aes(label=scales::percent(PROP, accuracy=.1)), nudge_y = 0.004, color="white", fontface="bold") + 
  geom_text(aes(label=scales::percent(PROP, accuracy=.1)), nudge_y = 0.004) + 
  labs(x="Number of children in the family", 
       y="Child has an autoimmune disease (%)") + 
  scale_x_continuous(labels=as.character(c(1:3, "+4"))) + 
  scale_y_continuous(labels=function(x) scales::percent(x, accuracy = 1)) + 
  theme_minimal() + 
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(margin=margin(0,0,5,0)),
        axis.text.y = element_text(margin=margin(0,0,0,5)),
        axis.title.x = element_text(margin=margin(0,0,5,0)),
        axis.title.y = element_text(margin=margin(0,0,0,5)))

saveRDS(fig, "project_analysis/figures/kids_autoimmune.RDS")
rm(fig)


# income and autoimmune disease ----
data <- masterRaw[INCGRP5 <= 4, ] %>% 
  .[, .N, by=c("INCGRP5", "AUTOIMM")] %>% 
  .[, PROP := N/sum(N, na.rm=T), by="INCGRP5"] %>%
  .[AUTOIMM == "Has autoimmune"]

fig <- ggplot(data=data, aes(x=INCGRP5, y=PROP)) + 
  geom_line(size=1.25, alpha=0.5) + 
  geom_point(size=5, color="white", 
             fill="white") + 
  geom_point(size=3, alpha=0.5, 
             color="black") + 
  geom_text(aes(label=scales::percent(PROP, accuracy=.1)), nudge_y = 0.002, color="white", fontface="bold") + 
  geom_text(aes(label=scales::percent(PROP, accuracy=.1)), nudge_y = 0.002) + 
  labs(x="Family income", 
       y="Child has an autoimmune disease (%)") + 
  scale_x_continuous(labels=c("$0 - $34,999", 
                              "$35,000 - $74,999", 
                              "$75,000 - $99,999", 
                              "$100,000+")) + 
  scale_y_continuous(labels=function(x) scales::percent(x, accuracy = 1)) + 
  theme_minimal() + 
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(margin=margin(0,0,5,0)),
        axis.text.y = element_text(margin=margin(0,0,0,5)),
        axis.title.x = element_text(margin=margin(0,0,5,0)),
        axis.title.y = element_text(margin=margin(0,0,0,5))) 

saveRDS(fig, "project_analysis/figures/income_autoimmune.RDS")
rm(fig, data)