# pkg requirements ----
source("helpers/checkPkg.R")
pkgs <- c("magrittr", "data.table", "crayon", "devtools", "ggplot2", "stringr")
for(i in seq_along(pkgs)){
  checkPkg(pkg=pkgs[[i]])
}

# load pkgs ----
pkgs <- c("magrittr", "data.table", "spotifyr", "ggplot2")
for(i in seq_along(pkgs)){
  if(!any(.packages() %in% pkgs[[i]])) library(pkgs[[i]], character.only = T)
}
rm(pkgs, i)

# spotifyr connection ----
# get your client ID and secret as per instructions
spotify_client_id = "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
spotify_client_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
# generate .httr-oauth
spotifyr::get_spotify_authorization_code()

# fig 1 - energy diff ----
# get portishhead track features
portishead <- setDT(spotifyr::get_artist_audio_features(artist = "Portishead"))

# randomly select death metal bands and cnsolidate it into a data frame
set.seed(1234)
deathmetal <- spotifyr::get_genre_artists(genre = "death metal", limit = 50)
deathmetal <- deathmetal[["artist_name"]] %>% 
  sample(., 10) %>% 
  lapply(., function(x){
    res <- try(spotifyr::get_artist_audio_features(x))
    if(inherits(res, "try-error")) return(NULL)
    return(res)
    })
deathmetal <- do.call(rbind, deathmetal)

# stack data
res <- setDT(do.call(rbind, list(portishead, deathmetal)))
rm(portishead, deathmetal)

# figure 
fig <- ggplot() + 
  geom_density(data=res[artist_name!= "Portishead", ], aes(x=energy, fill="Death Metal"), 
               alpha=0.24, colour=NA) + 
  geom_density(data=res[artist_name== "Portishead", ], aes(x=energy, fill="Portishead"), 
               alpha=0.24, colour=NA) + 
  theme_minimal() + 
  labs(y=NULL, title="Portishead trakcs score more often than not lower in \"energy\"\ncompared to randomly selected death metal artists") + 
  scale_fill_manual(name="", values = c(`Death Metal`="orange", Portishead="blue")) +
  theme(legend.position = c(0.1, 0.8))

png(filename = "./img/portishead_energy.png")
fig
dev.off()
rm(fig, res)

# fig 2 valence example ----
nirvana <- spotifyr::get_artist_audio_features("Nirvana")
offspring <- spotifyr::get_artist_audio_features("The Offspring")
fig <- ggplot() + 
  geom_density(data=nirvana, aes(x=valence, fill="Nirvana"), 
               alpha=0.24, colour=NA) + 
  geom_density(data=offspring, aes(x=valence, fill="The Offspring"), 
               alpha=0.24, colour=NA) + 
  theme_minimal() + 
  labs(y=NULL, title="Valence distribution for Nirvana and The Offspring tracks") + 
  scale_fill_manual(name="", values = c(`Nirvana`="orange", `The Offspring`="blue")) +
  theme(legend.position = c(0.9, 0.8))

png(filename = "./img/nirvana_valence.png")
fig
dev.off()
rm(nirvana, offspring, fig)

# Project data analysis ----
raw <- readRDS("data/raw.RDS")
data <- readRDS("data/data.RDS")

# raw distribution ----

fig <- raw[, .N/raw[, .N], by= "key_mode"][order(V1, decreasing = T), ] %>% 
  .[, key_mode := factor(key_mode, 
                         levels = raw[, .N/raw[, .N], by= "key_mode"][order(V1, decreasing = T), key_mode], 
                         labels = raw[, .N/raw[, .N], by= "key_mode"][order(V1, decreasing = T), key_mode])] %>% 
  ggplot() + 
  geom_bar(aes(x=key_mode, y=V1), stat="identity", 
           fill="blue", 
           colour="blue",
           alpha=0.25) + 
  geom_text(aes(x=key_mode, y=V1, label=scales::percent(V1)), 
            nudge_y = 0.0025, 
            size=3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x=NULL, y=NULL, title="Key mode distribution in the playlist dataset (N=723)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   margin=margin(-15)),
        axis.text.y = element_blank(),
        panel.grid = element_blank())
png(filename="img/key_mode_dist.png")
fig
dev.off()
rm(fig)

# count viz
fig <- ggplot(data=data[order(user_name), .N/nrow(data), by="user_name"]) + 
  geom_bar(aes(x = user_name, y=V1), stat="identity", fill="orange", 
           color="orange", alpha=0.5) + 
  geom_text(aes(x=user_name, y=V1, label=scales::percent(V1)), nudge_y = +0.015, size=4, fontface="bold") + 
  labs(x=NULL, y=NULL, title="Percentage of tracks per user") + 
  scale_x_discrete(label=function(x) stringr::str_to_title(x), ) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text = element_text(size=12, face="bold"), 
        axis.text.x = element_text(margin=margin(-10)))
png(filename = "img/track_perc.png")
fig
dev.off()
rm(fig)

# key distribution ----


# do variable distribution exploration ----

genDensity <- function(data, var, title, legendPosition, filename){
  sym <- as.name(var)
  fig <- ggplot(data=data) + 
    geom_density(aes(x=eval(sym), fill=user_name), alpha=0.25, color="transparent") + 
    theme_minimal() + 
    labs(y=NULL, title=title, x=stringr::str_to_title(var)) + 
    scale_fill_manual(name="", values = c("cesar" = "orange", 
                                          "jose" = "blue", 
                                          "maia" = "green")) + 
    scale_colour_manual(name="", values = c("cesar" = "orange", 
                                            "jose" = "blue", 
                                            "maia" = "green")) + 
    theme(legend.position = legendPosition, 
          axis.text.y = element_blank(), 
          legend.title = element_blank())
  
  png(filename=filename)
  print(fig)
  dev.off()
  return(fig)
}

# valence ----
fig <- genDensity(data=data, var="valence", title="Distribution by user for audio feature: valence", 
           legendPosition = c(0.1, 0.8), filename="img/valence_user.png")

# energy ----
fig <- genDensity(data=data, var="energy", "Distribution by user for audio feature: energy", 
                  legendPosition = c(0.1, 0.8), filename="img/energy_user.png")

# danceability ----
fig <- genDensity(data=data, var="danceability", "Distribution by user for audio feature: danceability", 
                  legendPosition = c(0.1, 0.8), filename="img/danceability_user.png")

# tempo ----
fig <- genDensity(data=data, var="tempo", "Distribution by user for audio feature: tempo", 
                  legendPosition = c(0.1, 0.8), filename="img/tempo_user.png")

# loudness ----
fig <- genDensity(data=data, var="loudness", "Distribution by user for audio feature: loudness", 
                  legendPosition = c(0.1, 0.8), filename="img/loudness_user.png")

# duration_ms ----
fig <- genDensity(data=data, var="duration_ms", "Distribution by user for audio feature: duration_ms", 
                  legendPosition = c(0.1, 0.8), filename="img/duration_ms_user.png")

# mode ----


# CLUSTERING ANALYSIS -----
clustgRes <- readRDS("model/clustering_res.RDS")
ccomp <- clustgRes$consensus_evaluate

# k = 3 has the highest PAC
pacDT <- as.data.table(ccomp$pac) 
pacDT[, avg_pac := rowMeans(.SD, na.rm=T), .SDcols=grep(x=names(pacDT), pattern="[A-Z]", value=T)]

# Davies-Bouldin index averaged out ---- 
iiDT <- lapply(seq_along(ccomp$ii), function(x){
  data.table(ccomp$ii[[x]])[, lapply(.SD, function(x){
    if(!is.numeric(x)) return(NULL)
    return(mean(x, na.rm=T))
  })][, k:=x+2]
  
})

iiDT <- do.call(rbind, iiDT)
iiDT <- melt(iiDT, id.vars = "k", variable.name = "validation_indice",  variable.factor = FALSE, value.factor = FALSE)

# viz section

# PAC viz ----
pac <- melt(pacDT, id.vars = "k", variable.name = "model", variable.factor = F, value.factor = F)[, k:=as.numeric(k)]
fig <- ggplot() +
  geom_line(data=pac[model!= "avg_pac"], aes(x=k, y=value, group=model), 
            colour="grey92", size=1.25, lineend="round", linejoin="round") + 
  geom_line(data=pac[model == "avg_pac"], aes(x=k, y=value, 
                                              colour="Averaged PAC"), 
            size=1.25, lineend="round", linejoin="round") + 
  geom_point(data=pac[model == "avg_pac"], aes(x=k, y=value), 
             colour="white", size=4) + 
  geom_point(data=pac[model == "avg_pac"], aes(x=k, y=value),
             colour="orange", size=2) + 
  scale_colour_manual(name="", values=c(`Averaged PAC`="orange")) + 
  scale_x_continuous(breaks = function(x) round(seq(min(x), max(x), by=1))) + 
  labs(x="# of clusters", y="Proportion of Ambiguous Clusters\n(PAC)", 
       title = "PAC (averaged across models) suggests k=3") + 
  theme_minimal() + 
  theme(axis.title = element_text(hjust=0), 
        legend.position = c(0.8,0.9))
png(filename = "img/pac_graph.png")
fig
dev.off()
rm(fig)

# Davies-bouldin viz ----
fig <- ggplot(data=iiDT[validation_indice == "davies_bouldin", ], aes(x=k,y=value)) + 
  geom_line(colour="orange", size=1.25, 
            lineend="round", linejoin="round") +
  geom_point(colour="white", size=4) + 
  geom_point(colour="orange", size=2) + 
  scale_x_continuous(breaks = function(x) round(seq(min(x), max(x), by=1))) + 
  labs(x="# of clusters", y="Davies-Bouldin Index\n(averaged across models)", 
       title = "Davies-Bouldin index (averaged across models) suggests k=3") + 
  theme_minimal() + 
  theme(axis.title = element_text(hjust=0))
png(filename = "img/db_graph.png")
fig
dev.off()
rm(fig)

# dunn viz ----
fig <- ggplot(data=iiDT[validation_indice == "dunn", ], aes(x=k,y=value)) + 
  geom_line(colour="orange", size=1.25, 
            lineend="round", linejoin="round") +
  geom_point(colour="white", size=4) + 
  geom_point(colour="orange", size=2) + 
  scale_x_continuous(breaks = function(x) round(seq(min(x), max(x), by=1))) + 
  labs(x="# of clusters", y="Dunn Index\n(averaged across models)", 
       title = "Dunn index (averaged across models) suggests k=3") + 
  theme_minimal() + 
  theme(axis.title = element_text(hjust=0))
png(filename = "img/dunn_graph.png")
fig
dev.off()
rm(fig)

# sillhouette viz ----
fig <- ggplot(data=iiDT[validation_indice == "silhouette", ], aes(x=k,y=value)) + 
  geom_line(colour="orange", size=1.25, 
            lineend="round", linejoin="round") +
  geom_point(colour="white", size=4) + 
  geom_point(colour="orange", size=2) + 
  scale_x_continuous(breaks = function(x) round(seq(min(x), max(x), by=1))) + 
  labs(x="# of clusters", y="Silhouette Index\n(averaged across models)", 
       title = "Silhouette index (averaged across models) suggests k=3") + 
  theme_minimal() + 
  theme(axis.title = element_text(hjust=0))
png(filename = "img/silhouette_graph.png")
fig
dev.off()
rm(list=ls())
