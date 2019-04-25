# pkg requirements ----
source("helpers/checkPkg.R")
pkgs <- c("magrittr", "data.table", "crayon", "devtools")
for(i in seq_along(pkgs)){
  checkPkg(pkg=pkgs[[i]])
}
rm(i, pkgs)

# load pkgs ----
if(any(.packages() %in% "magrittr")) library(magrittr)
if(any(.packages() %in% "data.table")) library(data.table)
if(any(.packages() %in% "spotifyr")) library(ggplot2)

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

# viz section -----

library(ggplot2)

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
rm(fig)
