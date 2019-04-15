library(magrittr)
library(ggplot2)

models <- readRDS("project_analysis/model_training/models.RDS")

metrics <- lapply(seq_along(names(models)), function(x){
  
  res <- data.frame(t(sapply(models[[x]], function(x) c(x[["MCC"]], x[["TPR"]]))))
  
  res[["Model"]] <- names(models)[[x]]
  
  return(res)
}) %>% 
  data.table::rbindlist(.)

data.table::setnames(metrics, c("X1", "X2"), c("MCC", "TPR"))

# graph
fig <- ggplot() + 
  geom_point(data=metrics[MCC < 0, ], aes(x=MCC,y=TPR, color=Model), 
             color="red", alpha=0.25) + 
  geom_point(data=metrics[MCC >= 0, ], aes(x=MCC,y=TPR, color=Model)) + 
  geom_vline(xintercept = 0) + 
  geom_segment(x=0, xend=metrics[MCC < 0, mean(MCC)], y=0.5, yend=0.5, arrow = arrow()) + 
  scale_colour_brewer(type = "qual", palette=2) + 
  theme_minimal() + 
  theme(text=element_text(family = "sans"), 
        legend.box = "horizontal", 
        legend.position = c(0.8,0.2), 
        legend.title = element_blank(), 
        legend.background = element_rect(color="black", fill="white"))

saveRDS(fig, "project_analysis/figures/mcc_tpr_models.RDS")

rm(fig)

# prediction mdel performance against trained models ----

predMetrics <- readRDS("project_analysis/results/pred_metrics.RDS")

fig <- ggplot() + 
  geom_point(data=metrics, aes(x=MCC,y=TPR), alpha=0.15) + 
  geom_vline(xintercept = 0) + 
  geom_point(data=predMetrics, aes(x=MCC, y=TPR), shape=17, size=2.5, color="red") +
  geom_segment(x=0, xend=metrics[MCC < 0, mean(MCC)], y=0.5, yend=0.5, arrow = arrow()) + 
  scale_colour_brewer(type = "qual", palette=2) + 
  theme_minimal() + 
  theme(text=element_text(family = "sans"), 
        legend.position = "none")

saveRDS(fig, "project_analysis/figures/pred_mcc_tpr_comp.RDS")

rm(fig)
