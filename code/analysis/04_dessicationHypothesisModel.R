library(tidyverse)
library(DHARMa)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(ggnewscale)

## read in model data frame
mdf <- read.csv("data/melanism_modelDF.csv") %>% 
  mutate(AnnualPrecipitation = scale(AnnualPrecipitation),
         MajorAxis_Dorsal = scale(MajorAxis_Dorsal),
         meanSoilMoisture = scale(meanSoilMoisture),
         beetleAbundance = scale(beetleAbundance),
         meanSoilTemp = scale(meanSoilTemp),
         mammalAbundance = scale(mammalAbundance),
         meanVPD = scale(meanVPD)) %>% 
  mutate(sppSite = paste(SpeciesName,IL_Site,sep = "_"))

## desiccation hypothesis model
# Dorsal lightness
dorsal_desiccationModel <- lmer(formula = ave_melanism_dorsal ~ 
                                  AnnualPrecipitation + 
                                  meanSoilMoisture +
                                  #mammalAbundance + 
                                  #beetleAbundance + 
                                  meanVPD   + 
                                  MajorAxis_Dorsal + # this is body size
                                  MajorAxis_Dorsal:meanVPD +
                                  MajorAxis_Dorsal:meanSoilMoisture +
                                  (1|Tribe/Genus) + (1|IL_Site) ,
                                data = mdf, 
                                REML = F,
                                na.action = "na.fail") # problem with beetleAbundance...fix this

summary(dorsal_desiccationModel)
#sjPlot::tab_model(dorsal_desiccationModel, 
#          file = "modelResults/dorsal_desiccationModel_satterthwaite.doc",
#          p.val = "satterthwaite", show.df = TRUE)

# generate sjplot data for making figures
ap_response <- plot_model(dorsal_desiccationModel, type = "pred", terms = "AnnualPrecipitation")$data
msm_response <- plot_model(dorsal_desiccationModel, type = "pred", terms = "meanSoilMoisture")$data
bs_response <- plot_model(dorsal_desiccationModel, type = "pred", terms = "MajorAxis_Dorsal")$data
int_response <- plot_model(dorsal_desiccationModel, type = "pred", 
                           terms = c("meanVPD", "MajorAxis_Dorsal [-2,0,2]"))$data

## figures
d <- ggplot() + 
  geom_ribbon(ap_response,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "black", alpha = 0.25) +
  geom_point(mdf, mapping = aes(x = AnnualPrecipitation, y = ave_melanism_dorsal), 
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             alpha = 0.08) +
  geom_line(ap_response, mapping = aes(x = x, y = predicted), color = "black") +
  ggtitle("") +
  labs(x = "Annual Precipitation", y = "Dorsal lightness") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.y = element_text(angle = 0)) +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title=element_text(size=14))

c <- ggplot() + 
  geom_ribbon(msm_response,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "black", alpha = 0.25) +
  geom_point(mdf, 
             mapping = aes(x = meanSoilMoisture, y = ave_melanism_dorsal),
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             alpha = 0.08) +
  geom_line(msm_response, mapping = aes(x = x, y = predicted), color = "black") +
  ggtitle("") +
  labs(x = "Soil moisture", y = "Dorsal lightness") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.y = element_text(angle = 0)) +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title=element_text(size=14))

a <- ggplot() +
  geom_ribbon(bs_response,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "black", alpha = 0.25) +
  geom_point(mdf, 
             mapping = aes(x = MajorAxis_Dorsal, y = ave_melanism_dorsal),
             alpha = 0.08) +
  geom_line(bs_response, mapping = aes(x = x, y = predicted), color = "black") +
  ggtitle("") +
  labs(x = "Body size", y = "Dorsal lightness") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.y = element_text(angle = 0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 13),
        axis.title=element_text(size=14))

b_bw <- ggplot() + 
  geom_ribbon(int_response,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.35) +
  geom_point(mdf, 
             mapping = aes(x = meanVPD, y = ave_melanism_dorsal),
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             alpha = 0.08) +
  geom_line(int_response, mapping = aes(x = x, y = predicted, color = group)) +
  ggtitle("") +
  labs(x = "Vapour pressure deficit", y = "Dorsal lightness", 
       color = "Body size",
       fill = "Body size") +
  scale_fill_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                    labels = c( paste("-2 s.d.", "(Small)"), 
                                paste("0 s.d.", "(Average)"),
                                paste("2 s.d.","(Large)"))) +
  scale_color_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                     labels = c( paste("-2 s.d.", "(Small)"), 
                                 paste("0 s.d.", "(Average)"),
                                 paste("2 s.d.","(Large)")))  +
  theme_classic() +
  theme(legend.position= c(0.25, 0.92),
        legend.text = element_text(size=8),
        axis.text = element_text(size = 12),
        axis.title=element_text(size=14))
b_color <- ggplot() + 
  geom_ribbon(int_response,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.35) +
  scale_fill_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                    labels = c( paste("-2 s.d.", "(Small)"), 
                                paste("0 s.d.", "(Average)"),
                                paste("2 s.d.","(Large)"))) +
  geom_point(mdf, 
             mapping = aes(x = meanVPD, y = ave_melanism_dorsal, color = MajorAxis_Dorsal),
             alpha = 0.08,
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             show.legend = FALSE) +
  scale_color_gradientn(colors = c("#e09f3e", "#9e2a2b", "#540b0e"),
                        breaks = c(-2, 0, 2),
                        limits = c(-2,3),
                        labels = c( paste("-2 s.d.", "(Small)"), 
                                    paste("0 s.d.", "(Average)"),
                                    paste("2 s.d.","(Large)"))) +
  new_scale_color() +
  geom_line(int_response, mapping = aes(x = x, y = predicted, color = group)) +
  scale_color_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                     labels = c( paste("-2 s.d.", "(Small)"), 
                                 paste("0 s.d.", "(Average)"),
                                 paste("2 s.d.","(Large)"))) +
  ggtitle("") +
  labs(x = "Vapour pressure deficit", y = "Dorsal lightness", 
       color = "Body size",
       fill = "Body size") +
  theme_classic() +
  theme(legend.position= c(0.25, 0.92),
        legend.text = element_text(size=10),
        axis.text = element_text(size = 12),
        axis.title=element_text(size=14))


cp_bw <- cowplot::plot_grid(a,b_bw,c,d, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
cp_bw
#ggsave(plot = cp_bw, filename = "figureOutputs/dorsalDesiccationModelResults_bwPoints.png",
#       dpi = 450, width = 8, height = 8 )

cp_color <- cowplot::plot_grid(a,b_color,c,d, labels = c("B", "C", "D", "E"), nrow = 2, ncol = 2)
cp_color
#ggsave(plot = cp_color, filename = "figureOutputs/dorsalDesiccationModelResults_colorPoints.png",
#       dpi = 450, width = 8, height = 8 )


plot_model(dorsal_desiccationModel, type = "pred", 
           terms = c("meanVPD", "MajorAxis_Dorsal"))
#car::vif(dorsal_model)
r.squaredGLMM(dorsal_desiccationModel)

# add on the Dorsal lightness scale to add beetles onto
scale_plot <- ggplot() +
  geom_point(aes(x = mdf$ave_melanism_dorsal, y = 0), color = NA) +
  labs(x = "Dorsal lightness (Average RGB value)", y = "") +
  theme_classic() +
  theme(xis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x=element_text(size=14))

cp2 <- cowplot::plot_grid(scale_plot, cp_color,nrow = 2, ncol = 1,
                          labels = c("A", ""), rel_heights = c(1,4)
                )
cp2
#ggsave(plot = cp2, filename = "figureOutputs/dorsalDesiccationModelResults_colorPoints_withDorsalScale.png",
#       dpi = 450, width = 8, height = 10 )

################ 
################
# Ventral model#
################
################
## let's fit some models
ventral_desiccationModel <- lmer(formula = ave_melanism_ventral ~ 
                                   AnnualPrecipitation + 
                                   meanSoilMoisture +
                                   #mammalAbundance + 
                                   #beetleAbundance + 
                                   meanVPD   + 
                                   MajorAxis_Dorsal + # this is body size
                                   MajorAxis_Dorsal:meanVPD +
                                   MajorAxis_Dorsal:meanSoilMoisture +
                                   (1|Tribe/Genus) + (1|IL_Site),
                                 data = mdf, 
                                 REML = F,
                                 na.action = "na.fail") # problem with beetleAbundance...fix this

summary(ventral_desiccationModel)
#tab_model(ventral_desiccationModel,
#          file = "modelResults/ventral_desiccationModel_satterthwaite.doc",
#          p.val = "satterthwaite", show.df = TRUE)

## ventral figures
ap_response_v <- plot_model(ventral_desiccationModel, type = "pred", terms = "AnnualPrecipitation")$data
msm_response_v <- plot_model(ventral_desiccationModel, type = "pred", terms = "meanSoilMoisture")$data
bs_response_v <- plot_model(ventral_desiccationModel, type = "pred", terms = "MajorAxis_Dorsal")$data
int_response_v <- plot_model(ventral_desiccationModel, type = "pred", 
                             terms = c("meanSoilMoisture", "MajorAxis_Dorsal [-2,0,2]"))$data

b_v <- ggplot() + 
  geom_ribbon(ap_response_v,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "black", alpha = 0.25) +
  geom_point(mdf, mapping = aes(x = AnnualPrecipitation, y = ave_melanism_ventral), 
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             alpha = 0.08) +
  geom_line(ap_response_v, mapping = aes(x = x, y = predicted), color = "black") +
  ggtitle("") +
  labs(x = "Annual Precipitation", y = "Ventral lightness") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.y = element_text(angle = 0)) +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title=element_text(size=14))

c_v <- ggplot() + 
  geom_ribbon(msm_response_v,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "black", alpha = 0.25) +
  geom_point(mdf, 
             mapping = aes(x = meanSoilMoisture, y = ave_melanism_ventral),
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             alpha = 0.08) +
  geom_line(msm_response_v, mapping = aes(x = x, y = predicted), color = "black") +
  ggtitle("") +
  labs(x = "Soil moisture", y = "Ventral lightness") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.y = element_text(angle = 0)) +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title=element_text(size=14))

a_v <- ggplot() + 
  geom_ribbon(bs_response_v,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "black", alpha = 0.25) +
  geom_point(mdf, 
             mapping = aes(x = MajorAxis_Dorsal, y = ave_melanism_ventral),
             alpha = 0.08) +
  geom_line(bs_response_v, mapping = aes(x = x, y = predicted), color = "black") +
  ggtitle("") +
  labs(x = "Body size", y = "Ventral lightness") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.y = element_text(angle = 0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 13),
        axis.title=element_text(size=14))

d_v <- ggplot() + 
  geom_ribbon(int_response_v,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.25) +
  geom_point(mdf, 
             mapping = aes(x = meanSoilMoisture, y = ave_melanism_ventral),
             alpha = 0.08,
             position = position_jitter(height = 0, width = 0.05, seed = 1)) +
  geom_line(int_response_v, mapping = aes(x = x, y = predicted, color = group)) +
  ggtitle("") +
  labs(x = "Soil moisture", y = "Ventral lightness", 
       color = "Body size",
       fill = "Body size") +
  scale_fill_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                    labels = c( paste("-2 s.d.", "(Small)"), 
                                paste("0 s.d.", "(Average)"),
                                paste("2 s.d.","(Large)"))) +
  scale_color_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                     labels = c( paste("-2 s.d.", "(Small)"), 
                                 paste("0 s.d.", "(Average)"),
                                 paste("2 s.d.","(Large)")))  +
  theme_classic() +
  theme(legend.position= c(0.25, 0.92),
        legend.text = element_text(size=8),
        axis.text = element_text(size = 12),
        axis.title=element_text(size=14))


cp_v <- cowplot::plot_grid(a_v,b_v,c_v,d_v, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
cp_v
#ggsave(plot = cp_v, filename = "figureOutputs/ventrallDesiccationModelResult_bw.png",
#       dpi = 450, width = 8, height = 8 )

d_v_color <- ggplot() + 
  geom_ribbon(int_response_v,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.25) +
  scale_fill_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                    labels = c( paste("-2 s.d.", "(Small)"), 
                                paste("0 s.d.", "(Average)"),
                                paste("2 s.d.","(Large)"))) +
  geom_point(mdf, 
             mapping = aes(x = meanSoilMoisture, y = ave_melanism_ventral, color = MajorAxis_Dorsal),
             alpha = 0.08,
             position = position_jitter(height = 0, width = 0.05, seed = 1),
             show.legend = FALSE) +
  scale_color_gradientn(colors = c("#e09f3e", "#9e2a2b", "#540b0e"),
                        breaks = c(-2, 0, 2),
                        limits = c(-2,3),
                        labels = c( paste("-2 s.d.", "(Small)"), 
                                    paste("0 s.d.", "(Average)"),
                                    paste("2 s.d.","(Large)"))) +
  new_scale_color() +
  geom_line(int_response_v, mapping = aes(x = x, y = predicted, color = group)) +
  scale_color_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                     labels = c( paste("-2 s.d.", "(Small)"), 
                                 paste("0 s.d.", "(Average)"),
                                 paste("2 s.d.","(Large)")))  +
  ggtitle("") +
  labs(x = "Soil moisture", y = "Ventral lightness", 
       color = "Body size",
       fill = "Body size") +
  theme_classic() +
  theme(legend.position= c(0.25, 0.92),
        legend.text = element_text(size=8),
        axis.text = element_text(size = 12),
        axis.title=element_text(size=14))


cp_v_color <- cowplot::plot_grid(a_v,b_v,c_v,d_v_color, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
cp_v_color
#ggsave(plot = cp_v_color, filename = "figureOutputs/ventrallDesiccationModelResult_color.png",
#       dpi = 450, width = 8, height = 8 )

r.squaredGLMM(ventral_desiccationModel)