library(tidyverse)
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

#### structural biotic interactions
dorsal_structuralModel <- lmer(formula = ave_melanism_dorsal ~ 
                                 #AnnualPrecipitation + 
                                 #meanSoilMoisture +
                                 mammalAbundance + 
                                 #meanVPD   + 
                                 MajorAxis_Dorsal + # this is body size
                                 MajorAxis_Dorsal:mammalAbundance +
                                 (1|Tribe/Genus) + (1|IL_Site),
                               data = mdf, 
                               REML = F,
                               na.action = "na.fail") # problem with beetleAbundance...fix this

summary(dorsal_structuralModel)


dsm <- plot_model(dorsal_structuralModel, type = "est", p.val = "satterthwaite")$data %>% 
  mutate(term = 
           case_when(term == "mammalAbundance" ~"Mammal abundance",
                     term == "MajorAxis_Dorsal" ~ "Body size",
                     term == "mammalAbundance:MajorAxis_Dorsal" ~ "Mammal abundance:Body size"))

dsm_a <- ggplot() +
  geom_point(dsm, mapping = aes(y = term, x = estimate)) +
  geom_errorbar(dsm, mapping = aes(y = term, xmin = conf.low, 
                                   xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-10,10)) +
  labs(x = "Fixed effect estimate", y = "") +
  theme_classic()


dsm_int_response <- plot_model(dorsal_structuralModel, type = "pred", p.val = "satterthwaite", 
                               terms = c("mammalAbundance", "MajorAxis_Dorsal [-2,0,2]"))$data


dsm_b <- ggplot() + 
  geom_ribbon(dsm_int_response,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.25) +
  scale_fill_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                    labels = c( paste("-2 s.d.", "(Small)"), 
                                paste("0 s.d.", "(Average)"),
                                paste("2 s.d.","(Large)"))) +
  geom_point(mdf, 
             mapping = aes(x = mammalAbundance, y = ave_melanism_ventral, color = MajorAxis_Dorsal),
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
  geom_line(dsm_int_response, mapping = aes(x = x, y = predicted, color = group)) +
  scale_color_manual(values = c("#e09f3e", "#9e2a2b", "#540b0e"),
                     labels = c( paste("-2 s.d.", "(Small)"), 
                                 paste("0 s.d.", "(Average)"),
                                 paste("2 s.d.","(Large)")))  +
  ggtitle("") +
  labs(x = "Mammal abundance", y = "Dorsal lightness", 
       color = "Body size",
       fill = "Body size") +
  theme_classic() +
  theme(legend.position= "left",
        legend.text = element_text(size=8),
        axis.text = element_text(size = 12),
        axis.title=element_text(size=14))


dsm_e <- egg::ggarrange(dsm_a, dsm_b, labels = c("A","B"),
                        ncol = 1, nrow = 2)

dsm_e
#ggsave(plot = dsm_e,
#       filename = "figureOutputs/dorsalStructuralModelResults.png",
#       width = 5, height = 6,
#       dpi = 450
#)

#sjPlot::tab_model(dorsal_structuralModel, 
#                  file = "modelResults/dorsal_structuralModel_satterthwaite.doc",
#                  p.val = "satterthwaite", show.df = TRUE)
#car::vif(dorsal_model)
r.squaredGLMM(dorsal_structuralModel)
