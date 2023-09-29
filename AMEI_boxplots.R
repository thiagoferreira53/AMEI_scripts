## Please make sure that you are using the latest code version available at
## https://github.com/thiagoferreira53/AMEI_scripts

rm(list = ls(all.names = TRUE))

library(ggplot2)
library(dplyr)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#NOTES - to fix
#1. using dplyr to summarize data ... might need to change it for the original 
#data (performance might be reduced but at least the outliers will show up)


#soil_temp_depth - should I break it down further? Currently, it is plotting TSLD
#by depth but not separated by aw nor soil type


##INPUTS #######################################################################

path_outputs <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/plot_outputs"


#sample - lai (0,2,7); models (DS,DE,SQ,SA,MO), sites (CAQC, COCA, DEMU, FRLU, FRMO, USGA, USMA) 
#aw (0), soil (SALO), platform (SQ)
data <- readRDS("/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/SoilTemperatureFakeData.rds")

raw_data <- data

raw_data$model_name <- NA

raw_data$model_name[raw_data$model=="DS"] <- "DSSAT"
raw_data$model_name[raw_data$model=="DE"] <- "DSSAT-EPIC"
raw_data$model_name[raw_data$model=="SQ"] <- "SiriusQuality"
raw_data$model_name[raw_data$model=="SA"] <- "SIMPLACE"
raw_data$model_name[raw_data$model=="MO"] <- "MONICA"


unique(raw_data$model_name)

#raw_data <- readRDS("/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/6-Analysis/SoilTemperatureRawData-SQ.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##BOXPLOT SOIL TEMPERATURE PER LAYER OVER TIME##################################

###input

#define soil layer: 0, 5, 15, 30, 45, 60, 90, 120, 150, 180, 210, NA (all)
layer <- 0

###process

if(is.na(layer)){
  #created a summary of the data to make it faster to process (NOTE: this approach does not show outliers)
  sum_data <- raw_data %>% 
    group_by(platform, model_name, site, soil, lai,aw, month) %>%
    summarise(ymin_TSLD = min(TSLD),
              ymax_TSLD = max(TSLD),
              median_TSLD = median(TSLD),
              y25_TSLD = quantile(TSLD, 0.25),
              y75_TSLD = quantile(TSLD, 0.75),
              lower_whisker_TSLD = max(min(TSLD), quantile(TSLD, 0.25) - (IQR(TSLD)*1.5)),#lower whisker
              upper_whisker_TSLD = min(max(TSLD), quantile(TSLD, 0.75) + (IQR(TSLD)*1.5))) #upper whisker
  
}else{
  #created a summary of the data to make it faster to process (NOTE: this approach does not show outliers)
  sum_data <- raw_data[raw_data$SLLB==layer,] %>% 
    group_by(platform, model_name, site, soil, lai,aw, month) %>%
    summarise(ymin_TSLD = min(TSLD),
              ymax_TSLD = max(TSLD),
              median_TSLD = median(TSLD),
              y25_TSLD = quantile(TSLD, 0.25),
              y75_TSLD = quantile(TSLD, 0.75),
              lower_whisker_TSLD = max(min(TSLD), quantile(TSLD, 0.25) - (IQR(TSLD)*1.5)),#lower whisker
              upper_whisker_TSLD = min(max(TSLD), quantile(TSLD, 0.75) + (IQR(TSLD)*1.5))) #upper whisker
  
}


boxplot_soil_temp <- ggplot(sum_data, aes(x = factor(month))) + 
  #stat_boxplot(geom = "errorbar", width = 40)+
  geom_errorbar(aes(ymin = lower_whisker_TSLD, 
                    ymax = upper_whisker_TSLD,
                    group=model_name), 
                width = 0.5, 
                position = position_dodge(width = 1))+
  geom_boxplot(aes(ymin = lower_whisker_TSLD, 
                   lower = y25_TSLD, 
                   middle = median_TSLD, 
                   upper = y75_TSLD, 
                   ymax = upper_whisker_TSLD,
                   fill=model_name),stat="identity",
               width = 0.8,position = position_dodge(width = 1))+
  geom_hline(yintercept=0)+
  facet_wrap(~site+lai, nrow =7, ncol=3,scales='free',labeller = label_wrap_gen(multi_line=FALSE))+
  theme_bw()+
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        legend.position="top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        axis.title =element_text(size=18),
        axis.text=element_text(size=10),
        legend.background = element_blank(),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.key.size = unit(2,"line"),
        strip.text.x = element_text(size = 14))+
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(-30,30,10))+#limit = c(min(raw_data$TSLD), max(raw_data$TSLD)))+
  ylab("Soil surface temperature (°C)")+
  xlab("Month")

boxplot_soil_temp

#update value for layer value for writing file in case all layer were selected
if(is.na(layer)) layer <- "all"

ggsave(file=paste0(path_outputs,"/boxplot_soil_temp_layer",layer,".jpg"),dpi =300,boxplot_soil_temp,width = 17, height = 13)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##BOXPLOT SOIL TEMPERATURE PER LAYER OVER DEPTH#################################

raw_data$soil_range <- paste0(raw_data$SLLT, "-",raw_data$SLLB)

raw_data$soil_range<- factor(raw_data$soil_range, 
                             levels=c("0-0","0-5","5-15","15-30","30-45","45-60","60-90","90-120","120-150","150-180","180-210"))

#limits for y-axis
ylimMin <- min(raw_data$TSLD)
ylimMax <- max(raw_data$TSLD)

soil_temp_depth <- ggplot(raw_data,aes(x = soil_range,
                    y = TSLD,
                    group=interaction(model_name,soil_range),
                    fill=model_name)) +
  geom_hline(yintercept=0)+
  stat_boxplot(geom="errorbar",position = position_dodge(width = 1))+
  geom_boxplot(width = 0.8,position = position_dodge(width = 1))+
  theme_bw()+
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        legend.position="top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        axis.title =element_text(size=18),
        axis.text=element_text(size=10),
        legend.background = element_blank(),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.key.size = unit(2,"line"),
        axis.text.x = element_text(angle = 20, vjust=0.8),
        strip.text.x = element_text(size = 14))+
  #scale_fill_manual(labels=c('DSSAT', 'SiriusQuality'),values=c('#F8766D','#00BFC4'))+
  scale_y_continuous(breaks=seq(-40, 40, 10))+#,limits = c(ylimMin,ylimMax))+
  facet_wrap(~site+lai, nrow =7, ncol=3,scales='free',labeller = label_wrap_gen(multi_line=FALSE))+
  ylab("Soil temperature (°C)")+
  xlab("Soil depth (cm)")

ggsave(file=paste0(path_outputs,"/soil_temp_depth.jpg"),dpi =300,soil_temp_depth,width = 17, height = 13)





