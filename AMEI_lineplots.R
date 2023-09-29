## Please make sure that you are using the latest code version available at
## https://github.com/thiagoferreira53/AMEI_scripts

rm(list = ls(all.names = TRUE))

library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

xlimMin <- min(raw_data$Date)
xlimMax <- max(raw_data$Date)

raw_data$soil_range <- paste0("Soil layer ",raw_data$SLLT, "-",raw_data$SLLB," (cm)")

raw_data$soil_range<- factor(raw_data$soil_range, 
                             levels=c("Soil layer 0-0 (cm)",
                                      "Soil layer 0-5 (cm)",
                                      "Soil layer 5-15 (cm)",
                                      "Soil layer 15-30 (cm)",
                                      "Soil layer 30-45 (cm)",
                                      "Soil layer 45-60 (cm)",
                                      "Soil layer 60-90 (cm)",
                                      "Soil layer 90-120 (cm)",
                                      "Soil layer 120-150 (cm)",
                                      "Soil layer 150-180 (cm)",
                                      "Soil layer 180-210 (cm)"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

line_plot_st <- ggplot()+
  geom_line(data=raw_data,aes(x=as.Date(Date, format = "%Y-%m-%d"),y=TSLD, color=model_name,
                              group=model_name))+
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
        legend.key.size = unit(1,"line"),
        axis.text.x = element_text(vjust=-0.5))+
  ylab("Soil surface temperature (°C)")+
  xlab("Date")+
  scale_x_date(date_breaks="2 years",limits = as.Date(c(xlimMin, xlimMax)),date_labels =  "%Y")+
  scale_y_continuous(breaks=seq(-30, 30, 10), limits=c(-30,30))+#,limits = c(ylimMin,ylimMax))+
  facet_wrap(~soil_range, nrow =4, ncol=3,scales='free',labeller = label_wrap_gen(multi_line=FALSE))
line_plot_st

ggsave(file=paste0(path_outputs,"/line_soil_temp_over_layers.jpg"),dpi =300,line_plot_st,width = 17, height = 13)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum_data <- raw_data %>%
  group_by(soil_range, Date) %>%
  summarise(mean_TSLD=mean(TSLD),
            median_TSLD=median(TSLD))


median_TSLD <- median(raw_data$TSLD)

ggplot()+
  geom_line(data=raw_data,aes(x=as.Date(Date, format = "%Y-%m-%d"),y=TSLD-median_TSLD, color=model_name,
                              group=model_name))+
  geom_line(data=sum_data,aes(x=as.Date(Date, format = "%Y-%m-%d"),y=median_TSLD))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
colnames(raw_data)

raw_data$dayMonth <- format(as.Date(raw_data$Date),format ="%m-%d")

sum_data <- raw_data %>%
  group_by(soil,lai,soil_range,aw,site, dayMonth, model_name) %>%
  summarise(mean_TSLD=mean(TSLD),
            median_TSLD=median(TSLD),
            max_TSLD = max(TSLD),
            min_TSLD = min(TSLD))

xlimMin = as.Date(min(sum_data$dayMonth), format = "%m-%d")
xlimMax = as.Date(max(sum_data$dayMonth), format = "%m-%d")

ggplot()+
  geom_line(data=sum_data,aes(x=as.Date(dayMonth, format = "%m-%d"),y=mean_TSLD, color=model_name,
                              group=model_name))+
  scale_x_date(date_breaks="1 month", date_labels = "%b", limits = as.Date(c(xlimMin, xlimMax)))+
  ylim(-40,40)+
  facet_wrap(~soil_range,scales='free',labeller = label_wrap_gen(multi_line=FALSE))+
  ylab("Soil temperature (°C)")+
  xlab("Month")


  geom_ribbon(aes(x = WYEAR, ymin = min, ymax = max, fill="blue"), alpha = 0.3)
  



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MATRIX PLOT 
colnames(raw_data)
#df <- dcast(raw_data[,c(3,4,5,6,7,17,12,18)],  Date+soil_range+soil+lai+aw+site ~ model_name)
df <- dcast(raw_data[,c(7,12,17)],  Date ~ model_name)
colnames(df)


pair_plot <- ggpairs(df[,-1],diag = list(continuous = "blankDiag"))+
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
        legend.key.size = unit(1,"line"),
        axis.text.x = element_text(vjust=-0.5))+
  geom_abline(intercept=0, slope=1)+
  xlim(-40,40)+
  ylim(-40,40)
  
pair_plot
?ggpairs
