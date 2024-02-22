## Please make sure that you are using the latest code version available at
## https://github.com/thiagoferreira53/AMEI_scripts

rm(list = ls(all.names = TRUE))

library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)
library(data.table)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##INPUTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path_output <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/plot_outputs"

#path_to_rds <- NA
path_to_rds <- "/Users/thiagoferreira53/Desktop/SoilTemperatureRawData-SQ_1year.rds"


#FILL THIS ONLY IF path_to_rds == NA
model <- "SQ"
data_folder <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/4-Spet1\ simulations/"
write_rds <- TRUE
n_rows <- 4018
rds_file_path <- "/Users/thiagoferreira53/Desktop/SoilTemperatureRawData-SQ_1year.rds"

#variables <- c("Date", "TSLD", "model_name","soil_range", "soil", "site", "lai", "aw")
variables <- c("Date", "TSLD", "soil", "site", "lai", "aw")


model_name <- c("DSSAT-EPIC", "DSSAT", "BioMA-Parton-SWAT", "SIMPLACE",
                "SiriusQuality", "STICS", "BioMA-SWAT")


runs_df <- expand.grid(model_name=model_name, layer_lvl=layer_lvl, stringsAsFactors=FALSE)

#variables to define limits of legend color
#min_legend_value <- -20
#max_legend_value <- 30

if(!is.na(path_to_rds)){
  raw_data <- readRDS(path_to_rds)
}else{
  files <- list.files(paste0(data_folder,model), full.names = T)
  
  read_files <- function(file){
    
    print(paste0("Reading file ",file," ..."))
    #df <- fread(file, na.strings = c("na","NA","-99",-99,"-99.000",-99.000),header = T)
    
    #testing
    if(!is.na(n_rows)){
      df <- read.table(file, na.strings = c("na","NA","-99",-99,"-99.000",-99.000),header = T, nrows = n_rows)
    }else{
      df <- read.table(file, na.strings = c("na","NA","-99",-99,"-99.000",-99.000),header = T)
    }
    
    df$Subdirectory <- dirname(file)
    df$File <- basename(file)
    df
  }
  
  #unlist function to change list of lists to a single big list
  raw_data <- bind_rows(lapply(files, read_files))
  
  raw_data$platform   <- substr(raw_data$File, 17, 18)
  raw_data$model      <- substr(raw_data$File, 20, 21)
  raw_data$site       <- substr(raw_data$File, 23, 26)
  raw_data$soil       <- substr(raw_data$File, 28, 31)
  raw_data$lai        <- as.numeric(substr(raw_data$File, 34, 34))
  raw_data$aw         <- as.numeric(substr(raw_data$File, 38, 41))
  
  raw_data$Date  <- as.Date(raw_data$Date)
  raw_data$year  <- as.numeric(format(raw_data$Date, format="%Y"))
  raw_data$month <- as.numeric(format(raw_data$Date, format="%m"))
  raw_data$year  <- as.numeric(raw_data$year)
  raw_data$month <- as.numeric(raw_data$month)
  
  raw_data$model_name[raw_data$model=="DE"] <- "DSSAT-EPIC"
  raw_data$model_name[raw_data$model=="DS"] <- "DSSAT"
  raw_data$model_name[raw_data$model=="PS"] <- "BioMA-Parton-SWAT"
  raw_data$model_name[raw_data$model=="SA"] <- "SIMPLACE"
  raw_data$model_name[raw_data$model=="SQ"] <- "SiriusQuality"
  raw_data$model_name[raw_data$model=="MO"] <- "MONICA"
  raw_data$model_name[raw_data$model=="ST"] <- "STICS"
  raw_data$model_name[raw_data$model=="SW"] <- "BioMA-SWAT"
  
  
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
  raw_data$aw <- as.factor(raw_data$aw)
  raw_data$lai <- as.factor(raw_data$lai)
  
  months <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
  raw_data$month_name <- months[ raw_data$month ]
  
  if(write_rds==TRUE) saveRDS(raw_data, rds_file_path)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##BOXPLOT SOIL TEMPERATURE PER LAYER OVER TIME##################################

###input

#define soil layer: 0, 5, 15, 30, 45, 60, 90, 120, 150, 180, 210, NA (all)

#soil lower limit
layer_lvl <- c(0, 5, 15, 30)

boxplot_temp_layer_by_month <- function(soil_layer){

    #READ THE DATA FOR THE SPECIFIC "soil_layer"
  #If soil_layer == NA, code will do an avg of all layers
  #created a summary of the data to make it faster to process (NOTE: this approach does not show outliers)
  if(!is.na(soil_layer)){
    raw_data <- raw_data[raw_data$SLLB==soil_layer,]
  }
  
  sum_data <- raw_data %>% 
    #group_by(model_name, site, soil, lai,aw, month) %>%
    group_by(model_name, site, soil, month) %>%
    summarise(ymin_TSLD = min(TSLD, na.rm = T),
              ymax_TSLD = max(TSLD, na.rm = T),
              median_TSLD = median(TSLD, na.rm = T),
              y25_TSLD = quantile(TSLD, 0.25, na.rm = T),
              y75_TSLD = quantile(TSLD, 0.75, na.rm = T),
              lower_whisker_TSLD = max(min(TSLD, na.rm = T), quantile(TSLD, 0.25, na.rm = T) - 
                                         (IQR(TSLD, na.rm = T)*1.5)),#lower whisker
              upper_whisker_TSLD = min(max(TSLD, na.rm = T), quantile(TSLD, 0.75, na.rm = T) + 
                                         (IQR(TSLD, na.rm = T)*1.5))) #upper whisker
  
  
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
                 width = 0.8,position = position_dodge(width = 1))+ #this removes the spaces in between months
    geom_hline(yintercept=0)+
    scale_x_discrete(labels=c("Jan","Feb","Mar","Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
    scale_y_continuous(breaks = seq(-30,40,10), limits = c(-30,40))+#limit = c(min(raw_data$TSLD), max(raw_data$TSLD)))+
    facet_wrap(~site+soil, nrow =7, ncol=4,scales='free',labeller = label_wrap_gen(multi_line=FALSE))+
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
    ylab(paste0("Layer ", soil_layer, " soil temperature (°C)"))+
    xlab("Month")
  
  ggsave(file=paste0(path_output,"/model_boxplot/boxplot_soil_temp_layer",soil_layer,".jpg"),
         dpi =300,boxplot_soil_temp,width = 17, height = 15)
  
  
}

lapply(layer_lvl, boxplot_temp_layer_by_month)
#boxplot_temp_layer_by_month(5)




