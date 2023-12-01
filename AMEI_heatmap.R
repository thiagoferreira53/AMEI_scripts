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

layer_lvl <- c(0,90,210)

model_name <- c("DSSAT-EPIC", "DSSAT", "BioMA-Parton-SWAT", "SIMPLACE",
                "SiriusQuality", "STICS", "BioMA-SWAT")


runs_df <- expand.grid(model_name=model_name, layer_lvl=layer_lvl, stringsAsFactors=FALSE)

#variables to define limits of legend color
max_legend_value <- -20
min_legend_value <- 30

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
##HEATMAP BY LAYER & MODEL ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#TESTING
#model_name <- "DSSAT-EPIC"
#soil_layer

######
heatmap_plots_by_model <- function(df){
  
  layer      <- as.integer(df[['layer_lvl']])
  model_name <- df[['model_name']]
  
  #df_heatmap_date <- raw_data[raw_data$model_name==model_name & raw_data$SLLB == soil_layer,variables]
  #df_heatmap_date$var <- paste(df_heatmap_date$site,df_heatmap_date$soil)
  #df_heatmap_date <- df_heatmap_date[ , -which(names(df_heatmap_date) %in% c("site","soil"))]
  
  #df_heatmap <- df_heatmap_date %>%
  #  group_by(lai, aw, var) %>%
  #  summarise(mean_TSLD = mean(TSLD))
  
  #df_heatmap$lai <- as.numeric(df_heatmap$lai)
  #df_heatmap$aw <- as.numeric(df_heatmap$aw)
  #heatmap(as.matrix(df_heatmap[, -3]), scale = "none")
  #colnames(df_heatmap)

  print(paste0("Heatmap is being generated for the model ...", model_name, " layer ", layer))
  
  df_heatmap <- raw_data[raw_data$model_name==model_name & raw_data$SLLB == layer,variables] %>%
    group_by(lai, aw, site, soil) %>%
    summarise(mean_TSLD = mean(TSLD))
  
  upper_limit <- max(df_heatmap$mean_TSLD, na.rm = T)
  lower_limit <- min(df_heatmap$mean_TSLD, na.rm = T)
  
  print(paste0("Model ...", model_name, " layer ", layer, " maximum mean soil temperature: ", upper_limit))
  if(upper_limit < max_legend_value) {
    warning(paste0("Model ...", model_name, " layer ", layer, 
                   " maximum mean soil temperature is higher than the max_legend_value. Adjust limits ..."))
  }
  print(paste0("Model ...", model_name, " layer ", layer, " minimum mean soil temperature: ", lower_limit))
  if(lower_limit > min_legend_value) {
    warning(paste0("Model ...", model_name, " layer ", layer,
                   " minimum mean soil temperature is lower than the min_legend_value. Adjust limits ..."))
  }
  
  df_heatmap$x <- paste0("LAI ",df_heatmap$lai, ", AW", df_heatmap$aw)
  
  heatmap_plot <- ggplot(df_heatmap, aes(y = interaction(soil, site, sep = ","), x = x, fill = mean_TSLD)) +
    geom_tile()+
    scale_x_discrete(position = "bottom")+
    scale_fill_gradient2(low = "#fffaf7", high = "#b8161b", name = "Mean soil\ntemp. (°C)", 
                         limits = c(max_legend_value,min_legend_value))+
    theme_bw()+
    theme(axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5,face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0("Model: ", model_name, "  Soil layer: ", layer)) 
    #scale_fill_manual(values = c("#fffaf7", "#fbe4e2", "#f8cdcc", "#f4b7b7", 
    #                             "#f1a0a1", "#ed8a8c", "#e97477", "#e65d61", 
    #                             "#e2474c", "#df3036", "#db1a21"))
  
  ggsave(file=paste0(path_output,"/model_heatmap/Heatmap_model-",model_name,"_layer-", layer,".jpg"),
         dpi =300,heatmap_plot,width = 17, height = 13)
}


apply(runs_df,MARGIN=1,FUN=heatmap_plots_by_model)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##HEATMAP BY LAYER ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heatmap_plots_layer <- function(layer){
  
  print(paste0("Heatmap is being generated for the layer ", layer))
  
  df_heatmap <- raw_data[raw_data$SLLB == layer,c(variables, "model_name")] %>%
    group_by(lai, aw, site, soil, model_name) %>%
    summarise(mean_TSLD = mean(TSLD))
  
  upper_limit <- max(df_heatmap$mean_TSLD, na.rm = T)
  lower_limit <- min(df_heatmap$mean_TSLD, na.rm = T)
  
  print(paste0("Layer ", layer, " maximum mean soil temperature: ", upper_limit))
  if(upper_limit < max_legend_value) {
    warning(paste0("Layer ", layer, 
                   " maximum mean soil temperature is higher than the max_legend_value. Adjust limits ..."))
  }
  print(paste0("Layer ", layer, " minimum mean soil temperature: ", lower_limit))
  if(lower_limit > min_legend_value) {
    warning(paste0("Layer ", layer,
                   " minimum mean soil temperature is lower than the min_legend_value. Adjust limits ..."))
  }
  
  df_heatmap$x <- paste0(model_name," LAI ",df_heatmap$lai, ", AW", df_heatmap$aw)
  
  heatmap_plot <- ggplot(df_heatmap, aes(y = x, x = interaction(soil, site, sep = ","), fill = mean_TSLD)) +
    geom_tile()+
    scale_x_discrete(position = "bottom")+
    scale_fill_gradient2(low = "#fffaf7", high = "#b8161b", name = "Mean soil\ntemp. (°C)", 
                         limits = c(max_legend_value,min_legend_value))+
    theme_bw()+
    theme(axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5,face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0("Model: ", model_name, "  Soil layer: ", layer)) 
  #scale_fill_manual(values = c("#fffaf7", "#fbe4e2", "#f8cdcc", "#f4b7b7", 
  #                             "#f1a0a1", "#ed8a8c", "#e97477", "#e65d61", 
  #                             "#e2474c", "#df3036", "#db1a21"))
  
  ggsave(file=paste0(path_output,"/model_heatmap/Heatmap_layer_flipped-", layer,".jpg"),
         dpi =300,heatmap_plot,width = 17, height = 13)
}

lapply(layer_lvl,heatmap_plots_layer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##BIG HEATMAP ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heatmap_plots <- function(layer_list){
  print(paste0("Heatmap is being generated using layers ", layer_list))

  df_heatmap <- raw_data[raw_data$SLLB %in% layer_list,c(variables, "model_name", "SLLB")] %>%
    group_by(lai, aw, site, soil, model_name, SLLB) %>%
    summarise(mean_TSLD = mean(TSLD))
  
  upper_limit <- max(df_heatmap$mean_TSLD, na.rm = T)
  lower_limit <- min(df_heatmap$mean_TSLD, na.rm = T)
  
  print(paste0("Maximum mean soil temperature: ", upper_limit))
  if(upper_limit < max_legend_value) {
    warning(paste0("Maximum mean soil temperature is higher than the max_legend_value. Adjust limits ..."))
  }
  print(paste0("Minimum mean soil temperature: ", lower_limit))
  if(lower_limit > min_legend_value) {
    warning(paste0("Minimum mean soil temperature is lower than the min_legend_value. Adjust limits ..."))
  }
  
  df_heatmap$x <- paste0(model_name," LAI ",df_heatmap$lai, ", AW", df_heatmap$aw)
  df_heatmap$y <- paste0(df_heatmap$soil,df_heatmap$site, ", L", df_heatmap$SLLB)
  
  heatmap_plot <- ggplot(df_heatmap, aes(y = y, x = x, fill = mean_TSLD)) +
    geom_tile()+
    scale_x_discrete(position = "bottom")+
    scale_fill_gradient2(low = "#fffaf7", high = "#b8161b", name = "Mean soil\ntemp. (°C)", 
                         limits = c(max_legend_value,min_legend_value))+
    theme_bw()+
    theme(axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5,face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_fill_manual(values = c("#fffaf7", "#fbe4e2", "#f8cdcc", "#f4b7b7", 
  #                             "#f1a0a1", "#ed8a8c", "#e97477", "#e65d61", 
  #                             "#e2474c", "#df3036", "#db1a21"))
  
  ggsave(file=paste0(path_output,"/model_heatmap/Heatmap.jpg"),
         dpi =300,heatmap_plot,width = 17, height = 13)
  }
  
heatmap_plots(layer_lvl)


TSLD_ensemble_median <- median(df_model_uncer$TSLD, na.rm=T)

#estimate error
df_model_uncer$TSLD_error <- df_model_uncer$TSLD - TSLD_ensemble_median
