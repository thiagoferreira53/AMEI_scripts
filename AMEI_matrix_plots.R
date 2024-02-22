## Please make sure that you are using the latest code version available at
## https://github.com/thiagoferreira53/AMEI_scripts

rm(list = ls(all.names = TRUE))

library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)
library(data.table)
library(cowplot)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##INPUTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path_output <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/plot_outputs/matrix_testing/"

variables <- c("Date", "TSLD", "model_name","soil_range", "soil", "site", "lai", "aw", "month_name")

#path_to_rds <- NA
path_to_rds <- "/Users/thiagoferreira53/Desktop/SoilTemperatureRawData-SQ_1year.rds"


#FILL THIS ONLY IF path_to_rds == NA
model <- "SQ"
data_folder <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/4-Spet1\ simulations/"
write_rds <- TRUE
n_rows <- 4018
rds_file_path <- "/blue/hoogenboom/t.berton/apps/Soil_Temp_CROP2ML/SQ_data.rds"
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##RAW DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
# MATRIX PLOT ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#functions to create plots in the matrix
upper_plots <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_text()+
    theme_grey()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top")
}


upper_plots2 <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_abline(intercept=0, slope=1)+
    geom_density2d()+
    xlim(-40,40)+
    ylim(-40,40)+
    #ylim(limits = c(0, 1))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top")
}


diag_plot <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_density()+
    xlim(-40,40)+
    ylim(0, 1)+
    #ylim(limits = c(0, 1))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top")
}

lower_plots <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_abline(intercept=0, slope=1)+
    geom_point(shape = ".")+
    xlim(-40,40)+
    ylim(-40,40)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top")
}

df <- reshape2::dcast(raw_data[,variables], Date+soil_range+soil+lai+aw+site+month_name ~ model_name,  
                      fun.aggregate = mean, value.var = "TSLD", na.rm=T)
#colnames(df)


potential_plot_variables <- c("soil_range", "soil", "site", "lai", "aw", "month_name")

#variable_list <- "month_name"
variable_list <- potential_plot_variables[potential_plot_variables %in% variables]

plot_matrixes <- function(var_name){
  print(paste0("Matrix plot is being generated using variable ...", var_name, " ... as color"))
  
  
  #consider only model columns
  p_matrix <- ggpairs(df, columns = 8:15, mapping=aes(color = !!sym(var_name)), 
                      #upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
                      upper = list(continuous = wrap(upper_plots2)),
                      lower = list(continuous = wrap(lower_plots)),
                      diag = list(continuous = wrap(diag_plot)),
                      #diag = list(continuous = "blankDiag")
                      legends=1)+
  theme(legend.position = "top") + 
  labs(fill = var_name, color = var_name)
  #scale_color_manual(values = my_colors)
  
  title_plot <- ""
  if(var_name == "aw"){
    title_plot <- "Water available"
  }else if(var_name == "soil"){
    title_plot <- "Soil type"
  }else if(var_name == "lai"){
    title_plot <- "Leaf area index"
  }else if(var_name == "site"){
    title_plot <- "Site"
  }
  
  legend_plot <- cowplot::get_legend(
    ggplot(df)+
    geom_point(aes(x=DSSAT, y=SiriusQuality, color = !!sym(var_name)))+
      guides(color = guide_legend(title = title_plot))+
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_text(size=18),
          legend.text = element_text(size=16))
    )
  
  p_matrix <- cowplot::plot_grid(legend_plot, ggmatrix_gtable(p_matrix), nrow = 2, rel_heights = c(0.05, 0.95))
  
  fileN <- paste0(path_output,"/matrix_plots_colored_by_",var_name,".jpg")
  
  ggsave(file=fileN,dpi =300,p_matrix,width = 17, height = 13)
  print(paste0("Matrix plot was generated and saved at ...",fileN))
  
}

lapply(variable_list, plot_matrixes)
#plot_matrixes(variable_list[5])

