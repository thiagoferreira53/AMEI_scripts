## Please make sure that you are using the latest code version available at
## https://github.com/thiagoferreira53/AMEI_scripts


rm(list = ls(all.names = TRUE))


## Subroutine to compare outputs from different soil temp drivers
time = Sys.time()
print(paste0("Running AMEI quality control script ... ", time))

library(stringr)
library(dplyr)
library(readbulk)
library(ggplot2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## INPUT DATA ##################################################################

data_folder <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/4-Spet1\ simulations/"

#variables_to_check <- c("SLLT",	"SLLB",	"TSLD",	"TSLX",	"TSLN")
variables_to_check <- c("SLLT",	"SLLB",	"TSLD")

max_absolute_diff <- 0.01

output_error_list <- "N"
plotting <- "N"

#inform the rds path or use "" if you don't have one (the system will generate
#it and leave it inside the data_folder path + platform)

#use_rds <- "/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/4-Spet1\ simulations/SoilTemp_DS_RawData.rds"
use_rds <- ""

#Platform       Code
#BioMa	        BI
#DSSAT	        DS
#OpenAlea	      OA
#SIMPLACE	      SI
#SiriusQuality	SQ
#STICS	        ST

platform1 <- "SQ"
platform2 <- "MO"

## Model name ####
#Model	            Code
#DSSAT-ST	          DS
#DSSAT-EPIC	        DE
#EPIC	              EP
#STICS              ST
#SIMPLACE-APEX	    SA
#MONICA	            MO
#SiriusQuality	    SQ
#BioMA-SWAT	        SW
#BioMA-Campbell	    CA
#BioMA-Parton-SWAT	PS
#APSIM	            AP

## Crop2ML Converted model name (DSSAT testing)
#C2ML converted model	 Code
#Crop2ML-DSSAT         CS
#Crop2ML-DSSAT-EPIC    CD

model1 <- "MO"
model2<- "MO"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## DATA PROCESSING #############################################################

print(paste0("Starting data processing step ... Time: ", Sys.time()))

#check path name and add slash if needed
if(str_sub(data_folder, start= -1) != "/") data_folder <- paste0(data_folder,"/")


file_list1 <- list.files(paste0(data_folder,platform1),
                         pattern = paste0("_",platform1,"_",model1,"_"),
                         full.names = T)

file_list2 <- list.files(paste0(data_folder,platform2),
                         pattern = paste0("_",platform2,"_",model2,"_"),
                         full.names = T)

len_model1 <- length(file_list1)
len_model2 <- length(file_list2)


read_files <- function(file, platform){
  print(paste0("Reading file ",file," ..."))
  df <- read.table(file, na.strings = c("na","NA","-99",-99,"-99.000","na ","na,", "NA,",-99.000),header = T)
  
  #Format the output files (increase execution time)
  if(platform == "MO" | platform == "BI"){
  #Editing columns names (Monica outputs has "," and changes the column names)
  colnames(df) <- c("DATE", "SLLT", "SLLB", "TSLD", "TSLX", "TSLN")
  
  #removing commas from rows (Monica outputs)
  df <- apply(df, 2, function(x) gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", x)))
  
  #detect the actual type of the data columns
  df <- as_tibble(df)%>% 
    type.convert(as.is = TRUE)
  
  #Rounding values to three decimal (BioMa outputs has more)
  df['TSLD'] <- round(df['TSLD'],3)
  df['TSLX'] <- round(df['TSLX'],3)
  df['TSLN'] <- round(df['TSLN'],3)
  
  }
  
  df$Subdirectory <- dirname(file)
  df$File <- basename(file)

  #print(head(df))
  df
}

print("Reading data files ...")
list_df1 <- lapply(file_list1, read_files, platform = platform1)
print(paste0("Finished reading data ... Running time: ", Sys.time() - time))
list_df2 <- lapply(file_list2, read_files, platform = platform2)
print(paste0("Finished reading data ... Running time: ", Sys.time() - time))

#print("Row binding all data files for analysis step ...")
model1_results <- bind_rows(list_df1)
model2_results <- bind_rows(list_df2)


#making sure the column headers match
#colnames(model1_results) <- c("Date","SLLT","SLLB","TSLD","TSLX","TSLN","Subdirectory","File")
#colnames(model2_results) <- c("Date","SLLT","SLLB","TSLD","TSLX","TSLN","Subdirectory","File")


print(paste0("Finished data processing step ... Running time: ", Sys.time() - time))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## DATA ANALYSIS ###############################################################

print(paste0("Starting data analysis step ... Time: ", Sys.time()))

if(len_model1 != len_model2){
  print(paste0("ERROR ... model1 (",len_model1,") does not have the same dataframe size as model2 (",len_model2,") ..."))
}else{
  #creating empty dataframe for tracking errors
  columns = c("variable","Date","dataframe_index_error","value_model1", "value_model2",
              "absolute_difference", "file_model1", "file_model2")  
  error_table = data.frame(matrix(nrow = 0, ncol = length(columns)))  
  colnames(error_table) = columns
  
  #simple_check
  print("Checking absolute differences between drivers across all output files ... ")
  print("Settings:")
  print(paste0("Maximum absolute difference acceptable between models: ",max_absolute_diff))
  print(paste0("Variable list: ", variables_to_check))
  print(paste0("Platform 1: ",platform1))
  print(paste0("Model 1: ",model1))
  
  print(paste0("Platform 2: ",platform2))
  print(paste0("Model 2: ",model2))
  
  check_abs_diff <- function(var){
    print(paste0("Checking ", var, " ... "))
    
    #checking absolute differences
    check_differences <- abs(abs(model1_results[var]) - abs(model2_results[var])) > max_absolute_diff
    
    #listing rows with differences from model1
    m1 <- model1_results[check_differences,var]
    
    #listing rows with differences from model2
    m2 <- model2_results[check_differences,var]
    
    if(length(check_differences[which(check_differences==TRUE)])==0){
      print(paste0(var, " ...OK..."))
    }else{
      print(paste0(var, " ...ERROR..."))
      print("Differences detected ... Please check data table 'error_table' for more information ... ")
      error_table <<- rbind(error_table, data.frame(variable = var,
                                                    Date = model1_results[check_differences,"Date"],
                                                    SLLT = model1_results[check_differences,"SLLT"],
                                                    SLLB = model1_results[check_differences,"SLLB"],
                                                    value_model1 =  m1,
                                                    value_model2 = m2,
                                                    absolute_difference = abs(abs(m1) - abs(m2)),
                                                    file_model1 = model1_results[check_differences,"File"],
                                                    file_model2 =model2_results[check_differences,"File"]))
      #might need to remove any comparisons made with missing values using na.omit(error_table)**
      error_table
    }
  }
  
  lapply(variables_to_check, check_abs_diff)
  
  print(paste0("Finished data analysis step ... Running time: ", Sys.time() - time))
  
}

if(output_error_list=="Y"){
  
  ## checking
  
  #tail(error_table)
  df2 <- na.omit(error_table)
  
  #max(subset$absolute_difference, na.rm = T)

  #list file names (based on model 1) with issues
  #unique(df$file_model1)  
  
  df <- na.omit(df2[df2$variable=="TSLD" & df2$absolute_difference>0.01,])
  write.csv(df, file = paste0("/Users/thiagoferreira53/Desktop/UF/-Research/AMEI/errors/TSLD_errors_",platform1,"_",model1,"_",platform2,"_",model2,".csv"))

}

# DSSAT testing
#- [X] DSDS x DSCS
#- [ ] DSDE x DSCD

#- [X] DSDS x SQDS
#- [X] DSSQ x SQSQ
#- [X] DSDE x SQDE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## DATA VISUALIZATION ##########################################################

if(plotting == "Y"){
  error_table[error_table$absolute_difference > 10 & error_table$variable =="TSLD",]
  
  ggplot(data=error_table[error_table$variable =="TSLD",],aes(x=variable, y=absolute_difference, fill = variable))+
    stat_boxplot(geom = "errorbar")+
    geom_boxplot()+
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=14),
          legend.title=element_blank(),
          axis.title =element_text(size=18),
          axis.text=element_text(size=10),
          legend.background = element_blank(),
          legend.spacing.x = unit(1.0, 'cm'),
          legend.key.size = unit(2,"line"),
          axis.text.x = element_text(vjust=-0.5))+
    ylab("Absolute difference")+
    xlab("Variable")
  
  print(paste0("Done ... ", Sys.time()))

}


