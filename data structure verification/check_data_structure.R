# Check data structure conformity ----
# OWFSOMM project, 29-04-2026

`%!in%` = Negate(`%in%`)


# Packages 
library(ggplot2)
library(dplyr)
library(sf)
library(ggspatial)
library(tidyr)
library(stringr)
library(units)
library(dsm)
library(Distance)
library(assertthat)
library(assertable)
library(glue)
library(cli)
library(hms)
library(readxl)
library(here)
library(tibble)
library(lubridate)
library(purrr)



# Load species code
Sp_Code <- read.csv2(paste0(path,'species_code_OWFSOMM.csv'), sep=";")


## General functions ---- 

#### column_ref

column_ref <- function(data){
  
  if("Transect_ID" %in% names(data) == TRUE){ID <- "Transect_ID"}
  if("Image_ID" %in% names(data) == TRUE){ID <- "Image_ID"}
  if("Object_ID" %in% names(data) == TRUE){ID <- "Object_ID"}
  if("Target_ID" %in% names(data) == TRUE){ID <- "Target_ID"}
  
  return(ID)
  
  
  #### table_name
  
  table_name <- function(data){
    
    if("Transect_ID" %in% names(data) == TRUE){name <- "transect"}
    if("Image_ID" %in% names(data) == TRUE){name <- "image"}
    if("Object_ID" %in% names(data) == TRUE){name <- "object"}
    if("Target_ID" %in% names(data) == TRUE){name <- "target"}
    if(all(c("Transect_ID","Image_ID","Target_ID","Object_ID") %!in% names(data))) {name <- "data"}
    return(name)
  }
  
  
  #### column_here
  
  column_here <- function(data, col, namedata = data){
    
    if(col %!in% names(data)){
      
      cli_alert_danger(glue_collapse("Column {col} is missing in {namedata}"))
    }
    
    else return(TRUE)
    
  }
  
  #### verif_column_ID
  
  verif_column_ID <- function(data, col) {
    
    if(deparse(substitute(col))=="Transect_ID") {namedata <- "transect"}
    if(deparse(substitute(col))=="Image_ID") {namedata <- "image"}
    if(deparse(substitute(col))=="Object_ID") {namedata <- "object"}
    if(deparse(substitute(col))=="Target_ID") {namedata <- "target"}
    if(all(c("Transect_ID","Image_ID","Target_ID","Object_ID") %!in% names(data))) {namedata <- "data"}
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      # ID unique
      if(isFALSE(length(unique(x)) == nrow(data))){
        
        out <- data %>%
          group_by({{col}}) %>%
          summarize(n=n()) %>%
          filter(n > 1) %>%
          pull({{col}})
        
        cli_alert_danger(glue_collapse('{deparse(substitute(col))} : {out} not unique, please check'))}
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        out <- which(is.na(x))
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} at lines {out}, please check'))}
      
      # Correct
      if(all(all(is.na(x) == FALSE) & isTRUE(length(unique(x)) == nrow(data)))){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
      
    }
    
  }
  
  #### verif_column_NA
  
  verif_column_NA <- function(data, col) {
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      
      x <- data %>%
        pull({{col}})
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        ID <- column_ref(data)
        out <- data %>%
          filter(is.na({{col}}) == TRUE) %>%
          pull(ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for {ID}: {out}, please check'))} 
      
      # Correct
      else {cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
    }
    
  }
  
  
  
  #### verif_column_facultative
  
  
  verif_column_facultative <- function(data, col) {
    
    namedata <- table_name(data)
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      x <- data %>%
        pull({{col}})
      
      if(any(all(x=="NA"), all(is.na(x)==TRUE))){cli_alert_success(glue('Only NA in column {deparse(substitute(col))}'))} else {cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
    }
    
  }
  
  
  #### verif_column_semi_facultative
  
  verif_column_semi_facultative <- function(data, col) {
    
    namedata <- table_name(data)
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      if(any(all(x=="NA"), all(is.na(x)==TRUE))){cli_alert_danger(glue('Column {deparse(substitute(col))} contains only NA'))} else {cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
    }
    
  }
  
  
  #### verif_column_num
  
  verif_column_num <- function(data, col) {
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      # Class
      if(is.numeric(x) == FALSE){cli_alert_danger(glue('Column {deparse(substitute(col))} should be numeric, try: "{namedata} %>% mutate_at("{deparse(substitute(col))}", as.numeric)'))}
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        ID <- column_ref(data)
        out <- data %>%
          filter(is.na(x) == TRUE) %>%
          pull(ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for {ID}: {out}, please check'))} 
      
      # Correct
      if(all(all(is.na(x) == FALSE) & is.numeric(x) == TRUE)){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
    }
    
  }
  
  
  #### verif_column_date
  
  verif_column_date <- function(data, col) {
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      # Class
      if(is.Date(x) == FALSE){cli_alert_danger(glue('Column {deparse(substitute(col))} should be in Date class, try: "{namedata} %>% mutate_at("{deparse(substitute(col))}", as.Date)"'))}
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        ID <- column_ref(data)
        out <- data %>%
          filter(is.na(x) == TRUE) %>%
          pull(ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for {ID}: {out}, please check'))} 
      
      # Correct
      if(all(all(is.na(x) == FALSE) & is.Date(x) == TRUE)){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
    }
    
  }
  
  
  #### verif_column_time
  
  verif_column_time <- function(data, col) {
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      # Class
      if(is_hms(x) == FALSE){cli_alert_danger(glue('Column {deparse(substitute(col))} should be in hms class, try: "{namedata} %>% mutate_at("{deparse(substitute(col))}", as_hms)"'))}
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        ID <- column_ref(data)
        out <- data %>%
          filter(is.na(x) == TRUE) %>%
          pull(ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for {ID}: {out}, please check'))} 
      
      # Correct
      if(all(all(is.na(x) == FALSE) & is_hms(x) == TRUE)){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      
    }
    
  }
  
  
  #### verif_longitude
  
  verif_longitude <- function(data, col, xmin = 6, xmax = 9){
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE){
      
      x <- data %>%
        pull({{col}})
      
      # Class
      if(is.numeric(x) == FALSE){cli_alert_danger(glue('Column {deparse(substitute(col))} should be numeric, try: "{namedata} %>% mutate_at("{deparse(substitute(col))}", as.numeric)"'))}
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        ID <- column_ref(data)
        out <- data %>%
          filter(is.na(x) == TRUE) %>%
          pull(ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for {ID}: {out}, please check'))} 
      
      
      if(all(all(is.na(x) == FALSE) & is.numeric(x) == TRUE)){
        
        # Pb longitude
        if(any((x < xmax & x > xmin) == FALSE, na.rm = TRUE)){
          
          if("Transect_ID" %in% names(data) == TRUE){ID <- "Transect_ID"}
          if("Image_ID" %in% names(data) == TRUE){ID <- "Image_ID"}
          if("Target_ID" %in% names(data) == TRUE){ID <- "Target_ID"}
          
          
          out <-  data %>%
            filter(x > xmax | x < xmin) %>%
            pull(ID)
          
          cli_alert_danger(glue_collapse('Longitude coordinates in {ID}: {out} not correct, please check'))} 
        
        # Correct
        if(all((x < xmax & x > xmin), na.rm = TRUE)){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      }
      
    }
    
  }
  
  #### verif_latitude
  
  
  verif_latitude <- function(data, col, ymin = 41, ymax = 53) {
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      # Class
      if(is.numeric(x) == FALSE){cli_alert_danger(glue('Column {deparse(substitute(col))} should be numeric, try: "{namedata} %>% mutate_at("{deparse(substitute(col))}", as.numeric)"'))}
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        ID <- column_ref(data)
        out <- data %>%
          filter(is.na(x) == TRUE) %>%
          pull(ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for {ID}: {out}, please check'))} 
      
      
      if(all(all(is.na(x) == FALSE) & is.numeric(x) == TRUE)){
        
        # Pb longitude
        if(any((x < ymax & x > ymin) == FALSE, na.rm = TRUE)){
          
          if("Transect_ID" %in% names(data) == TRUE){ID <- "Transect_ID"}
          if("Image_ID" %in% names(data) == TRUE){ID <- "Image_ID"}
          if("Target_ID" %in% names(data) == TRUE){ID <- "Target_ID"}
          
          out <-  data %>%
            filter(x > ymax | x < ymin) %>%
            pull(ID)
          
          
          cli_alert_danger(glue_collapse('Latitude coordinates in {ID}: {out} not correct, please check'))} 
        
        # Correct
        if(all((x < ymax & x > ymin), na.rm = TRUE)){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
      }
      
      
    }
  }
  
  
  #### verif_link
  
  verif_link <- function(data1, data2, col) {
    
    namedata1 <- table_name(data1)
    namedata2 <- table_name(data2)
    
    if(column_here(data1, deparse(substitute(col)), namedata1) == TRUE) {
      if(column_here(data2, deparse(substitute(col)), namedata2) == TRUE) {
        
        x1 <- data1 %>%
          filter(is.na({{col}}) == FALSE) %>%
          filter({{col}} != "NA") %>%
          pull({{col}})
        
        x2 <- data2 %>%
          pull({{col}})
        
        
        
        if(all(x1 %in% x2) == FALSE){
          
          ID <- column_ref(data1)   
          out <- data1 %>%
            filter({{col}} %!in% x2) %>%
            filter({{col}} != "NA") %>%
            filter(is.na({{col}}) == FALSE) %>%
            pull(ID)
          
          cli_alert_danger(glue_collapse('For {ID}: {out} of {namedata1}, {deparse(substitute(col))} associated are not in {namedata2}'))}
        
        if(all(x1 %in% x2)){cli_alert_success(glue('All {deparse(substitute(col))} of {namedata1} are in {namedata2}'))}
      }
    }
  }
  
  ## Table Transect ----
  
  #### verif_transect
  
  verif_transect <- function(data){
    
    
    # Verif by columns
    verif_column_ID(data, Transect_ID)
    verif_column_NA(data, Survey_ID)  
    verif_column_NA(data, Flight_ID)
    verif_column_NA(data, Plane)
    verif_column_NA(data, System)
    verif_column_date(data, Date)
    verif_column_num(data, Altitude)
    verif_column_num(data, Speed)
    verif_column_time(data, Start_Time)     
    verif_longitude(data, Start_Longitude)
    verif_latitude(data, Start_Latitude)
    verif_column_time(data, End_Time)
    verif_longitude(data, End_Longitude)
    verif_latitude(data, End_Latitude)
    verif_column_num(data, Nb_Images)
  }
  
  
  #### verif_nb_images
  
  verif_nb_image <- function(transect_obj, image_obj){
    
    tac <- image_obj %>%
      filter(is.na(Transect_ID) == FALSE) %>%
      group_by(Transect_ID) %>%
      summarise(n = n()) %>%
      st_drop_geometry()
    
    tic <- transect_obj %>%
      left_join(tac, by = "Transect_ID") %>%
      st_drop_geometry()
    
    
    if(any(is.na(tic$n) == TRUE)){
      
      out <- tic %>%
        filter(is.na(n) == TRUE) %>%
        pull(Transect_ID)
      
      cli_alert_danger(glue_collapse('0 image in image table for Transect_ID {out}. Verifications needed with image table or remove transects with 0 image from the transect table')) }  
    
    # Not identical
    if(length(which(tic$Nb_Images != tic$n))>0){
      
      out <- tic$Transect_ID[which(tic$Nb_Images != tic$n)]
      
      cli_alert_danger(glue_collapse('For Transect_ID: {out}, Nb_Images not identical between transect table and image table'))}
    
    if(all(all(is.na(tic$n) == FALSE) & isFALSE(length(which(tic$Nb_Images != tic$n))>0))){cli_alert_success('Nb_images identical between transect table and image table')}
  }
  
  
  
  ## Table Image ----
  
  #### verif_image
  
  verif_image <- function(data){
    
    # Verif by columns
    verif_column_ID(data, Image_ID)
    verif_column_NA(data, Transect_ID)
    verif_column_time(data, Time)
    verif_column_NA(data, Camera_ID)
    verif_longitude(data, Longitude_Plane)
    verif_latitude(data, Latitude_Plane)
    verif_longitude(data, Longitude_Top_Left)
    verif_latitude(data, Latitude_Top_Left)
    verif_longitude(data, Longitude_Top_Right)
    verif_latitude(data, Latitude_Top_Right)
    verif_longitude(data, Longitude_Bottom_Left)
    verif_latitude(data, Latitude_Bottom_Left)
    verif_longitude(data, Longitude_Bottom_Right)
    verif_latitude(data, Latitude_Bottom_Right)
    verif_info_image(data, Image_Focus)
    verif_info_image(data, Image_Contrast)
    verif_info_image(data, Glare)
    verif_info_image(data, Density_White_Caps)
    verif_info_image(data, Cloud_Cover)
    
  }
  
  
  #### verif_info_image
  
  verif_info_image <- function(data, col) {
    
    namedata <- table_name(data)
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      ID <- column_ref(data)
      
      if(all(is.na(x) == TRUE)){cli_alert_success(glue('Only NA in column {deparse(substitute(col))}'))}
      else{
        if(all(all(x %in% c(NA, "NA", 0, 1, 2, 3)) & any(is.na(x)==TRUE))){cli_alert_success(glue('Column {deparse(substitute(col))} is correct with some NA'))} 
        if(all(all(x %in% c(NA, "NA", 0, 1, 2, 3)) & all(is.na(x)==FALSE))){cli_alert_success(glue('Column {deparse(substitute(col))} is correct with some NA'))} 
        if(all(all(x %in% c(NA, 0, 1, 2, 3)) & all(is.na(x)==FALSE))){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
        
        if(any(x %in% c(NA, "NA", 0, 1, 2, 3) == FALSE)){
          
          out <- data %>%
            filter(x %!in% c(NA, "NA", 0, 1, 2, 3)) %>%
            pull(ID)
          
          cli_alert_danger(glue_collapse('In column {deparse(substitute(col))}, values for {ID}: {out} do not correspond to NA or numerical categories'))}
        
      }
    }
    
  }
  
  
  ## Table Target ----
  
  #### verif_target
  
  
  verif_target <- function(data){
    
    
    # Verif by columns
    verif_column_ID(data, Target_ID)
    verif_column_semi_facultative(data, Object_ID)
    verif_column_NA(data, Image_ID)
    verif_column_NA(data, Side)
    verif_target_confidence(data)
    verif_column_num(data, Resolution)
    verif_longitude(data, Longitude_Target)
    verif_latitude(data, Latitude_Target)
    verif_dist(data)
    verif_column_facultative(data, Length_Target)
    
  }
  
  
  #### verif_target_confidence
  
  ```{r}
  verif_target_confidence <- function(data){
    
    namedata <- table_name(data)
    
    if(column_here(data, "Target_Confidence", namedata) == TRUE) {
      
      x <- data %>%
        pull(Target_Confidence)
      
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        out <- data %>%
          filter(is.na(Target_Confidence) == TRUE) %>%
          pull(Target_ID)
        
        cli_alert_danger(glue_collapse('NA in column Target_Confidence for Target_ID: {out}, please check'))} 
      
      
      
      if(any(x %!in% c(TRUE, FALSE))){cli_alert_danger(glue('Column Target_Confidence should contains only TRUE or FALSE values'))}
      
      
      x1 <- data %>%
        filter(is.na(Object_ID) == TRUE)
      
      if(any(unique(x1$Target_Confidence)==TRUE, na.rm = TRUE)){
        
        out <- data %>%
          filter(is.na(Object_ID) == TRUE) %>%
          filter(Target_Confidence == TRUE) %>%
          pull(Target_ID)
        
        cli_alert_danger(glue_collapse('For Target_ID: {out}, Target_Confidence is TRUE whereas Object_ID is NA, please check'))} 
      
      x2 <- data %>%
        filter(is.na(Object_ID) == FALSE)
      
      out <- data %>%
        filter(is.na(Object_ID) == FALSE) %>%
        filter(Target_Confidence == FALSE) %>%
        pull(Target_ID)
      
      if(any(unique(x2$Target_Confidence)==FALSE)){cli_alert_danger(glue_collapse('For Target_ID: {out}, Target_Confidence is FALSE whereas there is a Object_ID associated'))} 
      
      if(all(all(x %in% c(TRUE, FALSE)), all(is.na(x) == FALSE), all(unique(x1$Target_Confidence)==FALSE), all(unique(x2$Target_Confidence)==TRUE))){cli_alert_success(glue('Column Target_Confidence is correct'))}
      
      
    }
    
  }
  
  #### verif_dist
  
  verif_dist <- function(data, dist_max = 500) {
    
    namedata <- table_name(data)
    
    if(column_here(data, "Perpendicular_Distance", namedata) == TRUE) {
      
      x <- data %>%
        pull(Perpendicular_Distance)
      
      if(is.numeric(x) == FALSE){cli_alert_danger(glue('Column Perpendicular_Distance should be numeric'))}
      
      else{
        
        # NA
        if(all(is.na(x) == TRUE)){
          
          cli_alert_success(glue_collapse('Column Perpendicular_Distance contains only NA'))}
        
        else{
          
          if(any((x < dist_max & x >= 0) == FALSE, na.rm = TRUE)){
            
            out <- data %>%
              filter(Perpendicular_Distance > dist_max | Perpendicular_Distance < 0) %>%
              pull(Target_ID)
            
            cli_alert_danger(glue_collapse('For Target_ID: {out}, Perpendicular_Distance is not correct, please check'))} 
          
          if(all(all(is.na(x) == FALSE) & is.numeric(x) == TRUE & all((x < dist_max & x > 0), na.rm = TRUE))){cli_alert_success(glue('Column Perpendicular_Distance is correct'))}
        } 
      }
      
    }
  }
  
  
  ## Table Object ----
  
  #### verif_object
  
  verif_object <- function(data){
    
    
    # Verif by columns
    verif_column_ID(data, Object_ID)
    verif_species_code(data, Species_Code)
    verif_species_code(data, Species_Name)
    verif_info_object(data, Calf)
    verif_info_object(data, Flying)
    verif_column_semi_facultative(data, Group_ID)
  }
  
  
  
  #### verif_species_code
  
  verif_species_code <- function(data, col, Sp_Code = Sp_Code) {
    
    namedata <- table_name(data)
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        pull({{col}})
      
      # NA
      if(any(is.na(x) == TRUE)){
        
        out <- data %>%
          filter(is.na({{col}}) == TRUE) %>%
          pull(Object_ID)
        
        cli_alert_danger(glue_collapse('NA in column {deparse(substitute(col))} for Object_ID: {out}, please check'))} 
      
      if({deparse(substitute(col))} == "Species_Code"){
        
        if(any(x %in% Sp_Code$Species_Code == FALSE)){
          
          out <- data %>%
            filter({{col}} %!in% Sp_Code$Species_Code) %>%
            pull(Object_ID)
          
          cli_alert_danger(glue_collapse('Species_Code for Object_ID: {out} not correct, please check'))}
        
        if(all(all(is.na(x) == FALSE) & all(x %in% Sp_Code$Species_Code))){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
        
      }
      
      if({deparse(substitute(col))} == "Species_Name"){
        
        if(any(x %in% Sp_Code$Species_Name == FALSE)){
          
          out <- data %>%
            filter({{col}} %!in% Sp_Code$Species_Name) %>%
            pull(Object_ID)
          
          cli_alert_danger(glue_collapse('Species_Name for Object_ID: {out} not correct, please check'))}
        
        
        if(all(all(is.na(x) == FALSE) & all(x %in% Sp_Code$Species_Name))){cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))}
        
      }
      
    }
  }
  
  
  #### verif_info_object
  
  verif_info_object <- function(data, col) {
    
    namedata <- table_name(data)
    
    
    if(column_here(data, deparse(substitute(col)), namedata) == TRUE) {
      
      x <- data %>%
        filter(is.na({{col}})==FALSE) %>%
        pull({{col}})
      
      if(all(x %in% c(NA, "NA", "unknown","UNKNOWN", "TRUE", "FALSE"))){
        cli_alert_success(glue('Column {deparse(substitute(col))} is correct'))} else {
          
          out <- data %>%
            filter(is.na({{col}})==FALSE) %>%
            filter({{col}} %!in% c(NA, "NA","unknown","UNKNOWN", "TRUE", "FALSE")) %>%
            pull(Object_ID)
          
          cli_alert_danger(glue_collapse('For Object_ID: {out}, column {deparse(substitute(col))} must contains only NA or categories TRUE, FALSE or unknown, please check'))}
      
      if(all(all(is.na(x) == TRUE), all(x == "NA"))){cli_alert_success(glue('Only NA in column {deparse(substitute(col))}'))}
      
      
    }
  }
  
  
  ## Global verifications ----
  
  
  verif_digital_data <- function(transect_table, image_table, target_table, object_table) {
    
    cli_h1("Table Transect")
    cli_h2("Check columns")
    verif_transect(transect_table)
    
    cli_h2("Check consistency between tables")
    verif_nb_image(transect_table, image_table)
    
    
    cli_h1("Table Image")
    cli_h2("Check columns")
    verif_image(image_table)
    
    cli_h2("Check consistency between tables")
    verif_link(image_table, transect_table, Transect_ID)
    
    
    cli_h1("Table Target")
    cli_h2("Check columns")
    verif_target(target_table)
    
    cli_h2("Check consistency between tables")
    verif_link(target_table, image_table, Image_ID)
    
    
    cli_h1("Table Object")
    cli_h2("Check columns")
    verif_object(object_table)
    
    cli_h2("Check consistency between tables")
    verif_link(object_table, target_table, Object_ID)
    verif_link(target_table, object_table, Object_ID)
    
  }
  
  
  
  