#####################################################
# Set working directory as the folders with all the raw hysplit ouput.
# Import all raw output files from Hysplit

library(stringr)
library(data.table)
library(dplyr)

#import all files
# Depend on how you save your files.
# I put 2019* because all of my files starts with 2019
# You can choose 20* instead if your data starts with 20

split_rmws <- data.frame()


temp = list.files(pattern= c('PMFFF'))

myfiles = lapply(temp, read.delim)

# Extract files for post processing
for (i in 1: length(temp)) {
  data <- myfiles[i]
  #data <- unlist(data)
  data <- as.data.frame(data)
  n_row <- nrow(data)
  data <- as.data.frame(data[1:n_row,1])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  # Remove spaces
  data_rmws <- data.frame()
  # Remove ws for all rows
  for (k in 1:nrow(data)) {
    data_1 <- data[k,1]
    data_1_1 <- str_squish(data_1)
    data_rmws[k,1] <- data_1_1
  }
  
  # Text to Columns
  colnames(data_rmws) <- c("data")
  data_split <- strsplit(as.character(data_rmws$data),' ')
  split <- as.data.frame(do.call(rbind, str_split(data_rmws$data, " ")))
  split_1 <- data.frame(lapply(split, as.character), stringsAsFactors=FALSE)
  
  # Checck files
  if (ncol(split_1) != 13){ # no. of columns are 13
    print("next")
    next #skip file if not match column number
  }
  else {
    # Combine all files
    split_rmws <- rbind(split_rmws,split_1)
    print(i)
  }
}

# Change column names
colnames(split_rmws) <- c("height_cat","receptor_cat","year","month","day","hour",
                          "no_need","no_hour_back","hour.inc","lat","lon","height","pressure")
split_rmws_double <- as.data.frame(sapply(split_rmws, as.double))
split_rmws_double <- na.omit(split_rmws_double)
split_rmws_double <- split_rmws_double %>% filter(height_cat<=4) # Only 4 heights

# Calculate date&time of backtrajectory
split_rmws_double_mutate <- split_rmws_double %>%
  mutate(trajectory_datetime = paste(split_rmws_double$year, split_rmws_double$month,
                                     split_rmws_double$day, split_rmws_double$hour,
                                     sep="-"))
# Change format of datetime of trajectory
split_rmws_double_mutate$trajectory_datetime <- as.POSIXct(strptime(split_rmws_double_mutate$trajectory_datetime,
                                                                    "%y-%m-%d-%H"))
# Calculate datetime at receptor site
split_rmws_double_mutate <- split_rmws_double_mutate %>%
  mutate(receptor_datetime = split_rmws_double_mutate$trajectory_datetime - 
           split_rmws_double_mutate$hour.inc *60*60)

# Create another column to calculate 4-4pm - For XR's daily
split_rmws_double_mutate$receptor_datetime_daily <- split_rmws_double_mutate$receptor_datetime

#format(as.Date(split_rmws_double_mutate$receptor_datetime[2739]+1*60*60*24),"%F")

library(lubridate)

n_row_df <- nrow(split_rmws_double_mutate)

for (i in 1:n_row_df) {
  if (hour(split_rmws_double_mutate$receptor_datetime)[i] >= 16) {
    split_rmws_double_mutate$receptor_datetime_daily[i] = 
      format(as.Date(split_rmws_double_mutate$receptor_datetime[i]+1*60*60*24),"%F")
    
  }
  
  else if (hour(split_rmws_double_mutate$receptor_datetime)[i] == 0) {
    split_rmws_double_mutate$receptor_datetime_daily[i] = 
      format(as.Date(split_rmws_double_mutate$receptor_datetime[i]+2*60*60*24),"%F")
  }
  
  else {
    split_rmws_double_mutate$receptor_datetime_daily[i] = 
      format(as.Date(split_rmws_double_mutate$receptor_datetime[i]),"%F")
  }
  print(i)
}

# Categorize different altitude trajectories:
split_rmws_double_mutate$height_cat <- as.factor(split_rmws_double_mutate$height_cat)

#write a data frame
write.csv(split_rmws_double_mutate,'split_rws_double_mutate_old.csv')

split_rmws_double_mutate_old <- read.csv('split_rmws_double_mutate_old.csv')
#remove column x
split_rmws_double_mutate_old$X<-NULL

write.csv(split_rmws_double_mutate_17_20_v2,'split_rws_double_mutate_17_20_v2.csv')

split_rmws_double_mutate_17_20_v2<-read.csv('split_rmws_double_mutate_17_20_v2.csv')
#remove column x
split_rmws_double_mutate_17_20$X<-NULL

#merge dataframe
split_rmws_double_mutate_combine_11_20<-rbind(split_rmws_double_mutate,split_rmws_double_mutate_17_20_v2,split_rmws_double_mutate_old)


#split_rmws_double_mutate$

# If you want to look at individual altiude, split_rmws_double_mutate$height_cat == 1 2 3 or 4
# 1 - 67m
# 2 - 500m
# 3 - 1000m
# 4 - 1500m
split_rmws_double_mutate_1 <- split_rmws_double_mutate[split_rmws_double_mutate$height_cat==1,]

#####################################
# Import concentration files.
# If you are using this for each factor analysis, each column should be conc. of each factors
# If you are examining raw data, each column should be the species (same as how in arrange inputs for PMF)
# Put your inputs file in CSV format, change the file name below
library(readr)
concentration <- read.csv("lvgn inter for R.csv", stringsAsFactors = FALSE)

# Format datetime for your conentration file
# Fill in ___ the column name of the date format
# Fill in xxxxxxxxx the current format of your datetime
# For example: Run View(concentration) in Console
# See your datetime column. If it's saved as 19-12-30 0:00 -> fill in xxxxxxxxx as %y-%m-%d %H:%M
# Run ?strptime in the console for help of abbreviations to be used for each type of datetime
concentration$Date <- as.POSIXct(strptime(concentration$Date,"%d/%m/%Y"))


# Fill in ___ the column name of the date format
merged <- merge(x=concentration,y=split_rmws_double_mutate_combine,
                by.x = "Date",
                by.y = "receptor_datetime_daily")

library(openair)
library (mapproj)
library (mapdata)
#change column names
# change column names of date at receptor site to "date_daily"
# 
colnames(merged)[1] <- "date"
colnames(merged)[13] <- "species1" #lvgn
colnames(merged)[14] <- "species2" #1st gen
colnames(merged)[15] <- "species3" #2nd gen
colnames(merged)[16] <- "species4" #3rd gen
colnames(merged)[17] <- "species5" #all inter
colnames(merged)[18] <- "species6" #lvgn+1st gen
colnames(merged)[19] <- "species7" #lvgn+1st_2nd gen
colnames(merged)[20] <- "species8" #lvgn+all inter
colnames(merged)[21] <- "species9" #lvgn_eff

# change column names of trajectory date to date2
#colnames(merged)[ncol(merged)-1] <- "date2"
# change column names of receptor date to date
#colnames(merged)[ncol(merged)] <- "date"
#change column names of hour_back to "hour.inc"
#colnames(merged)[ncol(merged)-5] <- "hour.inc"
#Height cat 1 is 67m, 2 is 500, 3 is 1000, 4 is 1500.
#U can change to either -24, -48, -72, -96

# PSCF plots
# statistic = pscf determine the method you are calculating
# Run ?trajLevel in Console to see other statistics that we can do.
# For example: statistics = "cwt" for concentration weighted trajectories
# change cut-off percentile in percentile = ___

#only show major smoke event in 2011, 2012, 2013, 2015, 2019
smoke <- subset(merged,Class=="SD"|Year=='2011'|Year=='2012'|Year=='2013'|Year=='2015'|Year=='2019')

Sys.setlocale("LC_ALL", "English")


#########

#plot PSCF by month and altitude
month <- c(8,9,10)
year <- c(2015,2019)
heights <- c(67,500,1000,1500)
species <-seq(9)

for (s in species) {
  
  for (n in year) {
    
    for (m in month) {
      
      
      subset_smoke <- subset(smoke, hour.inc >= -96 & Year==n & Month==m)
      
      # General plot
      tiff(filename = paste("(Monthly PSCF All altitudes)_species",s,n,m,"-96h.tiff", sep = "_"),width = 6,  height = 4, units = "in", res = 300)
      
      trajLevel(subset_smoke,lon = "lon",lat = "lat",
                map = TRUE, map.fill = TRUE, map.res = "world",
                orientation = c(90,0,90),
                
                #Enter your factor column
                #pollutant= paste("Factor", f, sep = "."),
                pollutant= paste("species",s,sep=""),
                
                #to show singapore range
                xlim=c(90,130), ylim=c(5,-9), percentil=75, 
                
                #if you want to analyze by month or by year
                #type ='Year_month',
                
                origin = TRUE, grid.col='transparent',
                statistic="pscf", smooth=TRUE, col = "default",
                
                #title of the plot
                #main= paste0("Factor ", f, " (", heights[altitude], "m, ", h, "h",")")
                # main= paste0("Factor 1+5 ", "(", heights[altitude], "m, ", h, "h",")")
                main= paste0("(Monthly PSCF All altitudes)_species",s,"_",n,"_",m,"(-96h).tiff", sep = "_"),
                
                
      )
      dev.off()
    }
  }
}

#-------------------------------------------------------------------------
#plot CWT by month and altitude
month <- c(8,9,10)
year <- c(2015,2019)
heights <- c(67,500,1000,1500)
species <-seq(9)

for (s in species) {
  
  for (n in year) {
    
    for (m in month) {
      
      
      subset_smoke <- subset(smoke, hour.inc >= -96 & Year==n & Month==m)
      
      # General plot
      tiff(filename = paste("(Monthly CWT All altitudes)_species",s,n,m,"-96h.tiff", sep = "_"),width = 6,  height = 4, units = "in", res = 300)
      
      trajLevel(subset_smoke,lon = "lon",lat = "lat",
                map = TRUE, map.fill = TRUE, map.res = "world",
                orientation = c(90,0,90),
                
                #Enter your factor column
                #pollutant= paste("Factor", f, sep = "."),
                pollutant= paste("species",s,sep=""),
                
                #to show singapore range
                xlim=c(90,130), ylim=c(5,-9), #percentil=75, 
                
                #if you want to analyze by month or by year
                #type ='Year_month',
                
                origin = TRUE, grid.col='transparent',
                statistic="cwt", smooth=TRUE, col = "default",
                
                #title of the plot
                #main= paste0("Factor ", f, " (", heights[altitude], "m, ", h, "h",")")
                # main= paste0("Factor 1+5 ", "(", heights[altitude], "m, ", h, "h",")")
                main= paste0("(Monthly CWT All altitudes)_species",s,"_",n,"_",m,"(-96h).tiff", sep = "_"),
                
                
      )
      dev.off()
    }
  }
}
#----------------------------------------------------------------------


# PLOT MULTIPLES HOURS & ALTITUDE
heights <- c(67,500,1000,1500)
hours <- c(-24,-48,-72,-96)

factors <- seq(1,11)
#factors <- c(1.5)

for (f in factors) {
  for (altitude in 1:4) {
    for (h in hours) {
      
      subset_merged <- subset(merged, hour.inc >= h & height_cat == altitude)
      # General plot
      tiff(filename = paste("Factor ", f, "_", heights[altitude], "m_", h, "h2.tiff", sep = ""))
      
      trajLevel(subset_merged,lon = "lon",lat = "lat",
                map = TRUE, map.fill = TRUE, map.res = "world",
                orientation = c(90,0,90),
                
                #Enter your factor column
                #pollutant= paste("Factor", f, sep = "."),
                pollutant= DEHP,
                
                #to show singapore range
                xlim=c(80,130), ylim=c(7,-7), percentil=75, 
                
                #if you want to analyze by month or by year
                #type ='year',
                
                origin = TRUE, grid.col='transparent',
                statistic="pscf", smooth=FALSE, col = "increment",
                
                #title of the plot
                #main= paste0("Factor ", f, " (", heights[altitude], "m, ", h, "h",")")
                # main= paste0("Factor 1+5 ", "(", heights[altitude], "m, ", h, "h",")")
                main= paste0("DEHP", " (", heights[altitude], "m, ", h, "h",")")
                
      )
      dev.off()
    }
  }
}


heights <- c(67,500,1000,1500)
hours <- c(-24,-96)
factors <- seq(1,9)
#factors <- c(1.5)

# BY YEAR
for (f in factors) {
  for (altitude in 1:4) {
    for (h in hours) {
      
      subset_merged <- subset(merged, hour>= h & height_cat == altitude)    
      # Yearly plot
      tiff(filename = paste("Yearly_Factor ", f,  "_", heights[altitude], "m_", h, "h.tiff", sep = ""), units = "px",
           height = 539, width = 765)
      
      trajLevel(subset_merged,lon = "lon",lat = "lat",
                map = TRUE, map.fill = TRUE, map.res = "world",
                orientation = c(90,0,90),
                
                #Enter your factor column
                pollutant= paste("Factor", f, sep = "."),
                
                #to show singapore range
                xlim=c(90,130), ylim=c(7,-7), percentil=75, 
                
                #if you want to analyze by month or by year
                type ='year',
                
                origin = TRUE, grid.col='transparent',
                statistic="pscf", smooth=FALSE, col = "increment",
                
                #title of the plot
                main= paste0("Factor ", f, " (", heights[altitude], "m, ", h, "h",")")
                #main= paste0("Factor 1+5", " (", height[altitude], "m, ", h, "h",")")
      )
      dev.off()
    }
  }
}

#--------------------------------------------------
#Time series
install.packages("ggplot2")
install.packages("tidyr")
install.packages("cowplot")
install.packages("ggpubr")
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggpubr)


#convert data frame from wide to long
concentration_long <- gather(concentration, species, 
                             value,Lvgn:Third_gen, factor_key=TRUE)

#subset a data frame of only SD data during aug-oct 2019
concentration_long_SD <- subset (concentration_long, Class=="SD")
concentration_long_2019_8_9_10 <- subset(concentration_long_SD, Year_month=="2019_9"| Year_month=="2019_8"| Year_month=="2019_10")

#dotted line plot
p1<-ggplot(concentration_long_2019_8_9_10) +
  geom_line(aes(x=Date, y=value, color=species))+
  geom_point(aes(x=Date, y=value, color=species), size = 4, shape = 21, position = position_dodge(0.2))+ #dodging to prevent overlapping points
  #scale_y_continuous(sec.axis = sec_axis(~./max(concentration_long_2019_8_9_10$All.inter.lvgn),name="All inter./lvgn"))+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text( size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, angle = 90, hjust = .5, vjust = .5, face = "bold"),
        legend.position = c(0.15, 0.7),legend.text=element_text(size=16,face="plain"),legend.title=element_blank())+
  ylab(bquote(μg/m^3))

p2<- ggplot(concentration_long_2019_8_9_10) +
  geom_bar(aes(x=Date, y=All.inter.lvgn),stat="identity", colour="yellow")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text( size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(size = 18, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(size = 18, angle = 90, hjust = .5, vjust = .5, face = "bold"))+
  ylab("All intermediates/lvgn")

figure<-ggarrange(p1, p2, ncol = 1, nrow = 2,align="v")

annotate_figure(figure,
                top = text_grob("2019 SD Aug-Oct", color = "black", face = "bold", size = 20))

