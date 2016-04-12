#set working directory
setwd("D:/Oht/R/US_labor_data/layoff/")

# library
library(ggplot2)
library(reshape2)
library(reshape)
library(stringr)
library(data.table)

# read csv file
ml_data_t <- read.csv("mass_layoffs_cleanTranspCSV.csv", header = TRUE)
ml_data <- read.csv("mass_layoffs_cleanCSV.csv", header = TRUE)

# highest number of total layoffs across industries
# ... Plot_1 ...
#plot(ml_data_t$Year, ml_data_t$Total.all.industries, pch = 19, col = 'blue')
ggplot(ml_data_t)+
  aes(x = as.factor(Year), y = Total.all.industries)+
  geom_point(color = 'deeppink3', size = 5, alpha = 0.5)+
  xlab ("Year") +
  ylab ("Total layoffs")+
  ggtitle ("Industry layoffs - Years: 2003 - 2012")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, vjust=1)) 

# transform data frame from columns to rows for plotting using 'melt'
df <- melt(ml_data_t, id.vars='Year')


# Visualizing layoffs across all industries (years: 2003-2012)
# ... Plot_2 ...
ggplot(df, aes(x=as.factor(Year), y=value, fill=variable)) + geom_bar(stat='identity')+
  xlab ("Year") +
  ylab ("Total layoffs")+
  ggtitle ("Industry layoffs")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, vjust=1))+
  guides(fill=guide_legend(ncol=3))+
  theme(legend.text = element_text(size = 14))

#plot(ml_data$Industry.Type, ml_data$X2009, 
#     type = "o", pch = 19, las = 2, 
#     color = "red")

# filtering those that are above 10000
ml_data$Industry.Type <- as.character(ml_data$Industry.Type)
ml_data_high <- subset(ml_data, X2009 > 5000)
#ml_data_high <- subset(ml_data, X2009 > 2500 & X2009 < 15000)

ml_data_high$Industry.Type <- as.factor(ml_data_high$Industry.Type)
#par(mar = c(10,4,4,2))
#plot(ml_data_high$Industry.Type, ml_data_high$X2009, las = 2)

# ... Plot_3...
ggplot(ml_data_high)+
  aes(x = Industry.Type, y = X2009)+
  geom_point(color = 'deeppink3', size = 5, alpha = 0.5)+
  xlab ("Type of industry") +
  ylab ("Total layoffs")+
  ggtitle ("Highest layoffs - Years: 2003 - 2012")+
  theme(text = element_text(size=18)) 
#
#
#
#
## ------------Part TWO ..............
# further analysis of 'high layoff' segments in highest year 2009
#
#
industry_code <- read.csv("Industry Reason Demographic CodesCSV.csv")
state_code <- read.csv("States Regions Divisions CodesCSV.csv")

m2_data <- read.csv("mass_layoffs_LEVEL2_cleanCSV.csv", header = TRUE)

#function 'readSeriesID() to convert series id into vector....
readSeriesID <- function (seriesid){
  temp <- 0
  count <- 0
  for (i in 1:(7+1)){
    if(count == 0){
      start <- 1
      stop <- 2
    }
    else if (count ==2){
      start <- 3
      stop <- 3
    }
    else if (count == 3){
      start <- 4
      stop <- 4
    }
    else if (count == 4){
      start <- 5
      stop <- 7
    }
    else if (count == 5){
      start <- 8
      stop <- 8
    }
    else if (count == 6){
      start <- 9
      stop <- 13
    }
    else if (count == 7){
      start <- 14
      stop <- 16
    }
    temp <- c(temp, substr(seriesid, start,stop))
    count <- count + 1
  }
  return (temp[2:8])
}
#..........


seriesid <- readSeriesID("MLUMS00NN0057003")
seriesid
##> seriesid
##[1] "ML"    "ML"    "U"     "M"     "S00"   "N"     "N0057"
#
# function 'to convert series id vector into columns
# States, Industry/Reason/Demographics
indCol <- function(dFrame1, dFrame2, col1, svec, col_num){
  dFrame1[,col1] <- as.character(dFrame1[,col1]) #<<< note: using "$" did not work so used "[]"
  dFrame2[,col1] <- as.character(dFrame2[,col1])
  num1 <- grep(svec[5], dFrame1[,col1]) #use grep to the number location
  num2 <- grep(svec[7], dFrame2[,col1])
  t1 <- as.character(dFrame1[num1, col_num]) # convert to 'character'
  t2 <- as.character(dFrame2[num2, col_num])
  t <- list(t1, t2)
  return (t)
}
re <- indCol(state_code, industry_code, "Code", seriesid, 2)
re
for (i in 1:nrow(m2_data)){
  seriesid <- readSeriesID(as.character(m2_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  m2_data$State[[i]] <- re[[1]]
  m2_data$Industry[[i]] <- re[[2]]
}
m2_data$X2009 <- as.integer(m2_data$X2009)
m2_data$State <- as.factor(m2_data$State)
m2_data$Industry <- as.factor(m2_data$Industry)

#par(mar = c(15,4,4,2))
#plot(m2_data$Industry, m2_data$X2009, pch = 19, col = 'blue', las = 2)
m2_data_manufacturing <- m2_data[3:23,]
# ... Plot_4...
ggplot(m2_data_manufacturing)+
  aes(x = Industry, y = X2009)+
  geom_point(color = 'deeppink3', size = 5, alpha = 0.5)+
  xlab ("Type of industry") +
  ylab ("Layoffs")+
  ggtitle ("Manufacturing layoffs in the year 2009")+
  theme(text = element_text(size=20),
  axis.text.x = element_text(angle=90, vjust=1, hjust = 1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#
#
#
#
# ...Year 2009: Manufacturing, Constructon, Admin Services


m3_data <- read.csv("mass_layoffs_LEVEL3CSV.csv", header = TRUE)
for (i in 1:nrow(m3_data)){
  seriesid <- readSeriesID(as.character(m3_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  m3_data$State[[i]] <- re[[1]]
  m3_data$Industry[[i]] <- re[[2]]
}


for (i in 1:nrow(m3_data)){
  m3_data_short <- m3_data[grep("", m3_data$X2009),] #removing all 'NA' rows
}

m3_data_short$X2009 <- as.integer(m3_data_short$X2009)
m3_data_short$State <- as.factor(m3_data_short$State)
m3_data_short$Industry <- as.factor(m3_data_short$Industry)
#par(mar = c(10,4,4,2))
#plot(m3_data_short$Industry, m3_data_short$X2009, pch = 19, las = 2)

# ... Plot_5...
ggplot(m3_data_short)+
  aes(x = Industry, y = X2009)+
  geom_point(color = 'deeppink3', size = 5, alpha = 0.5)+
  xlab ("Type of industry") +
  ylab ("Layoffs")+
  ggtitle ("Layoff industry: Manufacturing, Constructon, Admin Services")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


# ... Plot_6...
ggplot(m3_data_short)+
  aes(x = Industry, y = X2009, fill = State)+
  geom_bar(stat="identity")+
  xlab ("Type of industry") +
  ylab ("Number of layoffs")+
  ggtitle ("High layoffs across states")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


# ... Plot_7...
ggplot(m3_data_short)+
  aes(x = State, y = X2009, fill = Industry)+
  geom_bar(stat="identity")+
  xlab ("Type of industry") +
  ylab ("Number of layoffs")+
  ggtitle ("High layoffs across states and industries")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1, hjust = 1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#q1 <- ggplot(m3_data_short, aes(x = State, y = X2009, fill = Industry))+
#  geom_bar(stat="identity")+
#  ggtitle("State Layoff Industry")+
#  ylab("Total Count")
#q1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))



#.....------------------------------------------------------------------
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#
# New data set of Openings, hire and separations
#
#
# ............ LEVEL 1 ...............
to1_data <- read.csv("TO_opn_hire_sepCSV.csv", header = TRUE)
to_ind_code <- read.csv("TO_IndustryCSV.csv", header = TRUE)
to_ind_code$Code <- as.character(to_ind_code$Code)
to_ind_code[1,1] <- "000000"
to_hire_code <- read.csv("TO_Data_Element_CodesCSV.csv", header = TRUE)

to_region_code <- read.csv("TO_Region_codeCSV.csv", header = TRUE)

#function 'TO_readSeriesID() to convert series id into vector....
TO_readSeriesID <- function (seriesid){
  temp <- 0
  count <- 0
  for (i in 1:(7+1)){
    if(count == 0){
      start <- 1
      stop <- 2
    }
    else if (count ==2){
      start <- 3
      stop <- 3
    }
    else if (count == 3){
      start <- 4
      stop <- 9
    }
    else if (count == 4){
      start <- 10
      stop <- 11
    }
    else if (count == 5){
      start <- 12
      stop <- 13
    }
    else if (count == 6){
      start <- 14
      stop <- 14
    }
    temp <- c(temp, substr(seriesid, start,stop))
    count <- count + 1
  }
  return (temp[3:8])
}
#..........
#[1] "JT"     "U"      "110099" "00"     "HI"     "R" 
# States, Industry/Reason/Demographics
TO_indCol <- function(dFrame1, dFrame2, dFrame3, col1, svec, col_num){
  dFrame1[,col1] <- as.character(dFrame1[,col1]) #<<< note: using "$" did not work so used "[]"
  dFrame2[,col1] <- as.character(dFrame2[,col1])
  dFrame3[,col1] <- as.character(dFrame3[,col1])
  num1 <- grep(svec[3], dFrame1[,col1]) #Industry code
  num2 <- grep(svec[4], dFrame2[,col1]) #Region code
  num3 <- grep(svec[5], dFrame3[,col1]) #Hire code
  t1 <- as.character(dFrame1[num1, col_num]) # convert to 'character'
  t2 <- as.character(dFrame2[num2, col_num])
  t3 <- as.character(dFrame3[num3, col_num])
  t <- list(t1, t2, t3)
  return (t)
}

seriesid <- TO_readSeriesID("JTU11009900HIR")
re <- TO_indCol(to_ind_code, to_region_code, to_hire_code, "Code", seriesid, 2)
re

for (i in 1:nrow(to1_data)){
  seriesid <- TO_readSeriesID(as.character(to1_data$Series_ID[i]))
  re <- TO_indCol(to_ind_code, to_region_code, to_hire_code, "Code", seriesid, 2)
  to1_data$Industry[[i]] <- as.character(re[[1]])
  to1_data$Region[[i]] <- as.character(re[[2]])
  to1_data$Hires[[i]] <- as.character(re[[3]])
}

to1_data$X2009 <- as.integer(to1_data$X2009)
#to1_data$State <- as.factor(to1_data$State)
to1_data$Region <- as.factor(to1_data$Region)
to1_data$Industry <- as.factor(to1_data$Industry)
#


#par(mar = c(10,4,4,2))
#plot(to1_data$Region, to1_data$X2009, pch = 19, las = 2)
#

# .... convert data.frame columns to rows i.e. transpose ...
to1_data_t <- as.data.frame(t(to1_data))
str(to1_data_t)

#
# ... IMPORTANT the conversions makes "lists"
# Convert lists to characters before using the data
row.names(to1_data_t) <- NULL # clear the rownames
# ... IMPORTANT !!! ... convert columns to 'character'
to1_data_t$V1 <- as.character(to1_data_t$V1) # convert column to character
to1_data_t$V2 <- as.character(to1_data_t$V2) # convert column to character
to1_data_t$V3 <- as.character(to1_data_t$V3) # convert column to character

#
colnames(to1_data_t) <- to1_data_t[14, ] # convert row to column name
to1_data_t <- to1_data_t[-1, ]  # remove 1st row
to1_data_t <- to1_data_t[-c(11, 12, 13), ] # remove rows not needed
yr <- c(2006:2015) # create a new vector with year
to1_data_t <- cbind(Year = yr, to1_data_t) # add the year vector to data.frame
str(to1_data_t) #check the data.frame

#
to1_data_t[,1] <- as.numeric(to1_data_t[,1]) # convert column to numeric
to1_data_t[,2] <- as.numeric(to1_data_t[,2]) # convert column to numeric
to1_data_t[,3] <- as.numeric(to1_data_t[,3]) # convert column to numeric
to1_data_t[,4] <- as.numeric(to1_data_t[,4]) # convert column to numeric
str(to1_data_t)

#
df2 <- melt(to1_data_t, id.vars='Year') #melt data.frame for plotting mutliple variables

# ... Plot_8...
ggplot(df2)+
  aes(x = as.factor(Year), y = value, fill = variable)+
  geom_bar(stat="identity")+
  xlab ("Year") +
  ylab ("Turnover")+
  ggtitle ("Total turnover rate across all industries in US")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1, hjust = 1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Turnover type"))



#/////////////////////////// next level /////////////////////
#
#
to2_data <- read.csv("To_op_hire_sep_L2CSV.csv")

for (i in 1:nrow(to2_data)){
  seriesid <- TO_readSeriesID(as.character(to2_data$Series_ID[i]))
  re2 <- TO_indCol(to_ind_code, to_region_code, to_hire_code, "Code", seriesid, 2)
  to2_data$Industry[[i]] <- as.character(re2[[1]])
  to2_data$Region[[i]] <- as.character(re2[[2]])
  to2_data$Hires[[i]] <- as.character(re2[[3]])
}

to2_data$Hires <- as.factor(to2_data$Hires) # convert column to factor
to2_data$Industry <- as.factor(to2_data$Industry) # convert column to factor
to2_data$Region <- as.factor(to2_data$Region) # convert column to factor

to2_data_industry <- data.frame(to2_data[,2:12])
to2_data_hires <- data.frame(to2_data[,2:11], to2_data[ ,14])             

to2_data_t <- as.data.frame(t(to2_data))
colnames(to2_data_hires)[11] <- "Hires"

to2_data_industry_t <- as.data.frame(t(to2_data_industry))
row.names(to2_data_industry_t) <- NULL
str(to2_data_industry_t)
for (i in 1:ncol(to2_data_industry_t)){
  to2_data_industry_t[,i] <- as.character(to2_data_industry_t[,i]) # convert column to character
}

colnames(to2_data_industry_t) <- to2_data_industry_t[11, ]
to2_data_industry_t <- to2_data_industry_t[-c(11), ]
for (i in 1:ncol(to2_data_industry_t)){
  to2_data_industry_t[,i] <- as.numeric(to2_data_industry_t[,i]) # convert column to numeric
}
to2_data_industry_t$Year <- c(2006:2015)
str(to2_data_industry_t)

df3 <- melt(to2_data_industry_t, id.vars='Year')

#q3 <- ggplot(df3, aes(x = Year, y = value, fill = variable))+
#  geom_bar(stat="identity")+
#  ggtitle("State Layoff Industry")+
#  ylab("Total Count")
#q3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ... Plot_9...
ggplot(df3)+
  aes(x = as.factor(Year), y = value, fill = variable)+
  geom_bar(stat="identity")+
  xlab ("Year") +
  ylab ("Turnover")+
  ggtitle ("Turnover rate across industries in US")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1, hjust = 1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Turnover type"))
#........Hires.

to2_data_hires_t <- as.data.frame(t(to2_data_hires))
row.names(to2_data_hires_t) <- NULL
str(to2_data_hires_t)
for (i in 1:ncol(to2_data_hires_t)){
  to2_data_hires_t[,i] <- as.character(to2_data_hires_t[,i]) # convert column to character
}

colnames(to2_data_hires_t) <- to2_data_hires_t[11, ]
to2_data_hires_t <- to2_data_hires_t[-c(11), ]
for (i in 1:ncol(to2_data_hires_t)){
  to2_data_hires_t[,i] <- as.numeric(to2_data_hires_t[,i]) # convert column to numeric
}
to2_data_hires_t$Year <- c(2006:2015)


df4 <- melt(to2_data_hires_t, id.vars='Year')

df3 <- melt(to2_data_industry_t, id.vars='Year')

#plot_bar(df4, fill="Variable")
#plot(factor(variable), data=df4, geom="bar", fill=factor(v))

#q4 <- ggplot(df4, aes(x = Year, y = ccc, fill = variable))+
#  geom_bar(stat="identity")+
#  ggtitle("State Layoff Industry")+
#  ylab("Total Count")

#q4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#q4 + coord_flip()

ccc <- c(df3$value)
df3$variable <- as.character(df3$variable)
cccv <- as.factor(c(df3[ ,2]))


aaa <- c(df4[ , 3])
aaay <- c(df4[ , 1])

df4[,2] <- as.character(df4[ ,2])
aaav <- c(df4[ ,2])
aaav <- as.factor(aaav)

df4$ccc <- cbind(ccc)
df4$cccv <- cbind(as.character(cccv))

aa  <- data.frame(aaay, aaav, aaa)
aa[,2] <- as.factor(aa[ , 2])

# sort data frame 'aa' according to 'variable' for stacked bar plot
aa <- aa[order(aa$aaav), ]

#...Plot_10...
ggplot(aa)+
  aes(x = as.factor(aaay), y = aaa, fill = aaav)+
  geom_bar(stat="identity")+
  xlab ("Year") +
  ylab ("Turnover")+
  ggtitle ("Employee turnover")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+





#..........
# function to create data frames for each year
mix <- function(year_str, df_name, col_idx){
  Hires <- to2_data$Hires
  Industry <- to2_data$Industry
  Year <- year_str
  new_col = paste("X", Year, sep = "")
  Value <- df_name[ , col_idx]
  new_col <- data.frame(Year, Value, Hires, Industry)
  return (new_col)
}
year_str <- c(2006:2015)
for (i in 2:11){
  ttt <- rbind(mix(as.character(year_str[i-1]), to2_data, i))
  abc <- rbind(abc,ttt)
}

# ...Plot_11...
ggplot(abc)+
  aes(Hires, Value, fill = Year)+
  geom_bar(stat="identity")+
  xlab ("Type of turnover") +
  ylab ("Turnover")+
  ggtitle ("Turnover in industry (2006 - 2015)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


#.........................................

# ////////////////////////////////// Level 3 ////////////////////////////
#
#
ml3_data <- read.csv("mass_layoffs_LEVEL3CSV.csv")
ml3_data <- subset(ml3_data, ml3_data$X2009 != "NULL")
#ml3_data <- ml3_data[grep(-v 'NULL', ml3_data$X2009), ]

for (i in 1:nrow(ml3_data)){
  seriesid <- readSeriesID(as.character(ml3_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  ml3_data$State[[i]] <- re[[1]]
  ml3_data$Industry[[i]] <- re[[2]]
}
ml3_data$X2009 <- as.integer(ml3_data$X2009)
ml3_data$State <- as.factor(ml3_data$State)
ml3_data$Industry <- as.factor(ml3_data$Industry)
#
# ...Plot_11...
ggplot(ml3_data)+
  aes(Industry, X2009, fill = State)+
  geom_bar(stat="identity")+
  xlab ("Turnover") +
  ylab ("Industry")+
  ggtitle ("Turnover in the year 2009")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
#.........................................

# ////////////////////////////////// Level 4 ////////////////////////////
#
# .. reasons for layoffs ...

r_data <- read.csv("Reason_layoff_USCSV.csv")

for (i in 1:nrow(r_data)){
  seriesid <- readSeriesID(as.character(r_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  r_data$State[[i]] <- re[[1]]
  r_data$Industry[[i]] <- re[[2]]
}
r_data$State <- as.factor(r_data$State)
r_data$Industry <- as.factor(r_data$Industry)


mix2 <- function(year_str, df_name, col_idx){
  Industry <- r_data$Industry
  Year <- year_str
  new_col = paste("X", Year, sep = "")
  Value <- df_name[ , col_idx]
  new_col <- data.frame(Year, Value, Industry)
  return (new_col)
}
year_str <- c(2003:2012)
r_combo <- data.frame()
for (i in 2:11){
  rt <- rbind(mix2(as.character(year_str[i-1]), r_data, i))
  r_combo <- rbind(r_combo,rt)
}

# ... Plot_12...
ggplot(r_combo)+
  aes(Year, Value, fill = Industry)+
  geom_bar(stat="identity", position = "dodge")+facet_wrap(~Industry) +
  xlab ("Year") +
  ylab ("Layoffs")+
  ggtitle ("Reason for layoffs (2003 - 2012)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme(legend.title=element_blank())
#.........................


r2_data <- read.csv("Reason_year_ind2CSV.csv")
r2_data[is.na(r2_data)] <- 0 # replace 'NA' in data frame with 'zeros

for (i in 1:nrow(r2_data)){
  seriesid <- readSeriesID(as.character(r2_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  r2_data$State[[i]] <- re[[1]]
  r2_data$Industry[[i]] <- re[[2]]
}
r2_data$State <- as.factor(r2_data$State)
r2_data$Industry <- as.factor(r2_data$Industry)


mix2 <- function(year_str, df_name, col_idx){
  Industry <- r2_data$Industry
  Year <- year_str
  new_col = paste("X", Year, sep = "")
  Value <- df_name[ , col_idx]
  new_col <- data.frame(Year, Value, Industry)
  return (new_col)
}


year_str <- c(2003:2012)
r2 <- data.frame()
for (i in 2:11){
  r2t <- rbind(mix2(as.character(year_str[i-1]), r2_data, i))
  r2 <- rbind(r2,r2t)
}

r2_plot <- ggplot(r2, aes(Year, Value, fill = Industry)) + 
  geom_bar(stat="identity", position = "dodge")+ facet_wrap(~Industry) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

r2_plot 
# ... Plot_13...
ggplot(r2)+
  aes(Year, Value, fill = Industry)+
  geom_bar(stat="identity", position = "dodge")+facet_wrap(~Industry) +
  xlab ("Year") +
  ylab ("Layoffs")+
  ggtitle ("Reasons for layoffs (2003 - 2012)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme(legend.title=element_blank())
#
#
#
#........................
r4_data <- read.csv("Reason_stateCSV.csv")
r4_data[is.na(r4_data)] <- 0

for (i in 1:nrow(r4_data)){
  seriesid <- readSeriesID(as.character(r4_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  r4_data$State[[i]] <- re[[1]]
  r4_data$Reason[[i]] <- re[[2]]
}
r4_data$State <- as.factor(r4_data$State)
r4_data$Reason <- as.factor(r4_data$Reason)


mix4 <- function(year_str, df_name, col_idx){
  State <- r4_data$State
  Reason <- r4_data$Reason
  Year <- year_str
  new_col = paste("X", Year, sep = "")
  Value <- df_name[ , col_idx]
  new_col <- data.frame(Year, Value, State, Reason)
  return (new_col)
}

year_str <- c(2003:2012)
r4 <- data.frame()
for (i in 2:11){
  r4t <- rbind(mix4(as.character(year_str[i-1]), r4_data, i))
  r4 <- rbind(r4,r4t)
}
# ... Plot_14...
ggplot(r4)+
  aes(Year, Value, fill = Reason)+
  geom_bar(stat="identity", position = "dodge") +facet_wrap(~State) +
  xlab ("Year") +
  ylab ("Layoffs")+
  ggtitle ("Statewise reasons for layoffs (2003 - 2012)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme(legend.title=element_blank())
#
#........
#
#

r5_data <- read.csv("Reason_state_sepCSV.csv")
r5_data[is.na(r5_data)] <- 0

for (i in 1:nrow(r5_data)){
  seriesid <- readSeriesID(as.character(r5_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  r5_data$State[[i]] <- re[[1]]
  r5_data$Reason[[i]] <- re[[2]]
}
r5_data$State <- as.factor(r5_data$State)
r5_data$Reason <- as.factor(r5_data$Reason)

year_str <- c(2003:2012)
r5 <- data.frame()
for (i in 2:11){
  r5t <- rbind(mix4(as.character(year_str[i-1]), r5_data, i))
  r5 <- rbind(r5,r5t)
}

#...Plot_15...
ggplot(r5)+
  aes(Year, Value, fill = Reason)+
  geom_bar(stat="identity", position = "dodge") +facet_wrap(~State) +
  xlab ("Year") +
  ylab ("Layoffs")+
  ggtitle ("Statewise reasons for layoffs (2003 - 2012)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme(legend.title=element_blank())
#
#........
#
#

demo_data <- read.csv("Reason_demoCSV.csv")
demo_data[is.na(demo_data)] <- 0

for (i in 1:nrow(demo_data)){
  seriesid <- readSeriesID(as.character(demo_data$Series_ID[i]))
  re <- indCol(state_code, industry_code, "Code", seriesid, 2)
  demo_data$State[[i]] <- re[[1]]
  demo_data$Demographic[[i]] <- re[[2]]
}
demo_data$State <- as.factor(demo_data$State)
demo_data$Demographic <- as.factor(demo_data$Demographic)

mix5 <- function(year_str, df_name, col_idx){
  State <- demo_data$State
  Demographic <- demo_data$Demographic
  Year <- year_str
  new_col = paste("X", Year, sep = "")
  Value <- df_name[ , col_idx]
  new_col <- data.frame(Year, Value, State, Demographic)
  return (new_col)
}

year_str <- c(1996:2003)
dd <- data.frame()
for (i in 2:9){
  demot <- rbind(mix5(as.character(year_str[i-1]), demo_data, i))
  dd <- rbind(dd,demot)
}

demo_plot <- ggplot(dd, aes(Year, Value, fill = Demographic)) + 
  geom_bar(stat="identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

demo_plot 

#...Plot_16...
ggplot(dd)+
  aes(Year, Value, fill = Demographic)+
  geom_bar(stat="identity", position = "dodge")+
  xlab ("Year") +
  ylab ("Layoffs")+
  ggtitle ("Demographics for layoffs (1998 - 2003)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, vjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme(legend.title=element_blank())
#