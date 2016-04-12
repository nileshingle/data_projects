setwd("D:/Oht/R/Work Market")

# Laod raw data
jobs <- read.csv("jobs2.csv", header = TRUE)

str(jobs)
library(ggplot2)
library(stringr)
library(MASS)
library(plyr)
#library(thePackage) #not available
table(jobs$Agency)

jobs$Agency <- as.character(jobs$Agency)
jobs$Salary.Range.From <- as.integer(jobs$Salary.Range.From)
jobs$Salary.Range.To <- as.integer(jobs$Salary.Range.From)

jobs$agency.abr <- abbreviate(jobs$Agency)



x <- c(jobs$agency.abr)
y <- table(x)
xy <- data.frame(x,y)
#barplot(y, las = 2, ylab = "Number of vacancies")
# ... Plot_1...
ggplot(jobs)+
  aes(x = agency.abr, y= Number.Of.Positions, fill = Civil.Service.Title)+
  geom_bar(stat="identity")+
  xlab ("Agency") +
  ylab ("Number of vacancies")+
  ggtitle ("Total number of vacancies")+
  theme(axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=4))


jobs$Agency <- as.factor(jobs$Agency)

# Salary

salary <- table(jobs$agency.abr, jobs$Salary.Range.From, jobs$Salary.Range.To)

plot(Salary.Range.From~factor(agency.abr), jobs, las = 2)
# ... Plot_2...
ggplot(jobs)+
  aes(x = as.factor(agency.abr), y= Salary.Range.From, fill = Civil.Service.Title)+
  geom_bar(stat="identity")+
  xlab ("Agency") +
  ylab ("Salary From (lower limit)")+
  ggtitle ("Salary with agencies")+
  theme(axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=4))


#plot(Salary.Range.To~factor(agency.abr), jobs, las = 2)

# find low and high salary
salhigh <- max(jobs$Salary.Range.To, na.rm = TRUE)
print(salhigh)
sallow <-  min(jobs$Salary.Range.From, na.rm = TRUE)
print(sall)

# function to convert annual and monthly salary to hourly salary
salconvert <- function(sal, salfreq){
  salfreq <- as.character(salfreq)
  if (salfreq == "Annual"){
    #print(Salary.Range.From/(365*8))
    return (sal/(365*8))  #convert annual pay to hourly rate 8 h/day
  }else if (salfreq == "Monthly"){
    return (sal/(30*8))
  }else if (salfreq == "Hourly"){
    return (sal)
  } else{
    return ("NULL")
  }
}

jobs$hr.sal.fr <- "NULL"
jobs$hr.sal.to <- "NULL"
for (i in 1:nrow(jobs)){
  jobs[i,"hr.sal.fr"] <- as.integer(salconvert(jobs[i, "Salary.Range.From"], jobs[i, "Salary.Frequency"]))
  jobs[i,"hr.sal.to"] <- as.integer(salconvert(jobs[i, "Salary.Range.To"], jobs[i, "Salary.Frequency"]))
}

# Find lowest salary in column ---------------------------
findlow <- function(columnname, saltype){
  temp = 0
  count = 0
  #print(columnname)
  for (i in 1:nrow(jobs)){
    if (columnname < temp & saltype == "low"){
      temp <- jobs[i, columnname]
      count <- i
    } else if (columnname < temp & saltype == "high"){
      temp <- jobs[i, columnname]
      count <- i
    } 
  }
  return (jobs[count, "Agency"])
}

lowestsaldept <- "NULL"
highestsaldept <- "NULL"
lowestsaldept <- findlow("hr.sal.fr", "low")
highestsaldept <- findlow("hr.sal.to", "high")
print(lowestsaldept)
print(highestsaldept)

jobs$hr.sal.fr <- as.integer(jobs$hr.sal.fr)
jobs$hr.sal.to <- as.integer(jobs$hr.sal.to)

#plot(hr.sal.fr~factor(agency.abr), jobs, las = 2)
#plot(hr.sal.to~factor(agency.abr), jobs, las = 2)

#xm <- matrix(jobs$Salary.Range, jobs$Salary.Range.To)
#jobs$avg.salary <- rowMeans(xm, na.rm = TRUE)
jobs$Posting.Type <- as.character(jobs$Posting.Type)
# ... Plot_3...
ggplot(jobs)+
  aes(x = Level, fill = Posting.Type)+
  geom_bar(width=0.5)+
  facet_wrap(~agency.abr)+
  xlab ("Level") +
  ylab ("Total Count")+
  ggtitle ("Type of posting in agencies and levles")+
  theme(axis.text.x = element_text(size = 6, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 10, face ="bold"),
        legend.position="bottom",
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=4))



checkx <- function(valu){
  if (valu < 20){
    return (valu)
  } else{
    return ("NULL")
  }
}

# ... Plot_4...
ggplot(jobs)+
  aes(x=(hr.sal.fr < 20), fill = Level)+
  geom_bar()+
  facet_wrap(~agency.abr)+
  xlab ("FALSE: (salary < 20), TRUE: (salary >= 20)") +
  ylab ("Total Count")+
  ggtitle ("Agencies with salaries less or greater than $20/hour")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  
  
# Salary below 10 ---------------------------
sal.blo.10 <- data.frame()
sal.blo.10 <- jobs[which(jobs$hr.sal.to <= 10) , ]


# ... Plot_5...
ggplot(sal.blo.10)+
  aes(x=hr.sal.to, fill = Level)+
    geom_bar()+
    facet_wrap(~agency.abr + ~Civil.Service.Title)+
    xlab ("Salary (USD/hour)") +
    ylab ("Total Count")+
    ggtitle ("Agencies with salaries below $10/hour")+
    theme(title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
          axis.title=element_text(size=14,face="bold"),
          legend.text = element_text(size = 12, face ="bold"),
          plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  
  
  
# Salary above 25 ---------------------------
sal.abv.25 <- data.frame()
sal.abv.25 <- jobs[which(jobs$hr.sal.to >= 25) , ]

# ... Plot_6...
ggplot(sal.abv.25)+
    aes(x=hr.sal.to, fill = Level)+
    geom_bar()+
    facet_wrap(~agency.abr + ~Civil.Service.Title)+
    xlab ("Salary (USD/hour)") +
    ylab ("Total Count")+
    ggtitle ("Agencies with salaries above $25/hour")+
    theme(title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
          axis.title=element_text(size=14,face="bold"),
          legend.text = element_text(size = 12, face ="bold"),
          plot.margin=unit(c(1,1,1.5,1.2),"cm")) 


# Salary above 60 ---------------------------
sal.abv.60 <- data.frame()
sal.abv.60 <- jobs[which(jobs$hr.sal.to >= 60) , ]

# ... Plot_7...
ggplot(sal.abv.60)+
  aes(x=hr.sal.to, fill = Level)+
  geom_bar(width = 0.5)+
  facet_wrap(~agency.abr + ~Civil.Service.Title)+
  xlab ("Salary (USD/hour)") +
  ylab ("Total Count")+
  ggtitle ("Agencies with salaries above $60/hour")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) 

  
# convert dates to characters ---------------------------
jobs$Posting.Date <- as.character(jobs$Posting.Date)
jobs$Posting.Until <- as.character(jobs$Posting.Until)
jobs$Posting.Updated <- as.character(jobs$Posting.Updated)
jobs$Process.Date <- as.character(jobs$Process.Date)


# use as.Date( ) to convert strings to dates ---------------------------
# cs <- as.character(jobs$Posting.Date)
# tp = gsub('.{5}$', '', cs)
# td <- as.Date(tp, format = "%m/%d/%y")


Posting.Date <- as.Date((gsub('.{5}$', '',jobs$Posting.Date)), format = "%m/%d/%Y")
Posting.Until <- as.Date((gsub('.{5}$', '',jobs$Posting.Until)), format = "%m/%d/%Y")
Posting.Updated <- as.Date((gsub('.{5}$', '',jobs$Posting.Updated)), format = "%m/%d/%Y")
Process.Date <- as.Date((gsub('.{5}$', '',jobs$Process.Date)), format = "%m/%d/%Y")

# saving absolute values
jobs$post.to.updated <- abs(difftime(Posting.Updated,Posting.Date, units = c("days")))
jobs$post.to.process <- abs(difftime(Process.Date,Posting.Date, units = c("days")))

# convert days from 'difftime' to 'integer
jobs$post.to.updated <- as.integer(jobs$post.to.updated)
jobs$post.to.process <- as.integer(jobs$post.to.process)

jobs_sorted <- jobs[order(jobs$Level), ]
# plot days...................
#plot(post.to.process~factor(agency.abr), jobs, las = 2)
# ... Plot_8...
ggplot(jobs_sorted)+
  aes(x=as.factor(agency.abr), y = post.to.process, fill = sort(Level))+
  geom_point(size = 5, alpha = 0.5,aes(color=Level))+
  #facet_wrap(~agency.abr)+
  xlab ("Agency") +
  ylab ("Number of days to process")+
  ggtitle ("Days from post to process a job")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Level"))
       


plot(post.to.updated~factor(agency.abr), jobs, las = 2)
# ... Plot_9...
ggplot(jobs_sorted)+
  aes(x=as.factor(agency.abr), y = post.to.updated)+
  geom_point(size = 5, alpha = 0.5,aes(color=Level))+
  #facet_wrap(~agency.abr)+
  xlab ("Agency") +
  ylab ("Number of days to process")+
  ggtitle ("Days from post to update a job")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Level"))


#plot all positions vs. time for posting
#plot(post.to.updated~factor(Civil.Service.Title), jobs, las = 2)

#plot jobs that took more than 30 days to process
day365 <- jobs[which(jobs$post.to.process > 365),]
#plot(post.to.process~factor(day365$Civil.Service.Title), day365,las = 2)
# ... Plot_10...
ggplot(day365)+
  aes(x=factor(day365$Civil.Service.Title), y = post.to.process)+
  geom_point(size = 5, alpha = 0.5,aes(color=Level))+
  #facet_wrap(~agency.abr)+
  xlab ("Civil Service Title") +
  ylab ("Number of days >365 process")+
  ggtitle ("Days from post to update a job")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Level"))



day730 <- jobs[which(jobs$post.to.process > 730),]
#plot(post.to.process~factor(day730$Civil.Service.Title), day730,las = 2)
# ... Plot_11...
ggplot(day730)+
  aes(x=factor(day730$Civil.Service.Title), y = post.to.process)+
  geom_point(size = 5, alpha = 0.5,aes(color=Level))+
  #facet_wrap(~agency.abr)+
  xlab ("Civil Service Title") +
  ylab ("Number of days >365 process")+
  ggtitle ("Days from post to update a job")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Level"))


#plot jobs based on levels
#plot(post.to.process~factor(Level), jobs, las = 2)

# ... Plot_12...
ggplot(jobs)+
  aes(x = as.factor(Level), fill = Agency)+
  geom_bar(width=0.5) +
  facet_wrap(~Posting.Type + ~Salary.Frequency)+
  xlab ("Levels") +
  ylab ("Number of jobs")+
  ggtitle ("Levels of jobs")+
  theme(axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=2))


# hardest to fill positions ---------------------------
vac.fill <- data.frame()
#for (i in 1:nrow(jobs)){
#  if (jobs[i, "post.to.process"] >= 1000){ #change '1000'to choose days
#    vac.fill <- rbind(vac.fill, jobs[i,])
#  }
#}
vac.fill <- jobs[which(jobs$post.to.process >= 1000) , ]

# ... Plot_13...
ggplot(vac.fill)+
  aes(x=post.to.process, fill = Level)+
  geom_bar(width=50) +
  facet_wrap(~agency.abr + ~Civil.Service.Title)+
  xlab ("Number of days") +
  ylab ("Count")+
  ggtitle ("Days from post to process (>1000)")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=1))

vac.fill$Number.Of.Positions <- as.factor(vac.fill$Number.Of.Positions)

# ... Plot_14...
ggplot(vac.fill)+
  aes(x=post.to.process, fill = Level)+
  geom_bar(width=50) +
  facet_wrap(~Number.Of.Positions + ~agency.abr)+
  xlab ("Number of days") +
  ylab ("Count")+
  ggtitle ("Days from post to process (>1000)")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=1))

# filter residency requirements from 'not required' and 'no residency' ---------------------------
jobs$Residency.Requirement <- as.character(jobs$Residency.Requirement)


exfunc <- function(Residency.Requirement){
  Residency.Requirement <- as.character(Residency.Requirement)
  
  if (length(grep("not required", Residency.Requirement)) > 0){
    return ("N")
  } else if (length(grep("no residency", Residency.Requirement)) > 0){
    return ("N")
  }else {
    return ("Y")
  }
}

Residency <- NULL #initiate to NULL
for (i in 1:nrow(jobs)){
    Residency <- c(Residency, exfunc(jobs[i, "Residency.Requirement"]))
}
jobs$Residency <- as.factor(Residency)

# ... Plot_15...
ggplot(jobs)+
  aes(x=Level, fill = Posting.Type)+
  geom_bar() +
  facet_wrap(~Residency)+
  xlab ("Posting Type") +
  ylab ("Count")+
  ggtitle ("Posting type")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 10),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=1))



ggplot(jobs[,], aes(x=post.to.process, fill = Residency))+
  geom_bar(width=0.5) + #geom_bar(width=0.5) did not work
  facet_wrap(~Residency + ~Posting.Type)+
  ggtitle("post.to.process")+
  xlab("Days")+
  ylab("Total Count")+
  labs(fill = "Residency")






# days to fill vs. residency ---------------
ggplot(jobs[,], aes(x=post.to.process, fill = Level))+
  geom_bar(width=5) + #geom_bar(width=0.5) did not work
  facet_wrap(~Residency)+
  ggtitle("Level")+
  xlab("post.to.process")+
  ylab("Total Count")+
  labs(fill = "Levels")

plot(post.to.process~factor(Residency), jobs)

# ... Plot_16...
ggplot(jobs)+
  aes(x=post.to.process, fill = Level)+
  geom_bar(width=5) +
  facet_wrap(~Residency)+
  xlab ("Number of days") +
  ylab ("Count")+
  ggtitle ("Days to fill vs. residency")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=2))



# days to fill vs. salary ---------------
# ... Plot_17...


ggplot(jobs)+
  aes(x=post.to.process, y = hr.sal.to)+
  geom_point(size = 3, alpha = 0.5,aes(color=Level))+
  #facet_wrap(~hr.sal.to)+
  xlab ("Number of days") +
  ylab ("Hourly salary (USD)")+
  ggtitle ("Days to fill vs. salary")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=2))+
  #scale_x_continuous(trans='log2') +
  #scale_y_continuous(trans='log2') +
  scale_y_sqrt() +
  scale_x_sqrt() +
  #stat_smooth(method = "loess", formula = y ~ x, size = 1)
  #stat_smooth(method = "lm", formula = y ~ x, size = 1)
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
  geom_abline(intercept = 37, slope = -5, color="red",linetype="dashed", size=1.5)+



#p1 <- p + geom_text(x = 25, y = 300, label = lm(jobs, jobs$post.to.process, jobs$hr.sal.to), parse = TRUE)
#p1


#plot(post.to.process~factor(hr.sal.to), jobs)

# days to fill vs. salary frequency
# ... Plot_18...
ggplot(jobs)+
  aes(x=post.to.process, y = Salary.Frequency)+
  geom_point(size = 5, alpha = 0.5,aes(color=Level))+
  facet_wrap(~Residency)+
  xlab ("Number of days") +
  ylab ("Salary Frequency")+
  ggtitle ("Days to fill vs. salary frequency")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=2))

#plot(post.to.process~factor(Salary.Frequency), jobs)


# Look up a particular row corresponding to a value
zz <- which(grepl("SENIOR STATIONARY ENGINEER", jobs$Civil.Service.Title))
print(jobs[zz,])

# UNIQUE  ================================

# removing duplicate jobs that were posted, such as internal/external
# these jobs had the same Job.ID
ujobs <- subset(jobs, !duplicated(jobs$Job.ID))


# uplot_1--num_of_vacancies........x
x <- c(ujobs$agency.abr)
y <- table(x)
barplot(y, las = 2, ylab = "Number of vacancies")
# ................................x

# uplot_2--salary_range...........x
# function to convert annual and monthly salary to hourly salary
salconvert <- function(sal, salfreq){
  salfreq <- as.character(salfreq)
  if (salfreq == "Annual"){
    #print(Salary.Range.From/(365*8))
    return (sal/(365*8))  #convert annual pay to hourly rate 8 h/day
  }else if (salfreq == "Monthly"){
    return (sal/(30*8))
  }else if (salfreq == "Hourly"){
    return (sal)
  } else{
    return ("NULL")
  }
}
ujobs$hr.sal.fr <- "NULL"
ujobs$hr.sal.to <- "NULL"
for (i in 1:nrow(jobs)){
  ujobs[i,"hr.sal.fr"] <- as.integer(salconvert(ujobs[i, "Salary.Range.From"], ujobs[i, "Salary.Frequency"]))
  ujobs[i,"hr.sal.to"] <- as.integer(salconvert(ujobs[i, "Salary.Range.To"], ujobs[i, "Salary.Frequency"]))
}

# uplot_2--salary_range_hr...
ujobs$hr.sal.to <- as.integer(ujobs$hr.sal.to)
plot(hr.sal.to~factor(agency.abr), jobs, las = 2)

# ... Plot_19...
ggplot(jobs)+
  aes(x = agency.abr, y=hr.sal.to, fill = sort(Level))+
  geom_bar(stat = 'identity')+
  #facet_wrap(~agency.abr + ~Civil.Service.Title)+
  xlab ("Agency") +
  ylab ("Upper salary limit (USD) ")+
  ggtitle ("Agency salaries (unique)")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=90, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(title="Level"))



#...

#uplot_2--salary_u.sal.blo ::::
ujobs$hr.sal.to <- as.integer(ujobs$hr.sal.to)
u.sal.blo <- data.frame()
for (i in 1:nrow(ujobs)){
  if ((ujobs[i, "hr.sal.to"]) <= 15){
    u.sal.blo <- rbind(u.sal.blo, ujobs[i,])
  } 
}

# ... Plot_20...
ggplot(u.sal.blo)+
  aes(x=hr.sal.to, fill = Level)+
  geom_bar(width=0.5)+
  facet_wrap(~agency.abr + ~Civil.Service.Title)+
  xlab ("Salary (USD/hour)") +
  ylab ("Total Count")+
  ggtitle ("Agencies with salaries below $15/hour (unique)")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))




# uplot_2--salary_u.sal.abv ---------------------------
u.sal.abv <- data.frame()
for (i in 1:nrow(ujobs)){
  if (ujobs[i, "hr.sal.to"] >= 25){
    u.sal.abv <- rbind(u.sal.abv, ujobs[i,])
  }
}
# ... Plot_21...
ggplot(u.sal.abv)+
  aes(x=hr.sal.to, fill = Level)+
  geom_bar(width=5)+
  facet_wrap(~agency.abr + ~Civil.Service.Title)+
  xlab ("Salary (USD/hour)") +
  ylab ("Total Count")+
  ggtitle ("Agencies with salaries above $25/hour (unique)")+
  theme(title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12, face ="bold"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))




#----
u_vac.fill <- data.frame()
u_vac.fill <- jobs[which(jobs$post.to.process >= 1000) , ]


ggplot(u_vac.fill[,], aes(x=post.to.process, fill = Level))+ #note this does not account for 'Number.Of.Positions"
  geom_bar(width=50) + #geom_bar(width=0.5) did not work
  facet_wrap(~agency.abr + ~Civil.Service.Title + ~hr.sal.to + ~Residency)+
  ggtitle("post.to.process0")+
  xlab("post.to.process")+
  ylab("Total Count")+
  labs(fill = "Level")
# ... Plot_22...
ggplot(u_vac.fill)+
  aes(x=post.to.process, fill = Level)+
  geom_bar(width=50) +
  facet_wrap(~agency.abr + ~Civil.Service.Title)+
  xlab ("Number of days") +
  ylab ("Count")+
  ggtitle ("Days from post to process (>1000) (unique)")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  guides(fill=guide_legend(ncol=1))


# ========================================

#
#
# Level vs. Salary
levelConvert <- function(lx){
  lx <- as.character(lx)
  lvl <- c("0", "1", "1A", "1B", "2", "3", "4", "4A", "4B", "M1", "M2", "M3","M4", "M5", "M6", "M7", "M8")
  lvlno <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
  for (i in 1:17){
    #print(c(i, lx, lvl[i], lvlno[i]))    
    if(lx == lvl[i]){return (lvlno[i])}
  }
  return(0)
}
jobs.Level <- as.character(jobs$Level)
for (i in 1:nrow(jobs)){
  jobs$level_no[i] <- levelConvert(as.character(jobs$Level[i]))
}
jobs$hr.sal.fr <- as.numeric(jobs$hr.sal.fr)
jobs$level_no <- as.numeric(jobs$level_no)
plot(jobs$level_no, jobs$hr.sal.fr)

lm.D9 <- lm(jobs$level_no ~ jobs$hr.sal.fr)
lm.D90 <- lm(jobs$level_no ~ jobs$hr.sal.fr - 1) # omitting intercept

anova(lm.D9)
summary(lm.D90)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)
#........
#
#(unique)
ujobs.Level <- as.character(ujobs$Level)
for (i in 1:nrow(ujobs)){
  ujobs$level_no[i] <- levelConvert(as.character(ujobs$Level[i]))
}
ujobs$hr.sal.fr <- as.numeric(ujobs$hr.sal.fr)
ujobs$level_no <- as.numeric(ujobs$level_no)
plot(ujobs$level_no, ujobs$hr.sal.fr)

lm.D9u <- lm(ujobs$level_no ~ ujobs$hr.sal.fr)
lm.D90u <- lm(ujobs$level_no ~ ujobs$hr.sal.fr - 1) # omitting intercept

anova(lm.D9u)
summary(lm.D90u)

oparu <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9u, las = 1)      # Residuals, Fitted, ...
par(oparu)

#
#........................
# post.to.process vs sal
plot(ujobs$post.to.process, ujobs$hr.sal.fr)

lm.ptp <- lm(ujobs$post.to.process ~ ujobs$hr.sal.fr)
lm.ptp0 <- lm(ujobs$post.to.process ~ ujobs$hr.sal.fr - 1) # omitting intercept

anova(lm.ptp)
summary(lm.ptp0)

opar_ptp <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.ptp, las = 1)      # Residuals, Fitted, ...
par(opar_ptp)

#........................
# days to fill vs sal
plot(ujobs$post.to.process, ujobs$hr.sal.fr)

lm.ptp <- lm(ujobs$post.to.process ~ ujobs$hr.sal.fr)
lm.ptp0 <- lm(ujobs$post.to.process ~ ujobs$hr.sal.fr - 1) # omitting intercept

anova(lm.ptp)
summary(lm.ptp0)

opar_ptp <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.ptp, las = 1)      # Residuals, Fitted, ...
par(opar_ptp)

#
#



