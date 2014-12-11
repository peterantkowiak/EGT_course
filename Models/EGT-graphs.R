## EGT-graphs ##


#---------------------------------------
########### read data #################

filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_spatial_nb8"
#filename <- "Task1_PD_nonspatial_nb8"
#filename <- "Task1_PD_spatial_nb8"
#filename <- "Task2_HD_spatial_nb4"
#filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_nonspatial_nb8"
#filename <- "Task1_HD_nonspatial_nb8"

directory <- "/home/Peter/Dokumente/uni/WS_14_15/Evolutionary Game Theory/EGT_course/Report/ResultsAndRcode/"


exp <- read.csv(paste0(directory,filename,".csv",collapse=""), , head=T, skip=6, dec=".")

  
#exp <- read.csv("/home/Peter/Dokumente/uni/WS_14_15/Evolutionary Game Theory/EGT_course/Report/ResultsAndRcode/Task1_HD_nonspatial_nb8.csv", head=T, skip=6, dec=".")



#install.packages("data.table")
library(data.table)


exp.s <- data.table(exp, key="X.run.number.")
#exp.s

exp.s$r <- round(exp.s$cost/(2-exp.s$cost), digits = 3) 
setnames(exp.s,8,"propC")

r_levels <- matrix(nrow = length(unique(exp.s$r)), ncol = 4, dimnames = list(as.character(unique(exp.s$r)),c("r","mean","dev","serror")))
r_levels[,1] <- unique(exp.s$r)



#---------------------------------------
########### calculate means ###########

for (i in 1:nrow(r_levels)){
  j <- r_levels[i,1]
  rws <- subset(exp.s, r == j)
  r_levels[i,2] <- mean(rws$propC)
  r_levels[i,3] <- sd(rws$propC)
  r_levels[i,4] <- sd(rws$propC)/sqrt(length(rws$propC))
  rm(i,j,rws)
}


#---------------------------------------
########### plot ######################
# install.packages("Hmisc")
library(Hmisc)

plot(r_levels[,1], r_levels[,2], type = "p", pch=19, col="red", las=1, ylab="Frequency of cooperation  [ t = 5000, i = 10 ]", xlab="cost / benefit ratio r", main=paste(filename) )

arrows(r_levels[,1], r_levels[,2]-1.96*r_levels[,4], r_levels[,1], r_levels[,2]+1.96*r_levels[,4], length=0.05, angle=90, code=3)
# Error bars indicating the standard error with a 95 % confidence interval

abline(1,-1,lty=2)




