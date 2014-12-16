## EGT-graphs ##

#install.packages("data.table")
# install.packages("Hmisc")

library(Hmisc)
library(data.table)




#---------------------------------------
########### read data #################

#1#filename <- "Task1_HD_nonspatial"
#2#filename <- "Task1_HD_spatial_nb8"
#3#filename <- "Task1_PD_nonspatial" 
#4#filename <- "Task1_PD_spatial_nb8"
#5#filename <- "Task2_HD_spatial_nb4"
#6#filename <- "Task2_HD_spatial_nb12"
#7#filename <- "Task2_HD_spatial_nb24"
#8#filename <- "Task2_PD_spatial_nb4"
#9#filename <- "Task2_PD_spatial_nb12"
#10#filename <- "Task2_PD_spatial_nb24"
#11#filename <- "Task3_HD_spatial_nb8_pure_10000"
#12#filename <- "Task3_HD_spatial_nb8_mixed_10000_incorrect"
#13#filename <- "Task3_HD_spatial_nb8_mixed_10000_final"



autoplot <- function(files, which, directory){
  filestoplot <-files[which]
  for (h in filestoplot){

filename <- h

exp <- read.csv(paste0(directory,filename,".csv",collapse=""), , head=T, skip=6, dec=".")


setnames(exp,ncol(exp),"propC")


exp.s <- data.table(subset(exp, select = c(X.run.number.,benefit,cost,X.step.,propC)), key="X.run.number.")
#exp.s <- data.table(subset(exp, select = c(X.run.number.,benefit,cost,initial_propD,X.step.,propC)), key="X.run.number.")
#exp.s

exp.s$r <- round(exp.s$cost/(2-exp.s$cost), digits = 3) 

r_levels <- matrix(nrow = length(unique(exp.s$r)), ncol = 5, dimnames = list(as.character(unique(exp.s$r)),c("r","cost","mean","dev","serror")))
r_levels[,1] <- unique(exp.s$r)
r_levels[,2] <- (2*r_levels[,1])/(1+r_levels[,1])


#---------------------------------------
########### calculate means ###########

for (i in 1:nrow(r_levels)){
  j <- r_levels[i,1]
  rws <- subset(exp.s, r == j)
  r_levels[i,3] <- mean(rws$propC)
  r_levels[i,4] <- sd(rws$propC)
  r_levels[i,5] <- qnorm(0.975)*sd(rws$propC)/sqrt(length(rws$propC))
  rm(i,j,rws)
}


#---------------------------------------
########### plot ######################


plot(r_levels[,1], r_levels[,3], type = "p", pch=19, col="red", las=1, ylab="Frequency of cooperation  [ t = 5000, i = 10 ]", xlab="cost / benefit ratio r", main=paste(substr(filename,7, nchar(filename))), ylim = c(0,1))
grid()
arrows(r_levels[,1], r_levels[,3]-r_levels[,5], r_levels[,1], r_levels[,3]+r_levels[,5], length=0.05, angle=90, code=3)
# Error bars indicating the standard error with a 95 % confidence interval

if(substr(filename,7,8) == "HD") {
  abline(1,-1,lty=2) }  # HD nonspatial pure and mixed
if(substr(filename,7,8) == "PD") {
  abline(0,0,lty=2) } # PD nonspatial pure

#lines(r_levels[,1],1 - r_levels[,2]) # ???

  }
}

###########################################


files <- c("Task1_HD_nonspatial","Task1_HD_spatial_nb8","Task1_PD_nonspatial","Task1_PD_spatial_nb8","Task2_HD_spatial_nb4","Task2_HD_spatial_nb12","Task2_HD_spatial_nb24","Task2_PD_spatial_nb4","Task2_PD_spatial_nb12","Task2_PD_spatial_nb24","Task3_HD_spatial_nb8_pure_10000","Task3_HD_spatial_nb8_mixed_10000_final")

directory <- "/home/Peter/Dokumente/uni/WS_14_15/Evolutionary Game Theory/EGT_course/Report/ResultsAndRcode/"

which <- c(1:4)


par(mfrow=c(2,2))

autoplot(files,which,directory)
