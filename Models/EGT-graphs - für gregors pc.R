## EGT-graphs ##

#install.packages("data.table")
#install.packages("Hmisc")

library(Hmisc)
library(data.table)

#---------------------------------------
########### read data #################

#filename <- "Task1_HD_nonspatial"
#filename <- "Task1_HD_spatial_nb8"
#filename <- "Task1_PD_nonspatial" #### corupt! Too low initial_propD!
#filename <- "Task1_PD_spatial_nb8"
#filename <- "Task2_HD_spatial_nb4"
#filename <- "Task2_HD_spatial_nb12"
#filename <- "Task2_HD_spatial_nb24"
filename <- "Task2_PD_spatial_nb4"
#filename <- "Task2_PD_spatial_nb12"
#filename <- "Task2_PD_spatial_nb24"
#filename <- "Task3_HD_spatial_nb8_pure_10000"
#filename <- "Task3_HD_spatial_nb8_mixed_10000_new"
#filename <- "Task3_HD_spatial_nb8_mixed_10000_correct"
#filename <- "Task3_HD_spatial_nb8_mixed_10000_final"

### needs separate treatment --> in line "setnames", change ncol(exp) to ncol(exp)-1
#filename <- "Task3_HD_spatial_nb8_mixed_10000"



directory <- "C:/Users/G/Desktop/Masterstudium/EGT/EGT_course/Report/ResultsAndRcode/"


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


plot(r_levels[,1], r_levels[,3], type = "p", pch=19, col="red", las=1, ylab="Frequency of cooperation  [ t = 5000, i = 10 ]", xlab="cost / benefit ratio r", main=paste(filename), ylim = c(0,1))
grid()
arrows(r_levels[,1], r_levels[,3]-r_levels[,5], r_levels[,1], r_levels[,3]+r_levels[,5], length=0.05, angle=90, code=3)
# Error bars indicating the standard error with a 95 % confidence interval

abline(1,-1,lty=2) # HD nonspatial pure and mixed

#lines(r_levels[,1],1 - r_levels[,2]) # ???

filename <- "Task1_PD_nonspatial"


directory <- "C:/Users/G/Desktop/Masterstudium/EGT/EGT_course/Report/ResultsAndRcode/"


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
 
lines(r_levels[,1], r_levels[,3])


