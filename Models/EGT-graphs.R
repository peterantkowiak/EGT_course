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
#9#filename <- "Task2_PD_spatial_nb8_lowR"
#10#filename <- "Task2_PD_spatial_nb12"
#11#filename <- "Task2_PD_spatial_nb24"
#12#filename <- "Task3_HD_spatial_nb8_pure_10000"
#NA#filename <- "Task3_HD_spatial_nb8_mixed_10000_incorrect"
#13#filename <- "Task3_HD_spatial_nb4_mixed_10000"
#14#filename <- "Task3_HD_spatial_nb8_mixed_10000"
#15#filename <- "Task3_HD_spatial_nb12_mixed_10000"



########################################################################################
# autoplot
########################################################################################


autoplot <- function(files, which, directory, conf, nspl, maincutoff){
  filestoplot <-files[which]
  for (h in 1:length(filestoplot)){

filename <- filestoplot[h]

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


plot(r_levels[,1], r_levels[,3], type = "p", pch=19, col="red", las=1, ylab="frequency of cooperation", xlab="cost / benefit ratio r", main=paste(substr(filename,7, maincutoff[h])), ylim = c(0,1))
grid()
if(conf){
arrows(r_levels[,1], r_levels[,3]-r_levels[,5], r_levels[,1], r_levels[,3]+r_levels[,5], length=0.05, angle=90, code=3)
}
# Error bars indicating the standard error with a 95 % confidence interval
if(nspl){
if(substr(filename,7,8) == "HD") {
  abline(1,-1,lty=2)}  # HD nonspatial pure and mixed
if(substr(filename,7,8) == "PD") {
  PD_nonsp_x <- seq(0, 1, 0.05) 
  PD_nonsp_y <- c(0.9908, rep(0,19), 0.5)
  lines(PD_nonsp_x,PD_nonsp_y,lty=2)}
  #abline(0,0,lty=2)  # PD nonspatial pure
}
}}


###########################################


files <- c(
  "Task1_HD_nonspatial",
  "Task1_HD_spatial_nb8_pure",
  "Task1_PD_nonspatial",
  "Task1_PD_spatial_nb8",
  "Task2_HD_spatial_nb4",
  "Task2_HD_spatial_nb12",
  "Task2_HD_spatial_nb24",
  "Task2_PD_spatial_nb4",
  "Task2_PD_spatial_nb4_lowR",
  "Task2_PD_spatial_nb8_lowR",
  "Task2_PD_spatial_nb12_lowR",
  "Task2_PD_spatial_nb24_lowR",
  "Task2_PD_spatial_nb12",
  "Task2_PD_spatial_nb24",
  "Task3_HD_spatial_nb8_pure_10000_incorrect",
  "Task3_HD_spatial_nb4_mixed_10000",
  "Task3_HD_spatial_nb8_mixed_10000",
  "Task3_HD_spatial_nb12_mixed_10000",
  "Task3_HD_spatial_nb24_mixed_10000",
  "Task2_PD_spatial_nbvar_r003",
  "Task2_PD_spatial_nbvar_r0065",
  "Task2_PD_spatial_nbvar_r003_10000",
  "Task2_PD_spatial_nbvar_r0065_10000"
  )

directory <- "/home/Peter/Dokumente/uni/WS_14_15/EvolutionaryGameTheory/EGT_course/Report/ResultsAndRcode/"

which <- c(5,2,6,7)
which <- c(15:18)
which <- c(1,2,15,17)

par(mfrow=c(2,2))

autoplot(files,which,directory,conf=T,nspl=T, maincutoff=c(20,20,25,26))




########################################################################################
# several in one plot
########################################################################################

multiplot <- function(files, which, directory, plottype, conf, legend, nspl, lbp, xrange, maincutoff){
  
firstfiletoplot <-files[which[1]]
otherfilestoplot <-files[which[2:length(which)]] 

for (h in firstfiletoplot){
    
filename <- h
exp <- read.csv(paste0(directory,filename,".csv",collapse=""), , head=T, skip=6, dec=".")
setnames(exp,ncol(exp),"propC")
exp.s <- data.table(subset(exp, select = c(X.run.number.,benefit,cost,X.step.,propC)), key="X.run.number.")
exp.s$r <- round(exp.s$cost/(2-exp.s$cost), digits = 3) 
r_levels <- matrix(nrow = length(unique(exp.s$r)), ncol = 5, dimnames = list(as.character(unique(exp.s$r)),c("r","cost","mean","dev","serror")))
r_levels[,1] <- unique(exp.s$r)
r_levels[,2] <- (2*r_levels[,1])/(1+r_levels[,1])
     
########### calculate means ###########  
for (i in 1:nrow(r_levels)){
  j <- r_levels[i,1]
  rws <- subset(exp.s, r == j)
  r_levels[i,3] <- mean(rws$propC)
  r_levels[i,4] <- sd(rws$propC)
  r_levels[i,5] <- qnorm(0.975)*sd(rws$propC)/sqrt(length(rws$propC))
  rm(i,j,rws)
  }
        
########### plot ###################### 
if(plottype == "p"){
plot(r_levels[,1], r_levels[,3], type = "p", pch=21, col="red", bg="red", las=1, ylab="frequency of cooperation", xlab="cost / benefit ratio r", ylim = c(0,1), xlim = xrange)
#main=paste(substr(filename,7, maincutoff)),
if(lbp){lines(r_levels[,1], r_levels[,3], type = "l", lty=3, col="red", lwd=1)}
}
if(plottype == "l"){
plot(r_levels[,1], r_levels[,3], type = "l", lty=1, col="red", lwd=2, las=1, ylab="frequency of cooperation", xlab="cost / benefit ratio r", main=paste(substr(filename,7, maincutoff)), ylim = c(0,1), xlim = xrange)
}
grid()
if(conf){
arrows(r_levels[,1], r_levels[,3]-r_levels[,5], r_levels[,1], r_levels[,3]+r_levels[,5], length=0.05, angle=90, code=3)
}
if(nspl){
if(substr(filename,7,8) == "HD") {
  abline(1,-1,lty=3,lwd=2)}  # HD nonspatial pure and mixed
if(substr(filename,7,8) == "PD") {
  PD_nonsp_x <- seq(0, 1, 0.05) 
  PD_nonsp_y <- c(0.9908, rep(0,19), 0.5)
  lines(PD_nonsp_x,PD_nonsp_y,lty=2)}
    #abline(0,0,lty=2)  # PD nonspatial pure
}
}
for (g in 1:length(otherfilestoplot)){
  
filename <- otherfilestoplot[g]
exp <- read.csv(paste0(directory,filename,".csv",collapse=""), , head=T, skip=6, dec=".")
setnames(exp,ncol(exp),"propC")
exp.s <- data.table(subset(exp, select = c(X.run.number.,benefit,cost,X.step.,propC)), key="X.run.number.")
exp.s$r <- round(exp.s$cost/(2-exp.s$cost), digits = 3) 
r_levels <- matrix(nrow = length(unique(exp.s$r)), ncol = 5, dimnames = list(as.character(unique(exp.s$r)),c("r","cost","mean","dev","serror")))
r_levels[,1] <- unique(exp.s$r)
r_levels[,2] <- (2*r_levels[,1])/(1+r_levels[,1])
  
########### calculate means ###########  
for (i in 1:nrow(r_levels)){
  j <- r_levels[i,1]
  rws <- subset(exp.s, r == j)
  r_levels[i,3] <- mean(rws$propC)
  r_levels[i,4] <- sd(rws$propC)
  r_levels[i,5] <- qnorm(0.975)*sd(rws$propC)/sqrt(length(rws$propC))
  rm(i,j,rws)
}
  
########### plot ###################### 
if(plottype == "p"){
pchvec <- c(22:25)[g]
colvec <- c("blue","green3","orange","purple")[g]
bgvec <- c("blue","green3","orange","purple")[g]
points(r_levels[,1], r_levels[,3], type = "p", pch=pchvec, col=colvec, bg=bgvec, ylim = c(0,1))
if(lbp){lines(r_levels[,1], r_levels[,3], type = "l", lty=3, col=colvec, lwd=1)}
}
if(plottype == "l"){
colvec <- c("blue","green3","orange","purple")[g]
lines(r_levels[,1], r_levels[,3], type = "l", lty=1, col=colvec, lwd=2, ylim = c(0,1))
}
if(conf){
arrows(r_levels[,1], r_levels[,3]-r_levels[,5], r_levels[,1], r_levels[,3]+r_levels[,5], length=0.05, angle=90, code=3)
}

if(legend){
legenditems <- c(rep("NA",length(which)))
for (f in 1:length(which)){
legenditems[f] <- unlist(strsplit(files[which[f]], "[_]"))[4]
}
if(plottype == "p"){
legend("topright",legend=legenditems, col=c("red","blue","green3","orange","purple")[1:length(which)], pch=c(21:25)[1:length(which)], pt.bg=c("red","blue","green3","orange","purple")[1:length(which)],title="Neighbors")
}
if(plottype == "l"){
legend("topright",legend=legenditems, col=c("red","blue","green3","orange","purple")[1:length(which)], lwd=2)
}
}
}
}

par(mfrow=c(1,1))

which <- c(9,10,11,12)
which <- c(16:19)
which <- c(2,17)


multiplot(files,which,directory,"p",conf=F,legend=T,nspl=F,lbp=T,xrange=c(0,1),maincutoff=20)


########################################################################################
# pdf export
########################################################################################


pdf(file="/home/Peter/Dokumente/uni/WS_14_15/EvolutionaryGameTheory/EGT_course/Report/task2_4plot.pdf",width=6, height=7)
par(mfrow=c(2,2))
which <- (5,2,6,7)
autoplot(files,which,directory,conf=T,nspl=T)
dev.off()


pdf(file="/home/Peter/Dokumente/uni/WS_14_15/EvolutionaryGameTheory/EGT_course/Report/task2_multiplot.pdf",width=5, height=5.5)
par(mfrow=c(1,1))
which <- c(9,10,11,12)
multiplot(files,which,directory,"p",conf=F,legend=T,nspl=F,lbp=T,xrange=c(0,0.1),maincutoff=16)
dev.off()


pdf(file="/home/Peter/Dokumente/uni/WS_14_15/EvolutionaryGameTheory/EGT_course/Report/task3_multiplot.pdf",width=5, height=5.5)
par(mfrow=c(1,1))
which <- c(2,17)
multiplot(files,which,directory,"p",conf=F,legend=T,nspl=T,lbp=T,xrange=c(0,1),maincutoff=20)
dev.off()





