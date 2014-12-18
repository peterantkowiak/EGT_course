## Varying radius


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
  "Task2_PD_spatial_nbvar_r0065"
)

directory <- "/home/Peter/Dokumente/uni/WS_14_15/Evolutionary Game Theory/EGT_course/Report/ResultsAndRcode/"

which <- c(5,2,6,7)





########################################################################################
# several in one plot
########################################################################################


variplot <- function(files, which, directory, plottype, conf, legend, nspl, lbp, xrange, maincutoff){
  
  firstfiletoplot <-files[which[1]]
  if (length(which) > 1) {otherfilestoplot <-files[which[2:length(which)]] }
  
  for (h in firstfiletoplot){
    
    filename <- h
    exp <- read.csv(paste0(directory,filename,".csv",collapse=""), , head=T, skip=6, dec=".")
    setnames(exp,ncol(exp),"propC")
    exp.s <- data.table(subset(exp, select = c(X.run.number.,Neighborhood_size,benefit,cost,X.step.,propC)), key="X.run.number.")
    exp.s$r <- round(exp.s$cost/(2-exp.s$cost), digits = 3) 
    r_levels <- matrix(nrow = length(unique(exp.s$Neighborhood_size)), ncol = 5, dimnames = list(as.character(unique(exp.s$Neighborhood_size)),c("radius","Neighborhood_size","mean","dev","serror")))
    r_levels[,2] <- unique(exp.s$Neighborhood_size)
    for (i in 1:nrow(r_levels)){
    if (r_levels[i,2] == 4){r_levels[i,1] <- 1} else {
    if (r_levels[i,2] == 8){r_levels[i,1] <- sqrt(2)} else {
    if (r_levels[i,2] == 12){r_levels[i,1] <- 2} else {
    if (r_levels[i,2] == 24){r_levels[i,1] <- 2*sqrt(2)}
    }}}
rm(i)
    }
        
    ########### calculate means ###########  
    for (i in 1:nrow(r_levels)){
      j <- r_levels[i,2]
      rws <- subset(exp.s, Neighborhood_size == j)
      r_levels[i,3] <- mean(rws$propC)
      r_levels[i,4] <- sd(rws$propC)
      r_levels[i,5] <- qnorm(0.975)*sd(rws$propC)/sqrt(length(rws$propC))
      rm(i,j,rws)
    }
    
    ########### plot ###################### 
    if(plottype == "p"){
      plot(r_levels[,1], r_levels[,3], type = "p", pch=21, col="red", bg="red", las=1, ylab="frequency of cooperation", xlab="neighborhood radius", ylim = c(0,1), xlim = xrange)
      #main=paste(substr(filename,7, maincutoff)), 
     if(lbp){ lines(r_levels[,1], r_levels[,3], type = "l", lty=3, col="red", lwd=1)}
    }
    if(plottype == "l"){
      plot(r_levels[,1], r_levels[,3], type = "l", lty=1, col="red", lwd=2, las=1, ylab="frequency of cooperation", xlab="neighborhood radius", main=paste(substr(filename,7, maincutoff)), ylim = c(0,1), xlim = xrange)
    }
    grid()
    if(conf){
      arrows(r_levels[,1], r_levels[,3]-r_levels[,5], r_levels[,1], r_levels[,3]+r_levels[,5], length=0.05, angle=90, code=3)
    }
  }
  
if (length(which) > 1) {  
  for (g in 1:length(otherfilestoplot)){
    
    filename <- otherfilestoplot[g]
    exp <- read.csv(paste0(directory,filename,".csv",collapse=""), , head=T, skip=6, dec=".")
    setnames(exp,ncol(exp),"propC")
    exp.s <- data.table(subset(exp, select = c(X.run.number.,Neighborhood_size,benefit,cost,X.step.,propC)), key="X.run.number.")
    exp.s$r <- round(exp.s$cost/(2-exp.s$cost), digits = 3) 
    r_levels <- matrix(nrow = length(unique(exp.s$Neighborhood_size)), ncol = 5, dimnames = list(as.character(unique(exp.s$Neighborhood_size)),c("radius","Neighborhood_size","mean","dev","serror")))
    r_levels[,2] <- unique(exp.s$Neighborhood_size)
    for (i in 1:nrow(r_levels)){
    if (r_levels[i,2] == 4){r_levels[i,1] <- 1} else {
    if (r_levels[i,2] == 8){r_levels[i,1] <- sqrt(2)} else {
    if (r_levels[i,2] == 12){r_levels[i,1] <- 2} else {
    if (r_levels[i,2] == 24){r_levels[i,1] <- 2*sqrt(2)}
    }}}
    rm(i)
    }
    ########### calculate means ###########  
    for (i in 1:nrow(r_levels)){
      j <- r_levels[i,2]
      rws <- subset(exp.s, Neighborhood_size == j)
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
  }
    if(legend){
      legenditems <- c(rep("NA",length(which)))
      for (f in 1:length(which)){
        legenditems[f] <- unlist(strsplit(files[which[f]], "[_]"))[4]
        rm(f)
      }
      if(plottype == "p"){
        legend("topleft",legend=c("c/b ratio r = 0.03", "c/b ratio r = 0.065"), col=c("red","blue","green3","orange","purple")[1:length(which)], pch=c(21:25)[1:length(which)], pt.bg=c("red","blue","green3","orange","purple")[1:length(which)])
      }
      if(plottype == "l"){
        legend("topleft",legend=c("c/b ratio r = 0.03", "c/b ratio r = 0.065"), col=c("red","blue","green3","orange","purple")[1:length(which)], lwd=2)
      }
    }
  }
}

par(mfrow=c(1,1))

which <- c(9,10,11,12)
which <- c(16:19)
which <- c(20,21)

variplot(files,which,directory,"p",conf=T,legend=T,nspl=F,lbp=T,xrange=c(0.3,3.5),maincutoff=16)




########################################################################################
# pdf export
########################################################################################

pdf(file="/home/Peter/Dokumente/uni/WS_14_15/Evolutionary Game Theory/EGT_course/Report/task2_radiusplot.pdf",width=5, height=5.5)
par(mfrow=c(1,1))
which <- c(20,21)
variplot(files,which,directory,"p",conf=T,legend=T,nspl=F,lbp=T,xrange=c(0.3,3.5),maincutoff=16)
dev.off()



