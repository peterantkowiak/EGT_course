## EGT-graphs ##


#---------------------------------------
########### read data #################


#---------------------------------------
########### calculate means ###########


#---------------------------------------
########### pot #######################


exp <- read.csv("/home/Peter/Dokumente/uni/WS_14_15/Evolutionary Game Theory/r-scripts/PDstrategies.csv", head=T, skip=6, dec=".")

#install.packages("data.table")
library(data.table)

exp.s <- data.table(exp, key="X.run.number.")
exp.s$ALLD_inv <- ifelse(exp.s$count.players.with..strategy....ALL_D.. == 0, FALSE, TRUE)

ALLD_levels <- matrix(nrow = length(unique(exp.s$initial_ALL_C)), ncol = 4, dimnames = list(as.character(unique(exp.s$initial_ALL_C)),c("initial_ALL_C","nr.TRUE","nr.FALSE","PropInv")))
ALLD_levels[,1] <- unique(exp.s$initial_ALL_C)

for (i in 1:nrow(ALLD_levels)){
  j <- ALLD_levels[i,1]
  ALLD_levels[i,2] <- length(which((exp.s$initial_ALL_C == j) & (exp.s$ALLD_inv == TRUE)))
  ALLD_levels[i,3] <- length(which((exp.s$initial_ALL_C == j) & (exp.s$ALLD_inv == FALSE)))
  ALLD_levels[i,4] <- ALLD_levels[i,2] / (ALLD_levels[i,2] + ALLD_levels[i,3])
}

# View(ALLD_levels)

plot(ALLD_levels[,1], ALLD_levels[,4], type = "l", xaxp = c(0, 0.45, 9), lwd=2, col="red", las=1, ylab="Proportion of runs with successful invasion of ALL_D  [ i = 20 ]", xlab="Initial proportion of ALL_C", main= "ALL_D invasion success in relation to initial ALL_C")
