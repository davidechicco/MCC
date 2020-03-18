setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
# set.seed(11)
options(repos = list(CRAN="http://cran.rstudio.com/"))

# update R packages
list.of.packages <- c("easypackages", "partitions", "mltools", "Deducer", "ggplot2", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

# For the printed files
num_to_return <- 1
exe_num <- sample(1:as.numeric(10000), num_to_return)

tot_samples <- 100

NPV <- function(TP,TN,FP,FN){
  npv=NA
  if((TN+FN)>0){npv=TN/(TN+FN)}
  return(npv)
  }

BA <- function(TP,TN,FP,FN){
  thisBA=NA
  if( (TP+FN)>0 & (TN+FP)>0){ thisBA=0.5*(TP/(TP+FN)+TN/(TN+FP))}
  return(thisBA)
}

BM <- function(TP,TN,FP,FN){
  thisBM=NA
  if( (TP+FN)>0 & (TN+FP)>0){ thisBM <- (TP)/(TP+FN) + (TN)/(TN+FP) - 1  }
  return(thisBM)
}

MK <- function(TP,TN,FP,FN){
  thisMK=NA
  if( (TP+FP)>0 & (TN+FN)>0){ thisMK <- (TP)/(TP+FP) + (TN)/(TN+FN) - 1  }
  return(thisMK)
}

# dataframe for the 
mydata <- data.frame(TP=NA, TN=NA, FP=NA, FN=NA, MCC=NA, BA=NA, BM=NA, MK=NA)

z <- 0
all_parts <- restrictedparts(tot_samples,4,decreasing = TRUE)
for(j in 1:dim(all_parts)[2]){

	jForPrint <- 100
	if ((j %% jForPrint)==0) { cat(paste(round(100*j/dim(all_parts)[2],2),"% ",sep="")) }

	b <- Deducer::perm(all_parts[,j])
	  for(k in 1:dim(b)[1]){
	    z <- z+1
	    TP=b[k,1]
	    TN=b[k,2]
	    FP=b[k,3]
	    FN=b[k,4]
	    mydata[z,] <- c(TP, TN, FP, FN,
		mcc(TN=TN,TP=TP,FN=FN,FP=FP),
		BA(TN=TN,TP=TP,FN=FN,FP=FP),
		BM(TN=TN,TP=TP,FN=FN,FP=FP),
		MK(TN=TN,TP=TP,FN=FN,FP=FP))
	  }
}

# cor(mydata$NPV, mydata$MK,use="complete")
# cor(mydata$NPV, mydata$MCC,use="complete")
# cor(mydata$MK, mydata$MCC,use="complete")

cat("\n")

gridNX <- gridNY <- 20
pngHeight <- pngWidth <- 480

# MCC versus BA
outputPngFile <- paste0("../results/plot_MCC_versus_BA_rand", exe_num,".png")
# png(filename = outputPngFile, width = pngWidth, height = pngHeight, units = "px", pointsize = 12)
plot_MCC_BA <- ggplot(mydata, aes(x=MCC, y=BA)) + geom_point() + xlab("Matthews correlation coefficient (MCC)") + ylab("balanced accuracy (BA)")

# MCC versus BM
outputPngFile <- paste0("../results/plot_MCC_versus_BM_rand", exe_num,".png")
# png(filename = outputPngFile, width = pngWidth, height = pngHeight, units = "px", pointsize = 12)
plot_MCC_BM <- ggplot(mydata, aes(x=MCC, y=BM)) + geom_point() + xlab("Matthews correlation coefficient (MCC)") + ylab("bookmaker's informedness (BM)")

# MCC versus MK
outputPngFile <- paste0("../results/plot_MCC_versus_MK_rand", exe_num,".png")
# png(filename = outputPngFile, width = pngWidth, height = pngHeight, units = "px", pointsize = 12)
plot_MCC_MK <- ggplot(mydata, aes(x=MCC, y=MK)) + geom_point() + xlab("Matthews correlation coefficient (MCC)") + ylab("markedness (MK)")

# all the three plots together
# outputPngFile <- paste0("../results/plot_MCC_versus_all_rand", exe_num,".png")
#png(filename = outputPngFile, width = pngWidth*3, height = pngHeight, units = "px", pointsize = 12)
outputPdfFile <- paste0("../results/plot_MCC_versus_all_rand", exe_num,".pdf")
all3plots <- grid.arrange(plot_MCC_BA, plot_MCC_BM, plot_MCC_MK, ncol=3)
ggsave(plot=all3plots, filename=outputPdfFile, width=39, height=13, units="cm")
cat("Saved file ", outputPdfFile, "\n")

