options(stringsAsFactors = FALSE)
# library("clusterSim")

list.of.packages <- c("easypackages", "PRROC", "e1071", "Metrics", "MLmetrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(new.packages)
script_dir <- dirname(sys.frame(1)$ofile)
cat("script_dir: ", script_dir, "\n", sep="")
source(paste0(script_dir,"/utils.r"))

# regression rates
regression_rates <- function(actual_labels, predicted_values, keyword)
{

    thisRMSE <- rmse(actual_labels, predicted_values)
    thisMAE <- mae(actual_labels, predicted_values)
    thisMSE <- mse(actual_labels, predicted_values)
    thisSMAPE <- smape(actual_labels, predicted_values)
    
    thisR2score <- MLmetrics::R2_Score(predicted_values, actual_labels) # (predicted_values, actual_labels) # notice the swap
    # R2_Score(y_pred, y_true)

    cat("  @@@ regression :: \t RMSE \t MAE \t MSE  \t SMAPE \t R^2 \n")
    cat("  @@@ regression :: \t ", dec_three(thisRMSE), " \t ", dec_three(thisMAE), " \t ", dec_three(thisMSE),  " \t ", dec_three(thisSMAPE),  " \t ", dec_three(thisR2score)," \n", sep="")
    
    NUM_METRICS <- 5
    outputDataframe <- matrix(ncol=NUM_METRICS, nrow=1)
    outputDataframe[,1] <- thisRMSE
    outputDataframe[,2] <- thisMAE
    outputDataframe[,3] <- thisMSE
    outputDataframe[,4] <- thisSMAPE
    outputDataframe[,5] <- thisR2score
    colnames(outputDataframe) <- c("RMSE", "MAE", "MSE", "SMAPE", "R^2")

    return(outputDataframe)
}


# Confusion matrix rates
confusion_matrix_rates <- function (actual_labels, predicted_values, keyword)
{

    fg_test <- predicted_values[actual_labels==1]
    bg_test <- predicted_values[actual_labels==0]

    pr_curve_test <- pr.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
    # plot(pr_curve_test)
    # print(pr_curve_test)
    prc_auc <- pr_curve_test$auc.integral
    cat("\nPR AUC (integral) \t", prc_auc, "\n", sep="")    
    # cat("PRC AUC (Davis & Goadrich) ", pr_curve_test$auc.davis.goadrichl, "\n", sep="")

    roc_curve_test <- PRROC::roc.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
    # plot(pr_curve_test)
    # print(roc_curve_test)
    roc_auc <- roc_curve_test$auc
    cat("ROC AUC \t\t", roc_auc, "\n\n", sep="")

    predicted_values_binary <- as.numeric(predicted_values)
    predicted_values_binary[predicted_values_binary>=threshold]=1
    predicted_values_binary[predicted_values_binary<threshold]=0

    actual <- actual_labels
    predicted <- predicted_values_binary
  
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  
  
  cat("\nTOTAL:\n\n")
  cat(" FN = ", (FN), " / ", (FN+TP), "\t (truth == 1) & (prediction < threshold)\n");
  cat(" TP = ", (TP), " / ", (FN+TP),"\t (truth == 1) & (prediction >= threshold)\n\n");
	

  cat(" FP = ", (FP), " / ", (FP+TN), "\t (truth == 0) & (prediction >= threshold)\n");
  cat(" TN = ", (TN), " / ", (FP+TN), "\t (truth == 0) & (prediction < threshold)\n\n");
  
  sum1 <- TP+FP; sum2 <-TP+FN ; sum3 <-TN+FP ; sum4 <- TN+FN;
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  thisMcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  
  f1_score <- 2*TP / (2*TP + FP + FN)
  accuracy <- (TN+TP) / (TN + TP + FP + FN)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  cat("\n\n",keyword,"\t MCC \t F1_score \t accuracy \t TP_rate \t TN_rate \t PR AUC \t ROC AUC\n")
  cat(keyword,"      ", sep="")
  cat(dec_three(thisMcc), " \t ",  sep="")
  cat(dec_three(f1_score), " \t ",  sep="")
  cat(dec_three(accuracy), " \t ",  sep="")
  cat(dec_three(recall), " \t ",  sep="")
  cat(dec_three(specificity),  "\t\t ",  sep="")
  cat(dec_three(prc_auc), "\t\t",  sep="")
  cat(dec_three(roc_auc), sep="",  "\n\n")
 
  #  resultsList <- list("MCC" = thisMcc, "F1 score" = f1_score, "accuracy" = accuracy, "TP rate" = recall, "TN rate" = specificity, "PR AUC" = prc_auc, "ROC AUC" = roc_auc)

    NUM_METRICS <- 7
    outputDataframe <- matrix(ncol=NUM_METRICS, nrow=1)
    outputDataframe[,1] <- thisMcc
    outputDataframe[,2] <- f1_score
    outputDataframe[,3] <- accuracy
    outputDataframe[,4] <- recall
    outputDataframe[,5] <- specificity
    outputDataframe[,6] <- prc_auc
    outputDataframe[,7] <- roc_auc
    colnames(outputDataframe) <- c("MCC", "F1_score", "accuracy", "TP_rate", "TN_rate", "PR_AUC", "ROC_AUC")

    return(outputDataframe)
}

# Matthews correlation coefficient
mcc <- function (actual, predicted)
{
  # Compute the Matthews correlation coefficient (MCC) score
  # Jeff Hebert 9/1/2016
  # Geoffrey Anderson 10/14/2016 
  # Added zero denominator handling.
  # Avoided overflow error on large-ish products in denominator.
  #
  # actual = vector of true outcomes, 1 = Positive, 0 = Negative
  # predicted = vector of predicted outcomes, 1 = Positive, 0 = Negative
  # function returns MCC
  
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  #TP;TN;FP;FN # for debugging
  sum1 <- TP+FP; sum2 <-TP+FN ; sum3 <-TN+FP ; sum4 <- TN+FN;
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  mcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  
  cat("\nMCC = ", (mcc), "\n\n", sep="")
  
  return(mcc)
}
