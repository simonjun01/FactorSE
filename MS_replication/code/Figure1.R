############################################################################
# Figure 1: Expanded Factor Models (FSE Process)
# Note: This file generates intermediate results for Figure 1. 
# After running this script, execute the corresponding Figure1_plot.py file to create Figure 1
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
folder_name <- "../output/Figure1"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Figure1/Figure1_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Load supporting functions
source('fun_Section.R')

# Load 97 factor returns
Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)


# Standard asset pricing benchmark models
S_CAPM = c("MKT")
S_FF3 = c("MKT","SMB","HML")
S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_FF6 = c(S_FF5,'UMD' )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" ) 
S_DHS3 = c('MKT', "PEAD", "FIN")
S_BS6 =  c("MKT","SMB","IA", "ROE" , 'UMD',"HMLM" )
  
model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)

# Containers for expanded models
model_fin_FSE =  list()
for (m in 1:length(model_fin)) {
  print(m)
  # Current benchmark model
  bench_factor = unlist(model_fin[m])
  
  # Forward Stepwise Evaluation (FSE)
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
  model_fin_FSE[[m]] = S_FSE
  
}


max_length <- max(lengths(model_fin_FSE)) 
model_fin_all_FSE <- do.call(cbind, lapply(model_fin_FSE, function(x) c(x, rep(NA, max_length - length(x)))))

# Create model display matrix with benchmark names in first row
model_FSE = matrix(,nrow(model_fin_all_FSE),ncol(model_fin_all_FSE))

factor_model = c('CAPM','FF3','FF5','FF6','Q5','DHS3','BS6')
for (i in 1:length(model_fin)) {
  model_fin_all_FSE1 = na.omit(model_fin_all_FSE[,i])
  
  # Identify newly added factors (excluding original benchmark)
  model2 = setdiff(model_fin_all_FSE1, unlist(model_fin[i]))
  model_FSE[1:(length(model2)+1),i] = c(factor_model[i],model2) 
}

model_FSE <- model_FSE[!apply(is.na(model_FSE), 1, all), ]


GRS_FSE = matrix(,nrow(model_FSE),ncol(model_FSE))
SR_FSE = matrix(,nrow(model_FSE),ncol(model_FSE))
HDA_FSE = matrix(,nrow(model_FSE),ncol(model_FSE))

for (m in 1:length(model_fin)) {
  model_fin_all_FSE1 = na.omit(model_fin_all_FSE[,m])
  model2 = setdiff(model_fin_all_FSE1, unlist(model_fin[m]))
  
  for (j in 1:(length(model2)+1)) {
  Sj = model_fin_all_FSE1[1:(length(unlist(model_fin[m])) + j -1)]

factor_RHS = Factor97[c(Sj), , drop = FALSE] 
factor_LHS = Factor97[!rownames(Factor97) %in% c(Sj),, drop = FALSE]

F1 = t(factor_RHS)
F2 = t(factor_LHS)
## GRS test
result1 = GRS_test(F1, F2)
## HDA test
result2 = HDA_test(F1, F2)

GRS_FSE[j,m] = result1[[2]]
HDA_FSE[j,m] = judge.star_HDA(result2[[1]])
## Max SR
SR = sqrt(calc_Max_SR2(F1))
SR = SR*sqrt(12)
SR_FSE[j,m] = SR
}
}

colnames(model_FSE) = colnames(GRS_FSE) = colnames(SR_FSE) = 
colnames(HDA_FSE)  = c('CAPM','FF3', 'FF5', 'FF6','Q5', 'DHS3','BS6')
                            
write.csv(model_FSE, '../output/Figure1/Figure1_model_FSE.csv')
write.csv(GRS_FSE, '../output/Figure1/Figure1_GRS_FSE.csv')
write.csv(SR_FSE, '../output/Figure1/Figure1_SR_FSE.csv')
write.csv(HDA_FSE, '../output/Figure1/Figure1_HDA_FSE.csv')

end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()
