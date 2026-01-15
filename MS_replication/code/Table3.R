
############################################################################
# Table 3: Performance Improvement for Expanded and Reduced Models
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
folder_name <- "../output/Table3"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table3/Table3_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

source('fun_Section.R')

# Load 97 factor returns
Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

# Load 285 test assets
Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

# Standard asset pricing benchmark models
S_CAPM = c("MKT")
S_FF3 = c("MKT","SMB","HML")
S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_FF6 = c(S_FF5,'UMD' )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" )
S_DHS3 = c('MKT', "PEAD", "FIN")
S_BS6 =  c("MKT","SMB","IA", "ROE", 'UMD',"HMLM")

model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)

# Containers for expanded models
model_fin_FSE =  list()
model_fin_BSE =  list()
for (m in 1:length(model_fin)) {
  print(m)
  # Current benchmark model
  bench_factor = unlist(model_fin[m])
  
  # Forward Stepwise Evaluation (FSE):
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
  model_fin_FSE[[m]] = S_FSE
  
  # Backward Stepwise Evaluation (BSE):
  S_BSE = BSE_stage_bench(Factor97, lambda = 0.05, S_FSE)
  model_fin_BSE[[m]] = S_BSE
}


merged_list <- c(model_fin, model_fin_FSE, model_fin_BSE)
max_length <- max(lengths(merged_list)) 
model_fin_all <- do.call(cbind, lapply(merged_list, function(x) c(x, rep(NA, max_length - length(x)))))


# model_fin_all = read.csv('./Table1/Model_fin_all.csv')
model_fin_all = model_fin_all[,c(1:15)]

### Asset Pricing (unselected candidate factors)
AP_result_remaining = R2_all_factor(Factor97,model_fin_all)

### Asset Pricing (285 basis portfolios)
AP_result_test_asset = R2_all_test_asset(Factor97,Fama285,model_fin_all)

##### Investment performance
invest_result_all = investing_asset(Factor97,model_fin_all)

result_all = cbind(AP_result_remaining,AP_result_test_asset,invest_result_all)

colnames(result_all) = c('#M', 
                         'Aalpha','Atalpha','sign2',
                         'Total_R2','CS_R2',
                         'Aalpha','Atalpha','sign2',
                         'Total_R2','CS_R2',
                         "AVG","Ann.SR",'alpha_CAPM',"alpha_FF5") 
rownames(result_all) = c(
  'CAPM','FF3', 'FF5','FF6', 'Q5','DHS3','BS6',
  'CAPM$^F$','FF3$^F$', 'FF5$^F$','FF6$^F$', 'Q5$^F$','DHS3$^F$','BS6$^F$','M8')


write.csv(result_all,'../output/Table3/Table3.csv')


end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()

