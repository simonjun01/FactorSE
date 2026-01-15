
############################################################
## Table 1: Testing Factor Model Efficiency
############################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table1"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table1/Table1_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")


# Load user-defined functions used in the analysis
# (e.g., FSE_stage_bench, BSE_stage_bench, GRS_test, HDA_test)
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
S_BS6 =  c("MKT","SMB","IA", "ROE", 'UMD',"HMLM")
  
model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)

# Containers for expanded models
model_fin_FSE =  list()
model_fin_BSE =  list()
for (m in 1:length(model_fin)) {
  print(m)
  # Current benchmark model
  bench_factor = unlist(model_fin[m])
  
  # Forward Stepwise Evaluation (FSE)
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
  model_fin_FSE[[m]] = S_FSE
  
  # Backward Stepwise Evaluation (BSE)
  S_BSE = BSE_stage_bench(Factor97, lambda = 0.05, S_FSE)
  model_fin_BSE[[m]] = S_BSE
}


## 5. Model evaluation using GRS and HDA tests
model_fin_all1 = c(model_fin,model_fin_FSE)

result_p_all = matrix(,length(model_fin_all1),5)
  
  for (m in 1:length(model_fin_all1)) {
    print(m)
    S_factor = model_fin_all1[[m]]
    # RHS: factors included in the model
    factor_RHS = Factor97[c(S_factor), , drop = FALSE]
    # LHS: excluded factors (treated as test assets)
    factor_LHS = Factor97[!names97 %in% c(S_factor),, drop = FALSE]
    
    ###### GRS test
    result_GRS = GRS_test(t(factor_RHS), t(factor_LHS))
    p_GRS = result_GRS[[1]]
    GRS = result_GRS[[2]]

    ###### HDA test
    result_HDA = HDA_test(t(factor_RHS), t(factor_LHS))
    p_HDA = result_HDA[[1]]
    HDA = result_HDA[[2]]

    
    
    result_p_all[m,] = c(length(S_factor), sprintf("%.2f", GRS),  sprintf("%.3f", p_GRS), 
                         sprintf("%.2f", HDA),  sprintf("%.3f", p_HDA))
  }


rownames(result_p_all) =  c('CAPM','FF3', 'FF5', 'FF6','Q5', 'DHS3','BS6',
                             'CAPM$^F$','FF3$^F$', 'FF5$^F$','FF6$^F$', 'Q5$^F$','DHS3$^F$','BS6$^F$')

colnames(result_p_all) = c('# M','GRS' ,'p_GRS' ,'HDA' ,'p_HDA' )

write.csv(result_p_all, '../output/Table1/Table1.csv')


end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()







