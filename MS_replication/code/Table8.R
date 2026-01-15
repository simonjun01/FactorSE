############################################################################
# Table 8: Reduced Models with Other Baseline Models
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table8"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table8/Table8_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Load supporting functions
source('fun_Section.R')

# Load 97 factor returns
Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

# Load 285 test assets
Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

# Load RP-PCA factors
Factor_RPPCA5 = read.csv('../data/RP-PCA/RPPCA_Factor.csv')
Factor_RPPCA5 = as.matrix(t(Factor_RPPCA5))

# Load RRA factors
Factor_RRA5 = read.csv('../data/RRA/RRA_Factor.csv')
Factor_RRA5 = as.matrix(t(Factor_RRA5))

S_RRA5 = c('RRA1','RRA2','RRA3','RRA4','RRA5')
S_RPPCA5 = c('RPCA1','RPCA2','RPCA3','RPCA4','RPCA5')
S_KNS2020 = c("MKT", "SMB","BAB","ROE","ABR","CIM","dROE")
S_FGX2020 = c("MKT","UMD","BAB","CIM","R5A")

model_fin = list(S_RPPCA5,S_RRA5,S_FGX2020,S_KNS2020)
model_names = c('RPPCA','RRA','FGX2020','KNS2020')


model_fin_BSE =  list()
for (m in 1:length(model_names)) {
  
  if (model_names[m] == 'RPPCA'){
    factor_all = rbind(Factor97,Factor_RPPCA5)
  } else if (model_names[m] == 'RRA'){
    factor_all = rbind(Factor97,Factor_RRA5)
  } else {
    factor_all = Factor97
  }
  
  
  bench_factor = unlist(model_fin[m])
  # Forward Stepwise Evaluation (FSE)
  S_FSE = FSE_stage_bench(factor_all, lambda = 0.05,bench_factor)
  # Backward Stepwise Evaluation (BSE)
  S_BSE = BSE_stage_bench(factor_all, lambda = 0.05, S_FSE)
  model_fin_BSE[[m]] = S_BSE
  
}


merged_list <- c(model_fin, model_fin_BSE)
max_length <- max(lengths(merged_list))  
model_fin_all <- do.call(cbind, lapply(merged_list, function(x) c(x, rep(NA, max_length - length(x)))))


model_fin_all = model_fin_all[,c(1:2, 5:6 ,3:4, 7:8)]
colnames(model_fin_all) =  c('RP-PCA','RRA','RP-PCA$^{F+B}$','RRA$^{F+B}$',
                             'FGX2020','KNS2020','FGX2020$^{F+B}$','KNS2020$^{F+B}$')

write.csv(model_fin_all,'../output/Table8/Table8.csv')


end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()