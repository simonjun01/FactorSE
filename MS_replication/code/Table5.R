############################################################################
# Table 5: In-sample and Out-of-sample Performance
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table5"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table5/Table5_run.log", split = TRUE)
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

S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" )
S_M8 =  c('MKT','SMB','REG','PEAD','HMLM','STR','ILR','EPRD')
model_fin = list(S_FF5,S_Q5)


Ts = nrow(Fama285)
L = Ts / 3
times_name = c('Time1','Time2','Time3')


result_all_in_out = matrix(,10,15)

for (z in 1:3) {
  

Factor97_in = Factor97[,-c((L*(z-1)+1):(L*z))]
Fama285_in = Fama285[-c((L*(z-1)+1):(L*z)),]
  
Factor97_out = Factor97[,(L*(z-1)+1):(L*z)]
Fama285_out = Fama285[(L*(z-1)+1):(L*z),]


model_fin_BSE =  list()
for (m in 1:length(model_fin)) {
print(c(z,m))
  m1 = unlist(model_fin[m])
  bench_factor = m1
  # Forward Stepwise Evaluation (FSE)
  S_FSE = FSE_stage_bench(Factor97_in, lambda = 0.05,bench_factor)
  # Backward Stepwise Evaluation (BSE)
  bench_factor_BSE = S_FSE
  S_BSE =  BSE_stage_bench(Factor97_in, lambda = 0.05,bench_factor_BSE)
  model_fin_BSE[[m]] = S_BSE
}

  
merged_list <- c(model_fin,  model_fin_BSE, list(S_M8))
max_length <- max(lengths(merged_list))
model_fin_all <- do.call(cbind, lapply(merged_list, function(x) c(x, rep(NA, max_length - length(x)))))

colnames(model_fin_all) =  c('FF5', 'Q5', 'FF5$^{F+B}$', 'Q5$^{F+B}$','M8')


### Asset Pricing (unselected candidate factors)

AP_result_remaining_in = R2_all_factor(Factor97_in, model_fin_all)
AP_result_remaining_in = AP_result_remaining_in[,c(5,6)]
AP_result_remaining_out = R2_all_factor_out(Factor97_in, Factor97_out, model_fin_all)

### Asset Pricing (285 basis portfolios)
AP_result_test_asset_in = R2_all_test_asset(Factor97_in, Fama285_in, model_fin_all)
AP_result_test_asset_in = AP_result_test_asset_in[,c(4,5)]

AP_result_test_asset_out = R2_all_test_asset_out(Factor97_in,Factor97_out, Fama285_in,Fama285_out, model_fin_all)

##### Investment performance
invest_result_in = investing_asset(Factor97_in,model_fin_all)
invest_result_in = invest_result_in[,2]
invest_result_out = investing_asset_out(Factor97_in,Factor97_out,model_fin_all)


result_all_in_out[1:5,c(2*(z-1)+1, 2*(z-1)+2)] = AP_result_remaining_in
result_all_in_out[1:5,c(2*(z-1)+7, 2*(z-1)+8)] = AP_result_test_asset_in
result_all_in_out[1:5,c(z+12)] = invest_result_in

result_all_in_out[6:10,c(2*(z-1)+1, 2*(z-1)+2)] = AP_result_remaining_out
result_all_in_out[6:10,c(2*(z-1)+7, 2*(z-1)+8)] = AP_result_test_asset_out
result_all_in_out[6:10,c(z+12)] = invest_result_out
}




colnames(result_all_in_out) = c('Total_R2','CS_R2','Total_R2','CS_R2','Total_R2','CS_R2',
                                'Total_R2','CS_R2','Total_R2','CS_R2','Total_R2','CS_R2',
                                "Ann.SR","Ann.SR","Ann.SR") 

rownames(result_all_in_out) = c(
  'FF5','Q5', 'FF5$^{F+B}$', 'Q5$^{F+B}$', 'M8', 
  'FF5','Q5', 'FF5$^{F+B}$', 'Q5$^{F+B}$', 'M8')


file1_in = paste('../output/Table5/Table5_performance_in_out', '.csv',sep = '')
write.csv(result_all_in_out,file1_in)

end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()
