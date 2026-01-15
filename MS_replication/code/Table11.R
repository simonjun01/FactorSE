############################################################################
# Table 11: Reduced Factor Models with Transaction Costs (CAPM Example)
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table11"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table11/Table11_run.log", split = TRUE)
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

Factor_des = read.csv('../data/Factor_des.csv')

cost_annually = c(0.0002,0.0002)
cost_monthly = c(0.0012,0.0024)
cost_name = c('12bps','24bps')

Factor97_gross = Factor97
model_fin_BSE =  list()
for (c in 1:length(cost_annually)) {

names97_cost = names97
Factor97_net = Factor97_gross
for (i in 1:nrow(Factor97_gross)) {
  cost_label = Factor_des[i,3]
  if (cost_label == 1){ ## annually 
    Factor97_net[i,] = Factor97_gross[i,] - cost_annually[c]
    names97_cost[i] = paste('underline{',names97[i],'}',sep = '')
  }
  if (cost_label == 3){ ## monthly
    Factor97_net[i,] = Factor97_gross[i,] - cost_monthly[c]
  }
 
}

S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" ) 

model_fin = list(S_FF5,S_Q5)

for (m in 1:length(model_fin)) {

  bench_factor = unlist(model_fin[m])
  
  S_FSE = FSE_stage_bench(Factor97_net, lambda = 0.05,bench_factor)
  S_BSE = BSE_stage_bench(Factor97_net, lambda = 0.05,S_FSE)
  
  BSE_index = NULL
  for (i in 1:length(S_BSE)) {
    BSE_index = c(BSE_index, which(names97 == S_BSE[i]))
  }
  
  model_fin_BSE[[m+2*(c-1)]] = names97_cost[c(BSE_index)]

}


}

S_M8 =  c('MKT','REG','PEAD','HMLM','STR','ILR','SMB','EPRD')
S_M8_index = NULL
for (i in 1:length(S_M8)) {
  S_M8_index = c(S_M8_index, which(names97 == S_M8[i]))
}
S_M8_BSE = names97_cost[c(S_M8_index)]


merged_list <- c(list(S_M8_BSE),  model_fin_BSE)

max_length <- max(lengths(merged_list))  
model_fin_all <- do.call(cbind, lapply(merged_list, function(x) c(x, rep(NA, max_length - length(x)))))

colnames(model_fin_all) =  c('M8','FF5$^{F+B}$', 'Q5$^{F+B}$',
                             'FF5$^{F+B}$', 'Q5$^{F+B}$')

write.csv(model_fin_all,'../output/Table11/Table11.csv')


end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()