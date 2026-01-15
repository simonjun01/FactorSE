
############################################################################
# Table 4: Reduced Factor Models (BSE Process)
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table4"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table4/Table4_run.log", split = TRUE)
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
S_BS6 =  c("MKT","SMB","IA", "ROE", 'UMD',"HMLM")

model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)

# Containers for expanded models
model_fin_FSE =  list()
model_fin_BSE =  list()
for (m in 1:length(model_fin)) {
  print(m)
  # Current benchmark model
  bench_factor = unlist(model_fin[m])
  
  # Forward Selection Expansion (FSE)
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
  model_fin_FSE[[m]] = S_FSE
  
  # Backward Selection Expansion (BSE)
  S_BSE = BSE_stage_bench(Factor97, lambda = 0.05, S_FSE)
  model_fin_BSE[[m]] = S_BSE
  
  
}

max_length <- max(lengths(model_fin_FSE))
model_fin_FSE <- do.call(cbind, lapply(model_fin_FSE, function(x) c(x, rep(NA, max_length - length(x)))))
max_length <- max(lengths(model_fin_BSE))
model_fin_BSE <- do.call(cbind, lapply(model_fin_BSE, function(x) c(x, rep(NA, max_length - length(x)))))

model_fin_BSE = as.matrix(na.omit(model_fin_BSE))
model_fin_num = matrix(,3,ncol(model_fin_BSE))
for (m in 1:ncol(model_fin_BSE)) {
  print(m)
  FSE_factor = na.omit(model_fin_FSE[,m])
  BSE_factor = na.omit(model_fin_BSE[,m])
  removed_factor = setdiff(FSE_factor,BSE_factor)
  model_fin_num[,m] = c(length(FSE_factor),length(BSE_factor),length(removed_factor))
}


model_fin_BSE_report = rbind(model_fin_BSE,model_fin_num) 
  
colnames(model_fin_BSE_report) =  c('CAPM$^{F+B}$','FF3$^{F+B}$','FF5$^{F+B}$','FF6$^{F+B}$','Q5$^{F+B}$','DHS3$^{F+B}$','BS6$^{F+B}$')
rownames(model_fin_BSE_report) = c(seq(1,8,1),'# M$^F$', '# M^{F+B}','# Removed')



write.csv(model_fin_BSE_report,'../output/Table4/Table4.csv')



end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()
