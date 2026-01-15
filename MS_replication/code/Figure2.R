############################################################################
# # Figure 2: Expanded Factor Models (FF3 Example)
# Note: This file generates intermediate results for Figure 2. 
# After running this script, execute the corresponding Figure2_plot.py file to create Figure 2
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
folder_name <- "../output/Figure2"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Figure2/Figure2_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Load supporting functions
source('fun_Section.R')

# Load the 97-factor return data
Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

# Define FF3 benchmark factors
bench_factor = c('MKT', 'SMB', 'HML')
result_FF3_FSE = FSE_stage_bench_figure(Factor97, lambda = 0.05, bench_factor)
Factor_FSE =  result_FF3_FSE[[1]]
GRS = result_FF3_FSE[[2]]
Ann.SR = result_FF3_FSE[[3]]

model_fin1 = Factor_FSE[-c(1:3)]

m = seq(0, length(names97)-length(bench_factor),1)
GRS = c(GRS,0)
Ann.SR = c(Ann.SR, sqrt(calc_Max_SR2(t(Factor97)))*sqrt(12))

data = data.frame(cbind(m,GRS, Ann.SR))

colnames(data) = c("m","GRS","Ann.SR")
rownames(data) = c('FF3', Factor_FSE[-c(1:3)],setdiff(names97, Factor_FSE))


write.csv(data, '../output/Figure2/Figure2.csv')

end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()
