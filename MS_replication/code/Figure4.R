############################################################################
# Figure 4: Reduced Factor Models (FF3^F Example)
# Note: This file generates intermediate results for Figure 4. 
# After running this script, execute the corresponding Figure4_plot.py file to create Figure 4
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
folder_name <- "../output/Figure4"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Figure4/Figure4_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Load supporting functions
source('fun_Section.R')

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

# Define FF3 benchmark factors
bench_factor = c('MKT', 'SMB', 'HML')

## FSE stage
S_FF3_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
## BSE stage
result_FF3_BSE = BSE_stage_bench_figure(Factor97, lambda = 0.05, S_FF3_FSE)

Factor_BSE =  result_FF3_BSE[[1]]
GRS = result_FF3_BSE[[2]]
Ann.SR = result_FF3_BSE[[3]]


m = seq(0, length(Factor_BSE),1)
BSE_report_figure = data.frame(cbind(m,GRS, Ann.SR))

rownames(BSE_report_figure) = c('FF3$^F$', Factor_BSE)

colnames(BSE_report_figure) = c("m","GRS","Ann.SR")

write.csv(BSE_report_figure, '../output/Figure4/Figure4.csv')

end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()

