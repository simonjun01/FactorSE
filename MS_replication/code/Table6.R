############################################################################
# Table 6: Testing Individual Factors (Baseline = MKT + Row Factor)
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table6"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table6/Table6_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Load supporting functions
source('fun_Section.R')

# Load 97 factor returns
Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)
names97_MKT = names97[-1]


result97_MKT = matrix(,length(names97_MKT), length(names97_MKT))

for (s in 1:length(names97_MKT)) {
  cat("Processing factor", s, "of", length(names97_MKT), "\n")
  
  # Current benchmark: MKT + one specific factor
  bench_factor = c('MKT',names97_MKT[s])
  
  # Forward Stepwise Evaluation (FSE)
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
 
  bench_factor_BSE = S_FSE
  # Backward Stepwise Evaluation (BSE)
  S_BSE = BSE_stage_bench(Factor97, lambda = 0.05, bench_factor_BSE)
  result97_MKT[1:length(S_BSE),s] = S_BSE

}

colnames(result97_MKT) = names97_MKT

bench_factor = c('MKT')
S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
M8 =  BSE_stage_bench(Factor97, lambda = 0.05, S_FSE)


result_all97_MKT = matrix(,nrow(Factor97), 3)
result_all97_1_MKT  = matrix(0,nrow(Factor97), ncol(result97_MKT ))


for (i in 1:ncol(result97_MKT)) {
  result97_MKTi = result97_MKT[,i]
  result97_MKTi = result97_MKTi[!is.na(result97_MKTi)]
  
  result_all97_MKT[i+1,1] <- as.numeric(length(intersect(result97_MKTi, names97[i+1])) > 0)
  
  result_all97_MKT[i+1,2] = as.numeric(identical(sort(result97_MKTi), sort(M8))) 

  result_all97_1_MKT[c(which(names97 %in% result97_MKTi)),i] = 1
}

result_all97_MKT[,3] = round(rowMeans(result_all97_1_MKT),2)

rownames(result_all97_MKT) = names97
result_all97_MKT = cbind(seq(1,length(names97),1),result_all97_MKT)
colnames(result_all97_MKT) = c('ID','Selected','Same','Rate')

write.csv(result_all97_MKT,'../output/Table6/Table6.csv')



end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()



