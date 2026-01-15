
############################################################################
# Table 2: Expanded Factor Models (FSE Process)
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table2"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table2/Table2_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")


# Load user-defined functions used in the analysis
# (e.g., FSE_stage_bench, BSE_stage_bench, judge.star)
source('fun_Section.R')

# Load the 97-factor return data
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

result_FSE_span = matrix(, max(lengths(model_fin_FSE))+ 3,(length(model_fin)))

rownames(result_FSE_span) = c(seq(1,max(lengths(model_fin_FSE)) ), '# M','# M$^F$','# Added')
colnames(result_FSE_span) =  c('CAPM$^F$','FF3$^F$', 'FF5$^F$','FF6$^F$', 'Q5$^F$','DHS3$^F$','BS6$^F$')


## Factor-level spanning (alpha) tests
for (m in 1:length(model_fin_FSE)) {
  
  # Benchmark and FSE-expanded factor sets
  bench_factor = unlist(model_fin[m])
  FSE_factor = unlist(model_fin_FSE[m])
  
  # Factors newly added by FSE
  add_factor = setdiff(FSE_factor,bench_factor)
  
  factor_RHS_bench = Factor97[c(bench_factor),, drop = FALSE]
  factor_RHS_add = Factor97[c(add_factor),, drop = FALSE]
  
  # Record model sizes
  result_FSE_span[max(lengths(model_fin_FSE)) + 1,m] = length(bench_factor)
  result_FSE_span[max(lengths(model_fin_FSE)) + 2,m] = length(FSE_factor)
  result_FSE_span[max(lengths(model_fin_FSE)) + 3,m] = length(add_factor)
  
  result_FSE_span[1:length(bench_factor),m] = bench_factor

    # length(add_factor)
  # Time-series regressions:
  # added factor ~ benchmark factors
  for (i in 1:nrow(factor_RHS_add)) {

    # Row index corresponding to this factor
    factor_RHS_addi = factor_RHS_add[i,, drop = FALSE]
    lm1 = lm(t(factor_RHS_addi) ~ t(factor_RHS_bench))

    coeff_alpha = summary(lm1)$coefficients[,'Estimate'][1]*100
    p_alpha = summary(lm1)$coefficients[,'Pr(>|t|)'][1]
    result_FSE_span[length(bench_factor)+i,(m)] = paste0(add_factor[i], '(',sprintf("%0.2f", coeff_alpha) , judge.star(p_alpha),')')

  }
  
}

write.csv(result_FSE_span,'../output/Table2/Table2.csv')


end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()
