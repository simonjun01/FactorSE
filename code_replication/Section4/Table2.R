
output_path <- "Table2"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

source('../fun_Section.R')

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

model_fin_all = read.csv('./Table1/Model_fin_all.csv')
model_fin_all = model_fin_all[,-1]

model_fin = model_fin_FSE = list() 

for (col in 1:7) {
  non_zero_values <- as.vector(na.omit(model_fin_all[,col] ))
  model_fin[[col]] <- non_zero_values 
  non_zero_values <- as.vector(na.omit(model_fin_all[,col+7] ))
  model_fin_FSE[[col]] <- non_zero_values 

}

S_FSE_all <- unlist(model_fin_FSE)
S_FSE_all = unique(S_FSE_all)

result_FSE_span = matrix(,length(S_FSE_all) + 3,(length(model_fin)))

rownames(result_FSE_span) = c(S_FSE_all, '# M','# M$^F$','# Added')
colnames(result_FSE_span) =  c('CAPM$^F$','FF3$^F$', 'FF5$^F$','FF6$^F$', 'Q5$^F$','DHS3$^F$','BS6$^F$')

for (m in 1:length(model_fin_FSE)) {
  
  bench_factor = unlist(model_fin[m])
  FSE_factor = unlist(model_fin_FSE[m])
  add_factor = setdiff(FSE_factor,bench_factor)
  
  factor_RHS_bench = Factor97[c(bench_factor),, drop = FALSE]
  factor_RHS_add = Factor97[c(add_factor),, drop = FALSE]
  
  result_FSE_span[length(S_FSE_all) + 1,m] = length(bench_factor)
  result_FSE_span[length(S_FSE_all) + 2,m] = length(FSE_factor)
  result_FSE_span[length(S_FSE_all) + 3,m] = length(add_factor)
  
  for (i in 1:nrow(factor_RHS_add)) {

    bb = which(S_FSE_all %in% add_factor[i])
    factor_RHS_addi = factor_RHS_add[i,, drop = FALSE]
    lm1 = lm(t(factor_RHS_addi) ~ t(factor_RHS_bench))

    coeff_alpha = summary(lm1)$coefficients[,'Estimate'][1]*100
    p_alpha = summary(lm1)$coefficients[,'Pr(>|t|)'][1]
    result_FSE_span[bb,(m)] = paste0(sprintf("%0.2f", coeff_alpha) , judge.star(p_alpha))
    
  }
  
}


write.csv(result_FSE_span,'Table2/Table2.csv')



