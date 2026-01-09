
output_path <- "Table1"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

source('../fun_Section.R')

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))

names97 = rownames(Factor97)



S_CAPM = c("MKT")
S_FF3 = c("MKT","SMB","HML")
S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_FF6 = c(S_FF5,'UMD' )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" )
S_DHS3 = c('MKT', "PEAD", "FIN")
S_BS6 =  c("MKT","SMB","IA", "ROE", 'UMD',"HMLM")
  
model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)


model_fin_FSE =  list()
model_fin_BSE =  list()
for (m in 1:length(model_fin)) {

  bench_factor = unlist(model_fin[m])
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
  model_fin_FSE[[m]] = S_FSE
  
  S_BSE = BSE_stage_bench(Factor97, lambda = 0.05, S_FSE)
  model_fin_BSE[[m]] = S_BSE
}



merged_list <- c(model_fin, model_fin_FSE, model_fin_BSE)
max_length <- max(lengths(merged_list)) 
model_fin_all <- do.call(cbind, lapply(merged_list, function(x) c(x, rep(NA, max_length - length(x)))))

colnames(model_fin_all) =  c('CAPM','FF3', 'FF5', 'FF6','Q5', 'DHS3','BS6',
                             'CAPM$^F$','FF3$^F$', 'FF5$^F$','FF6$^F$', 'Q5$^F$','DHS3$^F$','BS6$^F$',
                             'CAPM$^{F+B}$','FF3$^{F+B}$','FF5$^{F+B}$','FF6$^{F+B}$','Q5$^{F+B}$','DHS3$^{F+B}$','BS6$^{F+B}$')


write.csv(model_fin_all,'Table1/Model_fin_all.csv')



model_fin_all1 = c(model_fin,model_fin_FSE)

result_p_all = matrix(,length(model_fin_all1),5)
  
  for (m in 1:length(model_fin_all1)) {
    
    S_factor = model_fin_all1[[m]]
    factor_RHS = Factor97[c(S_factor), , drop = FALSE]
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

write.csv(result_p_all, 'Table1/Table1.csv')












