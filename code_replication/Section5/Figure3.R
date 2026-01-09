

folder_name = "Figure3"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}


source('../fun_Section.R')

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)
Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

S_CAPM = c("MKT")
S_FF3 = c("MKT","SMB","HML")
S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_FF6 = c(S_FF5,'UMD' )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" ) 
S_DHS3 = c('MKT', "PEAD", "FIN")
S_BS6 =  c("MKT","SMB","IA", "ROE" , 'UMD',"HMLM" )
  

model_fin_all = read.csv('../Section4/Table1/Model_fin_all.csv')
model_fin_all = model_fin_all[,-1]
model_fin_all_FSE = model_fin_all[,8:14]

model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)
factor_model = c('CAPM','FF3','FF5','FF6','Q5','DHS3','BS6')
factor_model_FSE = c('CAPM$^F$','FF3$^F$','FF5$^F$','FF6$^F$','Q5$^F$','DHS3$^F$','BS6$^F$')

  
BSE_Path_all = matrix(,nrow(model_fin_all_FSE),ncol(model_fin_all_FSE))
for (m in 1:length(model_fin_all_FSE)) {

  m1 = unlist(model_fin[m])
  S_FSE = na.omit(model_fin_all_FSE[,m])
  
  S_BSE_path =  BSE_stage_bench_path(Factor97, lambda = 0.05, S_FSE)
  BSE_Path_all[1:length(S_BSE_path),m] = S_BSE_path

}

BSE_Path_all = rbind(factor_model_FSE,BSE_Path_all)
BSE_Path_all <- BSE_Path_all[!apply(is.na(BSE_Path_all), 1, all), ]


model_BSE = matrix(,(nrow(BSE_Path_all)),ncol(BSE_Path_all))
GRS_BSE = matrix(,nrow(model_BSE),ncol(model_BSE))
SR_BSE = matrix(,nrow(model_BSE),ncol(model_BSE))
HDA_BSE = matrix(,nrow(model_BSE),ncol(model_BSE))

for (m in 1:ncol(BSE_Path_all)) {

  
  BSE_Path_all_m = na.omit(BSE_Path_all[,m])

  
  for (j in 1:(length(BSE_Path_all_m))) {
    
    if (j == 1){
      model_BSE[j,m] = factor_model_FSE[m]
      Sj = na.omit(model_fin_all_FSE[,m])

    
    } else {
      model_BSE[j,m] = c(BSE_Path_all_m[j])
      Sj = setdiff(na.omit(model_fin_all_FSE[,m]),BSE_Path_all_m[2:(j)])
   
    }

    
    factor_RHS = Factor97[c(Sj), , drop = FALSE]
    factor_LHS = Factor97[!rownames(Factor97) %in% c(Sj),, drop = FALSE]
    
    F1 = t(factor_RHS)
    F2 = t(factor_LHS)
    result1 = GRS_test(F1, F2)
    result2 = HDA_test(F1, F2)
    
    GRS_BSE[j,m] = result1[[2]]
    HDA_BSE[j,m] = judge.star_HDA(result2[[1]])

    W = solve(cov(F1))
    mu = matrix(colMeans(F1))
    SR = sqrt(t(mu)%*%W%*%mu)
    SR = SR*sqrt(12) 
    SR_BSE[j,m] = SR
  }
}


colnames(model_BSE) = colnames(GRS_BSE) = colnames(SR_BSE) = 
  colnames(HDA_BSE)  = c('CAPM','FF3', 'FF5', 'FF6','Q5', 'DHS3','BS6')


write.csv(model_BSE, 'Figure3/Figure3_model_BSE.csv')
write.csv(GRS_BSE, 'Figure3/Figure3_GRS_BSE.csv')
write.csv(SR_BSE, 'Figure3/Figure3_SR_BSE.csv')
write.csv(HDA_BSE, 'Figure3/Figure3_HDA_BSE.csv')

