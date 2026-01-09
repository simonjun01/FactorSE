

folder_name <- "Figure1"
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
  

model_fin_all = read.csv('./Table1/Model_fin_all.csv')
model_fin_all = model_fin_all[,-1]
model_fin_all_FSE = model_fin_all[,8:14]

model_fin = list(S_CAPM,S_FF3, S_FF5,S_FF6, S_Q5,S_DHS3,S_BS6)


model_FSE = matrix(,nrow(model_fin_all_FSE),ncol(model_fin_all_FSE))

factor_model = c('CAPM','FF3','FF5','FF6','Q5','DHS3','BS6')
for (i in 1:length(model_fin)) {
  model_fin_all_FSE1 = na.omit(model_fin_all_FSE[,i])
  model2 = setdiff(model_fin_all_FSE1, unlist(model_fin[i]))
  model_FSE[1:(length(model2)+1),i] = c(factor_model[i],model2) 
}

model_FSE <- model_FSE[!apply(is.na(model_FSE), 1, all), ]


GRS_FSE = matrix(,nrow(model_FSE),ncol(model_FSE))
SR_FSE = matrix(,nrow(model_FSE),ncol(model_FSE))
HDA_FSE = matrix(,nrow(model_FSE),ncol(model_FSE))

for (m in 1:length(model_fin)) {
  model_fin_all_FSE1 = na.omit(model_fin_all_FSE[,m])
  model2 = setdiff(model_fin_all_FSE1, unlist(model_fin[m]))
  
  for (j in 1:(length(model2)+1)) {
  Sj = model_fin_all_FSE1[1:(length(unlist(model_fin[m])) + j -1)]

factor_RHS = Factor97[c(Sj), , drop = FALSE] 
factor_LHS = Factor97[!rownames(Factor97) %in% c(Sj),, drop = FALSE]

F1 = t(factor_RHS)
F2 = t(factor_LHS)
result1 = GRS_test(F1, F2)
result2 = HDA_test(F1, F2)

GRS_FSE[j,m] = result1[[2]]
HDA_FSE[j,m] = judge.star_HDA(result2[[1]])
## SR
SR = sqrt(calc_Max_SR2(F1))
SR = SR*sqrt(12)
SR_FSE[j,m] = SR
}
}

colnames(model_FSE) = colnames(GRS_FSE) = colnames(SR_FSE) = 
colnames(HDA_FSE)  = c('CAPM','FF3', 'FF5', 'FF6','Q5', 'DHS3','BS6')
                            
write.csv(model_FSE, 'Figure1/Figure1_model_FSE.csv')
write.csv(GRS_FSE, 'Figure1/Figure1_GRS_FSE.csv')
write.csv(SR_FSE, 'Figure1/Figure1_SR_FSE.csv')
write.csv(HDA_FSE, 'Figure1/Figure1_HDA_FSE.csv')


