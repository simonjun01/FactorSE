
output_path = "Table4"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}



model_fin_all = read.csv('../Section4/Table1/Model_fin_all.csv')
model_fin_all = model_fin_all[,-1]

model_fin_FSE = model_fin_all[,c(8:14)]
model_fin_BSE = model_fin_all[,c(15:21)]
model_fin_BSE = as.matrix(na.omit(model_fin_BSE))
model_fin_num = matrix(,3,ncol(model_fin_BSE))
for (m in 1:ncol(model_fin_BSE)) {
  
  FSE_factor = na.omit(model_fin_FSE[,m])
  BSE_factor = na.omit(model_fin_BSE[,m])
  removed_factor = setdiff(FSE_factor,BSE_factor)
  model_fin_num[,m] = c(length(FSE_factor),length(BSE_factor),length(removed_factor))
}


model_fin_BSE_report = rbind(model_fin_BSE,model_fin_num) 
  
colnames(model_fin_BSE_report) =  c('CAPM$^{F+B}$','FF3$^{F+B}$','FF5$^{F+B}$','FF6$^{F+B}$','Q5$^{F+B}$','DHS3$^{F+B}$','BS6$^{F+B}$')
rownames(model_fin_BSE_report) = c(seq(1,8,1),'# M$^F$', '# M^{F+B}','# Removed')



write.csv(model_fin_BSE_report,'Table4/Table4.csv')



