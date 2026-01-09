

source('../fun_Section.R')

folder_name = "Table6"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}


Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)


names97_MKT = names97[-1]


result97_MKT = matrix(,length(names97_MKT), length(names97_MKT))

for (s in 1:length(names97_MKT)) {
  
  bench_factor = c('MKT',names97_MKT[s])
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
 
  bench_factor_BSE = S_FSE
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

write.csv(result_all97_MKT,'Table6/Table6.csv')









