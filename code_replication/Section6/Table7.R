

source('../fun_Section.R')

folder_name = "Table7"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)


names97_FF3 = names97[-c(1:3)]

result97_FF3 = matrix(,length(names97_FF3), length(names97_FF3))

for (s in 1:length(names97_FF3)) {

  bench_factor = c('MKT','SMB','HML',names97_FF3[s])
  S_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)
  
  bench_factor_BSE = S_FSE
  S_BSE = BSE_stage_bench(Factor97, lambda = 0.05, bench_factor_BSE)
  result97_FF3[1:length(S_BSE),s] = S_BSE
  
}

colnames(result97_FF3) = names97_FF3


bench_factor = c('MKT')
S_FSE = FSE_stage_bench(Factor97, lambda = 0.05,bench_factor)
M8 =  BSE_stage_bench(Factor97, lambda = 0.05,S_FSE)


result_all97_FF3 = matrix(,nrow(Factor97), 3)
result_all97_1_FF3  = matrix(0,nrow(Factor97), ncol(result97_FF3 ))
for (i in 1:ncol(result97_FF3)) {
  result97_FF3i = result97_FF3[,i]
  result97_FF3i = result97_FF3i[!is.na(result97_FF3i)]
  
  result_all97_FF3[i+3, 1] <- as.numeric(length(intersect(result97_FF3i, names97[i+3])) > 0)
  
  result_all97_FF3[i+3,2] = as.numeric(identical(sort(result97_FF3i), sort(M8)))
  
  result_all97_1_FF3[c(which(names97 %in% result97_FF3i)),i] = 1
  
  
}

result_all97_FF3[,3] = round(rowMeans(result_all97_1_FF3),2)

rownames(result_all97_FF3) = names97
result_all97_FF3 = cbind(seq(1,length(names97),1),result_all97_FF3)
colnames(result_all97_FF3) = c('ID','Selected','Same','Rate')

write.csv(result_all97_FF3,'Table7/Table7.csv')

