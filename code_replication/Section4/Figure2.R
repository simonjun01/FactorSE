
folder_name <- "Figure2"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}


source('../fun_Section.R')


Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

bench_factor = c('MKT', 'SMB', 'HML')
result_FF3_FSE = FSE_stage_bench_figure(Factor97, lambda = 0.05, bench_factor)
Factor_FSE =  result_FF3_FSE[[1]]
GRS = result_FF3_FSE[[2]]
Ann.SR = result_FF3_FSE[[3]]

model_fin1 = Factor_FSE[-c(1:3)]

m = seq(0, length(names97)-length(bench_factor),1)
GRS = c(GRS,0)
Ann.SR = c(Ann.SR, sqrt(calc_Max_SR2(t(Factor97)))*sqrt(12))

data = data.frame(cbind(m,GRS, Ann.SR))

colnames(data) = c("m","GRS","Ann.SR")
rownames(data) = c('FF3', Factor_FSE[-c(1:3)],setdiff(names97, Factor_FSE))


write.csv(data, 'Figure2/Figure2.csv')


