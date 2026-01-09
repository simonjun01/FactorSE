
folder_name = "Figure4"
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

S_FF3_FSE = FSE_stage_bench(Factor97, lambda = 0.05, bench_factor)

result_FF3_BSE = BSE_stage_bench_figure(Factor97, lambda = 0.05, S_FF3_FSE)

Factor_BSE =  result_FF3_BSE[[1]]
GRS = result_FF3_BSE[[2]]
Ann.SR = result_FF3_BSE[[3]]


m = seq(0, length(Factor_BSE),1)
BSE_report_figure = data.frame(cbind(m,GRS, Ann.SR))

rownames(BSE_report_figure) = c('FF3$^F$', Factor_BSE)

colnames(BSE_report_figure) = c("m","GRS","Ann.SR")

write.csv(BSE_report_figure, 'Figure4/Figure4.csv')


