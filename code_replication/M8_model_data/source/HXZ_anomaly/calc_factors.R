

df_ilr = read.csv('data/portf_ilr_1_monthly_2021.csv')
df_eprd = read.csv('data/portf_eprd_monthly_2021.csv')

factor.calc <- function(df){
df_name = unique(df[,c(1,2)])
df_name = as.matrix(df_name)

rank = max(df[,3])

df1 = as.matrix(df[,5])/100
df1 = t(matrix(df1,rank,nrow(df1)/rank))
df2 = df1[,rank] -  df1[,1]

df_month = cbind(df_name,as.matrix(df2))

df_month = as.matrix(df_month)
if (mean(df_month[,3]) < 0) {
  df_month[,3] = -1*df_month[,3]
}
colnames(df_month) = c('year', 'month', 'return')
return(df_month)
}

ilr = data.frame(factor.calc(df_ilr))

write.csv(ilr, 'result/ilr.csv')

eprd = data.frame(factor.calc(df_eprd))

write.csv(eprd, 'result/eprd.csv')

