

source('../fun_Section.R')

folder_name = "Table9"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)
Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

Factor_RPPCA5 = read.csv('../data/RP-PCA/RPPCA_Factor.csv')
Factor_RPPCA5 = as.matrix(t(Factor_RPPCA5))

Factor_RRA5 = read.csv('../data/RRA/RRA_Factor.csv')
Factor_RRA5 = as.matrix(t(Factor_RRA5))


model_fin_all = read.csv('Table8/Table8.csv')
model_fin_all = model_fin_all[,c(-1)]

model_names = c('RPPCA','RRA','RPPCA','RRA','FGX2020','KNS2020','FGX2020','KNS2020')

### Asset Pricing (unselected candidate factors)
AP_result_remaining = R2_all_factor_new(Factor97,Factor_RPPCA5,Factor_RRA5,model_fin_all)

### Asset Pricing (285 basis portfolios)
AP_result_test_asset = R2_all_test_asset_new(Factor97,Factor_RPPCA5,Factor_RRA5, Fama285,model_fin_all)

### investing performance 
invest_result_all = investing_asset_new(Factor97,Factor_RPPCA5,Factor_RRA5,model_fin_all)


result_all = cbind(AP_result_remaining,AP_result_test_asset,invest_result_all)

colnames(result_all) = c('#Factor',
                         'Aalpha','Atalpha','sign2',
                         'Total_R2','CS_R2',
                         'Aalpha','Atalpha','sign2',
                         'Total_R2','CS_R2',
                         "AVG","Ann.SR",'alpha_CAPM',"alpha_FF5") 

rownames(result_all) = c(
  'RP-PCA','RRA','RP-PCA$^{F+B}$','RRA$^{F+B}$',
  'FGX2020', 'KNS2020', 'FGX2020$^{F+B}$', 'KNS2020$^{F+B}$')

result_all  = t(result_all)
write.csv(result_all,'Table9/Table9.csv')


