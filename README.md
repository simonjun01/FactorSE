
# FactorSE

This repository contains the replication code for Feng, Lan, Wang, and Zhang (Management Science, Forthcoming),  
**Selecting and Testing Asset Pricing Models: A Stepwise Approach**.

---

## How to Replicate the Results

### 1. For Linux Users
- In the command line, navigate to the directory: `cd ./MS_replication/`.
- Run the script: `sh submit.sh`. 
- The authors ran the code on an Intel(R) Xeon(R) CPU E5-2650 v4. It takes approximately **3 minutes** to complete all results.

### 2. For Windows/Mac Users
- Set the working directory to the  `./MS_replication/code/`.
- Run the R scripts individually in each folder to obtain the corresponding results.

---


## Details of the 8-Factor Model (M8)

Applying our proposed stepwise evaluation approach, we select **eight factors**, referred to as the **M8 model**, which are summarized below.

### 1. Description of the M8 Model
- **MKT**: Excess market return  
- **REG**: Expected growth  
- **PEAD**: Post-earnings announcement drift  
- **HMLM**: HML Devil  
- **STR**: Short-term reversal on prior (1-1)  
- **ILR**: Industry lead-lag effect in prior returns  
- **SMB**: Small minus big  
- **EPRD**: Earnings predictability  

### 2. Sources for the M8 Model

#### (1) MKT, SMB, STR
- **Website:**  
  https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
- **MKT, SMB**: Fama/French 3 Factors 
- **STR**: Short-Term Reversal Factor (ST Rev)

#### (2) HMLM
- **Website:**  
  https://www.aqr.com/Insights/Datasets/The-Devil-in-HMLs-Details-Factors-Monthly
- **HMLM**: The Devil in HML's Details: Factors, Monthly

#### (3) REG
- **Website:**  
  https://global-q.org/factors.html
- **REG**: Expected growth factor

#### (4) PEAD
- **Website:**  
  https://sites.google.com/view/linsunhome
- **PEAD**: PEAD factor returns

#### (5) ILR, EPRD
- **Website:**  
  https://global-q.org/testingportfolios.html
- **ILR**: 1-way sorts:  Ilr1 ("ilr_1"), industry lead-lag effect in prior returns, 1-month holding period
- **EPRD**: 1-way sorts: Eprd ("eprd"), earnings predictability

- **Calculation Method:**  
1-way sorts, take max quantile minus min quantile. If the calculated factor return is negative, we need to adjust the sign (multiply by -1) to make it a positive-return factor.

**Note**: To ensure consistency with the published version, the data provided here span the period 1973-2021. Readers may update the sample period in accordance with the guidelines.
---

## Reference

- Welcome to cite our paper.
- Working paper version: [Selecting and Testing Asset Pricing Models: A Stepwise Approach](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4502439)



```bibtex
@article{feng2026selecting,
  title   = {Selecting and Testing Asset Pricing Models: A Stepwise Approach},
  author  = {Feng, Guanhao and Lan, Wei and Wang, Hansheng and Zhang, Jun},
  journal = {Management Science, Forthcoming},
  year    = {2026}
}
```

## Contact

For questions or comments regarding the replication code, please contact:

- **Jun Zhang**
- Email: junzhang2025@seu.edu.cn
