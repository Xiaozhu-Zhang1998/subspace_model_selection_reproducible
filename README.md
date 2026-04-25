## Guidelines for Reproducing the Experiments in the FSSS Paper

### Figure 1
Run the file `1_teaser.R`



### Table 1
- For L0 and stability methods using l0 base procedure: Run the file `2_synthetic_l0.R` to obtain `RS_Syn_l0.RDS`
- For L1 and stability methods using l1 base procedure: Run the file `2_synthetic_lasso.R` to obtain `RS_Syn_lasso.RDS`
- Run the sections "l0 base procedure" and "lasso base procedure" in file `2_synthetic_getResults.R`

### Figure 2 
Run the section "Focus on one dataset and implement FSSS" in file `2_synthetic_getResults.R`

### Figure 3
- Run the file `3_realdata_l0.R` to obtain `RS_realdata.RDS`
- Run the section "Figure 4" in file `3_realdata_getResults.R`
- 
