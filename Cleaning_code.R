
# Project

library(dplyr)
library(readr)
library(caret) 

dataset_raw <- read_csv("dataset_raw.csv",
                        col_types = cols(datadate = col_date(format = "%Y%m%d"),
                                         rdq = col_date(format ="%Y%m%d")))

# Cleaning of dataset

# Adding industry median prcc_f to replace NA values
dataset1 <- dataset_raw %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_prcc_f =median(prcc_f, na.rm=TRUE))

dataset2 <-dataset1 %>% mutate(prcc_f=ifelse(is.na(prcc_f), ind_prcc_f, prcc_f))

# Adding industry median mv_crsp to replace NA values 
dataset3 <- dataset2 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_mv_crsp =median(mv_crsp, na.rm=TRUE))

dataset4 <-dataset3 %>% mutate(mv_crsp=ifelse(is.na(mv_crsp), ind_mv_crsp, mv_crsp))

# Adding industry median e_score to replace NA values
dataset5 <- dataset4 %>% group_by(FFI12_desc) %>%
                    mutate(ind_e_score =median(e_score, na.rm=TRUE))

dataset6 <-dataset5 %>% mutate(e_score=ifelse(is.na(e_score), ind_e_score, e_score))




# Adding industry median act to replace NA values
dataset7 <- dataset6 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_act =median(act, na.rm=TRUE))

dataset8 <-dataset7 %>% mutate(act=ifelse(is.na(act), ind_act, act))


# Adding industry median capx to replace NA values
dataset9 <- dataset8 %>% group_by(FFI12_desc,fyear) %>%
                    mutate(ind_capx =median(capx, na.rm=TRUE))

dataset10 <-dataset9 %>% mutate(capx=ifelse(is.na(capx), ind_capx, capx))

# Adding industry median ceq to replace NA values
dataset11 <- dataset10 %>% group_by(FFI12_desc,fyear) %>%
                    mutate(ind_ceq =median(ceq, na.rm=TRUE))

dataset12 <-dataset11 %>% mutate(ceq=ifelse(is.na(ceq), ind_ceq, ceq))

# Adding industry median dlc to replace NA values
dataset13 <- dataset12 %>% group_by(FFI12_desc,fyear) %>%
                    mutate(ind_dlc =median(dlc, na.rm=TRUE))

dataset14 <-dataset13 %>%mutate(dlc =ifelse(is.na(dlc),ind_dlc,dlc))


#Adding industry median dltt to replace NA values
dataset15 <- dataset14 %>% group_by(FFI12_desc,fyear) %>%
                    mutate(ind_dltt =median(dltt, na.rm=TRUE))

dataset16 <-dataset15 %>% mutate(dltt = ifelse(is.na(dltt), ind_dltt, dltt))


#Adding industry median dp to replace NA values
dataset17 <- dataset16 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_dp =median(dp, na.rm=TRUE))

dataset18 <-dataset17 %>% mutate(dp = ifelse(is.na(dp), ind_dp, dp))

#Adding industry median dvc to replace NA values
dataset19 <- dataset18 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_dvc =median(dvc, na.rm=TRUE))

dataset20 <-dataset19 %>% mutate(dvc = ifelse(is.na(dvc), ind_dvc, dvc))


#Adding industry median emp to replace NA values
dataset21 <- dataset20 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_emp =median(emp, na.rm=TRUE))

dataset22 <-dataset21 %>% mutate(emp = ifelse(is.na(emp), ind_emp, emp))

#Adding industry median gdwl to replace NA values
dataset23 <- dataset22 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_gdwl =median(gdwl, na.rm=TRUE))

dataset24 <-dataset23 %>% mutate(gdwl = ifelse(is.na(gdwl), ind_gdwl, gdwl))

#Adding industry median intan to replace NA values
dataset25 <- dataset24 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_intan =median(intan, na.rm=TRUE))

dataset26 <-dataset25 %>% mutate(intan = ifelse(is.na(intan), ind_intan, intan))


#Adding industry median invt to replace NA values
dataset27 <- dataset26 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_invt =median(invt, na.rm=TRUE))

dataset28 <-dataset27 %>% mutate(invt = ifelse(is.na(invt), ind_invt, invt))

#Adding industry median lct to replace NA values
dataset29 <- dataset28 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_lct =median(lct, na.rm=TRUE))

dataset30 <-dataset29 %>% mutate(lct = ifelse(is.na(lct), ind_lct, lct))

#Adding industry median lt to replace NA values
dataset31 <- dataset30 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_lt =median(lt, na.rm=TRUE))

dataset32 <-dataset31 %>% mutate(lt = ifelse(is.na(lt), ind_lt, lt))

#Adding industry median oancf to replace NA values
dataset33 <- dataset32 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_oancf =median(oancf, na.rm=TRUE))

dataset34 <-dataset33 %>% mutate(oancf = ifelse(is.na(oancf), ind_oancf, oancf))

#Adding industry median ppegt to replace NA values
dataset35 <- dataset34 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_ppegt =median(ppegt, na.rm=TRUE))

dataset36 <-dataset35 %>% mutate(ppegt = ifelse(is.na(ppegt), ind_ppegt, ppegt))

#Adding industry median ppent to replace NA values
dataset37 <- dataset36 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_ppent =median(ppent, na.rm=TRUE))

dataset38 <-dataset37 %>% mutate(ppent = ifelse(is.na(ppent), ind_ppent, ppent))

#Adding industry median re to replace NA values
dataset39 <- dataset38 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_re =median(re, na.rm=TRUE))

dataset40 <-dataset39 %>% mutate(re = ifelse(is.na(re), ind_re, re))

#Adding industry median rect to replace NA values
dataset41 <- dataset40 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_rect =median(rect, na.rm=TRUE))

dataset42 <-dataset41 %>% mutate(rect = ifelse(is.na(rect), ind_rect, rect))

#Adding industry median sstk to replace NA values
dataset43 <- dataset42 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_sstk =median(sstk, na.rm=TRUE))

dataset44 <-dataset43 %>% mutate(sstk = ifelse(is.na(sstk), ind_sstk, sstk))

#Adding industry median txdb to replace NA values
dataset45 <- dataset44 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_txdb =median(txdb, na.rm=TRUE))

dataset46 <-dataset45 %>% mutate(txdb = ifelse(is.na(txdb), ind_txdb, txdb))

#Adding industry median txp to replace NA values
dataset47 <- dataset46 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_txp =median(txp, na.rm=TRUE))

dataset48 <-dataset47 %>% mutate(txp = ifelse(is.na(txp), ind_txp, txp))

#Adding industry median xad to replace NA values
dataset49 <- dataset48 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_xad =median(xad, na.rm=TRUE))

dataset50 <-dataset49 %>% mutate(xad = ifelse(is.na(xad), ind_xad, xad))


#Adding industry median xlr to replace NA values
dataset51 <- dataset50 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_xlr =median(xlr, na.rm=TRUE))

dataset52 <-dataset51 %>% mutate(xlr = ifelse(is.na(xlr), ind_xlr, xlr))



#Adding industry median xpr to replace NA values
dataset53 <- dataset52 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_xpr =median(xpr, na.rm=TRUE))

dataset54 <-dataset53 %>% mutate(xpr = ifelse(is.na(xpr), ind_xpr, xpr))

#Adding industry median xrd to replace NA values
dataset55 <- dataset54 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_xrd =median(xrd, na.rm=TRUE))

dataset56 <-dataset55 %>% mutate(xrd = ifelse(is.na(xrd), ind_xrd, xrd))



#Adding industry median xsga to replace NA values
dataset57 <- dataset56 %>% group_by(FFI12_desc,fyear) %>%
  mutate(ind_xsga =median(xsga, na.rm=TRUE))

dataset58 <-dataset57 %>% mutate(xsga = ifelse(is.na(xsga), ind_xsga, xsga))

dataset_final <- write.csv(dataset58, 'updated.csv',row.names=FALSE)



