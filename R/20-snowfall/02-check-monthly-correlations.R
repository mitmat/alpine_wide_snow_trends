# check monthly correlations (-> for input to gapfilling/reconstruction)

library(data.table)
library(magrittr)
library(fs)
library(foreach)

mitmatmisc::init_parallel_ubuntu(4)

min_years <- 10

# monthly -----------------------------------------------------------------

dat_meta_monthly <- unique(rbind(readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/meta_long_HN_HS.rds"),
                                 readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/meta_long_HN_HS.rds")))

dat_monthly <- rbind(readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/data_long_HN_HS.rds"),
                     readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/data_long_HN_HS.rds"))
dat_monthly[, HS := NULL]

dat_n <- dat_monthly[, .(n_avail = sum(!is.na(HN))), .(Name, month)]
dat_n <- dat_n[n_avail >= min_years]

dat_corr <- foreach(
  i = 1:nrow(dat_n),
  .final = rbindlist,
  .inorder = F
) %dopar% {
  
  dat_i <- dat_n[i] %>% merge(dat_monthly)

  stations <- dat_meta_monthly$Name
  stations <- stations[stations != dat_n[i, Name]]
  
  dat_i_cor <- foreach(
    stn = stations,
    .final = rbindlist
  ) %do% {
    
    dat_ref <- dat_monthly[Name == stn & month == dat_n[i, month]]
    dat_i2 <- dat_i %>% 
      merge(dat_ref[, .(year, month, HN_ref = HN)], 
            by = c("year", "month")) %>% 
      na.omit()
    
    if(nrow(dat_i2) >= min_years){
      data.table(corr = cor(dat_i2$HN, dat_i2$HN_ref), 
                 nn = nrow(dat_i2), 
                 Name_test = dat_n[i, Name],
                 Name_ref = stn)   
    } else NULL
  
  }
  
  dat_i_cor
  
}

saveRDS(dat_corr, "~/projects/ALPINE_WIDE_SNOW/10_SNOWFALL/01_SPATCONS/monthly-correlations.rds")