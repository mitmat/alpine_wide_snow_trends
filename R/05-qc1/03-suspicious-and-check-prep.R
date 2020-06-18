# prep tables and figs for zero == NA issue

library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(directlabels)
library(readxl)
library(writexl)
library(foreach)


# prep data ---------------------------------------------------------------

original
dat_all[, month := month(Date)]


dat_all[, HN := as.integer(round(HN))]
dat_all[, HS := as.integer(round(HS))]




# read in stns ------------------------------------------------------------

fread("manual-qc/series-to-check.txt")
fread("manual-qc/suspicious-series.txt")

dat_meta[grepl("edf", Name)]
dat_meta[grepl("glac", Name)]




stns_to_check <- c(
  fread("manual-qc/series-to-check.txt") %>% .$Name,
  fread("manual-qc/suspicious-series.txt") %>% .$Name,
  dat_meta[grepl("edf", Name), Name]
)




# for manual check: plots and tables ----------------------------------------------------

for(i_stn in stns_to_check){
  
  dat_i_stn <- dat_all[Name == i_stn]
  dat_i_stn[, year := year(Date)]
  
  dat_i_stn[, idn := year - min(year)]
  dat_i_stn[, idn_grp := floor(idn / 2)]
  
  
  pdf(paste0("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/fig/suspicios-and-check/", i_stn, ".pdf"),
      width = 14, height = 7)
  
  for(i_idn_grp in sort(unique(dat_i_stn$idn_grp))){
    
    gg <- 
      dat_i_stn[idn_grp == i_idn_grp] %>% 
      ggplot(aes(Date))+
      geom_point(aes(y = HS, colour = "HS"))+
      geom_point(aes(y = HN, colour = "HN"), size = 0.5)+
      facet_wrap(~year, scales = "free", nrow = 2)+
      scale_x_date(date_labels = "%b")+
      theme_bw()+
      ylab("HS / HN  [cm]")
    
    
    print(gg)
    
    
  }
  
  dev.off()
  
  
  # table
  dat_table <- dat_i_stn[, .(year, Date, HN, HS, to_remove = NA_integer_)]
  
  setkey(dat_table, Date)
  
  write_xlsx(
    dat_table,
    paste0("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/table/suspicious-and-check/", i_stn, ".xlsx")
  )
  
}

# overview table

data.table(Name = stns_to_check,
           OK = NA,
           remove_some_values = NA,
           remove_whole_series = NA) %>% 
  write_xlsx("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/table/suspicious-and-check-overview_empty.xlsx")



# EOF ---------------------------------------------------------------------


