# plot monthly time series


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)
library(cowplot)
library(forcats)
library(foreach)
library(fs)



dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020_gapfilled_HS_sub/data_long_HS.rds")

# indices
dir_ls("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/indices/") %>% 
  grep("meta_all", ., invert = T, value = T) %>% 
  lapply(function(x) {
    dat <- readRDS(x)
    path_file(x) %>% 
      path_ext_remove -> name_index
    setnames(dat, name_index, "value")
    dat[, variable := name_index]
  }) %>% rbindlist -> dat_season


# spatcons
stns_ok <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/spatcons_stns_ok.rds")
dat_meta <- dat_meta[Name %in% stns_ok]
dat_month <- dat_month[Name %in% dat_meta$Name]
dat_season <- dat_season[Name %in% dat_meta$Name]





mitmatmisc::init_parallel_ubuntu(6)

# monthly plots -----------------------------------------------------------

# fill?
expand.grid(Name = sort(unique(dat_month$Name)),
            year = 1961:2020,
            month = c(11:12,1:5)) %>% 
  data.table %>% 
  merge(dat_month, by = c("Name", "year", "month"), all.x = T) -> dat_month_full

dat_month_full <- mitmatmisc::add_month_fct(dat_month_full, 10)

dat_loop_month <- unique(dat_month_full[, .(Name)])
dat_loop_month[, ii := 1:.N]
dat_loop_month[, chunk := floor((ii-1)/100)]


foreach(
  i_chunk = sort(unique(dat_loop_month$chunk)),
  .inorder = F
) %dopar% {
  
  all_stn <- sort(dat_loop_month[chunk == i_chunk, Name])
  
  foreach(
    i_stn = all_stn
  ) %do% {
    
    fn_out <- path("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/DASHBOARD/ts-month/", i_stn, ext = "png")
    
    dat_plot <- dat_month_full[Name == i_stn & month %in% c(11,12,1:5)]
    if(nrow(dat_plot) == 0) return(paste0("Nothing to do:", i_stn))
    
    tit <- dat_meta[Name == i_stn, paste0(Name, ", ", Elevation, "m", ", ", 
                                          round(Longitude, 3), " °E ", round(Latitude,3), " °N")]
    
    
    gg <-
      dat_plot %>% 
      ggplot(aes(year, HS))+
      geom_point(aes(colour = frac_gapfilled), na.rm = T)+
      facet_wrap(~month_fct, nrow = 2)+
      scale_colour_viridis_b("Fraction of gap filled observations per year", option = "E",
                             breaks = seq(0.25,0.75,by=0.25), limits = c(0,1), show.limits = T)+
      theme_bw()+
      theme(legend.position = "bottom")+
      ggtitle(tit)+
      xlab(NULL)+ylab("Mean monthly snow depth [cm]")+
      xlim(1961, 2020)
    
    
    ggsave(gg,
           filename = fn_out,
           width = 12, height = 6, dpi = 150)
    
  }
  
  
  return(tit)
}


# season plots ------------------------------------------------------------

dat_season[frac_gapfilled > 1, frac_gapfilled := 1]

# fill?
expand.grid(Name = sort(unique(dat_season$Name)),
            year = 1961:2020,
            variable = sort(unique(dat_season$variable))) %>% 
  data.table %>% 
  merge(dat_season, by = c("Name", "year", "variable"), all.x = T) -> dat_season_full


dat_loop_season <- unique(dat_season_full[, .(Name)])
dat_loop_season[, ii := 1:.N]
dat_loop_season[, chunk := floor((ii-1)/100)]


foreach(
  i_chunk = sort(unique(dat_loop_season$chunk)),
  .inorder = F
) %dopar% {
  
  all_stn <- sort(dat_loop_season[chunk == i_chunk, Name])
  
  foreach(
    i_stn = all_stn
  ) %do% {
    
    fn_out <- path("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/DASHBOARD/ts-season/", i_stn, ext = "png")
    
    dat_plot_hs <- dat_season_full[Name == i_stn & !startsWith(variable, "SCD")]
    dat_plot_scd <- dat_season_full[Name == i_stn & startsWith(variable, "SCD")]
    
    dat_plot_hs[, variable_fct := fct_relevel(factor(variable), "maxHS_NDJFMAM", after = Inf)]
    dat_plot_scd[, variable_fct := fct_relevel(factor(variable), "SCD_NDJF")]
    # if(nrow(dat_plot_hs) == 0) return(paste0("Nothing to do:", i_stn))
    
    tit <- dat_meta[Name == i_stn, paste0(Name, ", ", Elevation, "m", ", ", 
                                          round(Longitude, 3), " °E ", round(Latitude,3), " °N")]
    
    
    gg_hs1 <-
      dat_plot_hs[variable_fct != "maxHS_NDJFMAM"] %>% 
      ggplot(aes(year, value))+
      geom_point(aes(colour = frac_gapfilled), na.rm = T)+
      facet_grid(. ~ variable_fct)+
      scale_colour_viridis_b("Fraction of gap filled observations per year", option = "E",
                             breaks = seq(0.25,0.75,by=0.25), limits = c(0,1), show.limits = T)+
      theme_bw()+
      theme(legend.position = "none")+
      # ggtitle(tit)+
      xlab(NULL)+ylab("Mean seasonal snow depth [cm]")+
      xlim(1961, 2020)
    
    gg_hs2 <-
      dat_plot_hs[variable_fct == "maxHS_NDJFMAM"] %>% 
      ggplot(aes(year, value))+
      geom_point(aes(colour = frac_gapfilled), na.rm = T)+
      facet_grid(. ~ variable_fct)+
      scale_colour_viridis_b("Fraction of gap filled observations per year", option = "E",
                             breaks = seq(0.25,0.75,by=0.25), limits = c(0,1), show.limits = T)+
      theme_bw()+
      theme(legend.position = "none")+
      # ggtitle(tit)+
      xlab(NULL)+ylab("Maximum seasonal snow depth [cm]")+
      xlim(1961, 2020)
    
    
    gg_scd <-
    dat_plot_scd %>% 
      ggplot(aes(year, value))+
      geom_point(aes(colour = frac_gapfilled), na.rm = T)+
      facet_grid(. ~ variable_fct)+
      scale_colour_viridis_b("Fraction of gap filled observations per year", option = "E",
                             breaks = seq(0.25,0.75,by=0.25), limits = c(0,1), show.limits = T)+
      theme_bw()+
      theme(legend.position = "bottom")+
      # ggtitle(tit)+
      xlab(NULL)+ylab("Seasonal snow cover duration [days]")+
      xlim(1961, 2020)
    
    gg_tit <-
      ggdraw()+
      draw_label(tit, x = 0,  hjust = 0, fontface = "bold")+
      draw_label("NDJ... = N(ovember)D(ecember)J(anuary)...", x = 1, hjust = 1, fontface = "plain")+
      theme(plot.margin = margin(0, 10, 0, 20))
    
    gg_hs <- plot_grid(gg_hs1, gg_hs2, rel_widths = c(3,1.2))
    
    gg_out <- plot_grid(gg_tit, gg_hs, gg_scd,
                        ncol = 1, rel_heights = c(0.1,1,1.2))
    
    
    ggsave(gg_out,
           filename = fn_out,
           width = 14, height = 7, dpi = 150)
    
  }
  
  
  return(tit)
}
