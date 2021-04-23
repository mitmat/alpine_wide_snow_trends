# comparison trend models

library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
library(patchwork)

library(flextable)
library(officer)

pval_sym <- function(pval){
  symnum(pval, corr = FALSE,
         cutpoints = c(0,  .001,.01,.05, .1, 1),
         symbols = c("***","**","*","."," "))
}

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-02-1971-2019-sen.rda")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-03-1971-2019-scd.rda")




dat_seasonal_gls[term == "year0", 
                 .(Name, variable, est_gls = estimate, pval_gls = p.value)] %>% 
  merge(dat_seasonal_ols[term == "year0", 
                         .(Name, variable, est_ols = estimate, pval_ols = p.value)] ) %>%
  merge(dat_seasonal_sen[, 
                         .(Name, variable, est_sen = estimate, pval_sen = p.value)] ) %>% 
  merge(dat_seasonal_scd_nb[term == "year0", 
                            .(Name, variable, est_nb = estimate, pval_nb = p.value, est_nb_abs = trend.abs)],
        all = T) %>% 
  merge(dat_meta_clust) -> dat_comp_seasonal


dat_month_gls[term == "year0", 
              .(Name, month, est_gls = estimate, pval_gls = p.value)] %>% 
  merge(dat_month_ols[term == "year0", 
                      .(Name, month, est_ols = estimate, pval_ols = p.value)] ) %>%
  merge(dat_month_sen[, 
                      .(Name, month, est_sen = estimate, pval_sen = p.value)] ) %>% 
  merge(dat_meta_clust) -> dat_comp_month


dat_comp_seasonal[, elev_fct := cut(Elevation, breaks = seq(0,3000,by=500), dig.lab=5)]
dat_comp_month[, elev_fct := cut(Elevation, breaks = seq(0,3000,by=500), dig.lab=5)]


dat_comp_seasonal[, variable_fct := factor(variable,
                                           levels = c("SCD_NDJF", "SCD_MAM", "SCD_NDJFMAM",
                                                      "meanHS_DJF", "meanHS_MAM", "meanHS_NDJFMAM",
                                                      "maxHS_NDJFMAM"))]
mitmatmisc::add_month_fct(dat_comp_month, 10)
dat_comp_month[, variable_fct := month_fct]


# plot --------------------------------------------------------------------

f_plot <- function(dat, x, y, xlab, ylab){
  
  dat %>% 
    dplyr::group_by(variable_fct) %>% 
    dplyr::summarise(min = min({{x}}, {{y}}), max = max({{x}}, {{y}})) -> dat_range

  dat %>% 
    ggplot(aes({{x}}, {{y}}, colour = elev_fct))+
    geom_abline()+
    geom_point(size = 0.8, alpha = 0.8)+
    geom_blank(data = dat_range, inherit.aes = F, aes(min, max))+
    geom_blank(data = dat_range, inherit.aes = F, aes(max, min))+
    # facet_grid(. ~ variable_fct)+
    facet_wrap(~variable_fct, nrow = 1, scales = "free")+
    scale_color_viridis_d("Elevation [m]", option = "C", direction = -1)+
    theme_bw()+
    xlab(xlab)+
    ylab(ylab)
  
  
}

gg_seas_sen <- f_plot(dat_comp_seasonal, 10*est_gls, 10*est_sen,
                      "Trend GLS",
                      "Trend Sen")
gg_seas_ols <- f_plot(dat_comp_seasonal, 10*est_gls, 10*est_ols,
                      "Trend GLS",
                      "Trend OLS")
gg_seas_nb <- f_plot(dat_comp_seasonal[!is.na(est_nb_abs)], est_gls*10, est_nb_abs/49*10,
                     "Trend GLS",
                     "Trend NB")

gg_month_sen <- f_plot(dat_comp_month, 10*est_gls, 10*est_sen,
                       "Trend GLS",
                       "Trend Sen")
gg_month_ols <- f_plot(dat_comp_month, 10*est_gls, 10*est_ols,
                       "Trend GLS",
                       "Trend OLS")


gg_out <- (gg_seas_sen /
  gg_seas_ols / 
  (gg_seas_nb + plot_spacer() + plot_layout(widths = c(3,4))) / 
  gg_month_sen / 
  gg_month_ols) + plot_layout(guides = "collect")


ggsave(gg_out,
       filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/03_review2/fig/sm-trend-comp.png",
       width = 16, height = 10)

# table -------------------------------------------------------------------

f_summ <- function(est_x, est_y, pval_x, pval_y){
  
  nn <- length(est_x)
  
  data.table(bias = mean(est_x - est_y),
             mae = mean(abs(est_x - est_y)),
             corr = cor(est_x, est_y),
             x_sig_y_sig = sum(pval_x < 0.05 & pval_y < 0.05)/nn,
             x_sig_y_ns = sum(pval_x < 0.05 & pval_y >= 0.05)/nn,
             x_ns_y_sig = sum(pval_x >= 0.05 & pval_y < 0.05)/nn,
             x_ns_y_ns = sum(pval_x >= 0.05 & pval_y >= 0.05)/nn)
  
}

f_summ2 <- function(est_x, est_y, pval_x, pval_y){
  
  data.table(bias = mean(est_x - est_y),
             mae = mean(abs(est_x - est_y)),
             corr = cor(est_x, est_y),
             frac_sig_agree = (sum(pval_x < 0.05 & pval_y < 0.05) + 
                                 sum(pval_x >= 0.05 & pval_y >= 0.05)) / length(pval_x))
  
}

dat_table <- rbind(

data.table(m1 = "Sen", m2 = "GLS",
           dat_comp_seasonal[, 
                             f_summ(10*est_sen, 10*est_gls, pval_sen, pval_gls), 
                             keyby = .(grp = variable_fct)]),
data.table(m1 = "OLS", m2 = "GLS",
           dat_comp_seasonal[, 
                             f_summ(10*est_ols, 10*est_gls, pval_ols, pval_gls), 
                             keyby = .(grp = variable_fct)]),
data.table(m1 = "NB", m2 = "GLS",
           dat_comp_seasonal[!is.na(est_nb_abs), 
                             f_summ(est_nb_abs/49*10, est_gls*10, pval_ols, pval_gls), 
                             keyby = .(grp = variable_fct)]),

data.table(m1 = "Sen", m2 = "GLS",
           dat_comp_month[, 
                          f_summ(10*est_sen, 10*est_gls, pval_sen, pval_gls), 
                          keyby = .(grp = month_fct)]),
data.table(m1 = "OLS", m2 = "GLS",
           dat_comp_month[, 
                          f_summ(10*est_ols, 10*est_gls, pval_ols, pval_gls), 
                          keyby = .(grp = month_fct)])
)

dat_header <- data.table(col_keys = names(dat_table),
                         h1 = c("Model 1 (M1)", "Model 2 (M2)", "Variable",
                                "Bias", "MAE", "Corr",
                                "M1_sig_M2_sig", "M1_sig_M2_ns", "M1_ns_M2_sig", "M1_ns_M2_ns"))

ft <- dat_table %>% 
  flextable %>% 
  set_header_df(dat_header) %>%
  theme_booktabs() %>% 
  set_formatter(bias = function(x) sprintf("%.02f", x),
                mae = function(x) sprintf("%.02f", x),
                corr = function(x) sprintf("%.02f", x),
                x_sig_y_sig = scales::percent_format(),
                x_sig_y_ns = scales::percent_format(),
                x_ns_y_sig = scales::percent_format(),
                x_ns_y_ns = scales::percent_format()) %>% 
  # merge_v() %>% 
  # valign(j = c("m1", "m2"), valign = "top") %>% 
  hline(i = c(7,14,17,24), border = fp_border()) %>% 
  fontsize(size = 6, part = "all") %>% 
  font(fontname = "Arial", part = "all") %>% 
  autofit()

ft

read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/03_review2/table/comp-trend.docx")




# some numbers ------------------------------------------------------------


dat_table %>% summary
dat_table[, mean(x_sig_y_sig + x_ns_y_ns)]

dat_comp_seasonal[, summary(est_sen - est_gls)]
dat_comp_month[, summary(est_sen - est_gls)]
summary(c(dat_comp_seasonal$est_sen, dat_comp_month$est_sen) - 
          c(dat_comp_seasonal$est_gls, dat_comp_month$est_gls))

cor(c(dat_comp_seasonal$est_sen, dat_comp_month$est_sen),
    c(dat_comp_seasonal$est_gls, dat_comp_month$est_gls))

f_summ(c(dat_comp_seasonal$est_sen, dat_comp_month$est_sen),
        c(dat_comp_seasonal$est_gls, dat_comp_month$est_gls),
        c(dat_comp_seasonal$pval_sen, dat_comp_month$pval_sen),
        c(dat_comp_seasonal$pval_gls, dat_comp_month$pval_gls))


