# write data for zenodo

library(data.table)
library(magrittr)
library(fs)



load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")

fwrite(dat_month_gls[, .(Name, month, term, estimate, std.error, statistic, p.value)],
       file = "~/alps-snow/ALPINE_WIDE_SNOW/09_EXPORT/deniz-trends.csv")

dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

fwrite(dat_meta_clust,
       file = "~/alps-snow/ALPINE_WIDE_SNOW/09_EXPORT/deniz-meta.csv")
