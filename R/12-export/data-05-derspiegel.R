


library(data.table)
library(forcats)
library(ggplot2)
load("data/future-summary-elev-500m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

dat_ens_mean <- dat_ds[elev > 200 & elev < 3600,
                       .(scd = round(mean(scd))),
                       .(elev_f, elev, experiment, period, fp)]

fwrite(dat_ens_mean, "data-raw/spiegel-01-alps.csv")



load("data/future-summary-country-elev-500m.rda")
# load("data/future-summary-country-elev-200m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

# remove some countries
country_remove <- c("Hungary", "Liechtenstein", "San Marino", "Slovakia")

dat_ens_mean <- dat_ds[elev > 200 & elev < 3600 & ! country %in% country_remove,
                       .(scd = round(mean(scd))),
                       .(country, elev_f, elev, experiment, period, fp)]

setorder(dat_ens_mean, country, period, experiment, elev)

fwrite(dat_ens_mean, "data-raw/spiegel-02-country.csv")

