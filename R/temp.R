


# FVG has hn and hs mixed -------------------------------------------------

fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/ORIGINAL_DATA/FVG/read_AINEVA_HS.csv") %>% 
  melt(id.vars = "Date", variable.factor = F) -> dat_fvg_hs
dat_fvg_hs[, Date := dmy(Date)]

dat_fvg_hs[startsWith(as.character(value), "9"), table(value)]
dat_fvg_hs[startsWith(as.character(value), "8"), table(value)]
dat_fvg_hs[value >= 800 & value < 900]
# dat_fvg[value >= 800 | value < 0, value := NA]



fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/ORIGINAL_DATA/FVG/read_AINEVA_HN.csv") %>% 
  melt(id.vars = "Date", variable.factor = F) -> dat_fvg
dat_fvg[, Date := dmy(Date)]
dat_fvg[startsWith(as.character(value), "9"), table(value)]
dat_fvg[startsWith(as.character(value), "8"), table(value)]
dat_fvg[value == 999 | value == 899, value := 0] # traces of HN or traces of HN with rain
dat_fvg[value == 800 , value := 0] # rain on snow, no fresh snow
dat_fvg[value > 800, value := value - 800]
dat_fvg[value < 0, value := NA]
dat_fvg[value > 100, .N, variable]
summary(dat_fvg)

dat_zz <- merge(dat_fvg_hs[, .(Date, variable, hs = value)],
                dat_fvg[, .(Date, variable, hn = value)], all = T)

zz <- dat_zz[variable == "Florianca"]
zz <- dat_zz[variable == "Casetta_in_Canada"]
zz <- dat_zz[variable == "Monte_Zoncolan"]
dat_fvg[value > 100, .N, variable]
dat_fvg[value > 100 & variable == "Florianca", .N, year(Date)]
dat_zz[hn > 100 & variable == "Casetta_in_Canada", .N, year(Date)]
dat_zz[hn > 100 & variable == "Monte_Zoncolan", .N, year(Date)]

dat_zz[!is.na(hs) & !is.na(hn), .(hs = sum(hs == 0), hn = sum(hn == 0)), variable]

all_stns <- sort(unique(dat_zz$variable))

pdf("fvg.pdf", width = 16, height = 8)
for(i_stn in all_stns){
  
  gg <- dat_zz[variable == i_stn & 
                 !is.na(hs) & !is.na(hn) & 
                 hs >= 0 & hn >= 0 & hs < 800 & hn < 800] %>% 
    ggplot(aes(Date))+
    geom_point(aes(y=hs, colour = "hs"))+
    geom_point(aes(y=hn, colour = "hn"))+
    facet_wrap(~year(Date), scales = "free_x")+
    theme_bw()+
    ggtitle(i_stn)
  print(gg)
}
dev.off()

all_stns

mixed <- c("Casetta_in_Canada", "Florianca", "Monte_Zoncolan", "Paularo","Pradibsco", "Varmost")


# test normalize ----------------------------------------------------------



# need to normalize data because of heteroscedascity 

dat_intercept <- dat_lm2[term == "(Intercept)" & estimate > 0] # remove the single negative value

dat_intercept[, estimate_sqrt := sqrt(estimate)]
dat_intercept[, estimate_yj := car::yjPower(estimate, -0.1)]
dat_intercept[, estimate_boxcox := car::bcPower(estimate, -0.1)]

df_bc <- data.frame(dat_intercept[estimate > 0])
MASS::boxcox(lm(estimate ~ 1, data = df_bc)) %>% as.data.table
car::bcPower(df_bc$estimate, lambda = -0.1) %>% qplot
car::yjPower(dat_intercept$estimate, lambda = -0.1) %>% qplot
library(bestNormalize)
yj_intercept <- yeojohnson(dat_intercept$estimate)
dat_intercept[, estimate_yj := predict(yj_intercept)]

gm1_elev <- gam(estimate_yj ~ s(elev, k = 10), data = dat_intercept)
gm1_elev
gam.check(gm1_elev)
plot(gm1_elev)
plot(fitted(gm1_elev), resid(gm1_elev))

dat_intercept[, gm1_fit_yj := fitted(gm1_elev)]
dat_intercept[, gm1_fit := predict(yj_intercept, newdata = gm1_fit_yj, inverse = T)]
dat_intercept %>% 
  ggplot(aes(elev))+
  geom_line(aes(y = gm1_fit))+
  geom_point(aes(y = estimate))

# -> looks good

# test gam ----------------------------------------------------------------


gm1_elev <- gam(estimate_yj ~ s(elev, k = 10), data = dat_intercept)
gm1_elev
gam.check(gm1_elev)
plot(gm1_elev)
plot(fitted(gm1_elev), resid(gm1_elev))

# dat_intercept[, gm1_fit_yj := fitted(gm1_elev)]
# dat_intercept[, gm1_fit := predict(yj_intercept, newdata = gm1_fit_yj, inverse = T)]
# dat_intercept %>% 
#   ggplot(aes(elev))+
#   geom_line(aes(y = gm1_fit))+
#   geom_point(aes(y = estimate))

# -> looks good



gm2_lonlat <- gam(estimate_yj ~ s(lon, lat), data = dat_intercept)
vis.gam(gm2_lonlat, plot.type = "contour")
gam.check(gm2_lonlat)



# test s, te, ti
kk <- 8

gm3_lle_1a <- gam(estimate_yj ~ s(lon, k = kk) + s(lat, k = kk) + ti(lon, lat, k = c(8,8)) + s(elev), data = dat_intercept)
vis.gam(gm3_lle_1a, plot.type = "contour")

gm3_lle_1b <- gam(estimate_yj ~ s(lon, k = kk) + s(lat, k = kk) + s(elev, k = kk) +
                    ti(lon, lat, elev, k = c(kk,kk,kk)), data = dat_intercept)
gm3_lle_1b
vis.gam(gm3_lle_1b, plot.type = "contour")


gm3_lle_1 <- gam(estimate_yj ~ s(lon, lat, k = 8*8, bs = "tp") + s(elev), data = dat_intercept)
gm3_lle_1
gam.check(gm3_lle_1)
plot(gm3_lle_1)
vis.gam(gm3_lle_1, plot.type = "contour")

gm3_lle_2 <- gam(estimate_yj ~ te(lon, lat, k = c(8,8), bs = "tp") + s(elev), data = dat_intercept)
gm3_lle_2
gam.check(gm3_lle_2)
plot(gm3_lle_2)
vis.gam(gm3_lle_2, plot.type = "contour")

gm3_lle_1 <- gam(estimate_yj ~ s(lon, lat, elev, bs = "tp") + s(elev), data = dat_intercept)
gm3_lle_1
gam.check(gm3_lle_1)
plot(gm3_lle_1)
vis.gam(gm3_lle_1, plot.type = "contour")



cbind(AIC(gm1_elev, gm2_lonlat, gm3_lle_1, gm3_lle_2),
      ll = sapply(list(gm1_elev, gm2_lonlat, gm3_lle_1, gm3_lle_2), logLik))


# test s, but scale lle

dat_intercept[, 
              paste0(c("lon", "lat", "elev"), "_scaled") := lapply(.SD, scale),
              .SDcols = c("lon", "lat", "elev")]
gm3_raw <- gam(estimate_yj ~ s(lon, elev), data = dat_intercept)
gm3_raw
gm3_scaled <- gam(estimate_yj ~ s(lon_scaled, elev_scaled), data = dat_intercept)
gm3_scaled
plot(gm3_raw)
plot(gm3_scaled)
vis.gam(gm3_raw, plot.type = "contour")
vis.gam(gm3_scaled, plot.type = "contour")

# -> need to scale if combine latlon with elev


# smi piemonte vda --------------------------------------------------------


sf_meta_hs <- sf::st_as_sf(join_meta_hs[startsWith(provider, "IT")], 
                           coords = c("Longitude", "Latitude"), crs = sf::st_crs(4326))
sf_meta_hs$provider2 <- sf_meta_hs$provider == "IT_SMI"
mapview::mapview(sf_meta_hs, zcol = "provider2")


meta_it_vda$Name %>% sort
meta_it_piemonte$Name %>% sort

smi_duplicated <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/01_MERGING/SMI/duplicates_smi_piemonte.txt")
meta_it_piemonte[Name %in% smi_duplicated$Piedmont]
smi_duplicated[!Piedmont %in% meta_it_piemonte$Name]
meta_it_piemonte$Name %>% sort()

meta_it_smi_hn[Name %in% smi_duplicated$SMI]
meta_it_smi_hs[Name %in% smi_duplicated$SMI]

smi_duplicated[! SMI %in% meta_it_smi_hn$Name]

meta_it_smi_hn[Name == "Lago_Codelago_Devero"]
meta_it_smi_hn[startsWith(Name, "Lago_Codelago")]
meta_it_smi_hn[startsWith(Name, "Lago_")]

join_data_hn[provider == "IT_PIEMONTE" & variable %in% smi_duplicated$Piedmont, .N, variable]
join_data_hs[provider == "IT_PIEMONTE" & variable %in% smi_duplicated$Piedmont, .N, variable]



# stns meta == data -------------------------------------------------------

stns_data_hn <- unique(join_data_hn$variable)
missing_meta_hn <- sort(stns_data_hn[! stns_data_hn %in% join_meta_hn$Name])
missing_meta_hn_new <- sort(join_meta_hn[provider == "CH_METEOSWISS" & grepl("na", Name), Name])

names(missing_meta_hn_new) <- missing_meta_hn

join_data_hn[variable %in% missing_meta_hn, .N, .(variable, provider)]

# join_meta_hn[Name %in% stns_data_hn]

cbind(sort(missing_meta_hn),
      sort(join_meta_hn[provider == "CH_METEOSWISS" & grepl("na", Name), Name]))


# some plot backup --------------------------------------------------------


dt_year0_nonan[mw_start_year == 1980 & qprob == 0.5 & snow == "HS"] %>% 
  ggplot(aes(Elevation, estimate, shape = country))+
  geom_hline(yintercept = 0)+
  # geom_point(size = 0.5)+
  # geom_smooth(se = F)+
  geom_bin2d(bins = 50)+
  scale_fill_viridis_c(trans = "log10")+
  coord_cartesian(ylim = c(-5, 5))+
  # facet_grid(country ~ month_f)+
  facet_wrap(~month_f, nrow = 2)+
  theme_bw()+
  xlab("Elevation [m]")+
  ylab("Trend mean monthly HS [cm/year]")+
  ggtitle("Linear trend (1980-2000) mean monthly HS")



# check cv gapfill --------------------------------------------------------

dd <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/04_GAPFILL/rds/cv-01-1day.rds")
dd
dd[, .N, Name]
dd[is.na(i_rep)]
dd[!is.na(i_rep),
   .(mae = mean(abs(value_true - value_fill))), 
   .(month, Name)] %>% 
  dcast(Name ~ month)


# cv gapfill 5 days ----------------------------------------------------------

run_na <- sum_run(!is.na(mat_1[, i_stn]), k = 5, na_pad = T)

i_row_possible <- which(run_na == 5 & month(vec_dates) %in% c(11, 12, 1:5))

if(length(i_row_possible) == 0) return(NULL)

if(length(i_row_possible) < 100){
  i_row_sample <- i_row_possible
} else {
  i_row_sample <- sample(i_row_possible, 100)
}


vec_dates[i_row_sample[1] - 4:0]

mat_1[i_row_sample[1] - 4:0, i_stn]

# EOF ---------------------------------------------------------------------


