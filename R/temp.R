


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



# EOF ---------------------------------------------------------------------


