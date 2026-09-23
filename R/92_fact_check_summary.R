# Lightweight summaries of the independent audit and original chart tables.
suppressPackageStartupMessages({library(data.table);library(tigris)})
od <- "output/fact_check"
a <- fread(file.path(od,"aggregate_cells.csv"))
male <- a[native==TRUE & prime==TRUE & SEX==1 & MONTH<=8,
          .(epop=100*sum(emp)/sum(pop),lfpr=100*sum(lf)/sum(pop),
            unrate=100*sum(unemp)/sum(lf)),by=YEAR]
fwrite(male,file.path(od,"native_male_participation.csv"));print(male)
e <- fread("output/h5_native_unrate_educ3_monthly.csv")
ed <- e[month<=8 & year %in% c(2024,2026),.(avg_unrate=100*mean(unrate)),by=.(educ3,year)]
fwrite(ed,file.path(od,"education_jan_aug_summary.csv"));print(ed)
d <- dcast(e[year %in% c(2024,2026)],educ3+month~year,value.var="unrate_3m")
d <- d[month<=8,.(educ3,month,difference_pp=100*(`2026`-`2024`))]
fwrite(d,file.path(od,"education_monthly_differences.csv"))
w <- fread(file.path(od,"wages_recomputed.csv"))
w <- dcast(w[date %in% c("2024-12-01","2025-12-01","2026-08-01")],spec+window+pctile~date,value.var="yoy")
fwrite(w,file.path(od,"wage_sensitivity_summary.csv"))
a <- fread(file.path(od,"state_recomputed.csv"))[sensitivity=="original windows"]
data(fips_codes)
x <- unique(as.data.table(fips_codes)[,.(STATEFIP=as.integer(state_code),state_name)])
a <- merge(a,x,by="STATEFIP"); b<-fread("output/h11_state_exposure_panel.csv")
z <- merge(a,b,by="state_name")
cat("H5 raw-to-output x difference",max(abs(z$x+z$d_nc)),"y difference",max(abs(z$y-z$nat_jobs_beyond_pop)),"\n")
f <- lm(nat_jobs_beyond_pop~I(-d_nc),b)
print(summary(f));print(confint(f))
d <- fread(file.path(od,"bls_fresh.csv")); d[,date:=as.IDate(date)]
z <- merge(d,fread("output/h12_tightness.csv"),by="date")
cat("H9 fresh-vs-original max vu difference",max(abs(z$JTS000000000000000JOL/z$LNS13000000-z$vu),na.rm=TRUE),
    "quits difference",max(abs(z$JTS000000000000000QUR/100-z$quits_rate),na.rm=TRUE),"\n")
f <- fread("output/h7_flows_monthly.csv")[grp=="Native prime-age"];setorder(f,date)
f[,r:=frollmean(UE,12)]
print(f[date %in% as.IDate(c("2024-12-01","2025-12-01","2026-07-01")),.(date,UE,r)])
cat("Last earlier observation with lower smoothed rate:",as.character(max(f[date<max(date)&r<tail(f$r,1)]$date)),"\n")
capture.output(sessionInfo(),file=file.path(od,"session_info.txt"))
