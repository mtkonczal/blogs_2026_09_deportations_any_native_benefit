# Independent, read-only audit of the ten charts in blog_post.md.
# Writes diagnostics only to output/fact_check/; does not regenerate charts.
suppressPackageStartupMessages({library(ipumsr); library(data.table); library(lubridate)})
dir.create("output/fact_check", recursive = TRUE, showWarnings = FALSE)
out <- function(x, name) fwrite(x, file.path("output/fact_check", name))
ddi <- read_ipums_ddi("data_raw/cps_00055.xml")
vars <- c("YEAR","MONTH","CPSIDP","MISH","WTFINL","LNKFW1MWT","AGE","SEX",
          "RACE","CITIZEN","EMPSTAT","LABFORCE","EDUC","IND1990","STATEFIP",
          "UHRSWORKT","QEARNWEE","QHOURWAG")
ag <- list(); ed <- list(); co <- list(); st <- list(); fl <- list(); wa <- list(); i <- 0L
cb <- function(x, pos) {
  i <<- i + 1L; setDT(x)
  x <- x[AGE >= 16 & YEAR >= 2020]
  for(j in names(x)) set(x, j=j, value=as.numeric(x[[j]]))
  x[, `:=`(native = CITIZEN %in% 1:3, emp = EMPSTAT %in% c(10,12),
            unemp = EMPSTAT %in% c(20,21,22), prime = AGE >= 25 & AGE <= 54)]
  ag[[i]] <<- x[, .(pop=sum(WTFINL), emp=sum(WTFINL*emp), lf=sum(WTFINL*(LABFORCE==2)),
                     unemp=sum(WTFINL*unemp), n=.N), by=.(YEAR,MONTH,native,prime,SEX)]
  ed[[i]] <<- x[native & prime & LABFORCE==2,
    .(un=sum(WTFINL*unemp),lf=sum(WTFINL),n=.N),
    by=.(YEAR,MONTH,educ3=fcase(EDUC>1 & EDUC<73,"Less than high school",EDUC==73,"High school diploma",
                              EDUC>73 & EDUC<111,"Some college or associate", default="Other"))]
  co[[i]] <<- x[MONTH<=8, .(pop=sum(WTFINL),con=sum(WTFINL*emp*(IND1990==60))),
                         by=.(YEAR,native,prime,youth=AGE<=24)]
  st[[i]] <<- x[prime & YEAR>=2024, .(pop=sum(WTFINL),emp=sum(WTFINL*emp)),
                         by=.(YEAR,MONTH,STATEFIP,CITIZEN)]
  fl[[i]] <<- x[native & prime & CPSIDP>0,
                 .(YEAR,MONTH,CPSIDP,MISH,AGE,SEX,RACE,CITIZEN,emp,unemp,WTFINL,LNKFW1MWT)]
  wa[[i]] <<- x[native & emp & MISH %in% c(4,8),
                .(YEAR,MONTH,CPSIDP,MISH,UHRSWORKT,QEARNWEE,QHOURWAG)]
  NULL
}
read_ipums_micro_chunked(ddi, vars=all_of(vars), callback=IpumsSideEffectCallback$new(cb),
                        chunk_size=1e6, verbose=FALSE, var_attrs=NULL)
cat("Read raw CPS microdata.\n")
a <- rbindlist(ag)[,lapply(.SD,sum),by=.(YEAR,MONTH,native,prime,SEX),.SDcols=c("pop","emp","lf","unemp","n")]
out(a,"aggregate_cells.csv")
male <- a[native & prime & SEX==1,.(date=as.Date(sprintf("%d-%02d-01",YEAR,MONTH)),epop=emp/pop)]
old <- fread("output/b2_h24_male_levels_monthly.csv"); old[,date:=as.Date(date)]
cmp <- merge(male,old,by="date")
cat("H3 raw-to-output max difference:",max(abs(cmp$epop-cmp$epop_nat_men_prime)),"\n")
e <- rbindlist(ed)[,lapply(.SD,sum),by=.(YEAR,MONTH,educ3),.SDcols=c("un","lf","n")]
e[,date:=as.Date(sprintf("%d-%02d-01",YEAR,MONTH))]
e[,rate:=un/lf]
e[,rate3:=sapply(date,function(t) {z <- .SD[date>=t %m-% months(2) & date<=t];sum(z$un)/sum(z$lf)}),by=educ3]
out(e,"education_recomputed.csv")
old <- fread("output/h5_native_unrate_educ3_monthly.csv")
cmp <- merge(e[YEAR %in% c(2024,2026)],old,by.x=c("YEAR","MONTH","educ3"),by.y=c("year","month","educ3"))
cat("H4 raw-to-output max difference:",max(abs(cmp$rate3-cmp$unrate_3m)),"\n")
c <- rbindlist(co)[,lapply(.SD,sum),by=.(YEAR,native,prime,youth),.SDcols=c("pop","con")]
con <- c[,.(foreign_share=sum(con[!native])/sum(con),
             native_prime_con=sum(con[native & prime])/sum(pop[native & prime]),
             native_youth_con=sum(con[native & youth])/sum(pop[native & youth])),by=YEAR]
out(con,"construction_recomputed.csv"); print(con[YEAR>=2023])
old <- fread("output/h9_construction_native_emp_jan_aug.csv")
cmp <- merge(con,old,by.x="YEAR",by.y="yr")
cat("H10 raw-to-output max difference:",max(abs(cmp$native_prime_con-cmp$C_nat_prime_all_con_rate)),"\n")
s <- rbindlist(st)[,lapply(.SD,sum),by=.(YEAR,MONTH,STATEFIP,CITIZEN),.SDcols=c("pop","emp")]
s[,date:=as.Date(sprintf("%d-%02d-01",YEAR,MONTH))]
s[,window:=fcase(YEAR==2024,"pre",date>=as.Date("2025-09-01"),"post",default=NA_character_)]
win <- function(drop_oct=FALSE) {
 z <- s[!is.na(window) & (!drop_oct | MONTH!=10)]
 w <- z[,.(P=sum(pop)/uniqueN(date), N=sum(pop[CITIZEN %in% 1:3])/uniqueN(date),
            EN=sum(emp[CITIZEN %in% 1:3])/uniqueN(date), EC=sum(emp[CITIZEN==5])/uniqueN(date)),by=.(STATEFIP,window)]
 w <- dcast(w,STATEFIP~window,value.var=c("P","N","EN","EC"))
 w[,`:=`(x=100*(EC_pre/P_pre-EC_post/P_post),
          y=100*(EN_post/N_post-EN_pre/N_pre)*N_pre/P_pre,
          x_fixed=100*(EC_pre-EC_post)/P_pre)]
 w[,sensitivity:=if(drop_oct) "exclude October both windows" else "original windows"]
 w
}
ss <- rbind(win(),win(TRUE));out(ss,"state_recomputed.csv")
for(lab in unique(ss$sensitivity)) {
 z <- ss[sensitivity==lab & STATEFIP!=11]
 fit <- lm(y~x,data=z); fitw <- lm(y~x,data=z,weights=P_pre)
 cat("H5",lab,"OLS",coef(fit)[2],"SE",summary(fit)$coef[2,2],"pop weighted",coef(fitw)[2],"\n")
}
f <- rbindlist(fl); rm(ag,ed,co,st,fl); invisible(gc())
# Include all age-16+ destination observations, matching the source script;
# inspect demographic consistency separately rather than dropping bad links.
dst <- list(); j<-0L
cb2 <- function(x,pos) {
 setDT(x); x<-x[YEAR>=2020 & AGE>=16 & CPSIDP>0];j<<-j+1L
 for(k in names(x)) set(x,j=k,value=as.numeric(x[[k]]))
 dst[[j]]<<-x;NULL
}
read_ipums_micro_chunked(ddi,vars=all_of(c("YEAR","MONTH","CPSIDP","MISH","AGE","SEX","RACE","EMPSTAT")),
 callback=IpumsSideEffectCallback$new(cb2),chunk_size=2e6,verbose=FALSE,var_attrs=NULL)
d <- rbindlist(dst);rm(dst);invisible(gc())
f[,time:=YEAR*12+MONTH];d[,time:=YEAR*12+MONTH-1L]
setnames(d,c("MISH","AGE","SEX","RACE","EMPSTAT"),paste0("next_",c("MISH","AGE","SEX","RACE","EMPSTAT")))
d[,c("YEAR","MONTH"):=NULL]
links <- merge(f,d,by=c("CPSIDP","time"),allow.cartesian=FALSE)[next_MISH==MISH+1]
links[,wt:=fifelse(!is.na(LNKFW1MWT)&LNKFW1MWT>0,LNKFW1MWT,WTFINL)]
links[,validated:=SEX==next_SEX & RACE==next_RACE & next_AGE>=AGE & next_AGE<=AGE+2]
fr <- links[,.(UE=sum(wt*unemp*(next_EMPSTAT %in% c(10,12)))/sum(wt*unemp),
                UE_positive_link_weight=sum(LNKFW1MWT*unemp*(next_EMPSTAT %in% c(10,12)),na.rm=TRUE)/sum(LNKFW1MWT*unemp,na.rm=TRUE),
                UE_validated=sum(wt*unemp*(next_EMPSTAT %in% c(10,12))*validated)/sum(wt*unemp*validated),
                n=.N,n_fallback=sum(is.na(LNKFW1MWT)|LNKFW1MWT<=0),n_invalid=sum(!validated)),by=.(YEAR,MONTH)]
fr[,date:=as.Date(sprintf("%d-%02d-01",YEAR,MONTH))];setorder(fr,date)
fr[,roll12rows:=frollmean(UE,12)]
fr[,roll12calendar:=sapply(date,function(t) mean(fr[date>t %m-% months(12)&date<=t]$UE))]
out(fr,"flows_recomputed.csv")
old <- fread("output/h7_flows_monthly.csv")[grp=="Native prime-age"]; old[,date:=as.Date(date)]
cmp <- merge(fr,old,by="date",suffixes=c("_new","_old"))
cat("H7 raw-to-output max difference:",max(abs(cmp$UE_new-cmp$UE_old)),"\n"); print(tail(fr,5))
rm(f,d,links);invisible(gc())
w <- rbindlist(wa);rm(wa);invisible(gc())
out(w[,.(n=.N,flag_weekly=sum(QEARNWEE>0,na.rm=TRUE),flag_hourly=sum(QHOURWAG>0,na.rm=TRUE),
          missing_weekly=sum(is.na(QEARNWEE)),missing_hourly=sum(is.na(QHOURWAG))),by=YEAR],"earnings_flags.csv")
er <- as.data.table(read_ipums_micro("data_raw/cps_00056.xml",vars=all_of(c("YEAR","MONTH","CPSIDP","MISH","EARNWT","EARNWEEK2","HOURWAGE2","UHRSWORKORG")),verbose=FALSE,var_attrs=NULL))
er <- er[YEAR>=2020 & MISH %in% c(4,8)]
er[,`:=`(ew=fifelse(EARNWEEK2>0&EARNWEEK2<999999,EARNWEEK2,NA_real_),
          hw=fifelse(HOURWAGE2>0&HOURWAGE2<999,HOURWAGE2,NA_real_),
          uh=fifelse(UHRSWORKORG>0&UHRSWORKORG<997,UHRSWORKORG,NA_real_))]
er <- er[!is.na(ew)|!is.na(hw)]
cat("Wage keys duplicated:",anyDuplicated(w[,.(CPSIDP,YEAR,MONTH)]),anyDuplicated(er[,.(CPSIDP,YEAR,MONTH)]),"\n")
wg <- merge(w,er,by=c("CPSIDP","YEAR","MONTH","MISH"),allow.cartesian=FALSE)
wg[,hours:=fifelse(!is.na(uh),uh,fifelse(UHRSWORKT<997,UHRSWORKT,NA_real_))]
wg[,wage:=fifelse(!is.na(hw),hw,fifelse(hours>=1,ew/hours,NA_real_))]
wg[,imputed:=(!is.na(QEARNWEE)&QEARNWEE>0)|(!is.na(QHOURWAG)&QHOURWAG>0)]
wg <- wg[!is.na(wage)&wage>=2&wage<500&EARNWT>0]
wg[,date:=as.Date(sprintf("%d-%02d-01",YEAR,MONTH))]
wp <- function(z,band=TRUE) {
 setorder(z,wage); cw<-cumsum(z$EARNWT)/sum(z$EARNWT)
 sapply(c(.1,.2,.4,.5),function(p) mean(sapply(if(band) seq(p-.02,p+.02,.005) else p,
                                            function(q) z$wage[which(cw>=q)[1]])))
}
wr <- rbindlist(lapply(seq(as.Date("2022-01-01"),max(wg$date),by="month"),function(t) {
 rbindlist(lapply(c(6,12),function(nm) {
   z <- wg[date>=t %m-% months(nm-1) & date<=t]
   rbindlist(lapply(c("original","exact_percentile","include_imputed","exclude_october"),function(spec) {
     q <- if(spec=="include_imputed") z else z[imputed==FALSE]
     if(spec=="exclude_october") q<-q[MONTH!=10]
     data.table(date=t,window=nm,spec=spec,pctile=c("p10","p20","p40","p50"),wage=wp(copy(q),spec!="exact_percentile"),n=nrow(q))
   }))
 }))
}))
setorder(wr,spec,window,pctile,date)
wr[,yoy:=wage/shift(wage,12)-1,by=.(spec,window,pctile)]
out(wr,"wages_recomputed.csv")
old <- fread("output/h5_native_nominal_wage_percentiles_monthly.csv");old[,date:=as.Date(date)]
cmp <- merge(wr[spec=="original"],old,by=c("date","window","pctile"),suffixes=c("_new","_old"))
cat("H6 raw-to-output max wage difference:",max(abs(cmp$wage_new-cmp$wage_old)),"max growth difference:",max(abs(cmp$yoy_new-cmp$yoy_old),na.rm=TRUE),"\n")
print(wr[date==max(date)&window==6,.(spec,pctile,yoy)])
cat("Audit complete.\n")
