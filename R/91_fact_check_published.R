# Retrieve current BLS series into separate audit files, preserving chart vintage.
suppressPackageStartupMessages({library(blsR);library(data.table)})
dir.create("output/fact_check",recursive=TRUE,showWarnings=FALSE)
ids <- c("LNU04073413","LNU00073417","LNU00073418","LNU00073419",
         "LNU02073417","LNU02073418","LNU02073419",
         "LNS13000000","LNS11000000","LNS14000000",
         "JTS000000000000000JOL","JTS000000000000000QUR")
d <- as.data.table(get_n_series_table(ids,api_key=Sys.getenv("BLS_KEY"),start_year=2024,end_year=2026,tidy=TRUE))
for(j in names(d)) set(d,j=j,value=as.numeric(d[[j]]))
d[,date:=as.Date(sprintf("%d-%02d-01",year,month))];setorder(d,date)
fwrite(d,"output/fact_check/bls_fresh.csv")
d[,`:=`(native_epop=(LNU02073417+LNU02073418+LNU02073419)/(LNU00073417+LNU00073418+LNU00073419),
          overall_unrate=100*LNS13000000/LNS11000000)]
print(d[month==8,.(date,LNU04073413,native_epop,overall_unrate,LNS14000000)])
old <- fread("data/fig1_native_unrate_official.csv");old[,date:=as.Date(date)]
c <- merge(d,old,by="date")
cat("H1 cached vs fresh max difference",max(abs(c$LNU04073413/100-c$unrate_nat),na.rm=TRUE),"\n")
old <- fread("data/fig2_prime_epop_native_official.csv");old[,date:=as.Date(date)]
c <- merge(d,old,by="date")
cat("H2 cached vs fresh max difference",max(abs(c$native_epop-c$prime_epop_nat),na.rm=TRUE),"\n")
print(d[date>=as.Date("2024-10-01"),.(date,vu=JTS000000000000000JOL/LNS13000000,quits=JTS000000000000000QUR)])
q <- as.data.table(get_n_series_table("PRS85006173",api_key=Sys.getenv("BLS_KEY"),start_year=2024,end_year=2026,tidy=TRUE))
fwrite(q,"output/fact_check/bls_labor_share_fresh.csv");print(q)
writeLines(paste("Retrieved",format(Sys.time(),tz="UTC"),"UTC"),"output/fact_check/bls_retrieved.txt")
