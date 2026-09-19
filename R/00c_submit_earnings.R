# Supplementary extract: IPUMS replaced EARNWEEK/HOURWAGE with EARNWEEK2/
# HOURWAGE2 from 2023 onward (BLS changed earnings topcoding), so the main
# extract has no usable earnings after 2023. Pull the replacements for
# 2018m1-2026m8 and merge on CPSIDP + date.
suppressMessages({library(ipumsr); library(dplyr)})
set_ipums_api_key(Sys.getenv("IPUMS_API_KEY"))
samps <- get_sample_info("cps") %>%
  filter(grepl("^cps20\\d\\d_\\d\\d[sb]$", name), !grepl("ASEC", description)) %>%
  mutate(yr = as.integer(substr(name,4,7)), mo = as.integer(substr(name,9,10))) %>%
  filter(yr >= 2018) %>% arrange(yr, mo)
cat("samples:", nrow(samps), samps$name[1], "->", samps$name[nrow(samps)], "\n")
ext <- define_extract_micro("cps",
  description = "Deportations post: ORG earnings (EARNWEEK2/HOURWAGE2)",
  samples = samps$name,
  variables = c("YEAR","MONTH","SERIAL","PERNUM","CPSIDP","MISH","EARNWT",
                "EARNWEEK2","HOURWAGE2","PAIDHOUR","UNION","UHRSWORKORG"))
sub <- submit_extract(ext)
cat("submitted:", sub$number, "\n")
writeLines(as.character(sub$number), "data_raw/extract_id_earn.txt")
