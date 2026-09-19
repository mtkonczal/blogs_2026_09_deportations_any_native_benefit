# ==============================================================================
# 00_submit_extract.R
# Submit the IPUMS CPS basic monthly extract for the deportations/native-benefit
# post. 2015m1 - 2026m8. Writes the extract number to data_raw/extract_id.txt.
#
# No existing local extract works: cps_00031 has the nativity variables but ends
# 2025m8 and lacks state and earnings; cps_00036 has CITIZEN only.
# ==============================================================================
suppressMessages({library(ipumsr); library(dplyr)})
set_ipums_api_key(Sys.getenv("IPUMS_API_KEY"))

samps <- get_sample_info("cps") %>%
  filter(grepl("^cps(201[5-9]|202[0-6])_\\d\\d[sb]$", name)) %>%
  filter(!grepl("ASEC", description))

# Basic monthly only, 2015m1 onward, in calendar order.
samps <- samps %>%
  mutate(yr = as.integer(substr(name, 4, 7)),
         mo = as.integer(substr(name, 9, 10))) %>%
  filter(yr >= 2015) %>%
  arrange(yr, mo)

cat("samples:", nrow(samps), "| first", samps$name[1],
    "| last", samps$name[nrow(samps)], "\n")

VARS <- c(
  # identifiers, weights, rotation
  "YEAR","MONTH","SERIAL","PERNUM","CPSID","CPSIDP","MISH",
  "WTFINL","HWTFINL","EARNWT","LNKFW1MWT","PANLWT",
  # demographics
  "AGE","SEX","RACE","HISPAN","MARST","NCHILD","NCHLT5","FAMSIZE",
  # labor force
  "EDUC","EMPSTAT","LABFORCE","CLASSWKR",
  "OCC","OCC2010","IND","IND1990",
  "UHRSWORKT","AHRSWORKT","WKSTAT","WHYPTLWK","WHYABSNT","WHYUNEMP",
  "DURUNEMP","WNLOOK",
  # earnings (outgoing rotation groups only)
  "EARNWEEK","HOURWAGE","PAIDHOUR","UNION",
  # nativity: the whole point
  "NATIVITY","BPL","CITIZEN","YRIMMIG",
  # geography
  "STATEFIP","METFIPS"
)

spec <- lapply(VARS, function(v) {
  if (v %in% c("EARNWEEK","HOURWAGE")) var_spec(v, data_quality_flags = TRUE)
  else var_spec(v)
})

ext <- define_extract_micro(
  collection  = "cps",
  description = "Deportations / native-born benefit, 2015m1-2026m8",
  samples     = samps$name,
  variables   = spec
)

sub <- submit_extract(ext)
cat("submitted extract:", sub$number, "\n")
writeLines(as.character(sub$number), "data_raw/extract_id.txt")
