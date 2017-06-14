library(haven)
library(dplyr)

citations_url <- "http://elsa.berkeley.edu/~bhhall/pat/cite76_06.zip"
temp <- tempfile()
download.file(citations_url, temp, method = "libcurl", mode = "wb")
unzip(temp, "cite76_06.dta")
citations <- read_dta("cite76_06.dta")
unlink(temp)

patents_in_url <- "http://users.nber.org/~jbessen/pat76_06_assg.dta.zip"
temp <- tempfile()
download.file(patents_in_url, temp, method = "libcurl", mode = "wb")
unzip(temp, "pat76_06_assg.dta")
patents_in <- read_dta("pat76_06_assg.dta")
unlink(temp)

patents <- patents_in %>%
  select(cat, patent, pdpass) %>%
  filter(!is.na(pdpass))

pd_cite <- citations %>%
  left_join(patents, by = c("citing" = "patent")) %>%
  rename(citing_pdpass = pdpass, citing_cat = cat) %>%
  filter(!is.na(citing_pdpass)) %>%
  left_join(patents, by = c("cited" = "patent")) %>%
  rename(cited_pdpass = pdpass, cited_cat = cat) %>%
  filter(!is.na(citing_pdpass)) %>%
  select(-ncites7606) %>%
  filter(citing_cat == 3 & cited_cat == 3) %>% # too big for package if fit on all six categories
  select(-citing_cat, -cited_cat)  

pdpass_cites <- pd_cite %>%
  group_by(citing_pdpass, cited_pdpass) %>%
  summarize(count=n())

rm(pd_cite, patents, citations, patents_in, patents_in_url, citations_url)
