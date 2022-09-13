# Israel CBS social survey - data import and data manipulation R script


# Settings

Sys.setlocale("LC_ALL", "Hebrew") # This function is helpful for Hebrew compatibility

library(tidyverse)
library(janitor)

## Build datasets

### Lamas social survey (lss)

#### import raw datasets

lss_2015_raw <- read_csv("data/raw data/2015/H20151311Data.csv", na = c(NA,"999999","888888"))
lss_2016_raw <- read_csv("data/raw data/2016/H20161342Data.csv", na = c(NA,"999999","888888"))
lss_2017_raw <- read_csv("data/raw data/2017/H20171353Data.csv", na = c(NA,"999999","888888"))
lss_2018_raw <- read_csv("data/raw data/2018/H20181362Data.csv", na = c(NA,"999999","888888"))
lss_2019_raw <- read_csv("data/raw data/2019/H20191424data.csv", na = c(NA,"999999","888888"))
lss_2020_raw <- read_csv("data/raw data/2020/H20201452data.csv", na = c(NA,"999999","888888"))

lss_2015 <- lss_2015_raw %>% 
  janitor::clean_names() %>% 
  mutate(district = machoz,
         subdistrict = nafa,
         residence_type = zurat_yishuv,
         #trust_general = emun_klali %>% car::Recode("2=0"),
         life_satisfaction = merutze_chaim,
         trust_gov_raw = emun_memshala,
         trust_local_gov_raw = emun_rashut,
         #performance_health_raw = tifkud_briut,
         year = 2015,
         religion = dat,
         arab = ifelse(pop_group==2,1,0),
         ultra_orthodox = ifelse(datiut_yehudi==1,1,0),
         female = ifelse(minn==2,1,0),
         russian_ole = ifelse(yelid_brham==1,1,0),
         mizrahi_origin = ifelse(arab==0 & (semel_eretz %in% c(3,4)|
                                              semel_eretz_av_c %in% c(3,4)|
                                              semel_eretz_em_c %in% c(3,4)),1,0),
         household_income_n = hachnasa_kolelet_neto %>% as.numeric,
         age_g_n = gil %>% as.numeric,
         weight = nn) 


lss_2016 <- lss_2016_raw %>% 
  janitor::clean_names() %>% 
  mutate(district = machoz,
         subdistrict = nafa,
         residence_type = zurat_yishuv,
         #trust_general = emun_klali %>% car::Recode("2=0"),
         trust_gov_raw = emun_memshala,
         #trust_local_gov_raw = emun_rashut,
         trust_health_raw = emun_briut,
         #performance_health_raw = tifkud_briut,
         life_satisfaction = merutze_chaim,
         year = 2016,
         religion = dat,
         arab = ifelse(pop_group==2,1,0),
         ultra_orthodox = ifelse(datiut_yehudi==1,1,0),
         female = ifelse(minn==2,1,0),
         russian_ole = ifelse(yelid_brham==1,1,0),
         mizrahi_origin = ifelse(arab==0 & (semel_eretz %in% c(3,4)|
                                              semel_eretz_av_c %in% c(3,4)|
                                              semel_eretz_em_c %in% c(3,4)),1,0),
         household_income_n = hachnasa_kolelet_neto %>% as.numeric,
         age_g_n = gil %>% as.numeric,
         weight = nn) 




lss_2017 <- lss_2017_raw %>%
  janitor::clean_names() %>% 
  mutate(district = machoz,
         subdistrict = nafa,
         residence_type = zurat_yishuv,
         trust_general = emun_klali %>% car::Recode("2=0"),
         trust_gov_raw = emun_memshala,
         #trust_local_gov_raw = emun_rashut,
         trust_health_raw = emun_briut,
         #performance_health_raw = tifkud_briut,
         life_satisfaction = merutze_chaim,
         year = 2017,
         religion = dat,
         arab = ifelse(pop_group==2,1,0),
         ultra_orthodox = ifelse(datiut_yehudi==1,1,0),
         female = ifelse(minn==2,1,0),
         russian_ole = ifelse(yelid_brham==1,1,0),
         mizrahi_origin = ifelse(arab==0 & (semel_eretz %in% c(3,4)|
                                              semel_eretz_av_c %in% c(3,4)|
                                              semel_eretz_em_c %in% c(3,4)),1,0),
         household_income_n = hachnasa_kolelet_neto %>% as.numeric,
         age_g_n = gil %>% as.numeric,
         weight = nn) 




lss_2018 <- lss_2018_raw %>%
  janitor::clean_names() %>% 
  mutate(district = machoz,
         subdistrict = nafa,
         residence_type = zurat_yishuv,
         trust_general = emun_klali %>% car::Recode("2=0"),
         life_satisfaction = merutze_chaim,
         trust_gov_raw = emun_memshala,
         #trust_local_gov_raw = emun_rashut,
         trust_health_raw = emun_briut,
         performance_health_raw = tifkud_briut,
         performance_education_raw = tifkud_hinuch,
         performance_police_raw = tifkud_mishtara,
         performance_local_gov_raw = tifkud_iriya,
         year = 2018,
         religion = dat,
         arab = ifelse(pop_group==2,1,0),
         ultra_orthodox = ifelse(datiut_yehudi==1,1,0),
         female = ifelse(minn==2,1,0),
         russian_ole = ifelse(yelid_brham==1,1,0),
         mizrahi_origin = ifelse(arab==0 & (semel_eretz %in% c(3,4)|
                                              semel_eretz_av_c %in% c(3,4)|
                                              semel_eretz_em_c %in% c(3,4)),1,0),
         household_income_n = hachnasa_kolelet_neto %>% as.numeric,
         age_g_n = gil %>% as.numeric,
         weight = nn) 

lss_2019 <- lss_2019_raw %>% 
  janitor::clean_names() %>% 
  mutate(district = machoz,
         subdistrict = nafa,
         residence_type = zurat_yishuv,
         trust_general = emun_klali %>% car::Recode("2=0"),
         life_satisfaction = merutze_chaim,
         trust_gov_raw = emun_memshala,
         #trust_local_gov_raw = emun_rashut,
         trust_health_raw = emun_briut,
         performance_health_raw = tifkud_briut,
         performance_education_raw = tifkud_hinuch,
         performance_police_raw = tifkud_mishtara,
         performance_local_gov_raw = tifkud_iriya,
         year = 2019,
         religion = dat,
         arab = ifelse(pop_group==2,1,0),
         ultra_orthodox = ifelse(datiut_yehudi==1,1,0),
         female = ifelse(minn==2,1,0),
         russian_ole = ifelse(yelid_brham==1,1,0),
         mizrahi_origin = ifelse(arab==0 & (semel_eretz %in% c(3,4)|
                                              semel_eretz_av_c %in% c(3,4)|
                                              semel_eretz_em_c %in% c(3,4)),1,0),
         household_income_n = hachnasa_kolelet_neto %>% as.numeric,
         age_g_n = gil %>% as.numeric,
         weight = nn) 

lss_2020 <- lss_2020_raw %>% 
  janitor::clean_names() %>% 
  mutate(district = machoz,
         subdistrict = nafa,
         residence_type = zurat_yishuv,
         trust_general = emun_klali %>% car::Recode("2=0"),
         life_satisfaction = merutze_chaim,
         trust_gov_raw = emun_memshala,
         #trust_local_gov_raw = emun_rashut,
         trust_health_raw = emun_briut,
         performance_health_raw = tifkud_briut,
         performance_education_raw = tifkud_hinuch,
         performance_police_raw = tifkud_mishtara,
         performance_local_gov_raw = tifkud_iriya,
         year = 2020,
         religion = dat,
         arab = ifelse(pop_group==2,1,0),
         ultra_orthodox = ifelse(datiut_yehudi==1,1,0),
         female = ifelse(minn==2,1,0),
         russian_ole = ifelse(yelid_brham==1,1,0),
         mizrahi_origin = ifelse(arab==0 & (semel_eretz %in% c(3,4)|
                                              semel_eretz_av_c %in% c(3,4)|
                                              semel_eretz_em_c %in% c(3,4)),1,0),
         household_income_n = hachnasa_kolelet_neto %>% as.numeric,
         age_g_n = gil %>% as.numeric,
         weight = nn) 




lss_comb <-  bind_rows(
  lss_2015 %>% select(district:weight),
  lss_2016 %>% select(district:weight),
  lss_2017 %>% select(district:weight),
  lss_2018 %>% select(district:weight),
  lss_2019 %>% select(district:weight),
  lss_2020 %>% select(district:weight)
) %>% 
  mutate(household_income = household_income_n %>% car::Recode("1='0. 0-2500';
                                    2='1. 2500-4000';
                                    3='2. 4000-5000';
                                    4='3. 5000-6500';
                                    5='4. 6500-8000';
                                    6='5. 8000-10000';
                                    7='6. 10000-13000';
                                    8='7. 13000-17000';
                                    9='8. 17000-24000';
                                    10='9. 24000+';
                                    11=NA"),
         age_g = age_g_n %>% car::Recode("1='20-24';
                                    2='25-29';
                                    3='30-34';
                                    4='35-39';
                                    5='40-44';
                                    6='45-49';
                                    7='50-54';
                                    8='55-59';
                                    9='60-64';
                                    10='65-74';
                                    11='75+'")) %>% 
  mutate(trust_gov = (trust_gov_raw*-1+5-1)/(4-1),   #normalizing 0-1 
         trust_health = (trust_health_raw*-1+5-1)/(4-1),   #normalizing 0-1
         #trust_local_gov = (trust_local_gov_raw*-1+5-1)/(4-1),   #normalizing 0-1 
         performance_health = (performance_health_raw*-1+5-1)/(4-1),   #normalizing 0-1
         performance_education = (performance_education_raw*-1+5-1)/(4-1),   #normalizing 0-1
         performance_police = (performance_police_raw*-1+5-1)/(4-1),   #normalizing 0-1
         performance_local_gov = (performance_local_gov_raw*-1+5-1)/(4-1),   #normalizing 0-1
         sector = case_when(
           arab==1 ~ "3.Arab",
           ultra_orthodox==1 ~ "2.Ultra-orthodox",
           TRUE ~ "1.General"),
         sector_multiple = case_when(
           arab==1 & religion==3 ~ "Arab Christian",
           religion==4 ~ "Arab Druz",
           arab==1 & religion==2 & residence_type!=1~ "Arab Muslim",
           arab==1 & residence_type==1 ~ "Arab East Jerusalem",
           ultra_orthodox==1 ~ "Jewish Ultra-orthodox",
           russian_ole==1 ~ "Jewish russian origin",
           mizrahi_origin==1 & ultra_orthodox==0~ "Jewish Mizrahi origin",
           arab==0 & mizrahi_origin==0 & ultra_orthodox==0 ~ "Jewish Ashkenazi",
           TRUE ~ "Other")) %>% 
  mutate(trust_gov_bi = ifelse(trust_gov>0.5,1,0),
         trust_gov_low = ifelse(trust_gov<0.25,1,0),
         trust_health_bi = ifelse(trust_health>0.5,1,0),
         trust_health_low = ifelse(trust_health<0.25,1,0),
         performance_health_bi = ifelse(performance_health>0.5,1,0))%>%
  mutate(district_lab = district %>% car::Recode("1='1.Jerusalem';
                                                 2='2.North';
                                                 3='3.Haifa';
                                                 4='4.Center';
                                                 5='5.Tel Aviv';
                                                 6='6.South';
                                                 7='7.Yosh'")) 


rm(list=ls()[! ls() %in% c("lss_comb")])

save.image()
