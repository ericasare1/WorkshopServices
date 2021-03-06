

#Empirical Example
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools, ggplot2, broom, gtsummary)



# Import data 
#-----------------------------------------------
# wtp values from a mixed sample: us and canadian
# wtp values are in 2017 us$ for the us sample
# wtp values from canadian side are mixed interms of years : 2001, 2011, ..can$

#) 1. have to convert all the monetary values of wtp in one standard $ demon 2018 C$


cpi_us <- read_csv("data/cpi_usa.csv")
cpi_can <- read_csv("data/cpi_canada.csv") 
exch_rate_us_to_can <- read_csv("data/exc_us_to_can.csv") 
wtp_orginal <- read_csv("data/wtp_raw.csv")
meta_data <- read_csv("data/finaldata.csv") 

#Part A
exchrate <- exch_rate_us_to_can %>%  #step 1: get in raw data 
  group_by(Year) %>% 
  summarize(av_monthly_excrate = mean(exchrate)) %>%
  column_to_rownames("Year") 

exchrate %>% View()

exchrate_2018 <- exchrate["2018", "av_monthly_excrate"]
exchrate_2018

avcpi_us_2018 <- 105.9447
avcpi_can_2018 <- 104

rel_cpi_us <- cpi_us %>% 
  group_by(year) %>% 
  summarise(average_us_cpi = mean(us_cpi)) %>% 
  mutate(rel_cpi = avcpi_us_2018/average_us_cpi) %>% 
  column_to_rownames("year")

rel_cpi_can <- cpi_can %>% 
  group_by(year) %>% 
  summarise(average_can_cpi = mean(cpi_can))  %>%
  mutate(rel_cpi = avcpi_can_2018/average_can_cpi) %>%
  column_to_rownames("year") 


us_study_relcpi <- rel_cpi_us["2017", "rel_cpi"] #
tkac <- rel_cpi_can["2001", "rel_cpi"]
trenholm <- rel_cpi_can["2007", "rel_cpi"]
pattisson <- rel_cpi_can["2008", "rel_cpi"]
lantz <- rel_cpi_can["2009", "rel_cpi"]
rudd <- rel_cpi_can["2011", "rel_cpi"]
he <- rel_cpi_can["2013", "rel_cpi"]
vossler <- rel_cpi_can["2014", "rel_cpi"]


transformed_wtp <- wtp_orginal %>%
  mutate(rel_cpi = 0,
         rel_cpi = ifelse(Author == "us_study", us_study_relcpi, rel_cpi),
         rel_cpi = ifelse(Author == "tkac", tkac, rel_cpi),
         rel_cpi = ifelse(Author == "trenholm", trenholm, rel_cpi),
         rel_cpi = ifelse(Author == "pattisson", pattisson, rel_cpi),
         rel_cpi = ifelse(Author == "lantz", lantz, rel_cpi),
         rel_cpi = ifelse(Author == "rudd", rudd, rel_cpi),
         rel_cpi = ifelse(Author == "he", he, rel_cpi),
         rel_cpi = ifelse(Author == "vossler", vossler, rel_cpi),
         wtp_original = ori_wtp_us_in_lg,
         wtp_original = ifelse(Author == "us_study", exp(wtp_original)-1, wtp_original),
         wtp_2018 = rel_cpi * wtp_original,
         wtp_2018can = ifelse(Author == "us_study", wtp_2018*exchrate_2018, wtp_2018),
         lnwtp = log(wtp_2018can)) %>% 
  select(obsid,lnwtp)

meta_data %>% select(-c(lnwtp)) %>% inner_join(transformed_wtp, by = "obsid") %>% 
  mutate(lnacres = log(q1-q0),
         canada = ifelse(canada == 1, "canada", "usa")) %>% 
  write.csv("data/final_28_11_20")

# Part B --Exploration
finaldata <- read_csv("data/final_28_11_20")

#Summary Stats for whole data
sum_whole <- finaldata %>% descr(stats = "common") %>% tb()

# Grouped summary statis
grouped_canada <- finaldata %>% group_by(canada) %>% descr(stats = "common") %>% tb()


#graphs
finaldata %>%
  ggplot(aes(x = lnacres, y = lnwtp, color=canada)) + geom_point() 
ggsave("figures/lnwtp_lnacres.png")

finaldata %>%  
  ggplot(aes(x = lnacres, y = lnwtp)) + 
  geom_point() +
  facet_wrap(~canada) + 
  theme_bw()

#Part C--Model estimation
lmfit <- lm(lnwtp ~ lnacres, data = finaldata)

lmfit_sum <- lm(lnwtp ~ lnacres, data = finaldata) %>% summary()

lm(lnwtp ~ lnacres, data = finaldata) %>% tidy() #model parameter estimates

lm(lnwtp ~ lnacres, data = finaldata) %>% glance() # goodness of fitness measures and related statistics. 

lm(lnwtp ~ lnacres, data = finaldata) %>% augment() # fitted values, residuals 

lm(lnwtp ~ lnacres, data = finaldata) %>% tbl_regression() # my favorite


t1 <- tbl_regression(lmfit) #awesome package...
