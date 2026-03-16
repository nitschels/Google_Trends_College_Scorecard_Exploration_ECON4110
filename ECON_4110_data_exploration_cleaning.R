library(rio)
library(dplyr)
library(lubridate)
library(fixest)
library(stringr)
library(lubridate)
library(ggplot2)



files <- list.files("Lab3_Rawdata", pattern = 'trends_up_to', full.names = TRUE)
gtrends <- import_list(files, rbind = TRUE, fill = TRUE)

gtrends$monthorweek <- ymd(str_sub(gtrends$monthorweek, 1, 10))

gtrends <- gtrends %>% mutate(month = floor_date(as.Date(monthorweek), unit = "month"))


gtrends <- gtrends %>%
  group_by(schid, schname, keyword, keynum, month) %>%
  summarize(index = mean(index, na.rm = TRUE), .groups = "drop")

gtrends <- gtrends %>%
  group_by(schname, keyword) %>%
  mutate(index_standardized = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE))


scorecard <- import("Most+Recent+Cohorts+(Scorecard+Elements).csv")


id_name_link_clean <- scorecard %>%
  group_by(INSTNM) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  ungroup()

gtrends <- gtrends %>%
  mutate(schname = str_to_lower(schname))

id_name_link_clean <- id_name_link_clean %>%
  mutate(INSTNM = str_to_lower(INSTNM))

df <- gtrends %>%
  inner_join(id_name_link_clean, by = c("schname" = "INSTNM"))

final_df <- df %>%
  inner_join(scorecard, by = "UNITID")

final_df <- final_df %>%
  filter(PREDDEG.x == 3)


# RESEARCH QUESTION: Among colleges that predominantly grant bachelor's degrees
# did the release of the Scorecard shift student interest to high-earnings
# colleges relative to low earnings ones


ana_df <- final_df %>%
  select(schname, month, index, index_standardized, 'md_earn_wne_p10-REPORTED-EARNINGS.y')

ana_df <- na.omit(ana_df)

ana_df <- ana_df %>%
  rename(earnings = 'md_earn_wne_p10-REPORTED-EARNINGS.y')

ana_df$earnings <- as.integer(ana_df$earnings)
#Need to convert to integer since earnings values are 'characters'

ana_df <- na.omit(ana_df)
# remove na values

summary(ana_df$earnings)

ana_df$high_earnings <- ifelse(ana_df$earnings > 43158, TRUE, FALSE)
ana_df$post_treatment <- ifelse(ana_df$month >= ymd('2015-09-12'), TRUE, FALSE)
# 43158 comes from the mean of the earnings in my df

sc_did <- feols(index ~ high_earnings + post_treatment + high_earnings:post_treatment, data = ana_df)
etable(sc_did)

clean_df <- ana_df %>%
  group_by(month, high_earnings) %>%
  summarise(avg_index = mean(index_standardized, na.rm = TRUE))

ggplot(clean_df, aes(x = month, y = avg_index, color = high_earnings)) + geom_line() + geom_vline(xintercept = ymd("2015-09-12"))
etable(sc_did)

qtiles <- quantile(ana_df$earnings, probs = seq(0,1,0.25), na.rm = TRUE)
if(length(unique(qtiles)) == length(qtiles)) {
  quartile_df <- ana_df %>%
    mutate(earnings_quartile = cut(earnings, breaks = qtiles, include.lowest = TRUE, labels = c(1,2,3,4)))
}

cq_df <- quartile_df %>%
  group_by(month, earnings_quartile) %>%
  summarise(avg_index = mean(index_standardized, na.rm = TRUE))

feols(index ~ earnings_quartile + post_treatment + earnings_quartile:post_treatment, data = quartile_df)

ggplot(cq_df, aes(x = month, y = avg_index, color = earnings_quartile)) + geom_line() + geom_vline(xintercept = ymd("2015-09-12"))



