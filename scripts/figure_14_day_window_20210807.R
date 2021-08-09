# Create 14-Day Trend Figures
# Gabriel Odom
# 2021-08-07


library(lubridate)
library(tidyverse)
library(ggpubr)


###  Data  ###
start_Date <- ymd("2020-04-28")
end_Date <- start_Date + 14

fldohTesting_df <-
	readRDS("./data/FLDoH_COVID19_testing_20210806.RDS") %>% 
	mutate(Source = "FLDoH") %>% 
	filter(County == "Miami-Dade") %>% 
	filter(Date <= end_Date) %>% 
	filter(Date >= start_Date)


###  Positive Tests: 14-Day Window  ###
totPos_gg <- 
	ggplot(data = fldohTesting_df) +
	aes(x = Date, y = Positive_7DayAve) +
	scale_y_continuous(limits = c(0, 600)) +
	labs(
		title = "Total Positive COVID-19 Tests by Day",
		subtitle = "Miami-Dade County, April 28 to May 12, 2020",
		y = "Total Count of Positive Tests, 7-Day Rolling Average"
	) + 
	geom_point() +
	stat_smooth(se = FALSE, method = "lm")

totPos_gg


###  Proportion Positive Tests: 14-Day Window  ###
propPos_gg <- 
	ggplot(data = fldohTesting_df) +
	aes(x = Date, y = PropPositive_7DayAve) +
	scale_y_continuous(limits = c(0, 20)) +
	labs(
		title = "Proportion Positive COVID-19 Tests by Day",
		subtitle = "Miami-Dade County, April 28 to May 12, 2020",
		y = "Proportion of Positive Tests, 7-Day Rolling Average"
	) + 
	geom_point() +
	stat_smooth(se = FALSE, method = "lm")

propPos_gg


###  Combine  ###
ggpubr::ggarrange(
	totPos_gg, propPos_gg,
	nrow = 1
)

ggsave(
	"figures/two_week_trends_in_May2020_20210807.png",
	width = 8, height = 4.5, units = "in"
)
