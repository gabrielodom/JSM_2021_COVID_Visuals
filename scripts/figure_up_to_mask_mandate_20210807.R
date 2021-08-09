# Create Second Wave Figures
# Gabriel Odom
# 2021-08-07


library(lubridate)
library(tidyverse)
library(ggpubr)


###  Data  ###
end_Date <- ymd("2020-07-01")

fldohTesting_df <-
	readRDS("./data/FLDoH_COVID19_testing_20210806.RDS") %>% 
	mutate(Source = "FLDoH") %>% 
	filter(County == "Miami-Dade") %>% 
	filter(Date <= end_Date) 


###  Positive Tests: First Wave and Start of the Second Wave  ###
totPos_gg <- 
	ggplot(data = fldohTesting_df) +
	aes(x = Date, y = Positive_7DayAve) +
	scale_y_continuous(limits = c(0, 1800)) +
	labs(
		title = "Total Positive COVID-19 Tests by Day",
		subtitle = "Miami-Dade County, March 26 to July 1, 2020",
		y = "Total Count of Positive Tests, 7-Day Rolling Average"
	) + 
	geom_point() +
	stat_smooth(se = FALSE, method = "loess")

totPos_gg


###  Proportion Positive Tests: First Wave and Start of the Second Wave  ###
propPos_gg <- 
	ggplot(data = fldohTesting_df) +
	aes(x = Date, y = PropPositive_7DayAve) +
	scale_y_continuous(limits = c(0, 20)) +
	labs(
		title = "Proportion Positive COVID-19 Tests by Day",
		subtitle = "Miami-Dade County, March 26 to July 1, 2020",
		y = "Proportion of Positive Tests, 7-Day Rolling Average"
	) + 
	geom_point() +
	stat_smooth(se = FALSE, method = "loess")

propPos_gg


###  Combine  ###
ggpubr::ggarrange(
	totPos_gg, propPos_gg,
	nrow = 1
)

ggsave(
	"figures/trend_before_mask_mandate_20210807.png",
	width = 8, height = 4.5, units = "in"
)
