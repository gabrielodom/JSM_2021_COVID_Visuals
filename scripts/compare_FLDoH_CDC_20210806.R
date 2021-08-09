# Compare FLDoH Cases to CDC Cases
# Gabriel Odom
# 2021-08-06



######  Setup  ################################################################
###  Packages  ###
library(lubridate)
library(tidyverse)


###  Helper Functions  ###
RollUp <- function(.x, .w, .f = mean, ...){
	# Find "Rolling" values of a specified function
	# Inputs:
	#   .x: a vector (usually numeric) over which to "roll" the function .f
	#   .w: the window size
	#   .f: the function to "roll" over the values of .x
	#   ...: additional arguments to .f
	# Output: a vector the length of .x with the first (.w - 1) values set to NA
	#   and the remaining values equal to .f evaluated over the previous window.
	# Details: for a five-day moving average, set .w = 5. Then the moving average
	#   of .x at index 5 will be mean(.x[1], .x[2], ..., .x[5]).
	# Examples:
	#   # Rolling mean of first five integers
	#   RollUp(.x = 1:10, .w = 5)
	
	n <- length(.x)
	out <- rep(NA, times = n)
	class(out) <- class(.x)
	
	for(i in .w:n) {
		out[i] <- .f(.x[(i - .w + 1):i])
	}
	
	out
	
}



######  Import FLDoH Testing Data  ############################################

fldohMDC_df <- read_csv(
	file = "data_raw/FLDH_COVID19_cases_miamidade_20210530.csv"
) %>% 
	mutate(Date2 = as.POSIXct(Date, format = "%m/%d/%y")) %>% 
	mutate(Date3 = ymd(Date2)) %>% 
	mutate(Total = Positive + Negative) %>% 
	mutate(County = "Miami-Dade") %>% 
	mutate(
		Total_7DayAve = RollUp(.x = Total, .w = 7),
		Positive_7DayAve = RollUp(.x = Positive, .w = 7),
		# I hope that the CDC is finding the rolling averages of the counts, then
		#   calculating the proportions. Otherwise, a low-count day will have the
		#   same weight in the 7-day average proportion as a high-count day.
		PropPositive_7DayAve = 100 * Positive_7DayAve / Total_7DayAve
	) %>% 
	select(
		Date = Date3, County, Total_7DayAve, Positive_7DayAve, PropPositive_7DayAve
	)

fldohBC_df <- read_csv(
	file = "data_raw/FLDH_COVID19_cases_broward_20210530.csv"
) %>% 
	mutate(Date2 = as.POSIXct(Date, format = "%m/%d/%y")) %>% 
	mutate(Date3 = ymd(Date2)) %>% 
	mutate(Total = Positive + Negative) %>% 
	mutate(County = "Broward") %>% 
	mutate(
		Total_7DayAve = RollUp(.x = Total, .w = 7),
		Positive_7DayAve = RollUp(.x = Positive, .w = 7),
		PropPositive_7DayAve = 100 * Positive_7DayAve / Total_7DayAve
	) %>% 
	select(
		Date = Date3, County, Total_7DayAve, Positive_7DayAve, PropPositive_7DayAve
	)

fldohPBC_df <- read_csv(
	file = "data_raw/FLDH_COVID19_cases_palmbeach_20210530.csv"
) %>% 
	mutate(Date2 = as.POSIXct(Date, format = "%m/%d/%y")) %>% 
	mutate(Date3 = ymd(Date2)) %>% 
	mutate(Total = Positive + Negative) %>% 
	mutate(County = "Palm Beach") %>% 
	mutate(
		Total_7DayAve = RollUp(.x = Total, .w = 7),
		Positive_7DayAve = RollUp(.x = Positive, .w = 7),
		PropPositive_7DayAve = 100 * Positive_7DayAve / Total_7DayAve
	) %>% 
	select(
		Date = Date3, County, Total_7DayAve, Positive_7DayAve, PropPositive_7DayAve
	)

fldohTesting_df <- 
	bind_rows(fldohMDC_df, fldohBC_df, fldohPBC_df) %>% 
	arrange(Date)

saveRDS(fldohTesting_df, file = "data/FLDoH_COVID19_testing_20210806.RDS")

rm(fldohTesting_df, fldohPBC_df, fldohBC_df, fldohMDC_df)



######  Import CDC Data  ######################################################
cdcTesting_df <- read_csv(
	file = "data_raw/CDC_COVID_data_wrangled_20210802.csv"
) %>% 
	mutate(County = str_remove(County, pattern = " County, FL")) %>% 
	filter(County %in% c("Miami-Dade", "Broward", "Palm Beach")) %>% 
	mutate(
		Total_7DayAve =
			`Total RT-PCR diagnostic tests - last 7 days (may be an underestimate due to delayed reporting)` /
			  7,
		Positive_7DayAve = 
			`Viral (RT-PCR) lab test positivity rate - last 7 days` * Total_7DayAve,
		PropPositive_7DayAve = 
			`Viral (RT-PCR) lab test positivity rate - last 7 days` * 100
	) %>% 
	select(Date, County, Total_7DayAve, Positive_7DayAve, PropPositive_7DayAve)

saveRDS(cdcTesting_df, file = "data/CDC_COVID19_testing_20210806.RDS")

rm(list = ls())



######  Compare Testing  ######################################################

testing_df <- 
	bind_rows(
		readRDS("./data/FLDoH_COVID19_testing_20210806.RDS") %>% 
			# Earliest CDC records
			filter(Date >= "2020-12-17") %>% 
			mutate(Source = "FLDoH"),
		readRDS("./data/CDC_COVID19_testing_20210806.RDS") %>% 
			# Latest FLDoH records
			filter(Date <= "2021-05-30") %>% 
			mutate(Source = "CDC")
	) 


###  Total Number of Tests  ###
ggplot(data = testing_df) +
	aes(x = Date, y = Total_7DayAve, colour = Source) +
	geom_point() +
	facet_wrap(~County, ncol = 1)
	
ggplot(
	data = testing_df %>% 
		select(Date, County, Total_7DayAve, Source) %>% 
		pivot_wider(
			id_cols = c("Date", "County"),
			names_from = "Source",
			values_from = "Total_7DayAve"
		) %>% 
		mutate(
			ChangeRatio = FLDoH / CDC,
			ChangeDiff  = FLDoH - CDC
		)
) +
	aes(x = Date, y = ChangeRatio) +
	geom_point() +
	stat_smooth(se = FALSE) +
	facet_wrap(~County, ncol = 1)
	
# The values have gotten closer together since April
testing_df %>% 
	select(Date, County, Total_7DayAve, Source) %>% 
	filter(Date >= "2021-04-01") %>% 
	pivot_wider(
		id_cols = c("Date", "County"),
		names_from = "Source",
		values_from = "Total_7DayAve"
	) %>% 
	mutate(ChangeRatio = FLDoH / CDC) %>% 
	group_by(County) %>% 
	summarise(mean(ChangeRatio, na.rm = TRUE))
# To get "matching" total test data for each county:
#   - Miami-Dade: multiply CDC reported tests by 0.922
#   - Broward: multiply CDC reported tests by 1.07
#   - Palm Beach: multiply CDC reported tests by 1.12


###  Total Number of Positive Tests  ###
ggplot(data = testing_df) +
	aes(x = Date, y = Positive_7DayAve, colour = Source) +
	geom_point() +
	facet_wrap(~County, ncol = 1)

ggplot(
	data = testing_df %>% 
		select(Date, County, Positive_7DayAve, Source) %>% 
		pivot_wider(
			id_cols = c("Date", "County"),
			names_from = "Source",
			values_from = "Positive_7DayAve"
		) %>% 
		mutate(
			ChangeRatio = FLDoH / CDC,
			ChangeDiff  = FLDoH - CDC
		)
) +
	aes(x = Date, y = ChangeRatio) +
	geom_point() +
	stat_smooth(se = FALSE) +
	facet_wrap(~County, ncol = 1)

# Relatively stable since February
testing_df %>% 
	select(Date, County, Positive_7DayAve, Source) %>% 
	filter(Date >= "2021-02-01") %>% 
	pivot_wider(
		id_cols = c("Date", "County"),
		names_from = "Source",
		values_from = "Positive_7DayAve"
	) %>% 
	mutate(ChangeRatio = FLDoH / CDC) %>% 
	group_by(County) %>% 
	summarise(mean(ChangeRatio, na.rm = TRUE))
# To get "matching" total positive test data for each county:
#   - Miami-Dade: multiply CDC reported tests by 0.601
#   - Broward: multiply CDC reported tests by 0.682
#   - Palm Beach: multiply CDC reported tests by 0.863
# Recall: FLDoH only counts the first positive test, while the CDC counts all
#   positive tests. Therefore, if a person is tested regularly for their job, 
#   FLDoH will count only the first positive test, and not any subsequent tests


###  Proportion of Positive Tests  ###
ggplot(data = testing_df) +
	aes(x = Date, y = PropPositive_7DayAve, colour = Source) +
	geom_point() +
	facet_wrap(~County, ncol = 1)

ggplot(
	data = testing_df %>% 
		select(Date, County, PropPositive_7DayAve, Source) %>% 
		pivot_wider(
			id_cols = c("Date", "County"),
			names_from = "Source",
			values_from = "PropPositive_7DayAve"
		) %>% 
		mutate(
			ChangeRatio = FLDoH / CDC,
			ChangeDiff  = FLDoH - CDC
		)
) +
	aes(x = Date, y = ChangeRatio) +
	geom_point() +
	stat_smooth(se = FALSE) +
	facet_wrap(~County, ncol = 1)

# That is NOT STABLE at all. DO NOT USE


######  Try to Match the Proportion Positive  #################################
# This mapping fits the data after April 1st
ggplot(data = testing_df) +
	aes(x = Date, y = PropPositive_7DayAve, colour = Source) +
	geom_point() +
	facet_wrap(~County, ncol = 1)

testingMapped_df <- 
	testing_df %>% 
	filter(Source == "CDC") %>% 
	mutate(
		totalTestsMapped = case_when(
			County == "Miami-Dade" ~ Total_7DayAve * 0.922,
			County == "Broward" ~ Total_7DayAve * 1.07,
			County == "Palm Beach" ~ Total_7DayAve * 1.12
		)
	) %>% 
	mutate(
		positiveTestsMapped = case_when(
			County == "Miami-Dade" ~ Positive_7DayAve * 0.601,
			County == "Broward" ~ Positive_7DayAve * 0.682,
			County == "Palm Beach" ~ Positive_7DayAve * 0.863
		)
	) %>% 
	mutate(
		propPositiveMapped = 100 * positiveTestsMapped / totalTestsMapped
	) %>% 
	select(
		Date, County,
		Total_7DayAve = totalTestsMapped,
		Positive_7DayAve = positiveTestsMapped,
		PropPositive_7DayAve = propPositiveMapped
	) %>% 
	mutate(Source = "FLDoH Estimated")


###  Total Number of Tests  ###
ggplot(
	data = bind_rows(testing_df, testingMapped_df)
) +
	aes(x = Date, y = Total_7DayAve, colour = Source) +
	geom_point(alpha = 0.5) +
	facet_wrap(~County, ncol = 1)

###  Total Number of Positive Tests  ###
ggplot(
	data = bind_rows(testing_df, testingMapped_df)
) +
	aes(x = Date, y = Positive_7DayAve, colour = Source) +
	geom_point(alpha = 0.5) +
	facet_wrap(~County, ncol = 1)

###  Proportion of Positive Tests  ###
ggplot(
	data = bind_rows(testing_df, testingMapped_df)
) +
	aes(x = Date, y = PropPositive_7DayAve, colour = Source) +
	geom_point(alpha = 0.5) +
	facet_wrap(~County, ncol = 1)

# IT'S BEAUTIFUL!!!
# Summary of the weights:
#  - Miami-Dade: 0.922 * Total; 0.601 * Positive
#  - Broward   : 1.07  * Total; 0.682 * Positive
#  - Palm Beach: 1.12  * Total; 0.863 * Positive

