# Scale CDC Testing to be Comparable to FLDoH
# Gabriel Odom
# 2021-08-06

# The CDC uses different defitions and criteria when recording positive and
#   total COVID-19 tests in a county. Based on the script
#   "scripts/compare_FLDoH_CDC_20210806.R",
#   we determined that the testing counts reported by the CDC should be adjusted
#   in the following manner to more accurately reflect the numbers reported by
#   the FLDoH:
#   - Miami-Dade: 0.922 * Total; 0.601 * Positive
#   - Broward   : 1.07  * Total; 0.682 * Positive
#   - Palm Beach: 1.12  * Total; 0.863 * Positive
# While potentially controversial, this re-weighting procedure will allow us to
#   make "apples to apples" comparisons between the FLDoH data recorded over
#   March 2020 -- May 2021 and the CDC data recorded since December 2020.

library(lubridate)
library(tidyverse)


#####  Data Import and Scaling  ###############################################
fldohTesting_df <-
	readRDS("./data/FLDoH_COVID19_testing_20210806.RDS") %>% 
	mutate(Source = "FLDoH")
cdcTesting_df <- 
 	readRDS("./data/CDC_COVID19_testing_20210806.RDS") %>% 
 	mutate(Source = "CDC")

fldohTestingEstimated_df <- 
	cdcTesting_df %>% 
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

fldohTestingUpdated_df <- bind_rows(fldohTesting_df, fldohTestingEstimated_df)



######  Plots  ################################################################

###  Total Tests  ###
ggplot(data = fldohTestingUpdated_df) +
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = Total_7DayAve, colour = Source) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	labs(
		title = "Total COVID-19 Tests by County",
		y = "Total Tests, 7-Day Rolling Average"
	) +
	geom_point(alpha = 0.5) +
	facet_wrap(~County, ncol = 1, scales = "free")

ggsave(
	"figures/total_COVID19_tests_imputed_20210806.pdf",
	width = 5, height = 7, units = "in"
)
 

###  Total Positive Tests  ###
ggplot(data = fldohTestingUpdated_df) +
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = Positive_7DayAve, colour = Source) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	labs(
		title = "Total Positive COVID-19 Tests by County",
		y = "Total Positive Tests, 7-Day Rolling Average"
	) +
	geom_point(alpha = 0.5) +
	facet_wrap(~County, ncol = 1, scales = "free")

ggsave(
	"figures/positive_COVID19_tests_imputed_20210806.pdf",
	width = 5, height = 7, units = "in"
)


###  Proportion Positive Tests  ###
ggplot(data = fldohTestingUpdated_df) +
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = PropPositive_7DayAve, colour = Source) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	labs(
		title = "Proportion of Positive COVID-19 Tests by County",
		y = "Proportion of Positive Tests, 7-Day Rolling Average"
	) +
	geom_point(alpha = 0.5) +
	facet_wrap(~County, ncol = 1, scales = "free")

ggsave(
	"figures/prop_positive_COVID19_tests_imputed_20210806.pdf",
	width = 5, height = 7, units = "in"
)



######  South Florida Grouped Plots  ##########################################

sflTesting_df <- 
	fldohTestingUpdated_df %>% 
	filter(Source == "FLDoH") %>% 
	group_by(Date) %>% 
	summarise(
		Total = sum(Total_7DayAve),
		Positive = sum(Positive_7DayAve),
		PropPositive = 100 * Positive / Total
	)

###  Total Tests  ###
ggplot(data = sflTesting_df) +
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = Total) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	labs(
		title = "Total COVID-19 Tests per Day in South Florida",
		y = "Total Tests, 7-Day Rolling Average"
	) +
	geom_point(alpha = 0.5) 


###  Total Positive Tests  ###
ggplot(data = sflTesting_df) +
	# theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = Positive, colour = PropPositive) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	scale_colour_gradient(
		low = "white", high = "red"
	) +
	labs(
		title = "Total Positive COVID-19 Tests per Day in South Florida",
		y = "Total Positive Tests, 7-Day Rolling Average"
	) +
	geom_point(alpha = 0.5) 

ggsave(
	"figures/positive_COVID19_tests_by_prop_20210807.png",
	width = 12, height = 4, units = "in"
)


###  Proportion Positive Tests  ###
ggplot(data = sflTesting_df) +
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = PropPositive, colour = Source) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	labs(
		title = "Proportion of Positive COVID-19 Tests in South Florida",
		y = "Proportion of Positive Tests, 7-Day Rolling Average"
	) +
	geom_point(alpha = 0.5) 


###  Now with CDC Data  ###
sflTesting2_df <- 
	fldohTestingUpdated_df %>% 
	group_by(Date, Source) %>% 
	summarise(
		Total = sum(Total_7DayAve),
		Positive = sum(Positive_7DayAve),
		PropPositive = 100 * Positive / Total
	)

ggplot(data = sflTesting2_df) +
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)) +
	aes(x = Date, y = Positive, alpha = PropPositive, fill = Source) +
	scale_x_date(breaks = "month", date_labels = "%b '%y") +
	# scale_colour_gradient(low = "white", high = "red") +
	# scale_shape_manual(values = c(21, 23)) +
	scale_fill_manual(values = c("#FF0000", "#0000FF")) +
	labs(
		title = "Total Positive COVID-19 Tests per Day in South Florida",
		y = "Total Positive Tests, 7-Day Rolling Average",
		alpha = "Proportion of Positive Tests"
	) +
	geom_point(shape = 21) 

ggsave(
	"figures/positive_COVID19_tests_CDC_estimate_20210807.png",
	width = 12, height = 4, units = "in"
)
