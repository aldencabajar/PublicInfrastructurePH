
explore_report:
	Rscript -e "source('Makefile.R');\
	explore_report <- bind_plans(preProcess, report[1]);\
	make(explore_report)"