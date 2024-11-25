# rus-education

## Goal

To find correlations between eduation level of russian citizens and the other parameters.

##  Roadmap

[X] Download data from Russian Census Agency
	[X] Education

[X] Transform the data
	[X] NAs
	[X] Split the df
	[-] Outliers

[ ] Analyse the data
	[X] Regional Analysis
		[X] Calculate total population by region
		[X] Education level distribution by regions
	[ ] Urban vs Rural Analysis
		[ ] Compare education levels between urban and rural areas
	[ ] Labor Force Analysis
		[ ] Calculate labor force participation rate
		[ ] Employment rate by education level
	[ ] Corrplot - correlation between features

[ ] Hypothesis testing
	[ ] Impact of Education Levels on Employment Rate:
		- Correlation analysis between education levels and employment rates (There is no correlation between education levels and employment rates (ρ = 0))
		- Multiple linear regression with employment rate as dependent variable and different education levels as predictors (Education levels have no significant effect on employment rates)
	[ ] Urban-Rural Educational and Employment Disparities:
		- Chi-square test of independence to test association between region type (urban/rural) and education levels (There is no association between region type (urban/rural) and education levels)
		- Correlation analysis between urban/rural population percentages and employment metrics (There is no correlation between urbanization level and employment metrics)
		- Simple linear regression to predict employment rates from urbanization level 
		- ANOVA to compare education levels across region types (There are no significant differences in education levels between urban and rural areas)
	[ ] Regional Economic Performance and Labor Force Characteristics:
		- Multiple linear regression with labor force participation as dependent variable (Regional characteristics have no significant effect on labor force participation)
		- Check for multicollinearity among predictors
		- ANOVA to compare labor force participation across regions (There are no significant differences in labor force participation across regions)
		- Chi-square test for independence between region type and employment status (There is no association between region type and employment status (variables are independent))
 


## Features

all_pop	- Total Population older 15 YO ("Все население частных домохозяйств в возрасте 15 лет и более") 
ind_edu - Those Who Indicated Education ("Указавшие уровень образования")	
phd_edu	- PhD and equivalent level ("кадры высшей квалификации")
hig_edu	- Higher education ("высшее")
mast_edu - Master's degree ("магистратура")
spec_edu - Specialist degree ("специалитет") *
bac_edu	- Bachelor degree ("бакалавриат")
not_compl_edu - Not Completed higher education ("неоконченное высшее образование") **
prof_edu - Professional Education ("среднее профессиональное")	
prof_edu_mid_spec - Professional Education - Middle Specialist ("специалист среднего звена")
prof_edu_work - Professional Education - Worker ("квалифицированный рабочий, служащий")
mid_edu - Middle Education ("среднее")
mid_edu_gen - Middle Education - General ("основное")
mid_edu_prim - Middle Education - Primary and lower ("начальное и ниже")
non_edu	- Non-educated ("не имеющие образования")
not_ind_edu - Not Indicated Education ("Не указавшие уровень образования")


* Before recent years, Russian higher education for non-medical fields consisted of 'Specialist' degree which is 5-year study program equivalent to Master's; 'Postgraduate' which is a 3-years equivalent to PhD; and 'Doctoral', which is concidered higher then PhD, and gives you title of the 'Doctor of Science'. Now with the transition to Master's and Bachelor system, we see 'Specialist', 'Master's' and 'Bachelor' coexist; as well as 'Postgraduate' and 'Doctoral'.

** 'Not comleated' is a common term in Russian HIgher Education considering those who still studying in Uni, as well as those, who droped the Uni.