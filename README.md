# Concentration Data

##  EDF Mobile Monitoring

### Nitrogen Dioxide
<iframe width="700" height="700" src="http://rpubs.com/vatsouth/487204" frameborder="0" allowfullscreen> </iframe>

### Black Carbon
<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487201"> </iframe>

# Input Rates HIA

##  Asthma Emergency Room Visits

* Zip code: California Environmental Health Tracking Program (CEHTP)
  * Asthma ER visits <18 (pediatric)
  * Asthma ER visits >18 (adult)
  * Asthma ER visits all 
* Resolution: zip code
  * Missing:
  * County Asthma ED Visit Rates
  * Database: LGHC Indicators

### Asthma ER visits

<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487192"> </iframe>


## Asthma Prevalence

* AskCHIS Neighborhood Edition
* Output files:
  * CSV, raster, shape
  * Heart disease prevalence (self-report)
  * Asthma prevalence <18 (pediatric, self-report)
  * Asthma prevalence >18 (adult, self-report)
* Resolution: zip code
  * Missing: county (same database)
 
Data on asthma prevalence is from the California Health Interview Survey (CHIS) from the Ask Neighborhood CHIS. Prevalence was ascertained through “Has a doctor ever told you that you have asthma” for adults (greater than ages 18) and children (younger than age 18), for the year 2014, by zip code. The data was obtained by webscraping zip codes for all of California off of the publically available website. The rate is available by per 100 persons. Data that was missing for zip code was imputed with the county rate, also from CHIS, per 100 persons, for the year 2014.

Source: http://askchisne.ucla.edu/ask/SitePages/Login.aspx?ReturnUrl=%2fask%2f_layouts%2fAuthenticate.aspx%3fSource%3d%252Fask%252F%255Flayouts%252Fne%252Fdashboard%252Easpx&Source=%2Fask%2F_layouts%2Fne%2Fdashboard%2Easpx

 
### Asthma Prevalence

<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487191"> </iframe>



##  Mortality - Zip code

* Leading Causes of Death by ZIP Code
  * All-cause mortality
* Resolution: Zip code
  * Missing:
  * County Data CDC Wonder, 2016
  
Mortality counts are available by zip code for 2016 from the Leading Causes of Death by zip code dataset from the California Department of Public Health. Data were available for cardiovascular disease mortality, and all-cause. All-cause was determined by summing all available deaths reported in the dataset. As these were counts, rates were calculated using population data from the ACS 2012-2016 five year estimates of population by zip code. Missing data were imputed from the CDC Wonder’s 2016 rate of stroke and cardiovascular disease mortality. 

Mortality source: https://data.chhs.ca.gov/dataset/leading-causes-of-death-by-zip-code
Population denominator source: U.S. Census Bureau, 2012-2016 American Community Survey 5-Year Estimates, 2016
County rates (missing data for zip): CDC Wonder, 2016

### All-cause Mortality

## Alameda County Public Health Department Mortality Rates

* All-cause Mortality
  * Ages 25 to 99
  * Ages 65 to 99
* CVD Mortality
  * Ages 25 to 99
  * Ages 65 to 99

All-cause mortality and CVD mortality were created by the Alameda County Public Health Department for ages 25 and above and 65 and above. Rate denominators were calculated using the average population of 2012, 2014, and 2016. Rate numerators were counts of deaths from 2011 to 2017. Age-adjusted rates were calculated using standard age adjustment using the 2000 US Census standard population, such that the weighted rate is the age-specific rate (count of deaths per age group/average population/7) by the 2000 US age group proportion (population in the age group in 2000 divided by the sum of the population of all the age groups in 2000). This used standard five-year age groups with less than one year as the lowest age group and 85 and above as the highest.

### All-cause Mortality Alameda Census Block Group

#### All-cause mortality, ages 25-99
<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487194"> </iframe>

#### All-cause mortality, ages 65-99
<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487196"> </iframe>

### CVD Mortality Alameda Census Block Group

#### CVD Mortality, ages 25-99
<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487197"> </iframe>

#### CVD Mortality, ages 25-99
<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487199"> </iframe>


##  CVD Hospitalization

* CVD Hospitalizations by County

Asthma hospitalization rates data for ages 65 and above was extracted from BenMap-CE 1.4.14. BenMap is a software produced by the US Environmental Protection Agency (EPA) for conducting health impact assessment, and contains data inputs compiled by the EPA. We chose the incidence rate corresponding with the concentration response function in Peng et al. 2009 and black carbon, which is the hospitalization rate for cardiovascular disease less myocardial infarctions for those age 65 and above. EPA estimated the 2011-2014 incidence for CVD hospitalization based on discharge data from the Healthcare Cost and Utilization Project (HCUP), and provides calculated county-level data for age, sex, and ethnicity. The hospitalization rate for each county was determined by dividing an adjusted county-level hospitalization count by the Census estimated county-level population for 2010.

### CVD Hospitalizations
<iframe align = "center" width = "700" height = "700" src="http://rpubs.com/vatsouth/487189"> </iframe>






##  JS Annual Concentrations

### 2013

### 2014

### 2015

### 2016

### Mean 2013-2016

### Mean 2015-2016

### Daily 2016
