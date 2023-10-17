NYPD Shooting Incident Data Analysis
================
Maciej Dwornik
2023-09-22

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(ggplot2)
library(dplyr)
```

I used the NYPD Shooting Incident Data as well as another data set
regarding NYC population to analyze which borough had the most
incidents, as well as how many incidents there are relative to the
population.

``` r
data = read_csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
```

    ## Rows: 27312 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): OCCUR_DATE, BORO, LOC_OF_OCCUR_DESC, LOC_CLASSFCTN_DESC, LOCATION...
    ## dbl   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
    ## lgl   (1): STATISTICAL_MURDER_FLAG
    ## time  (1): OCCUR_TIME
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pop_data = read_csv("https://data.cityofnewyork.us/api/views/xywu-7bv9/rows.csv?accessType=DOWNLOAD")
```

    ## Rows: 6 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): Age Group, Borough
    ## dbl (20): 1950, 1950 - Boro share of NYC total, 1960, 1960 - Boro share of N...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Cleaning, Analyzing, and Visualizing the Data

From the NYPD Shooting Incident Data I derived the number of incidents
from each borough. We can see that Brooklyn had the most total
incidents, followed by Bronx, Queens, Manhattan, and Staten Island. No
data cleaning was necessary in this step.

``` r
table(data$BORO)
```

    ## 
    ##         BRONX      BROOKLYN     MANHATTAN        QUEENS STATEN ISLAND 
    ##          7937         10933          3572          4094           776

``` r
barplot(table(data$BORO),
  xlab = "Borough", 
  ylab = "Incidents",
  main = "Incidents by Borough",
  cex.names = 0.8)
```

![](NYPD-Shooting-Incident-Data-Analysis_files/figure-gfm/Borough%20data-1.png)<!-- -->

I knew each borough was not the same size and did not have the same
population, so I decided to find population data of each borough to
compare which borough has the most incidents per 100,000 residents.

Here we can see that Bronx actually had the most incidents per 100,000
residents, even though Brooklyn had the most incidents total (as per
previous graph). Queens and Manhattan also swapped spots. Queens had
more incidents total but Manhattan had more incidents per 100,000
residents.

In this step there was a bit of data wrangling I had to do. Firstly, the
column names were not working so I had to rename them. The data types
were also problematic and had to be changed. Another issue that came up
was that the borough names in the population data were in rows, while in
the other data set they were in columns so I had to switch them around.

``` r
colnames(pop_data)[2] = "BORO"
pop_data = pop_data %>%
  rename(`2020` = "2020") %>%
  mutate(`2020` = as.numeric(`2020`)) %>%
  select(BORO, `2020`)

incidents_per_boro = table(data$BORO)
pop_data_cols = pop_data %>%
  pivot_wider(names_from = BORO, values_from = `2020`)
colnames(pop_data_cols)[1] = "TOTAL"
pop_data_cols = pop_data_cols %>%
  select(-"TOTAL")

pop_values = unlist(pop_data_cols)

incidents_per_pop = data.frame(
  Borough = names(pop_data_cols),
  Incidents = as.numeric(incidents_per_boro),
  Population = pop_values
)

incidents_per_pop$IncidentsPer100k <- incidents_per_pop$Incidents / (incidents_per_pop$Population / 100000)

print(incidents_per_pop)
```

    ##                     Borough Incidents Population IncidentsPer100k
    ## Bronx                 Bronx      7937    1446788         548.5945
    ## Brooklyn           Brooklyn     10933    2648452         412.8072
    ## Manhattan         Manhattan      3572    1638281         218.0334
    ## Queens               Queens      4094    2330295         175.6859
    ## Staten Island Staten Island       776     487155         159.2922

``` r
barplot(incidents_per_pop$IncidentsPer100k, 
  names.arg = incidents_per_pop$Borough, 
  xlab = "Borough", 
  ylab = "Incidents per 100,000",
  main = "Incidents per 100,000 by Borough")
```

![](NYPD-Shooting-Incident-Data-Analysis_files/figure-gfm/pop_data-1.png)<!-- -->

I wanted to further analyze this data and see how it differs from year
to year.

``` r
data$OCCUR_DATE = as.character(data$OCCUR_DATE)
date_split = strsplit(data$OCCUR_DATE, "/")
data$Year = as.integer(sapply(date_split, function(x) x[3]))

incident_count = data %>%
  group_by(BORO, Year) %>%
  summarize(Incidents = n())
```

    ## `summarise()` has grouped output by 'BORO'. You can override using the
    ## `.groups` argument.

``` r
ggplot(incident_count, aes(x = Year, y = Incidents, fill = BORO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "NYPD Shooting Incidents by Borough and Year",
    x = "Year",
    y = "Incident Count",
    fill = "Borough"
  )
```

![](NYPD-Shooting-Incident-Data-Analysis_files/figure-gfm/Analyzing%20the%20data%20by%20year-1.png)<!-- -->

Here we do the same thing we did with the initial analysis, so we use
the number of incidents (this time it is incidents per year) and
calculate how many incidents happened per 100,000 residents of each
borough. It seems that the data is pretty consistent throughout the
years, with Bronx once again having the most incidents per 100,000
residents, while Staten Island and Queens have the least.

``` r
pop_data <- pop_data %>%
  filter(BORO != "NYC Total") %>%
  mutate(BORO = toupper(BORO))
data_with_pop = data %>%
  left_join(pop_data, by = c("BORO" = "BORO")) %>%
  group_by(BORO, Year) %>%
  reframe(Incidents = n(), Population = `2020`) %>%
  mutate(IncidentsPer100k = (Incidents / (Population / 100000)))

ggplot(data_with_pop, aes(x = Year, y = IncidentsPer100k, fill = BORO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "NYPD Shooting Incidents per 100,000 Residents by Borough and Year",
    x = "Year",
    y = "Incidents per 100,000 Residents",
    fill = "Borough"
  )
```

![](NYPD-Shooting-Incident-Data-Analysis_files/figure-gfm/Per%20year%20per%20100k-1.png)<!-- -->

## Bias

There is a potential bias problem here because the shooting incident
data spans over many years, however, I only used the 2020 population
data. The population of course changes over the years, as well as
different boroughs having different amounts of incidents per year. I
could have used incident data from just one year for the initial
analysis, however, I think having more data to work with was more
important. For the per year data I could have possibly used different
population data for different years, but the population data I had was
only for each decade. I could have technically used the 2010 population
data for years that were closer to 2010, and 2020 data for years that
were closer to 2020 but I chose not to for efficiency’s sake.

## Model

I also constructed a model predicting incidents per 100,000 residents.
The blue bar represents the original data, while the red bar represents
the prediction model. We can see that in the Bronx and Brooklyn boroughs
the actual number of incidents exceeds the predicted number of
incidents, while the rest of the boroughs the number of incidents is
lower than expected.

``` r
mod = lm(IncidentsPer100k ~ Population, data=incidents_per_pop)
summary(mod)
```

    ## 
    ## Call:
    ## lm(formula = IncidentsPer100k ~ Population, data = incidents_per_pop)
    ## 
    ## Residuals:
    ##         Bronx      Brooklyn     Manhattan        Queens Staten Island 
    ##        258.84         63.18        -81.27       -158.09        -82.66 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 2.177e+02  2.119e+02   1.027     0.38
    ## Population  4.982e-05  1.134e-04   0.439     0.69
    ## 
    ## Residual standard error: 191 on 3 degrees of freedom
    ## Multiple R-squared:  0.06044,    Adjusted R-squared:  -0.2528 
    ## F-statistic: 0.193 on 1 and 3 DF,  p-value: 0.6902

``` r
incidents_per_pop = incidents_per_pop %>% 
  mutate(Prediction = predict(mod))

incidents_per_pop %>%
  ggplot(aes(x = Borough, y = IncidentsPer100k)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(x = Borough, y = Prediction), stat = "identity", fill = "red", alpha = 0.5) +
  labs(
  title = "Actual vs. Predicted Incidents per 100,000 by Borough",
  x = "Borough",
  y = "Incidents per 100,000"
)
```

![](NYPD-Shooting-Incident-Data-Analysis_files/figure-gfm/mod-1.png)<!-- -->

``` r
incidents_per_pop %>%
  select(-Borough)
```

    ##               Incidents Population IncidentsPer100k Prediction
    ## Bronx              7937    1446788         548.5945   289.7590
    ## Brooklyn          10933    2648452         412.8072   349.6293
    ## Manhattan          3572    1638281         218.0334   299.2997
    ## Queens             4094    2330295         175.6859   333.7778
    ## Staten Island       776     487155         159.2922   241.9474

## Conclusion

In conclusion, the data suggests that even though Brooklyn had the most
incidents, the Bronx actually had the most incidents per 100,000
residents. We also saw that Queens and Staten Island were on the lower
end of incidents per 100,000 residents, even though Queens was third
overall in total incidents.
