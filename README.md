Analysis of Opiod drug overdoses data from the CDC
==================================================

Carlos Santillan

<a href="mailto:csantill@gmail.com" class="email">csantill@gmail.com</a>

<a href="https://www.linkedin.com/in/carlos-santillan/" class="uri">https://www.linkedin.com/in/carlos-santillan/</a>

    ## Warning: package 'knitr' was built under R version 3.6.3

    ## Warning: package 'tidyverse' was built under R version 3.6.3

    ## Warning: package 'ggplot2' was built under R version 3.6.3

    ## Warning: package 'tibble' was built under R version 3.6.3

    ## Warning: package 'tidyr' was built under R version 3.6.3

    ## Warning: package 'readr' was built under R version 3.6.3

    ## Warning: package 'purrr' was built under R version 3.6.3

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## Warning: package 'stringr' was built under R version 3.6.3

    ## Warning: package 'forcats' was built under R version 3.6.3

Load the data
-------------

Load CDC data \#\#\# Load Monthly Data

Dataset: Underlying Cause of Death, 1999-2018 Query Parameters: ICD-10
Codes:

-   X40 (Accidental poisoning by and exposure to nonopioid analgesics,
    antipyretics and antirheumatics)
-   X41 (Accidental poisoning by and exposure to antiepileptic,
    sedative-hypnotic, antiparkinsonism and psychotropic drugs, not
    elsewhere classified)
-   X42 (Accidental poisoning by and exposure to narcotics and
    psychodysleptics \[hallucinogens\], not elsewhere classified)
-   X43 (Accidental poisoning by and exposure to other drugs acting on
    the autonomic nervous system)
-   X44 (Accidental poisoning by and exposure to other and unspecified
    drugs, medicaments and biological substances)
-   X60 (Intentional self-poisoning by and exposure to nonopioid
    analgesics, antipyretics and antirheumatics)
-   X61 (Intentional self-poisoning by and exposure to antiepileptic,
    sedative-hypnotic, antiparkinsonism and psychotropic drugs, not
    elsewhere classified)
-   X62 (Intentional self-poisoning by and exposure to narcotics and
    psychodysleptics \[hallucinogens\], not elsewhere classified)
-   X63 (Intentional self-poisoning by and exposure to other drugs
    acting on the autonomic nervous system)
-   X64 (Intentional self-poisoning by and exposure to other and
    unspecified drugs, medicaments and biological substances)
-   Y10 (Poisoning by and exposure to nonopioid analgesics, antipyretics
    and antirheumatics, undetermined intent)
-   Y11 (Poisoning by and exposure to antiepileptic, sedative-hypnotic,
    antiparkinsonism and psychotropic drugs, not elsewhere classified,
    undetermined intent);
-   Y12 (Poisoning by and exposure to narcotics and psychodysleptics
    \[hallucinogens\], not elsewhere classified, undetermined intent)
-   Y13 (Poisoning by and exposure to other drugs acting on the
    autonomic nervous system, undetermined intent)
-   Y14 (Poisoning by and exposure to other and unspecified drugs,
    medicaments and biological substances, undetermined intent)

Group By:

-   State
-   Year
-   Month
-   ICD Chapter

Show Totals:

-   Disabled Show Zero Values:

-   False Show Suppressed:

-   False Calculate Rates Per:

-   100,000

Rate Options:

Default intercensal populations for years 2001-2009 (except Infant Age
Groups)

Query Date: Apr 29, 2020 6:40:12 PM

Suggested Citation: Centers for Disease Control and Prevention, National
Center for Health Statistics. Underlying Cause of Death 1999-2018 on CDC
WONDER Online Database, released in 2020. Data are from the Multiple
Cause of Death Files, 1999-2018, as compiled from data provided by the
57 vital statistics jurisdictions through the Vital Statistics
Cooperative Program. Accessed at
<a href="http://wonder.cdc.gov/ucd-icd10.html" class="uri">http://wonder.cdc.gov/ucd-icd10.html</a>
on Apr 29, 2020 6:40:12 PM

Messages:

1.  Totals are not available for these results due to suppression
    constraints. More Information:
    <a href="http://wonder.cdc.gov/wonder/help/faq.html#Privacy" class="uri">http://wonder.cdc.gov/wonder/help/faq.html#Privacy</a>.
2.  Rows with suppressed Deaths are hidden. Use Quick Options above to
    show suppressed rows.

Caveats:

1.  Population and rates are labeled ‘Not Applicable’ when Autopsy,
    Place of Death, Weekday or Month are grouped by or limited, due to
    lack of a valid population. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#NotApplicableRates" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#NotApplicableRates</a>.
2.  As of April 3, 2017, the underlying cause of death has been revised
    for 125 deaths in 2014. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#2014-Revision" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#2014-Revision</a>.
3.  Circumstances in Georgia for the years 2008 and 2009 have resulted
    in unusually high death counts for the ICD-10 cause of death code
    R99, Other ill-defined and unspecified causes of mortality. Caution
    should be used in interpreting these data. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#Georgia-Reporting-Anomalies" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#Georgia-Reporting-Anomalies</a>.
4.  Circumstances in New Jersey for the year 2009 have resulted in
    unusually high death counts for the ICD-10 cause of death code R99,
    Other ill-defined and unspecified causes of mortality and therefore
    unusually low death counts in other ICD-10 codes, most notably R95,
    Sudden Infant Death Syndrome and X40-X49, Unintentional poisoning.
    Caution should be used in interpreting these data. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#New-Jersey-Reporting-Anomalies" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#New-Jersey-Reporting-Anomalies</a>.
5.  Circumstances in California resulted in unusually high death counts
    for the ICD-10 cause of death code R99, Other ill-defined and
    unspecified causes of mortality for deaths occurring in years 2000
    and 2001. Caution should be used in interpreting these data. More
    information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#California-Reporting-Anomalies" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#California-Reporting-Anomalies</a>.
6.  These items in the results table are not fully selected: External
    causes of morbidity and mortality. The Query Description lists the
    actual values selected.
7.  Data are Suppressed when the data meet the criteria for
    confidentiality constraints. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#Assurance" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#Assurance</a>
    of Confidentiality.
8.  Death rates are flagged as Unreliable when the rate is calculated
    with a numerator of 20 or less. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#Unreliable" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#Unreliable</a>.
9.  Beginning with the 2018 data, changes have been implemented that
    affect the counts for ICD-10 cause of death codes O00-O99 compared
    to previous practice. In addition, data for the cause of death codes
    O00-O99 for 2003 through 2017 reflect differences in information
    available to individual states and probable errors. Caution should
    be used in interpreting these data. More information can be found
    at:
    <a href="https://www.cdc.gov/nchs/maternal-mortality/" class="uri">https://www.cdc.gov/nchs/maternal-mortality/</a>.
10. Changes to cause of death classification affect reporting trends.
    More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#ICD-10" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#ICD-10</a>
    Changes.

Use of this data allowed under
<a href="https://wonder.cdc.gov/DataUse.html#" class="uri">https://wonder.cdc.gov/DataUse.html#</a>

CDC Data was generated from the following Query
<a href="https://wonder.cdc.gov/controller/saved/D76/D82F627" class="uri">https://wonder.cdc.gov/controller/saved/D76/D82F627</a>

    ## Parsed with column specification:
    ## cols(
    ##   Notes = col_logical(),
    ##   State = col_character(),
    ##   `State Code` = col_character(),
    ##   Year = col_double(),
    ##   `Year Code` = col_double(),
    ##   Month = col_character(),
    ##   `Month Code` = col_character(),
    ##   `ICD Chapter` = col_character(),
    ##   `ICD Chapter Code` = col_character(),
    ##   Deaths = col_double(),
    ##   Population = col_character(),
    ##   `Crude Rate` = col_character()
    ## )

### Load Annual Data

Dataset: Underlying Cause of Death, 1999-2018 Query Parameters: ICD-10
Codes:

-   X40 (Accidental poisoning by and exposure to nonopioid analgesics,
    antipyretics and antirheumatics)
-   X41(Accidental poisoning by and exposure to antiepileptic,
    sedative-hypnotic, antiparkinsonism and psychotropic drugs, not
    elsewhere classified)
-   X42 (Accidental poisoning by and exposure to narcotics and
    psychodysleptics \[hallucinogens\], not elsewhere classified)
-   X43 (Accidental poisoning by and exposure to other drugs acting on
    the autonomic nervous system)
-   X44 (Accidental poisoning by and exposure to other and unspecified
    drugs, medicaments and biological substances)
-   X60 (Intentional self-poisoning by and exposure to nonopioid
    analgesics, antipyretics and antirheumatics)
-   X61 (Intentional self-poisoning by and exposure to antiepileptic,
    sedative-hypnotic, antiparkinsonism and psychotropic drugs, not
    elsewhere classified)
-   X62 (Intentional self-poisoning by and exposure to narcotics and
    psychodysleptics \[hallucinogens\], not elsewhere classified)
-   X63 (Intentional self-poisoning by and exposure to other drugs
    acting on the autonomic nervous system)
-   X64 (Intentional self-poisoning by and exposure to other and
    unspecified drugs, medicaments and biological substances)
-   Y10 (Poisoning by and exposure to nonopioid analgesics, antipyretics
    and antirheumatics, undetermined intent)
-   Y11 (Poisoning by and exposure to antiepileptic, sedative-hypnotic,
    antiparkinsonism and psychotropic drugs, not elsewhere classified,
    undetermined intent)
-   Y12 (Poisoning by and exposure to narcotics and psychodysleptics
    \[hallucinogens\], not elsewhere classified, undetermined intent)
-   Y13 (Poisoning by and exposure to other drugs acting on the
    autonomic nervous system, undetermined intent)
-   Y14 (Poisoning by and exposure to other and unspecified drugs,
    medicaments and biological substances, undetermined intent)

Group By:

-   State
-   Year
-   ICD Chapter

Show Totals:

-   False

Show Zero Values:

-   False Show Suppressed:

-   False Calculate Rates Per:

-   100,000 Rate Options: Default intercensal populations for years
    2001-2009 (except Infant Age Groups)

Help: See
<a href="http://wonder.cdc.gov/wonder/help/ucd.html" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html</a>
for more information.

Query Date: Apr 30, 2020 9:21:00 PM

Suggested Citation: Centers for Disease Control and Prevention, National
Center for Health Statistics. Underlying Cause of Death 1999-2018 on CDC
WONDER Online Database, released in 2020. Data are from the Multiple
Cause of Death Files, 1999-2018, as compiled from data provided by the
57 vital statistics jurisdictions through the Vital Statistics
Cooperative Program. Accessed at
<a href="http://wonder.cdc.gov/ucd-icd10.html" class="uri">http://wonder.cdc.gov/ucd-icd10.html</a>
on Apr 30, 2020 9:21:00 PM

Caveats:

1.  As of April 3, 2017, the underlying cause of death has been revised
    for 125 deaths in 2014. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#2014-Revision" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#2014-Revision</a>.
2.  Circumstances in Georgia for the years 2008 and 2009 have resulted
    in unusually high death counts for the ICD-10 cause of death code
    R99, Other ill-defined and unspecified causes of mortality. Caution
    should be used in interpreting these data. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#Georgia-Reporting-Anomalies" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#Georgia-Reporting-Anomalies</a>.
3.  Circumstances in New Jersey for the year 2009 have resulted in
    unusually high death counts for the ICD-10 cause of death code R99,
    Other ill-defined and unspecified causes of mortality and therefore
    unusually low death counts in other ICD-10 codes, most notably R95,
    Sudden Infant Death Syndrome and X40-X49, Unintentional poisoning.
    Caution should be used in interpreting these data. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#New-Jersey-Reporting-Anomalies" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#New-Jersey-Reporting-Anomalies</a>.
4.  Circumstances in California resulted in unusually high death counts
    for the ICD-10 cause of death code R99, Other ill-defined and
    unspecified causes of mortality for deaths occurring in years 2000
    and 2001. Caution should be used in interpreting these data. More
    information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#California-Reporting-Anomalies" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#California-Reporting-Anomalies</a>.
5.  These items in the results table are not fully selected: External
    causes of morbidity and mortality. The Query Description lists the
    actual values selected.
6.  Death rates are flagged as Unreliable when the rate is calculated
    with a numerator of 20 or less. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#Unreliable" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#Unreliable</a>.
7.  The population figures for year 2018 are bridged-race estimates of
    the July 1 resident population, from the Vintage 2018 postcensal
    series released by NCHS on June 25, 2019. The population figures for
    year 2017 are bridged-race estimates of the July 1 resident
    population, from the Vintage 2017 postcensal series released by NCHS
    on June 27, 2018. The population figures for year 2016 are
    bridged-race estimates of the July 1 resident population, from the
    Vintage 2016 postcensal series released by NCHS on June 26, 2017.
    The population figures for year 2015 are bridged-race estimates of
    the July 1 resident population, from the Vintage 2015 postcensal
    series released by NCHS on June 28, 2016. The population figures for
    year 2014 are bridged-race estimates of the July 1 resident
    population, from the Vintage 2014 postcensal series released by NCHS
    on June 30, 2015. The population figures for year 2013 are
    bridged-race estimates of the July 1 resident population, from the
    Vintage 2013 postcensal series released by NCHS on June 26, 2014.
    The population figures for year 2012 are bridged-race estimates of
    the July 1 resident population, from the Vintage 2012 postcensal
    series released by NCHS on June 13, 2013. The population figures for
    year 2011 are bridged-race estimates of the July 1 resident
    population, from the Vintage 2011 postcensal series released by NCHS
    on July 18,
8.  Population figures for 2010 are April 1 Census counts. The
    population figures for years 2001 - 2009 are bridged-race estimates
    of the July 1 resident population, from the revised intercensal
    county-level 2000 - 2009 series released by NCHS on October
    26, 2012. Population figures for 2000 are April 1 Census counts.
    Population figures for 1999 are from the 1990-1999 intercensal
    series of July 1 estimates. Population figures for the infant age
    groups are the number of live births. <br/><b>Note:</b> Rates and
    population figures for years 2001 - 2009 differ slightly from
    previously published reports, due to use of the population estimates
    which were available at the time of release.
9.  The population figures used in the calculation of death rates for
    the age group ‘under 1 year’ are the estimates of the resident
    population that is under one year of age. More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#Age" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#Age</a>
    Group.
10. Beginning with the 2018 data, changes have been implemented that
    affect the counts for ICD-10 cause of death codes O00-O99 compared
    to previous practice. In addition, data for the cause of death codes
    O00-O99 for 2003 through 2017 reflect differences in information
    available to individual states and probable errors. Caution should
    be used in interpreting these data. More information can be found
    at:
    <a href="https://www.cdc.gov/nchs/maternal-mortality/" class="uri">https://www.cdc.gov/nchs/maternal-mortality/</a>.
11. Changes to cause of death classification affect reporting trends.
    More information:
    <a href="http://wonder.cdc.gov/wonder/help/ucd.html#ICD-10" class="uri">http://wonder.cdc.gov/wonder/help/ucd.html#ICD-10</a>
    Changes.

CDC Data was generated from the following Query

-   <a href="https://wonder.cdc.gov/controller/saved/D76/D82F634" class="uri">https://wonder.cdc.gov/controller/saved/D76/D82F634</a>

<!-- -->

    ## Parsed with column specification:
    ## cols(
    ##   Notes = col_logical(),
    ##   State = col_character(),
    ##   `State Code` = col_character(),
    ##   Year = col_double(),
    ##   `Year Code` = col_double(),
    ##   `ICD Chapter` = col_character(),
    ##   `ICD Chapter Code` = col_character(),
    ##   Deaths = col_double(),
    ##   Population = col_double(),
    ##   `Crude Rate` = col_character(),
    ##   `Age Adjusted Rate` = col_character()
    ## )

Load Geo data
-------------

Latitudeo and Longitude Coordinates for each state were downloaded from
<a href="https://inkplant.com/code/state-latitudes-longitudes" class="uri">https://inkplant.com/code/state-latitudes-longitudes</a>

### Exploration

    ## tibble [10,107 x 7] (S3: tbl_df/tbl/data.frame)
    ##  $ State     : chr [1:10107] "Alabama" "Alabama" "Alabama" "Alabama" ...
    ##  $ YearMonth : chr [1:10107] "1999-01" "1999-02" "1999-03" "1999-04" ...
    ##  $ Year      : chr [1:10107] "1999" "1999" "1999" "1999" ...
    ##  $ Month.Code: chr [1:10107] "01" "02" "03" "04" ...
    ##  $ Deaths    : int [1:10107] 17 13 10 14 13 20 11 20 21 17 ...
    ##  $ Lat       : num [1:10107] 32.8 32.8 32.8 32.8 32.8 ...
    ##  $ Long      : num [1:10107] -86.8 -86.8 -86.8 -86.8 -86.8 ...

    ## # A tibble: 6 x 7
    ##   State   YearMonth Year  Month.Code Deaths   Lat  Long
    ##   <chr>   <chr>     <chr> <chr>       <int> <dbl> <dbl>
    ## 1 Alabama 1999-01   1999  01             17  32.8 -86.8
    ## 2 Alabama 1999-02   1999  02             13  32.8 -86.8
    ## 3 Alabama 1999-03   1999  03             10  32.8 -86.8
    ## 4 Alabama 1999-04   1999  04             14  32.8 -86.8
    ## 5 Alabama 1999-05   1999  05             13  32.8 -86.8
    ## 6 Alabama 1999-06   1999  06             20  32.8 -86.8

    ##     State            YearMonth             Year            Month.Code       
    ##  Length:10107       Length:10107       Length:10107       Length:10107      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      Deaths            Lat             Long        
    ##  Min.   : 10.00   Min.   :21.09   Min.   :-157.50  
    ##  1st Qu.: 26.00   1st Qu.:35.57   1st Qu.: -98.27  
    ##  Median : 51.00   Median :39.06   Median : -86.79  
    ##  Mean   : 74.97   Mean   :38.59   Mean   : -92.03  
    ##  3rd Qu.: 90.00   3rd Qu.:42.01   3rd Qu.: -79.81  
    ##  Max.   :546.00   Max.   :61.37   Max.   : -69.38

    ## [1] "Deaths reported in 2016 "

    ## [1] 63186

    ## [1] "Deaths reported in the state of Texas 2016 "

<table>
<thead>
<tr class="header">
<th style="text-align: right;">x</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">2822</td>
</tr>
</tbody>
</table>

    ## # A tibble: 6 x 3
    ## # Groups:   State, YearMonth [6]
    ##   State YearMonth Deaths
    ##   <chr> <chr>      <int>
    ## 1 Texas 1999-01       72
    ## 2 Texas 1999-02       97
    ## 3 Texas 1999-03      101
    ## 4 Texas 1999-04       99
    ## 5 Texas 1999-05       86
    ## 6 Texas 1999-06       77

![](DaysOfCode_files/figure-markdown_strict/plottexas-1.png)

    ## `geom_smooth()` using formula 'y ~ x'

![](DaysOfCode_files/figure-markdown_strict/plottexas-2.png)

<table>
<caption>Deaths attributed to opiods in the United State</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Year</th>
<th style="text-align: right;">Deaths</th>
<th style="text-align: right;">PercentChange</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">2018</td>
<td style="text-align: right;">67,251</td>
<td style="text-align: right;">-4.0573507</td>
</tr>
<tr class="even">
<td style="text-align: left;">2017</td>
<td style="text-align: right;">70,095</td>
<td style="text-align: right;">10.3475961</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2016</td>
<td style="text-align: right;">63,522</td>
<td style="text-align: right;">21.4314389</td>
</tr>
<tr class="even">
<td style="text-align: left;">2015</td>
<td style="text-align: right;">52,311</td>
<td style="text-align: right;">11.3616043</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2014</td>
<td style="text-align: right;">46,974</td>
<td style="text-align: right;">7.0120284</td>
</tr>
<tr class="even">
<td style="text-align: left;">2013</td>
<td style="text-align: right;">43,896</td>
<td style="text-align: right;">5.9726715</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2012</td>
<td style="text-align: right;">41,422</td>
<td style="text-align: right;">0.3926321</td>
</tr>
<tr class="even">
<td style="text-align: left;">2011</td>
<td style="text-align: right;">41,260</td>
<td style="text-align: right;">7.8213604</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2010</td>
<td style="text-align: right;">38,267</td>
<td style="text-align: right;">3.6091406</td>
</tr>
<tr class="even">
<td style="text-align: left;">2009</td>
<td style="text-align: right;">36,934</td>
<td style="text-align: right;">1.5172338</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2008</td>
<td style="text-align: right;">36,382</td>
<td style="text-align: right;">1.2044841</td>
</tr>
<tr class="even">
<td style="text-align: left;">2007</td>
<td style="text-align: right;">35,949</td>
<td style="text-align: right;">4.6550218</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2006</td>
<td style="text-align: right;">34,350</td>
<td style="text-align: right;">15.4389031</td>
</tr>
<tr class="even">
<td style="text-align: left;">2005</td>
<td style="text-align: right;">29,756</td>
<td style="text-align: right;">8.7891196</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2004</td>
<td style="text-align: right;">27,352</td>
<td style="text-align: right;">6.2750126</td>
</tr>
<tr class="even">
<td style="text-align: left;">2003</td>
<td style="text-align: right;">25,737</td>
<td style="text-align: right;">9.6357827</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2002</td>
<td style="text-align: right;">23,475</td>
<td style="text-align: right;">21.3052914</td>
</tr>
<tr class="even">
<td style="text-align: left;">2001</td>
<td style="text-align: right;">19,352</td>
<td style="text-align: right;">11.3207547</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2000</td>
<td style="text-align: right;">17,384</td>
<td style="text-align: right;">3.4207865</td>
</tr>
<tr class="even">
<td style="text-align: left;">1999</td>
<td style="text-align: right;">16,809</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

![](DaysOfCode_files/figure-markdown_strict/plottus-1.png)

    ## `geom_smooth()` using formula 'y ~ x'

![](DaysOfCode_files/figure-markdown_strict/plottus-2.png)

<table>
<caption>States with Highest number of deaths for 2016</caption>
<thead>
<tr class="header">
<th style="text-align: left;">State</th>
<th style="text-align: right;">Deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Florida</td>
<td style="text-align: right;">4,724</td>
</tr>
<tr class="even">
<td style="text-align: left;">California</td>
<td style="text-align: right;">4,649</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pennsylvania</td>
<td style="text-align: right;">4,609</td>
</tr>
<tr class="even">
<td style="text-align: left;">Ohio</td>
<td style="text-align: right;">4,318</td>
</tr>
<tr class="odd">
<td style="text-align: left;">New York</td>
<td style="text-align: right;">3,636</td>
</tr>
<tr class="even">
<td style="text-align: left;">Texas</td>
<td style="text-align: right;">2,822</td>
</tr>
</tbody>
</table>

<table>
<caption>States with Lowest number of deaths for 2016</caption>
<thead>
<tr class="header">
<th style="text-align: left;">State</th>
<th style="text-align: right;">Deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Vermont</td>
<td style="text-align: right;">125</td>
</tr>
<tr class="even">
<td style="text-align: left;">Nebraska</td>
<td style="text-align: right;">120</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Montana</td>
<td style="text-align: right;">118</td>
</tr>
<tr class="even">
<td style="text-align: left;">Wyoming</td>
<td style="text-align: right;">98</td>
</tr>
<tr class="odd">
<td style="text-align: left;">North Dakota</td>
<td style="text-align: right;">77</td>
</tr>
<tr class="even">
<td style="text-align: left;">South Dakota</td>
<td style="text-align: right;">68</td>
</tr>
</tbody>
</table>

![](DaysOfCode_files/figure-markdown_strict/plotmap-1.png)

    sessionInfo(package = NULL)

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19041)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] fiftystater_1.0.1 forcats_0.5.0     stringr_1.4.0     dplyr_0.8.5      
    ##  [5] purrr_0.3.4       readr_1.3.1       tidyr_1.0.2       tibble_3.0.1     
    ##  [9] ggplot2_3.3.0     tidyverse_1.3.0   knitr_1.28       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.0.0 xfun_0.13        splines_3.6.0    haven_2.2.0     
    ##  [5] lattice_0.20-41  colorspace_1.4-1 vctrs_0.2.4      generics_0.0.2  
    ##  [9] htmltools_0.4.0  mgcv_1.8-31      yaml_2.2.1       utf8_1.1.4      
    ## [13] rlang_0.4.6      pillar_1.4.3     glue_1.4.0       withr_2.2.0     
    ## [17] DBI_1.1.0        dbplyr_1.4.3     modelr_0.1.7     readxl_1.3.1    
    ## [21] lifecycle_0.2.0  munsell_0.5.0    gtable_0.3.0     cellranger_1.1.0
    ## [25] rvest_0.3.5      mapproj_1.2.7    evaluate_0.14    labeling_0.3    
    ## [29] fansi_0.4.1      highr_0.8        broom_0.5.6      Rcpp_1.0.4.6    
    ## [33] scales_1.1.0     backports_1.1.6  jsonlite_1.6.1   farver_2.0.3    
    ## [37] fs_1.4.1         hms_0.5.3        digest_0.6.25    stringi_1.4.6   
    ## [41] grid_3.6.0       cli_2.0.2        tools_3.6.0      maps_3.3.0      
    ## [45] magrittr_1.5     crayon_1.3.4     pkgconfig_2.0.3  Matrix_1.2-18   
    ## [49] ellipsis_0.3.0   xml2_1.3.2       reprex_0.3.0     lubridate_1.7.8 
    ## [53] assertthat_0.2.1 rmarkdown_2.1    httr_1.4.1       rstudioapi_0.11 
    ## [57] R6_2.4.1         nlme_3.1-147     compiler_3.6.0
