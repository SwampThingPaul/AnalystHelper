# AnalystHelper <img src="./hex/AnalystHelper_hex.png" align="right" height = "60"/>

[![DOI](https://zenodo.org/badge/179672539.svg)](https://zenodo.org/badge/latestdoi/179672539)

A package of functions that I have accumulated through the years and have kept as `CommonlyUsedFunctions.R` on my hard drive and would use `source()` to utilize these functions. Finally upgraded to building my `CommonlyUsedFunctions.R` into a functional package. 

Using `library(roxygen2)` and R-studio "R-package Project" I constructed this package to be shareable amongst collaborators and generally be helpful in day-to-day analyses. Most of these functions are highly specific and constructed in my mind knowing what I knew at the time. Most functions presumably could be optimized, make no sense to others or are redundant with other packages.  

***Please excuse any grammatical/text errors***

## AnalystHelper Installation

```
install.packages("devtools");# if you do not have it installed on your PC
devtools::install_github("SwampThingPaul/AnalystHelper")
```

## Package contents

This package has several objective includes accessing environmental data, manipulating data and visualizing the data. 

Objectives (*not all functions are identified here*): 

1. **South Florida Water Management District Online Environmental Database ([DBHYDRO](http://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.main_menu)).**

    Several functions were developed to directly download data into `R` from DBHYDRO using URL-based data access (see [DBHYDRO User Manual](https://www.sfwmd.gov/sites/default/files/documents/dbhydrobrowseruserdocumentation.pdf))

    - For water quality, soil, fish and vegetation specific data use `DBHYDRO_WQ()`. This function requires a date range, Station ID and parameter(s) to be specified. See [DBHYDRO User Manual](https://www.sfwmd.gov/sites/default/files/documents/dbhydrobrowseruserdocumentation.pdf) for more information.
    - Daily water water level, discharge and meterolgical specific data use `DBHYDRO_daily()`. This function requires a date range and site/data-type identifier called a DBKEY (for more information see [DBHYDRO User Manual](https://www.sfwmd.gov/sites/default/files/documents/dbhydrobrowseruserdocumentation.pdf)).
    - Breakpoint (typically 15-minute interval) water level, discharge and meterolgical specific data use `DBHYDRO_breakpoint()`. This function requires a date range and site/data-type identifier called a DBKEY (for more information see [DBHYDRO User Manual](https://www.sfwmd.gov/sites/default/files/documents/dbhydrobrowseruserdocumentation.pdf)). <ins>Currently this function is *UNDER FURTHER DEVELOPMENT*__* due to issues with internet connect timing out. The function is still available but buggy.</ins>

2. **Data manipulation/handling**

    Several functions revolve around data screening, calculating or converting data  for further evalution. 
    
    - Several of the functions revolve around evaluting dissolved oxygen data including `DO.Screening()`, `DO_PerSat()`,`DO.TOD.WQS.stream()`. `DO.Screening()` was developed to handle dissolved oxygen and associated data to identify possible erronous data (i.e. date recorded outside the normal range of Dissolved Oxygen, Temperature or time-frame). `DO_PerSat()` is to convert dissolved oxygen concentration to percent saturation to be used to evalute complaince with established water quality standards. `DO.TOD.WQS.stream()` was developed to calculate water quality standards specific to dissolved oxygen for class-III streams in Florida (See `?DO.TOD.WQS.stream` for more information).
    - A couple of functions were developed to handle salinity and conductance data. `cond.to.spc()` and `SalinityCalc()` were developed to calculate specific conductance and salinity from conductance and temperature. 
    - Once function was developed to handle nitrogen data. Total nitrogen is calculated as the sum of Nitrate, Nitrite and Total Kjeldahl Nitrogen. Total Nitrogen can also be directly determined. Therefore `TN_Combine()` will calculate total nitrogen and if total nitrogen (direct measure) data is present it will combine calculated and direct measure into one field. 
    
3. **Additional basic statistical analyses**

    Other basic statsitical functions including standard error (`SE()`), sample size/count (`N()`) and coefficient of variation (`cv.per()`) were developed to help provide statistical summaries of large datasets. 
    
4. **Plotting**

    I am partial to plotting using `base` functions. To reduce the amount of code and optimize data visulaizations several functions were developed. 
    - `pt_line()`, `pt_line_error()`, `pt_error()`, `errorbars()` were developed to quickly draw lines, points and error bar in base plotting.
    - `shaded.range()` was developed to quickly visulize model prediction and/or confidence intervals by plotting the upper and lower bounds of the `predict(... interval=c("prediction" "confidence"))` output(s). 
    - `log.scale.fun()` and `axis_fun()` allow for manipulation of axis tick marks and labels. 
    
5. **Date and time functions**

    Working with date fields correctly in R can be onerous, therefore a few functions were developed to handle these type of issues. `date.fun()` and `dst.to.est()` are the two major date handling functions. Other functions revolving around date fields include `WY()`, `hydro.day()`, `decimal.WY()` and `FL.Hydroseason()` to calcualte water year, day of water year and wet/dry season respectively. 
   
6. **read.access()**

    `read.access()` streamlines the process of reading MS Access tables into `R` by using the `RODBC` package. For some systems, Microsoft Access Database Engine 2010 may need to be install. [Here](https://www.microsoft.com/en-us/download/details.aspx?id=13255) is the link for Redistributable installer. 
    
***
