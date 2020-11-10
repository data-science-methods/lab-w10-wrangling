#' ---
#' title: "Data Science Methods, Lab for Week 10"
#' author: "Your Name"
#' email: Your Email
#' output:
#'   html_document:
#'     toc: true
#' ---

#' This lab develops some tools for wrangling data, and specifically for parsing date columns into `Date` variables.  Dates are notorious among data scientists for requiring specific attention and, often, accumulating errors as files are imported and exported across different pieces of software and operating systems. There are numerous ways to write dates, with different conventions in different countries and variant conventions used within countries.  Even worse, Excel is [enthusiastically bad at parsing dates](https://www.theverge.com/2020/8/6/21355674/human-genes-rename-microsoft-excel-misreading-dates), and [can use different internal representations for dates on Mac and Windows](https://docs.microsoft.com/en-us/office/troubleshoot/excel/1900-and-1904-date-system).  
#' 
#' 
#' The data for this lab come from the National Science Foundation's [Advanced Award Search tool](https://www.nsf.gov/awardsearch/advancedSearch.jsp).  These are full results for awards funded in three cognitive science-related programs: Perception, Action, and Cognition (7252); Robust Intelligence (7495); and Understanding the Brain (8089).  The search results are in the `data` folder, with one file for each program.  So not only do we need to parse and fix the data, we also need to encapsulate the parsing and fixing processes into functions, which can then be run across the three files.  
#' 
#' We'll start by reading in part of one file, use that to develop the code to parse and clean, and then apply it to the other files.  In a real script, to keep things clean, we would develop in a scratch script, and then only put the final version into the analysis script.  But labs have to narrate the development process.
#' 

## Setup
## **IMPORTANT**: Add all dependencies to `DESCRIPTION`
library(tidyverse)
library(readxl)
library(lubridate)
library(assertthat)

data_folder = file.path('data')
data_folder
## To check your answers locally, run the following: 
## testthat::test_dir('tests')
## Remember not to leave this line uncommented when you turn in your script.  

#' # Problem 1 #
#' 
#' 1. *Load the file `7495_Robust-Intelligence.xlsx` using `readxl::read_excel()`.  Set the `n_max` argument to only read the first 25 rows.  Assign the input to `ri_df`.*
#' 
#' 
ri_df = read_excel("data/7495_Robust-Intelligence.xlsx", n_max = 25)

#' 2. *Suppose we're interested in looking at the amount of money NSF has awarded to different UC campuses for cognitive science-related work, by the year each project started.  What variables in the dataframe would be especially important for this research question?*
#' AwardedAmountToDate
#' Organization
#' Program
#' StartDate
#' 

#' 3. *Before diving into dates, let's do a quick manual check of the money column, `AwardedAmountToDate`.  The name of this column can be misleading; for most types of awards, it represents the total amount of the award, not just the amount that has been distributed to date.  We can view the information for a specific award using a URL like this: <https://www.nsf.gov/awardsearch/showAward?AWD_ID=1813043> (this should match the top row of your dataframe).  Award IDs are in the `AwardNumber` column. For the first 8 awards, compare the `AwardedAmountToDate` values in the dataframe to the "Awarded Amount to Date" values shown in the award search system.  Based on these 8 awards, does `AwardedAmountToDate` seem to contain valid and correctly-formatted data?* 
#' 
#' 
#' Yes it is the same as the website award to date.


#' # Problem 2 #
#' 
#' 1. *The column `StartDate` is supposed to contain the date each program began.  What class is this column?  How are the dates formatted?  For convenience as we write some functions to parse these dates, create a variable `dates` that contains the dates from `StartDate`.* 
#' 
#' Character
#' 01/01/2001, or 22222

dates = ri_df$StartDate

#' 2. *Let's start with the dates formatted like "43109."  These represent days passed since a reference date, which is sometimes called the "serial date" representation or format.  Typically — and in this case — the reference date is December 30, 1899 (for backwards compatibility with a bug in Lotus 1-2-3); but note that it may be December 31, 1903 on systems where Excel is trying to be compatible with an old Mac format. (More here: <http://www.cpearson.com/excel/datetime.htm>.)  The base R function `as.Date()` has an `origin` argument that can be used with integers to parse this date format:* 

as.Date(2, origin = '1899-12-30')

#' *Write a pipe that first uses `as.integer()` to convert the dates to integers, then passes them through `as.Date()`.  Assign the result to the variable `int_dates1` (1 for the "first attempt").*
#' 
int_dates1 = dates %>% 
    as.integer() %>% 
    as.Date(origin = '1899-12-30')


#' 3. *This pipe moved us forward, but was unsuccessful in two ways.  Most obviously, the dates in the '08/15/2018' couldn't be coerced into integers, resulting in NAs.  There's a more subtle problem. For award 1813043 (which should be the first row of `ri_df`), its start date was September 1, 2018.  Explain how `int_dates1` has represented this incorrectly.  As a bonus, see if you can figure out why Excel failed to parse dates like "08/15/2018" and "03/15/2017."  Hint: `int_dates1` is being displayed in the ISO 8601 format.  Double check the order of elements for this format.*
#' Int_dates1 has flipped month and day.
#' Since Int_dates1 flipped month and day, and month can only go up to 12, any days that were over 12 were not parsed correctly.
#' 

#' 4. *Fortunately, it's quite easy to fix this problem using `lubridate`, a package designed to wrangle dates.  The `ydm()` function is designed to parse strings with dates in Year-Day-Month order, and quietly coerces dates into strings.  Write a pipe that adds a `ydm()` call after `as.Date()`, and assign the result to `int_dates2` (the second pass).  Confirm that this works, with the dates parsed correctly.*
#' 
int_dates2 = int_dates1 %>% 
    ydm()


#' # Problem 3 #
#' *The dates formatted like "08/15/2018" are easy to parse with the `mdy()` function from lubridate.  Confirm that this works correctly for the instances in `dates`.  The `quiet` argument can be used to suppress the warnings about parsing failures (which we expect here).  Assign the result to `mdy_dates`*
#' 
mdy_dates = mdy(dates, quiet = TRUE)



#' # Problem 4 #
#' 1. *Now we need to put these two approaches together.  We can do this using `ifelse()` (a base R function), `if_else()` or `case_when()` (`dplyr` functions).  All three provide a "vectorised if": if the value from `mdy_dates` is NA, then use the value from `int_dates2`; otherwise use the value from `mdy_dates`. Assign the result to `parsed_dates`.*
#' 
parsed_dates = if_else(is.na(mdy_dates),int_dates2, mdy_dates)
parsed_dates
#' 2. *We can encapsulate the logic for parsing the dates into a function to create more readable code.  Uncomment the code lines below (leave the lines starting with #') and fill in the blanks with the code you developed above.*
#' 

#' Parsing dates from the mangled NSF data
#' @param dates A character vector of dates to be parsed
#' @return A vector of class `Date`
parse_dates = function(d) {
    int_dates = d %>% 
        as.integer() %>% 
        as.Date(origin = '1899-12-30') %>% 
        ydm(quiet = TRUE)

    mdy_dates = mdy(d, quiet = TRUE)

    parsed_dates = if_else(is.na(mdy_dates),int_dates, mdy_dates)

    return(parsed_dates)
}

parse_dates(dates)



#' # Problem 5 #
#' 1. *Now that we have our function to parse/clean the dates, we want to apply it to each of the three input files.  In most languages we would use a for loop to do this.  Because R emphasizes vectors rather than length-one variables, in R the preferred idiom is to use `apply()` or the somewhat more user-friendly variations of `purrr::map()`.  Both of these functions apply a function to every element of a list.  So we'll want to write a single function that takes, as input, a path to a XLSX file; reads it using `read_excel()`; applies `parse_dates()` to the `StartDate` colum; and then returns the cleaned dataframe.  Call this function `read_and_parse()`.* 
#' 
read_and_parse = function(filePath) {
    Excelreader = read_excel(filePath) 
        parse_dates(Excelreader$StartDate) %>% 
        return(cleaned_df)
}



#' 2. *We'll put the paths to the data file into a single vector.  Note that `file.path()` is vectorized:* 

file.path(data_folder, c('foo', 'bar'))

#' *Construct a vector `data_files` with the paths to the three XLSX files in the `data` folder.* 
#' 
data_files = file.path(data_folder, c('7495_Robust-Intelligence.xlsx', '7252_Perception,-Action,-and-Cognition.xlsx', '8089_Understanding-the-Brain.xlsx'))

#' 3. *This variant of `purrr::map()` returns the results in a single combined dataframe, using `dplyr::bind_rows()`. (Just uncomment the code line.)*

cleaned_df = map_dfr(data_files, read_and_parse)

#' 4. *Some of the steps in `parse_dates()` generate warnings, e.g., when they create NAs because dates like "8/13/2018" can't be coerced to integers.  These warnings get passed up to the construction of `cleaned_df`.  We might suppress these warnings by wrapping parts of `parse_dates()` in `suppressWarnings()` or `purrr::quietly()`.  But it's a little simpler to add three checks on the parsed dates in `cleaned_df`: (1) no missing values, (2) nothing prior to 1999, and (3) nothing later than 2020. We'll do this using **assertions** from the `assertthat` package, which cause the script to exit with an error if their condition isn't satisfied.*
#' 
#' *Here's an assertion that all of the entries in `cleaned_df$StartDate` are non-missing. Uncomment it.* 
#' 
assert_that(all(!is.na(cleaned_df$StartDate)))

#' *Write two assertions, one stating that all of the `StartDate` values are greater than or equal to January 1, 1999; and a second stating that all of the values are less than or equal to December 31, 2020.*  
assert_that(all(cleaned_df$StartDate>=1999-01-01))
assert_that(all(cleaned_df$StartDate<=2020-12-31))
