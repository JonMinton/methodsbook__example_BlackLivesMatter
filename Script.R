
# The aim of this project is to illustrate how tidyverse principles can be used to 
# learn more about comparative mortality trends by gender and ethnicity in the USA. 

rm(list = ls())
# Note this convention is now opposed by the Tidyverse:
# https://www.tidyverse.org/articles/2017/12/workflow-vs-script/

# A better (though not mutually exclusive practice) is to make sure the R project options are set to 
# not save the R workspace when you close the IDE. 

pacman::p_load(
  tidyverse
)

# Background 

# Black Lives Matters 
#   - particular concerns about 
#  i) high rates of police shootings in the USA
#  ii) ethnic targetting of these shootings in the USA, predominantly USA 

# The aim of this project is therefore to see if publically available data can 
# provide analyses which may help develop our understanding of these two issues 


# Source of data: 

# CDC Wonder Dataset

# Centers for Disease Control and Prevention
# Wide-ranging Online Data for Epidemiologic Research

# https://wonder.cdc.gov/wonder/help/main.html

# There are detailed records of mortality available from 1999 onwards 
# Using the ICD-10 codes 

# https://wonder.cdc.gov/ucd-icd10.html

# You have to agree to use the data ethically before you can access the data.
# This includes not trying to identify any individual whose records are contained here

# On the page that appears, there are a number of drop-down menus and other options to select 

# For 1. Organize Table Layout
# Select group results by 
# i) Hispanic Origin
# ii) Race
# iii) Gender
# iv) Age groups 
# v) ICD chapter 

# Give the query a title
# Deaths_by_ethnicity_gender_age_chapter

# For 2. Select location
# Leave as is - *All* (The United States)

# For 3. Select demographics
# Select 'single-year ages' from 'Pick between'
# Keep all others as is (All ages, all genders, all origins, all races)

# For 4. Select year and month 
# Leave as is (all dates)

# Leave other fields unchanged for now

# In 7 Other Options
# Select 'Export Results'

# Then click 'send'


# Note: At the time of writing (15/1/2018) the progress bar
# is broken. A file will appear as downloaded when the operation has finished
# But the progress bar won't update to let you know.

# I put this file in a project directory called 'data/raw'


# Part 2: initial analysis 


# Let's load the data and get a sense of 
# i) what the janitorial challenges involved are 
# ii) What it substantively can tell use

# First we need to recognise the data type
# We can open the file in notepad or similar 

# It appears to be a text file that is either space delimited, or tab delimited
# We can also see that there is some trailing metadata at the end of the file
# We need to remove this metadata, but we can do this later.

dta <- read_tsv(
  "data/raw/deaths_by_ethnicity_gender_age_chapter.txt"
)

dta

glimpse(dta)
# We can see that all variables, except Deaths, are loaded in initially as characters. 
# This includes variables such as ages and age code

# The file has many rows, so using View is not recommended

# However if we look at the Notes field in notepad itself we can see 
# something interesting.
# Most rows have NA
# But at the very end the Notes field contains metadata
# For these rows the contents of the other columns are blank (NA)

dta %>% 
  filter(!is.na(Notes))

# There are therefor 78 rows of notes at the end of the file
# We do not need/want this in the data itself 

# So we can filter to INCLUDE those rows where Notes is NA 
dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes)

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>% 
  tail

# This seems to have removed the metadata, without losing any of the data

# Looking again at the data columns we notice a certain amount of redundency in the variables 
# For example, the same information seems to be contained in
# Hispanic Origin and Hispanic Origin Code
# Race and Race Code
# Gender and Gender Code
# Single-year ages and Single-year ages code

# We can test this by producing cross-tabs or group_by and summary operations, to 
# make sure neither of the columns in these pairs contain information not in the other column

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>%  
  xtabs(~ `Hispanic Origin` + `Hispanic Origin Code`, data = .)

# There are no off-diagonal elements in the cross tabs

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>% 
  group_by(`Hispanic Origin`, `Hispanic Origin Code`) %>% 
  tally()

# Note two things: 
# i) In both of the above examples we had to use the back tick quotation marks because 
# the column names included spaces in them
# ii) Both approaches produce the same finding: 
#    - there are no off-diagonal elements in the first example
#    - there are only three rows, meaning three combinations of origin and code, in the latter example


# We can now check there is the same 1-to-1 mapping in the latter case too 

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>% 
  xtabs(~ `Race` + `Race Code`, data = .)

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>% 
  xtabs(~ `Gender` + `Gender Code`, data = .)

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>% 
  xtabs(~ `ICD Chapter` + `ICD Chapter Code`, data = .)

# There are many different ICD chapters and Chapter Codes
# And there will also be many ages in single years
# The approach we used to make sure there was a unique 1-1 mapping for gender 
# Can't be as easily generalised to these cases.
# So instead let's build a function which
# 1) checks the mapping is unique
# 2) returns a usable lookup 

# Let's first prototype the function for age

# We can 'split the pipe' using an anonymous function in a parenthesis

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>% (
    function(..){
      c(
        .. %>% select(`Single-Year Ages`) %>% n_distinct,
        .. %>% select(`Single-Year Ages Code`) %>% n_distinct,
        .. %>% group_by(`Single-Year Ages`, `Single-Year Ages Code`) %>% tally() %>% nrow
      )
    }
  )
# This produces a vector of three values, each of which is the same
# This suggests both variables contain the same information, and so 
# we can make do with only one of them

# Note we needed to 
# i) put a parenthesis around the anonymous function we made
# ii) give the input name to the function a different name to the 
# standard placeholder symbol . 

# Conceptually, what this anonymous function allowed us to do is take a single input from the pipeline
# and split (clone) it into three different processes, each producing a different output
# The c() function then combined these three outputs into a single element, which the function then returned

# We might want to adapt this function so that what's displayed are the numbers and an 'ALL OK' 
# summary 

# But what the function outputs is the lookup

# Doing this is actually more tricky than I first expected:

check_and_get_lookup <- function(.., ...){

  grp_vars <- quos(...)
  first_col <- grp_vars[[1]]
  second_col <- grp_vars[[2]]
  

  c(
    .. %>% select(!!first_col) %>% n_distinct,
    .. %>% select(!!second_col) %>% n_distinct,
    .. %>% group_by(!!!grp_vars) %>% tally() %>% nrow
  ) -> checking_vector
  
  # Printed reports to user 
  print("The vector lengths are")
  print(checking_vector)
  all_are_equal <- unique(checking_vector) %>% length  == 1
  print(
    paste(
      "All vectors",
      ifelse(
        all_are_equal,
        "are", "are not"
      ),
      "equal"
    )
  )
  
  # Output from function
  .. %>% 
    group_by(!!!grp_vars) %>% 
    tally() %>% 
    select(-n) # This is what is output
}

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>%
  check_and_get_lookup(., `Single-Year Ages`, `Single-Year Ages Code`) -> age_code_lookup

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>%
  check_and_get_lookup(., `ICD Chapter`, `ICD Chapter Code`) -> icd_code_lookup



# The function above is obtuse in a number of ways because it involves 
# operators which adapt tidyverse packages to work more effectively in functions
# The reasons why these changes were needed are to do with the fact tidyverse packages 
# use something called non-standard evaluation (NSE). Recent developments in tidyverse 
# to allow code to work in functions are described in more detail here
# http://dplyr.tidyverse.org/articles/programming.html

# Some key points to note are :
# .. is distinct from . 
# ... is distinct from .. 
 # ... is an elipses containing the two column names we want to evaluate
# !!! is different from !!
# We use !!! when we are working from 



# Anyway, now we're reasonably confident we're not losing any information by losing half of these pairs, let's pare down 
# the number of columns in the dataset a bit further
# We can also rename the columns to save on typing

dta %>% 
  filter(is.na(Notes)) %>% 
  select(-Notes) %>%
  select(
    hispanic = `Hispanic Origin`,
    race = Race,
    gender = Gender,
    age = `Single-year`
         )
