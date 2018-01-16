
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
# i) Year
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
# We use !! when we are working with a single expression we want to evaluate (effectively turn from a label to a command)
# We use !!! when we are working with a group of expressions which we want to evaluate. 



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
    age = `Single-Year Ages Code`,
    icd_code = `ICD Chapter Code`,
    deaths = Deaths, 
    population = Population
  ) %T>% print %>% 
  mutate(
    ethnicity = case_when(
      hispanic == "Hispanic or Latino" ~ 'hispanic',
      hispanic != "Hispanic or Latino" & race == 'White' ~ 'white_nh',
      hispanic != "Hispanic or Latino" & race == 'Black or African American' ~ 'black_nh',
      TRUE ~ 'other'
    )
  )  %>% 
  mutate(
    population = as.numeric(population),
    deaths = as.numeric(deaths)
    ) %>% 
  filter(!is.na(population)) %>%
  group_by(ethnicity, gender, age, icd_code) %>% 
  summarise(deaths = sum(deaths), population = sum(population)) %>% 
  ungroup() %>% 
  select(ethnicity, gender, age, icd_code, deaths, population) -> dta_tidy



# Analysis ----------------------------------------------------------------


# With the data in this format, we can start to explore some of the ethnic differences by cause of death

dta_tidy %>% 
  group_by(ethnicity, gender) %>% 
  summarise(
    deaths = sum(deaths),
    population = sum(population)
  ) %>% 
  ungroup() %>% 
  mutate(death_rate = 100000 * deaths / population) %T>% print() %>% 
  select(ethnicity, gender, death_rate) %>% 
  mutate(relative_rate = death_rate / death_rate[ethnicity == 'white_nh' & gender == 'Female']) %>% 
  arrange(relative_rate) %T>% print %>% (
    function(..){
      .. %>% 
        unite(eth_gender, ethnicity, gender, remove = F) %>%
        mutate(eth_gender = forcats::fct_reorder(eth_gender, relative_rate)) %>% 
        ggplot(
          aes(x = eth_gender, y = relative_rate, colour = gender, fill = ethnicity)
               ) + 
        geom_bar(stat = "identity", size = 2,  alpha = 0.6) +
        geom_hline(yintercept = 1) + 
        coord_flip() 
      }
  )

# From this it appears that both BMH males and WNH males have similar mortality rates compared with 
# WNH females (the reference category). It also appears that H males and H females have much lower 
# mortality rates. 

# However, in these analyses we are not doing something very important to do in analyses: 
# controlling for differences in population structure. Younger people are, of course, usually 
# much less likely to die than older people, and so if one population tends to be much younger than 
# another, a smaller death rate might be expected through population structure alone.
# This might be particularly the case with Hispanic population groups. 
# But let's try to use the data to get a sense of how much of an issue this might be 


dta_tidy %>% 
  group_by(ethnicity, gender, age) %>% 
  summarise(
    population = sum(population)
  ) %>% 
  ungroup() %T>% (
    function(..){
      .. %>% 
        ggplot(aes(x = age, ymax = population)) + 
        facet_grid(ethnicity ~ gender) + 
        geom_ribbon(ymin = 0) +
        coord_flip() -> p
      
      print(p)
      browser("Population structure")
      NULL
    }
  ) %>% 
  group_by(ethnicity, gender) %>% 
  mutate(prop_of_pop = population / sum(population)) %T>% (
    function(..){
      .. %>% 
        ggplot(aes(x = age, ymax = prop_of_pop)) + 
        facet_grid(ethnicity ~ gender) + 
        geom_ribbon(ymin = 0) +
        coord_flip() -> p
      
      print(p)
      browser("Proportionate Population structure")
      NULL
    }
  ) %>% 
  mutate(agepop = age * population) %>% 
  summarise(mean_age = sum(agepop) / sum(population)) %>% 
  arrange(mean_age)

# We can see from this that there are some clear differences in the age structures 
# of the different populations. The mean age for hispanics is under 40, whereas for 
# White non-Hispanics it's around 40

# We might now want to reformulate the questions:
# compared with others of the same age and gender, what is the relative risk of dying 
# for different ethnic groups?

# Let's try to do this with age in single years, and see how far we get 

dta_tidy %>% 
  group_by(ethnicity, gender, age) %>% 
  summarise(
    deaths = sum(deaths),
    population = sum(population)
  ) %>% 
  ungroup() %>% 
  mutate(death_rate = 100000 * deaths / population) %>% (
    function(..){
      .. %>% 
        ggplot(aes(x = age, ymax = death_rate)) + 
        facet_grid(ethnicity ~ gender) + 
        geom_ribbon(ymin = 0) +
        coord_flip() -> p
      
      print(p)

      .. %>% 
        mutate(log_death_rate = log10(death_rate / 1000000)) %>% 
        ggplot(aes(x=age, y =log_death_rate)) + 
        facet_grid(ethnicity ~ gender) + 
        geom_point() -> p
      print(p)
      
      .. %>% 
        mutate(log_death_rate = log10(death_rate / 1000000)) %>% 
        ggplot(aes(x = age, y = log_death_rate, shape = ethnicity, colour = ethnicity)) + 
        facet_wrap(~gender) + 
        geom_point() -> p
      print(p)
      
      NULL
    }
  )


# These figures, especially the last, give us much to ponder.
# It appears that BNH mortality rates are higher than those of WNH of the 
# same gender and age for almost all ages, but that there are some important 
# distinct features.
# In infancy, BNH mortality rates are an outlier compared with those of other ethnic groups
# Mortality rates for all ethnicities tend to increase more with the onset of 
# early adulthood for males than for females, but appear to do so even more for BNH than 
# WNH. 

# Now let's look at the relative differences by age for BNH compared with WNH

dta_tidy %>% 
  group_by(ethnicity, gender, age) %>% 
  summarise(
    deaths = sum(deaths),
    population = sum(population)
  ) %>% 
  ungroup() %>% 
  mutate(death_rate = deaths / population) %>% 
  filter(ethnicity %in% c("black_nh", "white_nh")) %>%
  select(ethnicity, gender, age, death_rate) %>% 
  spread(ethnicity, death_rate) %>% 
  mutate(relative_mortality = black_nh / white_nh) %>% 
  group_by(gender) %>% mutate(avg_relative_mortality = mean(relative_mortality)) %>% ungroup() %>% 
  select(gender, age, relative_mortality, avg_relative_mortality) %T>% ( 
    function(..){
      .. %>% 
        group_by(gender) %>% 
        select(gender, avg_relative_mortality) %>% 
        distinct() %>% 
        print()
    }
  ) %>% 
  ggplot(aes(x = age, ymax = relative_mortality, fill = gender)) +
  geom_ribbon(ymin = 1, alpha = 0.5) + 
  geom_hline(aes(yintercept = avg_relative_mortality, group = gender, color = gender))


# This shows that BNH tend to have substantially higher mortality risks at almost all ages,
# with the exception of females in their late teens. The highest relative hazards are 
# infancy, and the lowest at the oldest ages. 
# The average hazard at all ages is 1.66 for females, and 1.74 for males. 

# Let's now return to the crude relative rates we looked at earlier, and this time 
# produce something similar that controls better for differences in age distributions 
dta_tidy %>% 
  group_by(ethnicity, gender, age) %>% 
  summarise(deaths = sum(deaths), population = sum(population)) %>% 
  mutate(death_rate = deaths / population) %>% 
  ungroup() %>% 
  mutate(relative_death_rate = death_rate / death_rate[ethnicity == 'white_nh'& gender == 'Female']) %>% 
  group_by(ethnicity, gender) %>% 
  summarise(avg_rel_death_rate = mean(relative_death_rate)) %>% 
  arrange(avg_rel_death_rate)  %T>% print %>% 
  ungroup() %>% (
    function(..){
      .. %>% 
        unite(eth_gender, ethnicity, gender, remove = F) %>%
        mutate(eth_gender = forcats::fct_reorder(eth_gender, avg_rel_death_rate)) %>% 
        ggplot(
          aes(x = eth_gender, y = avg_rel_death_rate, colour = gender, fill = ethnicity)
        ) + 
        geom_bar(stat = "identity", size = 2,  alpha = 0.6) +
        geom_hline(yintercept = 1) + 
        coord_flip() 
    }
  )

# This reveals some very important patterns about the relative risk of dying at each age
# Compared with WNH females, hispanic females have around a 15% lower risk of dying each year
# With the exception of BHF females, females tend to have lower risks than males.
# WNH males have a 77% higher risk on average of dying each year than WNH females. 
# Hispanic males have a 65% higher risk each year, slightly lower than WNH males.
# BNH males have exceptionally high mortality risks, being around 210% more likely 
# to die at any age than WHN females. 


# We now might want to know more about the different ways that people die at different ages,
# and then how these differ between ages and ethnicities

dta_tidy %>% 
  group_by(ethnicity, gender, age) %>% 
  mutate(total_deaths = sum(deaths)) %>% 
  mutate(prop_deaths_by_code = deaths / total_deaths) %>% 
  ungroup() %>% 
  arrange(icd_code, ethnicity, gender, age) %>%
  ggplot(aes(x = age, y = prop_deaths_by_code, fill = icd_code)) + 
  facet_grid(ethnicity ~ gender) + 
  geom_area()


# This is really weird - the visualisations work for two ethnic groups, but not others

#Are the data only available at 5 year age groups for the two groups affected?

dta_tidy %>% 
  filter(ethnicity == "hispanic") %>% 
  xtabs(~ age + icd_code + gender, .)

#Instead this seems to be a different issue, multiple records for hispanic for a single age and ICD, rather than one

# I've now gone back to the earlier code and aggregated death and pop counts by ethnicity, which seems to fix this



# The visualisation is somewhat psychedelic, but there are too many ICD codes to make visual sense of.

# We may instead want to focus on one particular type of ICD class: deaths from external causes 

icd_code_lookup

# We can see from this lookup that this code is 'V01-Y89'.
# Let's now reduce the number of categories to just two 

dta_tidy %>% 
  mutate(is_external = ifelse(icd_code == "V01-Y89", "External causes", "Other causes")) %>%
  group_by(ethnicity, gender, age, is_external) %>% 
  summarise(deaths = sum(deaths), population = population[1]) %>% 
  ungroup() %>% 
  group_by(ethnicity, gender, age) %>% 
  mutate(total_deaths = sum(deaths)) %>% 
  mutate(prop_deaths_by_code = deaths / total_deaths) %>% 
  ungroup() %>%
  ggplot(aes(x = age, y = prop_deaths_by_code, fill = is_external)) + 
  facet_grid(ethnicity ~ gender) + 
  geom_area() +
  scale_fill_manual("Cause of death", values = c("red", "grey"))


# Let's now plot just the external death proportions on one plot for each gender

dta_tidy %>% 
  mutate(is_external = ifelse(icd_code == "V01-Y89", "External causes", "Other causes")) %>%
  group_by(ethnicity, gender, age, is_external) %>% 
  summarise(deaths = sum(deaths), population = population[1]) %>% 
  ungroup() %>% 
  group_by(ethnicity, gender, age) %>% 
  mutate(total_deaths = sum(deaths)) %>% 
  mutate(prop_deaths_by_code = deaths / total_deaths) %>% 
  ungroup() %>% 
  filter(is_external == "External causes") %>% 
  rename(prop_deaths_external = prop_deaths_by_code) %>% 
  ggplot(aes(x = age, y = prop_deaths_external, colour = ethnicity, linetype = ethnicity)) + 
  geom_line() +
  facet_wrap(~ gender)


# This shows that either a similar or smaller proportion of all deaths tend to occur in 
# BNH due to external causes than amongst other groups, with the exception of deaths during 
# childhood. 


