library(tidyverse)
glimpse(msleep)

#selecting columns

msleep %>%
  select(name, genus, sleep_total, awake) %>%
  glimpse()

#To add a chunk of columns use the start_col:end_col syntax:
msleep %>%
  select(name:order, sleep_total:sleep_cycle) %>%
  glimpse

#to deselect
msleep %>%
  select(-conservation, -(sleep_total:awake)) %>%
  glimpse
#to deselect and re-add delected column
msleep %>%
  select(-(name:awake), conservation) %>%
  glimpse

#Selecting columns based on partial column names
msleep %>%
  select(name, starts_with("sleep")) %>%
  glimpse

#Selecting columns based on regex: ny column that contains an ‘o’, followed by one or more other letters, and ‘er’.

#selecting based on regex
msleep %>%
  select(matches("o.+er")) %>%
  glimpse

#Selecting columns based pre-identi!ed columns
classification <- c("name", "genus", "vore", "order", "conservation")
msleep %>%
  select(!!classification)

#The select_if function allows you to pass functions which return logical statements
msleep %>%
  select_if(is.numeric) %>%
  glimpse

msleep %>%
  select_if(~!is.numeric(.)) %>%
  glimpse

#In fact, select_if allows you to select based on any logical function, not just based ondata type. It is possible to select all columns with an average above 500 for instance: you need to do it on only numeric variables only
msleep %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)

or shorter:
  ## Observations: 83
  ## Variables: 5
  ## $ name <chr> "Cheetah", "Owl monkey", "Mountain beaver", "Grea...
  ## $ genus <chr> "Acinonyx", "Aotus", "Aplodontia", "Blarina", "Bo...
  ## $ vore <chr> "carni", "omni", "herbi", "omni", "herbi", "herbi...
  ## $ order <chr> "Carnivora", "Primates", "Rodentia", "Soricomorph...
  ## $ conservation <chr> "lc", NA, "nt", "lc", "domesticated", NA, "vu", N...
  msleep %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)

msleep %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE) > 10)


msleep %>%
  select_if(~n_distinct(.) < 10) # tilda is you want to apply a fucntion

#Re-ordering columns with select function
msleep %>%
  select(conservation, sleep_total, name) %>%
  glimpse

#If you are just moving a few columns to the front, you can use everything() afterwards
#which will add all the remaining columns and save a lot of typing.

msleep %>%
  select(conservation, sleep_total, everything()) %>%
  glimpse

#Renaming columns
msleep %>%
  select(animal = name, sleep_total, extinction_threat = conservation) %>%
  glimpse

#To get all column names in uppercase, you can use
msleep %>%
  select_all(toupper)

#cleaning the data
#making an unclean database:
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("name", "sleep total", "brain weight")

msleep2 %>%
  select_all(~str_replace(., " ", "_"))

#making an unclean database:
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("Q1 name", "Q2 sleep total", "Q3 brain weight")
msleep2[1:3,]

msleep2 %>%
  select_all(~str_replace(., "Q[0-9]+", "")) %>%
  select_all(~str_replace(., " ", "_"))

#Row names to column
mtcars %>%
  head
mtcars %>%
  tibble::rownames_to_column("car_model") 

#These functions allow to you detect if a data frame has row names (has_rownames()), remove them (remove_rownames()), or convert them back-and-forth between an explicit column (rownames_to_column() and column_to_rownames()). Also included is rowid_to_column(), which adds a column at the start of the dataframe of ascending sequential row ids starting at 1. Note that this will remove any existing row names.
#has_rownames(.data)
#remove_rownames(.data)
#rownames_to_column(.data, var = "rowname")
#rowid_to_column(.data, var = "rowid")
#column_to_rownames(.data, var = "rowname")

#does data has rownames
has_rownames(mtcars)
has_rownames(iris)
remove_rownames(mtcars) %>% has_rownames()
# Convert between row names and column
mtcars_tbl <- rownames_to_column(mtcars, var = "car") %>% as_tibble()
mtcars_tbl

column_to_rownames(mtcars_tbl, var = "car") %>% head()

# Adding rowid as a column --------------------------------------------
rowid_to_column(iris) %>% head()

#Part 2........................
#mutate
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_min = sleep_total * 60)

msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_vs_AVG = sleep_total - round(mean(sleep_total), 1),
         sleep_total_vs_MIN = sleep_total - min(sleep_total))

#alternative to using the actual arithmetics:
msleep %>%
  select(name, contains("sleep")) %>%
  rowwise() %>%
  mutate(avg = mean(c(sleep_rem, sleep_cycle)))
#ifelse..you don’t want to mutate the whole column in the same way.
msleep %>%
  select(name, brainwt) %>%
  mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
  arrange(desc(brainwt))
#You can also mutate string columns with stringr’s str_extract() function incombination with any character or regex patterns.
msleep %>%
  select(name) %>%
  mutate(name_last_word = tolower(str_extract(name, pattern = "\\w+$")))

#mutating several columns at once
#mutate_all() will mutate all columns based on your further instructionsmutate_if() first requires a function that returns a boolean to select columns. Ifthat is true, the mutate instructions will be followed on those variables.mutate_at() requires you to specify columns inside a vars() argument forwhich the mutation will be done.
msleep %>%
  mutate_all(tolower) #mutating action needs to be a func

#Mutate ifNot all cleaning functions can be done with mutate_all()
msleep %>% mutate_all(round)

#By using mutate_if() we need two arguments inside a pipe:First it needs information about the columns you want it to consider. Thisinformation needs to be a function that returns a boolean value. The easiest casesare functions like is.numeric , is.integer , is.double , is.logical ,is.factor , lubridate::is.POSIXt or lubridate::is.Date .Secondly, it needs instructions about the mutation in the form of a function. Ifneeded, use a tilde or funs() before (see above).

msleep %>%
  select(name, sleep_total:bodywt) %>%
  mutate_if(is.numeric, round)

#Mutate at to change speci!c columns
#First it needs information about the columns you want it to consider. In this caseyou can wrap any selection of columns (using all the options possible inside a select() function) and wrap it inside vars() .Secondly, it needs instructions about the mutation in the form of a function. Ifneeded, use a tilde or funs() before (see above).

msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60))

#Changing column names after mutation
msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60)) %>%
  rename_at(vars(contains("sleep")), ~paste0(.,"_min"))

#Or as Tomas McManus pointed out: you can assign a “tag” inside funs() which will beappended to the current name.

msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), funs(min = .*60))

#Recoding discrete columns
msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)
#A special version exists to return a factor: recode_factor():
#By default the .orderedargument is FALSE . To return an ordered factor set the argument to TRUE

msleep %>%
  mutate(conservation2 = recode_factor(conservation,
                                       "en" = "Endangered",
                                       "lc" = "Least_Concern",
                                       "domesticated" = "Least_Concern",
                                       .default = "other",
                                       .missing = "no data",
                                       .ordered = TRUE)) %>%
  count(conservation2)

#Creating new discrete column (two levels) with iflese
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_time = ifelse(sleep_total > 10, "long", "short"))

#Creating new discrete column (multiple levels): case_when() which allows as many statements as you like and is easier to read than many nested ifelse statements.

msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr = case_when(
    sleep_total > 13 ~ "very long",
    sleep_total > 10 ~ "long",
    sleep_total > 7 ~ "limited",
    TRUE ~ "short")) %>%
  mutate(sleep_total_discr = factor(sleep_total_discr,
                                    levels = c("short", "limited",
                                               "long", "very long")))

#The case_when() function does not only work inside a column, but can be used forgrouping across columns:
msleep %>%
  mutate(silly_groups = case_when(
    brainwt < 0.001 ~ "light_headed",
    sleep_total > 10 ~ "lazy_sleeper",
    is.na(sleep_rem) ~ "absent_rem",
    TRUE ~ "other")) %>% count(silly_groups)

#Splitting and merging columns
(conservation_expl <- read_csv("conservation_explanation.csv"))

#You can unmerge any columns by using tidyr’s separate()To do this, youhave to specify the column to be splitted, followed by the new column names, and which seperator it has to look for.
#The sample code shows seperating into two columns based on ‘=’ as a separator.

(conservation_table <- conservation_expl %>%
    separate(`conservation abbreviation`,
             into = c("abbreviation", "description"), sep = " = "))

#The opposite is tidyr’s unite() function. You specify the new column name, and thenthe columns to be united, and lastly what seperator you want to use
conservation_table %>%
  unite(united_col, abbreviation, description, sep=": ")

#joining other tables
msleep %>%
  select(name, conservation) %>%
  mutate(conservation = toupper(conservation)) %>%
  left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
  mutate(description = ifelse(is.na(description), conservation, description))


#Spreading and gathering data


#Turning data into NA
msleep %>%
  select(name:order) %>%
  na_if("omni") #any value -== omit is NA

#Data Wrangling Part 3: Basic and more advanced ways to !lter rows
library(dplyr)
library(stringr)
msleep <- ggplot2::msleep

#Basic row filters: > , >= , < , <= , == and != .
msleep %>%
  select(name, sleep_total) %>%
  filter(sleep_total > 18)

msleep %>%
  select(name, sleep_total) %>%
  filter(between(sleep_total, 16, 18)) #sleep_total >= 16, sleep_total <= 18

#Another function that can come in handy is near() , which will select all code that isnearly a given value. You have to specify a tolerance tol to indicate how far the valuescan be. You can add a specific number: filter(near(sleep_total, 17, tol = 0.5))for instance will return any rows where sleep_total is between 16.5 and 17.5, or you can add a formula.

msleep %>%
  select(name, sleep_total) %>%
  filter(near(sleep_total, 17, tol = sd(sleep_total)))

#Filtering based on a exact character variable matches
msleep %>%
  select(order, name, sleep_total) %>%
  filter(order == "Didelphimorphia")

#filter(order != "Rodentia") will select everything except the Rodentia rows.filter(name > "v") will just select the rows with a name in the alphabet after theletter v.
#If you want to select more than one animal you can use the %in% operator.

msleep %>%
  select(order, name, sleep_total) %>%
  filter(order %in% c("Didelphimorphia", "Diprotodontia")) #Thefollowing code will just select the rows with animals belonging to the order of Didelphimorphia and Diprotodontia.

remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>%
  select(order, name, sleep_total) %>%
  filter(!order %in% remove)


#filtering based on mult columns
msleep %>%
  select(name, order, sleep_total:bodywt) %>%
  filter(bodywt > 100, (sleep_total > 15 | order != "Carnivora"))

#eg with xor
msleep %>%
  select(name, bodywt:brainwt) %>%
  filter(xor(bodywt > 100, brainwt > 1))


msleep %>%
  select(name, sleep_total, brainwt, bodywt) %>%
  filter(brainwt > 1, !bodywt > 100)

#Filtering out empty rows
msleep %>%
  select(name, conservation:sleep_cycle) %>%
  filter(!is.na(conservation))

#Filtering across multiple columns
#The dplyr package has a few powerful variants to filter across multiple columns in onego:filter_all() will filter all columns based on your further instructionsfilter_if() requires a function that returns a boolean to indicate which columnsto filter on. If that is true, the filter instructions will be followed for those columns.filter_at() requires you to specify columns inside a vars() argument forwhich the filtering will be done.

#The below code basically asks to retain any rows where any of the variables has thepattern “Ca” inside.
msleep %>%
  select(name:order, sleep_total, -vore) %>%
  filter_all(any_vars(str_detect(., pattern = "Ca")))

msleep %>%
  select(name, sleep_total:bodywt) %>%
  filter_all(any_vars(. < 0.1))

#Filter if: e I can specifythat I want to just filter on character variables. In this case I only get 7 rows.
msleep %>%
  select(name:order, sleep_total:sleep_rem) %>%
  filter_if(is.character, any_vars(is.na(.)))

#Filter at

msleep %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(sleep_total, sleep_rem), all_vars(.>5))

msleep %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(contains("sleep")), all_vars(.>5))


#Data Wrangling Part 4: Summarizing and slicing your dataApril 2, 2018
#Counting cases and adding counts
msleep %>%
  count(order, sort = TRUE)
msleep %>%
  count(order, vore, sort = TRUE)

#Adding the number of observations in a column
msleep %>%
  tally()

msleep %>%
  select(1:3) %>%
  add_tally()

#which takes a variable as argument, and adds acolumn which the number of observations. This saves the combination of grouping,mutating and ungrouping again.
msleep %>%
  select(name:vore) %>%
  add_count(vore)

#Summarising data

msleep %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep_day = mean(sleep_total)/24)

#Summarise allSimilarly to the filter, select and mutate functions, summarise() comes with threeadditional functions for doing things to multiple columns in one go:summarise_all() will summarise all columns based on your further instructionssummarise_if() requires a function that returns a boolean. If that is true, thesummary instructions will be followedsumarise_at() requires you to specify columns inside a vars() argument forwhich the summary will be done.

msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm=TRUE)

msleep %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE)

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  rename_if(is.numeric, ~paste0("avg_", .))

msleep %>%
  group_by(vore) %>%
  summarise_at(vars(contains("sleep")), mean, na.rm=TRUE) %>%
  rename_at(vars(contains("sleep")), ~paste0("avg_", .))

#Arranging rows
msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total)) %>%
  arrange(desc(avg_sleep))

msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

#Showing only part of your data
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(-5)

msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

#A random selection of rows
msleep %>%
  sample_frac(.1)

msleep %>%
  slice(50:55) #show rows 50 to 55

#.............replace.value function/
## data with zeroes as missing values in responses
data(poleff)
## data with NA missing values in responses
data(poleffna)

## convert NA to 0:
dd  <- replace.value(poleffna,c("xsayself","xsay1","xsay2","xsay3","xsay4","xsay5"))

## convert 0 to NA:
dd2 <- replace.value(poleff,c("xsayself","xsay1","xsay2","xsay3","xsay4","xsay5"),0,as.double(NA))
