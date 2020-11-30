
#install tidyverse to allow me use all the other data analysis tools
install.packages("tidyverse") #install tidyverse
library("tidyverse") # activate the package

# first section will read inbuilt data from the ggplot2 package called msleep
#install.packages("ggplot2") # I am installing ggplot2 to get access to the msleep dataset
#library("ggplot2") # library activates the ggplot2 package so that i can access it

workshop_data <- msleep # i have assigned the msleep data in ggplot2 to workshop_data
write_csv(workshop_data, "data/workshop_data.csv") # save in built dataset

#View
workshop_data %>% 
  View() # just allows you to view the overall data
# structure
workshop_data %>% 
  glimpse() # summarise the data for u
#write_csv(workshop_data, "data/workshop_data.csv")


#import dataset
workshop_data <- read_csv("data/workshop_data.csv")


#Selection
data1 <- workshop_data %>% #first step data
  select(name, genus, sleep_total, awake) %>% #second is selection
  glimpse() #3 see that data

#to deselect
data2 <- workshop_data %>% #first step data
  select(-c(name, genus, sleep_total, awake)) %>% #second is selection
  glimpse() #3 see that data

data3 <- workshop_data %>% #first step data
  select(-c(conservation:bodywt)) %>% #second is selection
  glimpse() #3 see that data

independentvar <- c("name", "genus", "vore", "order", "conservation") 

good_independent <- msleep %>%
  select(independentvar)

# Filtering
data_filter <- workshop_data %>% #first step data
  filter(bodywt > 4) %>% #second is selection
  glimpse() #3 see that data

# Mutate
data_mutate <- workshop_data %>% #first step data
  mutate(bodywt_sq = bodywt * bodywt) %>% 
  select(bodywt, bodywt_sq) %>%
  View() #3 see that data

#Putting all together
Final_data <- workshop_data %>% #first step data
  select(name, genus, sleep_total, awake, bodywt) %>% # sel variables
  filter(sleep_total > 3) %>% #filter rows based to satisfy this condition sleep_total > 20
  mutate(bodywt_sq = bodywt * bodywt,
         bodywt_cube = bodywt * bodywt * bodywt) 
   

