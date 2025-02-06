####################   DATA CLEANING ####################
################### CONTACT INFORMATION #################
################# MADE BY HAMPUS NORDHOLM ###############
#########################################################


# Neccesary libraries for data cleansing tasks --
library(tidyverse)
library(janitor)

#Excel ---- 
library(readxl)
library(writexl)


#        Data import and introduction --

# Read sheets into DF:s 

contact1 <- read_excel("contacts.xlsx",sheet=1)
contact2 <- read_excel("contacts.xlsx",sheet=2)

# Combine the sheets --

cont <- bind_rows(contact1,contact2)

#      Make columnnames consistent and clean -- 

cont <- cont %>% clean_names()

cont %>% view()

#                --  COLUMN PHONE -- 
#       Consists of unstandardized phone numbers 
#       Task: Consistency: (123) 456-7890

# Count min and max digits --

cont %>% 
  summarise(min=min(nchar(str_replace_all(phone,"\\D",""))))

cont %>% 
  summarise(max=max(nchar(str_replace_all(phone,"\\D",""))))

# Some phone numbers contains 10 numbers and some 11 
# We need to make a case when depending on 10 or 11 digits --
#                     REGEX --

cont <- cont %>% 
  mutate(phone = str_replace_all(phone,"\\D",""),  
    phone = case_when(str_length(phone) == 10 ~ str_replace(phone,
    "(\\d{3})(\\d{3})(\\d{4})","(\\1) \\2-\\3"),
    str_length(phone) == 11 ~ str_replace(phone,
    "(\\d{1})(\\d{3})(\\d{3})(\\d{4})","+\\1 (\\2) \\3-\\4"),
    TRUE ~ phone))

#     Sample 20 numbers to check for consistency -- 

cont %>% 
  select(phone) %>% 
  sample_n(20)

#         - Phone numbers DONE -


#                  -- COLUMN ADRESS ---

#        Some entry:s contains apt or suite, this need to 
#        be defined in a new column address 2. 

cont <- cont %>%
  mutate(adress2=str_extract(adress,"(Apt|Suite|Unit)\\s?\\d+[A-Za-z]*"),
         adress=str_remove(adress,"(Apt|Suite|Unit)\\s?\\d+[A-Za-z]*"))

cont <- cont %>%
  mutate(adress2=str_extract(adress,"(Apt.|Suite.|Unit.)\\s?\\d+[A-Za-z]*"),
         adress=str_remove(adress,"(Apt.|Suite.|Unit.)\\s?\\d+[A-Za-z]*"))


#                 -- COPYS OF ROWS WITH SECOND EMAIL -- 


cont <- cont %>%  
  mutate(
    has_second_email = !is.na(second_email),
    newfirstname = ifelse(has_second_email, str_extract(second_email, "^[^@]+"), NA)) %>%
    bind_rows(cont %>%
      filter(!is.na(second_email)) %>%
      mutate(
        first_name = newfirstname,  
        email = second_email,      
        second_email = NA)) %>%
  select(-c(newfirstname, has_second_email)) 



#                      -- DUPLICATE CONTROL -- 

cont %>% 
  distinct()

# cont: 2414 entry:s, cont distinct: 2414 entry:s. 

#               -- COMBINE RECORDS WITH IDENTICAL NAME --
#                       (Retain most complete info)

cont %>%
  group_by(first_name,last_name) %>%
  summarise(across(everything(), ~first(na.omit(.)))) %>%
  ungroup()



#               -- CHECKING CONSISTENCY OF 5 DIGIT ZIP --

cont %>%
  filter(str_detect(zip, "^\\d{5}$"))


# 2414 entrys --

#              -- ARRANGE BY COMPANY --

cont <- cont %>% 
  arrange(company_name)


#             -- COLUMN RELOCATION -- 

cont <- cont %>% 
  relocate(adress,adress2,.before = city) %>% 
  relocate(second_email,.after = email)


#             -- FINISHED -- SAVE XLSX DOCUMENT -- 

cont %>% write_xlsx("contactclean.xlsx")






