############## KAP for COVID SCRIPT #################
############# Author: Prabhjot Juttla

#### Import data set #######
library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)
library(ggpubr)
library(arm)
library(brant)
library(broom)
library(dplyr)
library(flextable)
library(librarian)
library(gtsummary)
library(here)
library(knitr)
library(MASS)
library(officer)
library(ordinal)
library(psych)
library(readxl)
library(rio)



setwd("C:/Users/Prabhjot/OneDrive/Documents/Magoma's work/KAP for COVID/kap final I am serious")
kap_for_covid <- import("KAP for Covid Results 1.xlsx")


kap_for_covid <- KAP_for_Covid_Results_1


# Clean column names  -----------------------------------------------------


#### Clean column names 
kap_for_covid_clean <- kap_for_covid %>%
  rename(timestamp = Timestamp,
         work_place = `Where do you work?`,
         sex = Sex,
         age = `Age in completed years`,
         cadre = Cadre,
         education = `Highest level of education`,
         information_sources = `Which are your sources of information on COVID-19? (tick all that apply)`,
         main_symptoms = `The main clinical symptoms of COVID-19 are (tick all that apply)`,
         knowledge_q1 = `There is currently no cure for COVID19, but early symptomatic and supportive treatment can help most patients recover from the infection.`,
         knowledge_q2 = `Most persons with COVID-19 will develop severe cases.`,
         knowledge_q3 = `Eating or being in contact with wild animals would result in the COVID-19 virus infection`,
         knowledge_q4 = `Persons with COVID-19 cannot transmit the virus to others when a fever is not present`,
         knowledge_q5 = `The COVID-19 virus spreads via respiratory droplets of infected individuals`,
         knowledge_q6 = `Wearing medical masks can prevent one from acquiring infection by the COVID-19 virus`,
         knowledge_q7 = `It is not necessary for children and young adults to take measures to prevent the infection by the COVID-19 virus`,
         knowledge_q8 = `Isolation and treatment of people who are infected with the COVID-19 virus are effective ways to reduce the spread of the virus`,
         knowledge_q9 = `In Kiambu County, the main facility for isolation of COVID-19 patients is:`,
         knowledge_q10 = `When at work, if I come across a patient with a high index of suspicion for COVID-19, what should i do? (tick all that apply)`,
         knowledge_q11 = `Data for a suspected COVID-19 patient is entered in: (tick all that apply)`,
         att_q1 = `Black race is protective towards COVID-19 disease`,
         att_q2 = `Wearing a well fitting mask is effective in preventing COVID-19`,
         att_q3 = `Using normal soap for hand washing can prevent you from getting COVID-19.`,
         att_q4 = `When a patient has signs and symptoms of COVID-19, i can confidently participate in the management of the patient`,
         att_q5 = `In my everyday work, i feel confident that i am safe and protected from contracting COVID-19`,
         att_q6 = `I am well trained on the use of PPE to protect me from contracting COVID-19`,
         att_q7 = `i am confident about the vaccine production process for the COVID-19 vaccine.`,
         att_q8 = `When the vaccine comes, i will take the vaccine.`,
         att_q9 = `Kenya is in a good position to contain COVID-19.`,
         att_q10 = `Kiambu county is in a good position to contain COVID-19`,
         att_q11 = `At the beginning of the COVID-19 pandemic, Kenya handled the pandemic well.`,
         att_q12 = `At the beginning of the COVID-19 pandemic, Kiambu County handled the pandemic well.`,
         att_q13 = `Currently, Kenya is handling the pandemic well.`,
         att_q14 = `Currently, Kiambu County is handling the pandemic well`,
         practise_q1 =`In the last 1 week, I have worn a mask when in contact with patients.`,
         practise_q2 =`In the last 1 week, I have worn PPE when in contact with patients`,
         practise_q3 =`In the last 1 week, I have refrained from shaking hands`,
         practise_q4 =`In the last 1 week, I have washed my hands before and after handling each patient`,
         practise_q5 =`In the last 1 week, I have avoided patients with signs and symptoms suggestive of COVID-19`,
         practise_q6 =`In the last 1 week, I have taken time to sensitize patients and their families on COVID-19`,
         att_q15 = `From my interaction with patients, I believe that the Kiambu county community is well sensitised on COVID-19`)




#### Select only columns with the data required
### Remove columns with qualitative data 
cleaned_kap_for_covid <- subset(kap_for_covid_clean, select = c(real_timestamp,
                                                                work_place,
                                                                sex,
                                                                age,
                                                                cadre,
                                                                education,
                                                                information_sources,
                                                                main_symptoms,
                                                                knowledge_q1,
                                                                knowledge_q2,
                                                                knowledge_q3,
                                                                knowledge_q4,
                                                                knowledge_q5,
                                                                knowledge_q6,
                                                                knowledge_q7,
                                                                knowledge_q8,
                                                                knowledge_q9,
                                                                knowledge_q10,
                                                                knowledge_q11,
                                                                att_q1,
                                                                att_q2,
                                                                att_q3,
                                                                att_q4,
                                                                att_q5,
                                                                att_q6,
                                                                att_q7,
                                                                att_q8,
                                                                att_q9,
                                                                att_q10,
                                                                att_q11,
                                                                att_q12,
                                                                att_q13,
                                                                att_q14,
                                                                practise_q1,
                                                                practise_q2,
                                                                practise_q3,
                                                                practise_q4,
                                                                practise_q5,
                                                                practise_q6,
                                                                att_q15))
#### Replace all the zero values (0) with NA
cleaned_kap_for_covid[cleaned_kap_for_covid == 0] <- NA

cleaned_kap_for_covid$work_place[cleaned_kap_for_covid$work_place == "Private Sector"] <- NA

str(cleaned_kap_for_covid)


########Proportions 

# Place of work proportions -----------------------------------------------


clean <- cleaned_kap_for_covid

clean <- clean %>% 
  rename(knowledge_q12 = main_symptoms)

clean$work_place <- as.factor(clean$work_place)

clean %>%               
  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# sex proportions ---------------------------------------------------------
clean$sex <- as.factor(clean$sex)

clean %>%               
  tabyl(sex) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Age proportions ---------------------------------------------------------
clean$age <- as.factor(clean$age)

clean %>%               
  tabyl(age) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# HIghest level of education proportion -----------------------------------

clean$education <- gsub("Other",
                        "other", clean$education)
clean$education <- gsub("Masters",
                        "masters", clean$education)
clean$education <- gsub("PhD",
                        "phd", clean$education)
clean$education <- gsub("Diploma",
                        "diploma", clean$education)

# To clean the education for bachelors ----------------------------------
###Replace brackets and slashes with nothing 

clean$education <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "",
                        clean$education, perl=TRUE)
clean$education <- gsub("Bachelors",
                        "bachelors", clean$education)

clean$education <- as.factor(clean$education)

clean %>%               
  tabyl(education) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Cadre proportions -------------------------------------------------------
###Cadres must be first subdivided into further categories. 
## 1. Patient facing 
## 2. Non-patient facing 
## 3. Public health staff 

##Health facility staff who interact with patients directly (Patient Facing)
####Medical Officers /Consultants, Nurses, Clinical Officers, 
##Dental officers / Consultants
##Dental Technologists/ Community Oral Health Officers
##Pharmacists / Consultants
##Pharmaceutical staff (Technologist/Technician)
##Laboratory staff (Technologist/Technician)
##Orthopedic Technologist/Technician
##Nutritionists, Radiographers. Physiotherapists / Occupational Therapists
##Mortuary Attendants

####Health facility staff who do not interact with patients directly (Not-patient facing)
##Health administrative officers and staff
##Health supportive staff (secretaries, accountants, procurement etc)
##Medical Engineering Technologist/Technicians
##Health Records & Information Officers
##Medical Social Worker
##Ambulance Drivers
##HTS (HIV testing services)


######Public Health Staff
##Community health volunteers
##Health Promotion Officers
##Public Health Officers/Community health officers

#### RENAME THE CADRES INTO A NEW COLUMN
### To clean the names thus far 
clean2 <- clean

clean2$cadre <- gsub("Health Administrative officers and staff",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Health Records and Information Officers and staff",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Community Health Volunteers",
                     "public health", clean2$cadre)

clean2$cadre <- gsub("Medical Social Workers",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Ambulance Drivers",
                     "nonpatient facing", clean2$cadre)


clean2$cadre <- gsub("Public Health Officers/Community Health Officers and staff",
                     "public health", clean2$cadre)

clean2$cadre <- gsub("Medical Engineering technologists/technicians",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Health Promotion Officers",
                     "public health", clean2$cadre)



#####This is where is gets messed up

# To clean the stubborn names for cadres ----------------------------------
###Replace brackets and slashes with nothing 

clean2$cadre <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "",
                     clean2$cadre, perl=TRUE)

clean2$cadre <- gsub("Health supportive staff",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("HTS staff",
                     "nonpatient facing", clean2$cadre)


clean2$cadre <- gsub("Community Health Assistants/Community Health Extension Workers",
                     "public health", clean2$cadre)



# Patient facing ----------------------------------------------------------
clean2$cadre <- gsub("Mortuary Attendants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Mortuary attendants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Physiotherapists/Occupational Therapists",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Laboratory staff",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Pharmaceutical Staff",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Pharmaceutical Staff",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Medical Officers/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Nurses",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Clinical Officers",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Dental officers/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Dental Officers/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Dental Technologists/Community Oral Health Officers",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Pharmacists/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Nutritionists",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Radiographers",
                     "patient facing", clean2$cadre)

clean2$cadre <- as.factor(clean2$cadre)
clean2 %>%               
  tabyl(cadre) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean <- clean2



# To check knowledge q 11 acc to only medical cadres.. --------

suspected_covid <- clean

suspected_covid %>%               
  tabyl(knowledge_q11) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Sources of information --------------------------------------------------
clean$information_sources <- gsub("Official international health organisation sites and media e.g WHO, CDC",
                                  "international health organisation", clean$information_sources)

clean$information_sources <- gsub("News Media e.g TV, radios, magazines, newspapers",
                                  "news media", clean$information_sources)

clean$information_sources <- gsub("Social Media e.g Whatsapp, Facebook, Twitter, Instagram",
                                  "social media", clean$information_sources)

clean$information_sources <- gsub("Official government sites and media e.g Ministry of health circulars",
                                  "government sites", clean$information_sources)

clean$information_sources <- gsub("Medical Journals and other research sites",
                                  "journals", clean$information_sources)

clean$information_sources <- gsub("Continuous medical education fora",
                                  "fora", clean$information_sources)


#####Information sources proportions
clean %>%               
  tabyl(information_sources) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

library(stringr)

clean$international <- str_count(clean$information_sources,
                                 "international health organisation")

clean$news <- str_count(clean$information_sources,
                        "news media")

clean$social <- str_count(clean$information_sources,
                          "social media")

clean$govt <- str_count(clean$information_sources,
                        "government sites")

clean$journals <- str_count(clean$information_sources,
                            "journals")

clean$fora <- str_count(clean$information_sources,
                        "fora")

clean$international <- as.numeric(clean$international)
clean$news <- as.numeric(clean$news)
clean$social <- as.numeric(clean$social)
clean$govt <- as.numeric(clean$govt)
clean$journals <- as.numeric(clean$journals)
clean$fora <- as.numeric(clean$fora)

clean %>%               
  tabyl(international) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(news) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(social) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(govt) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(fora) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(journals) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()




# Knowledge questions -----------------------------------------------------

# Knowledge question proportions ------------------------------------------


clean$knowledge_q1 <- as.factor(clean$knowledge_q1)

clean %>%               
  tabyl(knowledge_q1) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q2 <- as.factor(clean$knowledge_q2)

clean %>%               
  tabyl(knowledge_q2) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q3 <- as.factor(clean$knowledge_q3)
clean %>%               
  tabyl(knowledge_q3) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q4 <- as.factor(clean$knowledge_q4)
clean %>%               
  tabyl(knowledge_q4) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q5 <- as.factor(clean$knowledge_q5)
clean %>%               
  tabyl(knowledge_q5) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q6 <- as.factor(clean$knowledge_q6)
clean %>%               
  tabyl(knowledge_q6) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q7 <- as.factor(clean$knowledge_q7)
clean %>%               
  tabyl(knowledge_q7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q8 <- as.factor(clean$knowledge_q8)
clean %>%               
  tabyl(knowledge_q8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q9 <- as.factor(clean$knowledge_q9)
clean %>%               
  tabyl(knowledge_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

##### Split main symtpoms into individual strings 
str_split(string = cleaned_kap_for_covid$main_symptoms,
          pattern = ",")



clean$knowledge_q10 <- as.factor(clean$knowledge_q10)
clean %>%               
  tabyl(knowledge_q10) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q11 <- as.factor(clean$knowledge_q11)
clean %>%               
  tabyl(knowledge_q11) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# Attitude questions ------------------------------------------------------

###Remove "i dont see patients" 

clean$att_q1[clean$att_q1 == "i dont see patients"] <- NA
clean$att_q6[clean$att_q6 == "i dont see patients"] <- NA
clean$att_q6[clean$att_q6 == "i dont know"] <- NA
clean$att_q4[clean$att_q4 == "i dont know"] <- NA
clean$att_q4[clean$att_q4 == "i dont see patients"] <- NA
clean$att_q15[clean$att_q15 == "i dont see patients"] <- NA

##### For question 8, collapse "Agree" into "YES"
#### collapse "i dont know", "not sure" into "MAYBE"
#### collapse "Diagree" into "No"
#### to have three outputs: YES, NO, MAYBE 


# Attitude proportions ----------------------------------------------------


clean$att_q8 <- gsub("Agree",
                     "Yes",
                     clean$att_q8)
clean$att_q8 <- gsub("I dont know",
                     "Maybe",
                     clean$att_q8)

clean$att_q8 <- gsub("Not sure",
                     "Maybe",
                     clean$att_q8)

clean$att_q8 <- gsub("Disagree",
                     "No",
                     clean$att_q8)

clean$att_q8 <- as.factor(clean$att_q8)
clean %>%               
  tabyl(att_q8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q1 <- as.factor(clean$att_q1)
clean %>%               
  tabyl(att_q1) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q2 <- as.factor(clean$att_q2)
clean %>%               
  tabyl(att_q2) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q3 <- as.factor(clean$att_q3)
clean %>%               
  tabyl(att_q3) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q4 <- as.factor(clean$att_q4)
clean %>%               
  tabyl(att_q4) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q5 <- as.factor(clean$att_q5)
clean %>%               
  tabyl(att_q5) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q6 <- as.factor(clean$att_q6)
clean %>%               
  tabyl(att_q6) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q7 <- as.factor(clean$att_q7)
clean %>%               
  tabyl(att_q7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q8 <- as.factor(clean$att_q8)
clean %>%               
  tabyl(att_q8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q9 <- as.factor(clean$att_q9)
clean %>%               
  tabyl(att_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q10 <- as.factor(clean$att_q10)
clean %>%               
  tabyl(att_q7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q11 <- as.factor(clean$att_q11)
clean %>%               
  tabyl(att_q11) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q12 <- as.factor(clean$att_q12)
clean %>%               
  tabyl(att_q12) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q13 <- as.factor(clean$att_q13)
clean %>%               
  tabyl(att_q13) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q14 <- as.factor(clean$att_q14)
clean %>%               
  tabyl(att_q14) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q15 <- as.factor(clean$att_q15)
clean %>%               
  tabyl(att_q15) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()



# New practise score ------------------------------------------------------

#### Changed practices analysis to "patient facing" only

practise_score_new <- clean



####Q1:Worn mask, Always = 1,
## Occassional, Never = 0

practise_score_new %>%               
  tabyl(practise_q1) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q1 <- gsub("Always",
                                       "1", practise_score_new$practise_q1)

practise_score_new$practise_q1 <- gsub("Never",
                                       "0", practise_score_new$practise_q1)

practise_score_new$practise_q1 <- gsub("Occasionally",
                                       "0", practise_score_new$practise_q1)

practise_score_new$practise_q1[practise_score_new$practise_q1 == "i dont see patients"] <- NA

####Q2:Worn PPE, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q2) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q2 <- gsub("Always",
                                       "1", practise_score_new$practise_q2)
practise_score_new$practise_q2 <- gsub("Never",
                                       "0", practise_score_new$practise_q2)
practise_score_new$practise_q2 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q2)
practise_score_new$practise_q2[practise_score_new$practise_q2 == "i dont see patients"] <- NA

####Q3:Refrained from shaking hands, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q3) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q3 <- gsub("Always",
                                       "1", practise_score_new$practise_q3)
practise_score_new$practise_q3 <- gsub("Never",
                                       "0", practise_score_new$practise_q3)
practise_score_new$practise_q3 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q3)


####Q4:Washed my hands before and after, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q4) %>%       
  adorn_pct_formatting()
practise_score_new$practise_q4 <- gsub("Always",
                                       "1", practise_score_new$practise_q4)
practise_score_new$practise_q4 <- gsub("Never",
                                       "0", practise_score_new$practise_q4)
practise_score_new$practise_q4 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q4)

practise_score_new$practise_q4[practise_score_new$practise_q4 == "i dont see patients"] <- NA

####Q5:Avoided patients with signs and symptoms, Never = 1,
## Occassional, Always = 0
practise_score_new %>%               
  tabyl(practise_q5) %>%       
  adorn_pct_formatting()#

practise_score_new$practise_q5 <- gsub("Always",
                                       "0", practise_score_new$practise_q5)
practise_score_new$practise_q5 <- gsub("Never",
                                       "1", practise_score_new$practise_q5)
practise_score_new$practise_q5 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q5)

practise_score_new$practise_q5[practise_score_new$practise_q5 == "i dont see patients"] <- NA

####Q6:time to sensitize patients, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q6) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q6 <- gsub("Always",
                                       "1", practise_score_new$practise_q6)
practise_score_new$practise_q6 <- gsub("Never",
                                       "0", practise_score_new$practise_q6)
practise_score_new$practise_q6 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q6)

practise_score_new$practise_q6[practise_score_new$practise_q6 == "i dont see patients"] <- NA

# Convert type to numeric for attitude ------------------------------------

practise_score_new$practise_q1 <- as.numeric(practise_score_new$practise_q1)
practise_score_new$practise_q2 <- as.numeric(practise_score_new$practise_q2)
practise_score_new$practise_q3 <- as.numeric(practise_score_new$practise_q3)
practise_score_new$practise_q4 <- as.numeric(practise_score_new$practise_q4)
practise_score_new$practise_q5 <- as.numeric(practise_score_new$practise_q5)
practise_score_new$practise_q6 <- as.numeric(practise_score_new$practise_q6)


# Create a total practise score column -----------------------------------
practise_score_new$total_practise_score <- rowSums(cbind(practise_score_new$practise_q1,
                                                         practise_score_new$practise_q2,
                                                         practise_score_new$practise_q3,
                                                         practise_score_new$practise_q4,
                                                         practise_score_new$practise_q5,
                                                         practise_score_new$practise_q6),
                                                   na.rm = FALSE)


# Determine NEW PRACTICE out of 6 possible points  --------------
###THESE ARE NEW RESULTS... 

mean(practise_score_new$total_practise_score, na.rm = TRUE)
sd(practise_score_new$total_practise_score, na.rm = TRUE)
summary(practise_score_new$total_practise_score, na.rm = TRUE)

#practise_score_new$practise_type <- ifelse(test = practise_score_new$total_practise_score >= 4.588997,
#                                           yes = "good",
#                                           no = "bad")

# Assuming practise_score_new$total_practise_score is your numerical data vector
# Creating a new column "practise_type" based on the conditions

practise_score_new$practise_type <- ifelse(practise_score_new$total_practise_score > 4.8, "Excellent",
                                           ifelse(practise_score_new$total_practise_score >= 3.6, "Good", "Poor"))

practise_score_new %>%               
  tabyl(practise_type) %>%       
  adorn_pct_formatting()

practise_score_new$practise_type <- as.factor(practise_score_new$practise_type)


# Assign meaningful levels and reorder them
practise_score_new$practise_type <- factor(practise_score_new$practise_type, levels = c("Poor", "Good", "Excellent"))

# Verify the levels again
levels(practise_score_new$practise_type)
practise_score_new$practise_type <- as.factor(practise_score_new$practise_type)
summary(practise_score_new$practise_type)

clean <- practise_score_new


# Calculating the actual knowledge scores ---------------------------------
##Create a new data frame 

k_score <- clean

####Q1: there is no effective cure: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q1 <- gsub("TRUE",
                             "1", k_score$knowledge_q1)

k_score$knowledge_q1 <- gsub("FALSE",
                             "0", k_score$knowledge_q1)

k_score$knowledge_q1 <- gsub("I dont know",
                             "0", k_score$knowledge_q1)


#Q2: Most persons will develop severe cases: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q2 <- gsub("TRUE",
                             "0", k_score$knowledge_q2)

k_score$knowledge_q2 <- gsub("FALSE",
                             "1", k_score$knowledge_q2)

k_score$knowledge_q2 <- gsub("I dont know",
                             "0", k_score$knowledge_q2)

#Q3: Eating or being close to animals: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q3 <- gsub("TRUE",
                             "0", k_score$knowledge_q3)

k_score$knowledge_q3 <- gsub("FALSE",
                             "1", k_score$knowledge_q3)

k_score$knowledge_q3 <- gsub("I dont know",
                             "0", k_score$knowledge_q3)

#Q4: Persons with COVID-19 cannot transmit: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q4 <- gsub("TRUE",
                             "0", k_score$knowledge_q4)

k_score$knowledge_q4 <- gsub("FALSE",
                             "1", k_score$knowledge_q4)

k_score$knowledge_q4 <- gsub("I dont know",
                             "0", k_score$knowledge_q4)

#Q5: Spreads via respiratory droplets: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q5 <- gsub("TRUE",
                             "1", k_score$knowledge_q5)

k_score$knowledge_q5 <- gsub("FALSE",
                             "0", k_score$knowledge_q5)

k_score$knowledge_q5 <- gsub("I dont know",
                             "0", k_score$knowledge_q5)

#Q6: Wearing masks can prevent one from acquiring: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q6 <- gsub("TRUE",
                             "1", k_score$knowledge_q6)

k_score$knowledge_q6 <- gsub("FALSE",
                             "0", k_score$knowledge_q6)

k_score$knowledge_q6 <- gsub("I dont know",
                             "0", k_score$knowledge_q6)

#Q7: Not necessary for kids to take measures: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q7 <- gsub("TRUE",
                             "0", k_score$knowledge_q7)

k_score$knowledge_q7 <- gsub("FALSE",
                             "1", k_score$knowledge_q7)

k_score$knowledge_q7 <- gsub("I dont know",
                             "0", k_score$knowledge_q7)

#Q8: Isolation & treatment are effective: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q8 <- gsub("TRUE",
                             "1", k_score$knowledge_q8)

k_score$knowledge_q8 <- gsub("FALSE",
                             "0", k_score$knowledge_q8)

k_score$knowledge_q8 <- gsub("I dont know",
                             "0", k_score$knowledge_q8)

#Q9: Kiambu county main facility: Tigoni = 1, anything else = 0
k_score$knowledge_q9 <- gsub("Tigoni L4 Hospital",
                             "1", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Kiambu L4 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Thika L5 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Lari L4 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Gatundu L5 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("i dont know",
                             "0", k_score$knowledge_q9)

k_score %>%               
  tabyl(knowledge_q9) %>%       
  adorn_pct_formatting()

#Q10: High index of suspicion for COVID-19
#Give mask. Isolate somewhere, inform the surveillance officer
#This is the correct answer as at when we were collecting data – at that time.

k_score %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

k_score$knowledge_q10 <- gsub("Give the patient a mask",
                              "1", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Inform the disease surveillance coordinator",
                              "1", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Isolate the patient and the patient relatives",
                              "1", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Call the subcounty COVID hotline",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Send the patient and relatives to the holding area",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Find and use the case definition to decide",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Send the paitent to the lab for the COVID-19test",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Send the paitent to the lab for the COVID-19test",
                              "0", k_score$knowledge_q10)

k_score %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

k_score$knowledge_q10 <- gsub("Inform the Medical Superintendent or hospital in charge",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("I dont know what to do",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- sapply(strsplit(k_score$knowledge_q10,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

k_score %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()


# Patient facing, non-patient facing and environemntal health staff Q10 -------

patient_facing_q10 <- k_score %>%
  filter(k_score$cadre == "patient facing")

patient_facing_q10 %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

nonpatient_facing_q10 <- k_score %>%
  filter(k_score$cadre == "nonpatient facing")

nonpatient_facing_q10 %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

public_q10 <- k_score %>%
  filter(k_score$cadre == "public health")

public_q10 %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

# Q11: Data for suspected COVID-19 patients: patient notes/case investigation form = 1,
###    anything else = 0

k_score %>%               
  tabyl(knowledge_q11) %>%       
  adorn_pct_formatting()

k_score$knowledge_q11 <- gsub("Patient notes",
                              "1", k_score$knowledge_q11)

k_score$knowledge_q11 <- gsub("Case definition form",
                              "0", k_score$knowledge_q11)

k_score$knowledge_q11 <- gsub("Case investigation form",
                              "1", k_score$knowledge_q11)

k_score$knowledge_q11 <- gsub("Clinical register",
                              "0", k_score$knowledge_q11)

k_score %>%               
  tabyl(knowledge_q11) %>%       
  adorn_pct_formatting()

k_score$knowledge_q11 <- gsub("I dont know",
                              "0", k_score$knowledge_q11)

k_score$knowledge_q11 <- sapply(strsplit(k_score$knowledge_q11,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

k_score %>%               
  tabyl(knowledge_q11) %>%       
  adorn_pct_formatting()

patientsss <- k_score %>%
  filter(k_score$cadre == "patient facing")

patientsss %>%
  tabyl(knowledge_q11) %>%
  adorn_pct_formatting()

###Q12: Main symptoms of COVID-19: 
## Fever, smell disturbance and cough = 1 each, anything else = 0. 
k_score %>%               
  tabyl(knowledge_q12) %>%       
  adorn_pct_formatting()

k_score$knowledge_q12 <- gsub("Fever",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Cough",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Smell disturbance",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("smell disturbance",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Confusion",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Confusion",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "",
                              k_score$knowledge_q12, perl=TRUE)

k_score %>%               
  tabyl(knowledge_q12) %>%       
  adorn_pct_formatting()

k_score$knowledge_q12 <- gsub("Sore throat",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Diarrhoea",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Myalgia",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Sneezing",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Runny nose",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Headache",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Runny nose",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Hair loss",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Seizures",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("lack of taste",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("runny nose",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("sore throat",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("diarrhoea",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("cough",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Burning sensetional",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("General body pain",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Skin color changes",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("sneezing",
                              "0", k_score$knowledge_q12)

str(k_score$knowledge_q1)

######Create k_score2 to remove the zeroes first, then remove the commas using
### "replace"

#### knowledge q1, 2,3,4,5,6,7,8,9 all single, 
## so can be converted to numeric
#### knowledge q10, 11, 12 have commas. 

k_score$knowledge_q1 <- as.numeric(k_score$knowledge_q1)
k_score$knowledge_q2 <- as.numeric(k_score$knowledge_q2)
k_score$knowledge_q3 <- as.numeric(k_score$knowledge_q3)
k_score$knowledge_q4 <- as.numeric(k_score$knowledge_q4)
k_score$knowledge_q5 <- as.numeric(k_score$knowledge_q5)
k_score$knowledge_q6 <- as.numeric(k_score$knowledge_q6)
k_score$knowledge_q7 <- as.numeric(k_score$knowledge_q7)
k_score$knowledge_q8 <- as.numeric(k_score$knowledge_q8)
k_score$knowledge_q9 <- as.numeric(k_score$knowledge_q9)

### create a column adding up all the scores for Q1 to 9

###Separate the numbers in Q10, Q11, Q12
k_score$knowledge_q10 <- as.character(k_score$knowledge_q10)

k_score$knowledge_q10 <- sapply(strsplit(k_score$knowledge_q10,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

# Adding the values in the knowledge q11-13 -------------------------------
k_score$knowledge_q11 <- as.character(k_score$knowledge_q11)
k_score$knowledge_q12 <- as.character(k_score$knowledge_q12)


k_score$knowledge_q11 <- sapply(strsplit(k_score$knowledge_q11,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

k_score$knowledge_q12 <- sapply(strsplit(k_score$knowledge_q12,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

# Create a total knowledge score column -----------------------------------
k_score$total_kscore <- rowSums(cbind(k_score$knowledge_q1,
                                      k_score$knowledge_q10,
                                      k_score$knowledge_q2,
                                      k_score$knowledge_q3,
                                      k_score$knowledge_q4,
                                      k_score$knowledge_q5,
                                      k_score$knowledge_q6,
                                      k_score$knowledge_q7,
                                      k_score$knowledge_q8,
                                      k_score$knowledge_q9,
                                      k_score$knowledge_q11,
                                      k_score$knowledge_q12),
                                na.rm = FALSE)


# Determine mean knowledge score out of 16 possible points  --------------

mean(k_score$total_kscore, na.rm = TRUE)
sd(k_score$total_kscore, na.rm = TRUE)
summary(k_score$total_kscore)


# Knowledge below mean  -----------------------------------------------

#### hOW MANY ARE BELOW MEAN? below == 11.6 = bad knowledge

#k_score$knowledge_type <- ifelse(test = k_score$total_kscore >= 11.52,
#                                 yes = "good",
#                                 no = "bad")


##### New Ordinal Knowledge type

k_score$knowledge_type <- ifelse(k_score$total_kscore > 12.8, "Excellent",
                                 ifelse(k_score$total_kscore >= 9.6, "Good", "Poor"))

k_score %>%               
  tabyl(knowledge_type) %>%       
  adorn_pct_formatting()

k_score$knowledge_type <- as.factor(k_score$knowledge_type)


# Assign meaningful levels and reorder them
k_score$knowledge_type <- factor(k_score$knowledge_type, levels = c("Poor", "Good", "Excellent"))

# Verify the levels again
levels(k_score$knowledge_type)



#####OLD METHOD

mean(k_score$total_kscore, na.rm = TRUE)
sd(k_score$total_kscore, na.rm = TRUE)
summary(k_score$total_kscore, na.rm = TRUE)


#k_score$old_know <- ifelse(k_score$total_kscore > 11.52, "1",
#                                           ifelse(practise_score_new$total_practise_score < 11.52, "0"))
#k_score$old_know <- as.factor(k_score$old_know)


clean <- k_score

# Making attitude a numeric based on responses ----------------------------
##Create a new data frame 

att_score <- clean


####Q1: Black race is protective: Strongly disagree/disagree = 1,
## Not sure, agree, strongly agree = 0
att_score$att_q1 <- gsub("Strongly disagree",
                         "1", att_score$att_q1)

att_score$att_q1 <- gsub("disagree",
                         "1", att_score$att_q1)

att_score$att_q1 <- gsub("strongly agree",
                         "0", att_score$att_q1)

att_score$att_q1 <- gsub("agree",
                         "0", att_score$att_q1)

att_score$att_q1 <- gsub("not sure",
                         "0", att_score$att_q1)

att_score$att_q1 <- gsub("i dont know",
                         "0", att_score$att_q1)

att_score %>%               
  tabyl(att_q1) %>%       
  adorn_pct_formatting()

####Q2: Wearing a well-fitting mask is protective: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0
att_score$att_q2 <- gsub("strongly agree",
                         "1", att_score$att_q2)

att_score$att_q2 <- gsub("agree",
                         "1", att_score$att_q2)

att_score$att_q2 <- gsub("Strongly disagree",
                         "0", att_score$att_q2)

att_score$att_q2 <- gsub("disagree",
                         "0", att_score$att_q2)
att_score$att_q2 <- gsub("dis1",
                         "0", att_score$att_q2)
att_score$att_q2 <- gsub("Strongly dis1",
                         "0", att_score$att_q2)
att_score$att_q2 <- gsub("Strongly 0",
                         "0", att_score$att_q2)

att_score$att_q2 <- gsub("not sure",
                         "0", att_score$att_q2)

att_score$att_q2 <- gsub("i dont know",
                         "0", att_score$att_q2)

att_score %>%               
  tabyl(att_q2) %>%       
  adorn_pct_formatting()

####Q3: Using normal soap: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0
att_score$att_q3 <- gsub("strongly agree",
                         "1", att_score$att_q3)

att_score$att_q3 <- gsub("agree",
                         "1", att_score$att_q3)

att_score$att_q3 <- gsub("Strongly disagree",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("disagree",
                         "0", att_score$att_q3)
att_score$att_q3 <- gsub("dis1",
                         "0", att_score$att_q3)
att_score$att_q3 <- gsub("Strongly dis1",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("not sure",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("strongly 0",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("i dont know",
                         "0", att_score$att_q3)

att_score %>%               
  tabyl(att_q3) %>%       
  adorn_pct_formatting()

####Q4: I can confidently manage a patient: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0
att_score$att_q4 <- gsub("Strongly agree",
                         "1", att_score$att_q4)
att_score$att_q4 <- gsub("Agree",
                         "1", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly disagree",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Disagree",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Dis1",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly dis1",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Not sure",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly 0",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly 1",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("i dont know",
                         "0", att_score$att_q4)

att_score$att_q4[att_score$att_q4 == "i dont see patients"] <- NA

att_score %>%               
  tabyl(att_q4) %>%       
  adorn_pct_formatting()

####Q5: I feel safe in my everyday work: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q5) %>%       
  adorn_pct_formatting()

att_score$att_q5 <- gsub("Strongly agree",
                         "1", att_score$att_q5)
att_score$att_q5 <- gsub("Agree",
                         "1", att_score$att_q5)
att_score$att_q5 <- gsub("Not sure",
                         "0", att_score$att_q5)
att_score$att_q5 <- gsub("Strongly disagree",
                         "0", att_score$att_q5)
att_score$att_q5 <- gsub("Disagree",
                         "0", att_score$att_q5)

####Q6: I am well trained: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q6) %>%       
  adorn_pct_formatting()

att_score$att_q6 <- gsub("Strongly agree",
                         "1", att_score$att_q6)
att_score$att_q6 <- gsub("Agree",
                         "1", att_score$att_q6)
att_score$att_q6 <- gsub("Not sure",
                         "0", att_score$att_q6)
att_score$att_q6 <- gsub("i dont know",
                         "0", att_score$att_q6)
att_score$att_q6 <- gsub("Strongly disagree",
                         "0", att_score$att_q6)
att_score$att_q6 <- gsub("Disagree",
                         "0", att_score$att_q6)

att_score$att_q6[att_score$att_q6 == "i dont see patients"] <- NA

####Q7: I am confident abt vaccine: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q7) %>%       
  adorn_pct_formatting()

att_score$att_q7 <- gsub("Strongly agree",
                         "1", att_score$att_q7)
att_score$att_q7 <- gsub("Agree",
                         "1", att_score$att_q7)
att_score$att_q7 <- gsub("Not sure",
                         "0", att_score$att_q7)

att_score$att_q7 <- gsub("Strongly disagree",
                         "0", att_score$att_q7)
att_score$att_q7 <- gsub("Disagree",
                         "0", att_score$att_q7)

####Q8: I am confident about vaccine: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q8) %>%       
  adorn_pct_formatting()

att_score$att_q8 <- gsub("Yes",
                         "1", att_score$att_q8)
att_score$att_q8 <- gsub("Agree",
                         "1", att_score$att_q8)
att_score$att_q8 <- gsub("Not sure",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("I dont know",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("Maybe",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("No",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("Strongly disagree",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("Disagree",
                         "0", att_score$att_q8)

####Q9:Kenya is in a good position to contain COVID-19: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q9) %>%       
  adorn_pct_formatting()

att_score$att_q9 <- gsub("strongly agree",
                         "1", att_score$att_q9)
att_score$att_q9 <- gsub("agree",
                         "1", att_score$att_q9)
att_score$att_q9 <- gsub("not sure",
                         "0", att_score$att_q9)
att_score$att_q9 <- gsub("strongly dis1",
                         "0", att_score$att_q9)
att_score$att_q9 <- gsub("dis1",
                         "0", att_score$att_q9)

####Q10:Kiambu is in a good position to contain COVID-19: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q10) %>%       
  adorn_pct_formatting()

att_score$att_q10 <- gsub("strongly agree",
                          "1", att_score$att_q10)
att_score$att_q10 <- gsub("agree",
                          "1", att_score$att_q10)
att_score$att_q10 <- gsub("not sure",
                          "0", att_score$att_q10)
att_score$att_q10 <- gsub("Strongly 0",
                          "0", att_score$att_q10)
att_score$att_q10 <- gsub("dis1",
                          "0", att_score$att_q10)

####Q11:in the beginning, Kenya handled it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q11) %>%       
  adorn_pct_formatting()

att_score$att_q11 <- gsub("Strongly Agree",
                          "1", att_score$att_q11)
att_score$att_q11 <- gsub("Agree",
                          "1", att_score$att_q11)
att_score$att_q11 <- gsub("Neutral",
                          "0", att_score$att_q11)
att_score$att_q11 <- gsub("Disagree",
                          "0", att_score$att_q11)
att_score$att_q11 <- gsub("Strongly 0",
                          "0", att_score$att_q11)

####Q12:in the beginning, Kiambu handled it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q12) %>%       
  adorn_pct_formatting()

att_score$att_q12 <- gsub("strongly agree",
                          "1", att_score$att_q12)
att_score$att_q12 <- gsub("agree",
                          "1", att_score$att_q12)
att_score$att_q12 <- gsub("not sure",
                          "0", att_score$att_q12)
att_score$att_q12 <- gsub("dis1",
                          "0", att_score$att_q12)
att_score$att_q12 <- gsub("Strongly 0",
                          "0", att_score$att_q12)

####Q13:Currently, Kenya handling it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q13) %>%       
  adorn_pct_formatting()

att_score$att_q13 <- gsub("Strongly agree",
                          "1", att_score$att_q13)
att_score$att_q13 <- gsub("Agree",
                          "1", att_score$att_q13)
att_score$att_q13 <- gsub("Not sure",
                          "0", att_score$att_q13)
att_score$att_q13 <- gsub("Disagree",
                          "0", att_score$att_q13)
att_score$att_q13 <- gsub("Strongly disagree",
                          "0", att_score$att_q13)

####Q14:Currently, Kiambu handling it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q14) %>%       
  adorn_pct_formatting()

att_score$att_q14 <- gsub("Strongly agree",
                          "1", att_score$att_q14)
att_score$att_q14 <- gsub("Agree",
                          "1", att_score$att_q14)
att_score$att_q14 <- gsub("Not sure",
                          "0", att_score$att_q14)
att_score$att_q14 <- gsub("Disagree",
                          "0", att_score$att_q14)
att_score$att_q14 <- gsub("Strongly disagree",
                          "0", att_score$att_q14)


####Q13:From my interaction with patients: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q15) %>%       
  adorn_pct_formatting()

att_score$att_q15 <- gsub("Strongly Agree",
                          "1", att_score$att_q15)
att_score$att_q15 <- gsub("Agree",
                          "1", att_score$att_q15)
att_score$att_q15 <- gsub("not sure",
                          "0", att_score$att_q15)
att_score$att_q15 <- gsub("disagree",
                          "0", att_score$att_q15)
att_score$att_q15 <- gsub("Strongly 0",
                          "0", att_score$att_q15)

att_score$att_q15[att_score$att_q15 == "i dont see patients"] <- NA



# Convert type to numeric for attitude ------------------------------------


att_score$att_q1 <- as.numeric(att_score$att_q1)
att_score$att_q2 <- as.numeric(att_score$att_q2)
att_score$att_q3 <- as.numeric(att_score$att_q3)
att_score$att_q4 <- as.numeric(att_score$att_q4)
att_score$att_q5 <- as.numeric(att_score$att_q5)
att_score$att_q6 <- as.numeric(att_score$att_q6)
att_score$att_q7 <- as.numeric(att_score$att_q7)
att_score$att_q8 <- as.numeric(att_score$att_q8)
att_score$att_q9 <- as.numeric(att_score$att_q9)
att_score$att_q10 <- as.numeric(att_score$att_q10)
att_score$att_q11 <- as.numeric(att_score$att_q11)
att_score$att_q12 <- as.numeric(att_score$att_q12)
att_score$att_q13 <- as.numeric(att_score$att_q13)
att_score$att_q14 <- as.numeric(att_score$att_q14)
att_score$att_q15 <- as.numeric(att_score$att_q15)

# Create a total attitude score column -----------------------------------
att_score$total_att_score <- rowSums(cbind(att_score$att_q1,
                                           att_score$att_q2,
                                           att_score$att_q3,
                                           att_score$att_q4,
                                           att_score$att_q5,
                                           att_score$att_q6,
                                           att_score$att_q7,
                                           att_score$att_q8,
                                           att_score$att_q9,
                                           att_score$att_q10,
                                           att_score$att_q11,
                                           att_score$att_q12,
                                           att_score$att_q13,
                                           att_score$att_q14,
                                           att_score$att_q15),
                                     na.rm = FALSE)

# Determine attitude out of 15 possible points  --------------

mean(att_score$total_att_score, na.rm = TRUE)
sd(att_score$total_att_score, na.rm = TRUE)
summary(att_score$total_att_score, na.rm = TRUE)

#att_score$att_type <- ifelse(test = att_score$total_att_score >= 8.565217,
#                                 yes = "good",
#                                 no = "bad")

# Assuming att_score$total_att_score is your numerical data vector
# Creating a new column "att_type" based on the conditions

# Create a new variable att_type based on the threshold value of 12
att_score$att_type <- ifelse(att_score$total_att_score > 12, "Excellent", 
                             ifelse(att_score$total_att_score >= 9, "Good", "Poor"))


# This code categorizes total_att_score into three categories: Excellent (>12), Good (>8 and <=12), and Medium (<=8)



att_score %>%               
  tabyl(att_type) %>%       
  adorn_pct_formatting()

att_score$att_type <- as.factor(att_score$att_type)


# Assign meaningful levels and reorder them
att_score$att_type <- factor(att_score$att_type, levels = c("Poor", "Good", "Excellent"))

# Verify the levels again
levels(att_score$att_type)

att_score$att_type <- as.factor(att_score$att_type)
summary(att_score$att_type)

clean <- att_score



# Exact p-values in output --------

options(scipen = 999)


# Remove the columns of interest ------------------------------------------

# Subset specific rows and columns
clean_OLS <- subset(clean, select = c(sex, age, 
                                        work_place, education, 
                                        cadre, govt, 
                                        news, international, social, fora, 
                                      journals, knowledge_type, att_type, practise_type
  
))

# Replace column names with the ones you want to keep



# Ordinal logistic regression: Package installation ---------------------------------------------

############# Perform ordinal logistic regression


# Knowledge type ordinal logistic regression ------------------------------

total_kscore <- clean$total_kscore
ecdf_function <- ecdf(total_kscore)
plot(ecdf_function, main="Cumulative Distribution Function", xlab="Knowledge", ylab="Cumulative Probability")
plot(ecdf_function, main="Cumulative Distribution Function", xlab="Total K-Score", ylab="Cumulative Probability", col="blue", type="l", xlim=c(min(total_kscore), max(total_kscore)), ylim=c(0, 1))


total_att_score <- clean$total_att_score
ecdf_function1 <- ecdf(total_att_score)
plot(ecdf_function1, main="Cumulative Distribution Function", xlab="Attitude", ylab="Cumulative Probability")
plot(ecdf_function1, main="Cumulative Distribution Function", xlab="Total Att-Score", ylab="Cumulative Probability", col="blue", type="l", xlim=c(min(total_kscore), max(total_kscore)), ylim=c(0, 1))


total_prac_score <- clean$total_practise_score
ecdf_function11 <- ecdf(total_prac_score)
plot(ecdf_function11, main="Cumulative Distribution Function", xlab="Practice", ylab="Cumulative Probability")
plot(ecdf_function11, main="Cumulative Distribution Function", xlab="Practice-Score", ylab="Cumulative Probability", col="blue", type="l", xlim=c(min(total_kscore), max(total_kscore)), ylim=c(0, 1))



# Create a null model

####Method 1
null_model_knowledge <- polr(knowledge_type ~ 1, 
                  data = clean_OLS, 
                  method = "probit")


summary(null_model_knowledge)


# Create the full logistic regression model

###Method 1
full_model_knowledge <- polr(knowledge_type ~ sex + age + 
                              work_place + education + 
                              cadre + govt + 
                         news + international + social + fora 
                         + journals, 
                       data = clean, method = "probit")

summary(full_model_knowledge)
confint(full_model_knowledge)
exp(coef(full_model_knowledge))
exp(confint(full_model_knowledge))


# Summary of the logistic regression model
summary(full_model_knowledge)


# # # # Fitted model
fit_model_knowledge <- polr(knowledge_type ~ fora + att_type + practise_type, 
                            data = clean_OLS, method = "probit")

fit_model_knowledge

fit_model_knowledge1 <- polr(knowledge_type ~ fora, 
                            data = clean_OLS, method = "probit")

fit_model_knowledge1

###FIt of model
brant(fit_model_knowledge)
brant(fit_model_knowledge1)

# Summary of the logistic regression model
summary(fit_model_knowledge)
confint(fit_model_knowledge)
exp(coef(fit_model_knowledge))
exp(confint(fit_model_knowledge))



# Prediction --------------------------------------------------------------

fit_model_knowledge$fitted.values

predict(fit_model_knowledge)

chisq.test(fit_model_knowledge$fitted.values,predict(fit_model_knowledge))

predictOLR <-  predict(fit_model_knowledge,clean)
predict(fit_model_knowledge)



# Create a summary table for the ordinal logistic regression model



# Attitude OLS ------------------------------------------------------------

# Create a null model
null_model_att <- polr(att_type ~ 1, 
                             data = clean, 
                             method = "probit")

summary(null_model_att)


# Create the full logistic regression model
full_model_att <- polr(att_type ~ sex + age + 
                               work_place + education + 
                               cadre + govt + 
                               news + international + social + fora 
                             + journals + knowledge_type + practise_type, 
                             data = clean, method = "probit")

# Summary of the logistic regression model
summary(full_model_att)

(full_model_att_p <- coef(summary(full_model_att)))

p_att_fit_2 <- pnorm(abs(full_model_att_p[, "t value"]),
                   lower.tail = FALSE)*2

(attitude_full_sign <- cbind(full_model_att_p, "p value" = p_att_fit_2))


#Fitted model
fit_model_att <- polr(att_type ~ 
                         govt + knowledge_type + practise_type, 
                       data = clean, method = "probit")

fit_model_att1 <- polr(att_type ~ 
                        govt, 
                      data = clean, method = "probit")

summary(fit_model_att1)

brant(fit_model_att)
brant(fit_model_att1)

# Summary of the logistic regression model
summary(fit_model_att)
confint(fit_model_att)
exp(coef(fit_model_att))
exp(confint(fit_model_att))



# Summary of the logistic regression model


(att_table_fit <- coef(summary(fit_model_att)))

p_att_fit <- pnorm(abs(att_table_fit[, "t value"]),
                    lower.tail = FALSE)*2

(att_fit <- cbind(att_table_fit, "p value" = p_att_fit))




# Practise OLS ------------------------------------------------------------

# Create a null model
null_model_prac <- polr(practise_type ~ 1, 
                       data = clean, 
                       method = "cloglog")

summary(null_model_prac)




# Create the full logistic regression model
full_model_prac <- polr(practise_type ~ sex + age + 
                         work_place + education + 
                         cadre + govt + 
                         news + international + social + fora 
                       + journals + knowledge_type + att_type, 
                       data = clean, method = "cloglog")

# Summary of the logistic regression model
summary(full_model_prac)

(prac_table_full <- coef(summary(full_model_prac)))

p_prac_full <- pnorm(abs(prac_table_full[, "t value"]),
                lower.tail = FALSE)*2

(final_prac_full <- cbind(prac_table_full, "p value" = p_prac_full))


# # # # work place, education, cadre, social media, medical fora, knowledge type and attitude type 

# # # # Fitted model
fit_model_prac <- polr(practise_type ~ work_place + education + 
                          cadre + social + fora + knowledge_type + att_type, 
                        data = clean, method = "cloglog")

brant(fit_model_prac)

fit_model_prac2 <- polr(practise_type ~  
                         cadre, 
                       data = clean, method = "cloglog")

summary(fit_model_prac2)

brant(fit_model_prac2)

summary(fit_model_prac2)
confint(fit_model_prac2)
exp(coef(fit_model_prac2))
exp(confint(fit_model_prac2))


# Summary of the logistic regression model
summary(fit_model_prac)
summary(fit_model_prac2)

(prac_table <- coef(summary(fit_model_prac2)))

p_prac <- pnorm(abs(prac_table[, "t value"]),
                lower.tail = FALSE)*2

(final_prac <- cbind(prac_table, "p value" = p_prac))





# Print model output ------------------------------------------------------








# Cronbach's alpha --------------------------------------------------------

# Select the variables for which you want to calculate Cronbach's alpha
variables <- clean[, c("knowledge_q1",
                                       "knowledge_q2",
                                       "knowledge_q3",
                                       "knowledge_q4",
                                       "knowledge_q5",
                                       "knowledge_q6",
                                       "knowledge_q7",
                                       "knowledge_q8",
                                       "knowledge_q9",
                                       "knowledge_q10",
                                       "knowledge_q11",
                                       "att_q1",
                                       "att_q2",
                                       "att_q3", 
                                       "att_q4",
                                       "att_q5",
                                       "att_q6",
                                       "att_q7",
                                       "att_q8",
                                       "att_q9",
                                       "att_q10",
                                       "att_q11",
                                       "att_q12",
                                       "att_q13",
                                       "att_q14",
                                       "att_q15",
                                       "practise_q1",
                                       "practise_q3",
                                       "practise_q4",
                                       "practise_q5",
                                       "practise_q6")]


# Calculate Cronbach's alpha OVERALL
alpha_result <- alpha(variables)

# Print the result
print(alpha_result)



# CRONBACH FOR KAP --------------------------------------------------------

# Load the psych package
library(psych)


# Select knowledge questions
knowledge_subset <- clean[, c("knowledge_q1", "knowledge_q2", "knowledge_q3", "knowledge_q4", 
                               "knowledge_q5", "knowledge_q6", "knowledge_q7", "knowledge_q8", 
                               "knowledge_q9", "knowledge_q10", "knowledge_q11")]

# Calculate Cronbach's alpha for knowledge questions
knowledge_alpha <- alpha(knowledge_subset)

# Select attitude questions
attitude_subset <- clean[, c("att_q1", "att_q2", "att_q3", "att_q4", "att_q5", "att_q6", 
                              "att_q7", "att_q8", "att_q9", "att_q10", "att_q11", "att_q12", 
                              "att_q13", "att_q14", "att_q15")]

# Calculate Cronbach's alpha for attitude questions
attitude_alpha <- alpha(attitude_subset)




# CORRELATION -------------------------------------------------------------
# Correlation -------------------------------------------------------------

library(broom)        
library(lmtest)     
library(parameters)

#### KNowledge and attitude?

hist(clean$total_kscore)
hist(clean$total_att_score)
hist(clean$total_practise_score)


# Linear regression!!!!!!!! -----------------------------------------------

lm_K <- lm(total_kscore ~ total_att_score, data = clean)
summary(lm_K)

corr_KandA <- cor.test(x=clean$total_kscore, y=clean$total_att_score,
                       method = 'spearman',
                       conf.level = TRUE)
corr_KandA

corr_KandA <- as.data.frame(corr_KandA)



# Assuming corr_KandA is a data frame containing the results of your correlation test
# Make sure to store the results of cor.test() appropriately in corr_KandA

# Check the class of corr_KandA and the structure of the data
str(corr_KandA)

# If corr_KandA is not a data frame, convert it into one
# corr_KandA <- as.data.frame(corr_KandA)

# Check if the variable names exist in the data frame and have the correct class

# Now, proceed with your ggplot code
knowledge_attitude <- ggplot(data = clean,   
       mapping = aes(     
         x = total_att_score,                       
         y = total_kscore,         
         color = total_kscore)) +     
  geom_point(
    colour = "#2c92e6",
    shape = "circle",      
    alpha = 3.7,
    position = position_jitter(width = 1, height = 0.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#b01515") +
  theme_minimal() +
  labs(title = "Knowledge Vs. Attitude",
       y = "Knowledge",
       x = "Attitude",
       color = " ") +
  theme(legend.position = "none") + 
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 20))+
  theme(legend.text = element_text(size = 10)) +
  theme(plot.title = element_text(size = 30, face = "bold"))


knowledge_attitude

ggsave(knowledge,filename = "knowledge_edited_size_7.png",
       width=9, height=7)

## Knowledge and practise
lm_P <- lm(total_kscore ~ total_practise_score, data = clean)
summary(lm_P)

corr_KandP <- cor.test(x=clean$total_kscore, y=clean$total_practise_score,
                       method = 'spearman',
                       conf.level = TRUE)
corr_KandP

predict(lm_P, data = total_practise_score, interval = 'confidence')

Ppoints <- augment(lm_P)
ggplot(Ppoints, aes(x = total_kscore)) + 
  geom_point(aes(y = total_practise_score)) + 
  geom_line(aes(y = .fitted), colour = "red")

practice_knowledge <- ggplot(data = Ppoints,   
                   mapping = aes(     
                     x = total_practise_score,                       
                     y = total_kscore,         
                     color = total_kscore))+ 
  scale_y_continuous(expand = (expansion(0)),
                     limits = c(0, 16)) +
  scale_x_continuous(expand = (expansion(0)),
                     limits = c(0, 6)) +
  geom_point(
    colour = "#2c92e6",
    shape = "circle",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#b01515") +
  theme_minimal() +
  labs(title = "Knowledge Vs. Practice",
       y = "Knowledge",
       x = "Practice",
       color = " ") +
  theme(legend.position = " ") +
  theme(text = element_text(size = 30)) +
  theme(axis.title = element_text(size = 20))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 30, face = "bold")) 

practice_knowledge


## Attitude and practise
lm_A <- lm(total_att_score ~ total_practise_score, data = clean)
summary(lm_A)

Apoints <- augment(lm_A)

corr_AandP <- cor.test(x=clean$total_att_score, y=clean$total_practise_score,
                       method = 'spearman',
                       conf.level = TRUE)
corr_AandP

attitude_practice <- ggplot(data = clean,   
                   mapping = aes(     
                     x = total_practise_score,                       
                     y = total_att_score,         
                     color = total_att_score))+ 
  scale_x_continuous(expand = (expansion(0)),
                     limits = c(0, 6))+
  geom_point(
    colour = "#2c92e6",
    shape = "circle",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(
    method = "lm", 
    size = 2.5,
    color = "#b01515") +
  theme_minimal() +
  labs(title = "Attitude Vs. Practice",
       y = "Attitude",
       x = "Practice",
       color = " ") +
  theme(legend.position = " ")+
  theme(text = element_text(size = 30)) +
  theme(axis.title = element_text(size = 20))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 30, face = "bold"))

attitude_practice


paned_image_KAP <- ggarrange(knowledge_attitude, 
                         practice_knowledge,
                         attitude_practice,
                         labels = c("A", "B", "C"),
                         font.label = 
                           list(size = 35,
                                colour = "black",
                                face = "bold"),
                         ncol = 1, nrow = 3)

ggsave(paned_image_KAP, filename = "KAP_Linear_Regression.png",
       width=12, height=25, dpi = 300)


# Linear regression with information sources and knowledge  ---------------

final$all_sources_information <- as.numeric(final$all_sources_information)


# Column graph for symptoms -----------------------------------------------

data <- data.frame(Percentage = c(91.9, 84.2, 66.0, 50.9, 46.6,
                                  44.1, 37.8, 31.3, 8.3, 6.3, 5.4),
                   Symptoms = c("Fever", "Cough", "Sore throat", "Sneezing",
                                "Anosmia", "Diarrhoea", "Rhinorrhea",
                                "Confusion","Hair loss", "Headache", "Myalgia"))

# Define the order of symptoms from largest to smallest
symptom_order <- c(
  "Fever", "Cough", "Sore throat", "Sneezing",
  "Anosmia", "Diarrhoea", "Rhinorrhea",
  "Confusion", "Hair loss", "Headache", "Myalgia"
)

# Convert Symptoms to a factor with specific levels
data$Symptoms <- factor(data$Symptoms, levels = symptom_order)


symptoms <- ggplot(data, 
                   aes(x = Symptoms, y = Percentage, 
                       fill = Symptoms)) +
  geom_bar(stat = "Identity", size = 0.5) +
  scale_fill_brewer() +
  geom_text(aes(label = paste0("%", Percentage)), 
            vjust = -0.5, size = 7) +
  theme_classic() +
  geom_col(width = .9, position = position_dodge(.3),
           color = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 100)) +
  labs(
    x = "Symptoms",
    y = "Percentage",
    title = "Main clinical symptoms of COVID-19") +
  theme(plot.title = element_text(size = 30, face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 15)),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 25, color = "black",
                                  face = "bold"),
        axis.text = element_text(size = 22, color = "black"),
        axis.text.x = element_text(margin = margin(t = 18)),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(margin = margin(r = 22)),
        axis.ticks.x = element_blank(),
        legend.position = "none")

symptoms

ggsave(symptoms, filename = "symptoms_post-reviw.png",
       width=25, height=12, dpi = 300)



# Knowledge levels --------------------------------------------------------


data_k <- data.frame(
  Percentage = c(43.0, 23.0, 34.0),
  Levels = c("Good", "Medium", "Poor"),
  SampleSize = c(181, 97, 143)
)

knowledge_graph <- ggplot(data_k, aes(x = Levels, y = Percentage, fill = Levels)) +
  geom_bar(stat = "identity", size = 0.5) +
  geom_text(aes(label = paste0("n = ", SampleSize)), 
            vjust = -0.5, size = 10) + # Add this line for sample size labels
  scale_fill_brewer() +
  theme_classic() +
  geom_col(width = .9, position = position_dodge(.3), color = "black") +
  scale_y_continuous(expand = expansion(0), limits = c(0, 100)) +
  labs(
    x = " ",
    y = "Percentage",
    title = "Knowledge"
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 25, color = "black", face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 18)),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(margin = margin(r = 22)),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

print(knowledge_graph)



# Attitude graphs ---------------------------------------------------------

data_att <- data.frame(
  Percentage = c(17.5, 35.6, 46.9),
  Levels = c("Good", "Medium", "Poor"),
  SampleSize = c(56, 114, 150)
)

att_graph <- ggplot(data_att, aes(x = Levels, y = Percentage, fill = Levels)) +
  geom_bar(stat = "identity", size = 0.5) +
  geom_text(aes(label = paste0("n = ", SampleSize)), 
            vjust = -0.5, size = 10) + # Add this line for sample size labels
  scale_fill_brewer() +
  theme_classic() +
  geom_col(width = .9, position = position_dodge(.3), color = "black") +
  scale_y_continuous(expand = expansion(0), limits = c(0, 100)) +
  labs(
    x = " ",
    y = " ",
    title = "Attitude"
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 25, color = "black", face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 18)),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(margin = margin(r = 22)),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )


print(att_graph)



# Practises type ----------------------------------------------------------

data_prac <- data.frame(
  Percentage = c(68.4, 15.7, 15.9),
  Levels = c("Good", "Medium", "Poor"),
  SampleSize = c(279, 64, 65)
)

prac_graph <- ggplot(data_prac, aes(x = Levels, y = Percentage, fill = Levels)) +
  geom_bar(stat = "identity", size = 0.5) +
  geom_text(aes(label = paste0("n = ", SampleSize)), 
            vjust = -0.5, size = 10) + # Add this line for sample size labels
  scale_fill_brewer() +
  theme_classic() +
  geom_col(width = .9, position = position_dodge(.3), color = "black") +
  scale_y_continuous(expand = expansion(0), limits = c(0, 100)) +
  labs(
    x = " ",
    y = " ",
    title = "Practice"
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 25, color = "black", face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 18)),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(margin = margin(r = 22)),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

print(prac_graph)




# Pie chart ---------------------------------------------------------------

# Data

# Define labels, percentages, and counts
#labels_prac <- c("Yes", "Maybe", "No")
#percentages_prac <- c(69.5, 21.1, 9.3) # Percentages
#counts_prac <- c(306, 41, 93) # Corresponding counts

# Create a data frame
#data_prac <- data.frame(labels_prac, percentages_prac, counts_prac)

# Create a pie chart using ggplot2
#pie_chart_prac <- ggplot(data_prac, aes(x = "", y = percentages_prac, fill = labels_prac)) +
#  geom_bar(width = 1, stat = "identity") +
#  coord_polar("y") +
#  scale_fill_brewer()+
#  theme_void() +
#  labs(title = "Vaccine Uptake Willingness") +
#  geom_text(aes(label = paste0(percentages_prac, "% (n = ", counts_prac, ")")), position = position_stack(vjust = 0.5)) +
#  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
#        panel.background = element_rect(fill = "white"),
#        plot.margin = margin(0, 0, 0, 0),  # Add margins around the pie chart
#        panel.grid = element_blank()) +
#  guides(fill = "none")   # Remove the margin

# Display the pie chart
#pie_chart_prac

  
   
# Print the pie chart
#print(pie_chart_prac)


#pie_chart_prac


# Paned images KAP --------------------------------------------------------

paned_image_kap <- ggarrange( 
                             knowledge_graph,
                         att_graph,
                         prac_graph,
                      
                         labels = c("A", "B", "C"),
                         font.label = 
                           list(size = 35,
                                colour = "black",
                                face = "bold"),
                         ncol = 3, nrow = 1)

ggsave(paned_image_kap, filename = "kap_paned_all.png",
       width=25, height=10, dpi = 300)

