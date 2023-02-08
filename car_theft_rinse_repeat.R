library(tidyverse)

library(readr)
car_theft_charge_statewide_010121_103122 <- read_csv("car_theft_charge_statewide_010121_103122.csv")
View(car_theft_charge_statewide_010121_103122)

charge <- car_theft_charge_statewide_010121_103122

library(readr)
car_theft_sentence_statewide_010121_103122 <- read_csv("car_theft_sentence_statewide_010121_103122.csv")
View(car_theft_sentence_statewide_010121_103122)

sentence <- car_theft_sentence_statewide_010121_103122

# How many unique cases in charge and sentence data? 

    # Charge

      charge %>% 
       distinct(case_number, .keep_all = TRUE)

       # In a year and a half of data, 10,373 cases
        # Confirmed via SQL. 10,373 cases.
          # SELECT DISTINCT(case_number)
          # FROM charge
    
    # Sentence
      
      sentence %>% 
        distinct(case_number, .keep_all = TRUE)

         # 4,198 cases with a sentence. 
          # Confirmed via SQL, 4,198 cases with a sentence
            # SELECT DISTINCT(case_number)
            # FROM sentence
            
      
  # Let's double check to make sure we're just looking at car theft cases and no other riff raff got in here. 
      
      charge %>% 
      group_by(law_description) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count)) %>% 
      View()
 
      # No riff raff, but there are a bunch of cases with "priors," which is hella interesting. 
      
      
################### unique DOB ########################
      
    # There is a date of birth column. Curious how many unique DOB's there are
      
      # distinct DOB
          
          uniq_charge <- charge %>% 
            distinct(case_number, .keep_all = TRUE)
      
              # Export
        
              uniq_charge %>% write_csv("uniq_charge.csv", na = "")
            
          
          uniq_charge %>% 
          distinct(date_of_birth_day, .keep_all = TRUE)
          
            # 5,541 distinct DOB's 
              # Confirmed in SQL, 5,541 rows
                # SELECT DISTINCT(date_of_birth_day)
                # FROM uniq_charge
          
          
      # count of dob's within the unique (aka a prolific dude)
      
          uniq_charge %>% 
            group_by(date_of_birth_day) %>% 
            summarize(count = n()) %>% 
            arrange(desc(count)) %>% 
            View()
      
        # Hundreds of repeat DOB's. 
          # Here are the top 10:
            # date_of_birth_day    count
              # 08/22/90             13
              # 02/27/95             12
              # 04/25/91             11
              # 09/21/98             11
              # 02/19/83             10
              # 06/04/98             10
              # 06/09/94             10
              # 06/23/99             10
              # 06/27/86             10
              # 01/23/98              9
          
      # Controlling for DOB, race and gender
          
          uniq_charge %>% 
            group_by(date_of_birth_day, gender, race) %>% 
            summarize(count = n()) %>% 
            arrange(desc(count)) %>% 
            View()
          
          # Couple of folks with at least 10 repeats
            # Verified by SQL, 6815 rows:
                # CREATE TABLE uniq_charge_demo AS
                # SELECT DISTINCT(case_number), date_of_birth_day, race, gender
                # FROM charge
          
                # SELECT date_of_birth_day, gender, race, COUNT(date_of_birth_day) AS "count"
                # FROM uniq_charge_demo
                # GROUP BY date_of_birth_day, gender, race
                # ORDER BY COUNT(date_of_birth_day) DESC
          
          library(tidyverse)
          
          uniq_charge %>% 
            group_by(date_of_birth_day, gender, race) %>% 
            summarize(case_number_count = n()) %>% 
            View()
          
         repeat_dob_race_gender <- uniq_charge %>% 
            group_by(date_of_birth_day, gender, race) %>% 
            summarize(case_number_count = n()) 
         
         # Export:        
         
          repeat_dob_race_gender %>% write_csv("repeat_dob_race_gender.csv", na = "")
         
          
          repeat_dob_race_gender %>% 
            filter(case_number_count >= 2) %>% 
            View()
          
          # 2,072 people with the same DOB, gender and race with at least two case numbers.    
             # Did a manual check and this is verified too. 
          
             # Also verified in SQL, 2,072 rows
                
                  # CREATE TABLE uniq_charge_demo AS
                  # SELECT DISTINCT(case_number), date_of_birth_day, race, gender
                  # FROM charge
                  
                  # CREATE TABLE "repeat_dob_race_gender" AS
                  # SELECT date_of_birth_day, gender, race, COUNT(date_of_birth_day) AS "case_number_count"
                  # FROM uniq_charge_demo
                  # GROUP BY date_of_birth_day, gender, race
                  # ORDER BY COUNT(date_of_birth_day) DESC
                  
                  # SELECT *
                  #   FROM repeat_dob_race_gender
                  # WHERE case_number_count >= 2
          
           repeat_dob_race_gender %>% 
            filter(case_number_count >= 2) %>% 
          arrange(desc(case_number_count))
          
           # Percent?
           
           ((2072/10373)*100)
           
            # 19.97%
           
         
############# priors ################      
           
    # That would insinuate that there is a universe of charges with repeat car theft folks.
      
        charge %>% 
          filter(grepl('priors',law_description))
      
        dplyr::filter(charge, grepl('priors', law_description))
        
        grepl("priors",charge$law_description)
      
        charge %>% 
          filter(str_detect(law_description, "$prior$"))
        
        # Because grepl has completely shit itself in such a thorough, nonsensical and infuriating way, I pulled it out in SQL to check for the law numbers
        
          # CREATE TABLE "priors" AS
          # SELECT *
          # from charge
          # where law_description like "%PRIORS%"
        
          # SELECT law_number, count(law_number)
          # FROM priors
          # GROUP BY law_number
          
       # After all of those shenanigans, there are two law numbers used for accusations of previously stealing cars:
            # 18-4-409(2),(3)(b)
              # Fudge me. This can't be used just by the numbers because this law number has two uses. 
                # https://law.justia.com/codes/colorado/2016/title-18/article-4/part-4/section-18-4-409
                # "(b) Class 3 felony if the value of the motor vehicle or motor vehicles involved is more than one hundred thousand dollars or if the defendant has twice previously been convicted or adjudicated of charges separately brought and tried either in this state or elsewhere of an offense involving theft of a motor vehicle under the laws of this state, any other state, the United States, or any territory subject to the jurisdiction of the United States."
            # 18-4-409(4)                         
        
       # Pulled out just the law descriptions. I'll filter from these just to sure. 
            # SQL code: 
              # SELECT law_description, count(law_description)
              # FROM priors
              # GROUP BY law_description
      
       # The results:
        # MOTOR VEHICLE THEFT/AGG 2-W/2 PRIORS-ATT
        # MOTOR VEHICLE THEFT/AGG-W/TWO PRIORS    
        # MOTOR VEHICLE THEFT/AGG1-W/2 PRIORS-ATT 
        # MOTOR VEHICLE THEFT/AGG1-W/2 PRIORS-CSP 
        
        
       # No that's sorted, I am going to pull them out. 
        
          prior_charge_pt1 <- charge %>% 
            filter(law_description == "MOTOR VEHICLE THEFT/AGG 2-W/2 PRIORS-ATT") 
          
          prior_charge_pt2 <- charge %>% 
            filter(law_description == "MOTOR VEHICLE THEFT/AGG-W/TWO PRIORS")
          
          prior_charge_pt3 <- charge %>% 
            filter(law_description == "MOTOR VEHICLE THEFT/AGG1-W/2 PRIORS-ATT")
          
          prior_charge_pt4 <- charge %>% 
            filter(law_description == "MOTOR VEHICLE THEFT/AGG1-W/2 PRIORS-CSP")
          
      
        # And let's put them together  
        
          prior_charge_pt1_2 <- prior_charge_pt1 %>% full_join(prior_charge_pt2)
          
          prior_charge_pt1_3 <- prior_charge_pt3 %>% full_join(prior_charge_pt1_2)
          
          prior_charge_pt1_4 <- prior_charge_pt4 %>% full_join(prior_charge_pt1_3)
          
        # And how many distinct?
        
          prior_charge_pt1_4 %>% 
            distinct(case_number, .keep_all = TRUE) %>% 
            View()
          
          # 502 cases were people were charged with stealing a car with two priors. 
            # Down from 704 to 502 cases.
              # Looks like there were a bunch of instances where please were entered and then withdrawn. 
              # Ex: D0012021CR002906
          
          # Confirmed by SQL, 502 distinct cases
            # SELECT DISTINCT(case_number)
            # FROM priors
          
        distinct_priors_charge <- prior_charge_pt1_4 %>% 
            distinct(case_number, .keep_all = TRUE)
        
        # Export:
        
        distinct_priors_charge %>% write_csv("distinct_priors_charge.csv", na = "")
          
    # How many of those charged were convicted of being convicted again of car theft after priors? 
      # "twice previously been convicted or adjudicated of charges separately brought and tried either in this state or elsewhere of an offense involving theft of a motor vehicle under the laws of this state"
        
        inner_join(distinct_priors_charge, sentence, by = "case_number") %>% 
          View()
        
        # this data has all of the convictions, there were likely convicted among a series of charges, so need to filter out for just priors
      
            prior_sentence_pt1 <- sentence %>% 
              filter(law_description == "MOTOR VEHICLE THEFT/AGG 2-W/2 PRIORS-ATT") 
            
            prior_sentence_pt2 <- sentence %>% 
              filter(law_description == "MOTOR VEHICLE THEFT/AGG-W/TWO PRIORS")
            
            prior_sentence_pt3 <- sentence %>% 
              filter(law_description == "MOTOR VEHICLE THEFT/AGG1-W/2 PRIORS-ATT")
            
            prior_sentence_pt4 <- sentence %>% 
              filter(law_description == "MOTOR VEHICLE THEFT/AGG1-W/2 PRIORS-CSP")
            
            
            # And let's put them together  
            
            prior_sentence_pt1_2 <- prior_sentence_pt1 %>% full_join(prior_sentence_pt2)
            
            prior_sentence_pt1_3 <- prior_sentence_pt3 %>% full_join(prior_sentence_pt1_2)
            
            prior_sentence_pt1_4 <- prior_sentence_pt4 %>% full_join(prior_sentence_pt1_3)
        
      # Distinct cases
        
            prior_sentence_pt1_4 %>% 
              distinct(case_number, .keep_all = TRUE)
        
            # 43 cases where someone was sentenced to stealing a car again after 2 prior convictions
              # Verified by SQL, 43 cases
                # SELECT DISTINCT(case_number)
                # FROM priors_sentence
              
            distinct_priors_sentenced <- prior_sentence_pt1_4 %>% 
              distinct(case_number, .keep_all = TRUE)
            
            
            # Export:
            
            distinct_priors_sentenced %>% write_csv("distinct_priors_sentenced.csv", na = "")
            
            
       # distinct case joined together, should get 43 hits 
        
         inner_join(distinct_priors_charge, distinct_priors_sentenced, by = "case_number") %>% 
          distinct(case_number, .keep_all = TRUE) %>% 
          View()
        
        # And 43 hits indeed
      
########################### Misdemeanor cases ##############################
# According to C.R.S., 2nd degree aggravated motor vehicle theft can be charged as a class 1 misdemeanor if the value of the vehicle is less than $2,000 and there are no aggravating factors.  
    # Group by law class            
         
         charge %>% 
         group_by(law_class) %>% 
           summarize(count = n()) %>% 
           arrange(desc(count))

        # Looks like there are two misdemeanor classes: M1 and M2
         
         m1 <- charge %>% 
           filter(law_class == "M1") 
         
         m2 <- charge %>% 
           filter(law_class == "M2")  
         
         # Join
         
             misdemeanor_1_2 <- m1 %>% full_join(m2)
         
    # What law numbers come up in this list? I think it should just be 18-4-409(4)(c) but I want to make sure I am not missing anything
             
             misdemeanor_1_2 %>% 
               group_by(law_number) %>% 
               summarize(count = n()) %>% 
               arrange(desc(count))
                  
          # Yup - just 18-4-409(4)(c)
             
             charge %>% 
               filter(law_number == "18-4-409(4)(c)")
             
              # same number of rows -- 2104
         
  # Some rows have "ATT" (or attempt) in the law description and Vaughan does not want those in there
     # if NA grepl: https://community.rstudio.com/t/filter-function-and-subset-of-variables-that-do-not-contain-a-certain-string-of-characters/94901/2
             
     misdemeanor_1_2 %>% 
     filter(!(grepl('ATT', law_description))) %>% 
       View()
     
     # Worked! 2,034 rows. let's check in SQL
     
     misdemeanor_1_2 <- misdemeanor_1_2 %>% 
       filter(!(grepl('ATT', law_description)))
     
  # Distinct cases: 
     
     misdemeanor_1_2 %>% 
       distinct(case_number, .keep_all = TRUE)
     
     # 1,975 unique cases
      # Verified in SQL, 1,975 cases.
     
         # CREATE TABLE "misdemeanor_1_2" AS
         # SELECT *
         #  FROM charge
         # where trim(law_class) = "M1"
         # or trim(law_class) = "M2"
         
         # CREATE TABLE "filtered_misdemeanor_1_2" AS
         # SELECT *
         #  FROM misdemeanor_1_2
         # WHERE law_description NOT LIKE "%ATT%"
         
         # SELECT DISTINCT(case_number)
         # FROM filtered_misdemeanor_1_2
     
# How many people were sentenced for misdemeanors?
     
     sentence_m1 <- sentence %>% 
       filter(law_class == "M1") 
     
     sentence_m2 <- sentence %>% 
       filter(law_class == "M2")  
     
     # Join
     
         sentence_misdemeanor_1_2 <- sentence_m1 %>% full_join(sentence_m2)

  # Remove attempted car theft misdemeanors        
     
     sentence_misdemeanor_1_2 %>% 
       filter(!(grepl('ATT', law_description))) %>% 
       View()
     
     # Worked! Remember - so many more rows because there is a row for each part of the sentence and each case likely has different lines for parts of the sentence
     
     sentence_misdemeanor_1_2 <- sentence_misdemeanor_1_2 %>% 
       filter(!(grepl('ATT', law_description)))
     
          # Same in SQL, 16,453 rows
     
             # CREATE TABLE "sentence_1_2" AS
             # SELECT *
             #  FROM sentence
             # where trim(law_class) = "M1"
             # or trim(law_class) = "M2"
             
             # CREATE TABLE "filtered_sentence_1_2" AS
             # SELECT *
             #  FROM sentence_1_2
             # WHERE law_description NOT LIKE "%ATT%"
     
     # Distinct cases: 
     
     sentence_misdemeanor_1_2 %>% 
       distinct(case_number, .keep_all = TRUE)
     
     # 1,678 unique cases with a sentence
      # Verified in SQL, 1,678 rows
        # SELECT DISTINCT(case_number)
        # FROM filtered_sentence_1_2
     
      # To double check I will do an inner join. All charged case numbers should be sentenced cases numbers too     
     
     distinct_misdemeanor_1_2 <- misdemeanor_1_2 %>% 
       distinct(case_number, .keep_all = TRUE)
     
     distinct_sentence_misdemeanor_1_2 <- sentence_misdemeanor_1_2 %>% 
       distinct(case_number, .keep_all = TRUE)
     
     inner_join(distinct_misdemeanor_1_2, distinct_sentence_misdemeanor_1_2, by = "case_number") %>% 
       View()
     
      # 1,678 cases with misdemeanor charges have misdemeanor sentences, which is good because that is what it should be in the data structure. Shouldn't drop any cases
     
     ((1678/1975)*100)
     
     # So 85% of misdemeanor car theft cases are sentenced. 
     
#### How many felony cases car theft cases pled down to a misdemeanor?  ####
  
  # Are there other law sentences in the data outside of car theft?
     
     sentence %>% 
       group_by(law_description) %>% 
       summarize(count = n()) %>% 
       arrange(desc(count)) %>% 
       View()
     
     # No, all car thefts 
     
  # Ok, how many felony types are in here?
  
   # Charge:
     
     charge %>% 
       group_by(law_class) %>% 
       summarize(count = n()) %>% 
       arrange(desc(count)) 
     
     # Mostly charged with F5 and F6 felonies
       
   # Sentence:
      
     sentence %>% 
       group_by(law_class) %>% 
       summarize(count = n()) %>% 
       arrange(desc(count))
     
           # Mostly M1 and F6
     
  # Are there cases who are sentenced to both a misdemeanor and felony?
     # charged with felony:
     
         f5 <- charge %>% 
           filter(law_class == "F5") 
         
         f4 <- charge %>% 
           filter(law_class == "F4")
                  
          f6 <- charge %>% 
         filter(law_class == "F6") 
                  
          f3 <- charge %>% 
          filter(law_class == "F3")         
        
     
   # Join
      
      felony_3_4 <- f4 %>% full_join(f3)  
     
      felony_3_5 <- f5 %>% full_join(felony_3_4)  
      
      felony_3_6 <- f6 %>% full_join(felony_3_5)  
      
        # Verified in SQL, 13,589 rows
      
          # CREATE TABLE "charge_felony_3_6" AS
          # SELECT *
          #  FROM charge
          # where trim(law_class) = "F3"
            # or trim(law_class) = "F4"
            # or trim(law_class) = "F5"
            # or trim(law_class) = "F6"
      
      
      charge_felony_3_6 <- felony_3_6
      
  # Filter out attempted crimes
      
      charge_felony_3_6 <- charge_felony_3_6 %>% 
        filter(!(grepl('ATT', law_description)))
     
      # pulled out a couple hundred (?) rows
        
  # Sentenced with felony:
      
          sen_f5 <- sentence %>% 
            filter(law_class == "F5") 
          
          sen_f4 <- sentence %>% 
            filter(law_class == "F4")
          
          sen_f6 <- sentence %>% 
            filter(law_class == "F6") 
          
          sen_f3 <- sentence %>% 
            filter(law_class == "F3")         
      
      
      # Join
      
          sentence_felony_3_4 <- sen_f4 %>% full_join(sen_f3)  
          
          sentence_felony_3_5 <- sen_f5 %>% full_join(sentence_felony_3_4)  
          
          sentence_felony_3_6 <- sen_f6 %>% full_join(sentence_felony_3_5)  
          
    
 # Filter out attempted crimes
          
          sentence_felony_3_6 <- sentence_felony_3_6 %>% 
            filter(!(grepl('ATT', law_description)))
      
# Distinct felony charge cases:
          
          charge_felony_3_6 %>% 
          distinct(case_number, .keep_all = TRUE)
          
          # 9,540 distinct rows with a felony charge  
          
            # Verified in SQL, 9,540 rows
          
              # SELECT DISTINCT(case_number)
              # FROM filtered_charge_felony_3_6
              
 # Inner join felony charge on felony sentence
      
          inner_join(sentence_felony_3_6, charge_felony_3_6, by = "case_number") %>% 
            distinct(case_number, .keep_all = TRUE) %>% 
            View()    
        
            # 2,383 distinct cases that were charged with felony car theft were sentenced to it
            
              inner_join(charge_felony_3_6, sentence_felony_3_6, by = "case_number") %>% 
                distinct(case_number, .keep_all = TRUE) %>% 
                View()  
          
              # Flipping them doesn't make a difference
              
              # SQL shows that there was also 2,383 distinct cases with a sentence
              
                 # CREATE TABLE "sentence_felony_3_6" AS
                 # SELECT *
                 #   FROM sentence
                 # where trim(law_class) = "F3"
                 # or trim(law_class) = "F4"
                 # or trim(law_class) = "F5"
                 # or trim(law_class) = "F6"
                  
                 # CREATE TABLE "filtered_sentence_felony_3_6" AS
                 # SELECT *
                 #   FROM sentence_felony_3_6
                 # WHERE law_description NOT LIKE "%ATT%"
                  
                 # SELECT DISTINCT(case_number)
                 # FROM filtered_sentence_felony_3_6
              
              # 9,540 distinct rows with a felony charge  
                
                ((2383/9540)*100)
              
                  # 25% (or 1 in 4) were charged and sentenced to a felony car theft charge
              
                  # Double check:
              
              inner_join(charge_felony_3_6, sentence_felony_3_6, by = "case_number") %>% 
                distinct(case_number, .keep_all = TRUE)      
              
                # Yup, 2,383 rows
              
          
 #  Number of felony charged that ended up with a misdemeanor sentence?       
          
              inner_join(charge_felony_3_6, sentence_misdemeanor_1_2, by = "case_number") %>% 
                distinct(case_number, .keep_all = TRUE)      
              
              # 1,382 cases where they were charged with a felony and sentenced to a misdemeanor
              
                # Verified by SQL, 1,382 rows:
              
                    # SELECT distinct(filtered_charge_felony_3_6.case_number) 
                    # FROM filtered_charge_felony_3_6, filtered_sentence_1_2
                    # WHERE filtered_charge_felony_3_6.case_number = filtered_sentence_1_2.case_number
                
                # 9,540 distinct rows with a felony charge  
              
                  ((1382/9540)*100)
              
                    # 14%
              
              charge_felony_sentence_misdemeanor <- inner_join(charge_felony_3_6, sentence_misdemeanor_1_2, by = "case_number") %>% 
                distinct(case_number, .keep_all = TRUE) 
              
          # How many sentenced to misdemeanor also sentenced to felony?    
              
              inner_join(sentence_felony_3_6, charge_felony_sentence_misdemeanor, by = "case_number") %>% 
                distinct(case_number, .keep_all = TRUE) %>% 
                View()    
              
              # 29 cases where cases were sentenced to misdemeanor and sentenced to felony
              
                  # Same as in SQL, 29 cases:
                    # SELECT distinct(charge_felony_sentence_misdemeanor.case_number) 
                    # FROM charge_felony_sentence_misdemeanor, filtered_sentence_felony_3_6
                    # WHERE charge_felony_sentence_misdemeanor.case_number = filtered_sentence_felony_3_6.case_number
                  
              
              