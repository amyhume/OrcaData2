library(redcapAPI)

url <- 'https://redcap.nyu.edu/api/'

#' @title Pulling ORCA Data
#' @description This function retrieves data from a REDCap project using the API.
#' @param token The API token for the project
#' @param form The name of the REDCap form to retrieve data from
#' @param raw_v_label The label for raw data fields
#' @param form_complete Indicating whether you want to return all responses or just ones marked as complete (default is all)
#' @return A data frame with the retrieved data
#' @export
get_orca_data <- function(token = token, form = form, raw_v_label = 'raw', form_complete = T) {
  if (form_complete) {
    record_filter = paste("[", form, "_complete]=2", sep = "")
  } else {
    record_filter = paste("[", form, '_complete] <> ""', sep = "")
  }
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id', 
                   'forms[0]'=form,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='true',
                   exportDataAccessGroups='false',
                   returnFormat='csv',
                   filterLogic=record_filter)
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  if (nrow(df) >= 1) {
    df <- dplyr::filter(df, !stringr::str_detect(record_id, "TEST"))
    df <- dplyr::filter(df, !stringr::str_detect(record_id, "test"))
    df <- dplyr::filter(df, !stringr::str_detect(record_id, "IRB"))
    df <- dplyr::filter(df, !stringr::str_detect(record_id, "D"))
    df <- dplyr::filter(df, record_id != '496' & record_id != '497' & record_id != '498' & record_id != '499')
  } else {
    print('no data returned - may need to set form_complete = F')
  }
  return (df)
}

#' @title Pulling ORCA Field
#' @description PULLS RECORD ID AND INDIVIDUAL FIELD
#' @param token Unique REDCap token ID
#' @param field Name of the specific field to be downloaded
#' @param raw_v_label Whether raw data or labels are requested
#' @return A data frame for the completed record_ids, redcap_event_name and field
#' @export
get_orca_field <- function(token = token, field=field, raw_v_label = 'raw') {
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id',
                   'fields[1]' = field,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='csv',
                   filterLogic=paste("[", field, '] <> ""', sep = ""))
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA
  df <- dplyr::select(df, record_id, redcap_event_name, all_of(field))
  df <- df[!is.na(df[[field]]),]
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "TEST"))
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "test"))
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "IRB"))
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "D"))
  df <- dplyr::filter(df, record_id != '496' & record_id != '497' & record_id != '498' & record_id != '499')
  return (df)
}

#' @title Replacing Multiple Values Function
#' @description ALLOWS DATA TO BE REPLACED IN A COLUMN
#' @param data The dataframe
#' @param column_name The column you wish to work on
#' @param old_values Vector of string variables you wish to replace
#' @param new_values Vector of string variables you wish to replace the old ones with 
#' @return the data frame with old strings in specified column replaced with new strings
#' @export
#' 
replace_multiple <- function(data, column_name, old_values, new_values) {
  data[[column_name]] <- new_values[match(data[[column_name]], old_values)]
  return(data)
}

#' @title Cleaning screener race responses
#' @description This function cleans race responses 
#' @param data data frame to act upon
#' @return a dataframe with the race responses cleaned
#' @export
clean_race <- function(data) {
  data$rec_race___1 <- gsub("1", "American Indian/Alaska Native", data$rec_race___1)
  data$rec_race___2 <- gsub("1", "Asian", data$rec_race___2)
  data$rec_race___3 <- gsub("1", "Black/African American/African", data$rec_race___3)
  data$rec_race___4 <- gsub("1", "Hispanic/Latino", data$rec_race___4)
  data$rec_race___5 <- gsub("1", "Middle Eastern/North African", data$rec_race___5)
  data$rec_race___6 <- gsub("1", "Native Hawaiian", data$rec_race___6)
  data$rec_race___7 <- gsub("1", "White", data$rec_race___7)
  data$rec_race___8 <- gsub("1", "None fully describe", data$rec_race___8)
  data$rec_race___9 <- gsub("1", "Prefer not to answer", data$rec_race___9)
  data <- unite(data, "race", rec_race___1:rec_race___9)
  data$race <- gsub("0", "", data$race)
  data$race <- gsub("_", "", data$race)
  return (data)
}

#' @title ZIP CODE AND LOCATION FUNCTION
#' @description This function pulls public US zipcode data and cleans it
#' @return a dataframe with the race responses cleaned
#' @export
zips <- function() {
  zipcodeR::download_zip_data(force=T)
  zip_info <- select(zipcodeR::zip_code_db, zipcode, major_city, state, timezone)
  zip_info$zipcode <- as.integer(zip_info$zipcode)
  zip_info <- mutate(zip_info,
                     final_city = ifelse(zipcode >= 10001 & zipcode <= 10282, "New York",
                                         ifelse(zipcode >= 10301 & zipcode <= 10314, "New York",
                                                ifelse(zipcode >= 10451 & zipcode <= 10475, "New York",
                                                       ifelse(zipcode >= 11004 & zipcode <= 11109, "New York",
                                                              ifelse(zipcode >= 11351 & zipcode <= 11697, "New York",
                                                                     ifelse(zipcode >= 11201 & zipcode <= 11256, "New York", major_city)))))))
  zip_info$location <- str_c(zip_info$final_city, " ", zip_info$state)
  zip_info <- select(zip_info, zipcode, location, timezone)
  zip_info <- data.frame(zip_info)
  return(zip_info)
}


#' @title Pulling City Data
#' @description this function pulls us city population data
#' @param data data frame to act upon
#' @return a dataframe with the race responses cleaned
#' @export
city_info <- function() {
  cities <- maps::us.cities
  cities <- select(cities, name, pop)
  cities <- rename(cities, location = name, population=pop)
  cities <- data.frame(cities)
  return(cities)
}

#' @title Calculate Rurality
#' @description this function returns rural/non rural status for given zipcodes
#' @param zip_code vector of zipcodes to act on (must be 5 digits)
#' @param population_threshold numeric value for population cut off for rural/non rural
#' @return a vector of rural/non rural/na statuses
#' @export
calculate_rurality <- function(zip_code, population_threshold=50000) {
  zip_info <- zips()
  cities <- city_info()
  
  zip_code <- data.frame(zipcode = zip_code)
  zip_code$zipcode <- as.character(zip_code$zipcode)
  zip_info$zipcode <- sprintf("%05d", zip_info$zipcode)
  zip_info$zipcode <- as.character(zip_info$zipcode)
  
  data <- left_join(zip_code, zip_info, by="zipcode")
  data <- left_join(data, cities, by='location')
  
  data <- mutate(data,
                 rural = ifelse(is.na(population) & !is.na(location), "Y",
                                ifelse(population >= population_threshold, "N",
                                       ifelse(population < population_threshold, "Y", "CHECK"))))
  
  rural <- data$rural
  
  return(rural)
}

#' @title Calculate Timezone
#' @description this function returns timezones for given zipcodes
#' @param zip_code vector of zipcodes to act on (must be 5 digits)
#' @return a vector of timezones for zipcodes
#' @export
calculate_timezone <- function(zip_code) {
  zip_info <- zips()
  cities <- city_info()
  
  zip_code <- data.frame(zipcode = zip_code)
  zip_code$zipcode <- as.character(zip_code$zipcode)
  zip_info$zipcode <- sprintf("%05d", zip_info$zipcode)
  zip_info$zipcode <- as.character(zip_info$zipcode)
  
  data <- left_join(zip_code, zip_info, by="zipcode")
  
  tz = data$timezone
  
  return(tz)
}

#' @title Calculate all zipcode info
#' @description this function pulls location, timezone, population size and rurality for given zipcodes
#' @param zip_code vector of zipcodes to act on (must be 5 digits)
#' @param population_threshold numeric value for population cut off for rural/non rural
#' @return a dataframe with zipcode, location, timezone, population size and rurality
#' @export
get_zip_info <- function(zip_code, population_threshold=50000) {
  zip_info <- zips()
  cities <- city_info()
  
  zip_code <- data.frame(zipcode = zip_code)
  zip_code$zipcode <- as.character(zip_code$zipcode)
  zip_info$zipcode <- sprintf("%05d", zip_info$zipcode)
  zip_info$zipcode <- as.character(zip_info$zipcode)
  
  data <- left_join(zip_code, zip_info, by="zipcode")
  data <- left_join(data, cities, by='location')
  
  data <- mutate(data,
                 rural = ifelse(is.na(population) & !is.na(location), "Y",
                                ifelse(population >= 50000, "N",
                                       ifelse(population < 50000, "Y", "CHECK"))))
  
  return(data)
}

#' @title Pulling cleaned screener responses 
#' @description this function pulls and cleans screener data for import to excel tracker 
#' @param token Unique REDCap API token
#' @param min_date_time The minimum timestamp to pull responses after
#' @return a dataframe with the race responses cleaned
#' @export
get_orca_screener <- function(token, min_date_time = "2022-01-01 00:00:00") {
  suppressMessages(suppressWarnings({
    library(tidyverse)
    screener = get_orca_data(token, "orca_screener_survey")
    screener = select(screener, -redcap_survey_identifier, -rec_unique_record_id, -orca_screener_survey_complete)
    screener = rename(screener, 
                      timestamp = orca_screener_survey_timestamp,
                      language = rec_language_preference,
                      caregiver_firstname = rec_caregiver_name,
                      caregiver_lastname = rec_caregiver_name_last,
                      phone = rec_phone_number,
                      texting_okay = rec_phone_number_text,
                      email = rec_caregiver_email,
                      over_18 = rec_over_18,
                      zipcode = rec_address_zipcode,
                      race_other = rec_race_other,
                      child_yesno = rec_child_yesno,
                      child_dob = rec_child_dob,
                      pregnant_yesno = rec_pregnant_yesno,
                      due_date = rec_due_date,
                      twin_yesno = rec_twin,
                      education = rec_education_level)
    screener$timestamp = strptime(screener$timestamp, format = "%Y-%m-%d %H:%M:%S") 
    min_date_time = strptime(min_date_time, format = "%Y-%m-%d %H:%M:%S") 
    screener = filter(screener, timestamp > min_date_time)
    
    if (nrow(screener) >= 1) {
      screener = clean_race(screener)
      screener$zipcode <- as.integer(screener$zipcode)
      zip_info <- zips()
      cities <- city_info()
      screener <- left_join(screener, zip_info, by="zipcode")
      screener <- left_join(screener, cities, by="location")
      screener <- mutate(screener,
                         rural = ifelse(is.na(population) & !is.na(location), "Y",
                                        ifelse(population >= 50000, "N",
                                               ifelse(population < 50000, "Y", "CHECK"))))
      screener <- mutate(screener, non_white = ifelse(race == "White", "N", "Y"))
      screener <- mutate(screener, low_ses = ifelse(education == 4, "N",
                                                    ifelse(education == 3, "N", "Y")))
      screener <- mutate(screener, priority = case_when(
        rural == 'N' & non_white == 'N' & low_ses == 'N' ~ 'Low Priority',
        rural == 'Y' | non_white == 'Y' | low_ses == 'Y' ~ 'High Priority'
      ))
      
      screener$current_age <- as.numeric(difftime(Sys.Date(), screener$child_dob, units = 'days'))
      
      screener$child_yesno <- ifelse(is.na(screener$child_yesno), 0, screener$child_yesno)
      screener$pregnant_yesno <- ifelse(is.na(screener$pregnant_yesno), 0, screener$pregnant_yesno)
      
      for (i in 1:nrow(screener)){
        if (screener$child_yesno[i] == 1 & screener$pregnant_yesno[i] == 1) {
          if (!is.na(screener$current_age[i]) & screener$current_age[i] <= 135) {
            new_row <- screener[i, ]
            new_row$child_yesno <- NA
            new_row$child_dob <- NA
            screener$pregnant_yesno[i] <- NA
            screener$due_date[i] <- NA
            screener <- rbind(screener, new_row)
          }
        }
      }
      
      screener <- mutate(screener, expected_invite_date = case_when(
        child_yesno == 1 & current_age <= 135 ~ child_dob + 107,
        pregnant_yesno == 1 ~ due_date + 107
      ))
      screener <- unite(screener, caregiver_name, caregiver_firstname, caregiver_lastname, sep = " ", na.rm = TRUE)
      screener$texting_okay <- gsub(1, "Yes", screener$texting_okay)
      screener$texting_okay <- gsub(0, "No", screener$texting_okay)
      screener$over_18 <- gsub(1, "Yes", screener$over_18)
      screener$over_18 <- gsub(0, "No", screener$over_18)
      screener$language <- gsub(1, "English", screener$language)
      screener$language <- gsub(2, "Spanish", screener$language)
      screener$language <- gsub(3, "Other (check redcap)", screener$language)
      
      screener <- screener %>%
        mutate(rec_source = case_when(
          rec_source == 1 ~ 'Social media',
          rec_source == 2 ~ 'Through a friend',
          rec_source == 3 ~ 'Organization',
          rec_source == 4 ~ 'Other',
          is.na(rec_source) ~ NA
        ))
      
      col_order <- c("record_id", "caregiver_name", "language", "phone", "texting_okay", "email", "over_18", "zipcode", "child_yesno", "child_dob", "pregnant_yesno", "due_date","twin_yesno", "timestamp","timezone", "location", "rec_source", "rec_snowball_email", "rural", "non_white", "low_ses", "priority","rec_comments", "expected_invite_date", "bot_check", "bot_pic_answer")
      screener <- screener[, col_order]
      screener <- dplyr::arrange(screener, timestamp)
    } else {
      print('no new screener responses, returning empty dataframe')
    }
    
  }))

  return (screener)
}


#' @title Duplicate contacts
#' @description Flags any participant with a duplicate phone or email
#' @param data The data frame you wish to act on
#' @return A dataframe with 2 new columns marking 1 for anybody with a duplicate phone or email
#' @export
flag_duplicate_contacts <- function(data) {
  library(dplyr)
  all_emails <- get_orca_field(token, field = 'rec_caregiver_email')
  all_phones <- get_orca_field(token, field = 'rec_phone_number')
  
  for (row in 1:nrow(data)) {
    temp_email <- all_emails %>%
      filter(rec_caregiver_email == data$email[row])
    temp_phones <- all_phones %>%
      filter(rec_phone_number == data$phone[row])
    if (nrow(temp_email) > 1) {
      data[row, 'duplicate_email'] = 1
    } else {data[row, 'duplicate_email'] = 0}
    
    if(nrow(temp_phones) > 1) {
      data[row, 'duplicate_phone'] = 1
    } else {data[row, 'duplicate_phone'] = 0}
  }

  return(data)
}



#' @title Flagging Numeric Names
#' @description Flags any participant with numbers in their name
#' @param data The data frame you wish to act on
#' @return A dataframe with a new column marking 1 for anybody who has numeric values in their name 
#' @export
flag_numeric_names <- function(data) {
  library(dplyr)
  data <- data %>%
    mutate(num_name_flag = ifelse(grepl("[0-9]", caregiver_name), 1, 0))
  return(data)
}

#' @title INELIGIBLE AGES
#' @description Flags any child ages older than 380 days from today
#' @param data The data frame you wish to act on
#' @param threshold_date The date threshold you wish to compare - default is 380 days prior to today
#' @return A dataframe with a new column marking 1 for anybody too old to participate
#' @export
flag_ineligible_age <- function(data, threshold_date = Sys.Date() - 380) {
  library(dplyr)
  for (p in 1:nrow(data)) {
    if (!is.na(data$child_dob[p]) & (data$pregnant_yesno[p] == 0 | is.na(data$pregnant_yesno[p]))) {
      current_age <- as.numeric(difftime(Sys.Date(), data$child_dob[p], units = 'days'))
      data[p, "age_ineligible"] <- ifelse(current_age > 135, 1, 0)
    } else if (!is.na(data$due_date[p])){
      data[p, "age_ineligible"] <- 0
    } else {
      data[p, "age_ineligible"] <- NA
    }
  }
  return(data)
}

#' @title SUSPICIOUS DUE DATES
#' @description Flags any response where the due date is over 40 weeks away or earlier than the day the screener was filled out
#' @param data The data frame you wish to act on
#' @return A dataframe with a new column marking 1 for anybody with an impossible due date
#' @export
flag_due_dates <- function(data) {
  data <- data %>%
    mutate(incorrect_due_date = ifelse(!is.na(due_date) & (difftime(due_date, Sys.Date(), units='days') > 280 | due_date < timestamp), 1, 0))
  return(data)
}

#' @title Flagging Suspicious Referrals
#' @description Checks to see if snowball responses were referred by somebody we have flagged
#' @param data The data frame you wish to act on
#' @param token Unique REDCap API token
#' @param flagged_emails Data frame containing known flagged emails
#' @return A dataframe with a new column marking 1 for anybody with flagged emails
#' @export
flag_referrals <- function(data, token, flagged_emails) {
  library(dplyr)
  ineligible <- get_orca_field(token, field='orca_contact_yesno')
  emails <- get_orca_field(token, 'rec_caregiver_email')
  
  ineligible <- ineligible %>%
    filter(orca_contact_yesno == 0) %>%
    left_join(emails, by='record_id') %>%
    select(-redcap_event_name.x, -redcap_event_name.y)
  
  data <- data %>%
    mutate(flagged_referral = NA)
  for (row in 1:nrow(data)) {
    if (!is.na(data$rec_snowball_email[row])) {
      if (data$rec_snowball_email[row] %in% flagged_emails$rec_caregiver_email | data$rec_snowball_email[row] %in% ineligible$rec_caregiver_email) {
        data[row, 'flagged_referral'] <- 1
      } 
    }
  }
  return(data)
}

#' @title Screens screener export
#' @description Checks for: duplicate contact info, under 18, numeric names, NA names/emails, babies above age threshold, names all lowercase
#' @param data The data frame you wish to act on
#' @return A list with the filtered screener, and a new column flagging lowercase names (doesn't remove). List also contains dataset with removed responses with duplicate contact info and ineligible ages
#' @export
screen_fraudulence <- function(data) {
  library(dplyr)
  data <- data %>%
    flag_ineligible_age(threshold_date = Sys.Date() - 135) %>%
    flag_duplicate_contacts() %>%
    flag_numeric_names() %>%
    flag_due_dates() %>%
    flag_referrals(token, flagged_emails)
  
  #creating new datasets
  ineligible_ages <- data %>%
    filter(age_ineligible == 1)
  duplicate_contacts <- data %>%
    filter(duplicate_email == 1 | duplicate_phone == 1)
  impossible_due_dates <- data %>%
    filter(incorrect_due_date == 1)
  failed_attention_checks <- data %>%
    filter(bot_check != 3 & bot_pic_answer != 4)
  flagged_referrals <- data %>%
    filter(flagged_referral == 1)
  #removing NA names, numeric names, under 18 caregivers, duplicate contact info and age_ineligible babies
  data <- data %>%
    filter(!is.na(phone) | !is.na(email)) %>%
    filter(!is.na(caregiver_name) & over_18 == "Yes" & num_name_flag == 0 & age_ineligible == 0 & incorrect_due_date == 0) %>%
    filter(bot_check == 3) %>%
    filter(duplicate_email == 0 & duplicate_phone == 0)  %>%
    filter(is.na(flagged_referral))
  #removing excess columns
  data <- data %>%
    select(-age_ineligible, -duplicate_email, -duplicate_phone, -num_name_flag, -incorrect_due_date, -bot_check, -bot_pic_answer, -flagged_referral)
  return(list(data = data, ineligible_ages = ineligible_ages, duplicate_contacts = duplicate_contacts, impossible_due_dates = impossible_due_dates, failed_attention_checks = failed_attention_checks, flagged_referrals = flagged_referrals))
}

#' @title Pulls US zipcode database
#' @description Loads a data base of US zipcodes and information (latitude, longitude, population)
#' @return A data frame with every us zipcode and information
#' @export
zip_data <- function() {
  data_path <- system.file("Data", "zip_code_database.csv", package = "OrcaData2")
  data <- read.csv(data_path, na.strings="")
  data$zipcode <- as.character(data$zipcode)
  data$zipcode <- sprintf("%05s", data$zipcode)
  data <- dplyr::mutate(data,
                        town_or_city = ifelse(zipcode >= 10001 & zipcode <= 10282, "New York",
                                              ifelse(zipcode >= 10301 & zipcode <= 10314, "New York",
                                                     ifelse(zipcode >= 10451 & zipcode <= 10475, "New York",
                                                            ifelse(zipcode >= 11004 & zipcode <= 11109, "New York",
                                                                   ifelse(zipcode >= 11351 & zipcode <= 11697, "New York",
                                                                          ifelse(zipcode >= 11201 & zipcode <= 11256, "New York", primary_city)))))))
  data <- dplyr::select(data,
                        zipcode, town_or_city, state, county, timezone, latitude, longitude, irs_estimated_population)
  return(data)
}


#' @title Import data to ORCA 2.0
#' @description Imports the fields in a dataframe to the corresponding record IDS to redcap
#' @param token Unique REDCap API token
#' @param data the data frame you wish to import. Column names must match fields
#' @return A summary of data to be imported/overwritten and requires user input (y) to continue with import
#' @export
import_data <- function(token, data = data) {
  library(redcapAPI)
  
  if ('redcap_event_name' %in% colnames(data)) {
    unique_events <- unique(data$redcap_event_name)
    
    unique_events <- unique(data$redcap_event_name)
    
    #pulling existing redcap data
    all <- get_all_data(token)
    all <- filter(all, redcap_event_name == unique_events)
    all <- all[, colnames(all) %in% colnames(data)]
    
    columns <- colnames(all)[!str_detect(colnames(all), 'record_id') & !str_detect(colnames(all), 'redcap_event_name')]
    
    test <- all %>%
      right_join(data, by=c('record_id', 'redcap_event_name'))
    
    #checking columns with conflicts
    conflicts_for <- as.character()
    
    for (column in columns) {
      column_data <- dplyr::select(test, record_id, redcap_event_name, paste0(column, '.x'), paste0(column, '.y'))
      column_data <- dplyr::filter(column_data, !is.na(column_data[3]) & !is.na(column_data[4]))
      
      #prints rows with conflicts that will be overwritten
      if (nrow(column_data) >= 1) {
        print(paste0('conflict found for field: ', column))
        print.data.frame(column_data)
        cat("\n")
        conflicts_for <- c(conflicts_for, column)
      }
      
    }
    
    #ask user to check above conflicts and respond 'y'/'n' to import
    if (length(conflicts_for) >= 1) {
      cat("Check the datasets above carefully. columns x represent the existing data contents, column y represents the data that will overwrite\n",
          "If column x contains data, this import will OVERWRITE that existing data\n",
          "If the cell contents are the same, there is no new data to import")
    } else {
      print('no conflicts found. No datasets will be overwritten')
      
      print(data) ; cat("check the import data carefully")
    }
    
    
    response <- readline(prompt = "Do you want to continue? (y/n): ")
    
    #data import if participant responds y
    if (tolower(response) == 'y') {
      conn <- redcapConnection(url = url, token = token)
      import_status <- importRecords(conn, data = data)
      print(paste0("Data import completed for ", import_status, " records"))
    } else {
      print("Data import terminated.")
    }
  } else {
    #pulling existing redcap data
    all <- get_all_data(token)
    all <- all[, colnames(all) %in% colnames(data)]
    
    columns <- colnames(all)[!str_detect(colnames(all), 'record_id')]
    
    test <- all %>%
      right_join(data, by=c('record_id'))
    
    #checking columns with conflicts
    conflicts_for <- as.character()
    
    for (column in columns) {
      column_data <- dplyr::select(test, record_id, paste0(column, '.x'), paste0(column, '.y'))
      column_data <- dplyr::filter(column_data, !is.na(column_data[2]) & !is.na(column_data[2]))
      
      #prints rows with conflicts that will be overwritten
      if (nrow(column_data) >= 1) {
        print(paste0('conflict found for field: ', column))
        print.data.frame(column_data)
        cat("\n")
        conflicts_for <- c(conflicts_for, column)
      }
      
    }
    
    #ask user to check above conflicts and respond 'y'/'n' to import
    if (length(conflicts_for) >= 1) {
      cat("Check the datasets above carefully. columns x represent the existing data contents, column y represents the data that will overwrite\n",
          "If column x contains data, this import will OVERWRITE that existing data\n",
          "If the cell contents are the same, there is no new data to import")
    } else {
      print('no conflicts found. No datasets will be overwritten')
      
      print(data) ; cat("check the import data carefully")
    }
    
    
    response <- readline(prompt = "Do you want to continue? (y/n): ")
    
    #data import if participant responds y
    if (tolower(response) == 'y') {
      conn <- redcapConnection(url = url, token = token)
      import_status <- importRecords(conn, data = data)
      print(paste0("Data import completed for ", import_status, " records"))
    } else {
      print("Data import terminated.")
    }
  }
  
  
}


#' @title Pull ORCA Data
#' @description pulls all records & survey responses for ORCA 2.0
#' @param token Unique REDCap API token
#' @return A data frame with all records, survey responses & events
#' @export
get_all_data <- function(token) {
  formData <- list("token"=token,
                   content='record',
                   action='export',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)
  result <- dplyr::filter(result, !stringr::str_detect(record_id, "TEST"))
  result <- dplyr::filter(result, !stringr::str_detect(record_id, "test"))
  result <- dplyr::filter(result, !stringr::str_detect(record_id, "IRB"))
  result <- dplyr::filter(result, !stringr::str_detect(record_id, "D"))
  result <- dplyr::filter(result, record_id != '496' & record_id != '497' & record_id != '498' & record_id != '499')
  
  
  return(result)
}


#' @title Input Reasons for Screener Ignores
#' @description asks for input for each import that has a 0 marked for contact (1-4 codes) and a description for any 'other' reasons
#' @param data the data frame to act on
#' @return A data frame with the reason columns added, only if there were 0s present in the first place
#' @export
input_reason_for_ignores <- function(data) {
  #adding reasons for not contacting
  if (0 %in% data$orca_contact_yesno) {
    for (x in 1:nrow(data)) {
      if (data$orca_contact_yesno[x] == 0) {
        cat(paste0("For record ", data$record_id[x], " why are we ignoring? Please enter a numeric code out of:\n",
                   "1: Potentially Fraudulent\n",
                   "2: Wrong child age\n",
                   "3: Caregiver under 18\n",
                   "4: Other"))
        
        response <- readline(prompt = "Enter your code here: ")
        data[x, "contact_no_why"] <- response
      } else {
        data[x, "contact_no_why"] <- NA
      }
    } 
    #adding other descriptions 
    if (4 %in% data$contact_no_why) {
      for (x in 1:nrow(data)) {
        if (data[x, "contact_no_why"] == 4 & !is.na(data[x, "contact_no_why"])) {
          response <- readline(prompt = paste0("For record ", data[x, "record_id"], " please describe why we are ignoring:"))
          
          data[x, "contact_no_other"] <- response
        } else {
          data[x, "contact_no_other"] <- NA
        }
      }
    }
  }
  return(data)
}

#' @title Standardizes birthweight responses
#' @description takes character birthweight responses, cleans them and splits them into lb and oz columns
#' @param data the data frame to act on
#' @return A data frame with the split birth weight columns (lb, oz)
#' @export
clean_birthweight <- function(data) {
  data$child_birth_weight <- tolower(data$child_birth_weight)
  
  for (row in 1:nrow(data)) {
    
    if (str_detect(data$child_birth_weight[row], "lbs") | str_detect(data$child_birth_weight[row], "libras")) {
      data$child_birth_weight[row] <- gsub("lbs", ",", data$child_birth_weight[row])
      data$child_birth_weight[row] <- gsub("libras", ",", data$child_birth_weight[row])
    } else if (str_detect(data$child_birth_weight[row], "lb") | str_detect(data$child_birth_weight[row], "libra")) {
      data$child_birth_weight[row] <- gsub("lb", ",", data$child_birth_weight[row])
      data$child_birth_weight[row] <- gsub("libra", ",", data$child_birth_weight[row])
    } else if (str_detect(data$child_birth_weight[row], " ") & !str_detect(data$child_birth_weight[row], ",")) {
      data$child_birth_weight[row] <- gsub(" ", ",", data$child_birth_weight[row])
      
    }
  }
  
  
  data <- data %>%
    separate(child_birth_weight, into = c("child_birth_lb", "child_birth_oz"), sep = ",(?=[^,]+$)", remove = F)
  
  data <- data %>%
    mutate(child_birth_lb = parse_number(child_birth_lb),
           child_birth_oz = parse_number(child_birth_oz))
  
  na_values <- sum(is.na(data$child_birth_lb))
  
  if (sum(is.na(data$child_birth_lb)) == sum(is.na(data$child_birth_weight))) {
    data <- dplyr::select(data, -child_birth_weight)
    return(data)
  } else {
    print("there may be an error with child_birth_weight column - check formatting and try again")
  }
}

#' @title Calculates ITN
#' @description calculates ITN based on poverty threshold for number of people in household and annual income
#' @param data the data frame to act on
#' @return A data frame with ITN
#' @export
calculate_itn <- function(data) {
  #creating poverty guideline data base (2024)
  poverty_guidelines <- data.frame(
    household_n = c(1,2,3,4,5,6,7,8),
    income_threshold = c(15060,20440,25820,31200,36580,41960,47340,52720)
  )
  data$annual_income <- as.numeric(data$annual_income)
  data <- dplyr::mutate(data, itn = NA)
  for (row in 1:nrow(data)) {
    total_household = (data$children_home[row] + data$adults_home[row])
    if (!is.na(total_household) & total_household <= 8) {
      index <- which(poverty_guidelines$household_n == total_household)
    } else if (is.na(total_household)) {
      index = NA
    } else {
      index = 8
    }
    
    if (!is.na(index)) {
      threshold = poverty_guidelines$income_threshold[index]
      data$itn[row] <- data$annual_income[row] / threshold
    } 
  }
  return(data)
}

#' @title Pulling Eligible Screener Responses
#' @description Pulls screener responses who have been enrolled in the study
#' @param token Unique REDCap API token
#' @param min_date_time The minimum timestamp to pull responses after
#' @return A data frame with screener record id and group participant was enrolled within (prenatal/postnatal)
#' @export
get_eligible_responses <- function(token, min_date_time = "2022-01-01 00:00:00") {
  screener <- get_orca_screener(token, min_date_time = min_date_time)
  eligible <- get_orca_field(token, field='orca_contact_yesno')
  
  eligible <- dplyr::filter(eligible, orca_contact_yesno == 1)
  
  screener <- dplyr::right_join(screener, eligible, by="record_id")
  
  screener$days_to_birth = as.numeric(difftime(screener$due_date, Sys.Date(), units="days"))
  screener$age_at_screener = as.numeric(difftime(screener$timestamp, screener$child_dob, units="days"))
  
  screener <- dplyr::mutate(screener, group = ifelse(!is.na(child_dob) & is.na(due_date) & age_at_screener <= 137, 'postnatal',
                                                     ifelse(!is.na(due_date) & is.na(child_dob), 'prenatal',
                                                            ifelse(!is.na(due_date) & !is.na(child_dob) & child_dob > 137, 'prenatal', 'hm'))))
  screener <- dplyr::select(screener, record_id, group)
  
  return(screener)
}


#' @title Pulling Screener Contact Info
#' @description Imports contact information from the screener
#' @param token Unique REDCap API token
#' @return A data frame with screener record id, caregiver first/last name, phone number, texting preferences and email
#' @export
get_screener_contact_info <- function(token) {
  library(dplyr)
  screener <- get_orca_data(token, form = "orca_screener_survey")
  screener <- screener %>%
    select(record_id, rec_caregiver_name, rec_caregiver_name_last, rec_phone_number, rec_phone_number_text, rec_caregiver_email)
  
  return(screener)
  
}


#' @title Pulling New Snowball Responses
#' @description Imports new screener responses which have been referred by another person. Will match up the email with the existing participant and put them into another df to compensate. If no id can be found for the email, user will be required to input one
#' @param token Unique REDCap API token
#' @param min_date_time The minimum timestamp to pull responses after
#' @return A list with 'snowball_responses': a df with new responses and the id that referred them, and 'to_compensate': a df with list of ids who have referred people, the number of people referred by them
#' @export
get_new_snowball_responses <- function(token, min_date_time = "2022-01-01 00:00:00") {
  library(dplyr)
  snowball <- get_orca_data(token, form = "orca_screener_survey")
  
  
  min_date_time <- as.POSIXct(min_date_time, format = "%Y-%m-%d %H:%M:%S")
  
  snowball <- snowball %>%
    filter(orca_screener_survey_timestamp >= min_date_time) %>%
    filter(rec_source == 2 & !is.na(rec_snowball_email) | !is.na(rec_snowball_name)) %>%
    filter(is.na(snowball_comp_resolved)) %>%
    select(record_id, orca_screener_survey_timestamp, rec_source, rec_snowball_name, rec_snowball_email, id_to_compensate, snowball_comp_resolved)
  
  contact <- get_screener_contact_info(token)
  contact <- contact %>%
    mutate(full_name = paste(rec_caregiver_name, rec_caregiver_name_last))
  
  if (nrow(snowball) >= 1) {
    for (x in 1:nrow(snowball)) {
      
      if (!is.na(snowball$rec_snowball_email[x])) {
        temp <- filter(contact, rec_caregiver_email == snowball$rec_snowball_email[x])
        id <- ifelse(nrow(temp) >= 1, as.character(temp$record_id[1]), 'cannot find id')
        
        snowball[x, 'id_to_compensate'] <- id
      } else if (!is.na(snowball$rec_snowball_name[x]) & is.na(snowball$rec_snowball_email[x])) {
        temp <- filter(contact, tolower(full_name) == tolower(snowball$rec_snowball_name[x]))
        id <- ifelse(nrow(temp) >= 1, as.character(temp$record_id[1]), 'cannot find id')
        
        snowball[x, 'id_to_compensate'] <- id
      } else {
        snowball[x, 'id_to_compensate'] <- 'manually check'
      }
      
    }
    
    #performing manual checks
    for (x in 1:nrow(snowball)) {
      if (snowball$id_to_compensate[x] == 'manually check' | snowball$id_to_compensate[x] == 'cannot find id') {
        cat("Find the screener record ID for the following participant:\n",
            paste0("Name: ", snowball$rec_snowball_name[x], "\n"),
            paste0("Email: ", snowball$rec_snowball_email[x], "\n"),
            "\n",
            "If they are not an ORCA particiapnt (e.g. Natalie Brito), enter 0\n",
            "If they are a participant from orca 1.0, enter 1.0_id (e.g. 1.0_067)")
        
        response <- readline(prompt = "Enter screener record id: ")
        snowball$id_to_compensate[x] <- response
        
      }
    }
    
    existing_snowball_comps <- get_orca_data(token, form= "prenatal_compensation", form_complete = F)
    existing_snowball_comps <- existing_snowball_comps %>%
      filter(!is.na(prenatal_snowball_number)) %>%
      select(record_id, prenatal_snowball_number, prenatal_snowball_total) %>%
      mutate(maxed_out = ifelse(prenatal_snowball_total == 50, 1, 0))
    
    existing_snowball_comps$record_id <- as.character(existing_snowball_comps$record_id)
    
    
    #creating dataframe of ids to compensate
    ids_to_compensate <- data.frame(table(snowball$id_to_compensate))
    ids_to_compensate <- ids_to_compensate %>%
      filter(Var1 != '0') %>%
      rename(record_id = Var1, n_new_referrals = Freq) %>%
      mutate(comp_amount = ifelse(n_new_referrals <= 5, n_new_referrals*10, 50))
    
    ids_to_compensate$record_id <- as.character(ids_to_compensate$record_id)
    
    ids_to_compensate <- ids_to_compensate %>%
      left_join(existing_snowball_comps, by="record_id") %>%
      filter(is.na(maxed_out) | maxed_out == 0)
    
    ids_to_compensate <- ids_to_compensate %>%
      mutate(prenatal_snowball_number = ifelse(!is.na(prenatal_snowball_number), prenatal_snowball_number + n_new_referrals, n_new_referrals),
             redcap_event_name = "prenatal_surveys_arm_1") 
    
    
    return(list(snowball_responses = snowball, ids_to_compensate = ids_to_compensate))
  } else {
    cat("\n", "No new snowball responses!")
  }
}

#' @title Pulling ALL Snowball Responses
#' @description Pulls new screener responses which have been referred by another person and all ids who have referred another person
#' @param token Unique REDCap API token
#' @param min_date_time The minimum timestamp to pull responses after
#' @return A list with 'snowball_responses': a df with new responses and the id that referred them, and 'ids_who_referred': a df with list of ids who have referred people, the number of people referred by them
#' @export
get_all_snowball_responses <- function(token, min_date_time = "2022-01-01 00:00:00") {
  snowball <- get_orca_data(token, form = "orca_screener_survey")
  
  
  min_date_time <- as.POSIXct(min_date_time, format = "%Y-%m-%d %H:%M:%S")
  
  snowball <- snowball %>%
    filter(orca_screener_survey_timestamp >= min_date_time) %>%
    filter(rec_source == 2 & !is.na(rec_snowball_email) | !is.na(rec_snowball_name)) %>%
    select(record_id, orca_screener_survey_timestamp, rec_source, rec_snowball_name, rec_snowball_email, id_to_compensate, snowball_comp_resolved)
  
  contact <- get_screener_contact_info(token)
  contact <- contact %>%
    mutate(full_name = paste(rec_caregiver_name, rec_caregiver_name_last))
  
  if (nrow(snowball) >= 1) {
    for (x in 1:nrow(snowball)) {
      
      if (!is.na(snowball$rec_snowball_email[x])) {
        temp <- filter(contact, rec_caregiver_email == snowball$rec_snowball_email[x])
        id <- ifelse(nrow(temp) >= 1, as.character(temp$record_id[1]), 'cannot find id')
        
        snowball[x, 'id_to_compensate'] <- id
      } else if (!is.na(snowball$rec_snowball_name[x]) & is.na(snowball$rec_snowball_email[x])) {
        temp <- filter(contact, tolower(full_name) == tolower(snowball$rec_snowball_name[x]))
        id <- ifelse(nrow(temp) >= 1, as.character(temp$record_id[1]), 'cannot find id')
        
        snowball[x, 'id_to_compensate'] <- id
      } else {
        snowball[x, 'id_to_compensate'] <- 'manually check'
      }
      
    }
  }
  
  existing_snowball_comps <- get_orca_data(token, form= "prenatal_compensation", form_complete = F)
  existing_snowball_comps <- existing_snowball_comps %>%
    filter(!is.na(prenatal_snowball_number)) %>%
    select(record_id, prenatal_snowball_number, prenatal_snowball_total, prenatal_snowball_pending) %>%
    mutate(maxed_out = ifelse(prenatal_snowball_total == 50, 1, 0))
  
  existing_snowball_comps$record_id <- as.character(existing_snowball_comps$record_id)
  
  return(list(snowball_responses = snowball, ids_who_referred = existing_snowball_comps))
  
}

#' Generating N for Specified Timepoint
#'
#' This function with return a numeric variable for the number of people who have completed a particular timepoint
#'
#' @param token Unique REDCap token ID
#' @param timepoint integer reflecting the timepoint (4,8,12)
#' @return Numeric variable for that event
#' @export
get_visit_n <- function(token, timepoint = 4) {
  
  if (timepoint == 4) {
    data <- get_orca_field(token, field="visit_date_4m")
    data <- data %>%
      filter(!str_detect(record_id, 'P') & !str_detect(record_id, 'p'))
    
    n <- nrow(data)
    return(n)
  } else if (timepoint == 8){
    data <- get_orca_field(token, field="visit_date_8m")
    data <- data %>%
      filter(!str_detect(record_id, 'P') & !str_detect(record_id, 'p'))
    n <- nrow(data)
    return(n)
  } else if (timepoint == 12) {
    data <- get_orca_field(token, field="visit_date_12m")
    data <- data %>%
      filter(!str_detect(record_id, 'P') & !str_detect(record_id, 'p'))
    n <- nrow(data)
    return(n)
  }
  
}


#Survey functions
#' @title Process EPDS postpartum depression data
#'
#' @description This function will download and return the totalsummed scores for the EPDS. Only total scores, and not cutoff values, are returned.
#'
#' @param token Unique REDCap token ID
#' @param timepoint REDCap event name (character) default is orca_4month_arm_1
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_orca_epds <- function(token, timepoint = 'orca_4month_arm_1', timestamp=T) {
  epds = get_orca_data(token, "edinburgh_postnatal_depression_scale", form_complete = T)
  epds <- dplyr::filter(epds, redcap_event_name == timepoint)
  
  if (timestamp) {
    epds = epds[,c("record_id", "edinburgh_postnatal_depression_scale_timestamp", "epds_total", "epds_dep", "epds_anx")]
  } else if (!timestamp) {
    epds = epds[,c("record_id", "epds_total", "epds_dep", "epds_anx")]
  }

  return (epds)
}


#' @title Process Perceived Stress Scale Data
#'
#' @description This function will download and return the total scores for pss
#'
#' @param token Unique REDCap token ID
#' @param timepoint REDCap event name (character) default is orca_4month_arm_1
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_orca_pss <- function(token, timepoint = 'orca_4month_arm_1', timestamp=T) {
  pss <- get_orca_data(token, form = 'perceived_stress_scale', form_complete=T)
  pss <- dplyr::filter(pss, redcap_event_name == timepoint)
  
  if (timestamp) {
    pss = pss[,c("record_id", "pss_date", "pss_score", "pss_14_score", 'pss_10_score_cutoff')]
  } else if (!timestamp) {
    pss = pss[,c("record_id", "pss_score", "pss_14_score", 'pss_10_score_cutoff')]
  }
  
  return (pss)
}

#' @title Process Postpartum Bonding Questionnaire Scores
#'
#' @description This function will download and return the total scores for pbq
#'
#' @param token Unique REDCap token ID
#' @param timepoint REDCap event name (character) default is orca_4month_arm_1
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_orca_pbq <- function(token, timepoint='orca_4month_arm_1', timestamp=T) {
  library(dplyr)
  pbq <- get_orca_data(token, form='postpartum_bonding_questionnaire', form_complete = T)
  pbq <- dplyr::filter(pbq, redcap_event_name == timepoint)
  
  pbq$impaired_bonding <- rowSums(pbq[, c("pbq1", "pbq2", "pbq6", "pbq7", "pbq8", "pbq9", "pbq10",
                                          "pbq12", "pbq13", "pbq15","pbq16", "pbq17")], na.rm = TRUE)
  pbq$rpa <- rowSums(pbq[, c("pbq3", "pbq4", "pbq5", "pbq11", "pbq14", "pbq21", "pbq23")], na.rm = TRUE)
  
  pbq$if_anxiety <- rowSums(pbq[, c("pbq19", "pbq20", "pbq22", "pbq25")], na.rm = TRUE)
  
  pbq$inc_abuse <- rowSums(pbq[, c("pbq18", "pbq24")], na.rm = TRUE)
  
  #cutoffs
  pbq <- pbq %>%
    mutate(ib_cutoff = ifelse(impaired_bonding >= 12, 1, 0),
           rpa_cutoff = ifelse(rpa >= 13, 1, 0),
           if_anxiety_cutoff = ifelse(if_anxiety >=10, 1, 0),
           inc_abuse_cutoff = ifelse(inc_abuse >= 3, 1,0))
  
  if (timestamp) {
    pbq <- pbq %>%
      select(record_id, postpartum_bonding_questionnaire_timestamp, impaired_bonding:inc_abuse_cutoff)
  } else if (!timestamp) {
    pbq <- pbq %>%
      select(record_id, impaired_bonding:inc_abuse_cutoff)
  }

  
  return(pbq)
}

#' @title Process IBQ Data
#' @description This function will download and return the mean scores for the IBQ subscales (surgency, negative affect, effortful control).
#' @param token Unique REDCap token ID
#' @param timepoint REDCap event name (character) default is orca_4month_arm_1
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_orca_ibq <- function(token, timepoint = 'orca_4month_arm_1', timestamp=T) {
  ibq <- get_orca_data(token, form = 'infant_behavior_questionnaire_very_short_form')
  ibq <- dplyr::filter(ibq, redcap_event_name == timepoint)
  
  #reversing item 11
  ibq[ibq== 999] = NA
  ibq$ibq11r <- (8-ibq$ibq_11)
  ibq$ibq_sur <- rowMeans(ibq[, c("ibq_01", "ibq_02", "ibq_07", "ibq_08", "ibq_13", "ibq_14", "ibq_15", 
                                  "ibq_20", "ibq_21", "ibq_26", "ibq_27", "ibq_36", "ibq_37")], na.rm = TRUE)
  ibq$ibq_neg <- rowMeans(ibq[, c("ibq_03", "ibq_04", "ibq_09", "ibq_10", "ibq_16", "ibq_17", "ibq_22", 
                                  "ibq_23", "ibq_28", "ibq_29", "ibq_32", "ibq_33")], na.rm = TRUE)
  ibq$ibq_ec <- rowMeans(ibq[, c("ibq_05", "ibq_06", "ibq11r", "ibq_12", "ibq_18", "ibq_19", "ibq_24", 
                                 "ibq_25", "ibq_30", "ibq_31", "ibq_34", "ibq_35")], na.rm = TRUE)
  
  if (timestamp) {
    ibq <- ibq[, c("record_id", "infant_behavior_questionnaire_very_short_form_timestamp", "ibq_sur", "ibq_neg", "ibq_ec")]
    
  } else if (!timestamp) {
    ibq <- ibq[, c("record_id", "ibq_sur", "ibq_neg", "ibq_ec")]
  }

  
  return(ibq)
}

#'@title Determine Study Eligibility
#'@description Takes a screener df and bins participants into different studies depending on age/demographic priority
#'@param data screener dataframe to act upon. Must contain 'child_dob' and 'priority' columns 
#'@return a dataframe with new column 'orca_study_enrollment' added
#'@export
study_eligibility <- function(data) {
  library(dplyr)
  data$current_age <- as.numeric(difftime(Sys.Date(), data$child_dob, units = 'days'))
  
  if ('child_dob' %in% colnames(data) & 'priority' %in% colnames(data)) {
    data$current_age <- as.numeric(difftime(Sys.Date(), data$child_dob, units = 'days'))
    
    data <- data %>%
      mutate(orca_study_enrollment = case_when(
        current_age > 137 & current_age <= 304 ~ 'ORCA 1.0',
        current_age >= 1 & current_age <= 137 ~ 'MICE BL',
        pregnant_yesno == 1 & priority == 'High Priority' ~ 'ORCA 2.0',
        pregnant_yesno == 1 & priority == 'Low Priority' ~ 'MICE',
        pregnant_yesno == 0 & current_age > 304 ~ 'ineligible_age'
      ))
    
    data <- data %>%
      select(-current_age)
  } else {
    print('either child_dob or priority columns not present in the data. Check')
  }

  return(data)
}

#' @title Process QUIC 5 Data
#' @description This function will download and return the total scores for the quic 5
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name for the timepoint you wish to pull
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_quic5 <- function(token, timepoint = "orca_4month_arm_1", timestamp=T) {
  library(dplyr)
  quic5 = get_orca_data(token, "quic_5")
  quic5 = dplyr::filter(quic5, redcap_event_name == timepoint)
  
  quic5$quic5_1r = 1 - quic5$quic5_1r
  quic5$quic5_4r = 1 - quic5$quic5_4r
  quic5$quic5_all = quic5$quic5_1r + quic5$quic5_2 + quic5$quic5_3 + quic5$quic5_4r + quic5$quic5_5
  
  if (timestamp) {
    quic5 = quic5[,c("record_id", "quic_5_timestamp","quic5_all")]
  } else if (!timestamp) {
    quic5 = quic5[,c("record_id","quic5_all")]
  }
  return (quic5)
}

#' @title Get Missing REDCap IDs
#' @description This function will pull any missing consecutive ids from redcap (that may need to be reassigned)
#' @param token Unique REDCap token ID
#' @return A numeric vector with the missing ids
#' @export
get_missing_ids <- function(token) {
  library(dplyr)
  all_ids <- get_all_data(token)
  
  ids_4m <- all_ids %>%
    filter(redcap_event_name == 'orca_4month_arm_1' & !str_detect(tolower(record_id), 'p'))
  
  ids_4m <- ids_4m %>%
    select(record_id, pre_visit_date_4m, visit_scheduled_4m, visit_date_4m)
  
  flagged_ids <- as.character()
  
  for (row in 1:nrow(ids_4m)) {
    if(!is.na(ids_4m$pre_visit_date_4m[row])) {
      time_since_pre <- as.numeric(difftime(Sys.Date(), ids_4m$pre_visit_date_4m[row], units='days'))
      
      if (time_since_pre >= 21 & is.na(ids_4m$visit_date_4m[row])) {
        flagged_ids <- c(flagged_ids, ids_4m$record_id[row])
      }
    }
  }
  
  all_ids <- all_ids %>%
    distinct(record_id,.keep_all = T) %>%
    filter(redcap_event_name == 'orca_participant_m_arm_1') %>%
    select(record_id) %>%
    filter(!str_detect(tolower(record_id), 'p'))
  
  x <- min(all_ids$record_id):max(all_ids$record_id)
  
  missing_ids <- as.numeric()
  
  for (n in x) {
    if (n %in% all_ids$record_id) {
    } else {
      missing_ids <- c(missing_ids, n)
    }
  }
  
  if (length(flagged_ids) > 0) {
    cat('Check whether the following ids can be reassigned: ', '\n',
        flagged_ids)
  }
  return(missing_ids)
}


#' @title Assign New Ids
#' @description this function will take existing ids and create new ones for new participants. It will fill missing ids.
#' @param existing_ids a vector of existing ids
#' @param new_participants a vector or new participants you want to create ids for 
#' @param prefix a string prefix if required in id format  e.g. 'pch_' for pch_001
#' @param digits the number of digits in your id e.g. 3 for pch_001
#' @return A numeric vector with the missing ids
#' @export
assign_ids <- function(existing_ids, new_participants, prefix, digits=3) {
  existing_ids <- as.numeric(gsub(prefix, '', existing_ids))
  existing_ids <- as.vector(na.omit(existing_ids))
  
  #checking for missing ids
  if (length(existing_ids) >= 1) {
    complete_sequence <- seq(min(existing_ids), max(existing_ids))
    missing_numbers <- setdiff(complete_sequence, existing_ids)
  } else {
    missing_numbers = as.numeric()
    existing_ids = 0
  }
  
  new_ids = as.numeric()
  
  if (length(missing_numbers) >= 1) {
    new_ids <- missing_numbers
  }
  
  start_n = length(new_ids)+1
  next_id = max(existing_ids) + 1
  new_id = next_id
  
  if (length(new_participants) > length(new_ids)) {
    for (x in start_n:length(new_participants)) {
      new_ids <- c(new_ids, new_id)
      new_id <- new_id + 1
    }
  }
  
  
  digits = paste0("%0", digits, "d")
  new_ids <- paste0(prefix, sprintf(digits, new_ids))
  return(new_ids)
}

#' @title Getting Trimester Numbers
#' @description This function will pull the number of prenatal participants in trimester 1, 2,3
#' @param token REDCap API token for SCREENER project
#' @param field string variable for name of your due date field, default is rec_due_date
#' @return A dataframe with 'trimester' and 'n' column
#' @export
get_trimester_n <- function(token, field = 'rec_due_date') {
  library(dplyr)
  due_date <- get_orca_field(token, field='rec_due_date')
  eligibility <- get_orca_field(token, field='orca_contact_yesno')
  
  due_date <- due_date %>%
    left_join(eligibility, by='record_id') %>%
    select(record_id, rec_due_date, orca_contact_yesno) %>%
    filter(orca_contact_yesno == 1)
  
  due_date <- due_date %>%
    filter(rec_due_date > Sys.Date())
  
  due_date$weeks_till_birth <- difftime(due_date$rec_due_date, Sys.Date(),units='w')
  due_date$gestational_age <- 40 - due_date$weeks_till_birth
  
  due_date <- due_date %>%
    mutate(trimester = case_when(
      gestational_age < 13 ~ 't1',
      gestational_age >= 13 & gestational_age < 27 ~ 't2',
      gestational_age >= 27 ~ 't3'
    ))
  
  trimesters <- data.frame(table(due_date$trimester))
  trimesters <- trimesters %>%
    rename(trimester = Var1, n = Freq)
  
  return(trimesters)
}

#' @title Process STAI 5 Data
#' @description This function will download and return the total state and trait scores for the stai 5
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name for the timepoint you wish to pull
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_stai5 <- function(token, timepoint = "orca_4month_arm_1", timestamp=T) {
  library(dplyr)
  stai5 = get_orca_data(token, "state_trait_anxiety_inventory")
  stai5 = dplyr::filter(stai5, redcap_event_name == timepoint)
  
  stai5 <- dplyr::filter(stai5, redcap_event_name == timepoint)
  stai5$stais <- rowSums(stai5[, c("stais1", "stais2", "stais3", "stais4", "stais5")], na.rm = TRUE)
  stai5$stait <- rowSums(stai5[, c("stait1", "stait2", "stait3", "stait4", "stait5")], na.rm = TRUE)
  stai5 <- stai5 %>% mutate(s_cutoff = ifelse(stais > 9.5, 1, 0), t_cutoff = ifelse(stait > 13.5, 1, 0))
  
  if (timestamp) {
    stai5 = stai5[,c("record_id", "state_trait_anxiety_inventory_timestamp","stais", "stait", "s_cutoff", "t_cutoff")]
  } else if (!timestamp) {
    stai5 = stai5[,c("record_id","stais", "stait", "s_cutoff", "t_cutoff")]
  }

  return(stai5)
}

#' @title Process Difficulties in Emotion Regulation Data
#' @description This function will download and return the total scores for each subscale of the DERS
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name for the timepoint you wish to pull
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_ders <- function(token, timepoint = "orca_4month_arm_1", timestamp=T) {
  library(dplyr)
  ders = get_orca_data(token, "difficulties_in_emotional_regulation_16")
  ders = dplyr::filter(ders, redcap_event_name == timepoint)
  
  ders <- dplyr::filter(ders, redcap_event_name == timepoint)
  ders$total_score <- rowSums(ders[, c("ders_1", "ders_2", "ders_3", "ders_4", "ders_5", "ders_6", "ders_7", "ders_8", "ders_9", "ders_10", "ders_11", "ders_12", "ders_13", "ders_14", "ders_15", "ders_16")], na.rm = TRUE)
  ders$clarity <- rowSums(ders[, c("ders_1", "ders_2")], na.rm = TRUE)
  ders$goals <- rowSums(ders[, c("ders_3", "ders_7", "ders_15")], na.rm = TRUE)
  ders$impulse <- rowSums(ders[, c("ders_4", "ders_8", "ders_11")], na.rm = TRUE)
  ders$strategies <- rowSums(ders[, c("ders_5", "ders_6", "ders_12", "ders_14", "ders_16")], na.rm = TRUE)
  ders$nonacceptance <- rowSums(ders[, c("ders_9", "ders_10")], na.rm = TRUE)
  
  if (timestamp) {
    ders = ders[,c("record_id", "difficulties_in_emotional_regulation_16_timestamp","total_score", "clarity", "goals", "impulse", "strategies", "nonacceptance")]
  } else if (!timestamp) {
    ders = ders[,c("record_id", "difficulties_in_emotional_regulation_16_timestamp","total_score", "clarity", "goals", "impulse", "strategies", "nonacceptance")]
  }
  
  return(ders)
}

#' @title Pull Screener Child DOB
#' @description This function will download and return the most up to date date of birth value from the screener
#' @param token Unique REDCap token ID
#' @return A data frame with all 3 dob variables (due date, rec_child_dob and child_dob_update) and final dob
#' @export
get_screener_child_dob <- function(token) {
  library(dplyr)
  cohort <- get_orca_cohort(token, screener=T)
  due_date <- get_orca_field(token, field='rec_due_date') 
  dob <- get_orca_field(token, field='rec_child_dob')
  dob2 <- get_orca_field(token, field='child_dob_update')
  
  total <- cohort %>%
    left_join(due_date, by='record_id') %>%
    left_join(dob, by='record_id') %>%
    left_join(dob2, by='record_id') %>%
    select(record_id, rec_due_date, rec_child_dob, child_dob_update)
  
  total <- total %>%
    mutate(final_dob = ifelse(is.na(rec_due_date) & is.na(child_dob_update) & !is.na(rec_child_dob), rec_child_dob,
                              ifelse(!is.na(rec_due_date) & is.na(child_dob_update), rec_due_date,
                                     ifelse(!is.na(rec_due_date) & !is.na(child_dob_update), child_dob_update,
                                            ifelse(!is.na(rec_due_date) & !is.na(rec_child_dob) & !is.na(child_dob_update), child_dob_update,
                                                   ifelse(is.na(rec_due_date) & !is.na(rec_child_dob) & !is.na(child_dob_update), child_dob_update, 'ERROR'))))))
  
  total$final_dob <- as.numeric(total$final_dob)
  total$final_dob <- as.Date(total$final_dob, origin='1970-01-01')
  
  return(total)
  
}

#' @title Pull Records in Cohort
#' @description This function will pull all records in a particular cohort (default is main ORCA postnatal)
#' @param token Unique REDCap token ID
#' @param screener Set to 'T' to pull screener cohort (prospective/prenatal)
#' @return A dataframe with all record ids included in cohort 
#' @export
get_orca_cohort <- function(token, screener=F) {
  library(dplyr)
  
  if (!screener) {
    records <- get_orca_field(token, field='record_id')
    records <- records %>%
      filter(redcap_event_name == 'orca_4month_arm_1') %>%
      select(record_id)
    
    #eligibility fields
    to_keep <- get_orca_field(token, field='optout_yesno') %>%
      select(-redcap_event_name)
    
    long <- get_orca_field(token, field='longitudinal_yesno') %>%
      select(-redcap_event_name)
    
    records <- records %>%
      left_join(to_keep, by="record_id") %>%
      left_join(long, by='record_id')
    
    records <- records %>%
      filter(optout_yesno == 0 | is.na(optout_yesno)) %>%
      filter(longitudinal_yesno != 0 & !is.na(longitudinal_yesno)) %>%
      filter(!str_detect(record_id, 'P'))
    
    records <- records %>%
      select(record_id)
    
    
    #visit date info
    date_4m <- get_orca_field(token, field='visit_date_4m')
    date_8m <- get_orca_field(token, field='visit_date_8m')
    date_12m <- get_orca_field(token, field='visit_date_12m')
    #date_12m <- get_orca_field(token, field='visit_date_12m')
    
    visit_dates <- date_4m %>%
      left_join(date_8m, by='record_id') %>%
      left_join(date_12m)
    
    cols <- colnames(visit_dates)[!str_detect(colnames(visit_dates), 'redcap_event_name')]
    visit_dates <- visit_dates[, cols]
    
    records <- records %>%
      left_join(visit_dates, by='record_id')
    
    
    return(records)
    
    
  } else if (screener) {
    records <- get_orca_field(token, field='record_id')
    records <- records %>%
      filter(redcap_event_name == 'orca_screener_arm_1') %>%
      select(-redcap_event_name)
    
    #pulling enrollment info
    eligibility <- get_orca_data(token, form='contact_log', form_complete = F) %>%
      filter(redcap_event_name=='orca_screener_arm_1')
    
    eligibility <- eligibility %>%
      filter(orca_study_enrollment___2 == 0) %>%
      select(record_id, orca_contact_yesno, orca_study_enrollment___1,orca_study_enrollment___3:orca_study_enrollment___6) %>%
      rename(orca = orca_study_enrollment___1, mice = orca_study_enrollment___3,
             mice_baseline = orca_study_enrollment___4, peach = orca_study_enrollment___5,
             reef = orca_study_enrollment___6
      )
    
    
    records <- records %>%
      left_join(eligibility, by='record_id') %>%
      filter(orca_contact_yesno == 1)
    
    records <- records %>%
      select(-orca_contact_yesno)
    
    return(records)
    
  }
  
}

#' @title Pull Expected Invites
#' @description This function will create a list with all future invites for a specified timepoint, a graph and counts showing invites per month
#' @param token Unique REDCap token ID - use screener token if looking at timepoints 4m and below
#' @param timepoint 'prenatal', 'newborn', '4m', '8m', '12m'
#' @param max_date Maximum date to cut off output - default is 'none'. Must be format of 'YYYY-MM-DD'
#' @return A list with dataframe of all invites, bar chart, and dataframe with invite counts per month
#' @export
get_expected_invites <- function(token, timepoint = '4m', max_date = 'none') {
  library(dplyr)
  library(zoo)
  age_in <- case_when(
    timepoint == '4m' ~ 107,
    timepoint == '8m' ~ 228,
    timepoint == '12m' ~  350,
    timepoint == 'newborn' ~ 7,
    timepoint == 'prenatal' ~ -84
  )
  
  #SCREENER PROJECT DEPENDENT - prenatal, peach, 4m
  #ORCA PROJECT DEPENDENT - 8m, 12m
  
  if (timepoint == 'prenatal') {
    screener_cohort <- get_orca_cohort(token, screener=T)
    due_date <- get_orca_field(token, field='rec_due_date')
    
    data <- screener_cohort %>%
      left_join(due_date, by='record_id') %>%
      select(record_id, rec_due_date) %>%
      filter(!is.na(rec_due_date)) 
    
    data <- data %>%
      mutate(exp_invite_date = rec_due_date -84,
             invite_month = format(exp_invite_date, "%b %y")) %>%
      filter(exp_invite_date > Sys.Date())
    
  } else if (timepoint == 'newborn') {
    screener_cohort <- get_orca_cohort(token, screener=T)
    due_date <- get_orca_field(token, field='rec_due_date')
    
    prenatal <- screener_cohort %>%
      left_join(due_date, by='record_id') %>%
      select(record_id, rec_due_date) %>%
      filter(!is.na(rec_due_date)) 
    
    prenatal <- prenatal %>%
      mutate(exp_invite_date = rec_due_date + age_in) %>%
      filter(exp_invite_date > Sys.Date())
    
    data <- get_orca_data(token, form='contact_log', form_complete = F) %>%
      filter(orca_study_enrollment___5 == 1) %>%
      select(record_id)
    
    data <- data %>%
      left_join(prenatal, by="record_id")
    data <- data %>%
      mutate(invite_month = format(exp_invite_date, "%b %y"))
    
  } else if (timepoint == '4m') {
    screener_cohort <- get_orca_cohort(token, screener=T)
    child_dob <- get_screener_child_dob(token)
    
    child_dob <- child_dob %>%
      select(record_id, final_dob)
    
    data <- screener_cohort %>%
      left_join(child_dob, by='record_id') %>%
      filter(orca == 1 | mice == 1) %>%
      select(record_id, final_dob, orca, mice)
    
    data$exp_invite_date = data$final_dob + 107
    
    data <- data %>%
      filter(exp_invite_date > Sys.Date()) %>%
      mutate(invite_month = format(exp_invite_date, "%b %y"))
    
  } else if (timepoint == '8m' | timepoint == '12m') {
    orca_cohort <- get_orca_cohort(token) %>%
      select(record_id)
    
    dob <- get_orca_field(token, field='child_dob') %>%
      select(-redcap_event_name)
    
    data <- orca_cohort %>%
      left_join(dob, by='record_id')
    
    data <- data %>%
      mutate(exp_invite_date = child_dob + age_in,
             invite_month = format(exp_invite_date, "%b %y")) %>%
      select(-child_dob) %>%
      filter(exp_invite_date > Sys.Date())
    
    
  }
  
  if (max_date != 'none') {
    max_date = as.Date(max_date, format='%Y-%m-%d')
    
    data <- data %>%
      filter(exp_invite_date <= max_date)
  }
  #GRAPH 
  data$invite_month <- factor(data$invite_month, levels = unique(data$invite_month))
  
  colors <- '#dabfff'
  
  
  title = paste0("Future invites by month for ", timepoint, " timepoint")
  
  plot <- ggplot(data, aes(x = invite_month)) +
    geom_bar(position = "stack", width = 0.8, fill='#dabfff', color='black') +
    labs(title = title,
         x = "Month-Year",
         y = "Number of Invites") + 
    theme(panel.background = element_rect(fill="white"),
          panel.border=element_rect(color="#3C1939", fill=NA, linewidth=1),
          legend.title = element_blank(), 
          axis.text = element_text(family = 'Arial', size = 10),
          axis.title = element_text(family='Arial', size=12),
          legend.text = element_text(family='Arial', size=11),
          title = element_text(family="Arial", size=14)) +
    scale_fill_manual(values = colors)
  plot
  
  
  #creating data frame with counts per month 
  if (timepoint == '4m') {
    orca_counts = data.frame(table(subset(data, orca == 1)$invite_month)) %>%
      rename(month = Var1, orca_total = Freq) %>%
      mutate(month = as.yearmon(month, "%b %y")) %>%
      arrange(month)
    
    mice_counts = data.frame(table(subset(data, mice == 1)$invite_month)) %>%
      rename(month = Var1, mice_total = Freq) %>%
      mutate(month = as.yearmon(month, "%b %y")) %>%
      arrange(month)
    
    counts = full_join(orca_counts, mice_counts, by='month')
    
  } else {
    counts <- data.frame(table(data$invite_month)) %>%
      rename(total = Freq, month = Var1) %>%
      mutate(month = as.yearmon(month, "%b %y")) %>%
      arrange(month)
    
  }
  
  result <- list(data = data, plot = plot, counts = counts)
  
  return(result)
  
}


#' Generates REDCap Survey Data
#'
#' Returns a data frame with contact info, completion status, access codes etc. for every ID for a given survey
#'
#' @param token Unique REDCap token ID
#' @param form Name of the survey to be downloaded
#' @param event Event name e.g. prenatal_surveys_arm_1
#' @return Data frame with record_ids and survey information for particular form
#' @export
#'
get_survey_data <- function(token, form=form, event=event) {
  
  formData <- list("token" = token,
                   content='participantList',
                   format = 'csv',
                   instrument = form,
                   event = event,
                   returnFormat='csv')
  response <- httr::POST(url, body = formData, encode="form")
  df <- httr::content(response)
  df <- dplyr::filter(df, !is.na(record))
  df <- dplyr::rename(df, record_id = record)
  df <- dplyr::select(df, record_id, email, invitation_sent_status:survey_queue_link)
  return(df)
}

#' This function returns a dataframe with infant dob, date, and age for a specified event
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap_event_name for the timepoint you wish to pull
#' @return dataframe with visit data for that timepoint
#' @export
get_visit_data <- function(token, timepoint = 4) {
  
  dobs <- get_orca_field(token, field='child_dob') %>%
    select(-redcap_event_name)
  
  if (timepoint == 4) {
    data <- get_orca_data(token, form="visit_notes_4m", form_complete = F)
    data <- data %>%
      filter(!str_detect(record_id, 'P') & !str_detect(record_id, 'p')) %>%
      filter(redcap_event_name == 'orca_4month_arm_1' & !is.na(visit_date_4m))
    
    data <- data %>%
      left_join(dobs, by='record_id') %>%
      select(record_id, child_dob, visit_date_4m, child_age_4m)
    return(data)
  } else if (timepoint == 8){
    data <- get_orca_data(token, form="visit_notes_8m", form_complete = F)
    data <- data %>%
      filter(!str_detect(record_id, 'P') & !str_detect(record_id, 'p')) %>%
      filter(redcap_event_name == 'orca_8month_arm_1' & !is.na(visit_date_8m))
    
    data <- data %>%
      left_join(dobs, by='record_id') %>%
      select(record_id, child_dob, visit_date_8m, child_age_8m)
    return(data)
  } else if (timepoint == 12) {
    data <- get_orca_data(token, form="visit_notes_12m", form_complete = F)
    data <- data %>%
      filter(!str_detect(record_id, 'P') & !str_detect(record_id, 'p')) %>%
      filter(redcap_event_name == 'orca_12month_arm_1' & !is.na(visit_date_12m))
    
    data <- data %>%
      left_join(dobs, by='record_id') %>%
      select(record_id, child_dob, visit_date_12m, child_age_12m)
    return(data)
  }
}

#' Provides timepoint completion info for every timepoint
#'
#' @param token Unique REDCap token ID
#' @return dataframe with each ID, and a column for each timepoint with date of completion
#' @export
get_all_timepoints <- function(token) {
  date4 <- get_orca_field(token, field='visit_date_4m')
  date8 <- get_orca_field(token, field='visit_date_8m')
  date12 <- get_orca_field(token, field = "visit_date_12m")
  
  dates <- date4 %>%
    full_join(date8, by='record_id') %>%
    full_join(date12, by='record_id')
  
  dates <- dates %>%
    select(record_id, visit_date_4m, visit_date_8m, visit_date_12m) %>%
    filter(!str_detect(record_id, 'P') & !str_detect(record_id,'p'))
  
  return(dates)
  
}


#' Provides timepoint completion info for every timepoint
#'
#' @param token Unique REDCap token ID
#' @return A list with tracker (a df with every kit that needs to be shipped, and the date it needs to be shipped by) and counts (number of kits to be shipped each day)
#' @export
get_shipping_timetable <- function(token) {
  library(tidyverse)
  kits_4m <- get_orca_report(token, report_code = '2666')
  kits_8m <- get_orca_report(token, report_code = '3240')
  kits_12m <- get_orca_report(token, report_code='3823')
  
  
  #4m
  kits_4m <- kits_4m %>%
    mutate(shipby_date = as.Date(rep(NA, nrow(kits_4m))))
  
  for (x in 1:nrow(kits_4m)) {
    if(!is.na(kits_4m$est_shipping_time[x])) {
      shipby_date = kits_4m$visit_scheduled_date_4m[x] - (kits_4m$est_shipping_time[x] + 1)
      
    } else{
      shipby_date = kits_4m$visit_scheduled_date_4m[x] - 8
    }
    
    
    
    if (!is.na(shipby_date)) {
      
      shipby_day = as.character(wday(shipby_date, label=T))
      days_between = seq(shipby_date, kits_4m$visit_scheduled_date_4m[x], by='day')
      days_between = as.character(wday(days_between, label=TRUE))
      
      if (shipby_day == 'Sun') {
        shipby_date = shipby_date - 2
      } else if ('Sun' %in% days_between | shipby_day == 'Sat') {
        shipby_date = shipby_date - 1
      }
      
      kits_4m$shipby_date[x] = shipby_date
    }
    
  }
  
  kits_8m <- kits_8m %>%
    mutate(shipby_date = as.Date(rep(NA, nrow(kits_8m))))
  
  for (x in 1:nrow(kits_8m)) {
    if(!is.na(kits_8m$est_shipping_time[x])) {
      shipby_date = kits_8m$visit_scheduled_date_8m[x] - (kits_8m$est_shipping_time[x] + 1)
    } else {
      shipby_date = kits_8m$visit_scheduled_date_8m[x] - 8
    }
    
    if (!is.na(shipby_date)) {
      
      shipby_day = as.character(wday(shipby_date, label=T))
      days_between = seq(shipby_date, kits_8m$visit_scheduled_date_8m[x], by='day')
      days_between = as.character(wday(days_between, label=TRUE))
      
      if (shipby_day == 'Sun') {
        shipby_date = shipby_date - 2
      } else if ('Sun' %in% days_between | shipby_day == 'Sat') {
        shipby_date = shipby_date - 1
      }
      
      kits_8m$shipby_date[x] = shipby_date
    }
    
  }
  
  kits_12m <- kits_12m %>%
    mutate(shipby_date = as.Date(rep(NA, nrow(kits_12m))))
  
  for (x in 1:nrow(kits_12m)) {
    
    if(!is.na(kits_12m$est_shipping_time[x])) {
      shipby_date = kits_12m$visit_scheduled_date_12m[x] - (kits_12m$est_shipping_time[x] + 1)
    } else {
      shipby_date = kits_12m$visit_scheduled_date_12m[x] - 8
    }
    
    if (!is.na(shipby_date)) {
      
      shipby_day = as.character(wday(shipby_date, label=T))
      days_between = seq(shipby_date, kits_12m$visit_scheduled_date_12m[x], by='day')
      days_between = as.character(wday(days_between, label=TRUE))
      
      if (shipby_day == 'Sun') {
        shipby_date = shipby_date - 2
      } else if ('Sun' %in% days_between | shipby_day == 'Sat') {
        shipby_date = shipby_date - 1
      }
      
      kits_12m$shipby_date[x] = shipby_date
    } 
    
  }
  
  kits_4m <- kits_4m %>%
    select(record_id, redcap_event_name, visit_scheduled_date_4m, est_shipping_time, shipby_date, visit_scheduled_exp_4m) %>%
    rename(visit_scheduled_date = visit_scheduled_date_4m, exp = visit_scheduled_exp_4m)
  
  kits_8m <- kits_8m %>%
    select(record_id, redcap_event_name, visit_scheduled_date_8m, est_shipping_time, shipby_date, visit_scheduled_exp_8m) %>%
    rename(visit_scheduled_date = visit_scheduled_date_8m, exp = visit_scheduled_exp_8m)
  
  kits_12m <- kits_12m %>%
    select(record_id, redcap_event_name, visit_scheduled_date_12m, est_shipping_time, shipby_date, visit_scheduled_exp_12m) %>%
    rename(visit_scheduled_date = visit_scheduled_date_12m, exp = visit_scheduled_exp_12m)
  
  shipping_tracker <- kits_4m %>%
    full_join(kits_8m) %>%
    full_join(kits_12m)
  
  shipping_tracker <- shipping_tracker %>%
    arrange(shipby_date) 
  
  counts = data.frame(table(shipping_tracker$shipby_date)) %>%
    rename(date = Var1, freq = Freq)
  
  x <- list(tracker=shipping_tracker, counts=counts)
  return(x)
}

#' Pulling a redcap report
#'
#' @param token The API token for the project
#' @param report_code the character report code for the report you wish to pull
#' @param raw_v_label The label for raw data fields
#' @param checkbox_label whether to combine checkbox fields
#' @return A dataframe with the report 
#' @export
get_orca_report <- function(token, report_code, raw_or_label = 'raw', checkbox_label = 'false') {
  formData <- list("token"=token,
                   content='report',
                   format='csv',
                   report_id=as.character(report_code),
                   csvDelimiter='',
                   rawOrLabel=raw_or_label,
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel=checkbox_label,
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)
  
}


#' @title Process DERS Data
#' @description This function will download and return the sum scores for the DIERS-16
#' @param token Unique REDCap token ID
#' @param timepoint REDCap event name (character) default is orca_4month_arm_1
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_orca_ders <- function(token, timepoint = 'orca_4month_arm_1', timestamp=T) {
  library(tidyverse)
  ders <- get_orca_data(token, form = 'difficulties_in_emotional_regulation_16')
  ders <- ders %>%
    filter(redcap_event_name == timepoint)
  
  ders$non_accept <- rowSums(ders[, c('ders_9', 'ders_10', 'ders_13')], na.rm=T)
  ders$goals <- rowSums(ders[, c('ders_3', 'ders_7', 'ders_15')], na.rm=T)
  ders$impulse <- rowSums(ders[, c('ders_4', 'ders_8', 'ders_11')], na.rm=T)
  ders$strategies <- rowSums(ders[, c('ders_5', 'ders_6', 'ders_12', 'ders_14', 'ders_16')], na.rm=T)
  ders$clarity <- rowSums(ders[, c('ders_1', 'ders_2')], na.rm=T)
  
  if (timestamp) {
    ders = ders[,c("record_id", "difficulties_in_emotional_regulation_16_timestamp", 'non_accept', 'goals', 'impulse', 'strategies', 'clarity')]
  } else if (!timestamp) {
    ders = ders[,c("record_id", 'non_accept', 'goals', 'impulse', 'strategies', 'clarity')]
  }
  
  return(ders)
}

#' @title Process BITSEA Data
#' @description This function will download and return the sum scores for the bitsea subscales
#' @param token Unique REDCap token ID
#' @param timestamp whether to include survey timestamp. Default is true
#' @return A data frame for the completed surveys
#' @export
get_orca_bitsea <- function(token, timestamp=T) {
  library(stringr)
  bitsea = get_orca_data(token, "brief_infant_toddler_social_and_emotional_assessme")
  bitsea$record_id = str_remove(bitsea$record_id, "^0+")
  bitsea[bitsea == -888] = NA
  bitsea[bitsea == 999] = NA
  
  bitsea$autism_competence  = rowSums(bitsea[,c("bitsea_1", "bitsea_10", "bitsea_13", "bitsea_15",
                                                "bitsea_22", "bitsea_25", "bitsea_29", "bitsea_31")], na.rm=T)
  bitsea$autism_problems = rowSums(bitsea[,c("bitsea_9", "bitsea_14", "bitsea_21", "bitsea_35",
                                             "bitsea_36", "bitsea_37", "bitsea_38", "bitsea_39",
                                             "bitsea_40")], na.rm=T)
  bitsea$autism_total = bitsea$autism_problems - bitsea$autism_competence
  bitsea$problems = rowSums(bitsea[,c("bitsea_2", "bitsea_3", "bitsea_4", "bitsea_6", "bitsea_7", "bitsea_8",
                                      "bitsea_9", "bitsea_11", "bitsea_12", "bitsea_14", "bitsea_16", "bitsea_17",
                                      "bitsea_18", "bitsea_21", "bitsea_23", "bitsea_24", "bitsea_26", "bitsea_27",
                                      "bitsea_28", "bitsea_30", "bitsea_32", "bitsea_33", "bitsea_34", "bitsea_35",
                                      "bitsea_36", "bitsea_37", "bitsea_38", "bitsea_39", "bitsea_40",
                                      "bitsea_41", "bitsea_42")], na.rm=T)
  bitsea$competence = rowSums(bitsea[,c("bitsea_1", "bitsea_5", "bitsea_10", "bitsea_13", "bitsea_15",
                                        "bitsea_19", "bitsea_20", "bitsea_22", "bitsea_25", "bitsea_29", "bitsea_31")], na.rm=T)
  
  if (timestamp) {
    bitsea = bitsea[,c("record_id", "brief_infant_toddler_social_and_emotional_assessme_timestamp","autism_competence", "autism_problems", "autism_total", "competence", "problems")]
  } else if (!timestamp) {
    bitsea = bitsea[,c("record_id","autism_competence", "autism_problems", "autism_total", "competence", "problems")]
  }
  bitsea$problem_thres = 0
  bitsea[bitsea$problems > 11,"problem_thres"] = 1
  bitsea$competence_thres = 0
  bitsea[bitsea$competence < 13,"competence_thres"] = 1
  bitsea$autism_total_thresh = 0
  bitsea[bitsea$autism_total > 7,"autism_total_thresh"] = 1
  bitsea$autism_comp_thresh = 0
  bitsea[bitsea$competence < 12,"autism_comp_thresh"] = 1
  bitsea$autism_prob_thresh = 0
  bitsea[bitsea$autism_problems > 4,"autism_prob_thresh"] = 1
  
  
  return(bitsea)
  
}

#' @title Pulling DOBs For Prenatal Participants
#' @description This function pulls all possible due date/dob update variables and returns most up to date
#' @param token Unique REDCap token ID
#' @return A data frame with final dob and whether this is confirmed after birth or is just the due date
#' @export
get_prenatal_dob <- function(token) {
  library(dplyr)
  
  dd <- get_orca_field(token, field='rec_due_date') %>%
    select(-redcap_event_name)
  bu1 <- get_orca_field(token, field='child_dob_update')%>%
    select(-redcap_event_name)
  bu2 <- get_orca_field(token, field='child_dob_update_fup')%>%
    select(-redcap_event_name)
  
  
  dobs <- dd %>%
    full_join(bu1) %>%
    full_join(bu2)
  
  dobs <- dobs %>%
    distinct(record_id, .keep_all = T)
  
  dobs <- dobs %>%
    mutate(child_dob_final = ifelse(!is.na(child_dob_update), child_dob_update,
                                    ifelse(is.na(child_dob_update) & !is.na(child_dob_update_fup), child_dob_update_fup,
                                           ifelse(is.na(child_dob_update) & is.na(child_dob_update_fup), rec_due_date, NA)))) %>%
    mutate(dob_confirmed = ifelse(!is.na(child_dob_update) | !is.na(child_dob_update_fup), 1, 0))
  
  dobs$child_dob_final <- as.Date(dobs$child_dob_final, origin='1970-01-01')
  
  dobs <- dobs %>%
    select(record_id, child_dob_final, dob_confirmed)
  
  return(dobs)
}

#' @title Importing MICE participant to 4m project
#' @description Pulls screener data for a mice id, reformats and imports it to the ORCA 2.0 redcap project
#' @param scr_token Unique REDCap token ID for the New ORCA Recruitment Screener project
#' @param orca_token Unique REDCap token ID for the ORCA 2.0 project
#' @param mice_record_id String variable for the MICE id e.g. 'mc_018'
#' @return User input for approval to import (respond y)
#' @export
import_mice_to_orca <- function(scr_token, orca_token, mice_record_id) {
  mice_ids <- get_orca_field(scr_token, field='mice_id')
  
  mice_ids <- mice_ids %>%
    filter(mice_id == mice_record_id)
  
  sri = mice_ids$record_id[1]
  
  prenatal_surveys = get_orca_field(scr_token, field='prams_complete') %>%
    filter(record_id == sri)
  
  pch_complete = get_orca_field(scr_token, field='pch_enroll_complete') %>%
    filter(record_id == sri)
  
  blood_returned = get_orca_field(scr_token, field='mice_returned') %>%
    filter(record_id == sri)
  
  mc_consent <- get_orca_field(scr_token, field='mice_consent_date') %>%
    filter(record_id == sri)
  
  prenatal <- ifelse(nrow(prenatal_surveys) == 1 & prenatal_surveys$prams_complete[1] == 2, 1, 0)
  blood <- as.Date(ifelse(nrow(blood_returned) == 1, blood_returned$mice_returned[1], NA))
  blood_valid <- ifelse(as.Date(mc_consent$mice_consent_date[1]) >= as.Date('2024-11-01'), 1, 0)
  
  import_file <- data.frame(record_id = mice_record_id,
                            redcap_event_name = 'mice_participant_m_arm_4',
                            screener_record_id = sri,
                            prenatal_yesno = prenatal,
                            mc_blood_date = blood,
                            mc_blood_valid = blood_valid)
  
  if (nrow(pch_complete) == 1) {
    peach_pp <- get_orca_field(token='B17171C96E09FE75231D605CBCE4B249', field='screener_record_id')
    peach_pp <- peach_pp %>%
      filter(screener_record_id == sri)
    
    pch_id = peach_pp$record_id[1]
    
    import_file$pch_id = pch_id
    
  }
  
  import_data(orca_token, import_file)
}

#' @title Get ORCA Screener Clean
#' @description Pulls new screener data, determines eligibility for enrollment
#' @param token Unique REDCap token ID for the New ORCA Recruitment Screener project
#' @param min_date_time The minimum timestamp to pull responses after
#' @return A list containing data (screener to enroll), orca15 (potential reef people), prenatal_counts and postnatal_counts
#' @export
get_orca_screener_clean <- function(token, min_date_time = '2022-01-01 00:00:00') {
  suppressMessages(suppressWarnings({
    cat(
      '\n\u2728 Screener Pull Summary \u2728',
      '\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
      '\nThis script will pull screener responses after: ', min_date_time, '\n - check this is after the last row on the excel screener',
      '\n\n\U0001F7E2 ELIGIBILITY CRITERIA:',
      '\n  \u2022 Any prenatal participant < 31 weeks gestational age',
      '\n  \u2022 Any postpartum participant or prenatal > 31 weeks GA',
      '\n    who is HIGH PRIORITY or has been REFERRED to the study',
      '\n\n\U0001F534 REMOVAL CRITERIA:',
      '\n  \u2022 Filled out screener multiple times',
      '\n  \u2022 Incorrect DOB or due date (e.g., due date in past',
      '\n  \u2022 Child too old for any study',
      '\n  \u2022 Failed screener attention checks',
      '\n  \u2022 Has been referred by a flagged email',
      '\n\n\U0001F5C2 OUTPUT LIST:',
      sprintf('\n  \u2022 %-20s %s', 'screener:', 'rows to paste into Excel screener'),
      sprintf('\n  \u2022 %-20s %s', 'orca15:', 'participants who may live in NYC'),
      sprintf('\n  \u2022 %-20s %s', 'prenatal_counts:', 'enrolled + new + future invites'),
      sprintf('\n  \u2022 %-20s %s', 'postnatal_counts:', 'enrolled + future 4m invites'),
      sprintf('\n  \u2022 %-20s %s', 'ineligible_responses:', 'removed rows with reasons'),
      '\n\n⚠️  To use different criteria, run the "Screener Pull RAW" script instead.',
      
      '\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n'
    )
    
    response <- readline(prompt = cat('Enter y to run function, enter any other key to abort: '))
    
    if (response == 'y') {
      
      cat(
        '\n\u2728 Starting Screener Pull \u2728',
        '\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
        '\n\n Step 1: Checking package installation: '
      )
      
      
      packages <- c("tidyverse", "zoo", 'zipcodeR','maps')
      to_install <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
      
      if (length(to_install) > 0) {
        cat('installing the following packages: ', packages)
        install.packages(to_install)
      } else {
        cat('all packages installed')
      }
      
      
      library(tidyverse)
      library(zipcodeR)
      library(maps)
      library(zoo)
      
      #STEP 1: Pulling and cleaning screener ----
      
      cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
          '\nStep 2: Downloading new screener responses...')
      
      
      screener <- get_orca_screener(token, min_date_time = min_date_time)
      
      if (nrow(screener) >= 1) {
        cat(nrow(screener), 'responses returned, starting cleaning')
        #Screening out already enrolled participants - will leave only new responses that haven't been checked
        
        contact_log <- get_orca_data(token, form='contact_log', form_complete = F) %>%
          filter(!is.na(contact_log_complete))
        
        contact_log <- contact_log %>%
          filter(is.na(orca_contact_yesno)) %>%
          select(record_id) %>%
          mutate(new=1)
        
        screener <- screener %>%
          left_join(contact_log, by='record_id') %>%
          filter(new == 1) %>%
          select(-new)
        
        
        
        #STEP 2: Extracting potential orca 1.5/ reef participants----
        cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
            '\nStep 3: Extracting potential reef/1.5 participants')
        screener <- screener %>%
          mutate(orca15 = ifelse(str_detect(location, 'New York') & child_dob <= Sys.Date() & child_dob > (Sys.Date() - 365), 1, 0))
        orca15 <- screener %>%
          filter(orca15 == 1)
        screener <- screener %>%
          filter(orca15 == 0 | is.na(orca15))
        
        cat('\n', nrow(orca15), ' potential reef participants extracted')
        
        #STEP 3: Calculating gestational age and prenatal eligibility
        cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
            '\nStep 4: Calculating prenatal eligibility')
        
        screener$weeks_till_birth <- difftime(screener$due_date, Sys.Date(),units='w')
        screener$gestational_age <- 40 - screener$weeks_till_birth
        
        screener <- screener %>%
          mutate(prenatal_eligible = ifelse(!is.na(due_date) & gestational_age < 31, 1, 0))
        
        screener <- screener %>%
          mutate(prenatal_invite_date = as.Date(ifelse(prenatal_eligible == 1 & gestational_age >= 28, Sys.Date(),
                                                       ifelse(prenatal_eligible == 1 & gestational_age < 28, due_date-84, NA)), origin='1970-01-01'),
                 prenatal_invite_month = format(prenatal_invite_date, '%b %y'))
        
        
        cat('\n',nrow(subset(screener, prenatal_eligible == 1)), ' initial eligible prenatal responses')
        #STEP 4: Determining postnatal eligibility
        cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
            '\nStep 5: Calculating postnatal eligibility')
        screener$current_age = as.numeric(difftime(Sys.Date(), screener$child_dob, unit='days'))
        
        screener <- screener %>%
          mutate(postnatal_eligible = ifelse(current_age < 137 | (gestational_age >= 31 & gestational_age < 56) & (priority == 'High Priority' | !is.na(rec_snowball_email)), 1, 0),
                 postnatal_eligible = ifelse(is.na(postnatal_eligible), 0, postnatal_eligible))
        
        
        cat('\n',nrow(subset(screener, postnatal_eligible == 1)), ' initial eligible postnatal responses')
        
        #STEP 5: REMOVING INELIGIBLE RESPONSES 
        cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
            '\nStep 6: Removing ineligible responses & Checking Final Numbers')
        
        screener_checked <- screen_fraudulence(screener)
        screened <- screener_checked$data
        
        ineligible <- screened %>%
          filter(prenatal_eligible == 0 & postnatal_eligible == 0) %>%
          mutate(failed_inclusion_checks = 1)
        
        screened <- screened %>%
          filter(prenatal_eligible == 1 | postnatal_eligible == 1)
        
        removed <- screener_checked$ineligible_ages %>%
          full_join(screener_checked$duplicate_contacts) %>%
          full_join(screener_checked$impossible_due_dates) %>%
          full_join(screener_checked$flagged_referral) %>%
          full_join(screener_checked$failed_attention_checks) %>%
          full_join(ineligible)
        
        removed <- removed %>%
          distinct(record_id, .keep_all = T)
        
        cat('\n',nrow(removed), 'removed for ineligibility')
        
        if (nrow(subset(screened, prenatal_eligible == 1)) >=1) {
          cat(
            '\n\n\u2728 Numbers of eligible prenatal participants \u2728',
            '\n\n\u2022 Total: ', nrow(subset(screened, prenatal_eligible == 1)),
            '\n\u2022 T1: ', nrow(subset(screened, gestational_age > 0 & gestational_age < 13)),
            '\n\u2022 T2: ', nrow(subset(screened, gestational_age >= 13 & gestational_age < 28)),
            '\n\u2022 T3: ', nrow(subset(screened, gestational_age >= 28 & gestational_age < 31))
          )
        } else {
          cat('\nNo eligible prenatal participants')
        }
        
        if (nrow(subset(screened, postnatal_eligible == 1)) >=1) {
          cat(
            '\n\n\u2728 Numbers of eligible postnatal participants \u2728',
            '\n\u2022 Total: ', nrow(subset(screened, postnatal_eligible == 1)),
            '\n\u2022 0-3.5m: ', nrow(subset(screened, postnatal_eligible == 1 & ((current_age > 0 & current_age < 107) | (gestational_age >= 31 & gestational_age < 55)))),
            '\n\u2022 3.5-4.5m (invited immediately): ', nrow(subset(screened, postnatal_eligible == 1 & (current_age >= 107 | gestational_age >= 55)))
          )
        } else {
          cat('\nNo eligible postnatal participants')
        }
        
        
        #CREATING COUNTS DF
        #PRENATAL
        cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
            '\nStep 7: Creating final counts by month datasets')
        
        suppressMessages(suppressWarnings({
          new_prenatal_counts <- data.frame(table(subset(screened, prenatal_eligible == 1)$prenatal_invite_month)) %>%
            rename(new = Freq, month = Var1) %>%
            mutate(month = as.yearmon(month, "%b %y")) %>%
            arrange(month)
          
          current_prenatal_counts <- get_expected_invites(token, timepoint='prenatal')$counts%>%
            rename(existing = total)
          
          prenatal_counts <- current_prenatal_counts %>%
            full_join(new_prenatal_counts, by='month')
          prenatal_counts$total <- rowSums(prenatal_counts[, c('existing', 'new')], na.rm=T)
          
          #POSTNATAL
          screened <- screened %>%
            mutate(expected_invite_month = format(expected_invite_date, '%b %y'))
          
          current_postnatal_counts <- get_expected_invites(token, timepoint='4m')$counts
          new_postnatal_counts <- data.frame(table(screened$expected_invite_month)) %>%
            rename(new = Freq, month = Var1) %>%
            mutate(month = as.yearmon(month, "%b %y")) %>%
            arrange(month)
          
          postnatal_counts <- full_join(current_postnatal_counts, new_postnatal_counts, by='month') 
          postnatal_counts$total <- rowSums(postnatal_counts[, c('orca_total', 'mice_total', 'new')], na.rm=T)
          
        }))
        
        
        
        #SAVING LIST
        cat('\n\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500',
            '\nStep 8: Cleaning dataset and building list')
        screened <- screened %>%
          select(-rec_snowball_email, -orca15:-expected_invite_month)
        
        result <- list(screener = screened, orca15 = orca15, prenatal_counts = prenatal_counts, postnatal_counts = postnatal_counts,
                       ineligible_responses = removed)
        
        cat('\n','Screener pull complete')
        return (result)
        
      } else {
        cat('\n','No new screener responses! No list returned')
      }
      
      
    } else {
      print('Recruitment pull aborted')
      return(NA)
    }
    
  }))
  
} 

#IN PROGRESS
get_response_rates <- function(screener_token, orca_token) {
  #to do' 
  #complete a prenatal call out of prenatal invites
  #consent to mice out of prenatal invites
  #consent to orca out of prenatal invites
  #complete peach out of prenatal invites? or out of people who show interest in peach at prenatal call?
  #complete 4m out of mice participants
  #complete 4m out of people who did only prenatal surveys
  #complete 8m out of people who did 4m 
  #complete 12m out of people who did 4m (or 4m and 8m?)
}