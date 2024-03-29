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
    record_filter = ""
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
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "TEST"))
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
                   returnFormat='csv')
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA
  df <- dplyr::select(df, record_id, redcap_event_name, field)
  df <- df[!is.na(df[[field]]),]
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "TEST"))
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


#' @title Pulling cleaned screener responses 
#' @description this function pulls and cleans screener data for import to excel tracker 
#' @param token Unique REDCap API token
#' @param min_date_time The minimum timestamp to pull responses after
#' @return a dataframe with the race responses cleaned
#' @export
get_orca_screener <- function(token, min_date_time = "2022-01-01 00:00:00") {
  library(tidyverse)
  screener = get_orca_data(token, "orca_screener_survey")
  screener = select(screener, -redcap_survey_identifier, -rec_unique_record_id, -orca_screener_survey_complete)
  screener = rename(screener, 
                    screener_record_id = record_id,
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
  
  screener <- filter(screener, bot_check == 3)
  col_order <- c("screener_record_id", "caregiver_name", "language", "phone", "texting_okay", "email", "over_18", "zipcode", "child_yesno", "child_dob", "pregnant_yesno", "due_date","twin_yesno", "timestamp","timezone", "location", "rec_source", "rural", "non_white", "low_ses", "priority", "expected_invite_date", "bot_check", "bot_pic_answer")
  screener <- screener[, col_order]
  screener <- dplyr::arrange(screener, timestamp)
  return (screener)
}


#' @title Duplicate contacts
#' @description Flags any participant with a duplicate phone or email
#' @param data The data frame you wish to act on
#' @return A dataframe with 2 new columns marking 1 for anybody with a duplicate phone or email
#' @export
flag_duplicate_contacts <- function(data) {
  library(dplyr)
  data <- data %>%
    mutate(duplicate_email = ifelse(!is.na(email) & duplicated(email) & !duplicated(screener_record_id), 1, 0),
           duplicate_phone = ifelse(!is.na(phone) & duplicated(phone) & !duplicated(screener_record_id), 1, 0))
  return(data)
}

#' @title Lowercase Names
#' @description Flags any participant where their name is all lowercase
#' @param data The data frame you wish to act on
#' @return A dataframe with a new column marking 1 for anybody with an all lowercase names
#' @export
flag_lowercase_names <- function(data) {
  library(dplyr)
  data <- data %>%
    mutate(lower_name_flag = ifelse(!grepl("[[:upper:]]", caregiver_name), 1, 0))
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
    flag_lowercase_names() %>%
    flag_numeric_names() %>%
    flag_due_dates()
  
  #creating new datasets
  ineligible_ages <- data %>%
    filter(age_ineligible == 1)
  duplicate_contacts <- data %>%
    filter(duplicate_email == 1 | duplicate_phone == 1)
  impossible_due_dates <- data %>%
    filter(incorrect_due_date == 1)
  failed_attention_checks <- data %>%
    filter(bot_check != 3 & bot_pic_answer != 4)
  #removing NA names, numeric names, under 18 caregivers, duplicate contact info and age_ineligible babies
  data <- data %>%
    filter(!is.na(caregiver_name) & over_18 == "Yes" & num_name_flag == 0 & age_ineligible == 0 & incorrect_due_date == 0) %>%
    filter(bot_check == 3) %>%
    filter(duplicate_email == 0 & duplicate_phone == 0) %>%
    filter(!is.na(phone) | !is.na(email))
  #removing excess columns other than flagged lowercase names 
  data <- data %>%
    select(-age_ineligible, -duplicate_email, -duplicate_phone, -num_name_flag, -incorrect_due_date, -bot_check, -bot_pic_answer)
  return(list(data = data, ineligible_ages = ineligible_ages, duplicate_contacts = duplicate_contacts, impossible_due_dates = impossible_due_dates, failed_attention_checks = failed_attention_checks))
}

#' @title Pulls US zipcode database
#' @description Loads a data base of US zipcodes and information (latitude, longitude, population)
#' @return A data frame with every us zipcode and information
#' @export
zip_data <- function() {
  data_path <- system.file("data", "zip_code_database.csv", package = "OrcaData")
  data <- read.csv(data_path, na.strings = "")
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
  unique_events <- unique(data$redcap_event_name)
  
  #pulling existing redcap data
  all <- get_all_data(token)
  all <- filter(all, redcap_event_name == unique_events)
  all <- all[, colnames(all) %in% colnames(data)]
  test <- all %>%
    right_join(data, by=c('record_id', 'redcap_event_name'))

  
  #ask user to check data import against existing data
  print(test); cat("Check the dataset above carefully. columns x represent the existing data contents, column y represents the data that will overwrite\n",
                   "If column x contains data, this import will OVERWRITE that existing data\n",
                   "If the cell contents are the same, there is no new data to import")
  
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
    
    if (str_detect(data$child_birth_weight[row], "lbs")) {
      data$child_birth_weight[row] <- gsub("lbs", ",", data$child_birth_weight[row])
    } else if (str_detect(data$child_birth_weight[row], " ") & !str_detect(data$child_birth_weight[row], ",")) {
      data$child_birth_weight[row] <- gsub(" ", ",", data$child_birth_weight[row])
      
    }
  }
  
  
  data <- data %>%
    separate(child_birth_weight, into = c("child_birth_lb", "child_birth_oz"), sep = ",", remove = F)
  
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
  
  for (row in 1:nrow(data)) {
    total_household = (data$children_home[row] + data$adults_home[row])
    
    if (total_household <= 8) {
      index <- which(poverty_guidelines$household_n == total_household)
    } else {
      index = 8
    }
    
    threshold = poverty_guidelines$income_threshold[index]
    data$itn[row] <- data$annual_income[row] / threshold
  }
  
  return(data)
}
