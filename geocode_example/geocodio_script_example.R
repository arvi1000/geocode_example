### EDIT THESE VARIABLES TO START WITH:

# set environment variable with geocodio api key
Sys.setenv(GEOCODIO_API_KEY = 'YOUR_API_KEY_HERE')
MY_CSV_INPUT_FILE = 'sample_addresses.csv'
MY_CSV_OUTPUT_FILE = 'output1.csv'

# load libraries. install them if absent
if (!require('tidyverse')) install.packages('tidyverse', dependencies = TRUE)
if (!require('httr')) install.packages('tidyverse', dependencies = TRUE)

# geocoding helper function
gio_batch_geocode <- function(addresses) {
  # modified from https://github.com/hrbrmstr/rgeocodio/blob/master/R/geocod-batch.R
  # which is out of date
  
  params <- list(api_key=Sys.getenv('GEOCODIO_API_KEY'))
  params$fields <- 'stateleg-next'
  
  res <- httr::POST("https://api.geocod.io/v1.7/geocode",
                    query=params, body=as.list(addresses), encode="json")
  
  httr::stop_for_status(res)
  
  res <- jsonlite::fromJSON(httr::content(res, as="text", encoding = "UTF-8"),
                            flatten = TRUE)
  
  res <- res$results
  
  new_names <- gsub("\\.", "_", colnames(res))
  new_names <- gsub("response_input|address_components|fields", "", new_names)
  new_names <- gsub("[_]+", "_", new_names)
  new_names <- sub("^_", "", new_names)
  
  res <- set_names(res, new_names)
  
  as_tibble(res)
  
}

# read the address from a csv file. I used random addresses from https://va.postcodebase.com/randomaddress for this
# I recommend you break up your list into chunks of ~1000, because you can only get ~1250 free calls per day per API key
if(!file.exists(MY_CSV_INPUT_FILE)) stop('Did not find the input file you specified')
addr <- read.csv(MY_CSV_INPUT_FILE, stringsAsFactors = FALSE)

# get results from geocod.io
results <- gio_batch_geocode(addr$address)

# extract state house and senate info in the desired format
house_results <- results$response_results %>%
  lapply(function(x) x$fields.state_legislative_districts.house[[1]]) %>% 
  bind_rows %>%
  select(house_name = name, 
         house_district_number = district_number,
         house_is_upcoming_state_legislative_district = is_upcoming_state_legislative_district)
senate_results <- results$response_results %>%
  lapply(function(x) x$fields.state_legislative_districts.senate[[1]]) %>% 
  bind_rows %>%
  select(senate_name = name, 
         senate_district_number = district_number,
         senate_is_upcoming_state_legislative_district = is_upcoming_state_legislative_district)
final_output <- bind_cols(addr, house_results, senate_results)

# write output to file
write.csv(final_output, MY_OUTPUT_FILE, row.names = F)
