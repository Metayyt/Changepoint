#install.packages("countrycode")
library(countrycode)
filter_by_region_and_count_countries <- function(data, country_column, region) {
  data$Continent <- countrycode(sourcevar = data[[country_column]],
                                origin = "country.name",
                                destination = "continent")
  
  valid_regions <- unique(data$Continent)
  if (!(region %in% valid_regions)) {
    stop(paste("All countries:", paste(valid_regions, collapse = ", ")))
  }
  
  filtered_data <- subset(data, Continent == region)
  
  num_countries <- length(unique(filtered_data[[country_column]]))
  
  filtered_data$Continent <- NULL
  
  #print('country counts: ', num_countries)
  print(num_countries)
  return(filtered_data)   
}

Dyadic_Europe_result <- filter_by_region_and_count_countries(Dyadic_COW_4_0, "importer2", "Europe")
write.csv(Dyadic_Europe_result, file = "Dyadic_Europe_result.csv", row.names = FALSE)

Dyadic_Asia_result <- filter_by_region_and_count_countries(Dyadic_COW_4_0, "importer2", "Asia")
write.csv(Dyadic_Asia_result, file = "Dyadic_Asia_result.csv", row.names = FALSE)

National_Europe_result <- filter_by_region_and_count_countries(National_COW_4_0, "statename", "Europe")
write.csv(National_Europe_result, file = "National_Europe_result.csv", row.names = FALSE)

National_Asia_result <- filter_by_region_and_count_countries(National_COW_4_0, "statename", "Asia")
write.csv(National_Asia_result, file = "National_Asia_result.csv", row.names = FALSE)

