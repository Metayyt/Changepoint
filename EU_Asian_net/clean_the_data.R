temp = unique(nation['statename'])
if (!require(countrycode)) install.packages("countrycode")

library(countrycode)

add_continent <- function(df, country_column) {
  #reset index
  rownames(df) <- NULL
  # 根据国家名称获取洲信息
  df$continent <- countrycode(sourcevar = df[[country_column]], 
                              origin = "country.name", 
                              destination = "continent")
  return(df)
}
country_continent = add_continent(temp, 'statename')
country_continent[169, 'continent'] = 'Not Existed during time' 
country_continent[154, 'continent'] = 'Asia'


### Asia country from country_continent
asia_countrys = country_continent[country_continent$continent == 'Asia', 'statename']
Europe_countrys = country_continent[country_continent$continent == 'Europe', 'statename']

####### Clean the data to get only asia data and only euro data seperately
asia_data = nation[nation$statename %in% asia_countrys,]
euro_data = nation[nation$statename %in% Europe_countrys,]
# save to csv file
write.csv(asia_data, 'july31/COW_Trade_4.0/COW_Trade_4.0/National_Asia_result.csv')
write.csv(euro_data, 'july31/COW_Trade_4.0/COW_Trade_4.0/National_Europe_result.csv')

####### Clean dyadic data, it has two col, importer1 and importer2, we need to check if they are all in the asia country list
asia_dyadic = dyadic[dyadic$importer1 %in% asia_countrys & dyadic$importer2 %in% asia_countrys,]
euro_dyadic = dyadic[dyadic$importer1 %in% Europe_countrys & dyadic$importer2 %in% Europe_countrys,]
# save to csv file
write.csv(asia_dyadic, 'july31/COW_Trade_4.0/COW_Trade_4.0/Dyadic_Asia_result.csv')
write.csv(euro_dyadic, 'july31/COW_Trade_4.0/COW_Trade_4.0/Dyadic_Europe_result.csv')