data = read.csv('dataset/data.csv')

datanames <- names(data)
datanames
# i want to check all names that contain "280"
event_names <- grep("280", datanames, value=TRUE)
# remove '280' in event_names
event_names <- gsub("280", "", event_names)
event_names


selected_data <- data[, c("CASEID", "STATUS", "EDUCA",'college','white','age',"RACE","hivVisit300" )]#
selected_data
