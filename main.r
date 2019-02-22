library(mongolite)
m <-  mongo("tickets", url = "mongodb://localhost:27017/agile")
print(m$count("{}"))
print(m$find("{}","{}")$description)