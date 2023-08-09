library(xml2)
library(rvest)
library(XML)

data_path = "C:\\Users\\u251639\\Downloads\\Dated GSR Data.csv"
prepare_data = function(path) {
  data = read.csv(path)
  data$Domain.Type = factor(data$Domain.Type)
  data$DateCreated = as.Date(data$DateCreated)
  data$DateUpdated = as.Date(data$DateUpdated)
  return(data)
}
gsrdata = prepare_data(data_path)

#DateCreated and much of DateUpdated obtained through manual input
#Description, Title, Sup, Doc, App, and Acc come from copying off the gsr webpage

#Collects all package links from main GSR package 
#webpage: https://surveillance.cancer.gov/genetic-simulation-resources/packages/
#Requires manual download of said webpage
get_package_urls = function() {
  text_html = read_html("C:\\Users\\u251639\\Documents\\Data\\Browse and Search.htm")
  html_raw = html_elements(text_html, ".profile-link")
  url_raw = html_attr(html_raw,"href")
}

#For every package, the package page is downloaded (unlike other webpages, eg. Wikipedia
#I couldn't get the get the link unless the page was downloaded). Then, the PMC link is scraped
get_pmc_urls = function() {
pmcurls = c()
for (i in 1:225) {
  print(i)
  path = paste("C:\\Users\\u251639\\Documents\\Data\\WebpageTemp",i,".htm",sep = "")
  download_html(url_raw[i],path)
  dlpage = read_html(path)
  htmldig = dlpage %>% html_elements("a") %>% html_attr("href")
  pmc = htmldig[substring(htmldig,nchar(htmldig)-11,nchar(htmldig)) == "?tool=pubmed"]
  pmcurls = append(pmcurls,pmc[2])
}
}

get_year = function() {
  for (i in 1:225) {
    gsrdata$Year[i] = as.numeric(substring(gsrdata$DateCreated[i],0,4))
  }
}

get_domain_nums = function() {
  store = c()  
  for (i in 1:225) {
    o = ifelse(gsrdata$Domain.Type[i] == "Public Code Repository", 1, 0)
    store = append(store,o)
  }
  gsrdata$domain_nums = store
}

get_lifespan() = function() {
  lifespan = as.numeric(gsrdata$DateUpdated)-as.numeric(gsrdata$DateCreated)
  gsrdata$Lifespan = lifespan/365
  write.csv(gsrdata,data_path)
}

get_average_citations = function() {
  current_date = as.Date("2023-8-7")
  years_extant = as.numeric((current_date - gsrdata$DateCreated)/365)
  gsrdata$AverageCitations = gsrdata$Citations/years_extant
}