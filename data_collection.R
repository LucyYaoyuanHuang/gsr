library(xml2)
library(rvest)

data_path = "C:\\Users\\u251639\\Downloads\\Dated GSR Data.csv"
prepare_data = function(path) {
  data = read.csv(path)
  data$Domain.Type = factor(data$Domain.Type)
  data$DateCreated = as.Date(data$DateCreated, "%m/%d/%Y")
  data$DateUpdated = as.Date(data$DateUpdated, "%m/%d/%Y")
  return(data)
}
gsrdata = prepare_data(data_path)

# Downloads many pages, will take a while
get_pmc_urls = function() {
  download.file("https://surveillance.cancer.gov/genetic-simulation-resources/packages/","~\\Browse and Search.htm")
  text_html = read_html("~\\Browse and Search.htm")
  html_raw = html_elements(text_html, ".profile-link")
  url_raw = html_attr(html_raw,"href")
    pmcurls = c()
  for (i in 1:225) {
    print(i)
    path = paste("~\\WebpageTemp",i,".htm",sep = "")
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

factor_domains = function() {
  DomainTypes = factor(gsrdata$Domain,
                       levels = c("alfsim","au","bioconductor","bioinform","bioinformatics","bit",
                                  "bitbucket","broadinstitute","case","chkuo","cibiv","cnsgenomics",
                                  "davidebolo1993","duke","ebi","ed","fiu","free","github","gitlab","gmail",
                                  "google","h-its","inra","katja-schiffers","kuleuven","mabs",
                                  "messerlab","molpopgen","nih","noaa","openabm","ox","patrickmeirmans",
                                  "pegase-biosciences","pitt","r-project","ritchielab","rosenberglab",
                                  "sak042","sammeth","scrm","scti","seqan","sjtu","soken","sourceforge",
                                  "splatche","stanford","tau","temple","ua","uchicago","ucl",
                                  "uhnresearch","umass","umich","umkc","uni-bielefeld","uni-hohenheim",
                                  "uni-tuebingen","unibe","unige","unil","unl","uoguelph","upenn",
                                  "uvigo","vanderbilt","washington","yale","yana-safonova"),
                       labels = c("Personal or Institutional","Personal or Institutional","Personal or Institutional","Public Code Repository","Public Code Repository","Public Code Repository",
                                  "Public Code Repository","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Public Code Repository","Public Code Repository","Public Code Repository",
                                  "Public Code Repository","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Public Code Repository","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Public Code Repository","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Public Code Repository","Personal or Institutional","Public Code Repository","Personal or Institutional","Personal or Institutional","Public Code Repository",
                                  "Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Public Code Repository","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional",
                                  "Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional","Personal or Institutional"))
}

get_other_yearlies = function() {
  dn = c()
  ac = c()
  for (i in 1:225) {
    o = ifelse(gsrdata$Domain.Type[i] == "Public Code Repository", 1, 0)
    dn = append(dn,o)
    b = gsrdata$AverageCitations[i] < 1 | is.na(gsrdata$AverageCitations[i])
    u = ifelse(b, 0, 1)
    ac = append(ac, u)
  }
  yearlies = data.frame(domain_type = dn, average_citation = ac)
  return(yearlies)
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

get_need_application_reevaulation = function() {
  test = subset(gsrdata, gsrdata$App == 0 & gsrdata$AverageCitations >1)
  write.csv(test,"C:\\Users\\u251639\\Downloads\\Application_Candidates.csv")
}