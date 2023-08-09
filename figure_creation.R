library(ggplot2)
library(gghalves)

data_path = "C:\\Users\\u251639\\Downloads\\Dated GSR Data.csv"
prepare_data = function(path) {
  data = read.csv(path)
  data$Domain.Type = factor(data$Domain.Type)
  data$DateCreated = as.Date(data$DateCreated)
  data$DateUpdated = as.Date(data$DateUpdated)
  return(data)
}
gsrdata = prepare_data(data_path)

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
factor_domains()

citations_over_time = function() {
  ggplot(data = gsrdata, mapping = aes(x= DateCreated, y = Citations)) +
    geom_smooth(method = "loess", formula = y~x) +
    geom_point() + 
    labs(title = "Citations over Time", x = "Date Created", y = "Number of Citations") +
    scale_y_log10()
}

citations_by_number_of_certifications = function() {
  ggplot(data = gsrdata, mapping = aes(x = factor(Checks), y = Citations, group = Checks)) + 
    geom_boxplot() +
    labs(x = "Number of Certifications", y = "Citations, logarithmic scale", title = "Citations by Number of Certifications") +
    scale_y_continuous(trans = "log10") +
    annotate("text", label = "ms", x = 3, y = 1400, size = 3.4) +
    annotate("text", label = "Genomic Variant Simulator", x = 3, y = 3700, size = 3.4) +
    annotate("text", label = "GCTA", x = 4, y = 4000, size = 3.2) +
    annotate("text", label = "Vortex", x = 4, y = 1100, size = 3.4) +
    annotate("text", label = "BOTTLENECK", x = 2, y = 960, size = 3.2) +
    annotate("text", label = "ART", x = 5, y = 750, size = 3.2)
}

certifcations_pre_and_post_gsr = function () {
  mfrow = par(1,2)
  gd = gsrdata$Checks[gsrdata$Year <= 2013]
  pie(table(gsrdata$Checks), main = "Certificates of All Packages")
  pie(table(gd), main = "Certificates of All Packages last updated before the creation of the GSR")
  mfrow = par(1,1)
}

#unused, not good for combining
create_mosaicplot() {
  #install.packages("ggmosaic")
  library("ggmosaic")
  gsrdata$Sup = factor(gsrdata$Sup, levels = c(1,0), labels = c("Yes","No"))
  ggplot(data = gsrdata) +
    geom_mosaic(mapping = aes(x = Year, fill = Sup), divider = mosaic())
}

#################Add Legends ASAP
    combined_without_confidence = function () {
  ggplot(data = gsrdata) +
    geom_smooth(mapping = aes(x = DateCreated, y = Sup), color = "forestgreen",se = FALSE) +
    geom_smooth(mapping = aes(x = DateCreated, y = Doc), color = "royalblue",se = FALSE) +
    geom_smooth(mapping = aes(x = DateCreated, y = App), color = "orange", se = FALSE) +
    geom_smooth(mapping = aes(x = DateCreated, y = Acc), color = "red", se = FALSE) +
    geom_smooth(mapping = aes(x = DateCreated, y = domain_nums), color = "green", se = FALSE) +
    labs(title = "Without confidence intervals")
    }
        
    combined_with_confidence = function() {
      ggplot(data = gsrdata) +
        geom_smooth(mapping = aes(x = DateCreated, y = Sup)) +
        geom_smooth(mapping = aes(x = DateCreated, y = Doc, color = "orange")) +
        geom_smooth(mapping = aes(x = DateCreated, y = App, color = "red")) +
        geom_smooth(mapping = aes(x = DateCreated, y = Acc, color = "pink")) + 
        geom_smooth(mapping = aes(x = DateCreated, y = domain_nums), color = "green") +
        labs(title = "Percent certifications over time")
    }

average_citations_yearly() {
  ggplot(data = gsrdata, mapping = aes(y = AverageCitations)) +
    geom_half_boxplot() +
    geom_half_dotplot() +
    scale_y_log10() +
    labs(title = "Average Citations per Year, Logarithmic Scale")
}
    