library(ggplot2)
library(gghalves)

source("data_collection.R")

data_path = "C:\\Users\\u251639\\Downloads\\Dated GSR Data.csv"
prepare_data = function(path) {
  data = read.csv(path)
  data$Domain.Type = factor(data$Domain.Type)
  data$DateCreated = as.Date(data$DateCreated)
  data$DateUpdated = as.Date(data$DateUpdated)
  return(data)
}
gsrdata = prepare_data(data_path)

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
  par(mfrow = c(1,2))
  gd = gsrdata$Checks[gsrdata$Year <= 2013]
  pie(table(gsrdata$Checks), main = "Certificates of All Packages")
  pie(table(gd), main = "Certificates of All Packages last updated\nbefore the creation of the GSR")
  par(mfrow = c(1,1))
}

combined = function (with_pre, year, with_confidence) {
  yearlies = get_other_yearlies()
  gdata = if (with_pre) gsrdata else subset(gsrdata,gsrdata$Year >= year)
  per_year = if (with_pre) yearlies else subset(yearlies,gsrdata$Year >= year)
  colors = c("Support" = "#56B4E9", "Documentation" = "#D55E00", "Application" = "#E69F00", 
              "Accessability" = "#F5C710", "Domain Type" = "#0072B2","Average\nCitations" = "#009E73")
   ggplot(data = gdata, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Sup, color = "Support"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Doc, color = "Documentation"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = App, color = "Application"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Acc, color = "Accessability"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = per_year$domain_type, color = "Domain Type")
                ,se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = per_year$average_citation, color = "Average\nCitations")
                ,se = with_confidence, linewidth = 1.3) +
    labs(title = "Attributes of Packages Over Time") +
     scale_color_manual(values = colors)
}

average_citations_yearly = function() {
  ggplot(data = gsrdata, mapping = aes(y = AverageCitations)) +
    geom_half_boxplot() +
    geom_half_dotplot() +
    scale_y_log10() +
    labs(title = "Average Citations per Year, Logarithmic Scale")
}
    