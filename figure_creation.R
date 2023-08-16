library(ggplot2)
library(gghalves)
library(RColorBrewer)

source("data_collection.R")

data_path = "C:\\Users\\u251639\\Downloads\\Dated GSR Data.csv"
prepare_data = function(path) {
  data = read.csv(path)
  data$Domain.Type = factor(data$Domain.Type)
  data$DateCreated = as.Date(data$DateCreated, "%m/%d/%Y")
  data$DateUpdated = as.Date(data$DateUpdated, "%m/%d/%Y")
  return(data)
}
gsrdata = prepare_data(data_path)
 
brewer = brewer.pal(5, "YlGnBu")
cols = c("0" = brewer[1], "1" = brewer[2], "2" = brewer[3], 
         "3" = brewer[4], "4" = brewer[5])

citations_over_time = function() {
  ggplot(data = gsrdata, mapping = aes(x= DateCreated, y = Citations)) +
    geom_smooth(method = "loess", formula = y~x) +
    geom_point() + 
    labs(title = "Citations over Time", x = "Date Created", y = "Number of Citations") +
    scale_y_log10()
}

certificates_over_time = function() {
  ggplot(data = gsrdata, mapping = aes(x= DateCreated, y = Checks)) +
    geom_smooth(method = "loess", formula = y~x) +
    geom_point() + 
    labs(title = "Certificates over Time", x = "Date Created", y = "Number of Certificates")
}

citations_by_number_of_certifications = function() {
  ggplot(data = gsrdata, mapping = aes(x = factor(Checks), y = Citations, group = Checks)) + 
    geom_boxplot(ou) +
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
  pie(table(gsrdata$Checks), main = "Certificates of All Packages", col = cols)
  pie(table(gd), main = "Certificates of All Packages last updated\nbefore the creation of the GSR", col = cols)
  par(mfrow = c(1,1))
}

combined = function (year, with_confidence) {
  yearlies = get_other_yearlies()
  colors = c("Support" = "#56B4E9", "Documentation" = "#D55E00", "Application" = "#E69F00", 
              "Accessability" = "#F5C710", "Domain Type" = "#0072B2","Average\nCitations" = "#009E73")
   ggplot(data = gsrdata) +
    geom_smooth(mapping = aes(x = DateCreated, y = Sup, color = "Support"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Doc, color = "Documentation"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = App, color = "Application"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Acc, color = "Accessability"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = yearlies$domain_type, color = "Domain Type"),
                se = with_confidence, linewidth = 1.3, linetype = 3) +
    geom_smooth(mapping = aes(x = DateCreated, y = yearlies$average_citation, color = "Average\nCitations"),
                se = with_confidence, linewidth = 1.3, linetype = 3) +
    labs(title = "Attributes of Packages Over Time", color = "Attribute") + xlab("Date Created") + ylab("Percent with attribute") +
     scale_color_manual(values = colors) +
     coord_cartesian(xlim = c(as.Date(paste(year,"/01/01", sep = "")),as.Date("2023/01/01"))) +
     ylim(c(0,1))
}
# Ask Dr. Peng
ggplot(data = gsrdata, mapping = aes(x = DateCreated, y = Sup)) + geom_point() + geom_smooth()

average_citations_distribution = function() {
  ggplot(data = gsrdata, mapping = aes(y = AverageCitations)) +
    geom_half_boxplot() +
    geom_half_dotplot() +
    scale_y_log10() +
    labs(title = "Average Citations per Year, Logarithmic Scale")
}

lifespan_distribution = function() {
  ggplot(data = gsrdata, mapping = aes(y = Lifespan)) +
    geom_half_boxplot() +
    geom_half_dotplot(binwidth = 1) +
    labs(title = "Support Length") +
    annotate("text",label="Vortex, c. 1993", x = 0.09, y = 27.6) +
    annotate("text",label="FastSlink, c. 1989", x = .1, y = 21.4) +
    annotate("text",label="Bottlesim, c. 2003", x = .1, y = 16.5) +
    annotate("text",label="Median: 2 years 7 months", x = -0.2, y = 6 )
}

lifepan_yearly = function() {
  ggplot(data = gsrdata, mapping = aes(x = Year, y = Lifespan, group = Year)) +
    geom_boxplot() + 
    geom_abline(slope = -1, intercept = 2023.6) +
    annotate("text", x = 2002.5, y = 24, size = 3,
             label = "<== Maximum possible lifespan,\naka years till present")
}

average_citations_by_number_of_certifications = function() {
  ggplot(data = gsrdata, mapping = aes(x = factor(Checks), y = AverageCitations, group = Checks)) + 
    geom_half_boxplot(outlier.shape = NA) +
    geom_half_point() +
    scale_y_log10() +
    labs(x = "Number of Certifications", y = "Average Citations, logarithmic scale", 
         title = "Average Citations by Number of Certifications") + 
    annotate("text",x = 1, y = 4, label = "Median: 1.405648") +
    annotate("text",x = 5, y = 65, label = "Median: 3.840268")
}

average_citations_over_time = function() {
  ggplot(data = gsrdata, mapping = aes(x= DateCreated, y = test)) +
    geom_smooth(method = "loess", formula = y~x) +
    geom_point() + 
    labs(title = "Average Citations over Time", x = "Date Created", y = "Average Number of Citations")
}

lifespan_by_number_of_certifications = function() {
  ggplot(data = gsrdata, mapping = aes(x = factor(Checks), y = Lifespan, group = Checks)) + 
    geom_half_boxplot(outlier.shape = NA) +
    geom_half_point() +
        labs(x = "Number of Certifications", y = "Lifespan, years", 
         title = "Lifespan by Number of Certifications") + 
    annotate("text",x = 1, y = 4, label = "Median: 0.02465753") +
    annotate("text",x = 5, y = 14, label = "Median: 5.024658")
}

lifespan_over_time = function() {
  test = as.numeric(as.Date("2023-8-7"))/365.25
  m = -1/365
  ggplot(data = gsrdata, mapping = aes(x= DateCreated, y = Lifespan)) +
    geom_abline(slope = m, intercept = test) +
    geom_smooth(method = "loess", formula = y~x) +
    geom_point() +
    labs(title = "Lifespan over Time", x = "Date Created", y = "Lifespan, years")
}

certificates_over_time_color = function() {
  ggplot(data = gsrdata) +
    geom_point(mapping = aes(x = DateCreated, color = Checks, y = AverageCitations)) + 
    scale_y_log10()
}
