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
    geom_half_boxplot(ou) +
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
              "Accessability" = "#F5C710", "Code\nRepository" = "#0072B2","Average\nCitations" = "#009E73")
   ggplot(data = gsrdata) +
    geom_smooth(mapping = aes(x = DateCreated, y = Sup*100, color = "Support"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Doc*100, color = "Documentation"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = App*100, color = "Application"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = Acc*100, color = "Accessability"),se = with_confidence, linewidth = 1.3) +
    geom_smooth(mapping = aes(x = DateCreated, y = yearlies$domain_type*100, color = "Code\nRepository"),
                se = with_confidence, linewidth = 1.3, linetype = 3) +
    geom_smooth(mapping = aes(x = DateCreated, y = yearlies$average_citation*100, color = "Average\nCitations"),
                se = with_confidence, linewidth = 1.3, linetype = 3) +
    labs(title = "Attributes of Packages Over Time", color = "Attribute") + xlab("Date Created") + ylab("Percent with attribute") +
     scale_color_manual(values = colors) +
     coord_cartesian(xlim = c(as.Date(paste(year,"/01/01", sep = "")), as.Date("2021-01-01")),ylim = c(0,100))
}
# Ask Dr. Peng
ggplot(data = gsrdata, mapping = aes(x = DateCreated, y = Sup)) + geom_point() + geom_smooth()

average_citations_distribution = function() {
  ggplot(data = gsrdata, mapping = aes(y = AverageCitations)) +
    geom_half_boxplot() +
    geom_half_dotplot() +
    scale_y_log10() +
    annotate("text",x = -0.2, y = 6, label = "Median = 1.773998") +
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
  ggplot(data = gsrdata) + 
    geom_half_boxplot(outlier.shape = NA,
                      mapping = aes(x = factor(Checks), y = AverageCitations, group = Checks)) +
    geom_half_point(mapping = aes(x = factor(Checks), y = AverageCitations, group = Checks)) +
    scale_y_log10() +
    annotate("point", x = -1, y = 2, color = NA) +
    geom_half_boxplot(mapping = aes(x = -0.4, y = AverageCitations), outlier.shape = NA, side = 0) +
    geom_half_point(mapping = aes(x = -0.05, y = AverageCitations), side = 0) +
    geom_half_violin(mapping = aes(x = -0.05, y = AverageCitations), side = 1) +
    labs(x = "Number of Certifications", y = "Average Citations, logarithmic scale", 
         title = "Average Citations by Number of Certifications") + 
    annotate("text",x = 1, y = 4, label = "Median: 1.405648") +
    annotate("text",x = 5, y = 65, label = "Median: 3.840268") +
    annotate("text",x = -0.6, y = 65, label = "Median: 1.773998") +
    coord_cartesian(xlim= c(-1,5))
}

average_citations_over_time = function() {
  ggplot(data = gsrdata, mapping = aes(x= DateCreated, y = AverageCitations)) +
    geom_smooth(method = "loess", formula = y~x) +
    geom_point() + scale_y_log10() +
    labs(title = "Average Citations over Time", x = "Date Created", y = "Average Number of Citations")
}

lifespan_by_number_of_certifications = function() {
  ggplot(data = gsrdata) + 
    geom_half_boxplot(outlier.shape = NA,
                      mapping = aes(x = factor(Checks), y = Lifespan, group = Checks)) +
    geom_half_point(mapping = aes(x = factor(Checks), y = Lifespan, group = Checks)) +
        labs(x = "Number of Certifications", y = "Lifespan, years", 
         title = "Lifespan by Number of Certifications") + 
    annotate("point", x = -0.6, y = 2, color = NA) +
    geom_half_boxplot(mapping = aes(x = 0, y = Lifespan), outlier.shape = NA, side = 0) +
    geom_half_point(mapping = aes(x = 0.35, y = Lifespan), side = 1) +
    annotate("text",x = 1, y = 4, label = "Median: 0.02465753") +
    annotate("text",x = 5, y = 14, label = "Median: 5.024658") +
    annotate("text",x = 0, y = 15.6, label = "Median: 2.586301") +
    annotate("text",label="Vortex, c. 1993", x = 4.3, y = 27) +
    annotate("text",label="FastSlink, c. 1989", x = 3.3, y = 21) +
    annotate("text",label="Bottlesim, c. 2003", x = 4.3, y = 16)
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
    geom_smooth(mapping = aes(x = DateCreated, color = Domain.Type, y = Checks))
}

certificates_by_domain = function () {
  ggplot(data = gsrdata, mapping = aes(x = Domain.Type, y = Checks)) +
    geom_half_boxplot() +
    geom_half_violin()
}

lifespan_by_domain = function () {
  ggplot(data = gsrdata, mapping = aes(x = Domain.Type, y = Lifespan)) +
    geom_half_boxplot() +
    geom_half_violin(side = 1)
}

average_citations_by_domain = function () {
  ggplot(data = gsrdata, mapping = aes(x = Domain.Type, y = AverageCitations)) +
    geom_half_boxplot() +
    geom_half_violin(side = 1) +
    scale_y_log10()
}

citations_by_domain = function() {
  ggplot(data = gsrdata, mapping = aes(x = Domain.Type, y = Citations)) +
    geom_half_boxplot() +
    geom_half_violin(side = 1) +
    scale_y_log10()
}

compare = function(daniel) {
  ggplot(gsrdata,mapping = aes(x = DateCreated, y = daniel, color = Domain.Type)) +
    geom_smooth() + geom_jitter(height = 0.2)
}

compare_box = function(daniel) {
  ggplot(gsrdata[gsrdata$Year > 2010,],mapping = aes(y = AverageCitations, color = Domain.Type)) +
    geom_half_boxplot() + geom_half_point(side = 1) +
    scale_y_log10()
}

compare_histo = function(daniel) {
  ggplot(gsrdata) +
    geom_bar(mapping = aes(x = factor(Domain.Type), fill = factor(daniel)),
             position = "stack")
}

prop_test = function(daniel) {
  x1 = sum(daniel[gsrdata$Domain.Type == "Public Code Repository"])
  x2 = sum(daniel[gsrdata$Domain.Type == "Personal or Institutional"])
  n1 = length(daniel[gsrdata$Domain.Type == "Public Code Repository"])
  n2 = length(daniel[gsrdata$Domain.Type == "Personal or Institutional"])
  p1 = x1/n1
  p2 = x2/n2
  prop.test(x = c(x1,x2), n = c(n1,n2), p = c(p1,p2), correct = TRUE)
}
