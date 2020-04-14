#tidyverse workshop
##materials borrowed from: https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

#if not installed
##install.packages("tidyverse")
library(tidyverse)
## install.packages("ggrepel") 
library("ggrepel")

#We need to load the data into R before we can do any analysis
#There are two options for this

#Import directly from my github page
housing <- read.csv(url('https://github.com/lgschaer/GSG-tidyverse/raw/master/landdata-states.csv'))
head(housing)

#Or you can download the data from my github page and import it from your computer (change file path to match your computer)
housing <- read_csv("/home/lgschaer/Desktop/Projects/TidyverseWorkshop/landdata-states.csv")
head(housing)


#use dplyr to summarize data
housing.summary <- housing %>%
  summarise(
    Median.Val = median(Home.Value),
    Mean.Val = mean(Home.Value),
    SD.Val = sd(Home.Value)
  )
head(housing.summary)

hist(housing$Home.Value)

#use dplyr to summarize data by groups
housing.summary2 <- housing %>%
  group_by(State)%>%
  summarise(
    Median.Val = median(Home.Value),
    Mean.Val = mean(Home.Value),
    SD.Val = sd(Home.Value)
  )
head(housing.summary2)

#use dplyr to get data ready for plotting:

#learn about the filter function
?dplyr::filter()

#filter
housing2 <- filter(housing, Date == 2001.25) 

#add columns
housing3 <- housing2 %>%
  mutate(
    LogLandVal = log(Land.Value)
  )
head(housing3)


#comparing ggplot2 to baseR for making graphics

#baseR
plot(CPI ~ HDI,
     col = factor(Country),
     data = dat)
legend("topleft",
       legend = dat$Country)


##ggplot2 is more customizable


#Make graphs by layering geometric objects

##view possible geometric objects ready for use
help.search("geom_", package = "ggplot2")

#Making a scatterplot
p1 <- ggplot(housing2, aes(y = Structure.Cost, x = Land.Value)) +
  geom_point()
p1

p1 + geom_text_repel(aes(label=State), size = 3)

#Aesthetic Mapping for more customization

#variables go inside aes()
#non-variables go outside aes()

#for example
p1 + geom_point(aes(size = 2),# incorrect! 2 is not a variable
             color="red") # this is fine -- all points red

#this is better, home value shown in color, region shown by shape
p1 + geom_point(aes(color=Home.Value, shape = region), size = 4)

#Some examples
dat <- read.csv(url('https://github.com/lgschaer/GSG-tidyverse/raw/master/EconomistData.csv'))
head(dat)

dat <- read_csv("/home/lgschaer/Desktop/Projects/TidyverseWorkshop/EconomistData.csv")
head(dat)

# make a scatterplot with CPI (Corruption Perception Index) on x axis and HDI (Human Development Index) on y-axis
ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point()

#now make all the points blue
ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(color = "blue")

#now make color of points indicate region
ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(aes(color = Region))

#make the size larger
ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(aes(color = Region), size = 2)

#set size to a variable
ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(aes(color = Region, size =  HDI.Rank))



#Let's make the graph I showed you at the beginning of the workshop
p1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))+
  geom_point()
p1

#change shape to open circles

## hint: use these commands to look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
ggplot(df2, aes(x = x, y = y))+ 
  geom_point(aes(shape = z), size = 4) + scale_shape_identity()

#shape 1 is an open circle

#now change the shape
p2 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))+
  geom_point(shape = 1, size = 2.5, stroke = 1.25)
p2

#add the trendline
p3 <- p2+
  geom_smooth(mapping = aes(linetype = "r2"),
            method = "lm",
            formula = y ~ x + log(x), se = FALSE,
            color = "red", show.legend = FALSE)
p3

#labelling points

#first make a list of points we want to label (since we only want to label some of the points)
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

#now label using geom_text
p3 +
     geom_text(aes(label = Country),
               color = "gray20",
               data = filter(dat, Country %in% pointsToLabel))
p3

#use ggrepel to prevent labels from overlapping
p4 <- p3 +
  geom_text_repel(aes(label = Country),
            color = "gray20",
            data = filter(dat, Country %in% pointsToLabel), force = 10)
p4

#change order/formatting of labels
dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))

#Notice change in order of legend items
p4

#other finishing touches
p5 <- p4 +
    scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                       limits = c(.9, 10.5),
                       breaks = 1:10) +
    scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                       limits = c(0.2, 1.0),
                       breaks = seq(0.2, 1.0, by = 0.1)) +
    scale_color_manual(name = "",
                       values = c("#24576D",
                                  "#099DD7",
                                  "#28AADC",
                                  "#248E84",
                                  "#F2583F",
                                  "#96503F")) +
    ggtitle("Corruption and Human development")
p5

#change theme, font sizes, labels, font, etc

library(grid) # for the 'unit' function
p6 <- p5 +
    theme_bw() + 
    theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        text = element_text(color = "gray20"),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="italic"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust=0),
        plot.title = element_text(size = 16, face = "bold"))
p6

