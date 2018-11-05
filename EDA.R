install.packages(c("ggplot2", "tidyverse", "reshape", "dplyr", "plotly"))
source('hw.R')
library(tidyverse)
library(reshape)
library(dplyr)
library(ggplot2)
library(plotly)
# to set the directory
setwd("~/Documents/grad/sem1/stat/proj/idea/stat")

# reading the NCHS.csv file.
nchs<-read.csv("NCHS.csv",header = TRUE)

# to get the basic summary of the nchs dataset.
str(nchs)

# population for year 2015 for all the states in the US
pop<-read.csv("population_2015.csv",header = TRUE)
str(pop_15)

# cleaned the population_2015.csv 
# only united states removed
# removed fixed (redundant values int he data)
# removed metro from the data
# floating removed
# only ages 0-84 is considered for the analysis
nchsnew<-filter(nchs, nchs$State!="United States") 
nchsnew<-filter(nchs, nchs$Benchmark!="2005 Fixed")
nchsloc<-filter(nchsnew, nchsnew$Locality=="All" ) 
nchs_float<-filter(nchsloc, nchsloc$Benchmark!="Floating")
nchs_float<-filter(nchs_float, nchs_float$Age_Range=="0-84")
str(nchs_float)
sum(nchs_float$Observed_Deaths)

# cleaning data for choropleth plot
nchs_choro<-filter(nchsloc, Benchmark!="Floating")#floating removed
nchs_choro<-filter(nchs_choro, Age_Range=="0-84")#only ages 0-84
nchs_choro<-filter(nchs_choro, Year=="2015")

# metro and non-metro reading from the files
non_metro<-read.csv("non_metro.csv",header = TRUE)
metro<-read.csv("metro.csv",header = TRUE)

# aggregating the number of deaths for state metro 
metroAggr <- aggregate(Observed_Deaths~State,metro, sum)
non_metroAggr <- aggregate(Observed_Deaths~State,non_metro, sum)
popAggr <- aggregate(Population~State,metro, mean)

#creating a dataframe 
choro_df<-data.frame(year=metro$Year,
                     state=metro$State,
                     total_population=pop$population,
                     observed_deaths=scat1$Observed_Deaths,
                     deaths_in_metro_region=metroAggr$Observed_Deaths,
                     deaths_in_non_metro_region=non_metro$Observed_Deaths)

#exporting to csv file to choro.csv
write.csv(nchsnew, file = "choro.csv")

# prepping the data for the choropleth plot
nchs_choro<-aggregate(nchs_float$Observed_Deaths~nchs_float$State
                      +nchs_float$Year
                      +nchs_float$Cause_of_Death
                      +nchs_float$State
                      +nchs_float$Age_Range
                      +nchs_float$Observed_Deaths, nchs_float, sum)

choroAgg <- aggregate(nchs_float$Observed_Deaths~nchs_float$State
                      +nchs_float$Observed_Deaths,nchs_choro, sum)


choroAgg_exp <- aggregate(nchs_float$Expected_Deaths~nchs_float$State
                      +nchs_float$Expected_Deaths, nchs_choro, sum)

choroAgg_exp <-filter(choroAgg_exp, choroAgg_exp$State!="United States")

choroAgg_potential <-aggregate(nchs_float$Potential_Excess_Deaths~nchs_float$State
                              ,nchs_choro , sum)
micro_obs1<-filter(micro_obs,micro_obs$`nchs_float$State`!="United States")

choroAgg_potential<-filter(choroAgg_potential, choroAgg_potential$`nchs_float$State`!="United States")
pop

chor_final_csv<-data.frame(potential=choroAgg_potential$`nchs_float$Potential_Excess_Deaths`,
                           expected=choroAgg_exp$`nchs_float$Expected_Deaths`,
                           met= metroAggr$Observed_Deaths,
                           nonmet= non_metroAggr$Observed_Deaths,
                           #per_hun=per_thou$per_thou,
                           micro_obs1$`nchs_float$Observed_Deaths`)

write.csv(chor_final_csv, "filename.csv")


totalA <- aggregate(nchs_float$Observed_Deaths~nchs_float$State,nchs_float, sum)
totalA<-filter(totalA, totalA$`nchs_float$State`!="United States")
totalA[,2]<-round(totalA[,2]/1000)


dilli<-ggplot(totalA, 
       aes(x=totalA$`nchs_float$State`, 
           y=totalA$`nchs_float$Observed`))+
  geom_bar(stat = "identity",fill="red", 
           width = 0.7, size = 1) +hw + 
  scale_y_continuous(expand = c(.005,0),
                     limits = c(0,1200),
                     labels = scales::comma,
                     breaks = c(0, 100,200, 300, 400,
                                500, 600, 700, 800, 900, 1000, 1100, 1200))+                     
  theme(legend.position = "top",
        axis.text.y = element_text(size = rel(1.), face = "bold.italic"),
        axis.text.x =element_text(angle = 90, vjust = 0, hjust = 1,size = rel(1), face = "bold"),
        panel.background = element_rect(gray(.95))) +
  labs(x="State",face = "bold.italic",
       y="Deaths \n in thousands",
       title=" Deaths due to Five major diseases  in USA(2005-2015)")
plot(dilli)

# for an interactive graph using plotly
ggplotly(dilli)

#freq graph for comparing both obs and exp
obs<-aggregate(usa$Observed_Deaths~usa$Year,usa, sum)

a<-ggplot(obs,aes(x=factor(obs$`usa$Year`),
                  y=obs$`usa$Observed_Deaths`, group=1))+
  geom_line(color="black", size=1) + 
  labs(x="YEAR",y="number of deaths",
       title="observed frequency graph") + hw+ 
  scale_y_continuous(labels = scales::comma,expand = c(.005,0),
                     limits = c(0,60000000)) +
  theme(panel.background = element_rect(gray(.96)))

exp<-aggregate(usa$Expected_Deaths~usa$Year,usa, sum)

b<-ggplot(exp,aes(x=factor(exp$`usa$Year`),
               y=exp$`usa$Expected_Deaths`, group=1))+
  geom_line(color="black", size=1) + 
  scale_y_continuous(labels = scales::comma,expand = c(.005,0),
                     limits = c(0,60000000)) + 
  labs(x="YEAR",y="number of deaths",
       title="expected frequency graph") + hw +
  theme(panel.background = element_rect(gray(.96)))


#facet graph
q<-ap + scale_fill_discrete(name = "causes of death")+xlab("year")+ylab("number of deaths(in thousands")
ggplotly(ap)


ab<-ggplot(nchso, aes(years, observed_deaths, group=cause_of_death, fill=cause_of_death)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_y_continuous(expand=c(0,0), limits = c(0,1000),
                     labels = scales::comma,
                     breaks = c(0, 100, 200,300, 400, 500, 600, 700, 800, 900, 1000)) + 
  scale_x_continuous( limits = c(2004,2016),
                      labels = scales::comma,
                      breaks = c(2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015 ))+
  labs(x="years",
       y="number of deaths \n (in thousands)",
       title="Comparison -top five causes of deaths(grouped by years")+
  scale_colour_grey() + theme_bw()
ggplotly(ab)
                      
# data for micromap

micro_obs<-aggregate(nchs_float$Observed_Deaths~nchs_float$State,nchs_float, sum)
micro_exp<-aggregate(nchs_float$Expected_Deaths~nchs_float$State,nchs_float, sum)
micr_map<-data.frame(state=micro_obs$`nchs_float$State`, obs=micro_obs$`nchs_float$Observed_Deaths`, exp=micro_exp$`nchs_float$Expected_Deaths`)
micr_map<-filter(micr_map, micr_map$state!="United States")
micr_map[,2:3]<-round(micr_map[,2:3]/1000)

#data for the scatterplot

scat1<-filter(nchs_float, Year==2015, State!="United States")#filter out 2015 and removed USA
newdata <- scat1[c(3,9)]
scatter_pop<-aggregate(newdata$Observed_Deaths~newdata$State, newdata, sum)
colnames(scatter_pop) <- c("state", "deaths in 2015")


# pop = populatoin for 2015
scatter_pop$popul<-pop$population
scatter_pop[,3]<-round(scatter_pop[,3]/1000000)
###################scatter plot smooth################

sil<-ggplot(scatter_pop, aes(x=scatter_pop$popul, y=scatter_pop$`deaths in 2015`))+
  geom_point(shape=21,size=4,color="black",fill="red") +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue")+
  labs(x="population \n (in Millions)",
       y="number of Deaths \n (due to top 5 diseases)",
       title="Population Vs Deaths \n (2015)")+ hw + coord_flip()
sil

# storing the points manually for all the states in the order
pts<- scatter_pop %>% filter(scatter_pop$state %in%
                                    c('California','Florida','Texas','New York', 'Illinois'))


sil+geom_point() + geom_text(aes(label=pts$state))

scat<-sil+geom_point() + geom_text(aes(label=c(" "," "," "," ","California"," "," "," "," ","Florida "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ","NEW YORK"," "," "," "," "," "," "," "," "," "," ","Texas"," "," "," "," "," "," "," " )),hjust=1, vjust=2)


# plot_ly(scat)
boom<-data.frame(sta=pts)
str(boom)

# writing to a .csv file
write.csv(scatter_pop, "prt_thou.csv")

library(xlsx)
per_thou<-read.csv("prt_thou.csv",header = TRUE)
str(per_thou)
per_thou[,4]<-round(per_thou[,4])


sil1<-ggplot(per_thou, aes(x=popul , y=per_thou))+
  geom_point(shape=21,size=4,color="black",fill="red") +
  labs(y="Number of deaths \n per 100,000",
       x="Population\n (in millionS)",
       title="Population Vs Deaths \n (2015)")+ hw + coord_flip()
xxx<-sil1+geom_point() + 
  geom_text(aes(label=c("","","","","California","","","","",
                                               "Florida ","","","","","","","","Kentucky",
                                               "","Maine","","","","","Mississippi",
                                               "","","","","","","","New York","","","","",
                                               "","","","","","Tennessee",
                                               "Texas","","","","","","","" )),
                                   hjust=1.2, vjust=0.1)+
  scale_y_continuous( limits = c(0,700),
                      breaks = c(0,100,200,300,400,500,600, 700))

ggplotly(xxx)
                                                                                                                                                                                                                                                                                                             

temp<-per_thou[order(per_thou$X.hundered.thou),c(1,4)]
rownames(temp) <- c()



# dat for micromap

micro_obs<-aggregate(nchs_float$Observed_Deaths~nchs_float$State,nchs_float, sum)
micro_exp<-aggregate(nchs_float$Expected_Deaths~nchs_float$State,nchs_float, sum)
micr_map<-data.frame(state=micro_obs$`nchs_float$State`, obs=micro_obs$`nchs_float$Observed_Deaths`, exp=micro_exp$`nchs_float$Expected_Deaths`)
micr_map<-filter(micr_map, micr_map$state!="United States")
micr_map[,2:3]<-round(micr_map[,2:3]/1000)
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(micromapST)

stateNames <- read.csv(file = 'stateNames.csv',
                       header = TRUE,as.is = TRUE)[,1]

toFix <- read.csv(file = "MathG8Y2011.csv",
                  header = TRUE,as.is = TRUE)

head(toFix)

stateNames <- read.csv(file = 'stateNames.csv',
                       header = TRUE,as.is = TRUE)[,1]
colDesc <- data.frame(
  type = c('id','dot','map','dot'),
  # lab1 = c('','','',''),
  lab1 = c('','observed','','expected'),
  # lab3 = c('','','Possible','booo'),
  col1 = c(NA,'obs',NA,'exp'))

micromapST(micr_map, colDesc,
           rowNamesCol = 'state', rowNames = 'full',
           plotNames= 'full',
           sortVar = 'obs',ascend=TRUE,
           axisScale = 's',
           title = c('DISEASES',
                     'DATA'))

