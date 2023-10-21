## VPD Project - Team 1
## Team Members: Isabela Miranda (up201806175); Karim Kousa (up202102687); Silvia Tavares (up202204392)

#Install packages if needed
install.packages("lisa")

library(easypackages)
libraries("readxl", "lubridate", "dplyr", "arules" , "ggplot2" , "fuzzyjoin","VIM", "finalfit" , "tidyverse", "ggfittext","scales ","lisa" )


getwd()
#Please change the directory
setwd("D:/College/20221(M)/VPD/project/OneDrive_1_10-29-2022")

## loading datasets
HabitationCost40_Europe<- read_xlsx("Housing_cost_overburden_rate_by_tenure_status.xlsx") 
PovertyRiskEmployedPop_Europe <- read_xlsx("Poverty_Risk_Rate_Employed_Pop_Europe.xlsx") 

InflationRate_PT<- read_xlsx("Inflation_Rate_PT.xlsx") 
MinimumSalary_PT <- read_xlsx("National_minimum_wage.xlsx") 
Unemployment_PT <- read_xlsx("Unemployment_total_and_by_age_group.xlsx") 

#Change column name to Year in some data sets
colnames(MinimumSalary_PT)[1]<-"Year"
colnames(Unemployment_PT)[1]<-"Year"


##First we will create an ID in Habitation Cost in Europe and in Poverty Risk for employed people in Europe in order to do the left join

HabitationCost40_Europe$YearCountry <- paste(HabitationCost40_Europe$Year, HabitationCost40_Europe$Country, sep="-")

PovertyRiskEmployedPop_Europe$YearCountry <- paste(PovertyRiskEmployedPop_Europe$Year, PovertyRiskEmployedPop_Europe$Country, sep="-")


## Left Join between Habitation Cost and Poverty Risk for employed people in Europe

ds_raw_EU <- HabitationCost40_Europe %>% 
  left_join(PovertyRiskEmployedPop_Europe, by = 'YearCountry')

ds_EU <- ds_raw_EU %>%
  select (- Year.y, - Country.y, - YearCountry ) %>%
  rename (Year = Year.x , Country = Country.x)


ds_PT <- InflationRate_PT %>% 
  left_join(MinimumSalary_PT,by='Year') %>% 
  left_join(Unemployment_PT,by='Year')

#Chnage names of columns
colnames(ds_PT)[2]<-"Total_Inflation"
colnames(ds_PT)[4]<-"Total_Unemployment"
colnames(ds_PT)[3]<-"Minimum_Salary"

#Number of missing values
ds_PT %>% ff_glimpse()
ds_EU %>% ff_glimpse() 

#Storing the Missing Values
ds_EU_mv <- data.frame( ds_EU, is.na(ds_EU))

#k-NN to replace the missing values based on the 2 nearest neighbors
ds_EU<- VIM::kNN( ds_EU , variable = c( "Poverty_Risk_Rate_Employed_Pop" ) , k = 2 )
ds_EU<- VIM::kNN( ds_EU , variable = c( "HabitationCost40" ) , k = 2 )
ds_PT<- VIM::kNN( ds_PT , variable = c( "Total_Inflation" ) , k = 2 )

#Remove unwanted columns
ds_EU <- select(ds_EU, -c("Poverty_Risk_Rate_Employed_Pop_imp", "HabitationCost40_imp"))
ds_PT <- select(ds_PT, -c("Total_Inflation_imp"))

#Select the Poverty Risk Rate in Portugal for employed people and merge with Portugal datasets
PT <- c("Portugal")

ds_EU_PT <- ds_EU %>% 
  filter ( Country %in% PT ) %>% 
  select(- HabitationCost40)

ds_PT <- ds_PT %>% 
  left_join(ds_EU_PT,by='Year')

ds_PT <- ds_PT %>% 
  select(- Country)

  

### Visualization

#In order to get a better breaks in the X axis we create this function
scale_x_fancy <- function(xval, ...) {
  scale_x_continuous(breaks = ~ sort(c(pretty(.x, 10), xval)), ...)
}

#In order to have multiple lines in the plot we will use gather to put several columns in one column
ds_visual <- ds_PT %>%   
  select(Year, Total_Unemployment, Minimum_Salary, Poverty_Risk_Rate_Employed_Pop, Total_Inflation ) %>%   
  gather(key = "variable", value = "value", -Year) 

#To change the names in the legend
var3 <- factor( ds_visual$variable  )
levels(var3)<- c( Minimum_Salary = "Minimum Salary Rate",
                  Poverty_Risk_Rate_Employed_Pop = "Poverty Risk Rate for Employed People",
                  Total_Inflation ="Inflation Rate",
                  Total_Unemployment = "Unemployment Rate")

#Creating the first plot to analyse the social economic index from 2005 to 2021
p <- ggplot(ds_visual, aes(x = Year, y = value)) + 
       geom_line(aes(color = var3 ))+# var3)) +
       ylim(-1,21)+
       scale_x_fancy(xval = 2.84, name = "Year")+
       labs(
         title= "The impact of social economic indices on poverty risk rate for employed people",
         subtitle = paste( "created on:" , Sys.Date(), "by Isabela Miranda, Karim Kousa & Silvia Tavares"),
         x="Year",
         colour = "Social Economic Indices",
         y="Percentage")+
        scale_color_manual(values= lisa_palette("MarcChagall", n = 5)) 

## Applying a theme
p <- p + theme_light()


## Highlighting  important event over the period 
Economic_Crisis <- data.frame(xmin=2010, xmax=2014, ymin=-1, ymax=21)
Covid <-data.frame(xmin=2020, xmax=2021, ymin=-1, ymax=21)

p <- p + geom_rect(data= Economic_Crisis, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                   fill="blue4", alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data= Covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue4", alpha=0.1, inherit.aes = FALSE) +
  geom_fit_text(grow=TRUE,aes(xmin = 2011,xmax=2013, ymin = 19,ymax=21, label = "Economic Crisis"),
            size = 6, vjust = 0, hjust = 0, color = "grey34") +
  geom_fit_text(grow=TRUE,aes(xmin = 2020,xmax=2021, ymin = 19,ymax=21, label = "Covid-19"),
            size = 6, vjust = 0, hjust = 0, color = "grey34")+
  theme (plot.title = element_text( size = 20, hjust = 0.5, face = "bold" , family = "serif", color ="black" ))+
  theme(plot.subtitle = element_text(size = rel( 0.9 ) , color = "gray25" , hjust=0.5 ))

p 



## Saving the plot 1
my_h <- 20
ggsave( filename = "plotPortugal.png" , plot = p , dpi= 600, width = my_h * 1.6  ,  height = my_h , units = "cm")


#European plots

#The plots will be to see the evolution from 2005 to 2021 so we are getting these two years
ds_EU_vis <-ds_EU %>% 
  filter((Year == 2005 | Year == 2021)) %>% 
  mutate(Hab_2021 = 0)%>% 
  mutate(Pov_2021 = 0)

# Get the countries to use in for loop
countries <- ds_EU_vis$Country[1:29]

#The goal of the function is to use it in ordering the countries in the plots by the value of year 2021.
for(country in countries){
    ds_EU_vis$Hab_2021[ds_EU_vis$Country == country] =
      ds_EU_vis$HabitationCost40[ds_EU_vis$Country == country &
                                                 ds_EU_vis$Year == 2021]
    
    ds_EU_vis$Pov_2021[ds_EU_vis$Country == country] =
      ds_EU_vis$Poverty_Risk_Rate_Employed_Pop[ds_EU_vis$Country == country &
                                                 ds_EU_vis$Year == 2021]
}


#We created this function to avoid repeating the code.
Create_Dot_Plots <- function(d,column,order_by,line_color,low_color,high_color,
                             Ymin,Ymax,rect_color,title_name,photo_name){
  
  pEU <- ggplot(data= d, aes(x=column, y=fct_reorder(Country, order_by),fill=Year)) +
    geom_line(color = line_color) +
    geom_point(shape=21, size=3) +
    scale_fill_gradient(low = low_color, high = high_color,name="Year",breaks=c(2005,2021),labels=c(2005,2021))+
    scale_shape_manual() + guides(fill = guide_legend(override.aes = list(shape = 21))) +
    geom_rect(data= Covid, aes(xmin= -Inf, xmax = Inf, ymin=Ymin, ymax=Ymax),
              fill=rect_color, alpha=0.3, inherit.aes = FALSE)
  
  
  pEU <- pEU + labs(  title= title_name,subtitle = paste( "created on:" , Sys.Date(), "by Isabela Miranda, Karim Kousa & Silvia Tavares"),
                        x="Percentage",y="Countries")
  pEU <- pEU + scale_x_continuous(limits = c( 0, 32 ) , breaks = seq(0, 32, by=2))
  pEU <- pEU + theme_light() + theme (plot.title = element_text( size = 20, hjust = 0.5, face = "bold" , family = "serif", color ="black" ))+
    theme(plot.subtitle = element_text(size = rel( 0.9 ) , color = "gray25" , hjust=0.5 ))
  
  ggsave( filename = photo_name , plot = pEU , dpi= 600, width = my_h * 1.6  ,  height = my_h , units = "cm")
  
  return(pEU)
}

#Creating the second plot
p2 <- Create_Dot_Plots(ds_EU_vis,ds_EU_vis$HabitationCost40,ds_EU_vis$Hab_2021,"gray47","turquoise4",
                       "turquoise1",14.5,15.5,"mediumaquamarine",
                       "Housing expenses that represents >=40 % of income","plotEU_Hab.png")
p2

#Creating the third plot
p3 <- Create_Dot_Plots(ds_EU_vis,ds_EU_vis$Poverty_Risk_Rate_Employed_Pop,ds_EU_vis$Pov_2021,"gray47",
                       "orangered4","red",22.5,23.5,"mediumaquamarine",
                       "Poverty risk rate for employed people","plotEU_Poverty.png" )

p3