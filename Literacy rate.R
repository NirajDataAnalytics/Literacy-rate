install.packages('ggmap')
install.packages('ggplot2')
install.packages("ggrepel")
install.packages("ggalt")
library(ggalt)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggmap)
library(scales)



getwd()
setwd('C:\Users\user\Documents\R')
getwd()

# Read the data set Literacy rates

lt <- read_excel('C:\\Users\\user\\Downloads\\Literacy rates.xlsx')
View(lt)

# Internal Structure of literacy rates

str(lt)

# Top 6 rows to observe

head(lt)

# To see every columns in literacy rates and observe the some rows data

glimpse(lt)

# Display the column name

names(lt)

# Summary of all records

summary(lt)

# Checking any Null Values

sum(is.na(lt))

# Checking the duplicated Values

sum(duplicated(lt))

lt %>% count(Year) 

lt%>% count(Country)

length(unique(lt$Country))

lt %>% count(lt$Age)

# Top and Bottom 10 Country High Literacy Rate Low Literacy Rate in 9 Years

literacy_rate_decimal <- lt %>% 
    group_by(Country) %>%
    
  summarise(Average_literacy_rate = mean(`Literacy rate`,na.rm = TRUE))
  
literacy_rate_percentage <- mutate(literacy_rate_decimal,
Average_literacy_rate=  Average_literacy_rate*100)
View(literacy_rate_percentage)

x1 <- arrange(literacy_rate_percentage, desc(Average_literacy_rate))
x2 <-head(x1,10)
x3 <-tail(x1,10)
df <- rbind(x2,x3)
View(df)

ggplot(df,aes(y= Country, x= Average_literacy_rate, group=1, cex=0.10))+
geom_point(color='orange', pch= 20)+ geom_line(color='blue', linestyle='dotted',size=0.25)+
labs(title='Average Literacy Rate Vs Country', 
subtitle='Top and Bottom 10 Country High Literay Rate Low Literacy Rate', 
caption= 'Highest and Lowest Literacy Rate is :')+
theme_bw()

# Distribution of literacy rate across regions by age

world_avg_lt <- lt %>% summarise(avg=mean(`Literacy rate`,na.rm = TRUE)) %>% pull(avg)

 lt %>%
  group_by(Region) %>% 
  mutate(region_avg= mean(`Literacy rate`,na.rm = T)) %>% ungroup() %>%
    ggplot(aes(Region,`Literacy rate`,color=Age))+
  geom_jitter(alpha= 0.7,size =3,height = 0.2)+
  geom_hline(aes(yintercept=world_avg_lt),color="grey40",size=0.9)+
  geom_segment(aes(x=Region,xend=Region,y=world_avg_lt,yend=region_avg),color="black")+
  coord_flip()+
  theme_bw()
  
# Distribution of literacy rate across regions by Gender

# Male Distribution
lt1 <- lt %>% group_by(Region) %>% filter(Gender=='male')
lt1 %>% group_by(Region, Age) %>% summarise (avg_male=mean(`Literacy rate`,na.rm = TRUE))

# Female Distribution
lt2 <- lt %>% group_by(Region) %>% filter(Gender=='female')
lt2 %>% group_by(Region, Age) %>% summarise (avg_female=mean(`Literacy rate`,na.rm = TRUE))

# Concatenate
df1 <- rbind(lt1, lt2)
View(df1)

ggplot(data=df1,aes(y=Region, x=`Literacy rate`, color=Gender))+
geom_point(aes(fill=Gender))+
labs(title='Average Literacy Rate Vs Gender', 
subtitle='Regionwise and Gender wise Literacy rate', 
caption= 'Highest and Lowest Literacy Rate is :')+
theme_bw()

# Region Wise Gender Wise Literacy rate

# Male Literacy rate

lt1 <- lt %>% group_by(Region) %>% filter(Gender=='male')
lt2 <- lt1 %>% summarise(avg_male=mean(`Literacy rate`))

# Female Literacy rate

lt3 <- lt %>% group_by(Region) %>% filter(Gender=='female')
lt4 <- lt3 %>% summarise(avg_female=mean(`Literacy rate`))

# mutate
lt5 <-  mutate(lt2, lt4)
View(lt5)

ggplot(lt5,aes(x=avg_female, y=avg_male, fill= Region))+
geom_hex(bins=25)+labs(title='')
theme_bw()

# Year Wise Literacy rate across Regions

Y10 <- lt %>% group_by(Region) %>% filter(Year=='2010') %>% 
summarise(avg_2010=mean(`Literacy rate`))

Y11 <- lt %>% group_by(Region) %>% filter(Year=='2011') %>% 
summarise(avg_2011=mean(`Literacy rate`))

Y12 <- lt %>% group_by(Region) %>% filter(Year=='2012') %>% 
summarise(avg_2012=mean(`Literacy rate`))

Y13 <- lt %>% group_by(Region) %>% filter(Year=='2013') %>% 
summarise(avg_2013=mean(`Literacy rate`))

Y14 <- lt %>% group_by(Region) %>% filter(Year=='2014') %>% 
summarise(avg_2014=mean(`Literacy rate`))

Y15 <- lt %>% group_by(Region) %>% filter(Year=='2015') %>% 
summarise(avg_2015=mean(`Literacy rate`))

Y16 <- lt %>% group_by(Region) %>% filter(Year=='2016') %>% 
summarise(avg_2016=mean(`Literacy rate`))

Y17 <- lt %>% group_by(Region) %>% filter(Year=='2017') %>% 
summarise(avg_2017=mean(`Literacy rate`))

Y18 <- lt %>% group_by(Region) %>% filter(Year=='2018') %>% 
summarise(avg_2018=mean(`Literacy rate`))

Cum_Year<- bind_rows(Y10,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18)
Cum_Year<- replace(Cum_Year, is.na(Cum_Year),0)
View(Cum_Year)
Cum_Year %>% group_by(Region) %>% summarise(across(everything(), list(sum))) 

# Countries where the average female literacy rate is greater than the average male literacy rate

countries_female <- lt %>% 
  group_by(Country) %>% 
  filter(mean(`Literacy rate`[Gender=="female"],na.rm = TRUE)>
  mean(`Literacy rate`[Gender=="male"]))
  
lt %>% semi_join(countries_female)%>%
 group_by(Country)%>%  
summarise(avg_lt_male=mean(`Literacy rate`[Gender=='male'],na.rm=TRUE),
avg_lt_female=mean(`Literacy rate`[Gender=='female'],na.rm=TRUE))%>% ungroup()%>%
ggplot(aes(y=reorder(Country,avg_lt_female),x=avg_lt_male,xend=avg_lt_female))+
  ggalt::geom_dumbbell( size=2, colour='grey',colour_x = 'black',colour_xend ='red')+
  ggrepel::geom_text_repel(aes(x=avg_lt_female,label=percent(avg_lt_female,accuracy = 1)))+
  ggrepel::geom_text_repel(aes(x=avg_lt_male,label=percent(avg_lt_male,accuracy = 1)))+
  labs(x="Literacy Rate", y='Country', title = "Top 20 Countries List",subtitle=('average female literacy rate is greater than the average male literacy rate'), color='pink')+
  theme_bw()

# Countries where the average male literacy rate is greater than the average female literacy rate
 
Countries_Male <- lt %>% group_by(Country) %>% 
filter(mean(`Literacy rate`[Gender=='male'], na.rm=TRUE)>
mean(`Literacy rate`[Gender=='female']))
 
lt %>% semi_join(Countries_Male)%>%
group_by(Country)%>% summarise(avg_lt_male=mean(`Literacy rate`[Gender=='male'], na.rm=TRUE),
avg_lt_female=mean(`Literacy rate`[Gender=='female'], na.rm=TRUE))%>% ungroup() %>% head(20)%>%
ggplot(aes(y=reorder(Country, avg_lt_male), x=avg_lt_female, xend=avg_lt_male))+
ggalt::geom_dumbbell(size=2, color='grey', color_x= 'black', color_xend='red')+
ggrepel::geom_text_repel(aes(x=avg_lt_male, label=percent(avg_lt_male, accuracy=1)))+
ggrepel::geom_text_repel(aes(x=avg_lt_female, label=percent(avg_lt_female, accuracy=1)))+
labs(x='Literacy Rate', y='Country', title= "Top 20 Countries Name", subtitle=('average male literacy rate is greater than the average female literacy rate'),color='pink')+
theme_bw()

# Countries with the highest discrepancy between male and female literacy rates

countries_male <- lt %>% 
  group_by(Country) %>% 
  filter(mean(`Literacy rate`[Gender=="female"],na.rm = T)<mean(`Literacy rate`[Gender=="male"]))

lt %>% 
  semi_join(countries_male) %>% 
  group_by(Country) %>% 
  summarise(avg_lt_male = mean(`Literacy rate`[Gender=='male'],na.rm = T),
            avg_lt_female=mean(`Literacy rate`[Gender=='female'],na.rm = T),
            diff= avg_lt_male-avg_lt_female) %>% 
  top_n(20,diff)%>% 
  ggplot(aes(y=reorder(Country,avg_lt_female),x=avg_lt_male,xend=avg_lt_female))+
  ggalt::geom_dumbbell( size=5, colour="grey",colour_x = "#005670",colour_xend = "#d20962")+
  geom_text(aes(x=avg_lt_female,label=percent(avg_lt_female,accuracy = 1)),vjust=-1)+
  geom_text(aes(x=avg_lt_male,label=percent(avg_lt_male,accuracy = 1)),vjust=-1)+
  geom_rect(aes(xmin=1,xmax=1.2,ymin=-Inf,ymax=Inf),fill="grey")+
  geom_text(aes(label=percent(diff,accuracy = 1), y=Country, x=1.1), fontface="bold", size=4)+
  geom_text(aes(x=1.1,y=20.5,label="Difference"))+
  labs(x='', y='',title = "Top 20 countries with highest discrepency between -literacy rates", subtitle = 'Difference in Literacy rate ')+
  theme_bw()


# Country India in the Region- 9th Position

lt %>% filter(Region=='Central and Southern Asia') %>% 
group_by (Country) %>% 
summarise(Avg_lt_Region=mean(`Literacy rate`)) %>% 
ggplot(aes(x=Avg_lt_Region, y=Country))+
  geom_bar(stat="identity", color='pink', fill='green')+
  geom_text(aes(x=Avg_lt_Region, label=percent(Avg_lt_Region, accuracy = 1)), vjust=0.6, color='black', size=3.5)+
  labs(x='Avg Literacy Rate', title="Literacy of Central and Southern Asia")+
  theme_bw()

# male and female Ratio
  
lt %>% filter(Region=='Central and Southern Asia') %>% group_by(Country)%>%
  summarise(avg_m=mean(`Literacy rate`[Gender=='male']),avg_f= mean(`Literacy rate`[Gender=='female']))%>%
  ggplot(aes(x=avg_m,y=Country, fill=avg_f))+
  geom_bar(stat='identity')+
  theme_minimal()

# Literacy Rate by Gender in India
  ind_bygender <- lt %>% filter(Country=='India') %>% group_by(Year, Gender)%>%
    summarise(avg=mean(`Literacy rate`, na.rm=T, .groups='drop')) %>% ungroup() 
    ggplot(ind_bygender, aes(factor(Year), avg, fill = Gender))+
    geom_bar(stat = "identity", width = 0.5, position = "dodge") +
    scale_fill_manual(values = c("male"="#005670","female"="#d20962","total"="#ada6a5"), labels = c("female", "male", "both"))+
    labs(y="Literacy Rates",x="Year",title = "Literacy Rate Trend across Years in India")+
    theme(text = element_text(family = "Roboto Condensed"))+
      theme_bw()


# Literacy rate across regions from 2011 to 2018
    
    avg_region <- lt %>% group_by(Year, Region)%>%
      summarise(avg=mean(`Literacy rate`, na.rm=T, .groups='drop'))%>%
      ungroup()
    
      avg_region %>% ggplot(aes(x=Year, y= avg, color='Region'))+
      geom_point(size = 2.0, aes(color = Region), shape = 10, stroke=2, na.rm=T)+
      scale_color_manual(values = c("Sub-Saharan Africa"="#02B58A","Oceania"="#FFD35C","Northern Africa and Western Asia"="#C62C3A","Latin America and the Caribbean"="#954567","Europe and Northern America"="#4BC4D5", "Eastern and South-Eastern Asia"="#21405F","Central and Southern Asia"="#FF8201"))+
      labs(y="Literacy Rate",x="Year",title = "Literacy Rate Trend across Regions")+
      theme_bw()
            
# Visualization Global Literacy rates on a Map
      
      world <- map_data(map = "world") %>%
        filter(region!="Antartica")
      
      long_lat <- lt %>%
        group_by(Country) %>% 
        summarise(`Literacy rate`=mean(`Literacy rate`,na.rm = T)) %>% 
        ungroup() %>% 
        left_join(world,by = c("Country"="region")) %>% 
        filter(!is.na(lat)) 
      
     P<- ggplot()+geom_map(data = world, map = world,
               aes(x=long, y=lat, group = group, map_id = region),
               fill = "#282828", color = "#282828") +
        geom_map(data = long_lat, map = world,
                 aes(fill = `Literacy rate`, map_id = Country),
                 color = "#282828", size = 0.5, alpha = .8)+
       scale_fill_gradient2(low = "#be0027", high = "#0a8ea0",mid = "#b4a996",midpoint = 0.6) +
       scale_y_continuous(breaks=c()) +
       scale_x_continuous(breaks=c()) +
       labs(x = "", y = "")+
       guides(
         fill = guide_legend(title = "Literacy Rate")
       ) +
       coord_map("gilbert", xlim = c(-300, 300))+
       theme_bw()

  
  