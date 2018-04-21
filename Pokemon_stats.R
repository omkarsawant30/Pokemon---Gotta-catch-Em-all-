#Setting thw Working directory
setwd("E:/DATA ANALYTICS JOURNEY/Hackathons/Kaggle/Pokemon with stats")

# Read the Pokémon stats file
pokemon <- read.csv("Pokemon.csv")

#Loading libraries
library(tidyverse)
library(gridExtra)
library(ggplot2)

# View of first 6 rows
head(pokemon)

# Express the Generation column as a factor
pokemon$Generation <- as.factor(pokemon$Generation)

# Structure of Pokémon data set
str(pokemon)

#The multiple occurences of same numbers in X indicate mega evolutions

#Lets explore the types
# Primary types
levels(pokemon$Type.1)

#Secondary types
levels(pokemon$Type.2)

#For the Pokémon with only one type we have empty levels in the Type.2 column

# How many Pokémon of primary type?
ggplot(pokemon,aes(x=fct_infreq(Type.1)))+geom_bar(fill="#a399ff",color="black")+labs(x="Types of Pokemon",y="count")+
theme(axis.text.x = element_text(angle = 45,hjust = 1))

# How many Pokémon of secondary type?
ggplot(pokemon,aes(x=fct_infreq(Type.2)))+geom_bar(fill="#ff999e",color="black")+labs(x="Types of Pokemon",y="count")+
theme(axis.text.x = element_text(angle = 35,hjust = 1))

# Counts of each type combination
types <- pokemon %>%
  group_by(Type.1, Type.2) %>%
  summarise(count=n())

#Tile table of Type.1 and Type.2 combo
ggplot(types, aes(x=Type.1,y=Type.2)) +
  geom_tile(aes(fill=count)) +
  geom_text(aes(label=count)) +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x="Type 1", y="Type 2") +   
  scale_fill_gradient(low="white", high="red")

# Histogram of Pokemon's HP
pw1 <- ggplot(pokemon, aes(x=HP)) +
  geom_histogram(fill="#ff99cd", colour="black") + 
  labs(x="HP", y="Frequency")
pw1

# Histogram of Pokemon's Attack
pw2 <- ggplot(pokemon, aes(x=Attack)) +
  geom_histogram(binwidth=5,fill="#ffc899", colour="black") + 
  labs(x="Attack", y="Frequency") 
pw2

# Histogram of Pokemon's Defense
pw3 <- ggplot(pokemon, aes(x=Defense)) +
  geom_histogram(binwidth=5,fill="#99ff9e", colour="black") + 
  labs(x="Defense", y="Frequency")
pw3

# Histogram of Special Attack
pw4 <- ggplot(pokemon, aes(x=Sp..Atk)) +
  geom_histogram(binwidth=5, fill="#ff99ad", colour="black") + 
  labs(x="Special Attack", y="Frequency") 
pw4

# Histogram of Special Defense
pw5 <- ggplot(pokemon, aes(x=Sp..Def)) +
  geom_histogram(binwidth=5, fill="#deff99", colour="black") + 
  labs(x="Special Defense", y="Frequency") 
pw5

# Histogram of Speed
pw6 <- ggplot(pokemon, aes(x=Speed)) +
  geom_histogram(binwidth=5, fill="#99f9ff", colour="black") + 
  labs(x="Speed", y="Frequency") 
pw6

# Histogram of Total 
pw7 <- ggplot(pokemon, aes(x=Total)) +
  geom_histogram(binwidth=5,fill="#99CCFF", colour="black") + 
  labs(x="Total", y="Frequency")
pw7

#Are the Legendary Pokémon better than the normal pokemon?
# Density plot of HP
pw8 <- ggplot(pokemon, aes(x=HP, fill=Legendary)) +
  geom_density(alpha=0.5) +
  labs(x="HP", y="Density") +
  theme(legend.position="right")
pw8

# Density plot of Attack
pw9 <- ggplot(pokemon, aes(x=Attack, fill=Legendary)) +
  geom_density(alpha=0.5) +
  labs(x="Attack", y="Density") +
  theme(legend.position="right")
pw9

# Density plot of Defense
pw10 <- ggplot(pokemon, aes(x=Defense, fill=Legendary)) +
  geom_density(alpha=0.6) +
  labs(x="Defense", y="Density") +
  theme(legend.position="right")
pw10

# Density plot of Special Attack
pw11 <- ggplot(pokemon, aes(x=Sp..Atk, fill=Legendary)) +
  geom_density(alpha=0.6) +
  labs(x="Special Attack", y="Density") +
  theme(legend.position="right")
pw11

# Density plot of Special Defense
pw12 <- ggplot(pokemon, aes(x=Sp..Def, fill=Legendary)) +
  geom_density(alpha=0.5) +
  labs(x="Special Defense", y="Density") +
  theme(legend.position="right")
pw12

# Density plot of Speed
pw13 <- ggplot(pokemon, aes(x=Speed, fill=Legendary)) +
  geom_density(alpha=0.5) +
  labs(x="Speed", y ="Density") +
  theme(legend.position="right")
pw13

# Density plot of Total
pw14 <- ggplot(pokemon, aes(x=Total, fill=Legendary)) +
  geom_density(alpha=0.5) +
  labs(x="Total", y="Density") +
  theme(legend.position="right")
pw14

#We can infer that Legendary Pokémon have better stats than normal ones

#Lets see the distribution according to the Pokémon's type in Boxplots to check outliers
#which may reveal the strongest pokemon

# Boxplot of HP
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(HP)),
       aes(x=reorder(Type.1, HP, FUN=median), y=HP)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#ffec99", high="#ff9c99") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

# Boxplot of Attack
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(Attack)),
       aes(x=reorder(Type.1, Attack, FUN=median), y=Attack)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#ffec99", high="#ff9999") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

# Boxplot of Defense
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(Defense)),
       aes(x=reorder(Type.1, Defense, FUN=median), y=Defense)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#ffff99", high="#99ffbc") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

# Boxplot of Special Attack
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(Sp..Atk)),
       aes(x=reorder(Type.1, Sp..Atk, FUN=median), y=Sp..Atk)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#99fbff", high="#b599ff") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

# Boxplot of Special Defense
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(Sp..Def)),
       aes(x=reorder(Type.1, Sp..Def, FUN=median), y=Sp..Def)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#99fbff", high="#b599ff") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

# Boxplot of Speed
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(Speed)),
       aes(x=reorder(Type.1, Speed, FUN=median), y=Speed)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#fffd99", high="#ff9999") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

# Boxplot of Total
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med=median(Total)),
       aes(x=reorder(Type.1, Total, FUN=median), y=Total)) +
  geom_boxplot(aes(fill=med)) +
  scale_fill_gradient(low="#fffd99", high="#ff9999") +
  coord_flip() +
  labs(x="Type 1") +
  theme(legend.position="right")

#From the boxplots we say that the Dragon-type Pokémon are superior to others 

#in which generation the stats are better?
pokemon %>%
  group_by(Generation) %>%
  summarize(Total=mean(Total)) %>%
  ggplot(aes(x=Generation, y=Total, group=1)) +
  geom_point()+
  geom_line(color="Red") +
  labs(x="Generation",y="Average Total")

#The fourth generation has the best average stats amongst all

# Average stats for each generation through individual attributes
pokemon %>%
  group_by(Generation) %>%
  summarize(HP=mean(HP),
            Attack=mean(Attack),
            Defense=mean(Defense),
            Sp..Atk=mean(Sp..Atk),
            Sp..Def=mean(Sp..Def),
            Speed=mean(Speed)) %>%
  gather(Stats, value, 2:7) %>%
  ggplot(aes(x=Generation, y=value, group=1)) +
  geom_line(colour="pink") +
  geom_point() +
  facet_wrap(~Stats) +
  labs(y="Average Stats")

#Lets get the Top 10 Best Pokémon for each stat now that we have covered the types n stats overall
# Top 10 HP Pokémon 
pokemon %>%
  select(Name, HP) %>%
  arrange(desc(HP)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name, HP), y=HP)) +
  geom_bar(stat="identity", fill="orange", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 HP Pokémon")

# Top 10 Attack Pokémon 
pokemon %>%
  select(Name, Attack) %>%
  arrange(desc(Attack)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name, Attack), y=Attack)) +
  geom_bar(stat="identity", fill="#ff9999", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 Attack Pokémon")

# Top 10 defense Pokémon 
pokemon %>%
  select(Name, Defense) %>%
  arrange(desc(Defense)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name,Defense), y=Defense)) +
  geom_bar(stat="identity", fill="#99ff9a", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 Defense Pokémon")

# Top 10 Special Attack Pokémon
pokemon %>%
  select(Name, Sp..Atk) %>%
  arrange(desc(Sp..Atk)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name, Sp..Atk), y=Sp..Atk)) +
  geom_bar(stat="identity", fill="#ff9999", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 Special Attack Pokémon")

# Top 10 Special Defense Pokémon
pokemon %>%
  select(Name, Sp..Def) %>%
  arrange(desc(Sp..Def)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name, Sp..Def), y=Sp..Def)) +
  geom_bar(stat="identity", fill="#99ff9a", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 Special Defense Pokémon")

# Top 10 Speed Pokémon
pokemon %>%
  select(Name, Speed) %>%
  arrange(desc(Speed)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name, Speed), y=Speed)) +
  geom_bar(stat="identity", fill="#fdff99", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 Speed Pokémon")

# Top 10 Total Pokémon
pokemon %>%
  select(Name, Total) %>%
  arrange(desc(Total)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Name, Total), y=Total)) +
  geom_bar(stat="identity", fill="#ff99c1", colour="black") +
  coord_flip() +
  labs(x="Name", title="Top 10 Total Pokémon")

# Number of Pokémon per generation
ggplot(pokemon, aes(x=Generation)) + 
  geom_bar(fill="#999aff", colour="black") +
  labs(x="Generation", y="Number of Pokémon")

# Number of Pokémon of each primary type per generation
ggplot(pokemon, aes(x=Type.1, fill=Generation)) + 
  geom_bar()+
  labs(x="Generation", y="Number of Pokémon") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Number of Legendary Pokémon per generation
ggplot(pokemon, aes(x=Generation, fill=Legendary)) + 
  geom_bar(position="dodge") +
  labs(x="Generation", y="Number of Pokémon")

table(pokemon$Legendary)
#Only 65 Pokémon (8.8%) are Legendary!
