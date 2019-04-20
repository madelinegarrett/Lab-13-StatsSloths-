# Lab-13-StatsSloths-

#Madeline Garrett, Zandy Boone, Kevin Luth, Katie Stewart

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(gapminder)

```


## Overall Question: How are most people dying from accidents and incidents?

### Dataset Descriptions:
* Poisoning deaths dataset: This dataset gives the number of deaths caused by poison per every 100,000 people by year by country from 1990 to 2016.
* Burn deaths dataset: This dataset gives the number of deaths caused by burns per every 100,000 people by year per country from 1990 to 2016.
* Fall deaths dataset: This dataset gives the number of deaths due to falls per every 100K people by year and by country from 1990 to 2016.
##### Conclusion 
* From our research we can conclude that most people are dying from burns of all three of the incident accident reports but there is a general decrease in accident and incident deaths in the past year 26 years. In general most people are more safe but there is still a lot we can do to help combat this issue. The number of deaths caused by posion is decreasing on every continent in the world because the real mean difference is different to the sampled mean difference which means the the labels do matter in this case. Each continents has had a decrease in the amount of deaths by poison per 100,000 people. From this we know that around the world the number of poison deaths as a whole has decreased since 1990. This is true of both falls and burns as well. There is still a lot we can do to help lower these numbers including, fire safety laws and building codes. 



```{r, include = FALSE}
perm_mean <- function(perms = 1000, values, n1)
{
  ## Variables ##
  # perms: The number of permutations 
  # values (num): 
  # n1 (int): Size of group 1
  ###############
  
  # Step 1:
  # Create vector of zeroes of length "perms" to store
  # permuted mean differnces
  means <- vector(mode = "double", length = perms)
  
  # Loop throught number of permutations
  for (i in c(1:perms))
  {
    # Step 2
    # Shuffle them using smample
    # Randomly separate vector "values" into disjoint 
    # groups of size "n1" and "length(values) - n1" respectively
    # group_one <- sample(values, n1)
    # group_two <- sample(values, length(values)-n1)
    sampled <- sample(values)
    group_one <- sampled[1:n1]
    group_two <- sampled[n1:length(values)]
    
    # Step 3:
    # Compute the sample means for the two groups from
    g1mean <- mean(group_one)
    g2mean <- mean(group_two)
    
    # Step 4: 
    # Compute the difference in sample means, store the
    # value in the vector from step 1
    diff <- g1mean - g2mean
    means[i] <- diff
  }
  # Step 5:
  # Return new updated vector, created in step 1
  means
}
```

## Individual

###  Question: Are deaths caused by poisonings increasing or decreasing on each continent?

Katie Stewart

* This questions helps to answer our main question because if poisonings are increasing anywhere, we will know that more people are dying from poisonings in that area. We can help to identify risk areas that may be pulling the data in an upward direction. 

* Map Code:


```{r}
#merge data sets togther to get continents
poison <- read.csv("poisonings_deaths_per_100000_people.csv")
gap <- gapminder
poison <- merge(poison, gap, by = "country")%>%
  select(2:29)
```

```{r, include = FALSE}
asia <- filter(poison, continent == "Asia")%>%
  select(-28)
europe <- filter(poison, continent == "Europe")%>%
  select(-28)
americas <- filter(poison, continent == "Americas")%>%
  select(-28)
africa <- filter(poison, continent == "Africa")%>%
  select(-28)
oceania <- filter(poison, continent == "Oceania")%>%
  select(-28)
```

* Asia: -1.245185 
```{r}
AsiaMeans <- map_dbl(asia, mean)
newMeanAsia <- AsiaMeans[27]
oldMeanAsia <- AsiaMeans[1]
differenceAsia <- newMeanAsia - oldMeanAsia
```
* Europe: -0.8063333 
```{r}
EuropeMeans <- map_dbl(europe, mean)
newMeanEurope <- EuropeMeans[27]
oldMeanEurope <- EuropeMeans[1]
differenceEurope <- newMeanEurope - oldMeanEurope
```
* Americas: -0.91375 
```{r}
AmericasMeans <- map_dbl(americas, mean)
newMeanAmericas <- AmericasMeans[27]
oldMeanAmericas <- AmericasMeans[1]
differenceAmericas <- newMeanAmericas - oldMeanAmericas
```
* Africa: -1.403725 
```{r}
AfricaMeans <- map_dbl(africa, mean)
newMeanAfrica <- AfricaMeans[27]
oldMeanAfrica <- AfricaMeans[1]
differenceAfrica <- newMeanAfrica - oldMeanAfrica
```
* Oceania: -0.165
```{r}
OceaniaMeans <- map_dbl(oceania, mean)
newMeanOceania <- OceaniaMeans[27]
oldMeanOceania <- OceaniaMeans[1]
differenceOceania <- newMeanOceania - oldMeanOceania
```

* Permutation: I sampled the number of deaths by poisonings for each country. I then calculated the difference in the means for the year 2016 and the year 1990. My null hypothesis was that none of the country's two groups would have the same mean and my test statistic was the difference in sample means.

```{r}
dataAfrica <- africa %>%
  gather(1:27, key = "year", value = "poisons")
mean_vals <- perm_mean(1000, dataAfrica$poisons, 27)
mean_data <- data_frame(mean_vals)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = mean_vals), binwidth = .02) +
  geom_vline(xintercept = -1.403725, color = "green") +
  ggtitle("Distribution of Mean Differences for Posion Deaths (AFRICA)")
  
```

```{r}
dataAsia <- asia %>%
  gather(1:27, key = "year", value = "poisons")%>%
  filter(!is.na(poisons))
values <- perm_mean(1000, dataAsia$poisons, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -1.245185, color = "green") +
  ggtitle("Distribution of Mean Differences for Posion Deaths (ASIA)")
```

```{r}
dataEurope <- europe %>%
  gather(1:27, key = "year", value = "poisons")%>%
  filter(!is.na(poisons))
values <- perm_mean(1000, dataEurope$poisons, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -0.8063333, color = "green") +
  ggtitle("Distribution of Mean Differences for Posion Deaths (EUROPE)")
 
dataAmericas <- americas %>%
  gather(1:27, key = "year", value = "poisons")%>%
  filter(!is.na(poisons))
values <- perm_mean(1000, dataAmericas$poisons, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -0.91375 , color = "green") +
  ggtitle("Distribution of Mean Differences for Posion Deaths (AMERICAS)")
  
dataOceania <- oceania %>%
  gather(1:27, key = "year", value = "poisons")%>%
  filter(!is.na(poisons))
values <- perm_mean(1000, dataOceania$poisons, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -0.165 , color = "green") +
  ggtitle("Distribution of Mean Differences for Posion Deaths (OCEANIA)")
```

Percentile: Africa, Asia, Europe and Americas all have a real mean difference in the 10th percentile. Oceania's real mean difference is in the 40th percentile.
Conclusion: The number of deaths caused by posion is decreasing on every continent in the world because the real mean difference is different to the sampled mean difference which means the the labels do matter in this case.
Answer to Question: Each of these continents has had a decrease in the amount of deaths by poison per 100,000 people. From this we know that around the world the number of poison deaths as a whole has decreased since 1990. 

### Question: Are burn deaths increasing or decreasing across the world?
Kevin Luth


* This question helps answer our main question because if deaths caused by burns are increasing, we will know that more people are dying from burns over time.
* Map Code:
```{r}
burns_df <- read_csv("burns_deaths_per_100000_people.csv") %>%
  select(-1) %>%
  na.omit()
original_means <- map_dbl(burns_df, mean)
current_mean <- original_means[27] #27 represents 2017
old_mean <- original_means[1] #1 represents 1990
original_diff <- current_mean - old_mean
```



* Permutation: I sampled the number of deaths by burns and then calculated the difference in the means of the two groups, one being 27 values long because there are 27 years in the dataset. My null hypothesis was that the two groups do not have the same mean and my test statistic was the difference in sample means.


```{r}
data <- read_csv("burns_deaths_per_100000_people.csv") %>%
  gather(2:28, key = "year", value = "burns") %>%
  filter(!is.na(burns))
values <- perm_mean(1000, data$burns, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -1.79, color = "blue") +
  ggtitle("Distribution of Mean Differences for Burn Deaths")
```


* Percentile: Real mean difference is in the 0th percentile
* Conclusion: The number of deaths caused by burns is decreasing in the world because the real mean difference is so rare to the sampled mean differences, indicating the year labels do matter.
* Question Answer: The actual difference in the average number of deaths per year from 1990 to 2016 is -1.79. This result and that of my permutation test lead to the conclusion that the number of deaths caused by burns per 100000 people is decreasing around the world.


###Question: Are deaths related to falls increasing over time?
Zandy Boone
* This question helps us to answer our main question by seeing if more or less people die due to falls over the years. 
* Map Code:
```{r}
falls_df <- read_csv("falls_deaths_per_100000_people.csv") %>%
  select(-1) %>%
  na.omit()
original_means <- map_dbl(falls_df, mean)
current_mean <- original_means[27] #27 represents 2016
old_mean <- original_means[1] #1 represents 1990
original_diff <- current_mean - old_mean
```
* Permutation: I sampled the number of deaths due to falls and then calculated the difference in the means of the two groups, one being 27 values long because there are 27 years in the dataset. My null hypothesis was that the two groups do not have the same mean and my test statistic was the difference in sample means.
```{r}
data <- read_csv("falls_deaths_per_100000_people.csv") %>%
  gather(2:28, key = "year", value = "falls") %>%
  filter(!is.na(falls))
values <- perm_mean(1000, data$falls, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -1.56, color = "red") +
  ggtitle("Distribution of Mean Differences for Fall Deaths")
```

* Percentile: Real mean difference is in the lower 20th  percentile
* Conclusion: The number of deaths caused by burns is staying about the same or only slightly decreasing in the world because the real mean difference is in the lower percentiles compared to the sampled mean differences, indicating the year labels do only slightly matter if not at all.
* Question Answer: The actual difference in the average number of deaths per year from 1990 to 2016 is -1.56. This result and that of my permutation test lead to the conclusion that the number of deaths caused by falls per 100000 people is decreasing overall around the world.


### Madeline's Section:
* Question: Have posions increased from 1990 to 2016?
* This question helps answer our main question because if deaths caused by posions are increasing, we will know that more people are dying from poisons over time. We can direct our efforts to trying to stop this from happening. 
* Map Code:
    This code tells us the original means and original differences from posionings in 1990 to posionings     in 2016
```{r}
p_df <- read_csv("poisonings_deaths_per_100000_people.csv") %>%
  select(-1) %>%
  na.omit()
original_means <- map_dbl(burns_df, mean)
current_mean <- original_means[27] #27 represents 2017
old_mean <- original_means[1] #1 represents 1990
original_diff <- current_mean - old_mean

means <- map_dbl(p_df, mean)
means2016 <- means[27] 
means1990 <- means[1]
diff <- means1990 - means2016
means1990
means2016
diff
```

```{r}
data <- read_csv("poisonings_deaths_per_100000_people.csv") %>%
  gather(2:28, key = "year", value = "burns") %>%
  filter(!is.na(burns))
values <- perm_mean(1000, data$burns, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02, fill = "lightblue") +
  geom_vline(xintercept = 1.06, color = "blue") +
  ggtitle("Distribution of Mean Differences for Poison Deaths")
```



```{r, include = FALSE}
poisons <- read_csv("poisonings_deaths_per_100000_people.csv")

p <- ggplot(data = data, na.rm = FALSE) +
  geom_bar(mapping = aes(x = as.factor(year), y = burns, color = as.factor(year)), stat = "identity")+
  ggtitle("Number of Posion Victims From 1990 to 2016")+ 
           xlab("1990 <----------------------------------------------------TIME------------------------------------------------> 2016") + 
           ylab("Number of Poisons")
```

```{r}
p + theme(axis.line=element_blank(),axis.text.x=element_blank(), legend.position="none")
```

Conclusion: The data that we were looking at was fairly rare with it lying fairly far from the main normal distribution of poison deaths. From my graph and the permutation test I can conclude that there are less poisonings now than there were in 1990 with a general decrease happening over the last 26 years.
