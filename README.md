# Lab-13-StatsSloths-

# How are most people dying from accidents and incidents?

Katie -> Poisoning
Zandy -> Falls
Kevin -> Burns

Perm_mean function from last week (corrected):
```{r}
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

### Kevin's Section:
* Question: Are burn deaths increasing or decreasing across the world?
* This question helps answer our main question because if deaths caused by burns are increasing, we will know that more people are dying from burns over time.
* Map code:
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
values <- perm_mean(1000, data$burns, 27)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .02) +
  geom_vline(xintercept = -1.79)
```
* Percentile: Real mean difference is in the Oth percentile
* Conclusion: The number of deaths caused by burns is decreasing in the world because the real mean difference is so rare to the sampled mean differences, indicating the year labels do matter.
* Question Answer: The actual difference in the average number of deaths per year from 1990 to 2016 is -1.79. This result and that of my permutation test lead to the conclusion that the number of deaths caused by burns per 100000 people is decreasing around the world.



