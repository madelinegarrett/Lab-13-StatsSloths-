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
