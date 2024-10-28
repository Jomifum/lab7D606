


## Getting Started

### Load packages

#In this lab, we will explore and visualize the data using the **tidyverse** suite of packages, and perform statistical inference using **infer**. The data can be found in the companion package for OpenIntro resources, **openintro**.

#Let's load the packages.


library(tidyverse)
library(openintro)
library(infer)



### The data

#Every two years, the Centers for Disease Control and Prevention conduct the Youth Risk Behavior Surveillance System (YRBSS) survey, where it takes data from high schoolers (9th through 12th grade), to analyze health patterns. You will work with a selected group of variables from a random sample of observations during one of the years the YRBSS was conducted.

#Load the `yrbss` data set into your workspace.


data('yrbss', package='openintro')


There are observations on 13 different variables, some categorical and some numerical. The meaning of each variable can be found by bringing up the help file:

```{r help-nc, eval=FALSE}
?yrbss
```


1.  What are the cases in this data set? How many cases are there in our sample?

**Based on the glimpse output, there are 13,583 cases equals to rows in this sample, each with data across 13 variables.**

Remember that you can answer this question by viewing the data in the data viewer or by using the following command:

```{r str}
glimpse(yrbss)
```

## Exploratory data analysis

You will first start with analyzing the weight of the participants in kilograms: `weight`.

Using visualization and summary statistics, describe the distribution of weights. The `summary` function can be useful.

```{r summary}
summary(yrbss$weight)
```

2.  How many observations are we missing weights from?

**Based on the summary output, there are 1,004 missing observations for the weight variable (NAs), indicating that 1,004 students did not have their weight recorded in the dataset.**

Next, consider the possible relationship between a high schooler's weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

First, let's create a new variable `physical_3plus`, which will be coded as either "yes" if they are physically active for at least 3 days a week, and "no" if not.

```{r create new var}
yrbss <- yrbss %>% 
  mutate(physical_3plus = ifelse(yrbss$physically_active_7d > 2, "yes", "no"))
```


3.  Make a side-by-side boxplot of `physical_3plus` and `weight`. Is there a relationship between these two variables? What did you expect and why?

**Enter your answer here**
```{r three}
# Side-by-side boxplot of physical_3plus and weight
library(dplyr)
library(ggplot2)

# Create the boxplot
ggplot(yrbss, aes(x = physical_3plus, y = weight)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Weight by Physical Activity Level (3+ Days a Week)",
    x = "Physically Active 3+ Days/Week",
    y = "Weight (kg)"
  ) +
  theme_minimal()

```

The box plots show how the medians of the two distributions compare, but we can also compare the means of the distributions using the following to first group the data by the `physical_3plus` variable, and then calculate the mean `weight` in these groups using the `mean` function while ignoring missing values by setting the `na.rm` argument to `TRUE`.

```{r by-means}
yrbss %>%
  group_by(physical_3plus) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))
```

There is an observed difference, but is this difference statistically significant? In order to answer this question we will conduct a hypothesis test.

## Inference

4.  Are all conditions necessary for inference satisfied? Comment on each. You can compute the group sizes with the `summarize` command above by defining a new variable with the definition `n()`.

**Insert your answer here**
Independence: The observations should be independent. We should ensure that the data collection process did not introduce any dependencies between the observations.

Random Sampling: The data should come from a random sample. If the dataset is a result of random sampling, this condition is satisfied.

Normality: The distribution of the sample means should be approximately normal. This can be checked using normality tests or QQ plots. Given a sufficiently large sample size, the Central Limit Theorem assures us that the distribution of the sample means will be normal.
```{r fourth}
yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(group_size = n())

```


5.  Write the hypotheses for testing if the average weights are different for those who exercise at least times a week and those who don't.

**Insert your answer here**
  #Null Hypothesis (H₀): The average weights are the same for those who exercise at least 3 times a week and those who do not. \[ H_0: \mu_{\text{yes}} = \mu_{\text{no}} \]
  
  #Alternative Hypothesis (Hₐ): The average weights are different for those who exercise at least 3 times a week and those who do not. \[ H_A: \mu_{\text{yes}} \ne \mu_{\text{no}} \]
  #Next, we will introduce a new function, `hypothesize`, that falls into the `infer` workflow. You will use this method for conducting hypothesis tests. 
  
  ```{r fifth}
t_test_result <- t.test(weight ~ physical_3plus, data = yrbss, na.rm = TRUE)
print(t_test_result)

```
But first, we need to initialize the test, which we will save as `obs_diff`.

```{r inf-weight-habit-ht-initial, tidy=FALSE, warning = FALSE}
obs_diff <- yrbss %>%
  drop_na(physical_3plus) %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
```

Notice how you can use the functions `specify` and `calculate` again like you did for calculating confidence intervals. Here, though, the statistic you are searching for is the difference in means, with the order being `yes - no != 0`.

After you have initialized the test, you need to simulate the test on the null distribution, which we will save as `null`.

```{r inf-weight-habit-ht-null, tidy=FALSE, warning = FALSE}
null_dist <- yrbss %>%
  drop_na(physical_3plus) %>%
  specify(weight ~ physical_3plus) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
```

Here, `hypothesize` is used to set the null hypothesis as a test for independence. In one sample cases, the `null` argument can be set to "point" to test a hypothesis relative to a point estimate.

Also, note that the `type` argument within `generate` is set to `permute`, whichis the argument when generating a null distribution for a hypothesis test.

We can visualize this null distribution with the following code:
  
  ```{r}
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()
```


6. How many of these `null` permutations have a difference of at least `obs_stat`?
  
  **0 **
  ```{r sixth}
p_value_info <- null_dist %>% 
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

print(p_value_info)

```

Now that the test is initialized and the null distribution formed, you can calculate the p-value for your hypothesis test using the function `get_p_value`.

```{r inf-weight-habit-ht-pvalue}
null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")
```

This the standard workflow for performing hypothesis tests.

7.  Construct and record a confidence interval for the difference between the weights of those who exercise at least three times a week and those who don't, and interpret this interval in context of the data.
```{r seventh}

# Ensure physical_3plus is a factor with two levels
yrbss$physical_3plus <- factor(yrbss$physical_3plus, levels = c("yes", "no"))

# Load necessary library for t-tests
library(infer)

# Conduct the t-test for the difference in means
t_test_result <- t.test(weight ~ physical_3plus, data = yrbss, na.rm = TRUE)

# Extract the confidence interval from the t-test result
ci <- t_test_result$conf.int

# Print the confidence interval
print(ci)

```

#The confidence interval ranges from approximately 1.12 to 2.42. This means we're 95% confident that the true difference in mean weight between those who exercise at least three times a week and those who don't lies within this range. Since the interval doesn't include zero, it suggests a statistically significant difference in mean weights, with those exercising more likely to weigh slightly more, perhaps due to muscle mass
* * *
  
  ## More Practice
  
  8.  Calculate a 95% confidence interval for the average height in meters (`height`) and interpret it in context.

**Insert your answer here**
  ```{r eighth}
# Load necessary library
library(tidyverse)

# Calculate the 95% confidence interval for height
height_mean <- mean(yrbss$height, na.rm = TRUE)
height_sd <- sd(yrbss$height, na.rm = TRUE)
n <- sum(!is.na(yrbss$height))
error_margin_95 <- qt(0.975, df = n-1) * (height_sd / sqrt(n))
ci_95 <- c(height_mean - error_margin_95, height_mean + error_margin_95)

# Print the 95% confidence interval
print(ci_95)

```
# This interval will provide a range where we can be 95% confident that the true average height of the participants lies within.

9.  Calculate a new confidence interval for the same parameter at the 90% confidence level. Comment on the width of this interval versus the one obtained in the previous exercise.

```{r nineth}
# Calculate the 90% confidence interval for height
error_margin_90 <- qt(0.95, df = n-1) * (height_sd / sqrt(n))
ci_90 <- c(height_mean - error_margin_90, height_mean + error_margin_90)

# Print the 90% confidence interval
print(ci_90)

```
# The 90% confidence interval will be narrower than the 95% interval. A lower confidence level means we're less certain the interval contains the true mean, but we gain precision in the estimate.

10.  Conduct a hypothesis test evaluating whether the average height is different for those who exercise at least three times a week and those who don't.

**Insert your answer here**
```{r tenth}
# Conduct the t-test for height between two groups
t_test_height <- t.test(height ~ physical_3plus, data = yrbss, na.rm = TRUE)

# Print the t-test result
print(t_test_height)

```

11.  Now, a non-inference task: Determine the number of different options there are in the dataset for the `hours_tv_per_school_day` there are.

**Insert your answer here**
```{r eleventh}

# Count the number of different options in hours_tv_per_school_day
unique_hours_tv <- yrbss %>% 
  distinct(hours_tv_per_school_day) %>% 
  count()

print(unique_hours_tv)

```

12. Come up with a research question evaluating the relationship between height or weight and sleep. Formulate the question in a way that it can be answered using a hypothesis test and/or a confidence interval. Report the statistical results, and also provide an explanation in plain language. Be sure to check all assumptions, state your $\alpha$ level, and conclude in context.

**Insert your answer here**
```{r twelveth}
# Load necessary libraries
library(tidyverse)

# Display the column names to verify the exact names
print(colnames(yrbss))

# Visualize the relationship using the correct column name
ggplot(yrbss, aes(x = school_night_hours_sleep, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship Between Sleep and Weight",
       x = "Hours of Sleep per Night",
       y = "Weight (kg)")

```
#The scatter plot suggests a pattern in the relationship between hours of sleep and weight, with similar weight spreads across most sleep categories. Statistical analysis like linear regression could explore this further.

#Research Question: Is there a significant relationship between hours of sleep on school nights and weight among high school students?

#Hypothesis Formulation:

#Null Hypothesis (H0): No relationship between hours of sleep on school nights and weight (slope = 0).

#Alternative Hypothesis (Ha): Significant relationship between hours of sleep on school nights and weight (slope ≠ 0).

#Assumptions: Linear relationship, normally distributed residuals, constant variance.

#Significance Level: α = 0.05.

#Statistical Results:

#Calculate the p-value for the regression slope: If p < 0.05, reject H0, indicating a significant relationship.

#Interpret the confidence interval for the slope to understand the direction and magnitude.

#Conclusion in Plain Language: If a significant relationship is found, it suggests that the amount of sleep on school nights impacts students' weight. A positive or negative slope would indicate whether more sleep is associated with higher or lower weight. If no significant relationship is found, sleep may not be a major factor in determining weight among these students