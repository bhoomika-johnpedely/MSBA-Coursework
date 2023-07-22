library(tidyverse)
library(rpart)
library(rpart.plot)

# Load the data
m <- read.csv("megatelco.csv")

# Clean the data
m_clean <- m %>% 
  mutate(reported_satisfaction = factor(reported_satisfaction,
                                        levels = c("low","avg", "high"),
                                        ordered = T),
         reported_usage_level = factor(reported_usage_level,
                                       levels = c("low",  "avg", "high"),
                                       ordered = T),
         considering_change_of_plan = factor(considering_change_of_plan,
                                             levels = c("no", "maybe", "yes")),
         leave = factor(leave),
         college = ifelse(college == "one", 1, 0)) %>% 
  filter(income > 0,
         house > 0,
         handset_price < 1000) %>% 
  na.omit %>% 
  select(-id)

# Inspect data
glimpse(m_clean)

ggplot(data = m_clean, aes(x = leave, y = house)) +
  geom_boxplot() +
  labs(title = "leave ~ house")
# Fit tree
(tree_model <- rpart(formula = leave ~ house, 
                     data = m_clean))
# Plot tree object
rpart.plot(x = tree_model)
tree_model
(predict(object = tree_model, type = "class") == m_clean$leave) %>% 
  mean
# Fit tree
(tree_model2 <- rpart(formula = leave ~ ., 
                      data = m_clean))
# Visualize
rpart.plot(x = tree_model2)
rpart.plot(tree_model2, tweak = 1, roundint=T)
# Evaluate accuracy
(predict(tree_model2, type = "class") == m_clean$leave)  %>% 
  mean
