---
title: "Hot Lead Scoring"
author: "Kevin Kusuma"
date: "2/22/2021"
output: html_document
---

# Data
```{r setup, include=FALSE}
domo <- rdomo::Domo()
library(tidyverse)
library(tictoc)
library(caret)
library(caTools)
library(dataPreparation)
library(ROSE)
library(rminer)
lead$Label %>% table()
# --------------------------------------------------------------------------------------------------------
# Lead Data
tic()
lead <- 
  domo$ds_query('cb038ea0-31bf-4cee-92fd-a5a616898f83', 
  'select `Contact ID`, `Contact Email`, 
  case when `StageDate Converted` is not null then \'Converted\' else \'Rejected\' end as Label,
  date(IFNULL(`StageDate Converted`, IFNULL(`StageDate Rejected`, `StageDate Disqualified`))) `Label Date`
    from table
    where date(IFNULL(`StageDate Converted`, IFNULL(`StageDate Rejected`, `StageDate Disqualified`))) >= date_sub(date(now()), interval 6 month) and `Contact ID` is not null
  group by 1,2,3,4')

# Some leads have multiple labels, we only take the latest label status
lead2 <- 
  lead %>% 
  group_by(`Contact ID`, `Contact Email`) %>% 
  mutate(rank = rank(desc(`Label Date`))) %>%  # Only take the latest lead status
  filter(rank == 1) %>%  select(-rank)

# Conversion Rate
lead2$Label %>%  table()                    # Converted 2629    | Rejected 156468
lead2$Label %>%  table() %>% prop.table()   # Converted 0.0165  | Rejected 0.983


# --------------------------------------------------------------------------------------------------------
# Web Data
web_visit <- 
  domo$ds_query('c68832da-6d10-429b-bf97-4bc6990448eb', 
  'select `c_id`, `c_email`, date, COUNT(DISTINCT `page_v1_evar1`) `web_click_count` 
  from table
  where date(date) >= date_sub(date(now()), interval 7 month) and c_id is not null
  group by 1,2,3 order by 1')

web_visit2 <- 
  inner_join(web_visit, lead2, by = c('c_id'='Contact ID', 'c_email'='Contact Email')) %>%   
  mutate(`Label Date` = as.Date(`Label Date`, format = "%Y-%m-%d"),
         date = as.Date(date, format = "%Y-%m-%d"))

# Obtain the difference between the date when a lead is converted/rejected and the activity dates
web_visit2$diff <- 
  web_visit2$`Label Date` - web_visit2$date

# Only include the last 14-day activities from the date a lead is converted/rejected
web_visit2 <- 
  web_visit2 %>% 
  group_by(c_id, c_email) %>% 
  summarise(web_click_count = sum(ifelse(diff>=0 & diff<=14, web_click_count,0))) %>% 
  mutate(web_click_count = replace_na(web_click_count, 0))


# --------------------------------------------------------------------------------------------------------
# Email Data
email <- 
  domo$ds_query('39bbc6ae-e1e9-4cfc-a3af-d5e0c7f7ceca',
  'select `c_id`, `c_email`, date(`activitydate`) date,
  COUNT(DISTINCT case when `activitytype`=\'EmailOpen\' then date(`activitydate`) end) open_count,
  COUNT(DISTINCT case when `activitytype`=\'EmailClickthrough\' then date(`activitydate`) end) click_count
  from table where c_id is not null and activitytype in (\'EmailOpen\', \'EmailClickthrough\') and date(activitydate) >= date_sub(date(now()), interval 7 month)
  group by 1,2,3 order by 1,2,3')

email2 <- 
  inner_join(email, lead2, by = c('c_id'='Contact ID', 'c_email'='Contact Email')) %>% 
  mutate(`Label Date` = as.Date(`Label Date`, format = "%Y-%m-%d"),
         date = as.Date(date, format = "%Y-%m-%d"))

# Obtain the difference between the date when a lead is converted/rejected and the activity dates
email2$diff <- 
  email2$`Label Date` - email2$date

# Only include the last 14-day activities from the date a lead is converted/rejected
email2 <- 
  email2 %>% 
  group_by(c_id, c_email) %>% 
  summarise(open_count = sum(ifelse(diff>=0 & diff<=14, open_count,0)),
            click_count = sum(ifelse(diff>=0 & diff<=14, click_count,0))) %>% 
  mutate(open_count = replace_na(open_count,0), click_count = replace_na(click_count,0))


# --------------------------------------------------------------------------------------------------------
# Joins
join_data <- 
  inner_join(lead2, web_visit2, by = c('Contact ID' = 'c_id', 'Contact Email' = 'c_email'))
  
# Label, Web Visit, and email
join_data2 <-
  inner_join(join_data, email2, by = c('Contact ID' = 'c_id', 'Contact Email' = 'c_email')) %>% 
  arrange(`Contact ID`) %>% 
  mutate(web_click_count = replace_na(web_click_count, 0),
         open_count = replace_na(open_count, 0),
         click_count = replace_na(click_count, 0))

join_data2 <- 
  join_data2[,c(-1,-2,-4)] %>% 
  mutate(Label = factor(Label, levels = c('Rejected','Converted'))) %>% 
  filter(open_count >= click_count)

# Conversion rate of the final data
join_data2$Label %>% table()                      # Converted 629     | Rejected 2202
join_data2$Label %>%  table() %>% prop.table()    # Converted 0.222   | Rejected 0.777
toc()
```

# Joins
```{r}
# Label and Web Visit
tic()
join_data <- 
  inner_join(lead2, web_visit2, by = c('Contact ID' = 'c_id', 'Contact Email' = 'c_email'))
  
# Label, Web Visit, and email
join_data2 <-
  inner_join(join_data, email2, by = c('Contact ID' = 'c_id', 'Contact Email' = 'c_email')) %>% 
  arrange(`Contact ID`) %>% 
  mutate(web_click_count = replace_na(web_click_count, 0),
         open_count = replace_na(open_count, 0),
         click_count = replace_na(click_count, 0))

join_data2 <- 
  join_data2[,c(-1,-2,-4)] %>% 
  mutate(Label = factor(Label, levels = c('Rejected','Converted'))) %>% 
  filter(open_count >= click_count)

join_data2$Label %>% levels()
toc()


# Correlations
join_data2 %>% 
  mutate(Label = ifelse(Label == 'Converted', 1,0)) %>% 
  cor()
```

# 1st Logistic Regression
```{r}
log_model_1a <- 
  train(Label ~ ., 
        join_data2,
        method = 'glm',
        metric = 'ROC',
        trControl = trainControl(
          method = "cv",
          number = 5,
          summaryFunction = twoClassSummary,
          classProbs = T,
          verboseIter = T))

summary(log_model_1a)

p_1a <- predict(log_model_1a, join_data2, type = 'prob')
colAUC(p_1a , join_data2$Label, plotROC = TRUE)

```

# 2nd Logistic Regression with Point System
```{r}
# Point System
join_data2a <- 
  join_data2 %>% 
  mutate(web_click_count = web_click_count*50,
         open_count = open_count*5,
         click_count = click_count*100)

log_model_1b <- 
  train(Label ~ ., 
        join_data2a,
        method = 'glm',
        metric = 'ROC',
        trControl = trainControl(
          method = "cv",
          number = 5,
          summaryFunction = twoClassSummary,
          classProbs = T,
          verboseIter = T))

summary(log_model_1b)
```

```{r point_sim}
sim_data <- join_data2a %>%
    mutate(total_points=web_click_count + open_count + click_count,
           w=case_when(Label == 'Converted' ~ 1,TRUE ~ 0))
 
sim_data$fit_prob_orig <- predict(log_model_1a,newdata=join_data2,type='prob')[,'Converted']
sim_data$fit_raw_orig <- predict(log_model_1a,join_data2,type='raw')
sim_data$fit_prob_scaled <- predict(log_model_1b,sim_data,type='prob')[,'Converted']
sim_data$fit_raw_scaled <- predict(log_model_1b,sim_data,type='raw')
 
sim_data <- sim_data %>% mutate(prob_bands=ceiling((fit_prob_orig*100)/10)*10) %>%
    mutate(point_bands=ceiling((total_points)/50)*50)
 
sim_data %>% group_by(point_bands) %>% summarise(w=mean(w))
 
sim_data %>%
    count(Label,fit_raw_orig) %>%
    group_by(Label) %>%
    mutate(t=sum(n),p=scales::percent(n/t)) %>%
    pivot_wider(names_from=fit_raw_orig,values_from=c(n,t,p)) %>%
    select(-t_Rejected,-t_Converted)
 
sim_data %>% group_by(Label,prob_bands) %>% summarise(min_p=min(total_points),max_p=max(total_points),.groups='drop')
 
ggplot(sim_data) +
    geom_boxplot(aes(x=Label,y=fit_prob)) +
    theme_bw()
 
ggplot(sim_data) +
    geom_point(aes(x=total_points,y=fit_prob_orig,color=Label)) +
    # facet_wrap(~Label) +
    geom_hline(yintercept=0.25) +
    geom_vline(xintercept=200) +
    theme_bw()
```
