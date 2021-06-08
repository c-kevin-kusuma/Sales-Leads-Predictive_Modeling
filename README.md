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

# Cleaned Data From DOMO
```{r, warning=F, message=F}
# Pull Data from "Leads Conversion | Predictive Data Source"
pred_data <-
  domo$ds_query('71c9a7ae-ba0a-491b-83d3-4fbc9d5cdcb2',
      'select * from table where label in (\'Converted\', \'Disqualified\')')

head(pred_data[1:5,1:5])
```

# Function To Break The Data To Training and Testing
```{r}
preProcess_function <- function(x,y){
  x <- x %>% filter(cohort == y)

  set.seed(212)
  partition <- createDataPartition(x$label, p = 0.7, list=FALSE) #70:30 prop
  training <- x[partition, ] %>% mutate(type = 'training')
  testing <- x[-partition, ] %>% mutate(type = 'testing')
  x <- rbind(training, testing)[,-c(115:121, 202:224)] # Remove unnecessary cols

  return(x)
}
```

# Training Data to Push to DOMO for Static Reference
```{r}
apac_pred <- preProcess_function(pred_data, 'APAC')
emea_mktg_pred <- preProcess_function(pred_data, 'EMEA Mktg')
emea_sales_pred <- preProcess_function(pred_data, 'EMEA Sales')
noa_mktg_pred <- preProcess_function(pred_data, 'NoA Mktg')
noa_sales_pred <- preProcess_function(pred_data, 'NoA Sales')
japan_pred <- preProcess_function(pred_data, 'Japan') 
other_pred <- preProcess_function(pred_data, 'Other')

all_pred <- rbind(apac_pred, emea_mktg_pred, emea_sales_pred, noa_mktg_pred,
                  noa_sales_pred, japan_pred, other_pred)

# Leads Conversion | Static Training Data
# domo$ds_update('cd1e8033-3376-48d5-80c5-433f0f90ef76', all_pred)
```

# AutomML Result Data
```{r}
# Get data from "Leads Conversion | Predictive Results"
data <- 
  domo$ds_query('c1716b1a-975f-44c9-a6ba-704c32274246', 
        'select * from table where label <> \'N/A\'')
data <- data %>% rename(probability = `_probability_`)

noa_mktg <- data %>% filter(cohort == 'NoA Mktg') %>% 
  mutate(label = factor(label, levels = c('Converted', 'Disqualified')))

noa_sales <- data %>% filter(cohort == 'NoA Sales') %>% 
  mutate(label = factor(label, levels = c('Converted', 'Disqualified')))
```

# ROC
NAM Marketing is reasonably not overfit.
NAM Sales is reasonably not overfit.
```{r}
# Marketing
colAUC(filter(noa_mktg, type == 'training')$probability, 
       filter(noa_mktg, type == 'training')$label) # 0.8941

colAUC(filter(noa_mktg, type == 'testing')$probability, 
       filter(noa_mktg, type == 'testing')$label) # 0.87783


# Sales
colAUC(filter(noa_sales, type == 'training')$probability, 
       filter(noa_sales, type == 'training')$label) # 0.83436

colAUC(filter(noa_sales, type == 'testing')$probability, 
       filter(noa_sales, type == 'testing')$label) #0.82701
```

# Cutoff Function
```{r}
cutoff_function <- function(x,y,z){
  x <- x %>% filter(type == y & label == 'Converted' & cohort == z) %>% 
    select(cohort, label, lead_id, probability) %>% arrange(probability)
  x <- x[1:5, ]
  return(x)
}
```

# Cutoffs
```{r}
cutoff_function(data, 'testing', 'APAC') # 0.000027271
cutoff_function(data, 'testing', 'Japan') # 0.30407
cutoff_function(data, 'testing', 'NoA Mktg') # 0.083415
cutoff_function(data, 'testing', 'NoA Sales') # 0.12039
cutoff_function(data, 'testing', 'EMEA Mktg') # 0.16016	
cutoff_function(data, 'testing', 'EMEA Sales') # 0.0017171
cutoff_function(data, 'testing', 'Other') # 0.056997
```

# Confusion Matrix Function
```{r}
conf_function <- function(x,y,z){
  x <- x %>% filter(type == y)
  
  pred <- ifelse(x$probability >= z, 'Converted', 'Disqualified') %>% 
    factor(levels = c('Converted', 'Disqualified'))
  
  ref <- x$label
  
  return(
    confusionMatrix(pred, ref, positive = 'Converted')
    )
}
```

# Cutoffs
```{r}
# Marketing
filter(noa_mktg, type == 'testing') %>% 
  select(label, lead_id, probability) %>% arrange(probability) # 0.07957285

conf_function(noa_mktg, 'testing', 0.07957284)
filter(noa_mktg, type == 'testing') %>% nrow()

# Sales
filter(noa_sales, type == 'testing' & label == 'Converted') %>% 
  select(label, lead_id, probability) %>% arrange(probability) # 0.1206277

conf_function(noa_sales, 'testing', 0.1206277)
filter(noa_sales, type == 'testing') %>% nrow()

# Combined
conf_function(rbind(noa_sales, noa_mktg), 'testing', 0.5)

noa_mktg %>% filter(type == 'testing' & label == 'Converted') %>% 
  arrange(probability) %>% select(label, probability) %>% view()

noa_sales %>% filter(type == 'testing' & label == 'Converted') %>% 
  arrange(probability) %>% select(label, probability) %>% view()
```


# Web and Email Data
```{r}
web_email_data <- rbind(noa_sales, noa_mktg) %>%   
  select(label, web_click_count, web_visit_count, email_open_count,
         mql_web_click_count, mql_web_visit_count, mql_email_open_count,
         mql_email_click_count, type)

set.seed(212)
registerDoSNOW(cl)
glm_model <- train(label ~.,
                   filter(web_email_data, type == 'training')[,-9],
                   method = 'glm',
                   metric = 'ROC',
                   trControl = trainControl(
                     method = 'cv',
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T,
                     verboseIter = T))

p <- predict(glm_model, web_email_data, type = 'prob')['Converted']
web_email_data$probability <- p$Converted

conf_function(web_email_data, 'testing', 0.5)
```

# Web & Email Data vs Target Variable
```{r}
# Marketing Correlations
mktg_cor <- filter(noa_mktg, type == 'testing') %>%   
  select(mql_web_click_count, mql_web_visit_count, mql_email_open_count,
         mql_email_click_count, web_click_count, web_visit_count, email_open_count,
         email_click_count,probability) %>% cor() %>% data.frame() %>% 
  rownames_to_column() %>% mutate(type = 'Mktg') %>% pivot_longer(cols = 2:10) %>% 
  filter(rowname != name & rowname == 'probability') %>% arrange(desc(value))

# Sales Correlations
sales_cor <- filter(noa_sales, type == 'testing') %>%   
  select(mql_web_click_count, mql_web_visit_count, mql_email_open_count,
         mql_email_click_count, web_click_count, web_visit_count, email_open_count,
         email_click_count,probability) %>% cor() %>% data.frame() %>% 
  rownames_to_column() %>% mutate(type = 'Sales') %>% pivot_longer(cols = 2:10) %>% 
  filter(rowname != name & rowname == 'probability') %>% arrange(desc(value))

# domo$ds_update("5e91c7f4-ed3a-4d5c-b7c7-d65647366a33", rbind(mktg_cor, sales_cor))
```

# Variable's Relationship with the Target Variable
```{r}
# Marketing
varimp_mktg <- 
  domo$ds_get('7176a0e2-247b-4bae-9aba-c6e7a3ce163b') # Varimp from AutoML

var_mktg <- 
  noa_mktg %>% 
  pivot_longer(cols = c(10:202), names_to = 'variable', values_to = 'value') %>% 
  filter(value <=1 & type == 'testing') %>% group_by(variable, value) %>% 
  summarise(avg_prob = mean(probability))

var_mktg_1 <- 
  filter(var_mktg, value == 1) %>% 
  inner_join(filter(var_mktg, value == 0), by = c("variable"="variable")) %>% 
  inner_join(varimp_mktg, by = c("variable"="Column name")) %>% 
  mutate(prob_diff = avg_prob.x - avg_prob.y) %>% 
  arrange(desc(`Feature Importance Value`)) %>% filter(prob_diff < 0)

write.csv(var_mktg_1, 'Variable Importance.csv')

# Sales
varimp_sales <- 
  domo$ds_get('9f9ea756-adc8-4f3e-acd1-ce866e5ad847') # Varimp from AutoML

var_sales <- 
  noa_sales %>% 
  pivot_longer(cols = 10:202, names_to = 'variable', values_to = 'value') %>% 
  filter(value <=1 & type == 'testing') %>% group_by(variable, value) %>% 
  summarise(avg_prob = mean(probability))

var_sales_1 <- 
  filter(var_sales, value == 1) %>% 
  inner_join(filter(var_sales, value == 0), by = c("variable"="variable")) %>% 
  inner_join(varimp_sales, by = c("variable"="Column name")) %>% 
  mutate(prob_diff = avg_prob.x - avg_prob.y) %>% 
  arrange(desc(`Feature Importance Value`))
```

