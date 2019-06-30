library(tidyverse)
library(janitor)
library(arules)
library(arulesViz)

# Prepare Data ----

raw <- readRDS("raw.rds") %>% clean_names()

# slice data frame by invoice
invoices <- raw %>% 
    # remove invalid customer id 
    # remove invalid invoice
    filter(!is.na(customer_id), !str_detect(invoice_no, "[AC]")) %>% 
    select(invoice_no, description) %>% 
    # remove multiple entries
    distinct() %>% 
    mutate(invoice_no = as.factor(invoice_no)) %>% 
    split(.$invoice_no)

# convert list to transactions S4
trans <- map(invoices, ~ as.vector(.$description)) %>% as("transactions")
summary(trans)

## EDA ----

# the most frequent bought items
itemFrequencyPlot(trans, support = 0.05, cex.names = 0.8)

# run algo
rules <- apriori(trans, parameter = list(support = 0.005, confidence = 0.5))
summary(rules)

# output
inspect(head(rules, n = 3, by = "lift"))

## Visualization ----

# support, confidence, lift
plot(rules, measure = c('support', 'lift'), shading = "confidence", jitter = 0.1)

# Unwin, Hofmann, and Bernt (2001)
plot(rules, method = "two-key plot", jitter = 0.1)

# graph based method
plot(head(rules, 10, by = "lift"), method = "graph")
