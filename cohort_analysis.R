library(tidyverse)
library(ggthemes)
library(janitor)
library(lubridate)

# ggplot theme
old <- theme_set(theme_tufte() + theme(text = element_text(family = 'Menlo')))

# from UCI repository
raw <- readxl::read_xlsx("data.xlsx") %>% clean_names()

# Tidy up data ---- 


# count multiple items as one invoice
by_invoice <- raw %>% 
    select(customer_id, invoice_date) %>% 
    mutate(invoice_date = as.Date(invoice_date)) %>% 
    # start from year 2011
    # remove invalid customer id
    filter(year(invoice_date) > 2010, !is.na(customer_id)) %>% 
    distinct()

# count multiple purchases as one customer
by_customer <- by_invoice %>% 
    group_by(customer_id) %>% 
    # customer's first seen
    mutate(cohort = min(invoice_date)) %>% 
    # make ordered factor here ---
    mutate_at(c("invoice_date", "cohort"), 
              ~ factor(month.abb[month(.)], levels = month.abb, ordered = TRUE)) %>% 
    ungroup() %>% 
    distinct()


# Cohort Analysis ---------------------------------------------------------


# for easy cohort plotting 
df <- by_customer %>%
    group_by(cohort, invoice_date) %>%
    summarise(n = n()) %>%
    # tag index month using factor for convenience
    mutate(
        ind = 1:length(invoice_date),
        ind = factor(
            ind,
            levels = 1:12,
            labels = paste0("mth.", 1:12),
            ordered = TRUE
        )
    ) %>%
    mutate(
        # retention of cohort
        retain = n / first(n),
        # percentage of cohort
        pct = n / sum(n),
        # compare to previous period
        diff = n - lag(n),
    ) %>%
    ungroup()


# Plot -------------------------------------------------------------------


# plot result via heatmap
df %>% 
    ggplot(aes(ind, forcats::fct_rev(cohort))) + 
    geom_raster(aes(fill = retain)) + 
    geom_text(aes(label = scales::percent(retain)), 
              col = 'white', size = 3) + 
    scale_fill_viridis_c(guide = 'none') +
    labs(x = "Month", y = "")
    
