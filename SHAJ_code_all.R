# Introduction ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    TRAPP literature review
#                         Data filtering
#           Snæbjörn Helgi Arnarsson Jack (nem.sha1@lbhi.is)
#                supervisor Isabel C Barrio (isabel@lbhi.is)
#                 & Ingibjörg Svala Jónsdóttir (isj@hi.is)
#
#                          21-April-2025
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the aim of this review is to survey the literature for evidence 
# of trapped ecosystems after grazing-cessation across the tundra biome 

# with an emphasis on sheep and reindeer

# the literature search builds on the systematic review by Barbero-Palacios et al (2023)

# this script selects the studies to keep for the TRAPP literature review
# and synthesizes information from the data extracted from the articles retrieved

# Libraries----
library(readxl)
library(tidyverse)
library(ggOceanMaps)
library(ggspatial)
library(cowplot)
library(ggrepel)

# Initial data exploration ----
## Data import ----

coded_data_filtered <- read_csv("data/coded_data.csv") |>  # some warning messages here but it should be fine :)
  # we will use studies that experimentally remove grazing
  filter(diversity_contrast %in% c("exclosure")) %>% 
  # we will assess changes over at least 10 years
  filter((as.numeric(year_end) - as.numeric(year_start)) > 10) %>%  
  # NOTE: need to check also articles that report a single year (in case this is an error in extracting info from the study)
  # we will keep studies that exclude large herbivores
  filter(change.long %in% c("large_herbivores", "large_and_small_herbivores",
                            "large_and_medium_herbivores", 
                            "large_medium_and_small_herbivores"))

# check how many studies match the conditions
coded_data_filtered %>% distinct(article_ID) %>% summarize(n = n())


# the list of articles (article_ID) that you should read in more detail are:
coded_data_filtered %>% distinct(article_ID)

# how many measured response variables
coded_data_filtered |> 
  count()


# pdf files for all articles are in the shared folder, organized by their article_ID

## How many articles per measured response variable ----

# We want to know which variables have been measured the most times. If a vari-
# able has been measured in four or more (n >= 4) articles then we are inter-
# ested in looking at it.

# group table by the new response variables (a grouping of the original variable
# names) Because an article might measure a variable more than once, it is poss-
# ible to use the distinct function to filter out for articles that measure it 
# at least once. We create a new dataframe where we have counted how many each 
# variable appears in a distinct article then we filter out the ones that appear
# in more than four articles, arrange in order of descending frequency then we 
# end by printing the table in the console and storing it in a new dataframe.

coded_data_filtered |> 
  group_by(measured_response_variable_new) |> 
  distinct(article_ID) |> 
  summarise(
    n = n()
  ) |> 
  filter(n >= 4) |> 
  arrange(desc(n)) |> 
  rename(var = measured_response_variable_new) |> 
  print(n = Inf) -> trapped_variables

# The total number of variables we want to look at for 10+ years of exclosure are 
# 6. But now the total number of articles might have gone down from 35.

# begin by grouping the table by articla_Id (the opposite of the last sentence)
# then filter out only where there is a match between the variables we want to 
# focus on summarise by counting how many response variables correspond to each 
# article_ID and then count to get the total number of articels, this will show 
# us how many studies are not of interest.

## Most measured variables ----

# Overview of response_variable_groups and the number of rows studies in each group

coded_data_filtered |> 
  filter(
    str_detect(response_variable_group, "^plant") | 
      str_detect(response_variable_group, "^soil")  |
      str_detect(response_variable_group, "^ecosystem")
  )|> # look for groups that have to do with plant or soil or ecosystem variables
  arrange(response_variable_group) |>
  summarise(
    n   = n(),
    .by = response_variable_group
  ) |> 
  arrange(desc(n))


# Results ----

# After initial data exploration I exctracted the appropriate variables 
# from each study and logged in to an excel spreadsheet. 

method_results <- read_excel(
  "data/data_synthesis.xlsx",
  # add special col types
  col_types = c("text", # article_id
                "numeric", # site_id
                "numeric", # coord_N
                "numeric", # coord_E
                "text", #region
                "numeric", # year_start
                "numeric", # year_end
                "text", # abundance
                "text", # significance
                "text", # higher_value
                "text" # more_info
  )
)

# fix the significance value to be single word with no spaces

method_results$significance <- replace(method_results$significance, 
                                       method_results$significance == "not significant", 
                                       "non_significant")

# rename coordinates to latitude and longitude

method_results <- rename(method_results, long = coord_E, lat = coord_N)

# factorise for some reason

method_results$region <- as.factor(method_results$region)
method_results$abundance <- as.factor(method_results$abundance)
method_results$significance <- as.factor(method_results$significance)
method_results$higher_value <- as.factor(method_results$higher_value)

## Calculate No-Change Index ----
# here I calculate the no-change index that shows the porportions of no changes
# vs. changes in abundance. This information will be used to graphically repre-
# sent my data on a map. 

changed_df <- method_results |> 
  group_by(site_id, long, lat, significance, year_start, year_end) |> 
  summarise(
    n = n()
  ) |> 
  pivot_wider(
    names_from = significance,
    values_from = n
  ) |> 
  replace_na(list(non_significant = 0, significant = 0)) |> 
  mutate(
    no_change_index = non_significant / (non_significant + significant),
    no_change_size = non_significant + significant
  ) |> 
  select(-non_significant, -significant) |> 
  transform_coord(lon = "long", lat = "lat", bind = TRUE)

## Plots ----

# Barplot summarising my main findings for changes in relative abundance for
# each vegetation group.

method_results |> 
  group_by(abundance, significance) |> 
  summarise(
    n = n()
  ) |> 
  ggplot(aes(fct_reorder(abundance, n))) +
  geom_bar(aes(y = n, fill = significance), stat = 'identity', position = 'dodge') +
  labs(
    title = "",
    x = "", y = "No. Studies"
  ) +
  guides(
    fill = guide_legend(title="Abundance:")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1.1),
    panel.grid.major = element_line(colour = "black"), 
    panel.background = element_blank()
  ) + 
  scale_fill_manual(values = c("#ef8a62", "#998ec3"), labels = c("no change", "change")) +
  lims(y = c(0,15))

# Barplot summarising which changed variables were higher in the exclosure vs.
# the control plots

method_results |> 
  filter(significance != "non_significant") |> 
  group_by(abundance, higher_value) |> 
  summarise(
    n = n()
  ) |> 
  ggplot(aes(fct_reorder(abundance, n))) +
  geom_bar(aes(y = n, fill = higher_value), stat = 'identity', position = position_dodge2(preserve = "single")) +
  labs(
    # title = "Difference in mean abundnance between control and exclosure",
    x = "", y = "No. Studies"
  ) +
  guides(
    fill = guide_legend(title="Mean \nabundance \nhigher in:") 
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1.1),
    panel.grid.major = element_line(colour = "black"), 
    # panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + 
  scale_fill_manual(values = c("#ef8a62", "#998ec3")) +
  lims(y = c(0,15))

## Maps ----

# This is a map that represents the geographical spread of the studies included

gr_map <- basemap(60, grid.col = NA, glaciers = TRUE) + 
  # here it is ok to use the normal long lat colums as coordinates
  geom_point(data = changed_df, 
             aes(x = lon.proj, y = lat.proj, fill = no_change_index, size = no_change_size), 
             shape = 21,
             color = "black",
             stroke = 0.1
  ) +
  scale_fill_gradientn(colors=c("#998ec3","white","#ef8a62"), name = "No-change Index") +
  geom_label_repel(
    data = changed_df, aes(x = lon.proj, y = lat.proj, label = site_id), max.overlaps = 20
  ) +
  scale_size_continuous(
    range = c(2, 8),  
    breaks= c(1, 3, 5, 7, 9, 13), 
    name="No. \nStudies") +
  labs(
    title = "" 
  )

gr_legend <- ggplot(method_results, aes(x = "", fill = significance)) +
  # leave the x empty and use only the significance in the fill parameter
  # use geom bar, positoin fill 
  geom_bar(position = "fill") +
  # polarise on the y field (the height)
  coord_polar("y") +
  # theme void is nice, reduces clutter
  theme_void() +
  theme(legend.position="bottom") +
  # facet_wrap so that there is one graph for each artivle
  facet_wrap(
    ~site_id, 
    nrow = 1
  ) +
  labs(
    fill = "Abundance:"
  ) +
  scale_fill_manual(values = c("#ef8a62", "#998ec3"), labels = c("No change", "Change"))

pan_arctic <- plot_grid(gr_map, gr_legend, ncol = 1, align = "h", rel_heights = c(0.7, 0.3))

pan_arctic

## Tables ----

# here is a table that counts how many studies were gathered for each vegetation
# group for each location

method_results |> 
  group_by(site_id, abundance) |> 
  summarise(
    n = n()
  ) |> 
  pivot_wider(
    names_from = abundance,
    values_from = n
  )

# here is a table summarising the percentage of changed vs. no change abundance

method_results |> 
  group_by(abundance, significance) |> 
  summarise(
    n = n()
  ) |> 
  pivot_wider(
    names_from = significance,
    values_from = n
  ) |> 
  mutate(
    percent_no_change = non_significant / (non_significant + significant) * 100,
    percent_changed = significant / (non_significant + significant) * 100,
    count = non_significant + significant
  ) |> 
  select(abundance, count, percent_no_change, percent_changed) |> 
  arrange(desc(percent_no_change))

