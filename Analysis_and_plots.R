##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Aland publication code
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

pck <- c("dplyr", "ggplot2", "openxlsx", "gplots")

lapply(pck, function(pck){
  if(!require(pck, character.only = T))
    install.packages()
  
  else
    library(pck, character.only = T)
})

## Standard error function
se <- function(x){
  sd(x, na.rm = T)/sqrt(sum(!is.na(x)))
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    1. Read data----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_int <- read.xlsx("interaction_dat22_23.xlsx", sheet = 1) ## Interaction data
dat_cov <- read.xlsx("plant_data.xlsx", sheet = 1) ## Coverage data
dat_con <- read.xlsx("all_connectivity_measures_Lena_final.xlsx", sheet = 1) %>% ## Connectivity
  select(- plot) %>% distinct() %>% ## As each plot has the same values I only need the distinct values
  rename(locality_2 = patch_nr) ## rename column so I can use join later

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    2. Combine the data sets to one----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- dat_cov %>% filter(in_interaction == 1) %>% ## Only use species that are interacted with
  group_by(locality_2) %>% 
  summarise(total_cov = sum(coverage)) %>% ## calculate total coverage of the species
  right_join(dat_int, by = "locality_2") %>% ## join with the transect data
  right_join(dat_con %>% select(-c(area, near_dist)), by = "locality_2")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    3. Top plant and pollinator species----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Insects
top_ins <- dat %>% filter(!is.na(Insect_Family)) %>%
  group_by(grazing, Insect_Family) %>% 
  summarise(nr_int_ins = n()) %>% ## calculate how many interactions each family got
  arrange(desc(nr_int_ins)) %>% ## arrange so the highest are on top
  slice_head(n = 10) ## slice the top

## Plot the top 10 insects in each treatment
ggplot(top_ins, aes(x = reorder(Insect_Family, desc(nr_int_ins)), 
                    y = nr_int_ins)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ grazing, 
             labeller = labeller(grazing = c("grazed" = "Grazed",
                                             "ungrazed" = "Ungrazed"))) +
  labs(x = "", y = "Number of interactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"))
  
## Plants
top_pla <- dat %>% group_by(grazing, plant) %>% 
  summarise(nr_int_pla = n()) %>% ## calculate how many interactions each species got
  arrange(desc(nr_int_pla)) %>% ## arrange so the highest are on top
  slice_head(n = 10) ## slice the top

## Plot the top 10 plants in each treatment
ggplot(top_pla, aes(x = reorder(plant, desc(nr_int_pla)), y = nr_int_pla)) +
         geom_bar(stat = "identity") + 
  facet_wrap(~ grazing, 
             labeller = labeller(grazing = c("grazed" = "Grazed",
                                             "ungrazed" = "Ungrazed"))) +
  labs(x = "", y = "Number of interactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"))

## Get the balloon plots
## Insects
ins_inter <- dat %>% filter(!is.na(Insect_Family) & Insect_Family != "? Is with flies in Finland") %>%
  group_by(grazing, Insect_Family) %>% 
  summarise(nr_int_ins = n()) %>% ## calculate how many interactions each family got
  arrange(desc(nr_int_ins))

## Ballonplot insects
balloonplot(ins_inter$grazing, ins_inter$Insect_Family, ins_inter$nr_int_ins)

## Plants
pla_inter <- dat %>%
  group_by(grazing, plant) %>% 
  summarise(nr_int_pla = n()) %>% ## calculate how many interactions each family got
  arrange(desc(nr_int_pla))

## Balloonplot plants
balloonplot(pla_inter$grazing, pla_inter$plant, pla_inter$nr_int_pla)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        
