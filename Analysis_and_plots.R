##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Aland publication code
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

pck <- c("data.table", "dplyr", "stringr", "magrittr", "openxlsx", "sf", 
         "vegan", "bipartite", "cli", "ggplot2", "lavaan", "piecewiseSEM", 
         "semPlot", "emmeans", "multcomp")

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
dat_int <- read.xlsx() %>%
  filter(!is.na(Insect_Family) & Insect_Family != "? Is with flies in Finland")## Interaction data
dat_cov <- read.xlsx("plant_data.xlsx", sheet = 1) ## Coverage data
dat_con <- read.xlsx("all_connectivity_measures_Lena_final.xlsx", sheet = 1) %>% ## Connectivity
  dplyr::select(- plot) %>% distinct() %>% ## As each plot has the same values I only need the distinct values
  rename(locality_2 = patch_nr) ## rename column so I can use join later

## dat_int has one wrong size for an area in the data
dat_int <- dat_int %>% mutate(are = case_when(locality_2 == "703" ~ 11596.3, TRUE ~ are))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    2. Combine the data sets to one----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- dat_cov %>% filter(in_interaction == 1) %>% ## Only use species that are interacted with
  group_by(locality_2) %>% 
  summarise(total_cov = sum(coverage)) %>% ## calculate total coverage of the species
  right_join(dat_int, by = "locality_2") %>% ## join with the transect data
  right_join(dat_con %>% dplyr::select(-c(area, near_dist)), by = "locality_2")

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

## Without the grazing Insects
all_top_ins <- dat %>% filter(!is.na(Insect_Family)) %>%
  group_by(Insect_Family) %>% 
  summarise(Ins_inter = n()) %>% slice_max(Ins_inter, n = 10)

## Insect plot
top10_ins <- ggplot(all_top_ins, 
                    aes(x = reorder(Insect_Family, desc(Ins_inter)), y = Ins_inter)) + 
  geom_bar(stat = "identity", fill = "#E69F00", color = "black", width = 0.8) +
  labs(x = "", y = "Number of interactions", title = "a) Top ten insect families") + 
  scale_y_continuous(limits = c(0, 500)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.ticks = element_line(color = "black"), 
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        panel.grid = element_blank())

## Without the grazing Plants
all_top_pla <- dat %>% filter(!is.na(Insect_Family)) %>%
  group_by(plant) %>%
  summarise(pla_inter = n()) %>%
  slice_max(pla_inter, n = 10)
  
top10_pla <- ggplot(all_top_pla, aes(x = reorder(plant, desc(pla_inter)),
                        y = pla_inter)) +
  geom_bar(stat = "identity", fill = "#228B22", color = "black", width = 0.8) +
  labs(x = "", y = "Number of interactions", title = "b) Top ten plants") +
  scale_y_continuous(limits = c(0, 500)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.ticks = element_line(color = "black"), 
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        panel.grid = element_blank())
top10_pla  

test <- ggpubr::ggarrange(top10_ins, top10_pla, nrow = 2)

ggsave("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/top_comb.jpeg", 
       dpi = 300, height = 10, width = 8, units = "in", plot = test)

ggsave("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/top10_ins.jpeg", 
       dpi = 300, height = 4, width = 6, units = "in", plot = top10_ins)
ggsave("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/top10_pla.jpeg", 
       dpi = 300, height = 4, width = 6, units = "in", plot = top10_pla)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    4. Number of interactions vs coverage----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## First without grazing
cover_int <- dat %>% group_by(locality_2, total_cov) %>% 
  summarise(nr_inter = n())

total_cov_int <- ggplot(cover_int, aes(x = total_cov, y = nr_inter)) + geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = F) +
  labs(x = "Total plant coverage", y = "Number of interactions", title = "All plant species included") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

ggsave("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/total_cov_int.jpeg",
       width = 6, height = 4, dpi = 300, plot = total_cov_int)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        5. SEM----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sem_dat <- dat_cov %>% filter(present == 1 & in_interaction == 1) %>%
  group_by(locality_2) %>% 
  summarise(nr_pla_spe = sum(present)) %>% ## calculate the diversity of plants interacted with
  right_join(cover_int, by = "locality_2")

dat_int %>% group_by(locality_2) %>% summarise(nr_int = n())

sem_dat <- dat %>% dplyr::select(locality_2, Insect_Family, connect1500, connect_decay1000,
                                 connect_buffer, are, grazing) %>%
  distinct() %>% 
  filter(!is.na(Insect_Family)) %>% group_by(locality_2, connect1500, connect_decay1000, 
                                             connect_buffer, are, grazing) %>%
  summarise(nr_ins_fam = n()) %>%
  right_join(sem_dat, by = "locality_2") %>%
  mutate(log_area = log(are), 
         connect1500 = as.numeric(connect1500),
         connect_decay1000 = as.numeric(connect_decay1000), 
         connect_buffer = as.numeric(connect_buffer))
 
sem_out <- psem(glm.nb(nr_pla_spe ~ grazing + log_area + connect_decay1000, data = sem_dat), 
     lm(total_cov ~ grazing + nr_pla_spe + log_area + connect_decay1000, data = sem_dat), 
     glm.nb(nr_inter ~ nr_pla_spe + total_cov + log_area + connect_decay1000+ grazing, data = sem_dat), 
     glm.nb(nr_ins_fam ~ nr_pla_spe + nr_inter + log_area + connect_decay1000 + grazing, data = sem_dat)) 

summary(sem_out)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        6. Make figures for the significant links in SEM----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Number of plant species against connectivity
pla_con_pl <- ggplot(sem_dat, aes(x = connect_decay1000, y = nr_pla_spe)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(y = "Plant species richness", x = "Connectivity", 
       title = "a)") +
  scale_y_continuous(limits = c(0, 30)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

## Number of plant species in different grazing types
mean_nr_pla <- sem_dat %>% group_by(grazing) %>% summarise(nr_pla = mean(nr_pla_spe), 
                                                           se = se(nr_pla_spe))

pla_gra_pl <- ggplot(mean_nr_pla, aes(x = grazing, y = nr_pla)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = nr_pla - se, ymax = nr_pla + se), width = 0.15) +
  geom_jitter(data = sem_dat, mapping = aes(x = grazing, y = nr_pla_spe), 
              alpha = 0.1, width = 0.05) +
  scale_y_continuous(limits = c(0, 30)) + 
  labs(x = "", y = "", title = "b)") +
  scale_x_discrete(labels = c("grazed" = "Grazed", "ungrazed" = "Ungrazed")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_blank(),
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

## Total cover vs nr plant species
cov_nr_pla_pl <- ggplot(sem_dat, aes(x = nr_pla_spe, y = total_cov)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(x = "Plant species richness", y = "Total plant cover (%)", title = "c)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

## Total cover in different grazing types
mean_cov <- sem_dat %>% group_by(grazing) %>% summarise(cov_pla = mean(total_cov), 
                                                        se = se(total_cov))

cov_gra_pl <- ggplot(mean_cov, aes(x = grazing, y = cov_pla)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = cov_pla + se, ymax = cov_pla - se), width = 0.15) +
  geom_jitter(data = sem_dat, mapping = aes(x = grazing, y = total_cov), 
              width = 0.05, alpha = 0.1) +
  labs(x = "", y = "", title = "d)") +
  scale_x_discrete(labels = c("grazed" = "Grazed", "ungrazed" = "Ungrazed")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_blank(),
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

## number interactions vs total cover
int_cov_pl <- ggplot(sem_dat, aes(x = total_cov, y = nr_inter)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(x = "Total plant cover (%)", y = "Number of interactions", title = "e)") +
  scale_y_continuous(limits = c(0, 60)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

## number interactions vs log area
int_area_pl <- ggplot(sem_dat, aes(x = log_area, y = nr_inter)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(x = "Patch area (log)", y = "", title = "f)") +
  scale_y_continuous(limits = c(0, 60)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_blank(), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))
 
## number interactions vs connectivity
int_con_pl <- ggplot(sem_dat, aes(x = connect_decay1000, y = nr_inter)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(x = "Connectivity", y = "Number of interactions", title = "g)") + 
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))
  
## number of insect families vs number of interactions
fam_int_pl <- ggplot(sem_dat, aes(x = nr_inter, y = nr_ins_fam)) + geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(x = "Number of interactions", y = "Number of insect families", title = "h)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

sig_sem_links <- ggpubr::ggarrange(pla_con_pl, pla_gra_pl, 
                                   cov_nr_pla_pl, cov_gra_pl,
                                   int_cov_pl, int_area_pl,
                                   int_con_pl, fam_int_pl, 
                                   ncol = 2, nrow = 4, align = "hv")

ggsave("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/sig_sem_links.jpeg", 
       dpi = 300, device = "jpeg", width = 10, height = 10)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        7. Network graph and H2----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## First for everything
dat_int$dummy <- "Åland pollinator" ## I need to set that so I can use frame2webs to define one big group

big_network <- frame2webs(dat_int, varnames = c('plant', 'Insect_Family', "dummy"))


png("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/network_plot.png", 
    width = 15, height = 18, units = "in", res = 300)
plotweb(big_network$`Åland pollinator`, text.rot = 90, labsize = 2)
dev.off()

## Calculate the H2 value of this Network 
networklevel(big_network$`Åland pollinator`, index = "H2")

## Consider grazing and ungrazed
herbivory_networks <- frame2webs(dat_int, 
                                 varnames = c("plant", "Insect_Family", "grazing"))
plotweb(herbivory_networks$grazed, text.rot = 90)
plotweb(herbivory_networks$ungrazed, text.rot = 90)

networklevel(herbivory_networks$grazed, index = "H2")
networklevel(herbivory_networks$ungrazed, index = "H2")

## Get H2 for each site and get mean plus se around it 
local_web <- frame2webs(dat_int, varnames = c('plant', "Insect_Family", "locality_2"))
 
h2_all <- lapply(local_web, function(x) networklevel(x, index = "H2")) %>% 
  unlist() %>% as.data.frame() %>% 
  rename(H2 = ".") %>%
  mutate(locality_2 = strsplit(rownames(.), "\\.") %>% 
           unlist() %>% matrix(ncol = 2, byrow = T) %>% .[,1]) %>% 
  left_join(dat_int %>% dplyr::select(locality_2, grazing), by = "locality_2")

data.frame(mean_h2 = mean(h2_all$H2, na.rm = T), 
           se = se(h2_all$H2))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          Supplemental plots----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Coverage vs interaction for the two top species
## Achillea and Galium ver
nr_int <- dat %>% 
  group_by(locality_2) %>% 
  summarise(nr_inter = n())

## Achillea
int_cov_ach <- dat_cov %>% filter(species == "Achillea millefolium") %>% 
  dplyr::select(locality_2, coverage, species) %>% distinct() %>%
  right_join(nr_int, by = "locality_2")

ach_cov_int <- ggplot(int_cov_ach, aes(x = coverage, y = nr_inter)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = F) + 
  labs(x = "Plant coverage", y = "Number of interactions", 
       title = expression(paste("a) ", italic("Achillea millefolium")))) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        panel.grid = element_blank(),
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 17))

## Galium
int_cov_galver <- dat_cov %>% filter(species == "Galium verum") %>% 
  dplyr::select(locality_2, coverage, species) %>% distinct() %>%
  right_join(nr_int, by = "locality_2")


gal_cov_int <- ggplot(int_cov_galver, aes(x = coverage, y = nr_inter)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = F) + 
  labs(x = "Plant coverage", y = "", title = expression(paste("b) ", italic("Gallium verum")))) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"), 
        axis.ticks.x = element_line(color = "black"), 
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 18))

singl_sp_cov_pl <- ggpubr::ggarrange(ach_cov_int, gal_cov_int, align = "hv")

ggsave("C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/01_Figures/spec_cov_int.jpeg",
       width = 6, height = 4, dpi = 300, plot = singl_sp_cov_pl)
