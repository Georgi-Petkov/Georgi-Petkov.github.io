library(gganimate)
library(gapminder)
str(migrants)
MIG <- read_csv("G:\\Rprojects\\OECD\\MIG_28122018222528727.csv")
dat_str <- get_data_structure("MIG") # check the structure of the data

dat_str[[3]] %>% 
  DT::datatable()
mig <- get_dataset("MIG",
                   start_time = 2007, end_time = 2016) # change time span for the data

saveRDS(mig,"mig.rds")
mig %>% 
  head() %>% 
  DT::datatable()


# Remove column with missing values ---------------------------------------



mig$OBS_STATUS <- NULL

# Filter out NA -----------------------------------------------------------


mig <- mig %>% 
  filter(!is.na(obsValue))

# Subset for inflow and outflow total -------------------------------------


mig_in_out <- mig %>% 
  filter(VAR %in% c("B11","B12"),GEN == "TOT")


# Clean mistakes ----------------------------------------------------------
mig_in_out %>% 
  filter(CO2 != "TOT" ,obsTime == 2010) %>% 
  filter(COU == "GBR") %>% 
  arrange(desc(obsValue))
  group_by(obsTime) %>% 
  summarise(sum(obsValue))
  unique(mig_in_out$CO2)
  ggplot(aes(obsTime,COU,fill= log(obsValue))) +
  geom_tile(na.rm = TRUE)


# Rename variables --------------------------------------------------------

mig %>% 
   filter(CO2 != "TOT",GEN == "TOT") %>% 
  transmute(birth_nat= factor(CO2),
            country= factor(COU),
            people= as.integer(obsValue),
            Year= as.integer(obsTime),
            direction= factor(ifelse(VAR == "B12","out","in"))) %>% 
    group_by(country,direction) %>% 
    summarise(sum(people))
  
  
  mig %>%
    filter(CO2 == "BGR")
  
  mig_country <- mig %>% 
   filter(CO2 != "TOT",GEN == "TOT") %>% 
   transmute(origin_country= factor(CO2),
             country= factor(COU),
             people= as.integer(obsValue),
             Year= as.integer(obsTime),
             direction= factor(ifelse(VAR == "B12","out","in"))) %>% 
    mutate(people=ifelse(direction == "out",people*(-1),people)) %>% 
    left_join(w_names, by= c("country"="adm0_a3")) %>%
    select(-geometry)  # remove geometry
    group_by(name,direction,continent,Year) %>% 
    summarise(people=sum(people)) %>% 
    filter(continent == "Europe") %>% 
    ggplot(aes(reorder(name,people),people,fill=direction)) +
    geom_point() +
    coord_flip() +
    facet_wrap(~Year,nrow = 1)
    
 
 




 world <- st_as_sf(rnaturalearth::countries110)
 europe <- dplyr::filter(world, region_un=="Europe" | name =='Turkey' & name != "Russia")

 eu_names <- europe %>% 
  select(adm0_a3,name)
 
 w_names <- world %>% 
   select(adm0_a3,name,continent,gdp_md_est,pop_est)
 
 
 ggplot(europe, aes(fill=gdp_md_est/1000)) +
   geom_sf(alpha=0.8,col='white') +
   coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
   hrbrthemes::import_titillium_web() +
   viridis::scale_fill_viridis(
     name='Median GDP \n(in Billions)', direction = -1, labels=scales::dollar) +
   labs(x=NULL, y=NULL, title=NULL,
        caption='Source: http://www.naturalearthdata.com/')
 

 
migrants_map <- migrants %>% 
  inner_join(Europe, by = c("country"= "adm0_a3"))


migrants_map %>% 
  filter(!is.na(people)) %>% 
  group_by(Year) %>% 
  ggplot(aes(reorder(name,abs(people)),people,fill=direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  
  theme(legend.position = "bottom",
        legend.spacing.x = unit(2.0, 'cm'),
        legend.direction = "horizontal") +
  guides(fill= guide_legend(title= "",reverse = TRUE),
         legend.text = element_text(margin = margin(t = 0.5))) +

  
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = '', y = '') +
  transition_time(Year) +
  ease_aes('linear')
anim_save("migration_bar", animation = last_animation())

# Animation points --------------------------------------------------------


mig_country %>%
  mutate(perc_of_pop= people/pop_est,pop_rank= rank(desc(pop_est))) %>%
  arrange(desc(perc_of_pop)) %>% 
  ggplot(aes(people,reorder(name,perc_of_pop),size=gdp_md_est,colour= direction)) +
  geom_point() +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = c(0.8, 0.2)) +
  guides(size=guide_legend(title="GDP")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=11),
        legend.background = element_rect( size=0.5, linetype="solid")) +
        geom_vline(xintercept = 0) +

  # Here comes the gganimate specific bits
  labs(title = 'Migration by year: {frame_time}',
       subtitle= "Inflows/outflows of foreign population.",x = '', y = '',
       caption = "Source: https://stats.oecd.org/") +
  transition_time(Year) +
  ease_aes('linear')
#Save the animated plot
anim_save("migration_point.gif", animation = last_animation())

# anim <- animate(animation = last_animation())
# magick::image_write(anim, path="migration_point.gif")


migrants_map %>% 
  filter(!is.na(people)) %>% 
  group_by(Year) %>% 
  summarise(mean(people))
  ggplot(aes(reorder(name,people),people,fill=direction)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(3.0, 'cm'),
        legend.text = element_text(margin = margin(t = 0.5))) +
  guides(fill = guide_legend(title = "",reverse=T,
                             label.position = "bottom",
                             title.position = "left", title.vjust = 1))


# Animated map ------------------------------------------------------------

migrants_map %>% 
  filter(Year == 2016) %>% 
  mutate(perc_of_pop= people/pop_est,pop_rank= rank(desc(pop_est))) %>%
  ggplot( aes(fill=people)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  hrbrthemes::import_titillium_web() +
  viridis::scale_fill_viridis(
    name='Median GDP \n(in Billions)', direction = -1, labels=scales::dollar) +
  labs(x=NULL, y=NULL, title=NULL,
       caption='Source: http://www.naturalearthdata.com/')

migrants_map %>% 
  select(-geometry) %>% 
  spread(direction,people) %>% 
  rename(inflow= `in`,outflow=out) %>% 
filter(name== "United Kingdom")
  glimpse()

 mig_country %>% 
   filter(country == "GBR") %>% 
   group_by(Year,direction) %>% 
   summarise(people=sum(people)) %>% 
   arrange(people)
  
 mig_country %>% 
   #glimpse()
   #filter(continent == "Europe") %>% 
   group_by(Year,direction,continent) %>% 
   summarise(people=sum(people)) %>% 
   ggplot(aes(direction,people,fill=continent)) +
   geom_col() +
   facet_grid(~ Year)
   ggplot(aes(continent,people,fill=direction)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   facet_wrap(~ Year,nrow = 1)
 
   mig_country %>% 
     filter(country == "DNK") %>% 
     ggplot(aes(direction,people)) +
     geom_boxplot()
   
  
#####################
   
   str(gapminder)

   ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
     geom_point(alpha = 0.7, show.legend = FALSE) +
     scale_colour_manual(values = country_colors) +
     scale_size(range = c(2, 12)) +
     scale_x_log10() +
     facet_wrap(~continent) +
     # Here comes the gganimate specific bits
     labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
     transition_time(year) +
     ease_aes('linear')
 
   migr_small <- migr %>%
     select(1:4,6,7) %>% 
     mutate_if(is.character,as.factor)
   
migr_small %>% 
  summarise_all(nlevels) %>%
  gather(variable, num_levels)
w_names %>% 
  inner_join(migr_small, by= c("adm0_3"))
  filter(CO2 !="TOT")
levels(migr_small$CO2)
   
mig %>% 
  filter(CO2 != "TOT",GEN == "TOT") %>% 
  filter(COU == "BGR")
   