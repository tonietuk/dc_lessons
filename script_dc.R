ggplot(data=surveys_complete, aes(x = weight, y = hindfoot_length)) + 
  geom_point()

surveys_plot <- ggplot(data=surveys_complete, aes(x = weight, y = hindfoot_length))

surveys_plot +
  geom_point(alpha = 0.1, aes(color=species_id))

install.packages("hexbin")
library(hexbin)

surveys_plot +
  geom_hex()



ggplot(data=surveys_complete, aes(x = species_id, y = weight)) + 
  geom_boxplot(alpha=0) + 
  geom_jitter(alpha=0.3, color="tomato")



ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot(alpha = 0) 





yearly_counts <- surveys_complete %>% 
  group_by(year, species_id) %>% 
  tally()



ggplot(data=yearly_counts, aes(x=year, y=n, group=species_id)) +
  geom_line()


ggplot(data=yearly_counts, aes(x=year, y=n, color=species_id)) +
  geom_line()


ggplot(data=yearly_counts, aes(x=year, y=n)) +
  geom_line() + 
  facet_wrap(~ species_id)


yearly_sex_counts <- surveys_complete %>% 
  group_by(year, species_id, sex) %>% 
  tally()

ggplot(data = yearly_sex_counts, aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title="Observed species in time",
       x="Year of observation",
       y="Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", 
                                   size = 12, angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour="grey20", size = 12), text=element_text(size = 16))





ggplot(data=yearly_sex_counts, aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() + 
  theme(panel.grid = element_blank())


yearly_mean_weight <- surveys_complete %>% 
  filter(!is.na(weight)) %>% 
  group_by(year, species_id) %>%
  summarize(mean_weight = mean(weight))


ggplot(data=yearly_mean_weight, aes(x=year, y=mean_)) +
  geom_line()

ggplot(data=yearly_mean_weight, aes(x=year, y=mean_weight)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() + 
  theme(panel.grid = element_blank())


yearly_weight_average <- surveys_complete %>% 
  filter(!is.na(weight)) %>% 
  group_by(year, species_id) %>% 
  summarize(mean_weight = mean(weight))

ggplot(data = yearly_weight_average, aes(x = year, y = mean_weight, color = species_id)) +
  geom_line() +
  facet_wrap(~ species_id) + theme_bw() + theme(panel.grid = element_blank())

yearly_sex_weight <- surveys_complete %>% 
  group_by(year, sex, species_id) %>% 
  summarize(avg_weight = mean(weight))

ggplot(data=yearly_sex_weight, aes(x=year, y=avg_weight, color=species_id)) +
  geom_line() + 
  facet_grid(sex ~ .)


surveys_complete <- surveys %>% 
  filter(species_id != "", 
         !is.na(weight),
         !is.na(hindfoot_length),
         sex != "")

species_counts <- surveys_complete %>% 
  group_by(species_id) %>% 
  tally %>% 
  filter(n>=50)

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, path = "data_output/surveys_complete2.csv")

surveys_sml <- surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

# inspect data
str(surveys)

df <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys1995 <-  surveys %>% 
  filter(year<1995) %>% 
  select(year,sex,weight)

surveys %>% 
  mutate(weight_kg = weight/1000) %>% 
  select(record_id, weight, weight_kg)

surveys %>% 
  mutate(weight_kg = weight/1000, weight_kg2 = weight_kg * 2)

# filtering
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000) %>% 
  head



head(df, 10) # display first 10 rows

# surveys_bf95
surveys_bf95 <- surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight)

# mutate
surveys %>%
  mutate(weight_kg = weight / 1000)

# mutate with pipe
surveys %>%
  mutate(weight_kg = weight / 1000,
         weight_kg2 = weight_kg * 2) %>% 
  head

# filter() in the chain
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head

# hindfoot_half
surveys %>%
  filter(!is.na(hindfoot_length), hindfoot_length <30) %>%
  mutate(hindfoot_half = hindfoot_length / 2) %>% 
  select(species_id, hindfoot_length, hindfoot_half)




test_subset <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight)) 
  # print(n = 25)

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))

# tallying
surveys %>%
  group_by(sex) %>%
  tally



# split-apply-combine
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# view rows - use print
surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% 
  print(n=40)

surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  print(n=20)

# rm na
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

# summarize multiple variables
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))

# tallying
surveys %>%
  group_by(sex) %>%
  tally

# challenge 1
surveys %>%
  group_by(plot_type) %>%
  tally

# challenge 2
surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarize(mean_hindfoot_length = mean(hindfoot_length),
            min_hindfoot_length = min(hindfoot_length),
            max_hindfoot_length = max(hindfoot_length))

# challenge 3
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year, genus, species_id) %>%
  summarize(max_weight = max(weight))  %>% 
  arrange(year, desc(max_weight)) %>% 
  print(n=336)

# challenge 4
surveys %>%
  filter(!is.na(weight)) %>% 
  group_by(year, species_id) %>%
  summarize(max_weight = max(weight), n())

# confirm count
surveys %>%
  filter(!is.na(weight), year == 1977, species_id == 'OL')

# view all data
View(surveys)

surveys_spread <- surveys_gw %>% 
  spread(genus, mean_weight, fill=0)


surveys_gather <- surveys_spread %>% 
  gather(key = genus, value = mean_weight, -plot_id)

# spreading
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))

str(surveys_gw)

surveys_spread <- surveys_gw %>%
  spread(key = genus, value = mean_weight)

surveys_gw %>%
  spread(genus, mean_weight, fill = 0) %>%
  head()

# gathering
surveys_gather <- surveys_spread %>%
  gather(key = genus, value = mean_weight, -plot_id)

str(surveys_gather)

# using : operator in gathering to specify contiguous columns
surveys_spread %>%
  gather(key = genus, value = mean_weight, Baiomys:Spermophilus) %>%
  head()

surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex


## extract the most common species_id
species_counts <- surveys_complete %>%
  group_by(species_id) %>%
  tally %>%
  filter(n >= 50)

## extract the most common species_id and sort (ascending by default, use 'desc' for descending order)
species_counts <- surveys_complete %>%
  group_by(species_id) %>%
  tally %>%
  filter(n >= 50)  %>% 
  arrange(n)

## only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

# write to output
write_csv(surveys_complete, path = "data_output/surveys_complete.csv")

# ggplot2
ggplot(data=surveys_complete)
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length))
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point()

# assign plot to a variable
surveys_plot <- ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length))

# draw the plot
surveys_plot + 
  geom_point()

# transparency
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

# colour
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

# colour species
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

# boxplot
ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot()

ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot(alpha = 0) 


# challenge 1
# violin plot or beanplot
ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_violin()

# challenge 2
# log scale
ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_violin() +
  scale_y_log10()

# plotting time series data
# compute yearly count
yearly_counts <- surveys_complete %>%
  group_by(year, species_id) %>%
  tally

# plot yearly count vs year
ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line()
  
# group by species_id
ggplot(data = yearly_counts, aes(x = year, y = n, group = species_id)) +
  geom_line()

# add colour to distinguish between species_id; this automatically groups the data
ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line()

# faceting
ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~ species_id)

# yearly sex count
yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally()

# plot by sex
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id)

# black and white theme
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

# one column, facet by rows
yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))

ggplot(data = yearly_sex_weight, aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(sex ~ .)

# one row, facet by column
ggplot(data = yearly_sex_weight, aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(. ~ sex)

# add sensible labels
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw()


# label orientation
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))

# save and re-use theme
grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 45, hjust = 0.5, vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 16))

ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  grey_theme

# using gridExtra package
install.packages("gridExtra")
library(gridExtra)

# arranging and exporting plots
spp_weight_boxplot <- ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  xlab("Species") + ylab("Weight (g)") +
  scale_y_log10()

spp_count_plot <- ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line() + 
  xlab("Year") + ylab("Abundance")

grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))

# save plot
my_plot <- ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text=element_text(size = 16))
ggsave("name_of_file.png", my_plot, width = 15, height = 10)

# ...also works for grid.arrange() plots
combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))
ggsave("combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)
