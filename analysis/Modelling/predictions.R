library(readr)

census2011_msoa_pop_breakdown <- read_csv("analysis/Modelling/census2011_msoa_pop_breakdown.csv")
View(census2011_msoa_pop_breakdown)

# rename columns 

yr2011 <- census2011_msoa_pop_breakdown %>%
  mutate(over_11 = prop_11_16,
         bame = 1-prop_white,
         male = prop_male,
         income_deprived = idaci)

view(yr2011)


## edit simple model ;
# the following is a linear model in which we use the variables male,
# over_11, bame, and income_deprived to predict the response variable 'anycd_ic'
# need to run part of naive_model script to get 'train'

simple <- glm(anycd_ic ~ male + over_11 + bame + income_deprived,
              data = train,
              family = "binomial")

summary(simple)
tidy(simple)

# exporting tidy summary table as csv and model performance as text file

summary_df <- as.data.frame(tidy(simple))
write.csv(summary_df, file = "simple_model_summary.csv")
          
sink(file = "simple_model_performance.txt")
model_performance(simple, train)
sink(file = NULL)


# use it to predict anyc_id probability for the 2011 msoa regions

predictions <- predict(simple, yr2011, type="response")


# adding predictions column to the yr2011 df

yr2011 <- yr2011 %>%
  mutate (simple_model_predictions = predictions )

## MAPPING
# load the packages at the begining of modelling.R
# then reading the .shp shape file for L&S and printing it

library(caret)
library(rgdal)
library(glmnet)
library(spdep)
library(R2BayesX)


LS_map <- readOGR(
  dsn = "~/git/mhcyp-2004/analysis/Modelling/MSOA Shapefile"
)

summary(LS_map)
length(LS_map)
head(LS_map@data)

plot(LS_map)

# merging the shapefile with the yr2011 data by the msoa column

install.packages("gpclib")
library(gpclib)

maptools::gpclibPermit()
tidy_LS_map <- tidy(LS_map, region = "msoa11nm") %>%
  inner_join(yr2011, by = c("id" = "msoa"))

write.csv(yr2011, file = "yr2011_table.csv")
write.csv(tidy_LS_map, file = "yr2011_map_table.csv")


install.packages("mapproj")
library(mapproj)

# blank map
ggplot() +
  geom_polygon(data = tidy_LS_map, aes(x = long, y = lat, group = group), fill = "#0c0d0d", colour = "white") +
  theme_void() +
  coord_map() # needs mapproj installed

# map of simple_model_predictions
ggplot() +
  geom_polygon(data = tidy_LS_map, aes(x = long, y = lat, group = group, fill = simple_model_predictions), colour = NA) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  labs(fill = "Simple Model Predictions", 
       title="Map of Behavioural Disorders across \nLambeth & Southwark") +
  theme(legend.title = element_text(size = 8),
        plot.title=element_text(hjust=0.5)) +
  theme_void() +
  coord_map()




  
  


