# Add GII to Spatial data of the World

# Load packages
library('readr')
library('sf')
library('janitor')
library('dplyr')
library('countrycode')
library('tmap')

#读数据
GII <- read_csv("Gender Inequality Index (GII).csv",
                locale = locale(encoding = "latin1"),
                na = "..",
                skip = 5)
World <- st_read("World_Countries__Generalized_.shp")

#筛选数据
GIIcol <- GII %>%
  clean_names()%>%
  select(country, x2019, x2010)%>%
  mutate(difference = x2019-x2010)%>%
  slice(1:189,) %>%
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))

#join
Join_GII <- World %>%
  clean_names() %>%
  left_join(.,
            GIIcol,
            by = c("aff_iso" = "iso_code"))

#not used
#Join_GII2 <- Join_GII %>%
#  mutate(change = case_when(difference < 0 ~ "improved",
#                            difference == 0 ~ "same",
#                            TRUE ~ "worse"))

#make the map
tmap_mode("plot")
qtm(Join_GII,
    fill="difference")

#change to Mollweide Projection to preserve size
Join_GII <- Join_GII %>% 
  st_transform(., crs = "+proj=moll")

#check if the new projection is correct
#summary(Join_GII)
#st_crs(Join_GII)

breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)
diffbreaks=c(-0.4,-0.3,-0.2,-0.1,0,0.1)

# plot each map
tm1 <- tm_shape(Join_GII) + 
  tm_polygons("x2019", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a) GII 2019", position=c(0,0.8), size=0.7)

tm2 <- tm_shape(Join_GII) + 
  tm_polygons("x2010",
              breaks=breaks,
              palette="PuBu") + 
  tm_legend(show=FALSE) +
  tm_layout(frame=FALSE) +
  tm_credits("(b) GII 2010", position=c(0,0.8), size=0.7)

tm3 <- tm_shape(Join_GII) + 
  tm_polygons("difference",
              style="fixed",
              breaks=diffbreaks,
              palette=("Blues"),
              midpoint = NA) + 
  tm_legend(show=FALSE) +
  tm_layout(frame=FALSE) +
  tm_credits("(c) Diff", position=c(0,0.8), size=0.7)

legend <- tm_shape(Join_GII) +
  tm_polygons("difference",
              palette=("Blues"),
              midpoint = NA) +
  #tm_scale_bar(position=c(0.2,0.04), text.size=0.6) +
  tm_compass(north=0, position=c(0.6,0.7), size=0.5) +
  tm_layout(legend.only = TRUE, legend.position=c(0.3,0.25), asp=0.1) +
  tm_credits("Mapped data:\nUN Gender Inequality Index\n\nWorld outline:\nArcGIS Hub",
             position=c(0.3,0.11)) +
  tm_shape(Join_GII) + 
  tm_fill("x2019", 
          title = "GII",
          breaks=breaks,
          palette="PuBu") +
  tm_borders(lwd=1) +
  tm_layout(legend.only = TRUE, legend.position=c(0.1,0.1), asp=0.1)

t = tmap_arrange(tm1, tm2, tm3, legend, ncol=2)
t
