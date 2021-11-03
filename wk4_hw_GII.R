# Add GII to Spatial data of the World

#读数据
library('readr')
library('sf')
GII <- read_csv(("Gender Inequality Index (GII).csv"),
                locale = locale(encoding = "latin1"),
                na = "..",
                skip = 5)
World <- st_read("World_Countries__Generalized_.shp")

#筛选数据
library('janitor')
library('dplyr')
#install.packages('countrycode')
library('countrycode')
GIIcol <- GII %>%
  clean_names()%>%
  select(country, x2019, x2010)%>%
  mutate(difference = x2019-x2010)%>%
  slice(1:189,) %>%
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso3c'))

#jion
Jion_GII <- World %>%
  clean_names()%>%
  left_join(.,
            GIIcol,
            by = c("iso" = "iso_code"))