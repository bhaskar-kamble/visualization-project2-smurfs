#https://www.r-exercises.com/2018/04/27/how-to-plot-with-ggiraph/
library(ggiraph)
library(ggplot2)
g <- ggplot(mpg, aes( x = displ, y = cty, color = hwy) )

my_gg <- g + geom_point_interactive(aes(tooltip = model), size = 2)
ggiraph(code = print(my_gg) )
ggiraph(code = print(my_gg), hover_css = "cursor:pointer;fill:red;stroke:red;")


crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

crimes$onclick <- sprintf("window.open(\"%s%s\")",
                          "http://en.wikipedia.org/wiki/", as.character(crimes$state) )

gg_crime <- ggplot(crimes, aes(x = Murder, y = Assault, color = UrbanPop )) +  geom_point_interactive(
        aes( data_id = state, tooltip = state, onclick = onclick ), size = 3 ) +  scale_colour_gradient(
          low = "#999999", high = "#FF3333")

ggiraph(code = print(gg_crime), hover_css = "fill-opacity:.3;cursor:pointer;")


#https://rdrr.io/cran/ggiraph/man/geom_polygon_interactive.html

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5) )

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2) )

datapoly <- merge(values, positions, by=c("id"))

datapoly$oc = "alert(this.getAttribute(\"data-id\"))"

# create a ggplot -----
gg_poly_1 <- ggplot(datapoly, aes( x = x, y = y ) ) +
  geom_polygon_interactive(aes(fill = value, group = id,
                               tooltip = value, data_id = value, onclick = oc))

# display ------
ggiraph(code = {print(gg_poly_1)})


############################################
#https://www.r-exercises.com/2018/04/27/how-to-plot-with-ggiraph/
world <- sf::st_as_sf(rnaturalearth::countries110)

urlfile <- "https://raw.github.com/bhaskarvk/user2017.geodataviz/master/inst/extdata/africa-internet_usage-2015.csv"
internet_usage <- read.csv(urlfile)
head(internet_usage)

names(internet_usage) <- c("Country Name",  "Country Code",   "Series Name",  "Series Code", 
                           "2014 [YR2014]", "2015 [YR2015]", "2015 [YR20156]")

africa <- dplyr::filter(world, region_un=='Africa') %>%
  dplyr::left_join(internet_usage %>% dplyr::select(
    `Country Code`, `2015 [YR2015]`
  ) %>% dplyr::rename(iso_a3=`Country Code`, internet.usage.2015=`2015 [YR2015]`),
  by = 'iso_a3') %>%
  st_transform(crs="+proj=laea +lon_0=18.984375")

africa$internet.usage.2015

africa.centers <- st_centroid(africa)
africa.spdf <- methods::as(africa, 'Spatial')
africa.spdf@data$id <- row.names(africa.spdf@data)
#africa.tidy <- broom::tidy(africa.spdf)


#################################################

#YAY!!! THIS IS IT!!!

gg <- ggplot(mymap)+geom_sf_interactive(aes(fill=value,tooltip=sprintf("%s<br/>%s",NAME_1,value),data_id=NAME_1))
ggiraph(ggobj = gg)