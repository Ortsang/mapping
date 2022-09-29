
#https://www.huduser.gov/portal/datasets/usps_crosswalk.html
#https://stackoverflow.com/questions/64463834/text-does-not-show-properly-in-r-plot

install.packages("showtext")
library(showtext)
showtext_auto()

library(rgeos)
library(maptools)
library(geojsonio)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(readxl)
library(ggthemes)

if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "mono", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

setwd("D:/Drives/OneDrive - Cuny GradCenter/issues/!CJRI/pres/plot")
setwd("~/Library/CloudStorage/OneDrive-CityUniversityofNewYork/issues/!CJRI/pres/plot")
#national_data <- geojson_read("cbsa.geojson", what="sp")
#national_map <- fortify(national_data, region="cbsa")
cbsa_cleaned = fread("cbsaDakhipr.csv") 
state = read_excel("states.xlsx")
  
cbsa_used = fread("cbsaused.csv")

physician = fread("moving_physician_bycbsa.csv")

cbsa_cjr = read_excel("cbsacjr.xlsx") %>%
  right_join(cbsa_used,.,by="cbsa")%>%
  mutate(cjr=ifelse(include==0,NA,
                    ifelse(include==1&is.na(treated),"No Data",
                           ifelse(include==1&treated==0,"Control",
                                  ifelse(include==1&treated==1,"Treated",NA)))))


library(rgdal)
my_spdf <- readOGR( 
  dsn=paste0(getwd(),"/cb_state"), 
  layer="cb_2018_us_state_500k",
  verbose=FALSE
)

my_spdf_cbsa <- readOGR( 
  dsn=paste0(getwd(),"/cb_cbsa"), 
  layer="cb_2018_us_cbsa_500k",
  verbose=FALSE
)


df_state = data.frame(my_spdf$NAME,my_spdf$STATEFP) %>%
  setnames(c("state","statecode")) %>%
  right_join(.,state,by="state") %>%
  mutate(statecode=as.integer(statecode))

state_map <- fortify(my_spdf, region="STATEFP") %>%
                mutate(statecode=as.integer(id)) %>%
  right_join(.,df_state,by="statecode")

cbsa_map <- fortify(my_spdf_cbsa, region="CBSAFP") %>%
  mutate(cbsa=as.integer(id)) %>%
  left_join(cbsa_cleaned,.,by="cbsa") %>%
  left_join(.,cbsa_cjr,by="cbsa") %>%
  drop_na(cjr)

gg <- ggplot()
gg <- gg + geom_path(data = state_map, 
                     aes(x = long,
                         y = lat,
                         group = group), 
                         color = "grey", size = 0.1)
gg <- gg + geom_polygon(data = cbsa_map,
                        aes(fill = cjr,
                            x = long,
                            y = lat,
                            group = group))
gg <- gg + coord_map()
gg <- gg + ggthemes::theme_map()
gg <- gg + theme(legend.position=c(0.1,0.5))
gg <- gg + theme(legend.key.size = unit(.8, 'cm')) 
gg <- gg + scale_fill_brewer(palette = "Pastel2", limits = c("Treated", "Control", "No Data"))
# gg <- gg + scale_color_viridis(discrete=TRUE,
#                               limits = c("Treated", "Control", "No Data", "Not Included")
  # in manual scales, one has to define colors, well, manually
  # I can directly access them using viridis' magma-function
  # values = rev(magma(4))
# )
gg = gg +  theme_map() + guides(fill=guide_legend(title="CJR Regions, MSAs")) +
  labs(x = NULL, 
       y = NULL, 
       title = "US National Map", 
       subtitle = "CJR Model Participating Regions and Study Included Regions", 
       caption = "Geometries: Cartographic Boundary Files; Data: Census Bureau, 2018 \n
                  https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")
gg


## two graphs 
cbsa_cleaned = fread("cbsaDakhipr.csv") 
state = read_excel("states.xlsx")

cbsa_used = fread("cbsaused.csv")

cbsa_cjr = read_excel("cbsacjr.xlsx") %>%
  right_join(cbsa_used,.,by="cbsa") %>%
  select(-c(fips)) %>%
  mutate(cjr_cbed=ifelse(is.na(treatedsid)&treatedcjr==0,"No Data, Control", 
                    ifelse(is.na(treatedsid)&treatedcjr==1,"No Data, Treated",
                           ifelse(treatedsid==0,"Control",
                                  ifelse(treatedsid==1,"Treated",NA)))),
         cjr=ifelse(treatedcjr==0,"Control",
                    ifelse(treatedcjr==1,"Treated",NA)),
         sid=ifelse(treatedsid==0,"Control",
                    ifelse(treatedsid==1,"Treated",NA)))



library(rgdal)
my_spdf <- readOGR( 
  dsn=paste0(getwd(),"/cb_state"), 
  layer="cb_2018_us_state_500k",
  verbose=FALSE
)

my_spdf_cbsa <- readOGR( 
  dsn=paste0(getwd(),"/cb_cbsa"), 
  layer="cb_2018_us_cbsa_500k",
  verbose=FALSE
)


df_state = data.frame(my_spdf$NAME,my_spdf$STATEFP) %>%
  setnames(c("state","statecode")) %>%
  right_join(.,state,by="state") %>%
  mutate(statecode=as.integer(statecode))

state_map <- fortify(my_spdf, region="STATEFP") %>%
  mutate(statecode=as.integer(id)) %>%
  right_join(.,df_state,by="statecode")

cbsa_map <- fortify(my_spdf_cbsa, region="CBSAFP") %>%
  mutate(cbsa=as.integer(id)) %>%
  left_join(cbsa_cleaned,.,by="cbsa") %>%
  left_join(.,cbsa_cjr,by="cbsa") %>%
  drop_na(cjr_cbed)


gg <- ggplot()
gg <- gg + geom_path(data = state_map, 
                     aes(x = long,
                         y = lat,
                         group = group), 
                     color = "grey", size = 0.1)
gg <- gg + geom_polygon(data = cbsa_map,
                        aes(fill = cjr,
                            x = long,
                            y = lat,
                            group = group)) # change "fill" to select the category you want to print
gg <- gg + coord_map()
# gg <- gg + ggthemes::theme_map()
# gg <- gg + theme(legend.position=c(0.1,0.5))
# gg <- gg + theme(legend.key.size = unit(.8, 'cm')) 
# gg <- gg + scale_fill_manual(values=c("#C73E3A", "#0F2540", "#FEDFE1", "#7DB9DE"),
#                              limits = c("Treated", "Control", "No Data, Treated", "No Data, Control"))
gg <- gg + scale_fill_brewer(palette = "Pastel1", limits = c("Treated", "Control"))
gg = gg + theme_map() + guides(fill=guide_legend(title="CJR Regions, MSAs")) +
  labs(x = NULL, 
       y = NULL, 
       title = "US National Map", 
       # subtitle = "CJR Model Participating Regions and Study Included Regions",
       subtitle = "Study CJR Participating Regions", 
       caption = "Geometries: Cartographic Boundary Files; Data: Census Bureau, 2018 \n
                  https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")

gg
