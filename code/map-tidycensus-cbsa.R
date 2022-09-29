library(pacman)

pacman::p_load(tidycensus,tidyverse,viridis,uuid,
               tigris,tmap,
               install = TRUE)

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

#census_api_key('415115a53886e4faeb0291389dee1f7ad9243568', install = TRUE)

setwd("D:/Drives/OneDrive - Cuny GradCenter/issues/!CJRI/pres/plot")
cbsa_cleaned = fread("cbsaDakhipr.csv") 
movingphysician = fread("moving_physician_bycbsa.csv")
state = read_excel("states.xlsx")

cbsa_cleaned = cbsa_cleaned %>%
  distinct(cbsa,cbsatitle,.keep_all = TRUE)

cbsa = core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  shift_geometry(.) %>%
  mutate(cbsa=as.numeric(CBSAFP)) %>%
  left_join(.,movingphysician,by=c("cbsa"))%>%
  inner_join(.,cbsa_cleaned,by=c("cbsa")) 

state_outline <- states(cb = TRUE) %>%
  shift_geometry(.) %>%
  mutate(state=NAME) %>%
  right_join(.,state,by=c("state"))



hennepin_black <- filter(hennepin_race, 
                         variable == "Black")


tm_shape(state_outline)+
  tm_polygons(palette = "White") +
  tm_shape(cbsa) + 
  tm_polygons(col = "n",
              palette = "Blues",
              style = "jenks")



geom_sf(data=cbsa,aes(fill=LSAD),color = "white")+
  coord_sf(datum = NA)


gg <- ggplot()
gg <- gg + geom_sf(data = state_outline, 
                     color = "white", size = 0.1)
gg <- gg + geom_sf(data = cbsa,
                        aes(fill = as.numeric(n))) # change "fill" to select the category you want to print
gg <- gg + coord_sf(datum = NA)
# gg <- gg + ggthemes::theme_map()
# gg <- gg + theme(legend.position=c(0.1,0.5))
# gg <- gg + theme(legend.key.size = unit(.8, 'cm')) 
# gg <- gg + scale_fill_manual(values=c("#C73E3A", "#0F2540", "#FEDFE1", "#7DB9DE"),
#                              limits = c("Treated", "Control", "No Data, Treated", "No Data, Control"))
gg <- gg + scale_fill_viridis_c(option = "magma")
gg = gg + theme_map() + guides(fill=guide_legend(title="CJR Regions, MSAs")) +
  labs(x = NULL, 
       y = NULL, 
       title = "US National Map", 
       # subtitle = "CJR Model Participating Regions and Study Included Regions",
       subtitle = "Study CJR Participating Regions", 
       caption = "Geometries: Cartographic Boundary Files; Data: Census Bureau, 2018 \n
                  https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")

gg
