library(ggplot2);library(sf);library(rgdal);library(raster);library(ggpolypath);library(ggrepel)

NZ_coast<-readOGR("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")
NZ_lakes<-readOGR("./shapefiles", layer = "nz-lake-polygons-topo-150k")
big_lakes<-subset(NZ_lakes, !is.na(name_ascii))
protected_areas<-readOGR("./shapefiles", layer = "protected-areas")
CRS.latlon<-CRS("+proj=longlat +datum=WGS84 +no_defs")
protected_areas<-sp::spTransform(protected_areas, CRS.latlon)
natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))
#conarea<-subset(protected_areas, (grepl("s.25", section) | grepl("s.20", section)))
WHS<-readOGR("./shapefiles", layer = "WDPA_WDOECM_Jul2021_Public_26652_shp-polygons")

base<-ggplot()+
  geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.9, fill = "white")+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab("Longitude")+
  ylab("Latitude")

NZ<-base+
  coord_sf(crs = 4269)+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  xlab("")+
  ylab("")+
  geom_rect(mapping = aes(xmin = 166, xmax = 171, ymin = -47.5, ymax = -43), fill = NA, color = "red")+
  theme_void()

TeWah_labels<-data.frame(label = c("Rakiura-Stewart\nIsland", "Ata Whenua-Fiordland\nNational Park"),
                         lat = c(-47.1, -45.3),
                         lon = c(167.8, 167.3))

TeWah_fill = c("Marine Reserve" = "orange", "Te Wāhipounamu" = "forestgreen")
TeWah_color = c("National Park" = "darkgreen")

TeWah<-base+
  geom_polypath(WHS, mapping = aes(long,lat,group = group, fill = "Te Wāhipounamu"), alpha = 0.5)+
  geom_polypath(mpa, mapping = aes(long,lat,group = group, fill = "Marine Reserve"), alpha = 1)+
  geom_path(natpark, mapping = aes(long,lat,group = group, color = "National Park"), alpha = 1, fill = NA, size = 0.1)+
  geom_polypath(big_lakes, mapping = aes(long,lat,group = group), alpha = 0.6, fill = "blue")+
  coord_sf(xlim = c(166,171), ylim = c(-47.25,-43), crs = 4269)+
  geom_text_repel(data = TeWah_labels, aes(x = lon, y = lat, label = label), size = 2, min.segment.length = 0, nudge_y = c(0.25,0.65), nudge_x = c(-1,-2))+
  scale_fill_manual(values = TeWah_fill)+
  scale_color_manual(values = TeWah_color)+
  theme(legend.position = c(0.83, 0.08),
        legend.title = element_blank(),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 5),
        legend.spacing.y = unit(-0.02, "cm"),
        legend.box.background = element_rect(color = "white",fill = "white"),
        legend.key = element_rect(fill = NA))

TeWahNZ<-cowplot::ggdraw() +
  cowplot::draw_plot(TeWah) +
  cowplot::draw_plot(NZ, x = 0.35, y = 0.68, width = 0.2, height = 0.3)

ggsave("./figures/TeWahNZ.svg", TeWahNZ, dpi = 320, width = 250, units = 'mm')


fiord_labels<-data.frame(label = c("Lake\nManapouri","Piopiotahi-Milford Sound","Te Hāpua-Sutherland Sound",
                                   "Hāwea-Bligh Sound","Te Houhou-George Sound","Taitetimu-Caswell Sound",
                                   "Taiporoporo-Charles Sound","Hinenui-Nancy Sound","Te Awa-o-Tū-Thompson Sound",
                                   "Patea-Doubtful Sound","Te Rā-Dagg Sound",
                                   "Te Puaitaha-Breaksea\nSound","Tamatea-Dusky\nSound","Taiari-Chalky Inlet",
                                   "Rakituma-Preservation Inlet", "Vancouver\nArm"),
                         lat = c(-45.51, -44.58, -44.72,
                                 -44.77, -44.85, -45.02,
                                 -45.05, -45.1, -45.15,
                                 -45.25, -45.38,
                                 -45.59, -45.75, -46.02,
                                 -46.1, -45.5),
                         lon = c(167.55, 167.8, 167.55,
                                 167.5, 167.36, 167.15,
                                 167.08, 167.02, 166.97,
                                 166.9, 166.8,
                                 166.67, 166.47, 166.51,
                                 166.6, 166.98))

fiord_fill = c("Marine Reserve" = "orange")

fiords<-base+
  geom_polygon(mpa, mapping = aes(long,lat,group = group, fill = "Marine Reserve"), alpha = 1)+
  geom_polypath(big_lakes, mapping = aes(long,lat,group = group), alpha = 0.6, fill = "blue")+
  coord_sf(xlim = c(166.0,168), ylim = c(-46.2,-44.5), crs = 4269)+
  scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.83, 0.12),
        legend.title = element_blank(),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 5),
        legend.spacing.y = unit(-0.02, "cm"),
        legend.box.background = element_rect(color = "white",fill = "white"),
        legend.key = element_rect(fill = NA))+
  geom_text_repel(data = fiord_labels, aes(x = lon, y = lat, label = label), size = 2, min.segment.length = 0, force_pull = 2, box.padding = 0.1,
                  nudge_x = c(0.29,-0.3,-0.4,
                              -0.3,-0.5,-0.4,
                              -0.5,-0.5,-2,
                              -0.3,-0.2,
                              -0.4,-0.3,-0.3,
                              -0.4,0.23),
                  nudge_y = c(-0.05,0.01,0.04,
                              0.1,0.05,0.1,
                              0.08,0.05,0.01,
                              0,0,
                              0.08,-0.02,-0.02,
                              -0.15,-0.09))
  
ggsave("./figures/fiords.svg", fiords, dpi = 320, width = 250, units = 'mm')

