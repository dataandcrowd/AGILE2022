library(tidyverse)
library(sf)
library(tmap)
library(spgwr)
library(spdep)

options(scipen = 999)

# call data

strava <- read_csv("Cleaned Files/strava.csv") # response
green <- read_csv("Cleaned Files/green.csv") # predictors
ptai <- read_csv("Cleaned Files/ptai.csv") # predictors
buildings <- read_csv("Cleaned Files/buildings.csv") # predictors
shp <- read_sf("Cleaned Files/Glasgow_IZ.shp")

# merge all 
strava %>% 
  left_join(green, by = "Name") %>% 
  left_join(ptai, by = "Name") %>% 
  left_join(buildings, by = "Name") -> glasgow_df


#shp %>% 
#  left_join(glasgow_df, by = "Name") %>% 
#  st_write("Cleaned Files/glasgow_full.shp")


#plot(glasgow, breaks = "jenks")
plot(glasgow_df$ride17, glasgow_df$ride18)
cor(glasgow_df$ride17, glasgow_df$ride18)


glasgow_df %>% 
  rename(Strava2017 = ride17,
         Strava2018 = ride18) %>% 
  pivot_longer(!Name,
               names_to = "Type",
               values_to = "Value") -> glasgow_df_long

glasgow_df_long %>% 
  ggplot() +
  geom_histogram(aes(Value), bins = 20) +
  facet_wrap(~Type, scales = "free") +
  labs(x = "", y = "Frequency") -> glasgow_hist

glasgow_hist

#ggsave("hist.jpg",glasgow_hist, width = 6, height = 4)

shp %>% 
  left_join(glasgow_df_long, by = "Name") -> gl_shp

# Strava Users
gl_shp %>% 
  filter(Type %in% c("Strava2017", "Strava2018")) %>% 
  mutate(Value2 = cut(Value,
                     breaks = c(0, 50000, 100000, 150000, 200000, 300000, +Inf),
                     labels = c("0-50", "50-100", "100-150", "150-200", "200-30", ">300"))) %>% 
  tm_shape() +
  tm_polygons("Value2", title = "Strava ('000s)", palette="-RdBu") +
  tm_facets(by = "Type", free.coords = F, free.scales = F, ncol = 2) -> gl_strava

gl_strava

#tmap_save(gl_strava, "strava.jpg", width = 1000, height = 400, dpi = 300)



# Other variables
gl_shp %>% 
  filter(Type %in% c("green", "PTAI", "height")) %>%
  tm_shape() +
  tm_polygons("Value", title = "", palette="-RdBu") +
  tm_facets(by = "Type", free.coords = F, free.scales = T, ncol = 1) +
  tm_layout(legend.position = c("right", "top"),  
            title.position = c('right', 'top')) -> gl_variable


#tmap_save(gl_variable, "variables.jpg", width = 1000, height = 2500, dpi = 300)


#Count data = Discrete Data
#continuous: quantitative data that can take any value in some interval ⇒ linear models
#discrete: quantitative data that takes a “countable” number of values
#(e.g. 0, 1, 2, . . .) ⇒ generalised linear models (GLMs)
#If your data are discrete but the counts are all fairly large, you can
#ignore the discreteness and use linear models anyway. If you have small
#counts and zeros though it is very important to use GLMs instead.

model17 <- lm(log(ride17) ~ log(green) + log(PTAI) + log(height), data = glasgow_df) 
summary(model17)
residuals(model17) %>% summary


#exp(coef(model17)["green"])
#exp(coef(model17)["PTAI"])
#exp(coef(model17)["height"])

car::vif(model17)
AIC(model17, k=3) # k = parameter


model18 <- lm(log(ride18) ~ log(green) + log(PTAI) + log(height), data = glasgow_df) 
summary(model18)
residuals(model18)%>% summary
AIC(model18, k=3) # k = parameter


#exp(coef(model18)["green"])
#exp(coef(model18)["PTAI"])
#exp(coef(model18)["height"])
#car::vif(model18)



shp %>% 
  left_join(glasgow_df, by = "Name") %>% 
bind_cols(
tibble(Residuals18 = residuals(model18),
       Residuals17 = residuals(model17))) -> glasgow_gwr

plot(glasgow_gwr["Residuals17"])


mapres17 <- qtm(glasgow_gwr, fill = "Residuals17") + tm_legend(legend.position = c("right", "top"))
mapres18 <- qtm(glasgow_gwr, fill = "Residuals18") + tm_legend(legend.position = c("right", "top"))
#(plot_residuals <- tmap_arrange(mapres17, mapres18, widths = 5, heights = 3))
#tmap_save(plot_residuals, "Residuals.jpg")

## Morans'I
nb <- poly2nb(glasgow_gwr, queen=TRUE) # calculate neighbours queen continuity
listw <- nb2listw(nb, style="W", zero.policy=TRUE)

globalMoran17 <- moran.test(glasgow_gwr$ride17, listw)
globalMoran18 <- moran.test(glasgow_gwr$ride18, listw)
globalMoran17
globalMoran18


glasgow_sp <- as_Spatial(glasgow_gwr)

#glasgow_sp <- as(glasgow_gwr, "Spatial")

gwr.bandwidth <-gwr.sel(log(ride17) ~ log(green) + log(PTAI) + log(height), 
                        data = glasgow_sp,
                        adapt = F) #estimated optimal bandwidth
gwr.bandwidth

gwr.bandwidth1 <-gwr.sel(log(ride18) ~ log(green) + log(PTAI) + log(height), 
                        data = glasgow_sp,
                        adapt = T) #estimated optimal bandwidth
gwr.bandwidth1

# The result indicates that the optimal bandwidth is 1677ms. This means that neighbouring UTLAs within a fixed radius of 39.79 kms will be taken to estimate local regressions. To estimate a GWR, we execute the code below in which the optimal bandwidth above is used as an input in the argument bandwidth.

gwr.fit1<-gwr(log(ride17) ~ log(green) + log(PTAI) + log(height), 
              data = glasgow_sp, 
              bandwidth = gwr.bandwidth, 
              #adapt = 0.05,
              se.fit=T, 
              hatmatrix=T)

gwr.fit1

#
gwr.fit2<-gwr(log(ride17) ~ log(green) + log(PTAI) + log(height), 
              data = glasgow_sp, 
              #bandwidth = gwr.bandwidth, 
              adapt = 0.03,
              se.fit=T, 
              hatmatrix=T)

gwr.fit2



#
results17 <-as.data.frame(gwr.fit2$SDF)
names(results17)



glasgow_gwr %>% 
  select(-c(green, PTAI, height)) %>% 
  bind_cols(results17) -> gwr_results17

strava17_localr2 <- qtm(gwr_results17, fill = "localR2") + tm_legend(legend.position = c("right", "top"))
strava17_green   <- qtm(gwr_results17, fill = "log.green.")   + tm_legend(legend.position = c("right", "top"))
strava17_ptai    <- qtm(gwr_results17, fill = "log.PTAI.")    + tm_legend(legend.position = c("right", "top"))
strava17_height  <- qtm(gwr_results17, fill = "log.height.")  + tm_legend(legend.position = c("right", "top"))

#
(plot_2017 <- tmap_arrange(strava17_localr2, strava17_green, strava17_ptai, strava17_height))
#tmap_save(plot_2017, "GWR2017.jpg")


#https://rpubs.com/quarcs-lab/tutorial-gwr1
#https://crd230.github.io/gwr.html
#https://gdsl-ul.github.io/san/geographically-weighted-regression.html
#https://rpubs.com/cdlloyd/spatialanalysis

gwr.bandwidth2 <-gwr.sel(log(ride18) ~ green + PTAI + height, 
                         data = glasgow_sp,
                         adapt = F) #estimated optimal bandwidth
gwr.bandwidth2

gwr.bandwidth3 <-gwr.sel(log(ride18) ~ green + PTAI + height, 
                         data = glasgow_sp,
                         adapt = T) #estimated optimal bandwidth
gwr.bandwidth3


gwr.fit3<-gwr(log(ride18) ~ log(green) + log(PTAI) + log(height),
              data = glasgow_sp, 
              bandwidth = gwr.bandwidth, 
              #adapt = 0.05,
              se.fit=T, 
              hatmatrix=T)

gwr.fit3

#
gwr.fit4<-gwr(log(ride18) ~ log(green) + log(PTAI) + log(height),
              data = glasgow_sp, 
              #bandwidth = gwr.bandwidth, 
              adapt = 0.03,
              se.fit=T, 
              hatmatrix=T)

gwr.fit4



#
results18 <-as.data.frame(gwr.fit4$SDF)
names(results18)



glasgow_gwr %>% 
  select(-c(green, PTAI, height)) %>% 
  bind_cols(results18) -> gwr_results18

strava18_localr2 <- qtm(gwr_results18, fill = "localR2") + tm_legend(legend.position = c("right", "top"))
strava18_green <- qtm(gwr_results18, fill = "log.green.")     + tm_legend(legend.position = c("right", "top"))
strava18_ptai <- qtm(gwr_results18, fill = "log.PTAI.")       + tm_legend(legend.position = c("right", "top"))
strava18_height <- qtm(gwr_results18, fill = "log.height.")   + tm_legend(legend.position = c("right", "top"))


(plot_2018 <- tmap_arrange(strava18_localr2, strava18_green, strava18_ptai, strava18_height))
#tmap_save(plot_2018, "GWR2018.jpg")

