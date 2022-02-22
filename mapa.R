pacman::p_load(terra, spData)
library(dplyr)
library(geobr)
library(ggplot2)
library(sf)


path="D:\\documentos"
my_rast <- rast(file.path(path,"brasil_coverage_2020.tif"))
            plot(my_rast)
            
mun = read_municipality(year =2020)
rio = mun %>% 
  filter(abbrev_state == "RJ") 


cr = crop(my_rast, rio)
ms = mask(cr, vect(rio))
ex = extract(ms, vect(rio))

plot(cr)
plot(ms)

cobertura <- ex %>%
  group_by(ID) %>%
  summarise(cobertura = n())


cobertura_vegetal <- ex %>%
  group_by(ID) %>%
  filter(brasil_coverage_2020 %in% c(1,3,4,5,49)) %>%
  summarise(cobertura_v = n())


cobertura_rio <- merge(cobertura, cobertura_vegetal, by=c("ID"), all.x=TRUE)


cobertura_rio <- cobertura_rio %>%
  mutate(p_v = cobertura_v/cobertura)


rio <- rio %>%
  mutate(ID = c(1:92), .after = code_muni) %>%
  left_join(cobertura_rio, by = "ID")


plot_rio_cobertura <- rio %>%
  ggplot() +
  geom_sf(aes(fill = p_v), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Porcentagem") +
  labs(title = "Porcentagem de Cobertura Vegetal", subtitle = "Estado do Rio de Janeiro ")
plot_rio_cobertura


Rio_vegetal <-rio  %>% 
  subset(select = c(name_muni, p_v))
