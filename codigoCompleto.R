
#------------LIBRERIAS-------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(fastDummies)
library(ggplot2)
library(sf)
library(plotly)
library(WDI)
library(purrr)
library(crosstalk)
library(leaflet)
library(raster)
library(tmap)
library(RColorBrewer)
library(raster)
library(grDevices)


#------------BASE DE DATOS ORIGINAL-------------------------------
pelis <- read.csv('peliculas.csv')
class(pelis)
colnames(pelis)



#------------ELIMINAR VARIABLES-------------------------------
pelis <- dplyr::select(pelis,-'X',-'Rotten.Tomatoes',-"Type")
colnames(pelis)


#------------CATEGORIAS DE LAS VARIABLES-------------------------------
class(pelis$ID)
class(pelis$Title)
class(pelis$Year)
class(pelis$Age)
class(pelis$IMDb)
class(pelis$Rotten.Tomatoes)#esta mal
class(pelis$Netflix)
class(pelis$Hulu)
class(pelis$Prime.Video)
class(pelis$Disney.)
class(pelis$Type)
class(pelis$Directors)
class(pelis$Genres)
class(pelis$Country)
class(pelis$Language)
class(pelis$Runtime)



#------------MODIFICACIONES VARIABLE GENERO-------------------------------

generos<- str_split_fixed(pelis$Genres, ",", n=Inf)

results <- fastDummies::dummy_cols(generos)

Action <- results$V1_Action + results$V2_Action + results$V3_Action + results$V4_Action
Adventure <- results$V1_Adventure + results$V2_Adventure + results$V3_Adventure + results$V4_Adventure
Sci_Fi <- results$`V1_Sci-Fi` + results$`V2_Sci-Fi`+ results$`V3_Sci-Fi`+ results$`V4_Sci-Fi`+ results$`V6_Sci-Fi`+ results$`V7_Sci-Fi`+ results$`V8_Sci-Fi`
Thriller <- results$V1_Thriller + results$V2_Thriller + results$V3_Thriller+ results$V4_Thriller+results$V5_Thriller + results$V6_Thriller+results$V7_Thriller+results$V8_Thriller+results$V9_Thriller
Comedy <- results$V1_Comedy+results$V2_Comedy+results$V3_Comedy+results$V4_Comedy+results$V5_Comedy
Westem <- results$V1_Western +results$V2_Western+results$V3_Western+results$V4_Western+results$V5_Western+results$V6_Western+results$V7_Western+results$V8_Western
Animation <- results$V1_Animation+results$V2_Animation+results$V3_Animation
Family <- results$V1_Family + results$V2_Family + results$V3_Family+results$V4_Family+results$V5_Family+results$V6_Family+results$V7_Family
Biography <- results$V1_Biography + results$V2_Biography + results$V3_Biography + results$V4_Biography
Drama <- results$V1_Drama + results$V2_Drama+results$V3_Drama+results$V4_Drama+results$V5_Drama+results$V6_Drama
Music <- results$V1_Music+results$V2_Music + results$V3_Music + results$V4_Music+results$V5_Music+results$V6_Music+results$V7_Music
War <- results$V1_War+results$V2_War+results$V3_War+results$V4_War+results$V5_War+results$V6_War+results$V7_War + results$V8_War
Crime <- results$V1_Crime+results$V2_Crime+results$V3_Crime+results$V4_Crime+results$V5_Crime
Documentary <- results$V1_Documentary +results$V2_Documentary+results$V3_Documentary
Fantasy <- results$V1_Fantasy +results$V2_Fantasy+results$V3_Fantasy+results$V4_Fantasy+results$V5_Fantasy+results$V6_Fantasy+results$V7_Fantasy
Film_noir <- results$`V1_Film-Noir`+results$`V2_Film-Noir`+results$`V3_Film-Noir`+results$`V4_Film-Noir`
Game_show <- results$`V1_Game-Show`+results$`V2_Game-Show`+results$`V3_Game-Show`+results$`V4_Game-Show`
History <- results$V1_History+results$V2_History+results$V3_History+results$V4_History+results$V5_History+results$V6_History
Horror <- results$V1_Horror+results$V2_Horror+results$V3_Horror+results$V4_Horror+results$V5_Horror+results$V6_Horror+results$V7_Horror
Musical <- results$V1_Musical+results$V2_Musical+results$V3_Musical+results$V4_Musical+results$V5_Musical+results$V6_Musical+results$V7_Musical
Mystery <- results$V1_Mystery+results$V2_Mystery+results$V3_Mystery+results$V4_Mystery+results$V5_Mystery+results$V6_Mystery+results$V7_Mystery+results$V8_Mystery
News <- results$V1_News+results$V2_News+results$V3_News+results$V4_News+results$V5_News
Reality_tv <- results$`V1_Reality-TV`+results$`V2_Reality-TV`+results$`V3_Reality-TV`
Romance <- results$V1_Romance+results$V2_Romance+results$V3_Romance+results$V4_Romance+results$V5_Romance+results$V6_Romance+results$V7_Romance+results$V8_Romance
Short <- results$V1_Short+results$V2_Short+results$V3_Short+results$V4_Short
Sport <- results$V1_Sport+results$V2_Sport+results$V3_Sport+results$V4_Sport+results$V5_Sport+results$V6_Sport+results$V7_Sport
Talk_show <- results$`V1_Talk-Show`+results$`V2_Talk-Show`+results$`V3_Talk-Show`+results$`V4_Talk-Show`



generos_def <- data.frame(Action,Adventure,Sci_Fi,Thriller,Comedy,Westem,Animation,Family,Biography,Drama,Music,War,Crime,Documentary,Fantasy,Film_noir,Game_show,History,Horror,Musical,Mystery,News,Reality_tv,Romance,Short,Sport,Talk_show)

peliculas <- cbind(pelis,generos_def)

#peliculas[,17:43] <- lapply(peliculas[,17:43], function(x) as.character(x))
class(peliculas$Action)



#------------MODIFICACIONES VARIABLE AGE-------------------------------
peliculas$Age[peliculas$Age == ""] <- "all"
barplot(table(peliculas$Age))


#------------MODIFICACIONES VARIABLE DIRECTORS-------------------------------
directores <- str_split_fixed(peliculas$Directors, ",",n=4)
colnames(directores)
colnames(directores) <- c('Director 1','Director 2','Director 3', 'Otros directores')

peliculas <- cbind(peliculas,directores)




#------------MODIFICACIONES VARIABLE COUNTRY-------------------------------

pais_grab<- str_split_fixed(peliculas$Country, ",", n=5)
pais_grab <- pais_grab[,-5]
#grabacion <- fastDummies::dummy_cols(pais_grab)
colnames(pais_grab) <- c('País de grabación 1','País de grabación 2','País de grabación 3', 'Otros paises de grabación ')
peliculas <- cbind(peliculas,pais_grab)




#------------MODIFICACIONES VARIABLE LANGUAGE-------------------------------
LanguagesSeven <- str_split_fixed(peliculas$Language, ",", n=8)
colnames(LanguagesSeven) <- c('Idioma1','Idioma2','Idioma3', 'Idioma4', 'Idioma5', 'Idioma6', 'Idioma7', 'OtrosIdiomas')
peliculas <- cbind(peliculas,LanguagesSeven)

colnames(peliculas)



#------------MODIFICACIONES VARIABLE RUNTIME-------------------------------
peliculas[13180,]$Runtime = 57
peliculas[13180,]
peliculas <- subset(peliculas,Runtime >= 5 & Runtime <= 250 )
boxplot(peliculas$Runtime)





#------------BOXPLOT-------------------------------
boxplot(peliculas$IMDb,peliculas$Rotten.Tomatoes)





#------------PELICULAS DE ACCIÓN (VALOR) -------------------------------

resumen <- peliculas %>% group_by(Year) %>% summarise(Action = sum(Action))
p <- resumen %>% plot_ly(x=~Year,y=~Action,hoverinfo='text', alpha = 0.5) %>% add_markers(frame = ~Year) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button( x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
  animation_slider(currentvalue = list(prefix = "AÑO ", font = list(color="gray")))
p




#------------PELICULAS DE ACCIÓN (EVOLUCIÓN)-------------------------------
prep <- resumen %>% split(.$Year) %>% accumulate(~bind_rows(.x,.y)) %>% bind_rows(.id="frame") 
class(prep)

prep %>% plot_ly(x=~Year,y=~Action) %>% add_lines(frame = ~frame)%>%
  animation_slider(currentvalue = list(prefix = "AÑO ", font = list(color="gray")))



#------------PELICULAS DE ACCIÓN Y AVENTURA (EVOLUCIÓN) -------------------------------


resumen2 <- peliculas %>% group_by(Year) %>% summarise(Action = sum(Action),Adventure=sum(Adventure),Thriller=sum(Thriller))
prep <- resumen2 %>% split(.$Year) %>% accumulate(~bind_rows(.x,.y)) %>% bind_rows(.id="frame") 
class(prep)

prep %>% plot_ly() %>% add_lines(x=~Year,y=~Adventure,type='lines',frame=~frame) %>% add_lines(x=~Year,y=~Action,frame = ~frame)%>%
  animation_slider(currentvalue = list(prefix = "AÑO ", font = list(color="gray")))


#------------EVOLUCION DE VARIOS GENEROS -------------------------------

resumen3 <- peliculas %>% dplyr::select(Year,15:41) %>% group_by(Year) %>% summarise_all(sum)
prep <- resumen3 %>% split(.$Year) %>% accumulate(~bind_rows(.x,.y)) %>% bind_rows(.id="frame") 

prep %>% plot_ly() %>% add_lines(x=~Year,y=~Adventure,name = "Aventura",frame=~frame) %>% add_lines(x=~Year,y=~Action,name="Acción",frame = ~frame)%>%add_lines(x=~Year,y=~Comedy,name="Comedia",frame = ~frame)%>%add_lines(x=~Year,y=~Animation,name="Animación",frame = ~frame)%>%add_lines(x=~Year,y=~War,name="Guerra",frame = ~frame)%>%add_lines(x=~Year,y=~Family,name="Familia",frame = ~frame)%>%add_lines(x=~Year,y=~Thriller,name="Suspenso",frame = ~frame)%>% animation_slider(currentvalue = list(prefix = "AÑO ", font = list(color="gray"))) %>% layout(title= 'Evolución de los generos',yaxis = list(title = "Nº de películas"),xaxis = list(title = "Año"))




#------------PELICULAS SEGÚN LA EDAD (EVOLUCIÓN) -------------------------------

age_a <- table(peliculas$Age,peliculas$Year)


myFrame <- as.data.frame(age_a)

pal <- colorRampPalette(c("red", "yellow"))
colores = pal(5)
colnames(myFrame) <- c('Age','Year','Freq')
tx <- highlight_key(myFrame)
gl <- tx %>% plot_ly(x = ~Year, y = ~Freq,type = "bar",color = ~Age,colors = c( "#68372b", "#98372b","#bf8763", "#f9b945", "#fef55b"),height = 300)

wingets <- bscols(list(filter_select("Año","Selecciona un año",tx, ~Year),filter_checkbox("Edad","Selecciona un rango de edad",tx, ~Age,inline = TRUE)))
bscols(gl,wingets,widths =c(14,7))




#------------ FUSIONAR LAS COLUMNAS DEL PAIS DE GRABACION -------------------------------

pais1 <- peliculas %>% dplyr::select(`País de grabación 1`,15:41) %>% group_by(`País de grabación 1`) %>% summarise_all(sum)
pais2 <- peliculas %>% dplyr::select(`País de grabación 2`,15:41) %>% group_by(`País de grabación 2`) %>% summarise_all(sum)
pais3 <- peliculas %>% dplyr::select(`País de grabación 3`,15:41) %>% group_by(`País de grabación 3`) %>% summarise_all(sum)


pais1_1 <- pais1 %>% 
  gather(categoria,frec,-`País de grabación 1`) %>% 
  filter(`País de grabación 1`!='') %>% 
  rename(Pais=`País de grabación 1`) %>% 
  data.frame()


pais2_2 <- pais2 %>% 
  gather(categoria,frec,-`País de grabación 2`) %>% 
  filter(`País de grabación 2`!='') %>% 
  rename(Pais=`País de grabación 2`) %>% 
  data.frame()

pais3_3 <- pais3 %>% 
  gather(categoria,frec,-`País de grabación 3`) %>% 
  filter(`País de grabación 3`!='') %>% 
  rename(Pais=`País de grabación 3`) %>% 
  data.frame()


n_generos <- merge(pais1_1,pais2_2,by =c('Pais','categoria')) %>% 
  mutate(frec=frec.x+frec.y) %>%
  dplyr::select(-frec.x,-frec.y) %>% 
  merge(.,pais3_3,by =c('Pais','categoria')) %>%
  mutate(frec=frec.x+frec.y) %>%
  dplyr::select(-frec.x,-frec.y)

class(n_generos)
#write.csv(n_generos,'n_generos.csv')





#------------ MAPAS -------------------------------

geoJson <- st_read("ne_110m_admin_0_countries.geojson")
merge <- merge(n_generos,geoJson,by.x="Pais",by.y="NAME_LONG",all=TRUE)
st_geometry(merge) <- merge$geometry
class(merge)


action_M <- filter(merge,categoria=='Action')
adventure_M <- filter(merge,categoria=='Adventure')
comedia_M <- filter(merge,categoria=='Comedy')
animacion_M <- filter(merge,categoria=='Animation')
guerra_M <- filter(merge,categoria=='War')
familia_M <- filter(merge,categoria=='Family')


#--Paleta colores---
degradado = function (color1, color2, color3,degradados)
{
  library(grDevices)
  palete = colorRampPalette(c(color1, color2,color3))
  palete (degradados)
}

mipaleta = degradado ("#e30000", "#90ed7d","#ffff00", 10)

mypal <- colorNumeric(palette = mipaleta, 
                      domain = comedia_M$frec,
                      alpha = 1,reverse = TRUE)



labels_ac <- sprintf(
  "<strong> Pais: %s</strong><br/> Nº de películas de acción grabadas: %g</sup>",
  action_M$Pais, action_M$frec
) %>% lapply(htmltools::HTML)

labels_av <- sprintf(
  "<strong> Pais: %s</strong><br/> Nº de películas de aventura grabadas: %g</sup>",
  adventure_M$Pais, adventure_M$frec
) %>% lapply(htmltools::HTML)

labels_co <- sprintf(
  "<strong> Pais: %s</strong><br/> Nº de películas de comedia grabadas: %g</sup>",
  comedia_M$Pais, comedia_M$frec
) %>% lapply(htmltools::HTML)


labels_fa <- sprintf(
  "<strong> Pais: %s</strong><br/> Nº de películas de familia grabadas: %g</sup>",
  familia_M$Pais, familia_M$frec
) %>% lapply(htmltools::HTML)


labels_gu <- sprintf(
  "<strong> Pais: %s</strong><br/> Nº de películas de guerra grabadas: %g</sup>",
  guerra_M$Pais, guerra_M$frec
) %>% lapply(htmltools::HTML)


labels_an <- sprintf(
  "<strong> Pais: %s</strong><br/> Nº de películas de animacion grabadas: %g</sup>",
  animacion_M$Pais, animacion_M$frec
) %>% lapply(htmltools::HTML)



leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data=action_M, 
              fillOpacity = 0.7,weight = 1.2,
              fillColor = ~mypal(action_M$frec),
              label = labels_ac,
              group = 'Action')%>%
  addPolygons(data=comedia_M, 
              fillOpacity = 0.7,weight = 1.2,
              fillColor = ~mypal(comedia_M$frec),
              label = labels_co,
              group = 'Comedia')%>%
  addPolygons(data=familia_M, 
              fillOpacity = 0.7,weight = 1.2,
              fillColor = ~mypal(familia_M$frec),
              label = labels_fa,
              group = 'Familia')%>%
  addPolygons(data=guerra_M, 
              fillOpacity = 0.7,weight = 1.2,
              fillColor = ~mypal(guerra_M$frec),
              label = labels_gu,
              group = 'Guerra')%>%
  addPolygons(data=animacion_M, 
              fillOpacity = 0.7,weight = 1.2,
              fillColor = ~mypal(animacion_M$frec),
              label = labels_an,
              group = 'Animacion')%>%
  addPolygons(data=adventure_M, 
              fillOpacity = 0.7,
              fillColor = ~mypal(adventure_M$frec),
              label = labels_av,
              group = 'Adventure') %>% 
  addLayersControl(
    baseGroups =c("Action","Comedia","Familia","Guerra","Animacion","Adventure"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(pal = mypal, values = comedia_M$frec, title = "Nº peliculas")








