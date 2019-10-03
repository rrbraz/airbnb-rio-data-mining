
# O dataset
#  Disponível em: http://insideairbnb.com/get-the-data.html
#  Download:
if(!file.exists("airbnb.csv.gz")) {
  download.file("http://data.insideairbnb.com/brazil/rj/rio-de-janeiro/2019-07-15/data/listings.csv.gz", "airbnb.csv.gz")
}

# Importa o dataset

library(tidyverse)
library(ggplot2)
library(leaflet)

airbnb <- read_csv(gzfile("airbnb.csv.gz"))

# seleciona as colunas
airbnb <- airbnb %>% select(name, host_name, neighbourhood_cleansed, latitude, longitude, property_type, room_type, price, accommodates,  bedrooms,  minimum_nights, availability_365,  number_of_reviews,  review_scores_rating,  cancellation_policy,  require_guest_profile_picture,  require_guest_phone_verification)

# transforma algumas colunas em fatores (dados categóricos)
names_to_factor <- c("host_name", "neighbourhood_cleansed", "room_type", "property_type", "cancellation_policy")
airbnb[names_to_factor] <- map(airbnb[names_to_factor], as.factor)

# transforma o preço em numérico
airbnb$price <- as.numeric(gsub('\\$|,', '', airbnb$price))

airbnb

### Descrição dos dados

airbnb %>% summary()
glimpse(airbnb)


## Missing Data
# Missing antes de remover

missing_airbnb <- summarise_all(airbnb, ~sum(is.na(.)))	
missing_airbnb <- gather(missing_airbnb, key = "variables", value = "missing")
missing_airbnb %>% filter(missing > 0)

# Remove sem reviews
airbnb <- airbnb %>% filter(number_of_reviews != 0)
# Remove preço 0
airbnb <- airbnb %>% filter(price != 0)
# Remove os NA
airbnb <- airbnb %>% drop_na(review_scores_rating)
airbnb <- airbnb %>% drop_na(bedrooms)

glimpse(airbnb)

# Missing após remover

missing_airbnb <- summarise_all(airbnb, ~sum(is.na(.)))	
missing_airbnb <- gather(missing_airbnb, key = "variables", value = "missing")
missing_airbnb %>% filter(missing > 0)

## Visualização

### Bairros
n_bairros <- 7 

bairros <- airbnb %>% 
  group_by(neighbourhood_cleansed) %>% 
  tally(sort=TRUE) %>%
  group_by(bairro = factor(c(
    as.character(neighbourhood_cleansed[1:n_bairros]), rep("Outros", n() - n_bairros)),
    levels = c(as.character(neighbourhood_cleansed[1:n_bairros]), "Outros"))) %>%
  tally(n) 

bairros %>%
  ggplot(aes(bairro, n, fill=bairro)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=-0.4, size=3.5) +
  theme(legend.position = "none") +
  xlab("Bairro") +
  ylab("Frquência")

### Tipo de quarto
ggplot(airbnb, aes(x=room_type, fill=room_type)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.4, size=3.5)

### Tipo de propriedade
n_tipos <- 6

tipos_propriedade <- airbnb %>% 
  group_by(property_type) %>% 
  tally(sort=TRUE) %>%
  group_by(tipo_propriedade = factor(c(
    as.character(property_type[1:n_tipos]), rep("Outros", n() - n_tipos)),
    levels = c(as.character(property_type[1:n_tipos]), "Outros"))) %>%
  tally(n) 

tipos_propriedade %>%
  ggplot(aes(tipo_propriedade, n, fill=tipo_propriedade)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=-0.4, size=3.5) +
  xlab("Tipo de propriedade") +
  ylab("Frequência") +
  theme(axis.text = element_blank())

### Política de cancelamento
airbnb %>%
  group_by(cancellation_policy) %>%
  tally(sort=TRUE) %>%
  ggplot(aes(x=reorder(cancellation_policy, -n), y=n, fill=reorder(cancellation_policy, -n))) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=n), vjust=-0.4, size=3.5) +
  theme(axis.text.x = element_blank()) +
  xlab("Política de cancelamento") +
  ylab("Frequência") +
  labs(fill="Política de cancelamento")

### Requer foto de perfil do hóspede
# Tema para pie charts
blank_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x=element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold") 
)

airbnb %>%
  ggplot(aes(x="", fill=require_guest_profile_picture)) +
  geom_bar(width=1) +
  coord_polar("y", start=0) +
  blank_theme + 
  geom_text(stat='count',aes(label=..count..), position = position_stack(vjust = 0.5), color="white") +
  labs(fill="") +
  ggtitle("Requer foto de perfil do hóspede")

### Requer que o hóspede tenha telefone verificado
airbnb %>%
  ggplot(aes(x="", fill=require_guest_phone_verification)) +
  geom_bar(width=1) +
  coord_polar("y", start=0) +
  blank_theme + 
  geom_text(stat='count',aes(label=..count..), position = position_stack(vjust = 0.5), color="white") +
  labs(fill="") +
  ggtitle("Requer que o hóspede tenha telefone verificado")

### Accommodates
airbnb$accommodates %>% summary()

airbnb %>% 
  filter(accommodates < 50) %>%
  ggplot(aes(x = "", y = accommodates)) +
  geom_boxplot(fill="purple") + 
  xlab("") + 
  ylab("Número") +
  ggtitle("Quantidade máxima de pessoas acomodadas",
          subtitle = "Removido um valor 160") +
  theme(legend.position = "none")

airbnb %>% 
  filter(accommodates < 50) %>%
  ggplot(aes(accommodates)) +
  geom_histogram(bins = 31, fill="purple") + 
  xlab("") + 
  ylab("Frequência") +
  ggtitle("Quantidade máxima de pessoas acomodadas",
          subtitle = "Removido um valor 160") +
  theme(legend.position = "none")

### Quartos
airbnb$bedrooms %>% summary()

airbnb %>% 
  ggplot(aes(x = "", y = bedrooms)) +
  geom_boxplot(fill="lightblue") + 
  xlab("") + 
  ylab("Número") +
  ggtitle("Número de quartos") +
  theme(legend.position = "none")

airbnb$bedrooms %>% summary()

airbnb %>% 
  ggplot(aes(bedrooms)) +
  geom_histogram(bins=28, fill="lightblue") + 
  xlab("Quartos") + 
  ylab("Frequência") +
  ggtitle("Número de quartos") +
  theme(legend.position = "none")

### Número mínimo de noites
airbnb$minimum_nights %>% summary()

airbnb %>% 
  ggplot(aes(x = "", y = minimum_nights)) +
  geom_boxplot(fill="lightblue") + 
  scale_y_log10() + 
  xlab("") + 
  ylab("Noites") +
  ggtitle("Número mínimo de noites", subtitle = "Com escala logarítmica") +
  theme(legend.position = "none")

airbnb %>% 
  #  filter(minimum_nights < 50) %>%
  ggplot(aes(minimum_nights)) +
  geom_histogram(bins=28, fill="lightgreen") + 
  xlab("Noites") + 
  ylab("Frequência") +
  ggtitle("Número mínimo de noites") +
  theme(legend.position = "none")

airbnb %>% 
  filter(minimum_nights <= 20) %>%
  ggplot(aes(minimum_nights)) +
  geom_histogram(bins=19, fill="lightgreen") + 
  xlab("Noites") + 
  ylab("Frequência") +
  ggtitle("Número mínimo de noites", subtitle = "Removidos valores maiores que 20") +
  theme(legend.position = "none")

### Disponibilidade 365
airbnb$availability_365 %>% summary()

airbnb %>% 
  ggplot(aes(x = "", y = availability_365)) +
  geom_boxplot(fill="blue") + 
  xlab("") + 
  ylab("Dias") +
  ggtitle("Disponibilidade 365") +
  theme(legend.position = "none")

airbnb %>% 
  ggplot(aes(availability_365)) +
  geom_histogram(bins=28, fill="blue") + 
  xlab("Dias") + 
  ylab("Frequência") +
  ggtitle("Disponibilidade 365") +
  theme(legend.position = "none")


### Número de avaliações
airbnb$number_of_reviews %>% summary()

airbnb %>% 
  ggplot(aes(x = "", y = number_of_reviews)) +
  geom_boxplot(fill="pink") + 
  scale_y_log10() +
  xlab("") + 
  ylab("Avaliações") +
  ggtitle("Número de avaliações", subtitle = "Com escala logaritmica") +
  theme(legend.position = "none")

airbnb %>% 
  filter(number_of_reviews <= 150) %>% 
  ggplot(aes(number_of_reviews)) +
  geom_histogram(bins=30, fill="pink") + 
  xlab("Avaliações") + 
  ylab("Frequência") +
  ggtitle("Número de avaliações", subtitle = "Filtrados valores acima de 150") +
  theme(legend.position = "none")


### Avaliação
airbnb$review_scores_rating %>% summary()

airbnb %>% 
  ggplot(aes(x = "", y = review_scores_rating)) +
  geom_boxplot(fill="#18d9cc") + 
  xlab("") + 
  ylab("Avaliações") +
  ggtitle("Número de avaliações") +
  theme(legend.position = "none")

airbnb %>% 
  ggplot(aes(review_scores_rating)) +
  geom_histogram(bins=50, fill="#18d9cc") + 
  xlab("Avaliação") + 
  ylab("Frequência") +
  ggtitle("Avaliação") +
  theme(legend.position = "none")

airbnb %>% 
  filter(review_scores_rating < 100) %>%
  ggplot(aes(review_scores_rating)) +
  geom_histogram(bins=50, fill="#18d9cc") + 
  xlab("Avaliação") + 
  ylab("Frequência") +
  ggtitle("Avaliação", subtitle = "Removidas notas 100") +
  theme(legend.position = "none")

airbnb %>%
  mutate(notamaxima = (review_scores_rating == 100)) %>%
  group_by(notamaxima) %>%
  ggplot(aes(x="", fill=notamaxima)) +
  geom_bar(width=1) +
  coord_polar("y", start=0) +
  blank_theme + 
  geom_text(stat="count", aes(label=..count..), position=position_stack(vjust = 0.5), color="white") +
  labs(fill="Nota máxima") +
  ggtitle("Nota máxima (100)")


### Preço
ggplot(airbnb, aes(price, fill=room_type)) +
  geom_histogram(bins = 30) + 
  ggtitle("Distribução de preço",
          subtitle = "A distribuição é muito inclinada") +
  theme(axis.title = element_text(), axis.title.x = element_text())

ggplot(airbnb, aes(price, fill=room_type)) +
  geom_histogram(bins = 30) + 
  ggtitle("Distribuição transformada do preço",
          subtitle = expression("Com uma transformação" ~'log'[10] ~ "do eixo x")) +
  scale_x_log10()

ggplot(airbnb, aes(price, fill=room_type)) +
  geom_histogram(bins = 30, aes(y = ..density..), show.legend = FALSE) +
  facet_wrap(~room_type) +  
  scale_x_log10() 

ggplot(airbnb, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  xlab("Tipo de quarto") + 
  ylab("Preço") +
  ggtitle("Boxplots de preço por tipo de quarto") +
  geom_hline(yintercept = mean(airbnb$price), color = "purple", linetype = 2) +
  theme(legend.position = "none")

## Correlação
library(corrplot)
airbnb_cor <- airbnb[, sapply(airbnb, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")

## Mapa com as listagens
pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"), domain = airbnb$room_type)

leaflet(data = airbnb) %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
  addCircleMarkers(~longitude, 
                   ~latitude, 
                   color=~pal(room_type), 
                   weight = 1, 
                   radius=1, 
                   fillOpacity = 0.1, 
                   opacity = 1,
                   label = paste("Name:", airbnb$name)) %>% addLegend("bottomright", pal = pal, values = ~room_type,
                                                                      title = "Room types",
                                                                      opacity = 1)

library(plotly)
plot_ly(airbnb, x = ~longitude, y = ~latitude, z = ~price, color = ~room_type) 



