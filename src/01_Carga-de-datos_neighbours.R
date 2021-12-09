library(tidyverse)
library(sf)
library(ggplot2)
library(lubridate)
library(corrplot)
library(vtreat)
library(CatEncoders)
library(caTools)
library(FNN)
install.packages("FNN")
#install.packages("CatEncoders")
#install.packages("caTools")
#install.packages("vtreat")
#install.packages('Amelia')
#install.packages('corrplot')

options(scipen=999)

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

barrios <- st_read("data/raw/GCABA/barrios/barrios_badata_wgs84.shp") %>% 
    st_transform(proj) %>% 
    select(BARRIO)

manzanas <- st_read("data/raw/GCABA/manzanas/manzanas.geojson") %>% 
    st_transform(proj) %>% 
    select(SM, MZ_SUP) %>% 
    st_join(barrios)

#_______________________________________________________________________________
### Filtramos el df de properati, que pesa 1GB

#prop <- vroom::vroom("data/raw/Properati/ar_properties.csv") %>% 
#    dplyr::filter(l2=="Capital Federal",
#                  operation_type=="Venta",
#                  property_type=="Departamento") %>% 
#    drop_na(lon, lat, created_on, l2, l3, rooms, bedrooms, bathrooms, surface_total, 
#            surface_covered, price, currency, property_type)

#write.csv (prop, "data/raw/Properati/ar_properties_filtrado.csv")
#_______________________________________________________________________________

prop <- vroom::vroom("data/raw/Properati/ar_properties_filtrado.csv") %>% 
    select(...1, created_on, lat, lon, rooms, bedrooms, bathrooms, surface_total, 
           surface_covered, price, currency) %>% 
    dplyr::filter(currency=="USD") %>% 
    rename(id=...1) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(proj) %>% 
    st_intersection(manzanas) %>% 
    mutate(price_m2=price/surface_total)

# Eliminamos outliers

boxplot(prop$price) # muchos valores extremos superiores

Q1 <- quantile(prop$price, 0.25)
Q3 <- quantile(prop$price, 0.75)
IQR <- Q1-Q3

prop <- prop %>% 
    dplyr::filter(price < (Q1-1.5*IQR),
                  price > (Q3+1.5*IQR))


# Precio promedio por barrio
prop_precio_barrio <- prop %>% 
    as.data.frame() %>% 
    group_by(BARRIO) %>% 
    summarise(promedio_barrio=mean(price))

# Manzanas con más ventas
venta_por_manzana <- prop %>% 
    as.data.frame() %>% 
    group_by(SM) %>% 
    summarise(cantidad_venta=n())

manzanas <- manzanas %>% left_join(venta_por_manzana, by="SM")

ggplot()+
    geom_sf(data=manzanas, fill="grey90", color="grey80")+
    geom_sf(data=manzanas %>% dplyr::filter(!is.na(cantidad_venta)), aes(fill=cantidad_venta), color=NA)+
    scale_fill_viridis_c(option = "magma", direction=-1) +
    theme_void()

##
prop <- prop %>% left_join(venta_por_manzana, by="SM")


# FECHAS


prop2 <- prop %>% 
    cbind(st_coordinates(.)) %>% 
    rename(lon=X,
           lat=Y) %>% 
    as.data.frame() %>% 
    select(-c(geometry, Z))


split <- sample.split(prop2, SplitRatio =0.75)

train <- subset(prop2, split==TRUE) #24779 instancias
test <- subset(prop2, split==FALSE) #8259 instancias

x <- prop2 %>% select(-c(price, price_m2, id, created_on, SM, currency, MZ_SUP, BARRIO))
y <- prop2$price #variable a predecir (PRECIO)


# boxplot para cada destribución
par(mfrow=c(1,4))
for(i in 1:4) {
    boxplot(x[,i], main=names(prop2)[i])
}



control <- trainControl(method="cv", number=10)
metric <- "RMSE"


#MODELS
prop2 <- prop2 %>% select(-c(price_m2, id, created_on, SM, currency, MZ_SUP, BARRIO))


pred_001 = knn.reg(train = train, test = test, y = y, k = 1)
pred_001 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 1)
pred_005 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 25)
pred_010 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 50)
pred_050 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 100)
pred_100 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 500)
pred_250 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 1000)

# a) linear algorithms
set.seed(7)
fit.lda <- train(price~., data=prop2, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(price~., data=prop2, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(price~., data=prop2, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(price~., data=prop2, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(price~., data=prop2, method="rf", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)

print(fit.lda)







#MISSING
#Amelia:: missmap(prop,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

# REGRESION

#lapply(prop2,class)
#str(prop2)

prop2 <- prop %>% 
    as.data.frame() %>% 
    select(id, rooms, bedrooms, bathrooms, surface_total, price)

# si le doy unlist a prop2, esto funciona
lab_enc = LabelEncoder.fit(prop2)


set.seed(123)

#Split the data , `split()` assigns a booleans to a new column based on the SplitRatio specified. 
split <- sample.split(prop2, SplitRatio =0.75)

train <- subset(prop2,split==TRUE)
test <- subset(prop2,split==FALSE)

model <- lm(price ~ rooms + bedrooms + bathrooms + surface_total , data = train)
summary(model)


res <- residuals(model)
res <- as.data.frame(res) # Convert residuals to a DataFrame 
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)

plot(model)


test$predicted.price <- predict(model,test)

test %>% 
    ggplot(aes(price, predicted.price)) +
    geom_point(alpha=0.5) + 
    stat_smooth(aes(color='black')) +
    xlab('Precio') +
    ylab('Valor predicho')+
    theme_bw()


error <- test$price-test$predicted.price
rmse <- sqrt(mean(error)^2)