

#---------------------------------------------------------------------------------------------------
#------------------------------------ REGRESIÓN Y SALARIOS NBA -------------------------------------
#------------------------------------- PREDICCIÓN (2020/2021) --------------------------------------
#---- 28/10/2020 -------------------------------------------------------- Marta Ruedas Burgos ------
#---------------------------------------------------------------------------------------------------

#### NBA-Predicción ####

#----------------------------------------------------------------------------------------------------
# OBJETIVO: determinar el mejor modelo para predecir el salario de los jugadores de la NBA.
#----------------------------------------------------------------------------------------------------


# Import dataset, nba.csv

library(readr)
library(car) # normalidad nba
library(tidyr) # modelo lineal
library(tidyverse) # modelo lineal

nba <- read_csv("nba.csv") 
View(nba)

#Tabla de la NBA compuesta por 485 jugadores y sus respectivas valoraciones.

# Presentamos un análisis descriptivo.
# Las variables que se nos presentan son categóricas y cuantitativas. 
# La columna "Salary" nos indica el Salario, que es una variable dependiente y es la columna donde nos vamos a enfocar durante este análisis.


#----------------------------------------------------------------------------------------------------
# Modelo de regresión
#----------------------------------------------------------------------------------------------------


vc_nba <- nba %>% select(Salary, NBA_DraftNumber, Age, G, MP, PER) # tabla de las variable cuantitativas

# He creado una tabla llamada vc_nba con las variables cuantitativas más relevantes para el análisis.

#----------------------------------------------------------------------------------------------------
# Modelo lineal 
#----------------------------------------------------------------------------------------------------


regresion_nba <- lm(Salary ~., data = vc_nba) 
summary(regresion_nba)


# Residuals:
#       Min        1Q    Median        3Q       Max 
# -15582460  -3242334   -616497   2478016  23586638 

# He hallado una media de cada individuo con la regresión

# El P valor indica que si es menor al 5 % significa que es una variable relevante dentro del modelo.
# Y si es mayor no resulta relevante, usando un nivel de significación del 5%.

coefficients(regresion_nba)

#(Intercept)          Age            G           MP          PER        `TS%`       `3PAr`          FTr 
#-1595886.922   513255.761  -162996.127     5920.031  -385430.514 -3421384.245 -5725399.371  -506844.390 
#    `ORB%`       `DRB%`       `TRB%`       `AST%`       `STL%`       `BLK%`       `TOV%`       `USG%` 
#-1271021.098  -982859.702  2344549.530   -29704.936  -221468.727   198202.813   -10270.630   176357.159 
#     OWS          DWS           WS      `WS/48`         OBPM         DBPM          BPM         VORP 
#-2871579.939 -3075987.692  3457772.884 -4061192.205  3026489.950  2284450.996 -2144170.837   433149.376 

# El ejemplo de la edad, cuanto mas años tenga el jugador va a cobrar 513255 dolares. 
# El ejemplo de los partidos, cuanto más partidos juegue va a cobrar menos. 

#----------------------------------------------------------------------------------------------------
# Predicción
#----------------------------------------------------------------------------------------------------

# Los modelos de regresión tiene de objetivos predecir la variable dependiente.

prediccion_nba <- predict(regresion_nba, newdata = vc_nba)
summary(prediccion_nba)

# Observamos
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -3355000  2440174  6103239  6660622 10070237 31091979        2

# El mínimo es de -3.355.000 y el máximo de 31.091.979.

#----------------------------------------------------------------------------------------------------
# Modelo de predicción 
# Ejemplo de 10 jugadores aleatorios
#----------------------------------------------------------------------------------------------------

# Primero quiero analizar un jugador, he escogido a LebronJames.

# Primer ejemplo LebronJames

# Primero creamos la función tipo salario para la tabla nba, la he llamado slary_nba_prediccion
# Segundo creamos la predicción de dicho modelo, llamada prediccion_nba

salary_nba_prediccion <- function(model, NBA_DraftNumber, Age, G, MP, PER){
  prediccion_nba <- predict(model, data.frame( NBA_DraftNumber, Age, G, MP, PER))
}
SalarioLeBronJames<- salary_nba_prediccion(regresion_nba, 1,
                       33,
                       78,
                       2898,
                       28.5)
SalarioLeBronJames

# Tercero cuadramos los valores asociados a LebronJames para que nos salga un resultado indicándonos el tipo de salario según la predicción.


# El salario obtenido ha sido 22.978.838 millones de dolares, el salario de LeBron James aproximadamente.


#----------------------------------------------------------------------------------------------------
# Ejemplo de 10 jugadores aleatorios
#----------------------------------------------------------------------------------------------------

# Set.seed(1234) 

set.seed(1234) # si ejecutamos set.seed(1234) nunca varian, siempre nos daría el mismo resultado aleatorio.

# Organizamos los diez jugadores aleatorios con su nombre, lo llamamos tabla_aleatoria_10jugadores

tabla_aleatoria_10jugadores <- nba %>% select(Player, Salary, NBA_DraftNumber, Age, G, MP, PER)

n <- 10
muestra_diez <- sample(1:nrow(tabla_aleatoria_10jugadores), size = n, replace = FALSE)
datos_muestra_diez <- tabla_aleatoria_10jugadores[muestra_diez, ]
datos_muestra_diez


# # A tibble: 10 x 7
# Player             Salary NBA_DraftNumber   Age     G    MP   PER
# <chr>               <dbl>           <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 Larry Drew         148318              62    27    10    70   3  
# 2 Darren Collison  10000000              21    30    65  1929  18.9
# 3 Denzel Valentine  2186400              14    24    77  2095  12.1
# 4 Miles Plumlee    12500000              26    29    52   888  10.7
# 5 Bismack Biyombo  17000000               7    25    78  1428  13.9
# 6 Ian Mahinmi      16661641              28    31    74  1096  12.9
# 7 Domantas Sabonis  2550000              11    21    70  1711  17.3
# 8 Caleb Swanigan    1465920              26    20    24   165   7.1
# 9 Tim Frazier       2000000              62    27    56   807  11.2
# 10 Kyle Singler      4666500              33    29    12    59   5.9

# Salario real
# Con esta muestra hallamos el modelo de predicción con la siguiente formula:

prediccion_nba <- predict(regresion_nba, datos_muestra_diez)
prediccion_nba

# Predicción basándonos en las variables de arriba. 

#1        2        3        4        5        6        7        8        9       10 
#3418989 11971318 11179077  2237362  4634581  7712372  9922487  6822006  5173834  5810496 

# El resultado que hemos obtenido son los salarios aleatorios de los datos_muestra_diez (10 jugadores aleatorios de la NBA).
# Podemos observar que el salario más elevado es el segundo con casi 12 millones de dolares.


#----------------------------------------------------------------------------------------------------
# Normalidad NBA 
#----------------------------------------------------------------------------------------------------


 qqPlot(regresion_nba, labels=row.names(vc_nba), id.method="identify",
        simulate=TRUE, main="Q-Q Plot")
 
 # Comparación de dos distribuciones, gráfico.


#----------------------------------------------------------------------------------------------------
# Histograma + densidad + normal + rug 
#----------------------------------------------------------------------------------------------------

 
 residplot <- function(fit, nbreaks=10) {
   z <- rstudent(fit)
   hist(z, breaks=nbreaks, freq=FALSE,
        xlab="Studentized Residual",
        main="Distribution of Errors")
   rug(jitter(z), col="brown")
   curve(dnorm(x, mean=mean(z), sd=sd(z)),
         add=TRUE, col="blue", lwd=2)
   lines(density(z)$x, density(z)$y,
         col="red", lwd=2, lty=2)
   legend("topright",
          legend = c( "Normal Curve", "Kernel Density Curve"),
          lty=1:2, col=c("blue","red"), cex=.7)
 }
 
 residplot(regresion_nba)
 
 #----------------------------------------------------------------------------------------------------
 # Jarque Bera, Shapiro-Wilk 
 #----------------------------------------------------------------------------------------------------

 # El test de Shapiro-Wilk permite comprobar si una muestra ha sido generada por un distribución normal.
 vResid <- resid(regresion_nba)
 shapiro.test(vResid)
 
 # Comprobamos si los residuos del modelo de regresion siguen una distribucion normal.
 #   Shapiro-Wilk normality test
 #        data:  vResid
 #    W = 0.97066, p-value = 2.981e-08
 
 # Podemos observar que el p-valor es menor que el 5% del nivel de significación por lo tanto concluimos que no sigue una distribución normal.
 
 #----------------------------------------------------------------------------------------------------
 # Linealidad
 #----------------------------------------------------------------------------------------------------

 crPlots(regresion_nba)
 
 # Se grafican los valores ajustados con respecto a los predictores, si no hay problemas de linealidad se obtiene un recta sobre las que se representan los puntos.
 # Componentes más residuales.
 # En este caso no se representan al haber problemas de linealidad.
 
 #----------------------------------------------------------------------------------------------------
 # Outliers
 #----------------------------------------------------------------------------------------------------
 
 outlierTest(regresion_nba)
 # Observamos los valores extremos de nuestros datos.
 
 #rstudent unadjusted p-value Bonferroni p
 #328 4.311181         1.9732e-05    0.0095698
 #114 3.994462         7.5059e-05    0.0364040
 
 # Esto significa que los jugadores que están en la posición 328 y 114 son valores extremos dentro de la regresión.
 # Por lo que si los quitamos dentro de la tabla obtendremos una mayor precisión en nuestro modelo de predicción.

 
 #----------------------------------------------------------------------------------------------------
 # Guardar en archivo html
 #----------------------------------------------------------------------------------------------------
 
  install.packages("XQuartz")
 
 # Al guardar el archivo en html me da un error en donde me indica que necesito el paquete XQuartz para poder guárdarlo.
 # He intentado descargar XQuartz para R, también por varias fuentes y por x motivos no me permite pasar este archivo a html. 




