#Instalamos la librerías a utilizar
install.packages('RhpcBLASctl')
install.packages('ggplot2')

# Las llamamos para ser utilizadas
library(RhpcBLASctl)
library(ggplot2)

# 1. Definir los factores exigentes para el experimento
tamanos <- c(4000, 6000, 8000, 10000)
hilos <- c(1, 2, 4, 8)
repeticiones <- 30

# 2. Crear el espacio muestral y aleatorizar
diseno <- expand.grid(Tamano = tamanos, Hilos = hilos, Repeticion = 1:repeticiones)

set.seed(42)
orden_aleatorio <- sample(nrow(diseno))
diseno_aleatorizado <- diseno[orden_aleatorio, ]

diseno_aleatorizado$Tiempo_Seg <- NA
total_corridas <- nrow(diseno_aleatorizado)

# 3. Ejecutar la simulación factorial en Linux
for (i in 1:total_corridas) {
  n <- diseno_aleatorizado$Tamano[i]
  h <- diseno_aleatorizado$Hilos[i]
  
  # Configuramos el planificador del kernel, dado que este es en Linux, se respetará esta instrucción de forma estricta
  blas_set_num_threads(h)
  omp_set_num_threads(h)
  
  gc(verbose = FALSE) 
  
  x <- matrix(runif(n * n), n, n)
  tiempo <- system.time({
    resultado <- x %*% x
  })[["elapsed"]]
  
  diseno_aleatorizado$Tiempo_Seg[i] <- tiempo
  
  cat(sprintf("Corrida %d/%d | Tamaño: %d | Hilos: %d | Tiempo: %.3f seg\n", 
              i, total_corridas, n, h, tiempo))
}

# Definimos los hilos como un factor
diseno_aleatorizado$Hilos <- as.factor(diseno_aleatorizado$Hilos)


# Generamos el gráfico de línea para intuir si puede existir interacción
ggplot(diseno_aleatorizado, aes(x=Tamano, y=Tiempo_Seg, group=Hilos)) + stat_summary(fun=mean, geom ='line', aes(linetype = Hilos))



# Exportarmos los datos crudos
write.csv(diseno_aleatorizado, file = 'datos_experimento.csv', row.names = FALSE)


# Definir una paleta de colores para los núcleos
colores <- c("lightcoral", "lightblue", "lightgreen", "mediumpurple")

# 1. Boxplot para la matriz
boxplot(Tiempo_Seg ~ Hilos, 
        data = diseno_aleatorizado,
        main = "Tiempo de resolución según núcleos",
        xlab = "Núcleos (Hilos)",
        ylab = "Tiempo (Segundos)",
        col = colores)
