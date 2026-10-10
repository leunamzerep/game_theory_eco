# =============================================================================
# Percepciones previas, precio negociado y excedente percibido
# Experimento de negociación bilateral "Duopoly" (vendedor - comprador)
#
# Orden del script (igual al de la metodología del working paper):
#   0. Preparación de los datos
#   1. Descripción de la muestra
#   2. Excedentes percibidos
#   3. Matriz de decisión (escenarios de percepción)
#   4. Eficiencia de Pareto
#   5. Estimación econométrica (MCO con intercepto y errores robustos HC1)
#   6. Controles: edad, sexo y conocimiento sobre consolas
#   7. Punto focal: prueba binomial de la opción "promedio"
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PREPARACIÓN DE LOS DATOS
# -----------------------------------------------------------------------------

# Paquetes: se instalan solo si faltan
for (p in c("readxl", "sandwich", "lmtest")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(readxl)     # leer el archivo de Excel
library(sandwich)   # errores estándar robustos (HC1)
library(lmtest)     # coeftest(): tabla de coeficientes con errores robustos

setwd("C:/Users/User/Downloads")

# duo: base completa, una fila por ronda de negociación
duo <- read_excel("DuopolyData2026.xlsx", sheet = "Data")

# La hoja tiene una fila de totales al final (sin precio): se elimina
duo <- subset(duo, !is.na(chosenPrice))

# duo_clean: solo las rondas con acuerdo (se quitan las abandonadas, "NN")
duo_clean <- subset(duo, chosenPrice != "NN")
duo_clean$chosenPrice <- as.numeric(duo_clean$chosenPrice)

# Variables de trabajo (cada una es una columna de duo_clean):
sper  <- duo_clean$sellerPercieved  # valor percibido por el vendedor
stop  <- duo_clean$sellerTop        # precio mínimo que aceptaría el vendedor (precio de reserva)
spro  <- duo_clean$sellerProposed   # precio que propondría el vendedor
bper  <- duo_clean$buyerPercieved   # valor percibido por el comprador
btop  <- duo_clean$buyerTop         # precio máximo que pagaría el comprador (precio de reserva)
bpro  <- duo_clean$buyerProposed    # precio que propondría el comprador
avg   <- duo_clean$average          # opción "promedio" = media de stop, spro, btop y bpro
pact  <- duo_clean$chosenPrice      # precio pactado (opción elegida por ambos)
sexp  <- duo_clean$sellerKnows      # 1 = el vendedor conoce de consolas / le gustan los videojuegos
bexp  <- duo_clean$buyerKnows       # 1 = el comprador conoce de consolas / le gustan los videojuegos
smale <- duo_clean$sellerMale       # 1 = vendedor hombre (sexo al nacer)
bmale <- duo_clean$buyerMale        # 1 = comprador hombre (sexo al nacer)
sage  <- duo_clean$sellerAge        # edad del vendedor
bage  <- duo_clean$buyerAge         # edad del comprador

avg_price <- mean(pact)             # precio pactado promedio de la muestra (umbral de la matriz)
diff_per  <- abs(sper - bper)       # distancia absoluta entre las dos percepciones


# -----------------------------------------------------------------------------
# 1. DESCRIPCIÓN DE LA MUESTRA
# -----------------------------------------------------------------------------

cat("Rondas totales:", nrow(duo),
    "| Con acuerdo:", nrow(duo_clean),
    "| Abandonadas (NN):", sum(duo$chosenPrice == "NN"), "\n")

# Edades de todos los participantes (vendedores y compradores juntos)
edades <- c(duo$sellerAge, duo$buyerAge)   # vector con todas las edades registradas
cat("Edad mínima:", min(edades), "| máxima:", max(edades),
    "| mediana:", median(edades), "| registros menores de 18:", sum(edades < 18), "\n")

cat("Precio pactado promedio:", round(avg_price), "\n")
cat("Distancia media entre percepciones (con acuerdo):", round(mean(diff_per)), "\n")

# Estadística descriptiva de cada variable
print(summary(pact)) 
print(summary(sper)) 
print(summary(bper))
print(summary(stop))
print(summary(btop))
print(summary(spro))
print(summary(bpro))
print(summary(sage))
print(summary(bage))
print(summary(sexp))
print(summary(bexp))
print(summary(smale))
print(summary(bmale))

# Zona de Posible Acuerdo (ZOPA): existe si el mínimo del vendedor <= máximo del comprador
# sin_zopa: TRUE si el mínimo del vendedor supera el máximo del comprador
sin_zopa <- duo$sellerTop > duo$buyerTop
cat("Rondas sin ZOPA:", sum(sin_zopa), "de", nrow(duo),
    "= Del total, el número de rondas que cerraron el trato fueron:", sum(sin_zopa & duo$chosenPrice != "NN"), "\n")

# Rondas abandonadas: distancia entre percepciones
duo_NN <- subset(duo, chosenPrice == "NN")                            # solo rondas abandonadas
diff_perNN <- abs(duo_NN$sellerPercieved - duo_NN$buyerPercieved)     # distancia de percepciones en ellas
cat("Distancia media entre percepciones (abandonadas):", round(mean(diff_perNN)), "\n")


# -----------------------------------------------------------------------------
# 2. EXCEDENTES PERCIBIDOS
#    Cada parte compara el precio pactado con su propio valor percibido.
# -----------------------------------------------------------------------------

duo_clean$su <- pact - sper   # su: excedente percibido del vendedor  = precio - valor percibido vendedor
duo_clean$bu <- bper - pact   # bu: excedente percibido del comprador = valor percibido comprador - precio
duo_clean$jt <- duo_clean$su + duo_clean$bu   # jt: excedente conjunto (ganancia total del intercambio)

summary(duo_clean$su)
summary(duo_clean$bu)

# -----------------------------------------------------------------------------
# 3. MATRIZ DE DECISIÓN (ESCENARIOS DE PERCEPCIÓN)
#    Estados del mundo: si cada parte percibió el bien por encima (sobreestima)
#    o por debajo (subestima) del precio pactado promedio de la muestra.
#    Alternativas: aceptar una de las cinco opciones o abandonar (pago 0, 0).
#    En cada celda: excedentes promedio (vendedor, comprador).
#    Nombres de los escenarios: primero VENDEDOR, luego COMPRADOR.
# -----------------------------------------------------------------------------

duo_clean$ss <- ifelse(sper > avg_price, 1, 0)  # ss: 1 = vendedor sobreestima, 0 = subestima
duo_clean$bs <- ifelse(bper > avg_price, 1, 0)  # bs: 1 = comprador sobreestima, 0 = subestima

# Escenarios (vendedor_comprador):
over_over <- subset(duo_clean, ss == 1 & bs == 1)  # vendedor sobreestima, comprador sobreestima
over_sub  <- subset(duo_clean, ss == 1 & bs == 0)  # vendedor sobreestima, comprador subestima
sub_over  <- subset(duo_clean, ss == 0 & bs == 1)  # vendedor subestima,   comprador sobreestima
sub_sub   <- subset(duo_clean, ss == 0 & bs == 0)  # vendedor subestima,   comprador subestima

# Excedentes promedio de cada escenario, redondeados (vendedor ; comprador)
celda <- function(df) paste0("(", round(mean(df$su)), " ; ", round(mean(df$bu)), ")")

# Formato de matriz de pagos 2x2: celdas "(vendedor ; comprador)"
pagos <- matrix(c(celda(over_over), celda(over_sub),
                  celda(sub_over),  celda(sub_sub)),
                nrow = 2, byrow = TRUE,
                dimnames = list(Vendedor  = c("Sobreestima", "Subestima"),
                                Comprador = c("Sobreestima", "Subestima")))
print(pagos)


# -----------------------------------------------------------------------------
# 4. EFICIENCIA DE PARETO (según los valores percibidos)
#    Un acuerdo es eficiente si el comprador valora el bien al menos tanto
#    como el vendedor (bper >= sper); si no, el bien pasó a quien lo valora menos.
#    Un abandono pierde ganancia si el comprador lo valoraba más (bper > sper).
# -----------------------------------------------------------------------------

duo_clean$eficiente <- bper >= sper   # eficiente: TRUE si el acuerdo es eficiente en sentido de Pareto
cat("Acuerdos eficientes:", sum(duo_clean$eficiente),
    "| ineficientes:", sum(!duo_clean$eficiente), "\n")
cat("Abandonos con ganancia perdida:",
    sum(duo_NN$buyerPercieved > duo_NN$sellerPercieved), "de", nrow(duo_NN), "\n")


# -----------------------------------------------------------------------------
# 5. ESTIMACIÓN ECONOMÉTRICA: MODELO PRINCIPAL
#    MCO con intercepto. Precio pactado explicado por las dos percepciones.
#    Errores estándar robustos a heterocedasticidad (HC1), porque la dispersión
#    de los errores crece con el nivel de los precios.
# -----------------------------------------------------------------------------

# model4: modelo principal (precio ~ percepción vendedor + percepción comprador)
model4 <- lm(chosenPrice ~ sellerPercieved + buyerPercieved, data = duo_clean)
V4 <- vcovHC(model4, type = "HC1")   # V4: matriz de varianzas y covarianzas robusta (HC1)
print(coeftest(model4, vcov = V4))
cat("R²:", round(summary(model4)$r.squared, 4), "| n =", nobs(model4), "\n")
# Lectura: cada coeficiente indica cuántos pesos cambia el precio pactado por
# cada peso adicional en el valor percibido de esa parte.

# -----------------------------------------------------------------------------
# 6. CONTROLES: EDAD, SEXO Y CONOCIMIENTO SOBRE CONSOLAS
#    Se añaden al modelo principal para verificar si alguna característica
#    altera el resultado. Se revisa: (a) el p-valor de cada control y
#    (b) si los coeficientes de las percepciones se mantienen.
# -----------------------------------------------------------------------------

# model7: modelo principal + características de ambos jugadores
model7 <- lm(chosenPrice ~ sellerPercieved + buyerPercieved +
               sellerAge + buyerAge + sellerMale + buyerMale +
               sellerKnows + buyerKnows, data = duo_clean)
print(coeftest(model7, vcov = vcovHC(model7, type = "HC1")))
cat("R² con controles:", round(summary(model7)$r.squared, 4), "\n")


# -----------------------------------------------------------------------------
# 7. PUNTO FOCAL: ¿LA OPCIÓN "PROMEDIO" SE ELIGE MÁS DE LO ESPERADO?
#    Si las cinco opciones fueran igual de atractivas, cada una se elegiría
#    el 20 % de las veces. Prueba binomial contra p = 0,2.
# -----------------------------------------------------------------------------

# opcion: cuál de las cinco opciones coincide con el precio pactado
# (si el promedio coincide con otra cifra, se clasifica como "promedio")
duo_clean$opcion <- with(duo_clean,
                         ifelse(chosenPrice == average,        "promedio",
                                ifelse(chosenPrice == sellerTop,      "minimo_vendedor",
                                       ifelse(chosenPrice == sellerProposed, "propuesta_vendedor",
                                              ifelse(chosenPrice == buyerTop,       "maximo_comprador",
                                                     ifelse(chosenPrice == buyerProposed,  "propuesta_comprador", "otro"))))))
print(table(duo_clean$opcion))

# 7a. Todas las rondas con acuerdo
print(binom.test(sum(duo_clean$opcion == "promedio"), nrow(duo_clean), p = 0.2))