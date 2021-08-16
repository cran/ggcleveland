## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", warning = FALSE, message = FALSE,
  # out.width = "100%",
  fig.width = 6, fig.height = 4,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
# Paquetes necesarios y otras configuraciones
library(ggcleveland)
library(ggplot2)
library(dplyr)
theme_set(theme_bw() + theme(panel.spacing = unit(0, "lines")))

## -----------------------------------------------------------------------------
data(futbol)

# Quedarse con solo dos grupos
futbol2 <-   
  futbol %>% 
  filter(longp %in% c("< 0.81 m", "0.81 a 0.90 m"))

# Gráfico de cuantiles para dos grupos
gg_quantiles(futbol2, dist, longp)

# Más de dos grupos
gg_quantiles(futbol, dist, longp, size = 0.4, color = "red", shape = 3) +
	labs(title = "Gráficos QQ de a pares", x = "Distancia (m)", y = "Distancia (m)")

## -----------------------------------------------------------------------------
futbol <- 
  futbol %>% 
  group_by(longp) %>% 
  mutate(ajuste = mean(dist), res = dist - ajuste)

gg_quantiles(futbol, res, longp, combined = TRUE) 

## -----------------------------------------------------------------------------
# Dos grupos
gg_tmd(futbol2, dist, longp)

# Múltiples grupos
gg_tmd(futbol, dist, longp, size = 0.5)

## -----------------------------------------------------------------------------
data(ozone)
gg_tmd_paired(ozone, stamford, yonkers)

## -----------------------------------------------------------------------------
gg_rf(futbol, dist, ajuste, res, ylabel = "Distancia (m)")

# Agregando las observaciones centradas por la media general
gg_rf(futbol, dist, ajuste, res, cen_obs = TRUE, ylabel = "Distancia (m)")

## -----------------------------------------------------------------------------
gg_sl(futbol2, dist, longp, xlabel = "Mediana de distancia jittered (m)", 
      jitterwidth = 1.5) +
  xlim(45, 68)

## ---- fig.height=2.5, fig.width=6.5-------------------------------------------
gg_pt(futbol2, dist, taus = c(-1, -0.5, 0, 0.5), nrow = 1)

## ---- fig.height=3.5, fig.width=6.5-------------------------------------------
# Para cada grupo por separado
gg_pt(futbol2, dist, longp, taus = c(-1, -0.5, 0, 0.5))

## -----------------------------------------------------------------------------
data(rubber)
# Slicing con intervalos solapados
gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
  number_bins = 6, overlap = 3/4,
  ylabel = "Pérdida de abrasión (g/hp-hour))",
  xlabel = "Resistencia a la tracción (kg/cm2)",
  facet_label = "Dureza (grados Shore)", 
  loess_family = "symmetric", size = 2)

# Slicing con intervalos sin solapamientos, con igual amplitud
gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
  number_bins = 6, overlap = 0,
  ylabel = "Pérdida de abrasión (g/hp-hour))",
  xlabel = "Resistencia a la tracción (kg/cm2)",
  facet_label = "Dureza (grados Shore)", 
  loess = FALSE, size = 2)

# Slicing con intervalos sin solapamientos, con aprox. igual cantidad de datos
gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
  number_bins = 6, overlap = 0, equal_length = F,
  ylabel = "Pérdida de abrasión (g/hp-hour))",
  xlabel = "Resistencia a la tracción (kg/cm2)",
  facet_label = "Dureza (grados Shore)", 
  loess = FALSE, size = 2)

## -----------------------------------------------------------------------------
data(galaxy)

# Slicing con los valores únicos de la variable de faceting
gg_coplot(galaxy, x = posicion.radial, y = velocidad,
  faceting = angulo, number_bins = 7, loess_span = .5, loess_degree = 2,
  facet_labeller = function(x) paste0("Ángulo = ", x, "º"),
  facet_label = "Ángulo (grado)", facets_nrow = 2, intervals_height = 0.2,
  xlabel = "Posición radial (arcsec)", ylabel = "Velocidad (km/s)")

