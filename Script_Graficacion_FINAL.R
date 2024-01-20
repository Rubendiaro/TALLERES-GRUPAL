## ---- warning = FALSE-----------------------------------------------------------------------------------------------------
source("Script_Modelos.R")


## -------------------------------------------------------------------------------------------------------------------------
library(pacman)
library(jtools)
p_load(margins) #Añadimos la libería Margins y los hacemos de nuestros modelos.
margins_m1 <- margins(modelo1)
margins_m2 <- margins(modelo2)
margins_m3 <- margins(modelo3)


## -------------------------------------------------------------------------------------------------------------------------
p_load(jtools) #Con jtools ya podemos graficar nuestros marginales.
plot_summs(margins_m2, style = "dotplot", scale = TRUE, ci = TRUE, model.names = c("Modelo 2"), colors = c("blue"), #Definimos el tipo de gráfico
coefs = c("Diversidad cultural" = "cult_div", 
"Existencia de democracia: No." = "demoNo Democrático", 
"Derechos políticos" = "derechos",
"Estabilidad política: Inestable" = "estabilidadInestable")) + ggplot2::theme_minimal()+ #Ajustamos etiquetas
ggplot2::labs(title = "Modelo Marginal 1") + 
  ggplot2::ylab("Categorías") +
  ggplot2::xlab("Marginal estimado")


## -------------------------------------------------------------------------------------------------------------------------
MMM <- plot_summs(margins_m3, style = "dotplot", scale = TRUE, ci = TRUE, model.names = c("Modelo 3"), colors = c("blue"),
coefs = c("Coeficiente de Gini" = "cgini",
  "Diversidad cultural" = "cult_div", 
"Existencia de democracia: No." = "demoNo Democrático", 
"Derechos políticos" = "derechos",
"Estabilidad política: Inestable" = "estabilidadInestable",
"IDH: Bajo" = "idhBajo",
"Calidad de la logística" = "logistics_quality",
"% del PIB que representa el comercio" = "trade_gdp_percent"))  + ggplot2::theme_minimal()+
ggplot2::labs(title = "Modelo Marginal 3") + 
  ggplot2::ylab("Categorías") +
  ggplot2::xlab("Marginal estimado")

print(MMM)


## -------------------------------------------------------------------------------------------------------------------------
p_load(summarytools)

descriptivos <- descr(df_models, stats = c("mean", "med", "sd", "max", "min"))

t(descriptivos)

