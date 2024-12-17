library(ggplot2)
library(cowplot)
library(sitools)

W=4.804
H=2
S=1
point_size=0.3
line_size=1
linecolors=scale_color_brewer(palette="Set1")
theme = theme_cowplot(font_size=7)

sisec=function(t) {
    t[is.na(t)] = 0
    sitools::f2si(t, 's')
}

args = commandArgs(trailingOnly=TRUE)

{
    args <- c(100, 5, 6)  # Asignar valores de prueba para los argumentos
    args <- as.integer(args)  # Convertir los argumentos a enteros
    print(args)  # Verifica que los valores sean correctos

    # Continuar con el cálculo de n
    n <- args[1] * args[2] * args[3]
    if (is.na(n) || n == 0) stop("Error: 'n' es cero o inválido")
    print(n)  # Verifica el valor de n
    tc=function(t)t/n

    data = read.csv(paste0('matmul_custom.csv'), header=T, sep=',')

    data = aggregate(data[, 4:6], list(s0=data$s0, s1=data$s1, s2=data$s2), mean)

    data[, "s2"] = paste0("Out matrix layout: ", data[, "s2"])
    
    ggsave(paste0('plots/heatmap_custom', '', args[1], '', args[2], '_', args[3], '.pdf'), device='pdf', units="in", scale=S, width=W*2, height=H*3,  # Aumenta el tamaño de la hoja
    ggplot(data, aes(x=s1, y=s0, fill=tc(action))) +
    geom_tile() +
    geom_text(aes(label=format(round((tc(action))*1e15, 0), nsmall = 0)), size=1.5) +  # Reduce el tamaño de la letra
    labs(x="Right-hand side matrix layout", y="Left-hand side matrix layout") +
    facet_wrap(~s2, ncol=3) + 
    scale_fill_gradientn(colors=colorspace::diverging_hcl(100, "Blue-Red", l1=66.6, l2=96), name="Normalized time", labels = sisec) +
    theme + background_grid() + theme(legend.position="right") +
    theme(axis.text = element_text(size=6),  # Reduce el tamaño de las etiquetas de los ejes
          strip.text = element_text(size=8),  # Reduce el tamaño de las etiquetas de las facetas
          legend.title = element_text(size=8),
          legend.text = element_text(size=6)
    )
)


}