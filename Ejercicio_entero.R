install.packages(c("affy", "stats", "VennDiagram", "enrichplot"))
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("arrayQualityMetrics", "limma", "annotate", "mouse4302.db", 
                       "clusterProfiler", "org.Mm.eg.db", "DOSE"))
if (!requireNamespace("mouse4302.db", quietly = TRUE)) {
  BiocManager::install("mouse4302.db")
}



filter_microarray <- function(allTargets, seed = 77129830) {
  # Configurar la semilla aleatoria
  set.seed(77129830)
  
  # Filtrar las filas donde 'time' no sea 'hour 2'
  filtered <- subset(allTargets, time != "hour 2")
  
  # Dividir el dataset por grupos únicos de 'infection' + 'agent'
  filtered$group <- interaction(filtered$infection, filtered$agent)
  
  # Seleccionar 4 muestras al azar de cada grupo
  selected <- do.call(rbind, lapply(split(filtered, filtered$group), function(group_data) {
    if (nrow(group_data) > 4) {
      group_data[sample(1:nrow(group_data), 4), ]
    } else {
      group_data
    }
  }))
  
  # Obtener los índices originales como nombres de las filas seleccionadas
  original_indices <- match(selected$sample, allTargets$sample)
  
  # Modificar los rownames usando 'sample' y los índices originales
  rownames(selected) <- paste0(selected$sample, ".", original_indices)
  
  # Eliminar la columna 'group' y devolver el resultado
  selected$group <- NULL
  return(selected)
}



# Simular el dataset basado en la descripción proporcionada
allTargets <- data.frame(
  sample = c("GSM944831", "GSM944838", "GSM944845", "GSM944852", "GSM944859",
             "GSM944833", "GSM944840", "GSM944847", "GSM944854", "GSM944861",
             "GSM944834", "GSM944841", "GSM944848", "GSM944855", "GSM944862",
             "GSM944832", "GSM944839", "GSM944846", "GSM944853", "GSM944860",
             "GSM944835", "GSM944842", "GSM944849", "GSM944856", "GSM944863",
             "GSM944836", "GSM944843", "GSM944850", "GSM944857", "GSM944864",
             "GSM944837", "GSM944844", "GSM944851", "GSM944858", "GSM944865"),
  infection = c(rep("uninfected", 15), rep("S. aureus USA300", 20)),
  time = c(rep("hour 0", 15), rep("hour 2", 5), rep("hour 24", 15)),
  agent = c(rep("untreated", 5), rep("linezolid", 5), rep("vancomycin", 5),
            rep("untreated", 5), rep("untreated", 5), rep("linezolid", 5), rep("vancomycin", 5))
)

# Aplicar la función (cambiar 123 por vuestro ID de la UOC u otro número que podáis escribir en el documento)
result <- filter_microarray(allTargets, seed=77129830)


print(result)





# Obtenemos los nombres de los archivos en la carpeta GSE38531_RAW
cel_files_in_directory <- list.files(path = "GSE38531_RAW", pattern = "\\.CEL$", full.names = TRUE)

# Verificamos los nombres en 'result' y mapearlos con los archivos reales
sample_names <- gsub("\\..*", "", rownames(result)) 
matched_files <- sapply(sample_names, function(sample) {
  # Buscamos en los archivos el que contiene el nombre de la muestra
  file <- grep(sample, cel_files_in_directory, value = TRUE)
  if (length(file) == 1) {
    return(file)
  } else {
    return(NA)  # Si no se encuentra = NA
  }
})

# Comprobamos si faltan archivos
missing <- which(is.na(matched_files))
if (length(missing) > 0) {
  cat("Faltan archivos para las siguientes muestras:\n")
  print(sample_names[missing])
} else {
  cat("Todos los archivos han sido mapeados correctamente.\n")
}



library(affy)

# Leemos los datos crudos desde los archivos mapeados
raw_data <- ReadAffy(filenames = matched_files)

# Revisamos los datos
summary(raw_data)

# Normalizamos los datos
normalized_data <- rma(raw_data)

# Agregamos información de las muestras desde 'result'
pData(normalized_data) <- result

# Verificamos ExpressionSet
print(normalized_data)



# Boxplot para verificar distribuciones

short_labels <- gsub("_.*", "", colnames(exprs(normalized_data)))
boxplot(exprs(normalized_data),
        main = "Distribución de valores de expresión normalizados",
        xlab = "Muestras", ylab = "Intensidad",
        names = short_labels, las = 2, col = "lightblue", cex.axis = 0.8)


# PCA (Análisis de Componentes Principales) para observar agrupamiento
library(stats)
pca <- prcomp(t(exprs(normalized_data)), scale. = TRUE)
plot(pca$x[, 1:2], col = as.factor(result$infection),
     pch = 19, main = "PCA de las muestras (Componentes 1 y 2)",
     xlab = "PC1", ylab = "PC2")
legend("topright", legend = unique(result$infection),
       col = 1:length(unique(result$infection)), pch = 19)




library(arrayQualityMetrics)

# Generar reporte de calidad
arrayQualityMetrics(expressionset = normalized_data,
                    outdir = "QualityControlReport",
                    force = TRUE)


variability <- apply(exprs(normalized_data), 1, sd)
summary(variability)


threshold <- quantile(variability, 0.9)
threshold


# Filtramoslas sondas que superen el umbral de variabilidad
filtered_indices <- which(variability > threshold)
filtered_data <- normalized_data[filtered_indices, ]

# Verificamos las dimensiones 
dim(filtered_data)


library(limma)

# Creamos una nueva variable que combine infección y agente
pData(filtered_data)$group <- interaction(pData(filtered_data)$infection, 
                                          pData(filtered_data)$agent, sep = "_")

# Verificamos los grupos únicos
unique(pData(filtered_data)$group)

# Creamos la matriz de diseño
design <- model.matrix(~ 0 + group, data = pData(filtered_data))

# Renombramos las columnas para que sean más legibles
colnames(design) <- levels(pData(filtered_data)$group)

# Verificamos la matriz
design


# Corregimos los nombres de las columnas de la matriz de diseño
colnames(design) <- make.names(colnames(design))
colnames(design)

# Creamos la matriz de contrastes con los nombres exactos de las columnas de diseño
contrast_matrix <- makeContrasts(
  Infectados_vs_NoInfectados = S..aureus.USA300_untreated - uninfected_untreated,
  Linezolid = S..aureus.USA300_linezolid - uninfected_linezolid,
  Vancomicina = S..aureus.USA300_vancomycin - uninfected_vancomycin,
  levels = design
)

# Verificamos
contrast_matrix


fit <- lmFit(exprs(filtered_data), design)

# Aplicamos los contrastes
fit_contrasts <- contrasts.fit(fit, contrast_matrix)

# Estimación Bayesiana
fit_contrasts <- eBayes(fit_contrasts)
summary(decideTests(fit_contrasts))


# Obtener las listas de genes diferencialmente expresados
topTable_Inf_vs_NoInf <- topTable(fit_contrasts, coef = "Infectados_vs_NoInfectados", number = Inf, adjust = "BH")
topTable_Linezolid <- topTable(fit_contrasts, coef = "Linezolid", number = Inf, adjust = "BH")
topTable_Vancomicina <- topTable(fit_contrasts, coef = "Vancomicina", number = Inf, adjust = "BH")

# Visualizar los primeros genes de cada lista
head(topTable_Inf_vs_NoInf)
head(topTable_Linezolid)
head(topTable_Vancomicina)



# Filtro de genes diferencialmente expresados (p-valor ajustado < 0.05)
DEGs_Inf_vs_NoInf <- topTable_Inf_vs_NoInf[topTable_Inf_vs_NoInf$adj.P.Val < 0.05, ]
DEGs_Linezolid <- topTable_Linezolid[topTable_Linezolid$adj.P.Val < 0.05, ]
DEGs_Vancomicina <- topTable_Vancomicina[topTable_Vancomicina$adj.P.Val < 0.05, ]

# Verificar el número de genes significativos
nrow(DEGs_Inf_vs_NoInf)
nrow(DEGs_Linezolid)
nrow(DEGs_Vancomicina)


# Instalar paquete VennDiagram si no está instalado
if (!requireNamespace("VennDiagram", quietly = TRUE)) install.packages("VennDiagram")
library(VennDiagram)

# Extraer los IDs de genes de cada lista
genes_Inf_vs_NoInf <- rownames(DEGs_Inf_vs_NoInf)
genes_Linezolid <- rownames(DEGs_Linezolid)
genes_Vancomicina <- rownames(DEGs_Vancomicina)

# Crear el diagrama de Venn
venn.plot <- venn.diagram(
  x = list(Infectados_vs_NoInfectados = genes_Inf_vs_NoInf,
           Linezolid = genes_Linezolid,
           Vancomicina = genes_Vancomicina),
  filename = NULL,
  fill = c("red", "blue", "green"),
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.2,
  main = "Diagrama de Venn de genes diferencialmente expresados"
)

# Mostrar el diagrama de Venn
grid.draw(venn.plot)


comparison <- decideTests(fit_contrasts)
summary(comparison)

# Visualizamos la comparación como un heatmap

heatmap(as.matrix(comparison),
        main = "Comparación de genes diferencialmente expresados",
        cexRow = 0.6,    # Ajusta el tamaño de las etiquetas en el eje Y
        cexCol = 0.8,    # Ajusta el tamaño de las etiquetas en el eje X
        margins = c(10, 10))  # Ajusta los márgenes (X, Y)


# Instalar paquetes necesarios
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("annotate", "org.Mm.eg.db"))

# Cargar los paquetes
library(annotate)
library(org.Mm.eg.db)


probe_ids <- rownames(topTable_Inf_vs_NoInf)
topTable_Inf_vs_NoInf <- topTable(fit_contrasts, coef = "Infectados_vs_NoInfectados", number = Inf, adjust = "BH")


if (!requireNamespace("mouse4302.db", quietly = TRUE)) {
  BiocManager::install("mouse4302.db")
}
# Instalar el paquete de anotación para la plataforma Mouse430
BiocManager::install("mouse4302.db")
library(mouse4302.db)

# Mapeamos las IDs de las sondas a Symbol, EntrezID y EnsemblID
gene_symbols <- mapIds(mouse4302.db, keys = probe_ids, column = "SYMBOL", keytype = "PROBEID", multiVals = "first")
entrez_ids <- mapIds(mouse4302.db, keys = probe_ids, column = "ENTREZID", keytype = "PROBEID", multiVals = "first")
ensembl_ids <- mapIds(mouse4302.db, keys = probe_ids, column = "ENSEMBL", keytype = "PROBEID", multiVals = "first")

# Creamos una tabla con las anotaciones
annotated_genes <- data.frame(
  ProbeID = probe_ids,
  Symbol = gene_symbols,
  EntrezID = entrez_ids,
  EnsemblID = ensembl_ids,
  stringsAsFactors = FALSE
)

head(annotated_genes)


# Combinar las anotaciones con los resultados de Limma
annotated_results <- merge(annotated_genes, topTable_Inf_vs_NoInf, by.x = "ProbeID", by.y = "row.names")

# Mostrar las primeras filas del resultado combinado
head(annotated_results)


with(annotated_results, {
  plot(logFC, -log10(P.Value),
       pch = 20, main = "Volcano Plot",
       xlab = "log2 Fold Change", ylab = "-log10 P-value")
  abline(h = -log10(0.05), col = "red", lty = 2)  # Línea de p-value significativo
  abline(v = c(-1, 1), col = "blue", lty = 2)     # Líneas para logFC ±1
})



# Filtrado de  genes sobreexpresados
upregulated_genes <- annotated_results[annotated_results$logFC > 1 & annotated_results$adj.P.Val < 0.05, ]

# Filtrado de genes subexpresados
downregulated_genes <- annotated_results[annotated_results$logFC < -1 & annotated_results$adj.P.Val < 0.05, ]

# Número de genes sobre y subexpresados
cat("Genes sobreexpresados:", nrow(upregulated_genes), "\n")
cat("Genes subexpresados:", nrow(downregulated_genes), "\n")

head(upregulated_genes)
head(downregulated_genes)


# Instalar paquetes necesarios
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("clusterProfiler", "org.Mm.eg.db", "enrichplot", "DOSE"))

# Cargar paquetes
library(clusterProfiler)
library(org.Mm.eg.db)
library(enrichplot)
library(DOSE)


# Preparar el ranking de genes basado en logFC
gene_list <- annotated_results$logFC
names(gene_list) <- annotated_results$EntrezID
gene_list <- sort(gene_list, decreasing = TRUE)

# Realizar GSEA en GO
gsea_results <- gseGO(
  geneList = gene_list,
  OrgDb = org.Mm.eg.db,
  keyType = "ENTREZID",
  ont = "BP",  # Biología de procesos
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05
)

# Visualizar resultados GSEA
gseaplot2(gsea_results, geneSetID = 1, title = "GSEA: Término GO más significativo")



# Términos GO más significativos
head(as.data.frame(gsea_results))

# Genes del término más significativo
gsea_genes <- gsea_results@result$geneID[1]  # Genes del primer término
cat("Genes del término enriquecido:", gsea_genes, "\n")

# Dotplot para los principales términos GO enriquecidos
dotplot(gsea_results, showCategory = 10) + ggtitle("Términos GO enriquecidos (GSEA)")


# Extraemos los Entrez IDs de los genes significativos
upregulated_entrez <- upregulated_genes$EntrezID
downregulated_entrez <- downregulated_genes$EntrezID

# ORA para genes sobreexpresados en "Biological Process" (BP)
go_up_BP <- enrichGO(
  gene = upregulated_entrez,
  OrgDb = org.Mm.eg.db,
  keyType = "ENTREZID",
  ont = "BP",  # Ontología: Biological Process
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.05
)

# ORA para genes subexpresados en "Biological Process" (BP)
go_down_BP <- enrichGO(
  gene = downregulated_entrez,
  OrgDb = org.Mm.eg.db,
  keyType = "ENTREZID",
  ont = "BP",
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.05
)



# Términos GO enriquecidos para genes sobreexpresados
dotplot(go_up_BP, showCategory = 10) + ggtitle("ORA: Términos GO enriquecidos (Sobreexpresados - BP)")

# Términos GO enriquecidos para genes subexpresados
dotplot(go_down_BP, showCategory = 10) + ggtitle("ORA: Términos GO enriquecidos (Subexpresados - BP)")


# ORA para genes sobreexpresados en rutas KEGG
kegg_up <- enrichKEGG(
  gene = upregulated_entrez,
  organism = "mmu",  # Organismo: mmu = Mus musculus
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.05
)

# ORA para genes subexpresados en rutas KEGG
kegg_down <- enrichKEGG(
  gene = downregulated_entrez,
  organism = "mmu",
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.05
)

# Visualización de rutas KEGG
dotplot(kegg_up, showCategory = 10) + ggtitle("ORA: Rutas KEGG enriquecidas (Sobreexpresados)")
dotplot(kegg_down, showCategory = 10) + ggtitle("ORA: Rutas KEGG enriquecidas (Subexpresados)")

