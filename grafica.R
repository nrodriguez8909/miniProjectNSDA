# # Libraries
library(data.table)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

# Sources
source("multiplot.R")

# # Functions
degrePlot <- function(data, namPlot, varName){
	ggplot(data, aes(x=eval(parse(text=varName)), y=N, color = "red")) + 
	    geom_point(size = 4) +
	    xlab("Number of Partners") + ylab("Number of Countries") +
	    theme_minimal() +
	    theme(legend.position = "None", 
	    	  axis.title.x = element_text(size=20),
	    	  axis.text.x = element_text(size=15),
	    	  axis.title.y = element_text(size=20),
	    	  axis.text.y = element_text(size=15))
	ggsave(file.path(outPath, paste0(namPlot, ".png")), width = 10, height = 8)
}

degrePlotWgts <- function(data, namPlot, varName){
	ggplot(data, aes(x=eval(parse(text=varName)))) + 
	    geom_histogram(binwidth = 0.1) +
	    	    theme_minimal() +
	    	    theme(legend.position = "None", 
	    	    	  axis.title.x = element_blank(),
	    	    	  axis.text.x = element_text(size=15),
	    	    	  axis.title.y = element_blank(),
	    	    	  axis.text.y = element_text(size=15))
	ggsave(file.path(outPath, paste0(namPlot, ".png")), width = 10, height = 8)
}
# # Paths
outPath <- "graficos"

# Load node data
db <- fread("nodos.csv")
setnames(db, names(db), gsub(" ", "_", names(db)))

# Degree distribution
degrePlot(db[,.N, by = degree], "dgreDistrNoWeights", "degree")
# In-Degree distribution
degrePlot(db[,.N, by = indegree], "inDgreDistrNoWeights", "indegree")
# Out-Degree distribution
degrePlot(db[,.N, by = outdegree], "outDgreDistrNoWeights", "outdegree")

# Degree distribution with Weights
degrePlotWgts(db, "dgreDistrWeights", "weighted_degree")
# Indegree distribution with Weights
degrePlotWgts(db, "InDgreDistrWeights", "weighted_indegree")
# Outdegree distribution with Weights
degrePlotWgts(db, "outDgreDistrWeights", "weighted_outdegree")



# regPlot <- function(data, namPlot, varY, varX, posX, posY, mainTitle = NULL, xLabel = NULL, save = TRUE){
# 	form <- as.formula(paste0(varY, "~", varX))
# 	mod1 <- lm(form, data)
# 	b0 <- round(coef(mod1)[1], 5)
# 	b1 <- round(coef(mod1)[2], 5)
# 	r2 <- round(summary(mod1)$r.squared, 5)
# 	graph <- ggplot(data, aes(x=eval(parse(text=varX)), y=eval(parse(text=varY)), color = "red")) +
# 	  geom_point(size = 4) +
# 	  geom_smooth(method=lm , color="blue", se=TRUE) +
# 	  geom_text(x=posX, y=posY, label = paste0("y = ", b0,"+",b1,"X", "\n", "R^2 = ", r2), size = 8, color="blue") +
# 	  ggtitle(mainTitle) +
# 	  theme_minimal() +
# 	  ylab(xLabel) +
# 	  theme(legend.position = "None",
# 	  		plot.title = if(!is.null(mainTitle)){element_text(hjust = 0.5, size = 30)} else {element_blank()},
# 	  	    axis.title.y = if(!is.null(xLabel)) {element_text(size = 30)} else {element_blank()},
# 	  	    axis.text.y = element_text(size=15),
# 	  	    axis.title.x = element_blank(),
# 	  	    axis.text.x = element_text(size=15))
# 	if (save) {
# 		ggsave(file.path(outPath, paste0(namPlot, ".png")), graph, width = 10, height = 8)
# 	}
# 	return(graph)
# }

# # # Opcion 1
# p1 <- regPlot(db, "betCenAllNodes", "betweenesscentrality", "degree", 20, 0.008, "All Nodes", "Normalized Betweenness Centrality", TRUE)
# p2 <- regPlot(db, "authAllNodes", "Authority", "degree", 19, 0.35, NULL, "Authority", TRUE)
dropDB1 <- db[betweenesscentrality > 0.000001 & Label != "China"]
# p3 <- regPlot(dropDB1, "betCenDropChinaAndZero", "betweenesscentrality", "degree", 15, 0.01, "Drop China & Zeros", NULL, TRUE)
dropDB2 <- db[Authority > 0.000001 & Label != "Russia"]
# p4 <- regPlot(dropDB1, "authDropChinaAndZero", "Authority", "degree", 20, 0.3, NULL, NULL, TRUE)

# multiplot(p1, p2, p3, p4, cols=2)


# # Opcion 2
db1 <- data.table(var = "betweenesscentrality", pop = "All Nodes", measure = db[, betweenesscentrality], degree = db[,degree])
db2 <- data.table(var = "Authority", pop = "All Nodes", measure = db[, Authority], degree = db[,degree])
db3 <- data.table(var = "betweenesscentrality", pop = "Drop Zero & Biggest Country*", measure = dropDB1[, betweenesscentrality], degree = dropDB1[,degree])
db4 <- data.table(var = "Authority", pop = "Drop Zero & Biggest Country*", measure = dropDB2[, Authority], degree = dropDB2[,degree])
dbFull <- rbind(db1, db2, db3, db4)
dbFull[, var := factor(var, c("betweenesscentrality", "Authority"), c("Normalized Betweenness Centrality", "Authority"), ordered = TRUE)]
dbFull[, b0:=round(coef(lm(measure ~ degree))[1], 5), by = .(var, pop)]
dbFull[, b1:=round(coef(lm(measure ~ degree))[2], 5), by = .(var, pop)]
dbFull[, r2:=round(summary(lm(measure ~ degree))$r.squared, 5), by = .(var, pop)]
labDB <- unique(dbFull[,.(var, pop, b0, b1, r2)])
# labDB[,label := sprintf("italic(y) == %.5f %+.5f * italic(x) \n italic(R^2) == %.5f", b0, b1, r2)]
labDB[,label := paste0("y = ", b0,"+",b1,"x", "\n", "R^2 = ", r2)]
ggplot(dbFull, aes(x=degree, y=measure, color = "#CA0020")) +
	  geom_point(size = 3, alpha = 0.7) +
	  facet_grid(var ~ pop, scales = "free_y", switch = "y") +
	  geom_smooth(method=lm , color="#0571B0", se=TRUE) +
	  geom_text(data = labDB, aes(label = label), x = 23, y = 0.01, parse = FALSE, size = 4.5, color="#0571B0") +
	  theme_bw() +
	  theme(legend.position = "None",
	  		axis.title.x = element_blank(),
	  		strip.text.x = element_text(size=16),
	  		axis.text.x = element_text(size=12),
	  		axis.title.y = element_blank(),
	  		strip.text.y = element_text(size=16),
	  		axis.text.y = element_text(size=12))
ggsave(file.path(outPath, "roleNode.png"), width = 10, height = 8)