# To do: search for 'xxx'
# xxx Include data points for localities CHECK
# xxx Include S and W for lat long in panels
# xxx Check how to make land in panels gray 

	# Open libraries

	library(maps)
	library(readxl)
	library(visreg)
	library(nlme)
	library(rnaturalearth)
	library(sp)
	library(plotrix) #new
	

	
	# World map 1:10
	
	countries10 <- rnaturalearth::ne_download(scale = 10,
	                                          type = 'countries',
	                                          category = 'cultural')
	
	# Chile
	
	cl <- countries10[countries10$SOVEREIGNT == 'Chile',]	
	cl$category <- "C"
	

	# Opening dataset

	
	fidd <- read_excel("Base_final_location.xlsx")
	fidd$size <- ifelse(fidd$size == "Big", "Large", fidd$size)
	fidd$size <- factor(fidd$size,levels=c('Small', 'Medium', 'Large'))
	fidd$zone <- factor(fidd$zone,levels=c('South', 'North'))	
	head(fidd)


	loc <- read_excel("localidades_fid.xlsx")
	o <- loc[which(loc$access=="OA"),];o
	a <- loc[which(loc$access=="AMERB"),];a
	r <- loc[which(loc$access=="RESERVA"),];r

# -------------------------------- Figure 1 --------------------------------
		
		quartz("Fig_1", 10,8)
   	 	layout(matrix(c(2,2,2,3,3,3,1,1,1,1,1,1,4,4,5,5,6,6),6,3),widths=c(1,1,2))
    	par(mar=c(1,0,1,2), oma = c(4,1,4,0))

	# Map of Chile 

		plot(1,1,xlim=c(-77,-63),ylim=c(-55,-18),  bty="n", bg = "white",xaxt="n",yaxt="n")
		map("world",regions=c("chile"),
    	    add = TRUE,xlim=c(-80,-35),ylim=c(-55,10), boundary = "gray", interior = T,fill = TRUE,col = rgb(0,0,0,0.05),lwd=0.7)
		
		polygon(c(-72,-72,-69,-69),c(-20,-25,-25,-20))
		polygon(c(-74,-74,-71,-71),c(-30,-35,-35,-30))		

		points(c(-67,-67,-67),c(-42,-44,-46),pch=c(21,22,23),cex=1.5,bg=c("black","gray","white"))
		text(c(-66,-66,-66),c(-42,-44,-46),c("Tarapaca","n=10","n=4"),adj=c(0,0.5),cex=1.2)

	# Subset Northern Chile
		
		par(mar=c(2,3.5,1,2))
		sp::plot(cl,xlim=c(-71,-69),ylim=c(-22,-20),axes = T, bg = "white",xaxt="n",yaxt="n",col=rgb(0,0,0,0.05)) # extra   col=rgb(0,0,0,0.05)
		
		points(cluster.overplot(o$x,o$y),pch=c(23),cex=1.5,bg=c("white")) # overplot
		
		points(cluster.overplot(a$x,a$y),pch=c(22),cex=1.5,bg=c("grey")) # overplot
		
		points(r$x,r$y,pch=c(21),cex=1.5,bg=c("black"))  
		axis(1,at=c(-71,-69),labels=c("71º W","69º W"))
		axis(2,las=1,at=c(-20,-21,-22),labels=c("20º S","21º S","22º S")) #extra at=c(-20,-22,-24),labels=c("20º S","22º S","24º S")
	
			# Segments	
			par(xpd=NA)
				segments(-68.85,-19.8,-66.8,-20.8)
				segments(-68.85,-25.2,-66.8,-22.3)
			par(xpd=FALSE)



	# Subset Central Chile
	
		sp::plot(cl,xlim=c(-73,-71),ylim=c(-34,-32),axes = T, bg = "white",xaxt="n",yaxt="n",col=rgb(0,0,0,0.05))

		points(cluster.overplot(o$x,o$y),pch=c(23),cex=1.5,bg=c("white")) # overplot
		
		points(cluster.overplot(a$x,a$y),pch=c(22),cex=1.5,bg=c("grey")) # overplot
		
		points(r$x,r$y,pch=c(21),cex=1.5,bg=c("black"))
		
		axis(1,at=c(-73,-71), labels=c("73º W","71º W"))
		axis(2,las=1,at=c(-32,-33,-34),labels=c("32º S","33º S", "34º S"))

			# Segments
			par(xpd=NA)
				segments(-71.7,-31.8,-69.05,-27.67)
				segments(-71.7,-34.2,-69.05,-29.15)
			par(xpd=FALSE)
		
	# Descriptive plots of species, size, protection and zone distribution

		par(mar=c(4,7,2,3))	

		barplot(table(fidd$species,fidd$size), main = "", xlab = "Size", ylab = "Freq", 
       		col=c("grey54","grey79","grey94"), cex.lab=1.3, cex.axis=1.1, las=1)

		barplot(table(fidd$species,fidd$protection), main = "", xlab = "Protection", ylab = "Freq", 
       		col=c("grey54","grey79","grey94"), cex.lab=1.3, cex.axis=1.1, las=1)

		par(mar=c(4,7,2,12.5),xpd=NA)			
		barplot(table(fidd$species,fidd$zone), main = "", xlab = "Zone", ylab = "Freq", 
       		col=c("grey54","grey79","grey94"), cex.lab=1.3, cex.axis=1.1, las=1)
		legend(x = 3, y = 1200,   		    
			legend = c("A. punctatus","C. variegatus","P. chilensis"), # Textos de la leyenda
    	 	fill = c("grey54","grey79","grey94"),text.font=3)                # Ancho de las líneas
		par(xpd=FALSE)	
		
     #New figure size x sp
     
     # Filter the data for the category "North".
        north_data <- fidd[fidd$zone == "North", ]

     # Calcular la tabla de frecuencias para "North"
        table_north <- table(north_data$species, north_data$size)

     # Crear una nueva ventana gráfica para la categoría "North"
        quartz()
        barplot(table_north, main = "North Zone", xlab = "Size", ylab = "Freq", 
          col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1)

     # Filtrar los datos para la categoría "South"
        south_data <- fidd[fidd$zone == "South", ]

     # Calcular la tabla de frecuencias para "South"
        table_south <- table(south_data$species, south_data$size)

     # Crear una nueva ventana gráfica para la categoría "South"
        quartz()
        barplot(table_south, main = "South Zone", xlab = "Size", ylab = "Freq", 
            col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1)
            
    #Figura nueva protection x sp
     
    # Filtrar los datos para la categoría "North"
            north_data_p <- fidd[fidd$zone == "North", ]

    # Calcular la tabla de frecuencias para "North"
            table_north_p <- table(north_data_p$species, north_data_p$protection)

    # Crear una nueva ventana gráfica para la categoría "North"
        quartz()
        barplot(table_north_p, main = "North Zone", xlab = "Protection", ylab = "Freq", 
            col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1,
            names.arg = c("TURF", "Open access"))
     # Filtrar los datos para la categoría "South"
south_data_p <- fidd[fidd$zone == "South", ]

# Calcular la tabla de frecuencias para "South"
table_south_p <- table(south_data_p$species, south_data_p$protection)

# Crear una nueva ventana gráfica
quartz()

# Crear el gráfico de barras
barplot(table_south_p, main = "South Zone", xlab = "Size", ylab = "Freq", 
        col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1,
        names.arg = c("MPA", "TURF", "Open access"))

##Other options

# Filtrar los datos para la categoría "South"
south_data_p <- fidd[fidd$zone == "South", ]

# Calcular la tabla de frecuencias para "South"
table_south_p <- table(south_data_p$species, south_data_p$protection)

# Crear una nueva ventana gráfica
quartz()

# Establecer los márgenes para dar espacio en la parte inferior
par(mar = c(5, 4, 2, 2) + 0.1)

# Crear el gráfico de barras en la parte superior
barplot(table_south_p, main = "South Zone", xlab = "Size", ylab = "Freq", 
        col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1,
        names.arg = c("MPA", "TURF", "Open access"))

# Crear la leyenda horizontal en la parte inferior
legend("bottom", legend = c("A. punctatus", "C. variegatus", "P. chilensis"),
       fill = c("grey54", "grey79", "grey94"), text.font = 3, horiz = TRUE)

# Restaurar la configuración original de los márgenes
par(mar = c(5, 4, 4, 2) + 0.1)

######

# Figura nueva size x sp

# Filtrar los datos para la categoría "North"
north_data <- fidd[fidd$zone == "North", ]

# Calcular la tabla de frecuencias para "North"
table_north <- table(north_data$species, north_data$size)

# Calcular observaciones por categoría de tamaño
size_counts <- table(north_data$size)

# Imprimir el conteo de observaciones por categoría de tamaño en la consola
cat("Observaciones por categoría de tamaño:\n")
print(size_counts)

# Crear una nueva ventana gráfica para la categoría "North"
quartz()
barplot(table_north, main = "North Zone", xlab = "Size", ylab = "Freq", 
        col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1)
#####

# Filtrar los datos para la categoría "South"
south_data <- fidd[fidd$zone == "South", ]

# Calcular la tabla de frecuencias para "South"
table_south <- table(south_data$species, south_data$size)

# Calcular observaciones por categoría de tamaño
size_counts_south <- table(south_data$size)

# Imprimir el conteo de observaciones por categoría de tamaño en la consola
cat("Observaciones por categoría de tamaño (South Zone):\n")
print(size_counts_south)

# Crear una nueva ventana gráfica para la categoría "South"
quartz()
barplot(table_south, main = "South Zone", xlab = "Size", ylab = "Freq", 
        col = c("grey54", "grey79", "grey94"), cex.lab = 1.3, cex.axis = 1.1, las = 1)






 -------------------------------- Figure 2 --------------------------------
	
		
	# Partial plots
	# Including only main effects (interactions are plotted in Fig 3)
			
		m1 <- lm(fid ~ size + zone + species + protection, data=fidd)	
		
			res1 <- visreg(m1, "species", type="conditional",plot=F)$res
			res2 <- visreg(m1, "size",type="conditional",plot=F)$res
			res3 <- visreg(m1, "protection", type="conditional",plot=F)$res
			res4 <- visreg(m1, "zone", type="conditional",plot=F)$res	

		
		quartz(,9,8)
		par(mfrow=c(2,2),mar=c(4,4.5,1,1))
		boxplot(res1$visregRes ~ res1$species,col = c("grey54","grey79","grey94"), xlab="Species", ylab="FID (cm)",las=1,outpch=21, outbg="gray",cex=1.3,cex.lab=1.3,ylim=c(0,430),xaxt="n")	
		axis(1,at=1:3,labels=c("A. punctatus","C. variegatus","P. chilensis"),font=3)
		boxplot(res2$visregRes ~ res2$size,col = "white", xlab="Size", ylab="FID (cm)",las=1,outpch=21, outbg="gray",cex=1.3,cex.lab=1.3,ylim=c(0,430))
		boxplot(res3$visregRes ~ res3$protection,col = "white", xlab="Protection", ylab="FID (cm)",las=1,outpch=21, outbg="gray",cex=1.3,cex.lab=1.3,ylim=c(0,430),xaxt="n")
		axis(1,at=1:3,labels=c("MPA","TURF","Open access"),font=1)
		boxplot(res4$visregRes ~ res4$zone,col = "white", xlab="Zone", ylab="FID (cm)", las=1,outpch=21, outbg="gray",cex=1.3,cex.lab=1.3,ylim=c(0,430))			
			
			
			
# -------------------------------- Figure 3 --------------------------------
	
			
	# Partial plots including strongest interactions


		m2 <- lm(fid ~ size * zone * species + protection, data=fidd)	
		res1 <- visreg(m2, "species", by="zone",type="conditional",plot=F)$res
		res2 <- visreg(m2, "size", by="zone",type="conditional",plot=F)$res


		quartz(,7,8)
		par(mfrow=c(2,1),mar=c(5,4.5,1,1))
		boxplot(res1$visregRes ~ res1$zone*res1$species,col = c("grey54","grey54","grey79","grey79","grey94","grey94"), xlab="", ylab="FID (cm)",las=1,
			xaxt="n",outpch=21, outbg="gray",cex=1.3,cex.lab=1.2,ylim=c(0,440))	
			axis(1,at=1:6,labels=c("South","North","South","North","South","North"))
			mtext("A. punctatus",side=1,line=3,adj=0.16,cex=1.1,font=3)
			mtext("C. variegatus",side=1,line=3,adj=0.5,cex=1.1,font=3)			
			mtext("P. chilensis",side=1,line=3,adj=0.84,cex=1.1,font=3)			

		boxplot(res2$visregRes ~ res2$zone*res2$size,col = "white", xlab="", ylab="FID (cm)",las=1,
			xaxt="n",outpch=21, outbg="gray",cex=1.3,cex.lab=1.2,ylim=c(0,440))	
			axis(1,at=1:6,labels=c("South","North","South","North","South","North"))
			mtext("Small",side=1,line=3,adj=0.16,cex=1.1)
			mtext("Medium",side=1,line=3,adj=0.5,cex=1.1)			
			mtext("Large",side=1,line=3,adj=0.84,cex=1.1)			

			
# -------------------------------- Mixed model --------------------------------
	
	          
	# Mixed model 
	
	mod1<- lm(fid~(size+zone+species+protection)^2, data=fidd)
	summary(mod1)
	anova(mod1)
	
	mod2<- aov(fid~zone*species*protection*size, data=fidd)
    summary(mod2)


		              
                    
                    
                    
