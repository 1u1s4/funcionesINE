# Para óptimo funcionamiento usar datos categóricos y con su respectivo porcentaje

graficaPackedBubbles <- function(data)
{
	# Create data
	db <- subset(data.frame(data), y >3)
	db <- arrange(db, desc(y))
	colnames(db) <- c("Categoría", "Valor")

	# Generate the layout. This function return a dataframe with one line per bubble. 
	# It gives its center (x and y) and its radius, proportional to "Valor"
	packing <- circleProgressiveLayout(db$Valor, sizetype='area')
	packing

	# We can add these packing information to the initial data frame
	db <- cbind(db, packing)

	# The next step is to go from one center + a radius to the coordinates of a circle that
	# is drawn by a multitude of straight lines.
	dat.gg <- circleLayoutVertices(packing, npoints=1000)
	dat.gg

	# Make the plot
	graph <- ggplot() + 
  
  	# Make the bubbles
  	geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "transparent", alpha = 1) +
  	scale_fill_gradient(low = pkg.env$color1, high = "#C1B9C3") +
  
  	# Add text in the center of each bubble + control its size
  	geom_text(data = db, aes(x, y, size= Valor, label = paste0(Categoría, "\n", round(Valor, 1)), family = "Open Sans Condensed Light", fontface = "plain")) +
  	scale_size_continuous(range = c(3.5,6)) +
  
    	# General theme:
  	theme_void() + 
  	theme(legend.position="none") +
  	coord_equal()

	# Returns only graph
	return(graph)
}
