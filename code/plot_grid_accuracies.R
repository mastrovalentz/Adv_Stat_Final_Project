plot_batch_epochs_grid <- function(file.txt, repr_plot_width=4, repr_plot_height=4, repr_plot_res=250) {

	options(repr.plot.width=repr_plot_width, repr.plot.height=repr_plot_height, repr.plot.res=repr_plot_res)


	xlab  <- "Batch size"
	ylab  <- "Epochs"
	title <- "(Batch size, Epochs) tuning"

	grid    <- read.table(file.txt)
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)

}



plot_unit1_unit2_grid <- function(file.txt, repr_plot_width=12, repr_plot_height=4, repr_plot_res=250) {

	par(mfrow=c(1,3))
	options(repr.plot.width=repr_plot_width, repr.plot.height=repr_plot_height, repr.plot.res=repr_plot_res)


	xlab  <- "Unit 1 neurons"
	ylab  <- "Unit 2 neurons"
	title <- "(Unit 1, Unit 2, Unit 3 = 64) tuning"

	grid    <- read.table(file.txt[1])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	g1 <-
	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))   +
	geom_tile(aes(fill=accs), colour="white")               +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1) +
	scale_fill_continuous(low="yellow",high="red")          +
	theme_bw()                                              +
	theme(legend.position="none")                           +
	xlab(xlab)                                              +
	ylab(ylab)                                              +
	ggtitle(title)                                          +
	theme(plot.title=element_text(hjust=0.5))               +
	coord_cartesian(expand=FALSE)


	xlab  <- "Unit 1 neurons"
	ylab  <- "Unit 3 neurons"
	title <- "(Unit 1, Unit 2, Unit 3 = 32) tuning"

	grid    <- read.table(file.txt[2])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	g2 <-
	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))   +
	geom_tile(aes(fill=accs), colour="white")               +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1) +
	scale_fill_continuous(low="yellow",high="red")          +
	theme_bw()                                              +
	theme(legend.position="none")                           +
	xlab(xlab)                                              +
	ylab(ylab)                                              +
	ggtitle(title)                                          +
	theme(plot.title=element_text(hjust=0.5))               +
	coord_cartesian(expand=FALSE)


	xlab  <- "Unit 1 neurons"
	ylab  <- "Unit 2 neurons"
	title <- "(Unit 1, Unit 2, Unit 3 = 16) tuning"

	grid    <- read.table(file.txt[3])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	g3 <-
	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))   +
	geom_tile(aes(fill=accs), colour="white")               +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1) +
	scale_fill_continuous(low="yellow",high="red")          +
	theme_bw()                                              +
	theme(legend.position="none")                           +
	xlab(xlab)                                              +
	ylab(ylab)                                              +
	ggtitle(title)                                          +
	theme(plot.title=element_text(hjust=0.5))               +
	coord_cartesian(expand=FALSE)

	grid.arrange(g1, g2, g3, nrow = 1)

}



plot_actI_actF_grid <- function(file.txt, repr_plot_width=12, repr_plot_height=4, repr_plot_res=250) {

	options(repr.plot.width=repr_plot_width, repr.plot.height=repr_plot_height, repr.plot.res=repr_plot_res)


	xlab  <- "Activation First Layer"
	ylab  <- "Activation Last Layer"
	title <- "(First, Last) Layer Activation tuning"

	grid    <- read.table(file.txt)
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))   +
	geom_tile(aes(fill=accs), colour="white")               +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1) +
	scale_fill_continuous(low="yellow",high="red")          +
	theme_bw()                                              +
	theme(legend.position="none")                           +
	xlab(xlab)                                              +
	ylab(ylab)                                              +
	ggtitle(title)                                          +
	theme(plot.title=element_text(hjust=0.5))               +
	coord_cartesian(expand=FALSE)

}



plot_optimizer_grid <- function(file.txt, repr_plot_width=16, repr_plot_height=4, repr_plot_res=250) {

	par(mfrow=c(1,4))
	options(repr.plot.width=repr_plot_width, repr.plot.height=repr_plot_height, repr.plot.res=repr_plot_res)


	grid    <- read.table(file.txt[1])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)


	grid    <- read.table(file.txt[2])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)


	grid    <- read.table(file.txt[3])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)


	grid    <- read.table(file.txt[4])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)

}




plot_l1_l2_grid <- function(file.txt, repr_plot_width=16, repr_plot_height=4, repr_plot_res=250) {

	par(mfrow=c(1,4))
	options(repr.plot.width=repr_plot_width, repr.plot.height=repr_plot_height, repr.plot.res=repr_plot_res)


	grid    <- read.table(file.txt[1])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)


	grid    <- read.table(file.txt[2])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)


	grid    <- read.table(file.txt[3])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)


	grid    <- read.table(file.txt[4])
	xticks  <- factor(grid[,1])
	yticks  <- factor(grid[,2])
	accs    <- grid[,"Mean_acc"]
	grid.df <- data.frame(xticks, yticks, accs)

	ggplot(data=grid.df, mapping=aes(x=xticks, y=yticks))     +
	geom_tile(aes(fill=accs), colour="white")                 +
	geom_text(aes(label = sprintf("%1.4f", accs)), vjust=1)   +
	scale_fill_continuous(low="yellow",high="red")            +
	theme_bw()                                                +
	theme(legend.position="none")                             +
	xlab(xlab)                                                +
	ylab(ylab)                                                +
	ggtitle(title)                                            +
	theme(plot.title=element_text(hjust=0.5))                 +
	coord_cartesian(expand=FALSE)

}
