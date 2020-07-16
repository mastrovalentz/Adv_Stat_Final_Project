drawPrediction <- function(builder, params, prob, cols){
    model <- builder(params)
    data  <- get_train_data(params$n_bkg) 
    history  <- model %>% fit (data$x_train, data$y_train, verbose=0,
                     epochs=params$epochs, batch_size=params$batch_size, validation_split=0.2)
    y_cut_lhcb_pred <- model %>% predict(x_cut_lhcb)
    y_lhcb_pred     <- model %>% predict(x_lhcb)
    par(mfrow=c(1,2))
    options(repr.plot.width=16, repr.plot.height=8, repr.plot.res=150)
    hist(data.lhcb$Lambda_b0_MM_F[y_lhcb_pred>=0.0], breaks=100, col="firebrick3", xlab="Lb0 Mass", main="LHCb data",  probability=FALSE)
    for(i in 1:length(prob)){
        hist(data.lhcb$Lambda_b0_MM_F[y_lhcb_pred>prob[i]],   breaks=100, col=cols[i], add=TRUE)
    }
    legend(6000, 13000, legend=c(paste("Optimizer: ", params$optimizer),"Full Data Set", paste("Event prob >",prob)),  
           fill=c("white", "firebrick3",cols), border="white", bty='n', x.intersp = 0.5)
   hist(data.cutted.lhcb$Lambda_b0_MM_F[y_cut_lhcb_pred>=0.0], breaks=100, col="firebrick3", xlab="Lb0 Mass", main="LHCb data",  probability=FALSE)
   for(i in 1:length(prob)){
       hist(data.cutted.lhcb$Lambda_b0_MM_F[y_cut_lhcb_pred>prob[i]],   breaks=100, col=cols[i], add=TRUE)
   }
   legend(6000,6000, legend=c(paste("Optimizer: ", params$optimizer),"Full Data Set", paste("Event prob >",prob)),  
          fill=c("white", "firebrick3",cols), border="white", bty='n', x.intersp = 0.5)
}

drawAllPrediction <- function(prob, cols){
    drawPrediction(Archi_build_model, ArchiParams_RMS   ,  prob, cols)
    drawPrediction(Archi_build_model, ArchiParams_ADAM  ,  prob, cols)
    drawPrediction(Archi_build_model, ArchiParams_NADAM ,  prob, cols)
    drawPrediction(Archi_build_model, ArchiParams_SGD   ,  prob, cols)
}