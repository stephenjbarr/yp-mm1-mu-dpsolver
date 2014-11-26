library(ggplot2)

plotdp <- function(soln, title = NULL) {
    Nq = length(soln$OptAct);
    queue_length = 1:Nq - 1;
    optimal_service_rate = soln$OptAct;
    dataq = data.frame(cbind(queue_length, as.numeric(optimal_service_rate)));
    colnames(dataq) = c("Queue_Length", "Optimal_Service_Rate")
    g <- ggplot( data=dataq, aes(x = Queue_Length, y = Optimal_Service_Rate, group = 1))
    if(!(is.null(title))) {
        g = g + ggtitle(title)
    }
    g + geom_line()
}
