# create temporary variables
temp <- NULL
plot <- NULL
numeric <- NULL
categorical <- NULL

# create function to always plot x axis with no decimals
scaleFunction <- function(x) sprintf("%.0f", x)

# loop over variables in 'partialPredictors'
for (i in 1:length(partialPredictors[[1]])) {
  var <- partialPredictors[[1]][i]
  temp <- pdp::partial(model,
                  pred.var = var,
                  prob = TRUE,
                  plot = FALSE,
                  progress = "text",
                  type = "classification",
                  which.class = "yes")

  # is the predictor numeric?
  if(is.numeric(temp[[var]]) == TRUE) {
    numeric <- TRUE }
  else { numeric <- FALSE }
  
  # is the predictor categorical?
  if(is.numeric(temp[[var]]) == FALSE) {
    categorical <- TRUE }
  else { categorical <- FALSE }
  
  plot[[i]] <- 
    ggplot(temp, aes_string(x = var, y = "yhat")) +
    
    # for categorical predictors, plot a point
    {if(categorical)geom_point()} +
    {if(categorical)scale_x_discrete(labels = c("0" = "No",
                                                "1" = "Yes"))} +
    
    # for numeric predictors, plot a line, use scaleFunction
    {if(numeric)geom_line()} +
    {if(numeric)scale_x_continuous(labels = scaleFunction)} +
    
    labs(title = "",
         x = partialPredictors[[2]][i],
         y = "") +
    theme(text = element_text(size = 7),
          axis.text = element_text(size = 7)) +
    ylim(0.2, 0.8)
}