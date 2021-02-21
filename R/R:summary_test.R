#' Load a Summary
#'
#' This function calculates the statistics of a dataset as the mean, variance, minimum, maximum ...
#' contains the rownames and the subsequent columns are the sample identifiers.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
summary_test <- function(variable){
  if (is.numeric(variable)) {
    Minimo <- min(variable, na.rm = TRUE)
    Q1 <- quantile(variable,probs = 0.25,  na.rm = TRUE)
    Media <- mean.default(variable, na.rm = TRUE)
    Mediana <- median.default(variable,  na.rm = TRUE )
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    Moda <- getmode(variable)
    Varianza <- var(variable, na.rm = TRUE)
    Desviación_Tipica <- sd(variable, na.rm = TRUE)
    Q3 <- quantile(variable,probs = 0.75,  na.rm = TRUE)
    Maximo <- max(variable, na.rm = TRUE)
    Simetria <- skewness(variable)
    Curtosis <- kurtosis(variable)

    descrptivo <- data.frame(Minimo,Q1,Media,Mediana,Moda,Varianza,Desviación_Tipica,Q3,Maximo,Simetria,Curtosis)

    par(mfrow=c(1,2))
    hist(variable)
    boxplot(variable)
  }
  if (is.factor(variable)) {
    Tabla<- table(variable)
    descrptivo<- Tabla
  }

  return(descrptivo)
  }




