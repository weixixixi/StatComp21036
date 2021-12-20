#' \code{Secondp} function
#'
#' This function outputs the variable selection results using Second-Generation P-values.
#'
#' @importFrom stats complete.cases coef lm predict
#' @param x Independent variables, can be a \code{matrix} or a \code{data.frame}
#' @param y Dependent variable, can be a \code{vector} or a column from a \code{data.frame}
#'
#' @return A list of following components:
#' \describe{
#' \item{var.index}{A vector of indices of selected variables}
#' \item{var.label}{A vector of labels of selected variables}
#' \item{x}{Input data \code{x}}
#' \item{y}{Input data \code{y}}
#' \item{null.bound}{Null bound in the SGPV screening}
#' \item{pe.can}{Point estimates in the candidate set}
#' \item{lb.can}{Lower bounds of CI in the candidate set}
#' \item{ub.can}{Upper bounds of CI in the candidate set}
#' }
#' @export
#' @seealso
#' * [print.secondp()] prints the variable selection results
#' * [coef.secondp()] extracts coefficient estimates
#' * [summary.secondp()] summarizes the OLS outputs

secondp <- function(x, y) {
  if (!is.numeric(as.matrix(x)) | !is.numeric(y)) 
    stop("The input data have non-numeric values.")
  if (any(complete.cases(x) == F) | any(complete.cases(y) == F)) {
    warning("Only complete records will be used.\n")
    comp.index <- complete.cases(data.frame(x, y))# delete invalid data
    x <- x[comp.index, ]
    y <- y[comp.index, ]
  }

  xs <- scale(x)
  ys <- scale(y)
  mod <- lm(ys ~ xs)
  pe <- summary(mod)$coef[-1, 1]
  se <- summary(mod)$coef[-1, 2]

  candidate.index <- 1:ncol(xs)
  null.bound.p <- mean(se)
  out.secondp <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.p)]
  pe.can <- pe
  lb.can <- pe - 1.96 * se
  ub.can <- pe + 1.96 * se

  out <- list(
    var.index = out.secondp,
    var.label = colnames(x)[out.secondp],
    x = data.frame(x),
    y = y,
    pe.can = pe.can,
    lb.can = lb.can,
    ub.can = ub.can,
    null.bound.p = null.bound.p
  )
  return(out)
}

#' \code{print.secondp}: Print variable selection results
#'
#' S3 method \code{print} for an S3 object of class \code{secondp}
#'
#' @param x An \code{secondp} object
#' @param ... Other \code{print} arguments
#'
#' @return Variable selection results
#' @export

print.secondp <- function(x, ...) {
  if (length(x$var.index) > 0) {
    x$var.label
  } else {
    message("None of variables are selected.\n")
  }
}

#' \code{coef.secondp}: Extract coefficients from the model fit
#'
#' S3 method \code{coef} for an S3 object of class \code{secondp}
#' @importFrom stats coef lm
#' @param object An \code{secondp} object
#' @param ... Other \code{coef} arguments
#'
#' @return Coefficients in the OLS model
#' @export

coef.secondp <- function(object, ...) {
  out.coef <- numeric(ncol(object$x))
  if (length(object$var.index) > 0) {
    data.d <- data.frame(yy = object$y, xx = object$x[, object$var.index])
    out.secondp.coef <- coef(lm(yy ~ ., data = data.d))[-1]
    for (i in 1:length(object$var.index)) {
      out.coef[object$var.index[i]] <- out.secondp.coef[i]
    }
  }
  return(out.coef)
}

#' \code{summary.secondp}: Summary of the final model
#'
#' @param object An \code{secondp} object
#' @param ... Other arguments
#'
#' @return Summary of a model
#' @export

summary.secondp <- function(object, ...) {
  if (length(object$var.index) > 0){
    data.d <- data.frame(yy = object$y, 
                         xx = object$x[, object$var.index])
    colnames(data.d)[1] <- "Response"
    colnames(data.d)[-1] <- object$var.label
    summary(lm(Response ~ 1, data = data.d))
  } else {
    message("None of variables are selected.")
    message("Therefore, the summary is shown for the model with intercept only\n")
  }
  data.d <- data.frame(yy = object$y)
  colnames(data.d)[1] <- "Response"
  summary(lm(Response ~ 1, data = data.d))
}