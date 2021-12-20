## ----eval=FALSE---------------------------------------------------------------
#  sgpv <- function(x, y) {
#    if (!is.numeric(as.matrix(x)) | !is.numeric(y))
#      stop("The input data have non-numeric values.")
#    if (any(complete.cases(x) == F) | any(complete.cases(y) == F)) {
#      warning("Only complete records will be used.\n")
#      comp.index <- complete.cases(data.frame(x, y))# delete invalid data
#      x <- x[comp.index, ]
#      y <- y[comp.index, ]
#    }
#    xs <- scale(x)
#    ys <- scale(y)
#    mod <- lm(ys ~ xs)
#    pe <- summary(mod)$coef[-1, 1]
#    se <- summary(mod)$coef[-1, 2]
#  
#    candidate.index <- 1:ncol(xs)
#    null.bound.p <- mean(se)
#    out.sgpv <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.p)]
#    pe.can <- pe
#    lb.can <- pe - 1.96 * se
#    ub.can <- pe + 1.96 * se
#  
#    out <- list(
#      var.index = out.sgpv,
#      var.label = colnames(x)[out.sgpv],
#      x = data.frame(x),
#      y = y,
#      pe.can = pe.can,
#      lb.can = lb.can,
#      ub.can = ub.can,
#      null.bound.p = null.bound.p
#    )
#  
#    class(out) <- "sgpv"
#    return(out)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  print.sgpv <- function(object, ...) {
#    out.coef <- numeric(ncol(object$x))
#    if (length(object$var.index) > 0) {
#      data.d <- data.frame(yy = object$y,
#                           xx = object$x[, object$var.index])
#      out.sgpv.coef <- coef(lm(yy ~ ., data = data.d))[-1]
#      for (i in 1:length(object$var.index)) {
#        out.coef[object$var.index[i]] <- out.sgpv.coef[i]
#      }
#    }
#    if (length(object$var.index) > 0) {
#      cat("Selected variables are", object$var.label, "\n")
#    } else {
#      cat("None of variables are selected.\n")
#    }
#    out <- list(object$var.label, out.coef)
#    return(out)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  summary.sgpv <- function(object, ...) {
#    if (length(object$var.index) > 0){
#      data.d <- data.frame(yy = object$y,
#                           xx = object$x[, object$var.index])
#      colnames(data.d)[1] <- "Response"
#      colnames(data.d)[-1] <- object$var.label
#      summary(lm(Response ~ 1, data = data.d))
#    } else {
#      message("None of variables are selected.")
#      message("Therefore, the summary is shown for the model with intercept only\n")
#    }
#    data.d <- data.frame(yy = object$y)
#    colnames(data.d)[1] <- "Response"
#    summary(lm(Response ~ 1, data = data.d))
#  }

