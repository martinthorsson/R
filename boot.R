quantreg.boot <- function(data = NULL, 
                          formula = NULL,
                          B = NULL) {
          library(boot)
          library(stringr)
          
          boot_coeff <- function(d, i) {
                    sample <- d[i,]
                    fit <- rq(formula = formula,
                              data = sample)
                    return(fit$coefficients)
          }
          
          boot_results <- boot(data, 
               boot_coeff,
               R = B,
               parallel = "multicore",
               ncpus = 20)
          
          # regression coefficients
          coefs <- apply(boot_results$t,
                         2,
                         mean)         
          
          #coefficient confidence interval
          coefs_ci <- apply(boot_results$t,
                            2,
                            quantile,
                            probs = c(0.025,0.975))
          
          #coefficient p - value
          null_value <- 0
          
          p_values <- apply(boot_results$t,
                            2,
                            function(x) {
                                      t <- mean(x < null_value) + 0.5 * mean(x == null_value)
                                      p <- 2 * min(t, 1 - t)
                                      return(p)
                            }
          )   
          

          s <- data.frame(
                    coefs,
                    t(coefs_ci),
                    p_values)
          
          formula_as_str <- deparse1(formula)
          split_formula <- str_extract_all(formula_as_str,"\\w+",
                                           simplify = TRUE)
          
          names <- c("Intercept")

          for(i in 2:length(split_formula)) {
                names[i] <- split_formula[i]  
          }       
          rownames(s) <- names
           
        
          return(s)
}



