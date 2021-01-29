library(broom)

tidy.survreg <- function(x, conf.level = .95, conf.int = FALSE, ...) {
  s <- summary(x)
  # add extra column name here
  nn <- c("estimate", "std.error", "(Naive SE)", "statistic", "p.value")
  ret <- fix_data_frame(s$table, newnames = nn)
  
  if(conf.int){
    # add confidence interval
    ci <- stats::confint(x, level = conf.level)
    colnames(ci) <- c("conf.low", "conf.high")
    ci <- fix_data_frame(ci)
    ret <- as_tibble(merge(ret, ci, all.x = TRUE, sort = FALSE))
  }
  
  ret
}
modsurvplot <- tidy(modsurv, conf.level = 0.95)
ggplot(modsurvplot, aes(estimate, term)) + 
  geom_point() +
  geom_vline(xintercept = 0)

