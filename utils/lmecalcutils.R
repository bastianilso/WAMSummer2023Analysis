library(lme4)
library(MuMIn)

# g_lme_table <- function(models, null, nullname) {
#   model_lme <- lapply(seq_along(models), function(i) { 
#     n=names(models)[[i]]
#     a=anova(null, models[[n]]) %>% bind_rows() %>% mutate(model = c(nullname, n))
#     return(a) })
#   model_r2 <- lapply(seq_along(models), function(i) { 
#     n=names(models)[[i]]
#     r=r.squaredGLMM(models[[n]], null=null) %>% as.data.frame() %>% mutate(model = n)
#     return(r) }) 
#   model_r2 <- model_r2 %>% bind_rows()
#   model_lme <- model_lme %>% bind_rows()
#   table <- model_lme %>% left_join(model_r2)  
#   table <- table %>%
#     mutate(AIC = format(round(AIC,2), nsmall = 2),
#            BIC = format(round(BIC,2), nsmall = 2),
#            logLik = format(round(logLik,2), nsmall = 2),
#            R2m = format(round(R2m,2), nsmall = 2),
#            R2c = format(round(R2c,2), nsmall = 2)
#     ) %>%
#     arrange(desc(R2m)) %>%
#     rename(`$\\chi^2$` = `Pr(>Chisq)`, `$R^2_m$` = R2m, `$R^2_c$` = R2c, `Fixed Effect` = model, `ML` = logLik)
#   
#   return(table)
# }

g_lme_table <- function(lmes) {
  g_lme <- function(df, predictors, random, fixed, null, threshold) {
    p = last(predictors)
    
    form_null = paste("(1|",null,")")
    form_base = form_null
    models = tibble(Predicted = p, nullmodel = NA, model = form_null, AIC = NA, BIC = NA, logLik = NA,
                    `Pr(>Chisq)` = NA, R2m = NA, R2c = NA, p_pass = T, r2_pass = T,
                    npar = NA, Chisq = NA, Df = NA, deviance = NA)
    models = models %>% bind_rows(g_lme_add(df, p, random, form_base, form_null, threshold, models, "random"))
    form_null = models %>% filter(p_pass, r2_pass) %>% arrange(R2m) %>% select(model) %>% dplyr::slice(n()) %>% pull()
    models = models %>% bind_rows(g_lme_add(df, p, fixed, form_base, form_null, threshold, models, "fixed"))
    models = models %>% mutate(Predicted = p)
    
    predictors = predictors[-length(predictors)]
    if (length(predictors) == 0) {
      return(models)
    } else {
      models = models %>% bind_rows(g_lme(df, predictors, random, fixed, null, threshold))
      return(models)
    }
  }
  
  g_lme_add <- function(df, p, features, form_base, form_null, threshold, models, mode) {
    if (length(features) == 0) {
      return(models)
    }
    
    if (mode == "random") {
      # 1) See how many random effects are needed
      new_models = g_lme_random(df, p, features, form_null)
    } else if (mode == "fixed") {
      # 2) See how many fixed effects are needed
      
      new_models = g_lme_fixed(df, p, features, form_null)
      
    }
    new_models = new_models %>% mutate(p_pass = ifelse(`Pr(>Chisq)` < threshold, T,F),
                                       r2_pass = ifelse(p_pass & R2m == max(R2m), T, F))
    
    models = models %>% bind_rows(new_models)
    
    
    form_base = models %>% filter(p_pass, r2_pass) %>% select(model) %>% dplyr::slice(n()) %>% pull()
    print(form_base)
    print(form_null)
    if (!identical(form_base, form_null)) {
      form_null = form_base
      features = features [ !str_detect(form_null, features) ]
      
      new_models = new_models %>% bind_rows(g_lme_add(df, p, features, form_base, form_null, threshold, models, mode = mode))
      return(new_models)
    } else {
      return(new_models)
    }
    
  }
  
  
  g_lme_random <- function(df, p, random, form_null) {
    r <- last(random)
    
    form_test = paste("(1|",r,")","+",form_null)
    form_r = paste(p,"~", form_test)
    form_n = paste(p,"~", form_null)
    m_r = lmer(form_r, data = df, REML=F)
    m_n = lmer(form_n, data = df, REML=F)
    a = anova(m_n, m_r) %>% bind_rows() %>% 
      mutate(nullmodel = form_null, model = form_test) %>% dplyr::slice(n())
    
    r2=r.squaredGLMM(m_r, null=m_n) %>% as.data.frame() %>% mutate(nullmodel = form_null, model = form_test)
    model = a %>% left_join(r2)
    
    random = random[-length(random)]
    if (length(random) == 0) {
      return(model)
    } else {
      model = model %>% bind_rows(g_lme_random(df, p, random, form_null))
      return(model)
    }
  }
  
  g_lme_fixed <- function(df, p, fixed, form_null) {
    f <- last(fixed)
    form_test = paste(f,"+",form_null)
    form_r = paste(p,"~", form_test)
    form_n = paste(p,"~", form_null)
    #browser()
    m_f = lmer(form_r, data = df, REML=F)
    m_n = lmer(form_n, data = df, REML=F)
    a = anova(m_n, m_f) %>% bind_rows() %>% 
      mutate(nullmodel = form_null, model = form_test) %>% dplyr::slice(n())
    r2=r.squaredGLMM(m_f, null=m_n) %>% as.data.frame() %>% mutate(nullmodel = form_null, model = form_test)
    model = a %>% left_join(r2)
    
    fixed = fixed[-length(fixed)]
    if (length(fixed) == 0) {
      return(model)
    } else {
      model = model %>% bind_rows(g_lme_fixed(df, p, fixed, form_null))
      return(model)
    }
  }
  
  table = g_lme(lmes$df,lmes$predictors,lmes$random,lmes$fixed,lmes$null,lmes$threshold)
  
  table <- table %>%
    mutate(AIC = format(round(AIC,2), nsmall = 2),
           BIC = format(round(BIC,2), nsmall = 2),
           logLik = format(round(logLik,2), nsmall = 2),
           R2m = format(round(R2m,2), nsmall = 2),
           R2c = format(round(R2c,2), nsmall = 2)
    ) %>%
    arrange(Predicted, desc(R2m)) %>%
    rename(`$\\chi^2$` = `Pr(>Chisq)`, `$R^2_m$` = R2m, `$R^2_c$` = R2c, `Fixed Effect` = model, `ML` = logLik) %>%
    filter(`$\\chi^2$` < lmes$threshold) %>% select(-p_pass, -r2_pass)
  
  
  
  return(table)
}
