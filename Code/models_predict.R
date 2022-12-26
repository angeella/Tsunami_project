#form.mod = formula as character or  model=list(past_obs=c(1, 12)) for tsglm
#DB = training set
#new_data = test set
#weights = 0.999^(rev(Time-1))
#model = type of models
#cov = additional covariance for tsglm as vector
# nday = how many day ahead?

#return models and predictions
#########################################################################################
###################################PREDICTION FUNCTION###################################
#########################################################################################
models_predict <- function(form.mod, 
                           DB, 
                           new_data, 
                           weights = NULL, 
                           model = "gam", 
                           cov = NULL,
                           nday = 1){
  
  if(model!="tsglm"){
    form.mod <- as.formula(form.mod)
  }  
    
    if(model == "glm"){
      mod <- glm(form.mod, data = dat_xgb, family = quasipoisson(link = "log"))
    }
    
    if(model == "gam"){
      if(!is.null(weights)){
        
        mod <- mgcv::bam(form.mod, data = DB, family = nb(), weights = eval(str2expression(weights)), select = TRUE)
        
      }else{
        mod <- mgcv::bam(as.formula(form.mod), data = DB, family = nb(),select = TRUE)
        
      }
    }
    
  if(model == "gamm"){
    
    mod <- gamm(form.mod, data = DB, 
                 method = "ML", correlation = corARMA(p = 1, q = 1))#from auto.arima
    
  }
  
    if(model == "tsglm"){
      
      mod <- tsglm(DB$Y, model = form.mod, link = "identity",
                   xreg = DB[,cov], 
                   distr = "nbinom")
      
    }
    
    if(model == "ucm"){
      mod <- ucm(formula = form.mod, 
                 data = DB,
                 season = TRUE,
                 season.length = 7,
                 irregular.var=1)
      
      # proc ucm data=WORK.PROVA_eventi_miss noprint;
      # id data interval=day;
      # model eventi= neve_AGG feste_RID temp_Medio_giornaliero     
      # /*delta_temp*/
      #   flu  delta16_2;
      # irregular variance=1;
      # level variance=1;
      # slope variance=0 noest;
      # season length= 7 type =trig ;
      # estimate SKIPFIRST=1003;
      # forecast lead=7 outfor=previsione_ev;
      # run;
    } 
    if(model !='ucm' & model!='tsglm' & model!= "gamm"){
      errors <- residuals(mod, type = "response")
      error_mod <- auto.arima(errors)
      error_fcast <- forecast(error_mod, h = nday)$mean
      pred_fcast <- predict(mod, newdata = new_data, type = "response", newdata.guaranteed = TRUE)
      pred <- pred_fcast
    }
  if(model == "gamm"){
    
    ## Extract random effects 
    #refa <- ranef(mod$lme)

    pred <- predict(mod$gam, newdata = new_data, type = "response")
    pred_fcast <- pred
    ## add in effect for fa = "2" and fb="2/4"...
    #p <- p0 + refa["2",1] + refb["2/4",1] 
    
  }
    if(model == "ucm"){

      newdata1 <- SSModel(as.formula(paste0("rep(NA,1) ~ ", gsub("Y", "NA", form.mod)[3], "+ SSMtrend(1, Q =  list(mod$est.var.level))",
                                            "+ SSMseasonal(7, Q = mod$est.var.season)")), 
                          H = mod$irr.var, data=new_data)
      
      pred <- predict(object = mod$model, n.ahead = nday, newdata = newdata1)[[1]]
      pred_fcast <- pred
      
      }
    if(model == 'tsglm'){
      pred_fcast <- predict(mod, newdata = new_data)$pred
      pred <- pred_fcast 
      
    }
    #Prediction
    out <- new_data
    if(length(pred)>1){
      out$Y <- pred
    }else{
      out$Y <- pred[[1]]
    }
    
    
  return(out = list(mod = mod, out = out, out1 = pred_fcast))
}

