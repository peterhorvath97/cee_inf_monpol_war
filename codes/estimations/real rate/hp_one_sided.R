hp_one_sided <- function(ts, burn, freq){
  fit <- ts[1:burn] %>% 
    ts(freq = freq) %>% 
    mFilter::hpfilter(freq = freq)
  
  trend <- fit$trend %>% as.numeric()
  cycle <- fit$cycle %>% as.numeric()
  
  for(i in (burn+1):length(ts)){
    fit <- ts[1:i] %>% 
      ts(freq = freq) %>% 
      mFilter::hpfilter(freq = freq)
    
    trend[i] <- fit$trend[i]
    cycle[i] <- fit$cycle[i]
  }
  
  out <- list(trend = trend,
              cycle = cycle)
  out
}