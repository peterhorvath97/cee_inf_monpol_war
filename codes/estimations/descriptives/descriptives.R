lapply(
  paste(file.path('codes', 
                  'estimations', 
                  'descriptives'), 
        list.files(file.path('codes', 
                             'estimations', 
                             'descriptives'))[!str_detect(list.files(file.path('codes', 
                                                                               'estimations', 
                                                                               'descriptives')), 
                                                          'descriptives')],
        sep = '/'),
  source)











