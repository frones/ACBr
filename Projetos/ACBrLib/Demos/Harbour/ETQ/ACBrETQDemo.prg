Function Main ()
   local etq
   
   etq := ACBrETQ():New()

   etq:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   etq:ConfigGravarValor("Principal", "LogPath", hb_dirBase() + "Logs")
   etq:ConfigGravar()
   
   ?etq:Nome()
   ?etq:Versao()

   etq:Destroy()
    
return NIL