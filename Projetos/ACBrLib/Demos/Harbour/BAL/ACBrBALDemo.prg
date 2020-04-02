Function Main ()
   local bal
   
   bal := ACBrBAL():New()

   bal:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   bal:ConfigGravarValor("Principal", "LogPath", hb_dirBase() + "Logs")
   bal:ConfigGravar()
   
   ?bal:Nome()
   ?bal:Versao()

   bal:Destroy()
    
return NIL