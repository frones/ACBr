Function Main ()
   local mdfe
   
   mdfe := ACBrMDFe():New()

   mdfe:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   mdfe:ConfigGravarValor("Principal", "LogPath", hb_dirBase() + "Logs")
   mdfe:ConfigGravar()
      
   ?mdfe:Nome
   ?mdfe:Versao

   mdfe:Destroy()
    
return NIL