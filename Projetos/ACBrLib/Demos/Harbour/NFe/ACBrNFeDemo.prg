Function Main ()
   local nfe
   
   nfe := ACBrNFe():New()

   nfe:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   nfe:ConfigGravarValor("Principal", "LogPath", hb_dirBase() + "Logs")
   nfe:ConfigGravar()
   
   nfe:CarregarXML(hb_dirBase() + "nfe.xml")

   nfe:Imprimir("", 1, "", "True", hb_dirBase() + "rftd.jpg", "", "")
   
   ?nfe:Nome
   ?nfe:Versao

   nfe:Destroy()
    
return NIL