Function Main ()
   local abecsPinpad
   
   abecsPinpad := ACBrAbecsPinpad():New()

   abecsPinpad:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   abecsPinpad:ConfigGravarValor("Principal", "LogPath", hb_dirBase() + "Logs")

   abecsPinpad:ConfigGravar()
      
   ?abecsPinpad:Nome
   ?abecsPinpad:Versao

   ?abecsPinpad:Ativar
   ?abecsPinpad:OPN
   ?abecsPinpad:PinPadCapabilities
   abecsPinpad:DSP("Teste DSP")
   abecsPinpad:CLO("Teste CLO")
   ?abecsPinpad:Desativar
   
return NIL