Function Main ()
   local nfe
   
   nfe := ACBrNFe():New()
   //                      Sessão       Chave    Valor
   // Configurações de log da Lib
   nfe:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   nfe:ConfigGravarValor("Principal", "LogPath", hb_dirBase())

   // Salvando configurações
   nfe:ConfigGravar("")
  
   ?nfe:Nome
   ?nfe:Versao

   nfe := nil
    
return NIL