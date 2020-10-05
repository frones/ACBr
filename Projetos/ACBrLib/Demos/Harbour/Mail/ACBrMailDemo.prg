Function Main ()
   local mail
   
   mail := ACBrMail():New()
   //                      Sessão       Chave    Valor
   // Configurações de log da Lib
   mail:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   mail:ConfigGravarValor("Principal", "LogPath", hb_dirBase())

   // Salvando configurações
   mail:ConfigGravar("")
  
   ?mail:Nome()
   ?mail:Versao()

   mail:Clear()

   //Adicionar email para envio
   mail:AddAddress("", "")

    
return NIL