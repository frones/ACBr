Function Main ()
   local boleto
   local vendaRetorno, strIniVenda
   local hIni, hIniVenda
   LOCAL cSection
   
   boleto := ACBrBoleto():New("", "")
   //                      Sessão       Chave    Valor
   // Configurações de log da Lib
   boleto:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   boleto:ConfigGravarValor("Principal", "LogPath", hb_dirBase())

   // Salvando configurações
   boleto:ConfigGravar("E:\Programacao\ACBr\ACBr\Projetos\ACBrLib\Demos\Harbour\Boleto\boleto.ini")
  
   ?boleto:Nome
   ?boleto:Versao

   boleto := nil
    
return NIL