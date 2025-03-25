Function Main ()
   local pixcd, cDadosCobranca
   
   pixcd := ACBrPIXCD():New()

   pixcd:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   pixcd:ConfigGravarValor("Principal", "LogPath", hb_dirBase() + "Logs")
   pixcd:ConfigGravar()
   
   ?pixcd:Nome()
   ?pixcd:Versao()

   pixcd:GerarQRCodeEstatico(100.00)

   cDadosCobranca := "[CobSolicitada]" + hb_eol() + ;
      "chave=informe a chave pix" + hb_eol() + ;
      "solicitacaoPagador=Pagamento de conta" + hb_eol() + ;
      "expiracao=3600" + hb_eol() + ;
      "valorOriginal=100,00" + hb_eol() + ;
      "modalidadeAlteracao=False" + hb_eol() + ;
      "devedorCPF=" + hb_eol() + ;
      "devedorCNPJ=" + hb_eol() + ;
      "devedorNome=" + hb_eol() + ;
      "[infoAdicionais001]" + hb_eol() + ;
      "nome=Observacao" + hb_eol() + ;
      "valor=Pagamento solicitado no dia 15/12/2023" + hb_eol() + ;
      "[infoAdicionais002]" + hb_eol() + ;
      "nome=Referencia" + hb_eol() + ;
      "valor=123456"

   pixcd:CriarCobrancaImediata(cDadosCobranca)

   pixcd:Destroy()
    
return NIL