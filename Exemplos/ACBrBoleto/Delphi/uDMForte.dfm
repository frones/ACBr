object dmForte: TdmForte
  Height = 188
  Width = 269
  object ACBrBoletoReport: TACBrBoletoFCFortes
    MostrarSetup = False
    SoftwareHouse = 'Projeto ACBr - http://acbr.sf.net'
    DirLogo = '..\..\..\Fontes\ACBrBoleto\Logos\Colorido'
    NomeArquivo = 'boleto.pdf'
    Left = 160
    Top = 38
  end
  object ACBrBoleto: TACBrBoleto
    MAIL = ACBrMail1
    Banco.Numero = 341
    Banco.TamanhoMaximoNossoNum = 8
    Banco.TipoCobranca = cobItau
    Banco.LayoutVersaoArquivo = 0
    Banco.LayoutVersaoLote = 0
    Banco.CasasDecimaisMoraJuros = 2
    Banco.DensidadeGravacao = '0'
    Cedente.Nome = 'TodaObra Materias p/ Construcao'
    Cedente.CodigoCedente = '4266443'
    Cedente.Agencia = '0284'
    Cedente.AgenciaDigito = '5'
    Cedente.Conta = '79489'
    Cedente.ContaDigito = '9'
    Cedente.CNPJCPF = '05.481.336/0001-37'
    Cedente.TipoInscricao = pJuridica
    Cedente.CedenteWS.ClientID = 'SGCBS02P'
    Cedente.IdentDistribuicao = tbBancoDistribui
    Cedente.PIX.TipoChavePIX = tchNenhuma
    DirArqRemessa = 'c:\temp'
    NumeroArquivo = 0
    ACBrBoletoFC = ACBrBoletoReport
    Configuracoes.Arquivos.LogRegistro = True
    Configuracoes.WebService.SSLHttpLib = httpOpenSSL
    Configuracoes.WebService.SSLType = LT_TLSv1_2
    Configuracoes.WebService.StoreName = 'My'
    Configuracoes.WebService.TimeOut = 30000
    Configuracoes.WebService.UseCertificateHTTP = False
    Configuracoes.WebService.Ambiente = taHomologacao
    Configuracoes.WebService.Operacao = tpInclui
    Configuracoes.WebService.VersaoDF = '1.2'
    Left = 40
    Top = 38
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    Left = 40
    Top = 112
  end
end
