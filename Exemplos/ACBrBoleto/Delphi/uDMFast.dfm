object dmFast: TdmFast
  Height = 188
  Width = 269
  object ACBrBoleto: TACBrBoleto
    MAIL = ACBrMail1
    Banco.Numero = 341
    Banco.TamanhoMaximoNossoNum = 8
    Banco.TipoCobranca = cobItau
    Banco.LayoutVersaoArquivo = 40
    Banco.LayoutVersaoLote = 30
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
    Cedente.PIX.TipoChavePIX = tchNenhuma
    DirArqRemessa = 'c:\temp'
    NumeroArquivo = 0
    ACBrBoletoFC = ACBrBoletoReport
    Configuracoes.Arquivos.LogRegistro = False
    Configuracoes.WebService.SSLHttpLib = httpOpenSSL
    Configuracoes.WebService.StoreName = 'My'
    Configuracoes.WebService.Ambiente = taHomologacao
    Configuracoes.WebService.Operacao = tpInclui
    Configuracoes.WebService.VersaoDF = '1.2'
    Left = 50
    Top = 48
  end
  object ACBrBoletoReport: TACBrBoletoFCFR
    DirLogo = '..\..\..\Fontes\ACBrBoleto\Logos\Colorido'
    FastReportFile = 'report\Boleto.fr3'
    ModoThread = False
    IncorporarBackgroundPdf = False
    IncorporarFontesPdf = False
    Left = 170
    Top = 40
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    Left = 114
    Top = 100
  end
end
