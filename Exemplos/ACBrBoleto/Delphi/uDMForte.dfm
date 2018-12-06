object dmForte: TdmForte
  OldCreateOrder = False
  Left = 511
  Top = 250
  Height = 150
  Width = 215
  object ACBrBoletoReport: TACBrBoletoFCFortes
    MostrarSetup = False
    SoftwareHouse = 'Projeto ACBr - http://acbr.sf.net'
    DirLogo = '..\..\..\Fontes\ACBrBoleto\Logos\Colorido'
    NomeArquivo = 'boleto.pdf'
    Left = 128
    Top = 30
  end
  object ACBrBoleto: TACBrBoleto
    Banco.Numero = 341
    Banco.TamanhoMaximoNossoNum = 8
    Banco.TipoCobranca = cobItau
    Banco.LayoutVersaoArquivo = 0
    Banco.LayoutVersaoLote = 0
    Cedente.Nome = 'TodaObra Materias p/ Construcao'
    Cedente.CodigoCedente = '4266443'
    Cedente.Agencia = '0284'
    Cedente.AgenciaDigito = '5'
    Cedente.Conta = '79489'
    Cedente.ContaDigito = '9'
    Cedente.CNPJCPF = '05.481.336/0001-37'
    Cedente.TipoInscricao = pJuridica
    DirArqRemessa = 'c:\temp'
    NumeroArquivo = 0
    ACBrBoletoFC = ACBrBoletoReport
    Left = 32
    Top = 30
  end
end
