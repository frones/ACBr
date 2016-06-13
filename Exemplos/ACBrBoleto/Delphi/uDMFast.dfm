object dmFast: TdmFast
  OldCreateOrder = False
  Height = 150
  Width = 215
  object ACBrBoleto: TACBrBoleto
    Banco.Numero = 341
    Banco.TamanhoMaximoNossoNum = 8
    Banco.TipoCobranca = cobItau
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
  object ACBrBoletoReport: TACBrBoletoFCFR
    ACBrBoleto = ACBrBoleto
    DirLogo = '..\..\..\Fontes\ACBrBoleto\Logos\Colorido'
    FastReportFile = 'report\Boleto.fr3'
    Left = 112
    Top = 32
  end
end
