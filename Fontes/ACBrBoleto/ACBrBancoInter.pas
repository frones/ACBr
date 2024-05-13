{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor Hugo Gonzales - Panda, Douglas Tybel,    }
{   Rodrigo Cardilo, WindSoft, Aggille Sistema de Gestão, Zoobre               }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$i ACBr.inc}
unit ACBrBancoInter;

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto, ACBrBoletoConversao;

type
  { TACBrBancoInter }
  TACBrBancoInter = class(TACBrBancoClass)
  private
    fValorTotalDocs : Double;
    fQtRegLote      : Integer;
    fNumeroRemessa  : Integer;
  protected
    function GetLocalPagamento: String; override;
  public
    constructor create(AOwner: TACBrBanco);
    function CalcularTamMaximoNossoNumero(const Carteira: String; const NossoNumero : String = '';const Convenio: String = ''): Integer; override;
    function CalcularNomeArquivoRemessa: string; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa: TStringList); override;
    function GerarRegistroHeader240(NumeroRemessa: Integer) : String; override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    Procedure LerRetorno240(ARetorno:TStringList); override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): string; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string; override;
  end;

implementation

uses
  {$ifdef COMPILER6_UP} DateUtils {$else} ACBrD5 {$endif}, StrUtils, Variants,
  ACBrValidador, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime ;

constructor TACBrBancoInter.Create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);
  fpDigito                := 9;
  fpNome                  := 'INTER';
  fpNumero                := 077;
  fpTamanhoMaximoNossoNum := 11;
  fpTamanhoAgencia        := 4;
  fpTamanhoConta          := 9;
  fpTamanhoCarteira       := 3;
  fValorTotalDocs         := 0;
  fQtRegLote              := 0;
  fpCodigosMoraAceitos    := '012';
end;

function TACBrBancoInter.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string;
begin
  Modulo.FormulaDigito        := frModulo10;
  Modulo.MultiplicadorFinal   := 1;
  Modulo.MultiplicadorInicial := 2;
  Modulo.Documento :=
    ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.Carteira
    + PadLeft(ACBrTitulo.NossoNumero, fpTamanhoMaximoNossoNum, '0');
  Modulo.Calcular;
  Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoInter.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras: string;
  Boleto : TACBrBoleto;
begin
  Boleto := ACBrTitulo.ACBrBoleto;

  if StrToInt64Def(ACBrTitulo.NossoNumero,0) = 0 then
    raise Exception.create(ACBrStr('Banco '+Boleto.Banco.Nome + ' Nosso Número não Informado'));


  FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

  CodigoBarras := IntToStrZero(Boleto.Banco.Numero, 3)
                  + '9'
                  + FatorVencimento
                  + IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10)
                  + Boleto.Cedente.Agencia
                  + ACBrTitulo.Carteira
                  + PadLeft(Boleto.Cedente.CodigoCedente, 7, '0')
                  + PadLeft(ACBrTitulo.NossoNumero, TamanhoMaximoNossoNum, '0');

  DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);

  Result := copy(CodigoBarras, 1, 4)
            + DigitoCodBarras
            + copy(CodigoBarras, 5, 39);
end;

function TACBrBancoInter.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string;
var
  NossoNr, NossoNrDv: string;
begin

  NossoNrDv := PadLeft(ACBrTitulo.NossoNumero, TamanhoMaximoNossoNum, '0') ;
  if ACBrTitulo.Carteira = '112' then
  begin
    NossoNr := ACBrTitulo.ACBrBoleto.Cedente.Agencia
               + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
               + '/'
               + ACBrTitulo.Carteira
               + '/'
               + Copy(NossoNrDv, 1, 10)
               + '-'
               + Copy(NossoNrDv, 11, 1);

  end else
  begin
    NossoNr := ACBrTitulo.ACBrBoleto.Cedente.Agencia
               + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
               + '/'
               + ACBrTitulo.Carteira
               + '/'
               + Copy(NossoNrDv, 1, 10)
               + '-'
               + CalcularDigitoVerificador(ACBrTitulo);
  end;
  Result := NossoNr ;

end;

function TACBrBancoInter.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
            + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
            + '/'
            + ACBrTitulo.ACBrBoleto.Cedente.Conta;

end;

function TACBrBancoInter.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

procedure TACBrBancoInter.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  wLinha       : string;
  Beneficiario : TACBrCedente;
begin

  fNumeroRemessa := NumeroRemessa;
  Beneficiario := ACBrBanco.ACBrBoleto.Cedente;

  wLinha := '0'                                  + // 001 a 001 Identificação do registro
            '1'                                  + // 002 a 002 Identificação do arquivo remessa
            'REMESSA'                            + // 003 a 009 Literal remessa
            '01'                                 + // 010 a 011 Código de serviço
            PadRight('COBRANCA', 15, ' ')        + // 012 a 026 Literal serviço
            Space(20)                            + // 027 a 046 Código da empresa
            PadRight(Beneficiario.Nome, 30, ' ') + // 047 a 076 Nome da empresa
            IntToStrZero(ACBrBanco.Numero, 3)    + // 077 a 079 Número do Inter na câmara de compensação
            PadRight('INTER', 15, ' ')           + // 080 a 094 Nome do banco por extenso
            FormatDateTime('ddmmyy', Now)        + // 095 a 100 Data da gravação do arquivo
            Space(10)                            + // 101 a 110 Branco
            IntToStrZero(NumeroRemessa, 7)       + // 111 a 117 Nº sequencial de remessa
            Space(277)                           + // 118 a 394 Branco
            '000001';                              // 395 a 400 Nº sequencial do registro de um em um

  ARemessa.Text := ARemessa.Text
                   + UpperCase(wLinha);

end;

procedure TACBrBancoInter.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList);
var
  ACodigoMoraJuros, ATipoCedente, ATipoSacado, ADataMoraJuros, ADataDesconto, wLinha,
  wCarteira, ValorMora, EspecieDoc, ADesconto, AMensagem, LDigitoVerificador : string;
  Boleto : TACBrBoleto;
  ADataLimitePagamento : Byte;
begin
  Boleto := ACBrTitulo.ACBrBoleto;

  { Configuração Juros }
  ACodigoMoraJuros := '0';
  ADataMoraJuros   := '';
  ValorMora  := '0';
  if (ACBrTitulo.ValorMoraJuros > 0) then
  begin
    ACodigoMoraJuros := IfThen((ACBrTitulo.CodigoMoraJuros = cjValorDia),'1','2');
    ADataMoraJuros   := DefineDataMoraJuros(ACBrTitulo, 'ddmmyy');
    ValorMora  := CurrToStr(round(ACBrTitulo.ValorMoraJuros * 100));
  end;

  // descontos
  case ACBrTitulo.TipoDesconto of
     tdNaoConcederDesconto:
       ADesconto := '0';
     tdValorFixoAteDataInformada:
       ADesconto := '1';
     tdValorAntecipacaoDiaCorrido:
       ADesconto := '2';
     tdValorAntecipacaoDiaUtil:
       ADesconto := '3';
     tdPercentualAteDataInformada:
       ADesconto := '4';
     tdPercentualSobreValorNominalDiaCorrido:
       ADesconto := '5';
     tdPercentualSobreValorNominalDiaUtil:
       ADesconto := '6';
   else
     ADesconto := '0';
   end;

   if (ACBrTitulo.ValorDesconto > 0) and (ACBrTitulo.DataDesconto <> Null) then
    ADataDesconto := FormatDateTime('ddmmyy', ACBrTitulo.DataDesconto);

  // tipo cedente
  case ACBrTitulo.ACBrBoleto.Cedente.TipoInscricao of
    pFisica   : ATipoCedente := '01';
    pJuridica : ATipoCedente := '02';
  end;

  // tipo de sacado
  case ACBrTitulo.Sacado.Pessoa of
    pFisica   : ATipoSacado := '01';
    pJuridica : ATipoSacado := '02';
  else
    ATipoSacado := '99';
  end;

  EspecieDoc := ACBrTitulo.EspecieDoc;

  if EspecieDoc = 'DM' then
    EspecieDoc := '01';

  if ACBrTitulo.DataLimitePagto >= ACBrTitulo.Vencimento then
     ADataLimitePagamento := Round(ACBrTitulo.DataLimitePagto - ACBrTitulo.Vencimento);

  if not (ADataLimitePagamento in [30,60]) then
    ADataLimitePagamento := 0;

  AMensagem := Copy(stringreplace(ACBrTitulo.Mensagem.Text,sLineBreak,' ',[rfReplaceAll]), 1, 70) ;
  wCarteira := Trim(ACBrTitulo.Carteira);

  if wCarteira = '112' then
  begin
    ACBrTitulo.NossoNumero := '0';
    LDigitoVerificador := '0';
  end else
    LDigitoVerificador := CalcularDigitoVerificador(ACBrTitulo);


  wLinha := '1'                                                                                                                                          + // 1 a 1 Identificação do registro de transação
            Space(19)                                                                                                                                    + // 002 a 020 Branco
            wCarteira                                                                                                                                    + // 021 a 023 carteira 110 ou 112
            '0001' + PadLeft(Boleto.Cedente.Conta + Boleto.Cedente.ContaDigito, 10, '0')                                                                 + // 021 a 037 Identificação da empresa beneficiária no Inter
            PadRight(ACBrTitulo.SeuNumero, 25, ' ')                                                                                                      + // 038 a 062 Número de controle para uso da empresa.
            Space(3)                                                                                                                                     + // 063 a 065 Branco
            IfThen((ACBrTitulo.PercentualMulta > 0), IfThen(ACBrTitulo.MultaValorFixo, '1', '2'), '0')                                                   + // 066 a 066 Campo de multa
            IfThen(ACBrTitulo.MultaValorFixo, IntToStrZero(Round(ACBrTitulo.PercentualMulta * 100), 13), PadLeft('', 13, '0'))                           + // 067 a 079 Valor da multa
            IfThen(ACBrTitulo.MultaValorFixo, PadLeft('', 4, '0'), IntToStrZero(Round(ACBrTitulo.PercentualMulta * 100), 4))                             +  // 080 a 083 Percentual de multa
            IfThen((ACBrTitulo.DataMulta > 0) and (ACBrTitulo.PercentualMulta > 0), FormatDateTime('ddmmyy', ACBrTitulo.DataMulta), PadLeft('', 6, '0')) + // 084 a 089 Data da multa
            PadLeft(ACBrTitulo.NossoNumero+LDigitoVerificador, 11, '0')                                                                                                     + // 090 a 100 Identificação do título no banco ("Nosso número") (será preenchido pelo Inter no arquivoretorno)
            Space(8)                                                                                                                                     + // 101 a 108 Branco
            '01'                                                                                                                                         + // 109 a 110 Identificação da ocorrência
            PadRight(ACBrTitulo.NumeroDocumento, 10, ' ')                                                                                                + // 111 a 120 Nº do documento (Seu número)
            FormatDateTime('ddmmyy', ACBrTitulo.Vencimento)                                                                                              + // 121 a 126 Data do vencimento do título
            IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 13)                                                                                     + // 127 a 139 Valor do título
            IntToStrZero(ADataLimitePagamento, 2)                                                                                                        + // 140 a 141 Data limite de pagamento
            Space(6)                                                                                                                                     + // 142 a 147 Branco
            PadLeft(EspecieDoc, 2, ' ')                                                                                                                  + // 148 a 149 Espécie do título
            'N'                                                                                                                                          + // 150 a 150 Identificação
            Space(6)                                                                                                                                     + // 151 a 156 Data da emissão do título
            Space(3)                                                                                                                                     + // 157 a 159 Branco
            ACodigoMoraJuros                                                                                                                             + // 160 a 160 Campo de Tipo de Mora/juros (Utilizar Percentual)
            PadLeft(IfThen(ACodigoMoraJuros = '1',ValorMora,'0'), 13, '0')                                                                               + // 161 a 173 (Valor zerado para utilizar percentual)
            PadLeft(ifThen(ACodigoMoraJuros = '2',ValorMora,'0'), 4, '0')                                                                                + // 174 a 177 Percentual a ser cobrado juros/mora
            PadLeft(ADataMoraJuros, 6, '0')                                                                                                              + // 178 a 183 Data da mora
            ADesconto                                                                                                                                    + // 184 a 184 Campo de descontos
            IntToStrZero(Round(ACBrTitulo.ValorDesconto * 100), 13)                                                                                      + // 185 a 197 Valor do desconto 1
            PadLeft('', 4, '0')                                                                                                                          + // 198 a 201 Percentual de desconto 1
            PadLeft(ADataDesconto, 6, '0')                                                                                                               + // 202 a 207 Data limite para concessão dodesconto 1
            IntToStrZero(Round(ACBrTitulo.ValorAbatimento * 100), 13)                                                                                    + // 208 a 220 Valor do abatimento a ser concedido
            ATipoSacado                                                                                                                                  + // 221 a 222 Identificação do tipo de inscrição do pagador
            PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF), 14, '0')                                                                                      + // 223 a 236 Nº Inscrição do pagador
            PadRight(TiraAcentos(ACBrTitulo.Sacado.NomeSacado), 40, ' ')                                                                                 + // 237 a 276 Nome do pagador
            PadRight( RemoverEspacosDuplos( TiraAcentos(
                                                         ACBrTitulo.Sacado.Logradouro
                                                         + ' '
                                                         + ACBrTitulo.Sacado.Numero
                                                         + ' '
                                                         + ACBrTitulo.Sacado.Complemento
                                                         + ' '
                                                         + ACBrTitulo.Sacado.Bairro
                                                         + ' '
                                                         + ACBrTitulo.Sacado.Cidade
                                                         + ' '
                                                         + ACBrTitulo.Sacado.UF ) ), 40, ' ')                                                            + // 277 a 316 Endereço completo
            PadLeft(OnlyNumber(ACBrTitulo.Sacado.CEP), 8, '0')                                                                                           + // 317 a 321 CEP || 322 a 324 Sufixo do CEP
            PadRight(AMensagem, 70, ' ')                                                                                                                 + // 325 a 394 1ª Mensagem
            IntToStrZero(ARemessa.Count + 1, 6);                                                                                                           // 395 a 400 Nº sequencial do registro

  ARemessa.Text := ARemessa.Text + UpperCase(wLinha);

end;

function TACBrBancoInter.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoRegistro);
end;

function TACBrBancoInter.CalcularTamMaximoNossoNumero(const Carteira: String; const NossoNumero : String = '';const Convenio: String = ''): Integer;
begin
  if Carteira = '112' then
    fpTamanhoMaximoNossoNum := 11
  else
    fpTamanhoMaximoNossoNum := 10;
  Result := fpTamanhoMaximoNossoNum;
end;

procedure TACBrBancoInter.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: string;
begin

  wLinha := '9'                                 +  // 001 a 001 Identificação registro
            IntToStrZero(ARemessa.Count - 1, 6) +  // 002 a 007 Quantidade de boletos
            Space(387)                          +  // 002 a 394 Branco
            IntToStrZero(ARemessa.Count + 1, 6);   // 395 a 400 Número sequencial de registro

  ARemessa.Text := ARemessa.Text + UpperCase(wLinha);

end;

procedure TACBrBancoInter.LerRetorno240(ARetorno: TStringList);
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

procedure TACBrBancoInter.LerRetorno400(ARetorno: TStringList);
var
  ContLinha: Integer;
  Linha, rCedente: string;
  Titulo: TACBrTitulo;
  Boleto : TACBrBoleto;
begin
  Boleto := ACBrBanco.ACBrBoleto;

  if (StrToIntDef(copy(ARetorno.Strings[0], 77, 3), -1) <> Numero) then
    raise Exception.create(ACBrStr(Boleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

  rCedente := trim(Copy(ARetorno[0], 47, 30));

  Boleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2)
                        + '/'
                        + Copy(ARetorno[0], 97, 2)
                        + '/'
                        + Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY');

  Boleto.ListadeBoletos.Clear;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if (Copy(Linha, 1, 1) <> '1') then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    if ACBrBanco.ACBrBoleto.LeCedenteRetorno then
    begin
      ACBrBanco.ACBrBoleto.Cedente.Nome    := rCedente;
      Titulo.Carteira                      := copy(Linha, 21, 3);
      ACBrBanco.ACBrBoleto.Cedente.Agencia := copy(Linha, 24, 4);
      ACBrBanco.ACBrBoleto.Cedente.Conta   := copy(Linha, 28,10);
    end;


    Titulo.SeuNumero               := copy(Linha, 38, 25);
    Titulo.NumeroDocumento         := copy(Linha, 98, 10);
    Titulo.Carteira                := copy(Linha, 87, 3);
    Titulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 90, 2), 0));

    if Titulo.OcorrenciaOriginal.Tipo = toRetornoRegistroRecusado then
      Titulo.DescricaoMotivoRejeicaoComando.Add(copy(Linha, 241, 140));

    Titulo.Sacado.NomeSacado       := copy(Linha, 325, 30);
    Titulo.DataOcorrencia          := StringToDateTimeDef(Copy(Linha, 92, 2) + '/' + Copy(Linha, 94, 2) + '/' + Copy(Linha, 96, 2), 0, 'DD/MM/YY');
    Titulo.EspecieDoc              := 'DM';

    if (StrToIntDef(Copy(Linha, 119, 6), 0) <> 0) then
      Titulo.Vencimento := StringToDateTimeDef(Copy(Linha, 119, 2)
                                               + '/'
                                               + Copy(Linha, 121, 2)
                                               + '/'
                                               + Copy(Linha, 123, 2), 0, 'DD/MM/YY');


    Titulo.ValorDocumento          := StrToFloatDef(Copy(Linha, 125, 13), 0) / 100;
    Titulo.ValorRecebido           := StrToFloatDef(Copy(Linha, 160, 13), 0) / 100;

    if Boleto.LerNossoNumeroCompleto or (Titulo.Carteira = '112') then
      Titulo.NossoNumero             := Copy(Linha, 71, 11)
    else
      Titulo.NossoNumero             := Copy(Linha, 71, 10);

    Titulo.Sacado.NomeSacado       := Copy(Linha, 182, 40);
    Titulo.Sacado.CNPJCPF          := Trim(Copy(Linha, 227, 14));
    if (StrToIntDef(Copy(Linha, 173, 6), 0) <> 0) then
      Titulo.DataCredito := StringToDateTimeDef(Copy(Linha, 173, 2)
                                                + '/'
                                                + Copy(Linha, 175, 2)
                                                + '/'
                                                + Copy(Linha, 177, 2), 0, 'DD/MM/YY');
  end;
end;

function TACBrBancoInter.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): string;
var
  CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação Normal';
    07: Result := '07-Baixa Simples';
  end;
end;

function TACBrBancoInter.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoBaixaSimples;
    else
      Result := toTipoOcorrenciaNenhum;
  end;
end;

function TACBrBancoInter.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): string;
begin
  Result := '';
  case TipoOcorrencia of
    toRetornoLiquidadoParcialmente                       : Result := '07';
    toRetornoBaixaCreditoCCAtravesSispag                 : Result := '59';
    toRetornoEntradaConfirmadaRateioCredito              : Result := '64';
    toRetornoChequePendenteCompensacao                   : Result := '65';
    toRetornoChequeDevolvido                             : Result := '69';
    toRetornoEntradaRegistradaAguardandoAvaliacao        : Result := '71';
    toRetornoBaixaCreditoCCAtravesSispagSemTituloCorresp : Result := '72';
    toRetornoConfirmacaoEntradaCobrancaSimples           : Result := '73';
    toRetornoChequeCompensado                            : Result := '76';
  end;

  if (Result <> '') then
    begin
      Exit;
    end;

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                                        : Result := '02';
    toRetornoRegistroRecusado                                          : Result := '03';
    toRetornoAlteracaoDadosNovaEntrada                                 : Result := '04';
    toRetornoAlteracaoDadosBaixa                                       : Result := '05';
    toRetornoLiquidado                                                 : Result := '06';
    toRetornoLiquidadoEmCartorio                                       : Result := '08';
    toRetornoBaixaSimples                                              : Result := '07';
    toRetornoBaixaPorTerSidoLiquidado                                  : Result := '10';
    toRetornoTituloEmSer                                               : Result := '11';
    toRetornoAbatimentoConcedido                                       : Result := '12';
    toRetornoAbatimentoCancelado                                       : Result := '13';
    toRetornoVencimentoAlterado                                        : Result := '14';
    toRetornoBaixaRejeitada                                            : Result := '15';
    toRetornoInstrucaoRejeitada                                        : Result := '16';
    toRetornoAlteracaoDadosRejeitados                                  : Result := '17';
    toRetornoCobrancaContratual                                        : Result := '18';
    toRetornoRecebimentoInstrucaoProtestar                             : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto                        : Result := '20';
    toRetornoRecebimentoInstrucaoNaoProtestar                          : Result := '21';
    toRetornoEncaminhadoACartorio                                      : Result := '23';
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente               : Result := '24';
    toRetornoAlegacaoDoSacado                                          : Result := '25';
    toRetornoTarifaAvisoCobranca                                       : Result := '26';
    toRetornoTarifaExtratoPosicao                                      : Result := '27';
    toRetornoTarifaDeRelacaoDasLiquidacoes                             : Result := '28';
    toRetornoTarifaDeManutencaoDeTitulosVencidos                       : Result := '29';
    toRetornoDebitoTarifas                                             : Result := '30';
    toRetornoBaixaPorProtesto                                          : Result := '32';
    toRetornoCustasProtesto                                            : Result := '33';
    toRetornoCustasSustacao                                            : Result := '34';
    toRetornoCustasCartorioDistribuidor                                : Result := '35';
    toRetornoCustasEdital                                              : Result := '36';
    toRetornoTarifaEmissaoBoletoEnvioDuplicata                         : Result := '37';
    toRetornoTarifaInstrucao                                           : Result := '38';
    toRetornoTarifaOcorrencias                                         : Result := '39';
    toRetornoTarifaMensalEmissaoBoletoEnvioDuplicata                   : Result := '40';
    toRetornoDebitoMensalTarifasExtradoPosicao                         : Result := '41';
    toRetornoDebitoMensalTarifasOutrasInstrucoes                       : Result := '42';
    toRetornoDebitoMensalTarifasManutencaoTitulosVencidos              : Result := '43';
    toRetornoDebitoMensalTarifasOutrasOcorrencias                      : Result := '44';
    toRetornoDebitoMensalTarifasProtestos                              : Result := '45';
    toRetornoDebitoMensalTarifasSustacaoProtestos                      : Result := '46';
    toRetornoBaixaTransferenciaParaDesconto                            : Result := '47';
    toRetornoCustasSustacaoJudicial                                    : Result := '48';
    toRetornoTarifaMensalRefEntradasBancosCorrespCarteira              : Result := '51';
    toRetornoTarifaMensalBaixasCarteira                                : Result := '52';
    toRetornoTarifaMensalBaixasBancosCorrespCarteira                   : Result := '53';
    toRetornoTarifaMensalLiquidacoesCarteira                           : Result := '54';
    toRetornoTarifaMensalLiquidacoesBancosCorrespCarteira              : Result := '55';
    toRetornoCustasIrregularidade                                      : Result := '56';
    toRetornoInstrucaoCancelada                                        : Result := '57';
    toRetornoEntradaRejeitadaCarne                                     : Result := '60';
    toRetornoTarifaEmissaoAvisoMovimentacaoTitulos                     : Result := '61';
    toRetornoDebitoMensalTarifaAvisoMovimentacaoTitulos                : Result := '62';
    toRetornoTituloSustadoJudicialmente                                : Result := '63';
    toRetornoInstrucaoNegativacaoExpressaRejeitada                     : Result := '74';
    toRetornoConfRecebimentoInstEntradaNegativacaoExpressa             : Result := '75';
    toRetornoConfRecebimentoInstExclusaoEntradaNegativacaoExpressa     : Result := '77';
    toRetornoConfRecebimentoInstCancelamentoNegativacaoExpressa        : Result := '78';
    toRetornoNegativacaoExpressaInformacional                          : Result := '79';
    toRetornoConfEntradaNegativacaoExpressaTarifa                      : Result := '80';
    toRetornoConfCancelamentoNegativacaoExpressaTarifa                 : Result := '82';
    toRetornoConfExclusaoEntradaNegativacaoExpressaPorLiquidacaoTarifa : Result := '83';
    toRetornoTarifaPorBoletoAte03EnvioCobrancaAtivaEletronica          : Result := '85';
    toRetornoTarifaEmailCobrancaAtivaEletronica                        : Result := '86';
    toRetornoTarifaSMSCobrancaAtivaEletronica                          : Result := '87';
    toRetornoTarifaMensalPorBoletoAte03EnvioCobrancaAtivaEletronica    : Result := '88';
    toRetornoTarifaMensalEmailCobrancaAtivaEletronica                  : Result := '89';
    toRetornoTarifaMensalSMSCobrancaAtivaEletronica                    : Result := '90';
    toRetornoTarifaMensalExclusaoEntradaNegativacaoExpressa            : Result := '91';
    toRetornoTarifaMensalCancelamentoNegativacaoExpressa               : Result := '92';
    toRetornoTarifaMensalExclusaoNegativacaoExpressaPorLiquidacao      : Result := '93';
  else
    Result := '02';
  end;
end;

function TACBrBancoInter.CalcularNomeArquivoRemessa: string;
var Boleto : TACBrBoleto;
begin
  Boleto := ACBrBanco.ACBrBoleto;

  fNumeroRemessa := Boleto.NumeroArquivo;

  Boleto.NomeArqRemessa := 'CI400_001_'
                           + FormatFloat('0000000', fNumeroRemessa) // número sequencial de remessa com 7 digitos posição 111 a 117 remessa
                           + '.rem';

  Result := Boleto.DirArqRemessa
            + PathDelim
            + Boleto.NomeArqRemessa;

end;

function TACBrBancoInter.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia;
  CodMotivo: Integer): string;
begin
  case TipoOcorrencia of
    // tabela 1
    toRetornoRegistroRecusado,
    toRetornoEntradaRejeitadaCarne:
      case CodMotivo of
        03: Result := 'AG. COBRADORA -NÃO FOI POSSÍVEL ATRIBUIR A AGÊNCIA PELO CEP OU CEP INVÁLIDO';
        04: Result := 'ESTADO -SIGLA DO ESTADO INVÁLIDA';
        05: Result := 'DATA VENCIMENTO -PRAZO DA OPERAÇÃO MENOR QUE PRAZO MÍNIMO OU MAIOR QUE O MÁXIMO';
        07: Result := 'VALOR DO TÍTULO -VALOR DO TÍTULO MAIOR QUE 10.000.000,00';
        08: Result := 'NOME DO SACADO -NÃO INFORMADO OU DESLOCADO';
        09: Result := 'AGENCIA/CONTA -AGÊNCIA ENCERRADA';
        10: Result := 'LOGRADOURO -NÃO INFORMADO OU DESLOCADO';
        11: Result := 'CEP -CEP NÃO NUMÉRICO';
        12: Result := 'SACADOR / AVALISTA -NOME NÃO INFORMADO OU DESLOCADO (BANCOS CORRESPONDENTES)';
        13: Result := 'ESTADO/CEP -CEP INCOMPATÍVEL COM A SIGLA DO ESTADO';
        14: Result := 'NOSSO NÚMERO -NOSSO NÚMERO JÁ REGISTRADO NO CADASTRO DO BANCO OU FORA DA FAIXA';
        15: Result := 'NOSSO NÚMERO -NOSSO NÚMERO EM DUPLICIDADE NO MESMO MOVIMENTO';
        18: Result := 'DATA DE ENTRADA -DATA DE ENTRADA INVÁLIDA PARA OPERAR COM ESTA CARTEIRA';
        19: Result := 'OCORRÊNCIA -OCORRÊNCIA INVÁLIDA';
        21: Result := 'AG. COBRADORA - CARTEIRA NÃO ACEITA DEPOSITÁRIA CORRESPONDENTE/'
                      + 'ESTADO DA AGÊNCIA DIFERENTE DO ESTADO DO SACADO/' + 'AG. COBRADORA NÃO CONSTA NO CADASTRO OU ENCERRANDO';
        22: Result := 'CARTEIRA -CARTEIRA NÃO PERMITIDA (NECESSÁRIO CADASTRAR FAIXA LIVRE)';
        26: Result := 'AGÊNCIA/CONTA -AGÊNCIA/CONTA NÃO LIBERADA PARA OPERAR COM COBRANÇA';
        27: Result := 'CNPJ INAPTO -CNPJ DO CEDENTE INAPTO';
        29: Result := 'CÓDIGO EMPRESA -CATEGORIA DA CONTA INVÁLIDA';
        30: Result := 'ENTRADA BLOQUEADA -ENTRADAS BLOQUEADAS, CONTA SUSPENSA EM COBRANÇA';
        31: Result := 'AGÊNCIA/CONTA -CONTA NÃO TEM PERMISSÃO PARA PROTESTAR (CONTATE SEU GERENTE)';
        35: Result := 'VALOR DO IOF -IOF MAIOR QUE 5%';
        36: Result := 'QTDADE DE MOEDA -QUANTIDADE DE MOEDA INCOMPATÍVEL COM VALOR DO TÍTULO';
        37: Result := 'CNPJ/CPF DO SACADO -NÃO NUMÉRICO OU IGUAL A ZEROS';
        42: Result := 'NOSSO NÚMERO -NOSSO NÚMERO FORA DE FAIXA';
        52: Result := 'AG. COBRADORA -EMPRESA NÃO ACEITA BANCO CORRESPONDENTE';
        53: Result := 'AG. COBRADORA -EMPRESA NÃO ACEITA BANCO CORRESPONDENTE - COBRANÇA MENSAGEM';
        54: Result := 'DATA DE VENCTO -BANCO CORRESPONDENTE - TÍTULO COM VENCIMENTO INFERIOR A 15 DIAS';
        55: Result := 'DEP/BCO CORRESP -CEP NÃO PERTENCE À DEPOSITÁRIA INFORMADA';
        56: Result := 'DT VENCTO/BCO CORRESP -VENCTO SUPERIOR A 180 DIAS DA DATA DE ENTRADA';
        57: Result := 'DATA DE VENCTO -CEP SÓ DEPOSITÁRIA BCO DO BRASIL COM VENCTO INFERIOR A 8 DIAS';
        60: Result := 'ABATIMENTO -VALOR DO ABATIMENTO INVÁLIDO';
        61: Result := 'JUROS DE MORA -JUROS DE MORA MAIOR QUE O PERMITIDO';
        63: Result := 'DESCONTO DE ANTECIPAÇÃO -VALOR DA IMPORTÂNCIA POR DIA DE DESCONTO (IDD) NÃO PERMITIDO';
        64: Result := 'DATA DE EMISSÃO -DATA DE EMISSÃO DO TÍTULO INVÁLIDA';
        65: Result := 'TAXA FINANCTO -TAXA INVÁLIDA (VENDOR)';
        66: Result := 'DATA DE VENCTO -INVALIDA/FORA DE PRAZO DE OPERAÇÃO (MÍNIMO OU MÁXIMO)';
        67: Result := 'VALOR/QTIDADE -VALOR DO TÍTULO/QUANTIDADE DE MOEDA INVÁLIDO';
        68: Result := 'CARTEIRA -CARTEIRA INVÁLIDA';
        69: Result := 'CARTEIRA -CARTEIRA INVÁLIDA PARA TÍTULOS COM RATEIO DE CRÉDITO';
        70: Result := 'AGÊNCIA/CONTA -CEDENTE NÃO CADASTRADO PARA FAZER RATEIO DE CRÉDITO';
        78: Result := 'AGÊNCIA/CONTA -DUPLICIDADE DE AGÊNCIA/CONTA BENEFICIÁRIA DO RATEIO DE CRÉDITO';
        80: Result := 'AGÊNCIA/CONTA -QUANTIDADE DE CONTAS BENEFICIÁRIAS DO RATEIO MAIOR DO QUE O PERMITIDO (MÁXIMO DE 30 CONTAS POR TÍTULO)';
        81: Result := 'AGÊNCIA/CONTA -CONTA PARA RATEIO DE CRÉDITO INVÁLIDA / NÃO PERTENCE AO ITAÚ';
        82: Result := 'DESCONTO/ABATI-MENTO -DESCONTO/ABATIMENTO NÃO PERMITIDO PARA TÍTULOS COM RATEIO DE CRÉDITO';
        83: Result := 'VALOR DO TÍTULO -VALOR DO TÍTULO MENOR QUE A SOMA DOS VALORES ESTIPULADOS PARA RATEIO';
        84: Result := 'AGÊNCIA/CONTA -AGÊNCIA/CONTA BENEFICIÁRIA DO RATEIO É A CENTRALIZADORA DE CRÉDITO DO CEDENTE';
        85: Result := 'AGÊNCIA/CONTA -AGÊNCIA/CONTA DO CEDENTE É CONTRATUAL / RATEIO DE CRÉDITO NÃO PERMITIDO';
        86: Result := 'TIPO DE VALOR -CÓDIGO DO TIPO DE VALOR INVÁLIDO / NÃO PREVISTO PARA TÍTULOS COM RATEIO DE CRÉDITO';
        87: Result := 'AGÊNCIA/CONTA -REGISTRO TIPO 4 SEM INFORMAÇÃO DE AGÊNCIAS/CONTAS BENEFICIÁRIAS DO RATEIO';
        90: Result := 'NRO DA LINHA -COBRANÇA MENSAGEM - NÚMERO DA LINHA DA MENSAGEM INVÁLIDO';
        97: Result := 'SEM MENSAGEM -COBRANÇA MENSAGEM SEM MENSAGEM (SÓ DE CAMPOS FIXOS), PORÉM COM REGISTRO DO TIPO 7 OU 8';
        98: Result := 'FLASH INVÁLIDO -REGISTRO MENSAGEM SEM FLASH CADASTRADO OU FLASH INFORMADO DIFERENTE DO CADASTRADO';
        99: Result := 'FLASH INVÁLIDO -CONTA DE COBRANÇA COM FLASH CADASTRADO E SEM REGISTRO DE MENSAGEM CORRESPONDENTE';
        91: Result := 'DAC -DAC AGÊNCIA / CONTA CORRENTE INVÁLIDO';
        92: Result := 'DAC -DAC AGÊNCIA/CONTA/CARTEIRA/NOSSO NÚMERO INVÁLIDO';
        93: Result := 'ESTADO -SIGLA ESTADO INVÁLIDA';
        94: Result := 'ESTADO -SIGLA ESTADA INCOMPATÍVEL COM CEP DO SACADO';
        95: Result := 'CEP -CEP DO SACADO NÃO NUMÉRICO OU INVÁLIDO';
        96: Result := 'ENDEREÇO -ENDEREÇO / NOME / CIDADE SACADO INVÁLIDO';
        else
          Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;

    // tabela 2
    toRetornoAlteracaoDadosRejeitados:
      case CodMotivo of
        02: Result := 'AGÊNCIA COBRADORA INVÁLIDA OU COM O MESMO CONTEÚDO';
        04: Result := 'SIGLA DO ESTADO INVÁLIDA';
        05: Result := 'DATA DE VENCIMENTO INVÁLIDA OU COM O MESMO CONTEÚDO';
        06: Result := 'VALOR DO TÍTULO COM OUTRA ALTERAÇÃO SIMULTÂNEA';
        08: Result := 'NOME DO SACADO COM O MESMO CONTEÚDO';
        09: Result := 'AGÊNCIA/CONTA INCORRETA';
        11: Result := 'CEP INVÁLIDO';
        13: Result := 'SEU NÚMERO COM O MESMO CONTEÚDO';
        16: Result := 'ABATIMENTO/ALTERAÇÃO DO VALOR DO TÍTULO OU SOLICITAÇÃO DE BAIXA BLOQUEADA';
        21: Result := 'AGÊNCIA COBRADORA NÃO CONSTA NO CADASTRO DE DEPOSITÁRIA OU EM ENCERRAMENTO';
        53: Result := 'INSTRUÇÃO COM O MESMO CONTEÚDO';
        54: Result := 'DATA VENCIMENTO PARA BANCOS CORRESPONDENTES INFERIOR AO ACEITO PELO BANCO';
        55: Result := 'ALTERAÇÕES IGUAIS PARA O MESMO CONTROLE (AGÊNCIA/CONTA/CARTEIRA/NOSSO NÚMERO)';
        56: Result := 'CGC/CPF INVÁLIDO NÃO NUMÉRICO OU ZERADO';
        57: Result := 'PRAZO DE VENCIMENTO INFERIOR A 15 DIAS';
        60: Result := 'VALOR DE IOF - ALTERAÇÃO NÃO PERMITIDA PARA CARTEIRAS DE N.S. - MOEDA VARIÁVEL';
        61: Result := 'TÍTULO JÁ BAIXADO OU LIQUIDADO OU NÃO EXISTE TÍTULO CORRESPONDENTE NO SISTEMA';
        66: Result := 'ALTERAÇÃO NÃO PERMITIDA PARA CARTEIRAS DE NOTAS DE SEGUROS - MOEDA VARIÁVEL';
        81: Result := 'ALTERAÇÃO BLOQUEADA - TÍTULO COM PROTESTO';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 3
    toRetornoInstrucaoRejeitada:
      case CodMotivo of
        01: Result := 'INSTRUÇÃO/OCORRÊNCIA NÃO EXISTENTE';
        06: Result := 'NOSSO NÚMERO IGUAL A ZEROS';
        09: Result := 'CGC/CPF DO SACADOR/AVALISTA INVÁLIDO';
        10: Result := 'VALOR DO ABATIMENTO IGUAL OU MAIOR QUE O VALOR DO TÍTULO';
        14: Result := 'REGISTRO EM DUPLICIDADE';
        15: Result := 'CGC/CPF INFORMADO SEM NOME DO SACADOR/AVALISTA';
        21: Result := 'TÍTULO NÃO REGISTRADO NO SISTEMA';
        22: Result := 'TÍTULO BAIXADO OU LIQUIDADO';
        23: Result := 'INSTRUÇÃO NÃO ACEITA POR TER SIDO EMITIDO ÚLTIMO AVISO AO SACADO';
        24: Result := 'INSTRUÇÃO INCOMPATÍVEL - EXISTE INSTRUÇÃO DE PROTESTO PARA O TÍTULO';
        25: Result := 'INSTRUÇÃO INCOMPATÍVEL - NÃO EXISTE INSTRUÇÃO DE PROTESTO PARA O TÍTULO';
        26: Result := 'INSTRUÇÃO NÃO ACEITA POR TER SIDO EMITIDO ÚLTIMO AVISO AO SACADO';
        27: Result := 'INSTRUÇÃO NÃO ACEITA POR NÃO TER SIDO EMITIDA A ORDEM DE PROTESTO AO CARTÓRIO';
        28: Result := 'JÁ EXISTE UMA MESMA INSTRUÇÃO CADASTRADA ANTERIORMENTE PARA O TÍTULO';
        29: Result := 'VALOR LÍQUIDO + VALOR DO ABATIMENTO DIFERENTE DO VALOR DO TÍTULO REGISTRADO, OU VALOR'
                    + 'DO ABATIMENTO MAIOR QUE 90% DO VALOR DO TÍTULO';
        30: Result := 'EXISTE UMA INSTRUÇÃO DE NÃO PROTESTAR ATIVA PARA O TÍTULO';
        31: Result := 'EXISTE UMA OCORRÊNCIA DO SACADO QUE BLOQUEIA A INSTRUÇÃO';
        32: Result := 'DEPOSITÁRIA DO TÍTULO = 9999 OU CARTEIRA NÃO ACEITA PROTESTO';
        33: Result := 'ALTERAÇÃO DE VENCIMENTO IGUAL À REGISTRADA NO SISTEMA OU QUE TORNA O TÍTULO VENCIDO';
        34: Result := 'INSTRUÇÃO DE EMISSÃO DE AVISO DE COBRANÇA PARA TÍTULO VENCIDO ANTES DO VENCIMENTO';
        35: Result := 'SOLICITAÇÃO DE CANCELAMENTO DE INSTRUÇÃO INEXISTENTE';
        36: Result := 'TÍTULO SOFRENDO ALTERAÇÃO DE CONTROLE (AGÊNCIA/CONTA/CARTEIRA/NOSSO NÚMERO)';
        37: Result := 'INSTRUÇÃO NÃO PERMITIDA PARA A CARTEIRA';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 4
    toRetornoBaixaRejeitada:
      case CodMotivo of
        01: Result := 'CARTEIRA/Nº NÚMERO NÃO NUMÉRICO';
        04: Result := 'NOSSO NÚMERO EM DUPLICIDADE NUM MESMO MOVIMENTO';
        05: Result := 'SOLICITAÇÃO DE BAIXA PARA TÍTULO JÁ BAIXADO OU LIQUIDADO';
        06: Result := 'SOLICITAÇÃO DE BAIXA PARA TÍTULO NÃO REGISTRADO NO SISTEMA';
        07: Result := 'COBRANÇA PRAZO CURTO - SOLICITAÇÃO DE BAIXA P/ TÍTULO NÃO REGISTRADO NO SISTEMA';
        08: Result := 'SOLICITAÇÃO DE BAIXA PARA TÍTULO EM FLOATING';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 5
    toRetornoCobrancaContratual:
      case CodMotivo of
        16: Result := 'ABATIMENTO/ALTERAÇÃO DO VALOR DO TÍTULO OU SOLICITAÇÃO DE BAIXA BLOQUEADOS';
        40: Result := 'NÃO APROVADA DEVIDO AO IMPACTO NA ELEGIBILIDADE DE GARANTIAS';
        41: Result := 'AUTOMATICAMENTE REJEITADA';
        42: Result := 'CONFIRMA RECEBIMENTO DE INSTRUÇÃO – PENDENTE DE ANÁLISE';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 6
    toRetornoAlegacaoDoSacado:
      case CodMotivo of
        1313: Result := 'SOLICITA A PRORROGAÇÃO DO VENCIMENTO PARA';
        1321: Result := 'SOLICITA A DISPENSA DOS JUROS DE MORA';
        1339: Result := 'NÃO RECEBEU A MERCADORIA';
        1347: Result := 'A MERCADORIA CHEGOU ATRASADA';
        1354: Result := 'A MERCADORIA CHEGOU AVARIADA';
        1362: Result := 'A MERCADORIA CHEGOU INCOMPLETA';
        1370: Result := 'A MERCADORIA NÃO CONFERE COM O PEDIDO';
        1388: Result := 'A MERCADORIA ESTÁ À DISPOSIÇÃO';
        1396: Result := 'DEVOLVEU A MERCADORIA';
        1404: Result := 'NÃO RECEBEU A FATURA';
        1412: Result := 'A FATURA ESTÁ EM DESACORDO COM A NOTA FISCAL';
        1420: Result := 'O PEDIDO DE COMPRA FOI CANCELADO';
        1438: Result := 'A DUPLICATA FOI CANCELADA';
        1446: Result := 'QUE NADA DEVE OU COMPROU';
        1453: Result := 'QUE MANTÉM ENTENDIMENTOS COM O SACADOR';
        1461: Result := 'QUE PAGARÁ O TÍTULO EM:';
        1479: Result := 'QUE PAGOU O TÍTULO DIRETAMENTE AO CEDENTE EM:';
        1487: Result := 'QUE PAGARÁ O TÍTULO DIRETAMENTE AO CEDENTE EM:';
        1495: Result := 'QUE O VENCIMENTO CORRETO É:';
        1503: Result := 'QUE TEM DESCONTO OU ABATIMENTO DE:';
        1719: Result := 'SACADO NÃO FOI LOCALIZADO; CONFIRMAR ENDEREÇO';
        1727: Result := 'SACADO ESTÁ EM REGIME DE CONCORDATA';
        1735: Result := 'SACADO ESTÁ EM REGIME DE FALÊNCIA';
        1750: Result := 'SACADO SE RECUSA A PAGAR JUROS BANCÁRIOS';
        1768: Result := 'SACADO SE RECUSA A PAGAR COMISSÃO DE PERMANÊNCIA';
        1776: Result := 'NÃO FOI POSSÍVEL A ENTREGA DO BLOQUETO AO SACADO';
        1784: Result := 'BLOQUETO NÃO ENTREGUE, MUDOU-SE/DESCONHECIDO';
        1792: Result := 'BLOQUETO NÃO ENTREGUE, CEP ERRADO/INCOMPLETO';
        1800: Result := 'BLOQUETO NÃO ENTREGUE, NÚMERO NÃO EXISTE/ENDEREÇO INCOMPLETO';
        1818: Result := 'BLOQUETO NÃO RETIRADO PELO SACADO. REENVIADO PELO CORREIO';
        1826: Result := 'ENDEREÇO DE E-MAIL INVÁLIDO. BLOQUETO ENVIADO PELO CORREIO';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 7
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente:
      case CodMotivo of
        1610: Result := 'DOCUMENTAÇÃO SOLICITADA AO CEDENTE';
        3111: Result := 'SUSTAÇÃO SOLICITADA AG. CEDENTE';
        3228: Result := 'ATOS DA CORREGEDORIA ESTADUAL';
        3244: Result := 'PROTESTO SUSTADO / CEDENTE NÃO ENTREGOU A DOCUMENTAÇÃO';
        3269: Result := 'DATA DE EMISSÃO DO TÍTULO INVÁLIDA/IRREGULAR';
        3301: Result := 'CGC/CPF DO SACADO INVÁLIDO/INCORRETO';
        3319: Result := 'SACADOR/AVALISTA E PESSOA FÍSICA';
        3327: Result := 'CEP DO SACADO INCORRETO';
        3335: Result := 'DEPOSITÁRIA INCOMPATÍVEL COM CEP DO SACADO';
        3343: Result := 'CGC/CPF SACADOR INVALIDO/INCORRETO';
        3350: Result := 'ENDEREÇO DO SACADO INSUFICIENTE';
        3368: Result := 'PRAÇA PAGTO INCOMPATÍVEL COM ENDEREÇO';
        3376: Result := 'FALTA NÚMERO/ESPÉCIE DO TÍTULO';
        3384: Result := 'TÍTULO ACEITO S/ ASSINATURA DO SACADOR';
        3392: Result := 'TÍTULO ACEITO S/ ENDOSSO CEDENTE OU IRREGULAR';
        3400: Result := 'TÍTULO SEM LOCAL OU DATA DE EMISSÃO';
        3418: Result := 'TÍTULO ACEITO COM VALOR EXTENSO DIFERENTE DO NUMÉRICO';
        3426: Result := 'TÍTULO ACEITO DEFINIR ESPÉCIE DA DUPLICATA';
        3434: Result := 'DATA EMISSÃO POSTERIOR AO VENCIMENTO';
        3442: Result := 'TÍTULO ACEITO DOCUMENTO NÃO PROSTESTÁVEL';
        3459: Result := 'TÍTULO ACEITO EXTENSO VENCIMENTO IRREGULAR';
        3467: Result := 'TÍTULO ACEITO FALTA NOME FAVORECIDO';
        3475: Result := 'TÍTULO ACEITO FALTA PRAÇA DE PAGAMENTO';
        3483: Result := 'TÍTULO ACEITO FALTA CPF ASSINANTE CHEQUE';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 8
    toRetornoInstrucaoCancelada:
      case CodMotivo of
        1156: Result := 'NÃO PROTESTAR';
        2261: Result := 'DISPENSAR JUROS/COMISSÃO DE PERMANÊNCIA';
        else
          Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;

    // tabela 9
    toRetornoChequeDevolvido:
      case CodMotivo of
        11: Result := 'CHEQUE SEM FUNDOS - PRIMEIRA APRESENTAÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        12: Result := 'CHEQUE SEM FUNDOS - SEGUNDA APRESENTAÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO ';
        13: Result := 'CONTA ENCERRADA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        14: Result := 'PRÁTICA ESPÚRIA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        20: Result := 'FOLHA DE CHEQUE CANCELADA POR SOLICITAÇÃO DO CORRENTISTA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        21: Result := 'CONTRA-ORDEM (OU REVOGAÇÃO) OU OPOSIÇÃO (OU SUSTAÇÃO) AO PAGAMENTO PELO EMITENTE OU PELO '
                      + 'PORTADOR - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        22: Result := 'DIVERGÊNCIA OU INSUFICIÊNCIA DE ASSINATURAb - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        23: Result := 'CHEQUES EMITIDOS POR ENTIDADES E ÓRGÃOS DA ADMINISTRAÇÃO PÚBLICA FEDERAL DIRETA E INDIRETA, '
                      + 'EM DESACORDO COM OS REQUISITOS CONSTANTES DO ARTIGO 74, § 2º, DO DECRETO-LEI Nº 200, DE 25.02.1967. - '
                      + 'PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        24: Result := 'BLOQUEIO JUDICIAL OU DETERMINAÇÃO DO BANCO CENTRAL DO BRASIL - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        25: Result := 'CANCELAMENTO DE TALONÁRIO PELO BANCO SACADO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        28: Result := 'CONTRA-ORDEM (OU REVOGAÇÃO) OU OPOSIÇÃO (OU SUSTAÇÃO) AO PAGAMENTO OCASIONADA POR FURTO OU ROUBO - '
                      + 'PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        29: Result := 'CHEQUE BLOQUEADO POR FALTA DE CONFIRMAÇÃO DO RECEBIMENTO DO TALONÁRIO PELO CORRENTISTA - '
                      + 'PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        30: Result := 'FURTO OU ROUBO DE MALOTES - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        31: Result := 'ERRO FORMAL (SEM DATA DE EMISSÃO, COM O MÊS GRAFADO NUMERICAMENTE, AUSÊNCIA DE ASSINATURA, '
                      + 'NÃO-REGISTRO DO VALOR POR EXTENSO) - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        32: Result := 'AUSÊNCIA OU IRREGULARIDADE NA APLICAÇÃO DO CARIMBO DE COMPENSAÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        33: Result := 'DIVERGÊNCIA DE ENDOSSO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        34: Result := 'CHEQUE APRESENTADO POR ESTABELECIMENTO BANCÁRIO QUE NÃO O INDICADO NO CRUZAMENTO EM PRETO, SEM O '
                      + 'ENDOSSO-MANDATO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        35: Result := 'CHEQUE FRAUDADO, EMITIDO SEM PRÉVIO CONTROLE OU RESPONSABILIDADE DO ESTABELECIMENTO BANCÁRIO '
                      + '("CHEQUE UNIVERSAL"), OU AINDA COM ADULTERAÇÃO DA PRAÇA SACADA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        36: Result := 'CHEQUE EMITIDO COM MAIS DE UM ENDOSSO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        40: Result := 'MOEDA INVÁLIDA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        41: Result := 'CHEQUE APRESENTADO A BANCO QUE NÃO O SACADO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        42: Result := 'CHEQUE NÃO-COMPENSÁVEL NA SESSÃO OU SISTEMA DE COMPENSAÇÃO EM QUE FOI APRESENTADO - '
                      + 'PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        43: Result := 'CHEQUE, DEVOLVIDO ANTERIORMENTE PELOS MOTIVOS 21, 22, 23, 24, 31 OU 34, NÃO-PASSÍVEL '
                      + 'DE REAPRESENTAÇÃO EM VIRTUDE DE PERSISTIR O MOTIVO DA DEVOLUÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        44: Result := 'CHEQUE PRESCRITO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        45: Result := 'CHEQUE EMITIDO POR ENTIDADE OBRIGADA A REALIZAR MOVIMENTAÇÃO E UTILIZAÇÃO DE RECURSOS FINANCEIROS '
                      + 'DO TESOURO NACIONAL MEDIANTE ORDEM BANCÁRIA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        48: Result := 'CHEQUE DE VALOR SUPERIOR AO ESTABELECIDO, EMITIDO SEM A IDENTIFICAÇÃO DO BENEFICIÁRIO, DEVENDO SER '
                      + 'DEVOLVIDO A QUALQUER TEMPO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
        49: Result := 'REMESSA NULA, CARACTERIZADA PELA REAPRESENTAÇÃO DE CHEQUE DEVOLVIDO PELOS MOTIVOS 12, 13, 14, 20, '
                      + '25, 28, 30, 35, 43, 44 E 45, PODENDO A SUA DEVOLUÇÃO OCORRER A QUALQUER TEMPO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;

    // tabela 10
    toRetornoRegistroConfirmado:
      case CodMotivo of
        01: Result := 'CEP SEM ATENDIMENTO DE PROTESTO NO MOMENTO';
        else
          Result := IntToStrZero(CodMotivo, 2)
                    + ' - Outros Motivos';
      end;
    else
      Result := IntToStrZero(CodMotivo, 2)
                + ' - Outros Motivos';
  end;
end;

function TACBrBancoInter.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRemessaBaixar; { Pedido de Baixa }
    04: Result := toRemessaConcederAbatimento; { Concessão de Abatimento }
    05: Result := toRemessaCancelarAbatimento; { Cancelamento de Abatimento concedido }
    06: Result := toRemessaAlterarVencimento; { Alteração de vencimento }
    07: Result := toRemessaAlterarUsoEmpresa; { Alteração do uso Da Empresa }
    08: Result := toRemessaAlterarSeuNumero; { Alteração do seu Número }
    09: Result := toRemessaProtestar; { Protestar (emite aviso ao sacado após xx dias do vencimento, e envia ao cartório após 5 dias úteis) }
    10: Result := toRemessaCancelarInstrucaoProtesto; { Sustar Protesto }
    11: Result := toRemessaProtestoFinsFalimentares; { Protesto para fins Falimentares }
    18: Result := toRemessaCancelarInstrucaoProtestoBaixa; { Sustar protesto e baixar }
    30: Result := toRemessaExcluirSacadorAvalista; { Exclusão de Sacador Avalista }
    31: Result := toRemessaOutrasAlteracoes; { Alteração de Outros Dados }
    34: Result := toRemessaBaixaporPagtoDiretoCedente; { Baixa por ter sido pago Diretamente ao Cedente }
    35: Result := toRemessaCancelarInstrucao; { Cancelamento de Instrução }
    37: Result := toRemessaAlterarVencimentoSustarProtesto; { Alteração do Vencimento e Sustar Protesto }
    38: Result := toRemessaCedenteDiscordaSacado; { Cedente não Concorda com Alegação do Sacado }
    47: Result := toRemessaCedenteSolicitaDispensaJuros; { Cedente Solicita Dispensa de Juros }
    else
      Result := toRemessaRegistrar; { Remessa }
  end;
end;

end.
