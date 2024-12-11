{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Samuel "Muka" David                             }
{                Victor H. Gonzales - Pandaaa                                  }
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
//INCLUIDO EM 11/12/2024

{$I ACBr.inc}
unit ACBrBoletoW_Kobana;

interface

uses
  SysUtils,
  Classes,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoWS.REST,
  ACBrBoletoConversao,
  ACBrBoletoRetorno,
  ACBrBoleto.Kobana.Classes;

type
  TBoletoW_Kobana = class(TBoletoWSREST)
  private
    FProducao: Boolean;
    procedure RequiscaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure BuscarDadosCarteira;

    function GetBaseURL(pCommand: string = ''): string;
    function ACBrTituloToBankBillet(pACBrTitulo: TACBrTitulo): TBankBillet;
    function GetBoletoID: Integer;
    function TratarRetornoBoletoID: Integer;
    function GetTokenAutenticacao: string;
    function DefinirParametros: string;
    function EncontrarBoleto: Boolean;
    function DefinirAceite(const ACBrTitulo: TACBrTitulo): string;
    function DefinirCodigoMoraJuros(const ACBrTitulo: TACBrTitulo): Integer;
    function DefinirTipoJurosMora(const ACBrTitulo: TACBrTitulo): Integer;
    function DefinirCodigoMulta(const ACBrTitulo: TACBrTitulo): Integer;
    function DefinirCaracTitulo(const ACBrTitulo: TACBrTitulo): Integer;
    function DefinirTipoCobranca(const ACBrTitulo: TACBrTitulo): Integer;
    function DefinirEspecieDoc(const ACBrTitulo: TACBrTitulo): string;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure DefinirAuthorization; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
  public
    function GerarRemessa: String; override;
    function Enviar: Boolean; override;

    constructor Create(ABoletoWS: TBoletoWS); override;
    destructor Destroy; override;
  end;

implementation

uses
  ACBrJSON,
  ACBrUtil.Base,
  pcnConversao,
  ACBrUtil.Strings;

const
  C_ACCEPT = 'application/json';
  C_CONTENT_TYPE = 'application/json';
  BASE_URL_HOM = 'https://api-sandbox.kobana.com.br';
  BASE_URL_PRD = 'https://api.kobana.com.br';
  BANK_BILLETS = '/v1/bank_billets';

resourcestring
  C_CANCELADO = 'CANCELADO';
  C_BAIXADO = 'BAIXADO';
  C_EXPIRADO = 'EXPIRADO';
  C_VENCIDO = 'VENCIDO';
  C_EMABERTO = 'EM ABERTO';
  C_PAGO = 'Liquidado';

  { TBoletoW_Kobana }

function TBoletoW_Kobana.ACBrTituloToBankBillet(pACBrTitulo: TACBrTitulo): TBankBillet;
begin
  Result := TBankBillet.Create;

  Result.Bank_Billet_Account_ID := StrToIntDef(Boleto.Cedente.CedenteWS.KeyUser,0);
  Result.Amount := pACBrTitulo.ValorDocumento;
  Result.Expire_At := pACBrTitulo.Vencimento;
  Result.Interest_type := 0; //DefinirCodigoMoraJuros(pACBrTitulo); //.CodigoMoraJuros;
  Result.Interest_Days_Type := DefinirTipoJurosMora(pACBrTitulo); //.TipoDiasNegativacao;

  Result.Fine_Type := DefinirCodigoMulta(pACBrTitulo); //.CodigoMulta;
  Result.Charge_type := DefinirCaracTitulo(pACBrTitulo); //.CaracTitulo;
  Result.Dispatch_type := DefinirTipoCobranca(pACBrTitulo); //.CateiraEnvio;
  Result.Document_Type := DefinirEspecieDoc(pACBrTitulo); //.EspecieDoc
  Result.Acceptance := DefinirAceite(pACBrTitulo); // .Aceite;
  Result.Ignore_email := False;
  Result.Ignore_sms := False;
  Result.Ignore_whatsapp := False;
  Result.Instructions_mode := 1;
  Result.Customer_Person_Name := pACBrTitulo.Sacado.NomeSacado;
  Result.Customer_Cnpj_Cpf := pACBrTitulo.Sacado.CNPJCPF;
  Result.Customer_State := pACBrTitulo.Sacado.UF;
  Result.Customer_City_Name := pACBrTitulo.Sacado.Cidade;
  Result.Customer_Zipcode := pACBrTitulo.Sacado.CEP;
  Result.Customer_Address := pACBrTitulo.Sacado.Logradouro;
  Result.Customer_address_complement := pACBrTitulo.Sacado.Complemento;
  Result.Customer_Address_Number := pACBrTitulo.Sacado.Numero;
  Result.Customer_Neighborhood := pACBrTitulo.Sacado.Bairro;
  Result.Customer_email := pACBrTitulo.Sacado.Email;
  Result.Customer_email_cc := pACBrTitulo.Sacado.Email;
  Result.Our_number := pACBrTitulo.NossoNumero;
  Result.Instructions := pACBrTitulo.Mensagem.Text;
  Result.Document_date := pACBrTitulo.DataDocumento;
  Result.Document_number := pACBrTitulo.NumeroDocumento;

  if Boleto.Cedente.CedenteWS.IndicadorPix then
  begin
    Result.Pix_txid := Boleto.Cedente.PIX.Chave;
    Result.Prevent_Pix := False;
  end;

  if Result.Interest_type = 1 then
    Result.Interest_Percentage := 0.0;

  Result.Prevent_registration := Boleto.Homologacao;
end;

constructor TBoletoW_Kobana.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  DefinirContentType;
  FPAccept := C_ACCEPT;
end;

function TBoletoW_Kobana.DefinirAceite(const ACBrTitulo: TACBrTitulo): string;
begin
  case ACBrTitulo.Aceite of
    atSim:
      Result := 'A';
  else
    Result := 'N';
  end;
end;

function TBoletoW_Kobana.DefinirCaracTitulo(const ACBrTitulo: TACBrTitulo): Integer;
begin
  case ACBrTitulo.CaracTitulo of
    tcVinculada:
      Result := 2;
    tcCaucionada:
      Result := 3;
    tcDescontada:
      Result := 4;
    tcVendor:
      Result := 5;
  else
      Result := 1;
  end;
end;

function TBoletoW_Kobana.DefinirCodigoMoraJuros(const ACBrTitulo: TACBrTitulo): Integer;
begin
  if ACBrTitulo.CodigoMora <> '' then
    Result := StrToIntDef(ACBrTitulo.CodigoMora,0)
  else
  begin
    if ACBrTitulo.ValorMoraJuros > 0 then
    begin
      case ACBrTitulo.CodigoMoraJuros of
        cjTaxaMensal, cjValorMensal:
          Result := 2;
      else
        Result := 1;
      end;
    end
    else
      Result := 3;
  end
end;

function TBoletoW_Kobana.DefinirCodigoMulta(const ACBrTitulo: TACBrTitulo): Integer;
begin
  if ACBrTitulo.PercentualMulta > 0 then
    Result := 2
  else
    Result := 0
end;

function TBoletoW_Kobana.DefinirEspecieDoc(const ACBrTitulo: TACBrTitulo): string;
begin
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CH') then
    Result := '01'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DM') then
    Result := '02'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DMI') then
    Result := '03'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DS') then
    Result := '04'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DSI') then
    Result := '05'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DR') then
    Result := '06'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'LC') then
    Result := '07'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NCC') then
    Result := '08'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NCE') then
    Result := '09'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NCI') then
    Result := '10'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NCR') then
    Result := '11'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NP') then
    Result := '12'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NPR') then
    Result := '13'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'TM') then
    Result := '14'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'TS') then
    Result := '15'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NS') then
    Result := '16'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'RC') then
    Result := '17'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'FAT') then
    Result := '18'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'ND') then
    Result := '19'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'AP') then
    Result := '20'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'ME') then
    Result := '21'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'PC') then
    Result := '22'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NF') then
    Result := '23'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DD') then
    Result := '24'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CPR') then
    Result := '25'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CTR') then
    Result := '26'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CSG') then
    Result := '27'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'EC') then
    Result := '28'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CPS') then
    Result := '29'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'WR') then
    Result := '30'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DP') then
    Result := '31'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CSR') then
    Result := '32'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CAR') then
    Result := '33'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'ARE') then
    Result := '34'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CC') then
    Result := '35'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'BDP') then
    Result := '36'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'NPD') then
    Result := '37'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DAE') then
    Result := '38'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DAM') then
    Result := '39'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DAU') then
    Result := '40'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'CCB') then
    Result := '41'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'FI') then
    Result := '42'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'RD') then
    Result := '43'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'DRI') then
    Result := '44'
  else if AnsiSameText(ACBrTitulo.EspecieDoc, 'ECI') then
    Result := '45'
  else
    Result := '99';
end;

function TBoletoW_Kobana.DefinirTipoCobranca(const ACBrTitulo: TACBrTitulo): Integer;
begin
  case ACBrTitulo.CarteiraEnvio of
    tceCedente:
      Result := 1;
    tceBanco:
      Result := 2;
    tceBancoEmail:
      Result := 3;
  else
    Result := 1;
  end
end;

function TBoletoW_Kobana.DefinirTipoJurosMora(const ACBrTitulo: TACBrTitulo): Integer;
begin
  case ACBrTitulo.TipoDiasNegativacao of
    diCorridos:
      Result := 0;
    diUteis:
      Result := 1;
  else
    Result := 0;
  end
end;

procedure TBoletoW_Kobana.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GetTokenAutenticacao;
end;

procedure TBoletoW_Kobana.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

function TBoletoW_Kobana.DefinirParametros: String;
var
  lConsulta: TStringList;
  lCNPJ_CPF: string;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    lConsulta := TStringList.Create;
    lConsulta.Delimiter := '&';
    try
      if Assigned(ATitulo) then
      begin
        if (OnlyNumber(ATitulo.NossoNumero) <> '00000000') and (OnlyNumber(ATitulo.NossoNumero) <> EmptyStr) then
          lConsulta.Add(Format('our_number=%s', [RemoveZerosEsquerda(OnlyNumber(ATitulo.NossoNumero))]));

        if OnlyNumber(ATitulo.Sacado.CNPJCPF) <> EmptyStr then
        begin
          lCNPJ_CPF := StringReplace(ATitulo.Sacado.CNPJCPF, '/', '%2F', [rfReplaceAll]);
          lConsulta.Add(Format('cnpj_cpf=%s', [lCNPJ_CPF]));
        end;
      end else begin
        lConsulta.Add(Format('updated_from=%s', [FormatDateTime('YYYY-MM-DD',Date-1)]));
      end;

      if StrToIntDef(Boleto.Cedente.CedenteWS.KeyUser,0) <> 0 then
        lConsulta.Add(Format('bank_billet_account_id=%s', [Boleto.Cedente.CedenteWS.KeyUser]));

      lConsulta.Add(Format('per_page=%s', ['50']));
    finally
      Result := lConsulta.DelimitedText;
      lConsulta.Free;
    end;
  end;
end;

procedure TBoletoW_Kobana.DefinirURL;
begin
  FProducao := Boleto.Configuracoes.WebService.Ambiente = tawsProducao;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      FPURL := GetBaseURL(BANK_BILLETS);
    tpAltera:
      begin
        if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaOutrasAlteracoes then
          FPURL := FPURL + GetBaseURL(BANK_BILLETS) + '/' + IntToStr(GetBoletoID);

        if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaBaixaporPagtoDiretoCedente then
          FPURL := FPURL + GetBaseURL(BANK_BILLETS) + '/' + IntToStr(GetBoletoID) + '/pay';
      end;
    tpConsulta:
      begin
        FPURL := GetBaseURL(BANK_BILLETS) + '?' + DefinirParametros;
        FMetodoHTTP := htGET;
      end;
    tpConsultaDetalhe:
      begin
        FPURL := GetBaseURL(BANK_BILLETS) + '/' + IntToStr(GetBoletoID);
        FMetodoHTTP := htGET;
      end;
    tpBaixa:
      FPURL := FPURL + GetBaseURL(BANK_BILLETS) + '/' + IntToStr(GetBoletoID) + '/cancel';
  end;
end;

destructor TBoletoW_Kobana.Destroy;
begin
  inherited;
end;

function TBoletoW_Kobana.EncontrarBoleto: Boolean;
var
  lConsultaBoletoKobana: TConsultaBoleto;
begin
  lConsultaBoletoKobana := TConsultaBoleto.Create(Boleto, ATitulo, GetBaseURL);
  try
    case Boleto.Configuracoes.WebService.Operacao of
      tpInclui, tpAltera:
        Result := lConsultaBoletoKobana.EncontrouBoleto;
    else
      raise EACBrBoletoWSException.Create(ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
        [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
    end;
  finally
    lConsultaBoletoKobana.Free;
  end;
end;

function TBoletoW_Kobana.Enviar: Boolean;
begin
  Result := False;
  if Boleto.Configuracoes.WebService.Operacao in [tpInclui, tpAltera] then
  begin
    if not(EncontrarBoleto) then
      Result := inherited Enviar;
  end else
    Result := inherited Enviar;
end;

procedure TBoletoW_Kobana.GerarDados;
begin
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        FMetodoHTTP := htPOST;
        RequiscaoJson;
      end;
    tpAltera:
      begin
        FMetodoHTTP := htPUT;
        RequisicaoAltera;
      end;
    tpBaixa:
      begin
        FMetodoHTTP := htPUT;
      end;
    tpConsulta:
      begin
        FMetodoHTTP := htGET;
        RequisicaoConsulta;
      end;
    tpConsultaDetalhe:
      begin
        FMetodoHTTP := htGET;
        RequisicaoConsultaDetalhe;
      end;
  else
    raise EACBrBoletoWSException.Create(ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
      [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
  end;
end;

procedure TBoletoW_Kobana.GerarHeader;
begin
  ClearHeaderParams;
  if (Boleto.Cedente.CedenteWS.ClientID <> EmptyStr) then
    AddHeaderParam('X_ACCOUNT_UID',Boleto.Cedente.CedenteWS.ClientID);

  DefinirContentType;
end;

function TBoletoW_Kobana.GerarRemessa: String;
begin
  BuscarDadosCarteira;
  Result := inherited GerarRemessa;
end;

function TBoletoW_Kobana.GetTokenAutenticacao: string;
begin
  Result := Boleto.Cedente.CedenteWS.ClientSecret;
end;

function TBoletoW_Kobana.GetBaseURL(pCommand: string): string;
begin
  if FProducao then
    Result := BASE_URL_PRD
  else
    Result := BASE_URL_HOM;

  Result := Result + pCommand;
end;

function TBoletoW_Kobana.GetBoletoID: Integer;
var
  lOperacaoOrigemInicial: TOperacao;
  lDadosMsgOrigemInicial: string;
  lMetodoHTTPOrigemInicial: TMetodoHTTP;
begin
  lOperacaoOrigemInicial := Boleto.Configuracoes.WebService.Operacao;
  lDadosMsgOrigemInicial := FPDadosMsg;
  lMetodoHTTPOrigemInicial := FMetodoHTTP;

  FPDadosMsg := '';
  FMetodoHTTP := htGET;
  Boleto.Configuracoes.WebService.Operacao := tpConsulta;

  inherited Enviar;

  Result := TratarRetornoBoletoID;

  FRetornoWS := '';
  Boleto.Configuracoes.WebService.Operacao := lOperacaoOrigemInicial;
  FPDadosMsg := lDadosMsgOrigemInicial;
  FMetodoHTTP := lMetodoHTTPOrigemInicial;
end;

procedure TBoletoW_Kobana.RequiscaoJson;
var
  lACBrTituloToBankBillet: TBankBillet;
  lJson: string;
begin
  if Assigned(ATitulo) then
  begin
    lACBrTituloToBankBillet := ACBrTituloToBankBillet(ATitulo);
    try
      lJson := lACBrTituloToBankBillet.ToJsonString;
      FPDadosMsg := lJson;
    finally
      lACBrTituloToBankBillet.Free;
    end;
  end;
end;

procedure TBoletoW_Kobana.RequisicaoAltera;
var
  lJson: TACBrJSONObject;
  lInstrucoes: TStringList;
begin
  if not Assigned(ATitulo) then
    Exit;

  lJson := TACBrJSONObject.Create;
  try
    if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaOutrasAlteracoes then
    begin
      if ATitulo.ValorDocumento > 0 then
        lJson.AddPair('amount', ATitulo.ValorDocumento);

      if ATitulo.Vencimento > 0 then
        lJson.AddPair('expire_at', FormatDateTime('yyyy-mm-dd', ATitulo.Vencimento));

      if ATitulo.DiasDeProtesto > 0 then
        lJson.AddPair('days_for_sue', ATitulo.DiasDeProtesto);

      lInstrucoes := TStringList.Create;
      try
        lInstrucoes.Add(ATitulo.Instrucao1);
        lInstrucoes.Add(ATitulo.Instrucao2);
        lInstrucoes.Add(ATitulo.Instrucao3);
        lInstrucoes.Add(ATitulo.Instrucao4);
        lInstrucoes.Add(ATitulo.Instrucao5);

        if NaoEstaVazio(lInstrucoes.Text) then
          lJson.AddPair('instructions', lInstrucoes.Text);
      finally
        lInstrucoes.Free;
      end;

      if ATitulo.ValorAbatimento > 0 then
        lJson.AddPair('reduction_amount', ATitulo.ValorAbatimento);
    end
    else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaBaixaporPagtoDiretoCedente then
    begin
      if ATitulo.ValorPago > 0 then
        lJson.AddPair('paid_amount', ATitulo.ValorPago);

      if ATitulo.DataCredito > 0 then
        lJson.AddPair('paid_at', FormatDateTime('yyyy-mm-dd', ATitulo.DataCredito));
    end;

    if NaoEstaVazio(lJson.ToJSON) then
      FPDadosMsg := lJson.ToJSON;

  finally
    lJson.Free;
  end;
end;

procedure TBoletoW_Kobana.RequisicaoConsulta;
begin
  DefinirParametros;
end;

procedure TBoletoW_Kobana.RequisicaoConsultaDetalhe;
begin
  DefinirParametros;
end;

procedure TBoletoW_Kobana.BuscarDadosCarteira;
var
  lRetornoDadosCarteiraKobana: TRetornoCarteira;
begin
  FProducao := Boleto.Configuracoes.WebService.Ambiente = tawsProducao;
  lRetornoDadosCarteiraKobana := TRetornoCarteira.Create(Boleto, GetBaseURL);
  try
    Boleto.Banco.TipoCobranca := Boleto.GetTipoCobranca(Boleto.Banco.Numero);

    Boleto.Cedente.CedenteWS.KeyUser := IntToStr(lRetornoDadosCarteiraKobana.IDCarteira);
    Boleto.Cedente.CedenteWS.ClientID := lRetornoDadosCarteiraKobana.UIDConta;
  finally
    lRetornoDadosCarteiraKobana.Free;
  end;
end;

function TBoletoW_Kobana.TratarRetornoBoletoID: Integer;
var
  LACBrJSONArray, lJSONArrayError: TACBrJSONArray;
  LACBrJSONObject: TACBrJSONObject;
begin
  Result := 0;

  LACBrJSONArray := TACBrJSONArray.Parse(FRetornoWS);

  try
    if not Assigned(LACBrJSONArray) then
    begin
      raise Exception.CreateFmt(
        'Não foi possível validar o JSON de retorno para o método chamado: URL=%s ' + #1310 +
        'Requisição=%s ' + #1013 + 'Headers:%s' + #1013 + 'BodyEnvio:%s' + #1013 + 'JSONRetornado:%s',
        [FPURL, MetodoHTTPToStr(FMetodoHTTP), Headers.Text, FPDadosMsg, FRetornoWS]);
    end;

    LACBrJSONObject := LACBrJSONArray.ItemAsJSONObject[0];

    if LACBrJSONObject.ValueExists('id') then
      Result :=  LACBrJSONObject.AsInteger['id']
    else
      raise Exception.Create('Não foi possível encontrar o ID do boleto');

  finally
    LACBrJSONObject.Free;
  end;
end;


end.
