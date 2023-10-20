{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Marcelo Santos,  }
{ Delmar de Lima                                                               }
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

{$I ACBr.inc}

unit ACBrBoletoW_Bancoob;

interface

uses
  Classes,
  SysUtils,
  pcnConversao,
  synacode,
  strutils,
  DateUtils,
  ACBrDFeSSL,
  synautil,
  ACBrBoleto,
  Jsons,
  httpsend,
  ACBrBoletoConversao,
  ACBrBoletoWS,
  ACBrBoletoWS.Rest,
  Math;

type

  { TBoletoW_Bancoob}
  TBoletoW_Bancoob = class(TBoletoWSREST)
  private
    function DateBancoobtoDateTime(const AValue: String): TDateTime;
    function DateTimeToDateBancoob( const AValue:TDateTime ):String;
    procedure GerarInstrucao(AJson: TJsonObject);
    procedure AlterarEspecie(AJson: TJsonObject);
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    function GerarTokenAutenticacao: string; override;
    function DefinirParametros: String;
    procedure DefinirParamOAuth; override;
    procedure DefinirKeyUser;
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsultaDetalhe;
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarBenificiarioFinal(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure AlteraDataVencimento(AJson: TJsonObject);
    procedure AtribuirDesconto(AJson: TJsonObject);
    procedure AlteracaoDesconto(AJson: TJsonObject);
    procedure AlterarProtesto(AJson: TJsonObject);
    procedure AtribuirAbatimento(AJson: TJsonObject);
    procedure AtribuirJuros(AJson: TJsonObject);
    procedure AtribuirMulta(AJson: TJsonObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;
  end;

const
  C_URL             = 'https://api.sicoob.com.br/cobranca-bancaria/v2';
  C_URL_HOM         = 'https://sandbox.sicoob.com.br/sicoob/sandbox/cobranca-bancaria/v2';

  C_URL_OAUTH_PROD  = 'https://auth.sicoob.com.br/auth/realms/cooperado/protocol/openid-connect/token';
  C_URL_OAUTH_HOM   = 'https://auth.sicoob.com.br/auth/realms/cooperado/protocol/openid-connect/token';

  C_CONTENT_TYPE    = 'application/json';
  C_ACCEPT          = 'application/json';
  C_AUTHORIZATION   = 'Authorization';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = 'utf-8';
  C_SICOOB_CLIENT   = 'client_id';

  C_ACCESS_TOKEN_HOM = '1301865f-c6bc-38f3-9f49-666dbcfc59c3';

implementation

uses
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Base, ACBrJSON, pcnAuxiliar;

{ TBoletoW_Bancoob}

procedure TBoletoW_Bancoob.DefinirURL;
var
  LNossoNumero, LContrato: string;
begin

  if( aTitulo <> nil ) then
  begin
    LNossoNumero := ACBrUtil.Strings.RemoveZerosEsquerda(OnlyNumber(aTitulo.NossoNumero)+aTitulo.ACBrBoleto.Banco.CalcularDigitoVerificador(aTitulo));
    LContrato    := OnlyNumber(aTitulo.ACBrBoleto.Cedente.CodigoCedente);
  end;
  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL,C_URL_HOM);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:  FPURL := FPURL + '/boletos';
    tpAltera:
    begin
       if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaBaixar then
         FPURL := FPURL + '/boletos/baixa'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaConcederDesconto then
         FPURL := FPURL + '/boletos/descontos'
       else if aTitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarVencimento then
         FPURL := FPURL + '/boletos/prorrogacoes/data-vencimento'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaProtestar then
         FPURL := FPURL + '/boletos/protestos'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaSustarProtesto then
         FPURL := FPURL + '/boletos/protestos'
       else if ATitulo.OcorrenciaOriginal.Tipo in [ACBrBoleto.toRemessaCobrarJurosMora, ACBrBoleto.toRemessaAlterarJurosMora] then
         FPURL := FPURL + '/boletos/encargos/juros-mora'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarMulta then
         FPURL := FPURL + '/boletos/encargos/multas'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarDesconto then
         FPURL := FPURL +  '/boletos/descontos'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarValorAbatimento then
         FPURL := FPURL + '/boletos/abatimentos'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarSeuNumero then
         FPURL := FPURL + '/boletos/seu-numero'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarEspecieTitulo then
         FPURL := FPURL + '/boletos/especie-documento';
    end;
    tpConsultaDetalhe:  FPURL := FPURL + '/boletos?numeroContrato='+LContrato+'&modalidade=1&nossoNumero='+LNossoNumero;
    tpBaixa:  FPURL := FPURL + '/boletos/baixa';
  end;

end;

procedure TBoletoW_Bancoob.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_Bancoob.GerarHeader;
begin
  FPHeaders.Clear;
  DefinirContentType;
  DefinirKeyUser;

  if NaoEstaVazio(Boleto.Cedente.CedenteWS.ClientID) then
    FPHeaders.Add(C_SICOOB_CLIENT + ': ' + Boleto.Cedente.CedenteWS.ClientID);
//  HTTPSend.Headers.Add('Accept-Encoding: ' + C_ACCEPT_ENCODING);
end;

procedure TBoletoW_Bancoob.GerarDados;
begin
  if Assigned(Boleto) then

    DefinirURL;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        FMetodoHTTP := htPOST; // Define Método POST para Incluir
        RequisicaoJson;
      end;
    tpAltera:
      begin
        FMetodoHTTP := htPATCH;
        RequisicaoAltera;
      end;
    tpBaixa:
      begin
        FMetodoHTTP := htPATCH; // Define Método POST para Baixa
        RequisicaoBaixa;
      end;
    tpConsultaDetalhe:
      begin
        FMetodoHTTP := htGET; // Define Método GET Consulta Detalhe
        RequisicaoConsultaDetalhe;
      end;

  else
    raise EACBrBoletoWSException.Create
      (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
      [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
  end;
end;

procedure TBoletoW_Bancoob.DefinirAuthorization;
begin
  if Boleto.Configuracoes.WebService.Ambiente = taProducao then
    FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao
  else
    FPAuthorization := C_AUTHORIZATION + ': Bearer ' + C_ACCESS_TOKEN_HOM;
end;

function TBoletoW_Bancoob.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Bancoob.DefinirKeyUser;
begin
  FPKeyUser := '';
end;

function TBoletoW_Bancoob.DefinirParametros: String;
var
  Consulta: TStringList;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    Consulta := TStringList.Create;
    Consulta.Delimiter := '&';
    try
      Consulta.Add( 'numeroContrato='+Boleto.Cedente.CodigoCedente);
      Consulta.Add( 'modalidade=1' );
      // Consulta.Add( 'nossoNumero=124' );
    finally
      result := Consulta.DelimitedText;
      Consulta.Free;
    end;
  end;
end;

procedure TBoletoW_Bancoob.DefinirParamOAuth;
begin
  FParamsOAuth := Format( 'client_id=%s&scope=%s&grant_type=client_credentials',
                   [Boleto.Cedente.CedenteWS.ClientID,
                    Boleto.Cedente.CedenteWS.Scope] );
  
end;

function TBoletoW_Bancoob.DateBancoobtoDateTime(const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'.','/', [rfReplaceAll] ),0);
end;

function TBoletoW_Bancoob.DateTimeToDateBancoob(const AValue: TDateTime): String;
begin
  //result := DateTimeToIso8601(DateTimeUniversal('',AValue),BiasToTimeZone(LTZ.Bias));
  result := FormatDateBr( aValue, 'YYYY-MM-DD') + 'T' + FormatDateTime('hh:nn:ss', AValue) + GetUTCSistema;
end;

procedure TBoletoW_Bancoob.DefinirAutenticacao;
begin

end;

function TBoletoW_Bancoob.ValidaAmbiente: Integer;
begin
  result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1','2'), 2);
end;

procedure TBoletoW_Bancoob.RequisicaoBaixa;
var
  Json: TJsonObject;
  Data: string;
begin
  if not Assigned(aTitulo) then
    Exit;

  Json := TJsonObject.Create;
  try
    Json.Add('numeroContrato').Value.AsInteger := StrToIntDef(aTitulo.ACBrBoleto.Cedente.CodigoCedente, 0);
    Json.Add('modalidade').Value.AsInteger     := StrToIntDef(aTitulo.ACBrBoleto.Cedente.Modalidade, 1);
    Json.Add('nossoNumero').Value.AsInteger    := StrToIntDef(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 0);
    Json.Add('seuNumero').Value.asString       := IfThen(ATitulo.SeuNumero <> '',
                                                    ATitulo.SeuNumero,
                                                    IfThen(ATitulo.NumeroDocumento <> '',
                                                      ATitulo.NumeroDocumento,
                                                      OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
                                                    )
                                                  );

    Data := Json.Stringify;
    FPDadosMsg := '[' + Data + ']';
  finally
    Json.Free;
  end;
end;

procedure TBoletoW_Bancoob.RequisicaoJson;
var
  Data: string;
  Json: TJsonObject;
begin
  if not Assigned(aTitulo) then
    Exit;

  Json := TJsonObject.Create;
  try
    Json.Add('numeroContrato').Value.AsInteger                  := StrToIntDef(aTitulo.ACBrBoleto.Cedente.CodigoCedente, 0);
    Json.Add('modalidade').Value.AsInteger                      := strtoIntdef(aTitulo.ACBrBoleto.Cedente.Modalidade, 1);
    Json.Add('numeroContaCorrente').Value.AsInteger             := strtoInt(aTitulo.ACBrBoleto.Cedente.Conta + aTitulo.ACBrBoleto.Cedente.ContaDigito);
    Json.Add('especieDocumento').Value.AsString                 := aTitulo.EspecieDoc;
    Json.Add('dataEmissao').Value.AsString                      := DateTimeToDateBancoob(aTitulo.DataDocumento);
    {
      Número que identifica o boleto de cobrança no Sisbr.
      Caso deseje, o beneficiário poderá informar o nossoNumero,
      Caso contrário, o sistema gerará automáticamente.
    }
    if StrToInt(ATitulo.NossoNumero) > 0 then
      Json.Add('nossoNumero').Value.AsInteger                   := StrToIntDef(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 0);

    Json.Add('seuNumero').Value.asString                        := IfThen(ATitulo.NumeroDocumento <> '',
                                                                     ATitulo.NumeroDocumento,
                                                                     IfThen(ATitulo.SeuNumero <> '',
                                                                       ATitulo.SeuNumero,
                                                                       OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
                                                                     )
                                                                   );
    Json.Add('identificacaoBoletoEmpresa').Value.AsString       := IfThen(ATitulo.SeuNumero <> '',
                                                                     ATitulo.SeuNumero,
                                                                     OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
                                                                   );
    Json.Add('identificacaoEmissaoBoleto').Value.AsInteger      := StrToInt(IfThen(ATitulo.ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite,'1','2')); // 2 Cliente Emite - 1 Banco Emite
    Json.Add('identificacaoDistribuicaoBoleto').Value.AsInteger := StrToInt(IfThen(ATitulo.ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite,'1','2')); // 2 Cliente Dist - 1 Banco Dist


    Json.Add('valor').Value.asNumber                            := aTitulo.ValorDocumento;
    Json.Add('dataVencimento').Value.asString                   := DateTimeToDateBancoob(aTitulo.Vencimento);
    Json.Add('numeroParcela').Value.AsInteger                   := max(1,ATitulo.Parcela);
    Json.Add('aceite').Value.AsBoolean                          := ATitulo.Aceite = atSim;

    if (ATitulo.DataProtesto > 0) then
    begin
      Json.Add('codigoProtesto').Value.AsInteger                := IfThen(ATitulo.TipoDiasProtesto = diCorridos, 1, 2);
      Json.Add('numeroDiasProtesto').Value.AsInteger            := Trunc(ATitulo.DataProtesto - ATitulo.Vencimento);
    end;
    if (ATitulo.DiasDeNegativacao > 0) then
    begin
      Json.Add('codigoNegativacao').Value.AsInteger             := 2;
      Json.Add('numeroDiasNegativacao').Value.AsInteger         := ATitulo.DiasDeNegativacao;
    end;

    GerarDesconto(Json);
    GerarJuros(Json);
    GerarMulta(Json);
    GerarPagador(Json);
    GerarBenificiarioFinal(Json);
    GerarInstrucao(Json);

    Json.Add('gerarPdf').Value.AsBoolean              := false;
    Json.Add('codigoCadastrarPIX').Value.AsInteger    := StrToInt(IfThen(Boleto.Cedente.CedenteWS.IndicadorPix,'1','0'));

    Data := Json.Stringify;

    FPDadosMsg := '['+Data+']';
  finally
    Json.Free;
  end;
end;

procedure TBoletoW_Bancoob.RequisicaoAltera;
var
  Data: string;
  Json: TJsonObject;
  NumeroDocumento: string;
begin
  if not Assigned(aTitulo) then
    Exit;

  Json := TJsonObject.Create;
  try
    Json.Add('numeroContrato').Value.AsInteger := StrToIntDef(aTitulo.ACBrBoleto.Cedente.CodigoCedente, 0);
    Json.Add('modalidade').Value.AsInteger := strtoIntdef(aTitulo.ACBrBoleto.Cedente.Modalidade, 1);
    Json.Add('nossoNumero').Value.AsInteger := StrtoIntdef(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 0);

    NumeroDocumento := Trim(ATitulo.SeuNumero);
    if EstaVazio(NumeroDocumento) then
      NumeroDocumento := Trim(ATitulo.NumeroDocumento);
    if EstaVazio(NumeroDocumento) then
      NumeroDocumento := OnlyNumber(ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));

    case aTitulo.ACBrBoleto.ListadeBoletos.Objects[0].OcorrenciaOriginal.Tipo of
      toRemessaBaixar, toRemessaAlterarSeuNumero:
        Json.Add('seuNumero').Value.AsString := NumeroDocumento;
      toRemessaConcederDesconto:
        AtribuirDesconto(Json);
      toRemessaAlterarVencimento:
        AlteraDataVencimento(Json);
      toRemessaProtestar: begin
        FMetodoHTTP := htPOST;
        AlterarProtesto(Json);
      end;
      toRemessaSustarProtesto: begin
        FMetodoHTTP :=  htDELETE;
        AlterarProtesto(Json);
      end;
      toRemessaAlterarJurosMora, toRemessaCobrarJurosMora:
        AtribuirJuros(Json);
      toRemessaAlterarMulta:
        AtribuirMulta(Json);
      toRemessaAlterarDesconto:
        AlteracaoDesconto(Json);
      toRemessaAlterarValorAbatimento:
        AtribuirAbatimento(Json);
      toRemessaAlterarEspecieTitulo:
        AlterarEspecie(Json);
    end;

    Data := Json.Stringify;

    FPDadosMsg := '['+Data+']';
  finally
    Json.Free;
  end;
end;

procedure TBoletoW_Bancoob.RequisicaoConsultaDetalhe;
begin
  FPDadosMsg := '';
end;

procedure TBoletoW_Bancoob.GerarPagador(AJson: TJsonObject);
 var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJSONPair;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  JsonDadosPagador := TJsonObject.Create;
  try
    JsonDadosPagador.Add('numeroCpfCnpj').Value.asString := OnlyNumber(aTitulo.Sacado.CNPJCPF);
    JsonDadosPagador.Add('nome').Value.asString          := aTitulo.Sacado.NomeSacado;
    JsonDadosPagador.Add('endereco').Value.asString      := aTitulo.Sacado.Logradouro + ' ' + aTitulo.Sacado.Numero;
    JsonDadosPagador.Add('bairro').Value.asString        := aTitulo.Sacado.Bairro;
    JsonDadosPagador.Add('cidade').Value.asString        := aTitulo.Sacado.Cidade;
    JsonDadosPagador.Add('cep').Value.asString           := OnlyNumber(aTitulo.Sacado.CEP);
    JsonDadosPagador.Add('uf').Value.asString            := aTitulo.Sacado.UF;
    //JsonDadosPagador.Add('complemento').Value.asString :=aTitulo.Sacado.Complemento;
    //JsonDadosPagador.Add('telefone').Value.asString := IfThen(aTitulo.Sacado.Fone = '', '0', aTitulo.Sacado.Fone);

    { TODO : Corrigir Escrita do Email precisa ser dentro de chaves [ ] }

//        if aTitulo.Sacado.Email <> '' then
//        begin
//          JsonDadosPagador.Add('email').Value.asString   :=  aTitulo.Sacado.Email;
//        end;

    JsonPairPagador := TJSONPair.Create(AJson, 'pagador');
    try
      JsonPairPagador.Value.AsObject := JsonDadosPagador;
      AJson.Add('pagador').Assign(JsonPairPagador);
    finally
      JsonPairPagador.Free;
    end;
  finally
    JsonDadosPagador.Free;
  end;
end;

procedure TBoletoW_Bancoob.GerarInstrucao(AJson: TJsonObject);
var
  JsonDadosInstrucao: TJsonObject;
  JsonArrayInstrucao: TJsonArray;
  JsonPairInstrucao, JsonPairMensagem : TJSONPair;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  if ATitulo.Instrucao1 = '' then
    Exit;

  JsonDadosInstrucao := TJsonObject.Create;
  JsonArrayInstrucao := TJsonArray.Create;
  try
    JsonDadosInstrucao.Add('tipoInstrucao').Value.AsInteger := 1;

    JsonPairMensagem := TJSONPair.Create(JsonArrayInstrucao, 'mensagens');

    JsonArrayInstrucao.Add.AsString := ATitulo.Instrucao1;

    if ATitulo.Instrucao2 <> '' then
     JsonArrayInstrucao.Add.AsString := ATitulo.Instrucao2;

    if ATitulo.Instrucao3 <> '' then
     JsonArrayInstrucao.Add.AsString := ATitulo.Instrucao3;

    JsonPairMensagem.Value.AsArray := JsonArrayInstrucao;
    JsonDadosInstrucao.Add('mensagens').Assign(JsonPairMensagem);

    JsonPairInstrucao := TJSONPair.Create(AJson, 'mensagensInstrucao');
    try
      JsonPairInstrucao.Value.AsObject := JsonDadosInstrucao;
      AJson.Add('mensagensInstrucao').Assign(JsonPairInstrucao);
    finally
      JsonPairInstrucao.Free;
    end;
  finally
    JsonDadosInstrucao.Free;
    JsonArrayInstrucao.Free;
    JsonPairMensagem.Free;
  end;
end;

procedure TBoletoW_Bancoob.GerarBenificiarioFinal(AJson: TJsonObject);
var
  JsonSacadorAvalista: TJsonObject;
  JsonPairSacadorAvalista: TJSONPair;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  if aTitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
    Exit;

  JsonSacadorAvalista := TJsonObject.Create;
  try
    JsonSacadorAvalista.Add('nome').Value.asString       := aTitulo.Sacado.SacadoAvalista.NomeAvalista;
    JsonSacadorAvalista.Add('cpfCnpj').Value.asString    := OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF);
    JsonSacadorAvalista.Add('tipoPessoa').Value.asString := IfThen(Length(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11, 'FISICA', 'JURIDICA');
    JsonSacadorAvalista.Add('cep').Value.asString        := aTitulo.Sacado.SacadoAvalista.CEP;
    JsonSacadorAvalista.Add('endereco').Value.asString   := aTitulo.Sacado.SacadoAvalista.Logradouro;
    JsonSacadorAvalista.Add('bairro').Value.asString     := aTitulo.Sacado.SacadoAvalista.Bairro;
    JsonSacadorAvalista.Add('cidade').Value.asString     := aTitulo.Sacado.SacadoAvalista.Cidade;
    JsonSacadorAvalista.Add('uf').Value.asString         := aTitulo.Sacado.SacadoAvalista.UF;

    JsonPairSacadorAvalista := TJSONPair.Create(AJson, 'beneficiarioFinal');
    try
      JsonPairSacadorAvalista.Value.AsObject := JsonSacadorAvalista;
      AJson.Add('beneficiarioFinal').Assign(JsonPairSacadorAvalista);
    finally
      JsonPairSacadorAvalista.Free;
    end;
  finally
    JsonSacadorAvalista.Free;
  end;
end;

procedure TBoletoW_Bancoob.GerarJuros(AJson: TJsonObject);
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  if ATitulo.CodigoMora = '' then
  begin
    case aTitulo.CodigoMoraJuros of
      cjValorDia   : aTitulo.CodigoMora := '1';
      cjTaxaMensal : aTitulo.CodigoMora := '2';
      cjIsento     : aTitulo.CodigoMora := '3';
      else
        aTitulo.CodigoMora := '3';
    end;
  end;

  case (StrToIntDef(aTitulo.CodigoMora, 0)) of
    0, 3:    // Isento
      begin
        AJson.Add('tipoJurosMora').Value.AsInteger := 3;
        AJson.Add('valorJurosMora').Value.asNumber := 0;
      end;
    1:     // Dia
      begin
       // AJson.Add('taxa').Value.asNumber := aTitulo.ValorMoraJuros;
        AJson.Add('tipoJurosMora').Value.AsInteger := StrToInt(aTitulo.CodigoMora);
        AJson.Add('dataJurosMora').Value.AsString  := DateTimeToDateBancoob(aTitulo.DataMoraJuros);
        AJson.Add('valorJurosMora').Value.asNumber := aTitulo.ValorMoraJuros;
      end;
    2: // Mês
      begin
        AJson.Add('tipoJurosMora').Value.AsInteger := StrToInt(aTitulo.CodigoMora);
        AJson.Add('dataJurosMora').Value.AsString  := DateTimeToDateBancoob(aTitulo.DataMoraJuros);
        AJson.Add('valorJurosMora').Value.asNumber := aTitulo.ValorMoraJuros;
      end;
  end;
end;

procedure TBoletoW_Bancoob.GerarMulta(AJson: TJsonObject);
var
  ACodMulta: Integer;
  ADataMulta : TDateTime;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  if aTitulo.PercentualMulta > 0 then
  begin
    if aTitulo.MultaValorFixo then
      ACodMulta := 1
    else
      ACodMulta := 2;
  end
  else
    ACodMulta := 3;

  if (aTitulo.DataMulta > 0) then
    ADataMulta :=  aTitulo.DataMulta
  else
    ADataMulta  := ATitulo.DataMoraJuros;

  case ACodMulta of
    1:
      begin
        AJson.Add('tipoMulta').Value.AsInteger  := 1; // Valor Fixo
        AJson.Add('dataMulta').Value.asString   := DateTimeToDateBancoob(ADataMulta);
        AJson.Add('valorMulta').Value.asNumber  := aTitulo.PercentualMulta;
      end;
    2:
      begin
        AJson.Add('tipoMulta').Value.AsInteger := 2; // Percentual
        AJson.Add('dataMulta').Value.asString  := DateTimeToDateBancoob(ADataMulta);
        AJson.Add('valorMulta').Value.asNumber := aTitulo.PercentualMulta;
      end;
    3:
      begin
        AJson.Add('tipoMulta').Value.AsInteger := 0;
        AJson.Add('valorMulta').Value.asNumber := 0;
      end;
  end;
end;

procedure TBoletoW_Bancoob.GerarDesconto(AJson: TJsonObject);
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  // '0'  =  Não Conceder desconto
  // '1'  =  Valor Fixo Até a Data Informada
  // '2'  =  Percentual Até a Data Informada"

  if (aTitulo.DataDesconto > 0) then
  begin
    case Integer(aTitulo.TipoDesconto) of
      1:
        begin
          AJson.Add('tipoDesconto').Value.AsInteger := 1;
          AJson.Add('dataPrimeiroDesconto').Value.asString :=  DateTimeToDateBancoob(aTitulo.DataDesconto);
          AJson.Add('valorPrimeiroDesconto').Value.asNumber := aTitulo.ValorDesconto;
        end;
      2:
        begin
          AJson.Add('tipoDesconto').Value.AsInteger := 2;
          AJson.Add('dataPrimeiroDesconto').Value.asString :=  DateTimeToDateBancoob(aTitulo.DataDesconto);
          AJson.Add('valorPrimeiroDesconto').Value.asNumber := aTitulo.ValorDesconto;
        end;
    end;
  end
  else
    AJson.Add('tipoDesconto').Value.AsInteger := 0;

  if (aTitulo.DataDesconto2 > 0) then
  begin
    AJson.Add('dataSegundoDesconto').Value.asString :=  DateTimeToDateBancoob(aTitulo.DataDesconto2);
    AJson.Add('valorSegundoDesconto').Value.asNumber := aTitulo.ValorDesconto2;
  end;

  if (aTitulo.DataDesconto3 > 0) then
  begin
    AJson.Add('dataTerceiroDesconto').Value.asString :=  DateTimeToDateBancoob(aTitulo.DataDesconto3);
    AJson.Add('valorTerceiroDesconto').Value.asNumber := aTitulo.ValorDesconto3;
  end;
end;

procedure TBoletoW_Bancoob.AlteraDataVencimento(AJson: TJsonObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  if (ATitulo.Vencimento = 0) then
    Exit;

  AJson.Add('dataVencimento').Value.asString := DateTimeToDateBancoob(aTitulo.Vencimento);
end;

procedure TBoletoW_Bancoob.AtribuirAbatimento(AJson: TJsonObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  if (ATitulo.ValorAbatimento = 0) then
    Exit;

  AJson.Add('valorAbatimento').Value.AsNumber := aTitulo.ValorAbatimento;
end;

procedure TBoletoW_Bancoob.AlterarEspecie(AJson: TJsonObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  if (ATitulo.EspecieDoc = '') then
    Exit;

  AJson.Add('especieDocumento').Value.AsString := aTitulo.EspecieDoc;
end;

procedure TBoletoW_Bancoob.AtribuirDesconto(AJson: TJsonObject);
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  GerarDesconto(AJson);
end;

procedure TBoletoW_Bancoob.AlteracaoDesconto(AJson: TJsonObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  GerarDesconto(AJson);
end;

procedure TBoletoW_Bancoob.AlterarProtesto(AJson: TJsonObject);
begin
  // Só Precisa de Numero de Contrato, Modalidade e Nosso Numero

  // Já preenchidos
end;

procedure TBoletoW_Bancoob.AtribuirJuros(AJson: TJsonObject);
begin
  GerarJuros(AJson);
end;

procedure TBoletoW_Bancoob.AtribuirMulta(AJson: TJsonObject);
begin
  GerarMulta(AJson);
end;

constructor TBoletoW_Bancoob.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    OAuth.URL := C_URL_OAUTH_PROD;
    OAuth.Payload := True;
  end;
end;

function TBoletoW_Bancoob.GerarRemessa: string;
begin
  DefinirCertificado;
  result := inherited GerarRemessa;
end;

function TBoletoW_Bancoob.Enviar: boolean;
begin
  DefinirCertificado;
  result := inherited Enviar;
end;

end.

