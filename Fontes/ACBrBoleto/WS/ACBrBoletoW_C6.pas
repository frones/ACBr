{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda                   }
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

{$I ACBr.inc}
unit ACBrBoletoW_C6;

interface

uses
  Classes, SysUtils, ACBrBoletoWS, pcnConversao, ACBrBoletoConversao,
  synacode, strutils, DateUtils, ACBrDFeSSL, synautil, ACBrBoleto, httpsend, Math,
  ACBrBoletoWS.Rest, ACBrJSON;

type

  { TBoletoW_C6 }
  TBoletoW_C6 = class(TBoletoWSREST)
  private
//    function CodigoTipoTitulo(AEspecieDoc: String): String;
    function DatetoDateTime(const AValue: String): TDateTime;
    function DateTimeToDate( const AValue:TDateTime ):String;
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
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;
  end;

const
  C_URL             = 'https://baas-api.c6bank.info/v1/bank_slips';
  C_URL_HOM         = 'https://baas-api-sandbox.c6bank.info/v1/bank_slips';

  C_URL_OAUTH_PROD  = 'https://baas-api.c6bank.info/v1/auth';
  C_URL_OAUTH_HOM   = 'https://baas-api-sandbox.c6bank.info/v1/auth';

  C_CONTENT_TYPE    = 'application/json';
  C_ACCEPT          = 'application/json';
  C_AUTHORIZATION   = 'Authorization';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = 'utf-8';


implementation

uses
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Base,
  ACBrUtil.Math;

{ TBoletoW_C6 }

procedure TBoletoW_C6.DefinirURL;
var
  LNossoNumeroCorrespondente: string;
begin
  if Assigned(Atitulo) then
    LNossoNumeroCorrespondente := ATitulo.NossoNumeroCorrespondente;

  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui           :    FPURL := FPURL + '/';
    tpAltera           :    FPURL := FPURL + '/' + LNossoNumeroCorrespondente;
    tpConsultaDetalhe  :    FPURL := FPURL + '/' + LNossoNumeroCorrespondente;
    tpCancelar,
    tpBaixa            :    FPURL := FPURL + '/' + LNossoNumeroCorrespondente+'/cancel';
  end;
end;

procedure TBoletoW_C6.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_C6.GerarHeader;
begin
  FPHeaders.Clear;
  DefinirContentType;
  DefinirKeyUser;
  FPHeaders.Add('partner-software-name: ProjetoACBr');
end;

procedure TBoletoW_C6.GerarDados;
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
        FMetodoHTTP := htPUT; // Define Método DELETE para Baixa
        RequisicaoAltera;
      end;
    tpConsultaDetalhe:
      begin
        FMetodoHTTP := htGET; // Define Método GET Consulta Detalhe
        // Sem Payload
      end;
    tpBaixa, tpCancelar:
      begin
        FMetodoHTTP := htPUT; // Define Método DELETE para Baixa
        // Sem Payload
      end;
  else
    raise EACBrBoletoWSException.Create
      (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
      [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
  end;

end;

procedure TBoletoW_C6.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_C6.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_C6.DefinirKeyUser;
begin
  if Assigned(aTitulo) then
    FPKeyUser := '';
end;

function TBoletoW_C6.DefinirParametros: String;
begin
//
end;

procedure TBoletoW_C6.DefinirParamOAuth;
begin
  FParamsOAuth := Format( 'client_id=%s&client_secret=%s&grant_type=client_credentials',
                   [Boleto.Cedente.CedenteWS.ClientID,
                    Boleto.Cedente.CedenteWS.ClientSecret] );
end;

function TBoletoW_C6.DatetoDateTime(const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'.','/', [rfReplaceAll] ),0);
end;

function TBoletoW_C6.DateTimeToDate(const AValue: TDateTime): String;
begin
  result := FormatDateBr( aValue, 'YYYY-MM-DD');
end;

procedure TBoletoW_C6.RequisicaoJson;
var
  LJson : TACBrJSONObject;
  LDesconto : TACBrJSONObject;
  LMensagem : TACBrJSONArray;
  I: Integer;
  LValorMoraJuros : Double;
begin
  if Assigned(ATitulo) then
  begin
    if (Length(ATitulo.SeuNumero) < 1) or (Length(ATitulo.SeuNumero) > 10) then
      raise Exception.Create('Campo SeuNumero [a-zA-Z0-9] é inválido min. 1 max. 10!');

    if not ((ATitulo.NossoNumero = '0') or (ATitulo.NossoNumero = Poem_Zeros('',Boleto.Banco.TamanhoMaximoNossoNum)) ) then
      raise Exception.Create('Campo NossoNumero é inválido obrigatóriamente deve ser informado valor 0!');

    try
      LJson := TACBrJSONObject.Create
        .AddPair('external_reference_id',ATitulo.SeuNumero)
        .AddPair('amount',ATitulo.ValorDocumento)
        .AddPair('due_date',DateTimeToDate(ATitulo.Vencimento));


      if (Trim(ATitulo.Mensagem.Text) <> '') then
      begin
        LMensagem := TACBrJSONArray.Create;
        for I := 0 to 3 do
        begin
          LMensagem.AddElement(Copy(Trim(ATitulo.Mensagem[I]),0,80));
          if LMensagem.Count + 1 > I then
            Break;
        end;
        LJson.AddPair('instructions',LMensagem)
      end;

      if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto2 > 0) or (ATitulo.ValorDesconto3 > 0) then
      begin
        LDesconto :=  TACBrJSONObject.Create;
        LDesconto.AddPair('discount_type',IfThen(ATitulo.CodigoDesconto = cdValorFixo, 'V', 'P'));

        if (ATitulo.ValorDesconto > 0) then
        begin
          LDesconto.AddPair('first',
              TACBrJSONObject.Create
              .AddPair('value',ATitulo.ValorDesconto)
              .AddPair('dead_line',DaysBetween(ATitulo.Vencimento,ATitulo.DataDesconto))
            );
        end;
        if (ATitulo.ValorDesconto2 > 0) then
        begin
          LDesconto.AddPair('second',
              TACBrJSONObject.Create
              .AddPair('value',ATitulo.ValorDesconto2)
              .AddPair('dead_line',DaysBetween(ATitulo.Vencimento,ATitulo.DataDesconto2))
            );
        end;
        if (ATitulo.ValorDesconto3 > 0) then
        begin
          LDesconto.AddPair('third',
              TACBrJSONObject.Create
              .AddPair('value',ATitulo.ValorDesconto3)
              .AddPair('dead_line',DaysBetween(ATitulo.Vencimento,ATitulo.DataDesconto3))
            );
        end;

        LJson.AddPair('discount', LDesconto);
      end;

      if (ATitulo.ValorMoraJuros > 0) then
      begin
        case ATitulo.CodigoMoraJuros of
          cjValorDia    : LValorMoraJuros := ATitulo.ValorMoraJuros;
          cjTaxaDiaria  : LValorMoraJuros := RoundABNT((ATitulo.ValorDocumento / 100 ) * ATitulo.ValorMoraJuros, 2);
          cjValorMensal : LValorMoraJuros := RoundABNT(ATitulo.ValorMoraJuros / 30, 2);
          cjTaxaMensal  : LValorMoraJuros := RoundABNT((ATitulo.ValorDocumento / 100 ) * (ATitulo.ValorMoraJuros / 30), 2);
          else
            LValorMoraJuros := ATitulo.ValorMoraJuros;
        end;

        LJson.AddPair('interest',
          TACBrJSONObject.Create
            .AddPair('type',IfThen(ATitulo.CodigoMoraJuros in [cjTaxaDiaria,cjTaxaMensal], 'P', 'V') )
            .AddPair('value',LValorMoraJuros )
            .AddPair('dead_line',IfThen(ATitulo.DataMoraJuros > 0,DaysBetween(ATitulo.Vencimento,ATitulo.DataMoraJuros), 1) )
        );
      end;
      if (ATitulo.PercentualMulta > 0) then
      begin
        LJson.AddPair('fine',
          TACBrJSONObject.Create
            .AddPair('type',IfThen(ATitulo.MultaValorFixo, 'V', 'P') )
            .AddPair('value',ATitulo.PercentualMulta )
            .AddPair('dead_line',IfThen(ATitulo.DataMulta > 0, DaysBetween(ATitulo.Vencimento,ATitulo.DataMulta), 1) )
        );
      end;
      LJson.AddPair('payer',
          TACBrJSONObject.Create
            .AddPair('name', ATitulo.Sacado.NomeSacado )
            .AddPair('tax_id', OnlyNumber(ATitulo.Sacado.CNPJCPF) )
            .AddPair('address',
              TACBrJSONObject.Create
                .AddPair('street', ATitulo.Sacado.Logradouro )
                .AddPair('number', StrToInt64Def(ATitulo.Sacado.Numero,0) )
                .AddPair('complement',ATitulo.Sacado.Complemento )
                .AddPair('city',ATitulo.Sacado.Cidade )
                .AddPair('state',ATitulo.Sacado.UF )
                .AddPair('zip_code',OnlyNumber(ATitulo.Sacado.CEP) )
            )
        );
        if ATitulo.Sacado.Email <> '' then
          LJson.AsJSONObject['payer'].AddPair('email',  ATitulo.Sacado.Email);

      FPDadosMsg := LJson.ToJSON;
    finally
      LJson.Free;
    end;
  end;
end;

procedure TBoletoW_C6.RequisicaoAltera;
var LJson : TACBrJSONObject;
begin
  LJson := TACBrJSONObject.Create;
  try
  case Atitulo.OcorrenciaOriginal.Tipo of
    toRemessaAlterarVencimento :
      begin
        LJson.AddPair('due_date',DateTimeToDate(ATitulo.Vencimento));
      end;
    toRemessaAlterarValorTitulo :
      begin
        LJson.AddPair('amount',ATitulo.ValorDocumento);
      end;
    toRemessaAlterarDesconto :
      begin
        if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) then
        begin
          LJson.AddPair('discount',
            TACBrJSONObject.Create
              .AddPair('discount_type',IfThen(ATitulo.CodigoDesconto = cdValorFixo, 'V', 'P'))
            );
            if (ATitulo.ValorDesconto > 0) then
            begin
              LJson.AddPair('first',
                 TACBrJSONObject.Create
                   .AddPair('value',ATitulo.ValorDesconto)
                   .AddPair('dead_line',DaysBetween(ATitulo.Vencimento,ATitulo.DataDesconto))
                 );
            end;
            if (ATitulo.ValorDesconto2 > 0) then
            begin
              LJson.AddPair('second',
                TACBrJSONObject.Create
                  .AddPair('value',ATitulo.ValorDesconto2)
                  .AddPair('dead_line',DaysBetween(ATitulo.Vencimento,ATitulo.DataDesconto2))
                );
            end;
            if (ATitulo.ValorDesconto3 > 0) then
            begin
              LJson.AddPair('third',
                TACBrJSONObject.Create
                  .AddPair('value',ATitulo.ValorDesconto3)
                  .AddPair('dead_line',DaysBetween(ATitulo.Vencimento,ATitulo.DataDesconto3))
                );
            end
        end;
      end;
    toRemessaAlterarJurosMora :
      begin
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          if not (ATitulo.CodigoMoraJuros in [cjTaxaMensal,cjValorMensal]) then
            raise Exception.Create('CodigoMoraJuros Inválido: permitido somente :: cjTaxaMensal,cjValorMensal !');

          LJson.AddPair('interest',
            TACBrJSONObject.Create
              .AddPair('type',IfThen(ATitulo.CodigoMoraJuros = cjTaxaMensal, 'P', 'V') )
              .AddPair('value',ATitulo.ValorMoraJuros )
              .AddPair('dead_line',IfThen(ATitulo.DataMoraJuros > 0,DaysBetween(ATitulo.Vencimento,ATitulo.DataMoraJuros), 1) )
          );
        end;
      end;
    toRemessaAlterarMulta :
      begin
        if (ATitulo.PercentualMulta > 0) then
        begin
            LJson.AddPair('fine',
              TACBrJSONObject.Create
                .AddPair('type',IfThen(ATitulo.MultaValorFixo, 'V', 'P') )
                .AddPair('value',ATitulo.PercentualMulta )
                .AddPair('dead_line',IfThen(ATitulo.DataMulta > 0, DaysBetween(ATitulo.Vencimento,ATitulo.DataMulta), 1) )
            );  
        end;
      end;
  end;
    FPDadosMsg := LJson.ToJSON;    
  finally
    LJson.Free;
  end;      
end;

constructor TBoletoW_C6.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;

    OAuth.Payload := OAuth.Ambiente = taHomologacao;
  end;
end;

function TBoletoW_C6.GerarRemessa: string;
begin
  DefinirCertificado;
  result := inherited GerarRemessa;
end;

function TBoletoW_C6.Enviar: boolean;
var
  LJsonObject : TACBrJSONObject;
begin
  DefinirCertificado;
  Result := inherited Enviar;
end;

end.

