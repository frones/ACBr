{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXBase;

interface

uses
  Classes, SysUtils,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  ACBrBase;

resourcestring
  sErroMetodoNaoImplementado = 'Método %s não implementado para Classe %s';

const
  cGUIPIX = 'br.gov.bcb.pix';
  cBRCurrency = 986;
  cBRCountryCode = 'BR';
  cMPMValueNotInformed = '***';
  cEMVLimit = 99;

type
  TACBrPIXTipoChave = ( tchNenhuma,
                        tchCPF,
                        tchCNPJ,
                        tchCelular,
                        tchAleatoria );

  TACBrPIXStatusCobranca = ( stcNENHUM,
                             stcATIVA,
                             stcCONCLUIDA,
                             stcREMOVIDA_PELO_USUARIO_RECEBEDOR,
                             stcREMOVIDA_PELO_PSP );

  TACBrPIXStatusDevolucao = ( stdNENHUM,
                              stdEM_PROCESSAMENTO,
                              stdDEVOLVIDO,
                              stdNAO_REALIZADO );

  TACBrPIXNaturezaDevolucao = ( ndNENHUMA, ndORIGINAL, ndRETIRADA ) ;

  TACBrPIXModalidadeAgente = ( maNENHUM,
                               maAGTEC,
                               maAGTOT,
                               maAGPSS );

  TACBrPIXTipoCobranca = ( tcoNenhuma, tcoCob, tcoCobV );

  EACBrPixException = class(EACBrException);

  { TACBrPIXSchema }

  TACBrPIXSchema = class
  private
    function GetAsJSON: String; virtual;
    procedure SetAsJSON(AValue: String); virtual;
  public
    procedure Clear; virtual;
    procedure WriteToJSon(AJSon: TJsonObject); virtual;
    procedure ReadFromJSon(AJSon: TJsonObject); virtual;

    property AsJSON: String read GetAsJSON write SetAsJSON;
  end;


  function PIXStatusCobrancaToString(AStatus: TACBrPIXStatusCobranca): String;
  function StringToPIXStatusCobranca(const AString: String): TACBrPIXStatusCobranca;

  function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
  function StringToPIXModalidadeAgente(const AString: String): TACBrPIXModalidadeAgente;

  function PIXStatusDevolucaoToString(AStatus: TACBrPIXStatusDevolucao): String;
  function StringToPIXStatusDevolucao(const AString: String): TACBrPIXStatusDevolucao;

  function PIXNaturezaDevolucaoToString(AStatus: TACBrPIXNaturezaDevolucao): String;
  function StringToPIXNaturezaDevolucao(const AString: String): TACBrPIXNaturezaDevolucao;

  function PIXTipoCobrancaToString(ACob: TACBrPIXTipoCobranca): String;
  function StringToPIXTipoCobranca(const AString: String): TACBrPIXTipoCobranca;

implementation

uses
  ACBrUtil;

function PIXStatusCobrancaToString(AStatus: TACBrPIXStatusCobranca): String;
begin
  case AStatus of
    stcATIVA: Result := 'ATIVA';
    stcCONCLUIDA: Result := 'CONCLUIDA';
    stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := 'REMOVIDA_PELO_USUARIO_RECEBEDOR';
    stcREMOVIDA_PELO_PSP: Result := 'REMOVIDA_PELO_PSP';
  else
    Result := '';
  end;
end;

function StringToPIXStatusCobranca(const AString: String): TACBrPIXStatusCobranca;
begin
  if (AString = 'ATIVA') then
    Result := stcATIVA
  else if (AString = 'CONCLUIDA') then
    Result := stcCONCLUIDA
  else if (AString = 'REMOVIDA_PELO_USUARIO_RECEBEDOR') then
    Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR
  else if (AString = 'REMOVIDA_PELO_PSP') then
    Result := stcREMOVIDA_PELO_PSP
  else
    Result := stcNENHUM;
end;

function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
begin
  case AModalidadeAgente of
    maAGTEC: Result := 'AGTEC';
    maAGTOT: Result := 'AGTOT';
    maAGPSS: Result := 'AGPSS';
  else
    Result := '';
  end;
end;

function StringToPIXModalidadeAgente(const AString: String
  ): TACBrPIXModalidadeAgente;
begin
  if (AString = 'AGTEC') then
    Result := maAGTEC
  else if (AString = 'AGTOT') then
    Result := maAGTOT
  else if (AString = 'AGPSS') then
    Result := maAGPSS
  else
    Result := maNENHUM;
end;

function PIXStatusDevolucaoToString(AStatus: TACBrPIXStatusDevolucao): String;
begin
  case AStatus of
    stdEM_PROCESSAMENTO: Result := 'EM_PROCESSAMENTO';
    stdDEVOLVIDO: Result := 'DEVOLVIDO';
    stdNAO_REALIZADO: Result := 'NAO_REALIZADO';
  else
    Result := '';
  end;
end;

function StringToPIXStatusDevolucao(const AString: String
  ): TACBrPIXStatusDevolucao;
begin
  if (AString = 'EM_PROCESSAMENTO') then
    Result := stdEM_PROCESSAMENTO
  else if (AString = 'DEVOLVIDO') then
    Result := stdDEVOLVIDO
  else if (AString = 'NAO_REALIZADO') then
    Result := stdNAO_REALIZADO
  else
    Result := stdNENHUM;
end;

function PIXNaturezaDevolucaoToString(AStatus: TACBrPIXNaturezaDevolucao): String;
begin
  case AStatus of
    ndORIGINAL: Result := 'ORIGINAL';
    ndRETIRADA: Result := 'RETIRADA';
  else
    Result := '';
  end;
end;

function StringToPIXNaturezaDevolucao(const AString: String
  ): TACBrPIXNaturezaDevolucao;
begin
  if (AString = 'ORIGINAL') then
    Result := ndORIGINAL
  else if (AString = 'RETIRADA') then
    Result := ndRETIRADA
  else
    Result := ndNENHUMA;
end;

function PIXTipoCobrancaToString(ACob: TACBrPIXTipoCobranca): String;
begin
  case ACob of
    tcoCobV: Result := 'cobv';
    tcoCob: Result := 'cob';
  else
    Result := '';
  end;
end;

function StringToPIXTipoCobranca(const AString: String): TACBrPIXTipoCobranca;
begin
  if (AString = 'cobv') then
    Result := tcoCobV
  else if (AString = 'cob') then
    Result := tcoCob
  else
    Result := tcoNenhuma;
end;

{ TACBrPIXSchema }

function TACBrPIXSchema.GetAsJSON: String;
var
  js: TJsonObject;
begin
  js := TJsonObject.Create;
  try
    WriteToJSon(js);
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     Result := js.ToJSON();
    {$Else}
     Result := js.Stringify;
    {$EndIf}
  finally
    js.Free;
  end;
end;

procedure TACBrPIXSchema.SetAsJSON(AValue: String);
var
  js: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   js := TJsonObject.Parse(AValue) as TJsonObject;
   try
     ReadFromJSon(js);
   finally
     js.Free;
   end;
  {$Else}
   js := TJsonObject.Create;
   try
     js.Parse(AValue);
     ReadFromJSon(js);
   finally
     js.Free;
   end;
  {$EndIf}
end;

procedure TACBrPIXSchema.Clear;
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['Clear', ClassName]);
end;

procedure TACBrPIXSchema.WriteToJSon(AJSon: TJsonObject);
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['WriteToJSon', ClassName]);
end;

procedure TACBrPIXSchema.ReadFromJSon(AJSon: TJsonObject);
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['ReadFromJSon', ClassName]);
end;

end.

