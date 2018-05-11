{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
*******************************************************************************}

{$I ACBr.inc}

unit ACBrReinf;

interface

uses
  Classes, SysUtils, ACBrUtil,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrReinfConfiguracoes, ACBrReinfWebServices, ACBrReinfEventos,
  pcnConversao, pcnConversaoReinf;

resourcestring
  ACBRREINF_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do configurado no Componente (Configuracoes.WebServices.Ambiente)';
  ACBRREINF_CErroSignLib = 'Necessário DigestMethod Algorithm SHA256. use xsXmlSec ou xsLibXml2 na propriedade SSLXmlSignLib.';
  ACBRREINF_CErroCryptLib = 'Necessário DigestMethod Algorithm SHA256. use cryOpenSSL ou cryWinCrypt na propriedade SSLCryptLib.';

const
  ACBRREINF_VERSAO = '1.3';
  ACBRREINF_NAMESPACE_ENV = 'http://sped.fazenda.gov.br/RecepcaoLoteReinf';
  ACBRREINF_NAMESPACE_CON = 'http://sped.fazenda.gov.br/ConsultasReinf';

  ACBRREINF_NAMESPACE = ' http://www.reinf.gov.br/servicos/empregador/lote/eventos/envio/v1_1_0';
  ACBRREINF_NAMESPACE_URI = 'http://www.reinf.esocial.gov.br/schemas/';
  ACBRREINF_MODELODF = 'Reinf';

type

  EACBrReinfException = class(EACBrDFeException);
  TNotifyEventosReinf = procedure(const AXML: AnsiString; ATipo: TEventosReinf) of object;

  { TACBrReinf }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrReinf = class(TACBrDFe)
  private
    FEventos: TEventos;
    FStatus : TStatusReinf;
    FWebServices: TWebServices;

    FOnTransmissaoEventos: TNotifyEventosReinf;

    function GetConfiguracoes: TConfiguracoesReinf;
    procedure SetConfiguracoes(AValue: TConfiguracoesReinf);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    function GetAbout: String; override;
    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
    function VersaoSchemaDoubleToString(AVersao: Double): String; override;
    function VersaoSchemaStringToDouble(AVersao: String): Double; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AssinarEventos;
    procedure LerServicoDeParams(LayOutServico: TLayOutReinf; var Versao: Double; var URL: String); overload;
    procedure SetStatus(const stNewStatus: TStatusReinf);

    function GetNomeModeloDFe: string; override;
    function GetNameSpaceURI: string; override;
    function LerVersaoDeParams(LayOutServico: TLayOutReinf): String; reintroduce; overload;

    function Enviar: boolean;
    function Consultar(const AProtocolo: string): boolean;

    property Eventos: TEventos read FEventos write FEventos;
    property Status: TStatusReinf read FStatus;
    property WebServices: TWebServices read FWebServices write FWebServices;

  published
    property Configuracoes: TConfiguracoesReinf read GetConfiguracoes write SetConfiguracoes;
    property OnTransmissaoEventos: TNotifyEventosReinf read FOnTransmissaoEventos write FOnTransmissaoEventos;

  end;

implementation

{$IFDEF FPC}
 {$IFDEF CPU64}
  {$R ACBrReinfServicos.res}  // Dificuldades de compilar Recurso em 64 bits
 {$ELSE}
  {$R ACBrReinfServicos.rc}
 {$ENDIF}
{$ELSE}
 {$R ACBrReinfServicos.res}
{$ENDIF}

{ TACBrReinf }

uses
  ACBrDFeSSL;

constructor TACBrReinf.Create(AOwner: TComponent);
begin
  inherited;

  SSL.SSLDgst := dgstSHA256;
  FEventos := TEventos.Create(Self);
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrReinf.Destroy;
begin
  FEventos.Free;
  FWebServices.Free;

  inherited;
end;

function TACBrReinf.GetConfiguracoes: TConfiguracoesReinf;
begin
  Result := TConfiguracoesReinf(FPConfiguracoes);
end;

procedure TACBrReinf.SetConfiguracoes(AValue: TConfiguracoesReinf);
begin
  FPConfiguracoes := AValue;
end;

function TACBrReinf.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesReinf.Create(Self);
end;

function TACBrReinf.GetAbout: String;
begin
  Result := 'ACBrReinf Ver: ' + ACBRREINF_VERSAO;
end;

function TACBrReinf.Enviar: boolean;
begin
  WebServices.EnvioLote.Clear;

  result := WebServices.Envia;
end;

function TACBrReinf.Consultar(const AProtocolo: string): boolean;
begin
  Result := WebServices.Consulta(AProtocolo);
end;

procedure TACBrReinf.AssinarEventos;
begin
  Eventos.GerarXMLs;
  if Configuracoes.Geral.Salvar then
    Eventos.SaveToFiles;
end;

function TACBrReinf.NomeServicoToNomeSchema(const NomeServico: String): String;
var
  ok: Boolean;
  ALayout: TLayOutReinf;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaReinfToStr( LayOutReinfToSchema( ALayout ) )
  else
    Result := '';
end;

function TACBrReinf.VersaoSchemaDoubleToString(AVersao: Double): String;
var
  StrVer: String;
begin
  Result := '';

  if (AVersao > 0) then
  begin
    StrVer := FloatToString(AVersao, '.', '0.00');
    StrVer := StringReplace(StrVer,'.','',[rfReplaceAll]);
    Result := StrVer[1] + '_' + StrVer[2] + '_' + StrVer[3];
  end;
end;

function TACBrReinf.VersaoSchemaStringToDouble(AVersao: String): Double;
var
  StrVer: String;
begin
  Result := 0;
  if (AVersao <> '') then
  begin
    StrVer := StringReplace(AVersao,'_','',[rfReplaceAll]);
    StrVer := PadRight(StrVer, 3, '0');
    Result := StringToFloatDef(StrVer[1]+'.'+StrVer[2]+StrVer[3], 0);
  end;
end;

procedure TACBrReinf.LerServicoDeParams(LayOutServico: TLayOutReinf; var Versao: Double; var URL: String);
Var
  Sessao: string;
begin
  if Configuracoes.WebServices.Ambiente = taHomologacao then
    Sessao := 'Reinf_H'
  else
    Sessao := 'Reinf_P';

  LerServicoChaveDeParams(Sessao, LayOutReinfToServico(LayOutServico), Versao, URL);
  Versao := VersaoReinfToDbl(Configuracoes.Geral.VersaoDF);
end;

function TACBrReinf.LerVersaoDeParams(LayOutServico: TLayOutReinf): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutReinfToServico(LayOutServico),
    VersaoReinfToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrReinf.SetStatus(const stNewStatus: TStatusReinf);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrReinf.GetNomeModeloDFe: string;
begin
  Result := 'Reinf';
end;

function TACBrReinf.GetNameSpaceURI: string;
begin
  Result := ACBRREINF_NAMESPACE_ENV;
end;

end.
