{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNRE;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrGNReConfiguracoes, ACBrGNReWebServices, ACBrGNReGuias,
  ACBrGNReGuiaClass,
  pgnreGNRE, pcnConversao, pgnreConversao,
  ACBrUtil;

//  ACBrGNREGuiasRetorno,

const
  ACBRGNRE_VERSAO = '1.0.0a';
  ACBRGNRE_NAMESPACE = 'http://www.gnre.pe.gov.br';
  ACBRGNRE_CErroAmbDiferente = 'Ambiente do XML (tpAmb) é diferente do ' +
               'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrGNReException = class(EACBrDFeException);

  { TACBrGNRe }

  TACBrGNRe = class(TACBrDFe)
  private
    FGNREGuia: TACBrGNREGuiaClass;

    FGuias: TGuias;
//    FGuiasRetorno: TGuiasRetorno;
    FStatus: TStatusACBrGNRe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesGNRe;
    procedure SetConfiguracoes(AValue: TConfiguracoesGNRe);
    procedure SetGNREGuia(const Value: TACBrGNREGuiaClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetAbout: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamGNRe: TStream = nil; NomeArq: String = ''); override;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function CstatConfirmada(AValue: integer): Boolean;
    function CstatProcessado(AValue: integer): Boolean;

    function Enviar: Boolean;
    function ConsultarResultadoLote(ANumRecibo: String): Boolean;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
    procedure LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOut): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaGNRe;
    function GerarNomeArqSchema(const ALayOut: TLayOut; VersaoServico: Double
      ): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Guias: TGuias read FGuias write FGuias;
//    property GuiasRetorno: TGuiasRetorno read FGuiasRetorno write FGuiasRetorno;
    property Status: TStatusACBrGNRe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrGNRe);
  published
    property Configuracoes: TConfiguracoesGNRe read GetConfiguracoes write SetConfiguracoes;
    property GNREGuia: TACBrGNREGuiaClass      read FGNREGuia        write SetGNREGuia;
  end;








(*
  TACBrGNRE = class(TComponent)
  private
    fsAbout: TACBrGNREAboutInfo;
    FGuias: TGuias;
    FGuiasRetorno: TGuiasRetorno;
    FGNREGuia: TACBrGNREGuiaClass;
    FWebServices: TWebServices;
    FConfiguracoes: TConfiguracoes;
    FStatus : TStatusACBrGNRE;
    FOnStatusChange: TNotifyEvent;
    FOnGerarLog : TACBrGNRELog;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Enviar: Boolean;
    function ConsultarResultadoLote(ANumRecibo: String): Boolean;

    property WebServices: TWebServices   read FWebServices  write FWebServices;
    property Guias: TGuias read FGuias write FGuias;
    property GuiasRetorno: TGuiasRetorno read FGuiasRetorno write FGuiasRetorno;
    property Status: TStatusACBrGNRE     read FStatus;
    procedure SetStatus( const stNewStatus : TStatusACBrGNRE );
  published
    property Configuracoes: TConfiguracoes     read FConfiguracoes  write FConfiguracoes;
    property OnStatusChange: TNotifyEvent      read FOnStatusChange write FOnStatusChange;
    property GNREGuia: TACBrGNREGuiaClass      read FGNREGuia       write FGNREGuia;
    property AboutACBrGNRE: TACBrGNREAboutInfo read fsAbout         write fsAbout stored false;
    property OnGerarLog: TACBrGNRELog          read FOnGerarLog     write FOnGerarLog;
  end;

procedure ACBrAboutDialog;
*)
implementation

uses
  strutils, dateutils,
  pcnAuxiliar, synacode;

{$IFDEF FPC}
 {$R ACBrGNReServicos.rc}
{$ELSE}
 {$R ACBrGNReServicos.res}
{$ENDIF}

{ TACBrGNRe }

constructor TACBrGNRe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGuias := TGuias.Create(Self, Guia);
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrGNRe.Destroy;
begin
  FGuias.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrGNRe.EnviarEmail(sPara, sAssunto: String; sMensagem, sCC,
  Anexos: TStrings; StreamGNRe: TStream; NomeArq: String);
begin
  SetStatus( stGNReEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamGNRe, NomeArq);
  finally
    SetStatus( stIdle );
  end;
end;

procedure TACBrGNRe.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FGNREGuia <> nil) and
    (AComponent is TACBrGNREGuiaClass) then
    FGNREGuia := nil;
end;

function TACBrGNRe.GetAbout: String;
begin
  Result := 'ACBrGNRe Ver: ' + ACBRGNRE_VERSAO;
end;

function TACBrGNRe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesGNRe.Create(Self);
end;

procedure TACBrGNRe.SetGNREGuia(const Value: TACBrGNREGuiaClass);
var
  OldValue: TACBrGNREGuiaClass;
begin
  if Value <> FACBrGNRe then
  begin
    if Assigned(FACBrGNRe) then
      FACBrGNRe.RemoveFreeNotification(Self);

    OldValue := FACBrGNRe; // Usa outra variavel para evitar Loop Infinito
    FACBrGNRe := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrGNRe) then
        OldValue.ACBrGNRe := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      Value.ACBrGNRe := Self;
    end;
  end;
end;

function TACBrGNRe.GetNomeModeloDFe: String;
begin
  Result := 'GNRE';
end;

function TACBrGNRe.GetNameSpaceURI: String;
begin
  Result := ACBRGNRE_NAMESPACE;
end;

function TACBrGNRe.CstatConfirmada(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrGNRe.CstatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
    else
      Result := False;
  end;
end;

function TACBrGNRe.IdentificaSchema(const AXML: String): TSchemaGNRe;
var
  lTipoEvento: String;
  I: integer;
begin

  Result := schGNRe;
  I := pos('<infGNRe', AXML);
  if I = 0 then
    Result := schError;
end;

function TACBrGNRe.GerarNomeArqSchema(const ALayOut: TLayOut;
  VersaoServico: Double): String;
var
  NomeServico, NomeSchema, ArqSchema: String;
  Versao: Double;
begin
  // Procura por Versão na pasta de Schemas //
  NomeServico := LayOutToServico(ALayOut);
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  ArqSchema := '';
  if NaoEstaVazio(NomeSchema) then
  begin
    Versao := VersaoServico;
    AchaArquivoSchema( NomeSchema, Versao, ArqSchema );
  end;

  Result := ArqSchema;
end;

function TACBrGNRe.GetConfiguracoes: TConfiguracoesGNRe;
begin
  Result := TConfiguracoesGNRe(FPConfiguracoes);
end;

procedure TACBrGNRe.SetConfiguracoes(AValue: TConfiguracoesGNRe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrGNRe.LerVersaoDeParams(LayOutServico: TLayOut): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoDFToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrGNRe.LerServicoDeParams(LayOutServico: TLayOut;
  var Versao: Double; var URL: String);
var
  AUF: String;
begin
  Versao := VersaoDFToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  AUF := Configuracoes.WebServices.UF;

  LerServicoDeParams(GetNomeModeloDFe,
                     AUF,
                     Configuracoes.WebServices.Ambiente,
                     LayOutToServico(LayOutServico),
                     Versao,
                     URL);
end;

procedure TACBrGNRe.SetStatus(const stNewStatus: TStatusACBrGNRe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrGNRe.NomeServicoToNomeSchema(const NomeServico: String): String;
Var
  ok: Boolean;
  ALayout: TLayOut;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaNFeToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

function TACBrGNRe.Enviar: Boolean;
begin
  if Guias.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma GNRe adicionada ao Lote'));

  if Guias.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de GNRe transmitidas (máximo de 50 GNRe)' +
      ' excedido. Quantidade atual: ' + IntToStr(Guias.Count)));

  Result := WebServices.Envia;

  if FGNREGuias <> nil then
  begin
    for i := 0 to Guias.Count - 1 do
    begin
      if Guias.Items[i].Confirmada and Imprimir then
        Guais.Items[i].Imprimir;
    end;
  end;

end;

function TACBrGNRe.ConsultarResultadoLote(ANumRecibo: String): Boolean;
begin
 Result := WebServices.ConsultaResultadoLote(ANumRecibo);
end;

end.
