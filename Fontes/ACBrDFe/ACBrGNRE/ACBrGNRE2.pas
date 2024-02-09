{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit ACBrGNRE2;

interface

uses
  Classes,
  SysUtils,
  ACBrDFe,
  ACBrDFeException,
  ACBrDFeConfiguracoes,
  ACBrBase,
  ACBrGNREConfiguracoes,
  ACBrGNREWebServices,
  ACBrGNREGuias,
  ACBrGNREGuiasRetorno,
  ACBrGNREGuiaClass,
  pcnConversao,
  pgnreConversao,
  ACBrDFeUtil;

const
  ACBRGNRE_NAMESPACE = 'http://www.gnre.pe.gov.br';
  ACBRGNRE_CErroAmbDiferente = 'Ambiente do XML (tpAmb) é diferente do ' +
               'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrGNREException = class(EACBrDFeException);

  { TACBrGNRE }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrGNRE = class(TACBrDFe)
  private
    FGNREGuia: TACBrGNREGuiaClass;

    FGuias: TGuias;
    FGuiasRetorno: TGuiasRetorno;
    FStatus: TStatusACBrGNRE;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesGNRE;
    procedure SetConfiguracoes(AValue: TConfiguracoesGNRE);
    procedure SetGNREGuia(const Value: TACBrGNREGuiaClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      sCC: TStrings = nil; Anexos: TStrings = nil; StreamGNRE: TStream = nil;
      const NomeArq: String = ''; sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function CstatConfirmada(AValue: integer): Boolean;
    function CstatProcessado(AValue: integer): Boolean;

    function Enviar(Imprimir: Boolean = True): Boolean;
    function ConsultarResultadoLote(ANumRecibo: String): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutGNRE; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutGNRE): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaGNRE;
    function GerarNomeArqSchema(const ALayOut: TLayOutGNRE; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Guias: TGuias read FGuias write FGuias;
    property GuiasRetorno: TGuiasRetorno read FGuiasRetorno write FGuiasRetorno;
    property Status: TStatusACBrGNRE read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrGNRE);
  published
    property Configuracoes: TConfiguracoesGNRE read GetConfiguracoes write SetConfiguracoes;
    property GNREGuia: TACBrGNREGuiaClass      read FGNREGuia        write SetGNREGuia;
  end;

implementation

uses
  Dateutils,
  ACBrUtil.Base,
  ACBrUtil.Strings;

{$IFDEF FPC}
 {$R ACBrGNREServicos.rc}
{$ELSE}
 {$R ACBrGNREServicos.res}
{$ENDIF}

{ TACBrGNRE }

constructor TACBrGNRE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGuias := TGuias.Create(Self, Guia);
  FWebServices := TWebServices.Create(Self);
  FGuiasRetorno := TGuiasRetorno.Create(Self, GuiaRetorno);
end;

destructor TACBrGNRE.Destroy;
begin
  FGuias.Free;
  FWebServices.Free;
  FGuiasRetorno.Free;

  inherited;
end;

procedure TACBrGNRE.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamGNRE: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stGNREEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamGNRE,
                          NomeArq, sReplyTo, sBCC);
  finally
    SetStatus( stGNREIdle );
  end;
end;

procedure TACBrGNRE.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FGNREGuia <> nil) and
    (AComponent is TACBrGNREGuiaClass) then
    FGNREGuia := nil;
end;

function TACBrGNRE.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesGNRE.Create(Self);
end;

procedure TACBrGNRE.SetGNREGuia(const Value: TACBrGNREGuiaClass);
var
  OldValue: TACBrGNREGuiaClass;
begin
  if Value <> FGNREGuia then
  begin
    if Assigned(FGNREGuia) then
      FGNREGuia.RemoveFreeNotification(Self);

    OldValue := FGNREGuia; // Usa outra variavel para evitar Loop Infinito
    FGNREGuia := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrGNRE) then
        OldValue.ACBrGNRE := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      Value.ACBrGNRE := Self;
    end;
  end;
end;

function TACBrGNRE.GetNomeModeloDFe: String;
begin
  Result := 'GNRE';
end;

function TACBrGNRE.GetNameSpaceURI: String;
begin
  Result := ACBRGNRE_NAMESPACE;
end;

function TACBrGNRE.CstatConfirmada(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrGNRE.CstatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
    else
      Result := False;
  end;
end;

function TACBrGNRE.IdentificaSchema(const AXML: String): TSchemaGNRE;
var
//  lTipoEvento: String;
  I: integer;
begin
  Result := schGNRE;
  I := pos('<infGNRE', AXML);
  if I = 0 then
    Result := schErro;
end;

function TACBrGNRE.GerarNomeArqSchema(const ALayOut: TLayOutGNRE;
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

function TACBrGNRE.GetConfiguracoes: TConfiguracoesGNRE;
begin
  Result := TConfiguracoesGNRE(FPConfiguracoes);
end;

procedure TACBrGNRE.SetConfiguracoes(AValue: TConfiguracoesGNRE);
begin
  FPConfiguracoes := AValue;
end;

function TACBrGNRE.LerVersaoDeParams(LayOutServico: TLayOutGNRE): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoGNREToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrGNRE.LerServicoDeParams(LayOutServico: TLayOutGNRE;
  var Versao: Double; var URL: String);
var
  AUF: String;
begin
  Versao := VersaoGNREToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  AUF := Configuracoes.WebServices.UF;

  LerServicoDeParams(GetNomeModeloDFe,
                     AUF,
                     Configuracoes.WebServices.Ambiente,
                     LayOutToServico(LayOutServico),
                     Versao,
                     URL);
end;

procedure TACBrGNRE.SetStatus(const stNewStatus: TStatusACBrGNRE);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrGNRE.NomeServicoToNomeSchema(const NomeServico: String): String;
Var
  ok: Boolean;
  ALayout: TLayOutGNRE;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaGNREToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

function TACBrGNRE.Enviar(Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  Guias.GerarGNRE;
  
  if Guias.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma GNRE adicionada ao Lote'));

  if Guias.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de GNRE transmitidas (máximo de 50 GNRE)' +
      ' excedido. Quantidade atual: ' + IntToStr(Guias.Count)));

  Result := WebServices.Envia;

  if FGNREGuia <> nil then
  begin
    for i := 0 to Guias.Count - 1 do
    begin
      if Guias.Items[i].Confirmada and Imprimir then
        Guias.Items[i].Imprimir;
    end;
  end;

end;

function TACBrGNRE.ConsultarResultadoLote(ANumRecibo: String): Boolean;
begin
 Result := WebServices.ConsultaResultadoLote(ANumRecibo);
end;

end.
