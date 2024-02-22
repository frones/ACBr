{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrONE;

interface

uses
  Classes, SysUtils, ACBrBase,
  ACBrUtil.Base, ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrONEConfiguracoes, ACBrONEWebServices,
  pcnConversao, ACBrONEConversao;

const
  ACBRONE_NAMESPACE = 'http://www.portalfiscal.inf.br/one';
  ACBRONE_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do ' +
               'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrONEException = class(EACBrDFeException);

  { TACBrONE }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrONE = class(TACBrDFe)
  private
    FWebServices: TWebServices;
    FStatus: TStatusACBrONE;
//    FManutencaoEQP: TManutencaoEQP;
//    FRecepcaoLeitura: TRecepcaoLeitura;

    function GetConfiguracoes: TConfiguracoesONE;

    procedure SetConfiguracoes(AValue: TConfiguracoesONE);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamONE: TStream = nil; const NomeArq: String = ''; 
	  sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

//    function EhAutorizacao(AVersao: TVersaoONE; AUFCodigo: Integer): Boolean;
//    function CstatConfirmada(AValue: Integer): Boolean;
//    function CstatProcessado(AValue: Integer): Boolean;
//    function CstatCancelada(AValue: Integer): Boolean;
//    function IdentificaSchema(const AXML: String): TSchemaONE;

    procedure LerServicoDeParams(LayOutServico: TLayOutONE; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutONE): String; reintroduce; overload;

    function GerarNomeArqSchema(const ALayOut: TLayOutONE; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Status: TStatusACBrONE read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrONE);

//    property ManutencaoEQP: TManutencaoEQP read FManutencaoEQP write FManutencaoEQP;
//    property RecepcaoLeitura: TRecepcaoLeitura read FRecepcaoLeitura write FRecepcaoLeitura;

//    function EnviarManutencao: Boolean;
//    function EnviarLeitura: Boolean;
    function DistLeiturasPorCNPJ(TipoDist: TtpDist;
                                 const ultNSU: string;
                                 const NSUFin: string = ''): Boolean;
    function DistLeiturasPorUF(TipoDist: TtpDist;
                                 const ultNSU: string;
                                 cUF: Integer;
                                 const NSUFin: string = ''): Boolean;
    function DistLeiturasPorEQP(TipoDist: TtpDist;
                                 const ultNSU, cEQP: string;
                                 const NSUFin: string = ''): Boolean;

    function ConsultarFoto(const aVerAplic, aNSULeitura: string): Boolean;
    function ConsultarPlaca(const aVerAplic, aPlaca: string; aDataRef: TDateTime): Boolean;
  published
    property Configuracoes: TConfiguracoesONE read GetConfiguracoes write SetConfiguracoes;
  end;

Const
  ModeloDF = 'ONE';

implementation

uses
  dateutils;

{$IFDEF FPC}
 {$R ACBrONEServicos.rc}
{$ELSE}
 {$R ACBrONEServicos.res}
{$ENDIF}

{ TACBrONE }

function TACBrONE.ConsultarFoto(const aVerAplic, aNSULeitura: string): Boolean;
begin
  Result := WebServices.ConsultaFoto(aVerAplic, aNSULeitura);

  if not Result then
    GerarException( WebServices.ConsultarFoto.Msg );
end;

function TACBrONE.ConsultarPlaca(const aVerAplic, aPlaca: string;
  aDataRef: TDateTime): Boolean;
begin
  Result := WebServices.ConsultaPlaca(aVerAplic, aPlaca, aDataRef);

  if not Result then
    GerarException( WebServices.ConsultarPlaca.Msg );
end;

constructor TACBrONE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWebServices := TWebServices.Create(Self);
//  FManutencaoEQP := TManutencaoEQP.Create;
//  FRecepcaoLeitura := TRecepcaoLeitura.Create;
end;

destructor TACBrONE.Destroy;
begin
  FWebServices.Free;
//  FManutencaoEQP.Free;
//  FRecepcaoLeitura.Free;

  inherited;
end;

procedure TACBrONE.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamONE: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
{
  SetStatus( stONEEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamONE, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stIdleONE );
  end;
 }
end;

procedure TACBrONE.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

//  if (Operation = opRemove) and (FDAONE <> nil) and
//    (AComponent is TACBrONEDAONEClass) then
//    FDAONE := nil;
end;

function TACBrONE.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesONE.Create(Self);
end;

function TACBrONE.GetNomeModeloDFe: String;
begin
  Result := 'ONE';
end;

function TACBrONE.GetNameSpaceURI: String;
begin
  Result := ACBRONE_NAMESPACE;
end;

function TACBrONE.GerarNomeArqSchema(const ALayOut: TLayOutONE;
  VersaoServico: Double): String;
var
  NomeServico, NomeSchema, ArqSchema: String;
  Versao: Double;
begin
  // Procura por Versão na pasta de Schemas //
  NomeServico := LayOutONEToServico(ALayOut);
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  ArqSchema := '';
  if NaoEstaVazio(NomeSchema) then
  begin
    Versao := VersaoServico;
    AchaArquivoSchema( NomeSchema, Versao, ArqSchema );
  end;

  Result := ArqSchema;
end;

function TACBrONE.GetConfiguracoes: TConfiguracoesONE;
begin
  Result := TConfiguracoesONE(FPConfiguracoes);
end;

procedure TACBrONE.SetConfiguracoes(AValue: TConfiguracoesONE);
begin
  FPConfiguracoes := AValue;
end;

function TACBrONE.LerVersaoDeParams(LayOutServico: TLayOutONE): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutONEToServico(LayOutServico),
    VersaoONEToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrONE.LerServicoDeParams(LayOutServico: TLayOutONE;
  var Versao: Double; var URL: String);
var
  AUF: String;
begin
  case Configuracoes.Geral.FormaEmissao of
    teNormal: AUF := Configuracoes.WebServices.UF;
    teSVCAN: AUF := 'SVC-AN';
    teSVCRS: AUF := 'SVC-RS';
  else
    AUF := Configuracoes.WebServices.UF;
  end;

  Versao := VersaoONEToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, AUF,
    Configuracoes.WebServices.Ambiente, LayOutONEToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrONE.SetStatus(const stNewStatus: TStatusACBrONE);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;
{
function TACBrONE.EnviarManutencao: Boolean;
begin
  Result := WebServices.EnvManutencao.Executar;

  if not Result then
    GerarException( WebServices.EnvManutencao.Msg );
end;

function TACBrONE.EnviarLeitura: Boolean;
begin
  Result := WebServices.EnvLeitura.Executar;

  if not Result then
    GerarException( WebServices.EnvLeitura.Msg );
end;
}
function TACBrONE.NomeServicoToNomeSchema(const NomeServico: String): String;
var
  ALayout: TLayOutONE;
begin
  ALayout := ServicoToLayOutONE(NomeServico);
  Result := SchemaONEToStr( LayOutToSchema( ALayout ) )
end;

function TACBrONE.DistLeiturasPorCNPJ(TipoDist: TtpDist; const ultNSU,
  NSUFin: string): Boolean;
begin
  WebServices.DistLeituras.tpDist   := TipoDist;
  WebServices.DistLeituras.ultNSU   := ultNSU;
  WebServices.DistLeituras.NSUFin   := NSUFin;
  WebServices.DistLeituras.verAplic := Configuracoes.Geral.verAplic;
  WebServices.DistLeituras.CNPJOper := Configuracoes.Geral.CNPJOper;
  WebServices.DistLeituras.cEQP     := '';
  WebServices.DistLeituras.cUF      := 0;

  Result := WebServices.DistLeituras.Executar;

  if not Result then
    GerarException( WebServices.DistLeituras.Msg );
end;

function TACBrONE.DistLeiturasPorEQP(TipoDist: TtpDist; const ultNSU,
  cEQP, NSUFin: string): Boolean;
begin
  WebServices.DistLeituras.tpDist   := TipoDist;
  WebServices.DistLeituras.ultNSU   := ultNSU;
  WebServices.DistLeituras.cEQP     := cEQP;
  WebServices.DistLeituras.NSUFin   := NSUFin;
  WebServices.DistLeituras.verAplic := Configuracoes.Geral.verAplic;
  WebServices.DistLeituras.CNPJOper := '';
  WebServices.DistLeituras.cUF      := 0;

  Result := WebServices.DistLeituras.Executar;

  if not Result then
    GerarException( WebServices.DistLeituras.Msg );
end;

function TACBrONE.DistLeiturasPorUF(TipoDist: TtpDist; const ultNSU: string;
  cUF: Integer; const NSUFin: string): Boolean;
begin
  WebServices.DistLeituras.tpDist   := TipoDist;
  WebServices.DistLeituras.ultNSU   := ultNSU;
  WebServices.DistLeituras.cUF      := cUF;
  WebServices.DistLeituras.NSUFin   := NSUFin;
  WebServices.DistLeituras.verAplic := Configuracoes.Geral.verAplic;
  WebServices.DistLeituras.CNPJOper := '';
  WebServices.DistLeituras.cEQP     := '';

  Result := WebServices.DistLeituras.Executar;

  if not Result then
    GerarException( WebServices.DistLeituras.Msg );
end;

end.

