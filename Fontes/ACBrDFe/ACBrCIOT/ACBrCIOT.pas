{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrCIOT;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeConfiguracoes, ACBrDFeException, ACBrBase,
  ACBrCIOTConfiguracoes, ACBrCIOTWebServices, ACBrCIOTContratos,
  pcnConversao, pcnConversaoCIOT;

const
  ACBRCIOT_VERSAO = '1.0.0';
  ACBRCIOT_NAMESPACE = '';
  ACBRCIOT_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
               'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrCIOTException = class(EACBrDFeException);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  TACBrCIOT = class(TACBrDFe)
  private
    FContratos: TContratos;
    FStatus: TStatusACBrCIOT;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesCIOT;
    procedure SetConfiguracoes(AValue: TConfiguracoesCIOT);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamCIOT: TStream = nil; const NomeArq: String = ''; 
	  sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function Enviar(const ANomePDF: String = ''): Boolean;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmado(AValue: integer): Boolean;
    function cStatProcessado(AValue: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutCIOT; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutCIOT): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaCIOT;

    function GerarNomeArqSchema(const ALayOut: TLayOutCIOT; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Contratos: TContratos read FContratos write FContratos;
    property Status: TStatusACBrCIOT read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrCIOT);

  published
    property Configuracoes: TConfiguracoesCIOT read GetConfiguracoes write SetConfiguracoes;
  end;

implementation

uses
  dateutils,
  ACBrUtil.Base,
  ACBrUtil.Strings;

{$IFDEF FPC}
 {$R ACBrCIOTServicos.rc}
{$ELSE}
 {$R ACBrCIOTServicos.res}
{$ENDIF}

{ TACBrCIOT }

constructor TACBrCIOT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FContratos := TContratos.Create(Self, Contrato);
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrCIOT.Destroy;
begin
  FContratos.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrCIOT.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamCIOT: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stCIOTEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamCIOT, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stCIOTIdle );
  end;
end;

procedure TACBrCIOT.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  {
  if (Operation = opRemove) and (FDACIOT <> nil) and
     (AComponent is TACBrCIOTDACIOTClass) then
    FDACIOT := nil;
 }
end;

function TACBrCIOT.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesCIOT.Create(Self);
end;

function TACBrCIOT.GetNomeModeloDFe: String;
begin
  Result := 'CIOT';
end;

function TACBrCIOT.GetNameSpaceURI: String;
begin
  Result := ACBRCIOT_NAMESPACE;
end;

function TACBrCIOT.cStatConfirmado(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
  else
    Result := False;
  end;
end;

function TACBrCIOT.cStatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
  else
    Result := False;
  end;
end;

function TACBrCIOT.IdentificaSchema(const AXML: String): TSchemaCIOT;
//var
// lTipoEvento: TpcnTpEvento;
// I: Integer;
// Ok: Boolean;
begin
  Result := schEnviar;
(*
  Result := schCIOT;

  I := pos('<infCIOT', AXML);
  if I = 0 then
  begin
    I := pos('<infEvento', AXML);
    if I > 0 then
    begin
      lTipoEvento := StrToTpEvento(Ok, Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>')));

      case lTipoEvento of
        teCancelamento: Result := schevCancCIOT;
        teEncerramento: Result := schevEncCIOT;
        else Result := schevIncCondutorCIOT;
      end;
    end;
  end;
*)
end;

function TACBrCIOT.GerarNomeArqSchema(const ALayOut: TLayOutCIOT;
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

function TACBrCIOT.GetConfiguracoes: TConfiguracoesCIOT;
begin
  Result := TConfiguracoesCIOT(FPConfiguracoes);
end;

procedure TACBrCIOT.SetConfiguracoes(AValue: TConfiguracoesCIOT);
begin
  FPConfiguracoes := AValue;
end;

function TACBrCIOT.LerVersaoDeParams(LayOutServico: TLayOutCIOT): String;
var
  Versao: Double;
  UF: String;
begin
  // Para qualquer UF a URL é sempre a mesma.
  UF := 'XX';
  Versao := LerVersaoDeParams(GetNomeModeloDFe, UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoCIOTToint(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrCIOT.NomeServicoToNomeSchema(const NomeServico: String): String;
//var
//  ok: Boolean;
//  ALayout: TLayOutCIOT;
begin
  Result := '';
  (*
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaCIOTToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
  *)
end;

procedure TACBrCIOT.LerServicoDeParams(LayOutServico: TLayOutCIOT;
  var Versao: Double; var URL: String);
var
  UF: String;
begin
  Versao := VersaoCIOTToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  // Para qualquer UF a URL é sempre a mesma.
  UF := 'XX';
  LerServicoDeParams(GetNomeModeloDFe, UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrCIOT.SetStatus(const stNewStatus: TStatusACBrCIOT);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrCIOT.Enviar(const ANomePDF: String = ''): Boolean;
begin
  if Contratos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum CIOT adicionado'));

  if Contratos.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de CIOT transmitidos (máximo de 1 CIOT)' +
      ' excedido. Quantidade atual: ' + IntToStr(Contratos.Count)));

  Contratos.Assinar;

  Result := WebServices.Envia(ANomePDF);
end;

end.
