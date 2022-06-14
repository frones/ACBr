{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrGTIN;

interface

uses
  Classes, SysUtils, synautil,
  ACBrBase, ACBrDFe, ACBrDFeConfiguracoes, ACBrDFeException, ACBrXmlBase,
  ACBrGTINConfiguracoes, ACBrGTINWebServices, ACBrGTINConversao;

const
  ACBRGTIN_NAMESPACE = 'http://www.portalfiscal.inf.br/nfe';
  ACBRGTIN_CErroAmbDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
     'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrGTINException = class(EACBrDFeException);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrGTIN = class(TACBrDFe)
  private
    FStatus: TStatusACBrGTIN;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesGTIN;

    procedure SetConfiguracoes(AValue: TConfiguracoesGTIN);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function Consultar(const aGTIN: String): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutGTIN; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutGTIN): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaGTIN;

    function GerarNomeArqSchema(const ALayOut: TLayOutGTIN; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Status: TStatusACBrGTIN read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrGTIN);

  published
    property Configuracoes: TConfiguracoesGTIN
      read GetConfiguracoes write SetConfiguracoes;
  end;

implementation

uses
  dateutils,
  ACBrUtil.Base, ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrGTINServicos.rc}
{$ELSE}
 {$R ACBrGTINServicos.res}
{$ENDIF}

{ TACBrGTIN }

constructor TACBrGTIN.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrGTIN.Destroy;
begin
  FWebServices.Free;

  inherited;
end;

function TACBrGTIN.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesGTIN.Create(Self);
end;

function TACBrGTIN.GetNomeModeloDFe: String;
begin
  Result := 'GTIN';
end;

function TACBrGTIN.GetNameSpaceURI: String;
begin
  Result := ACBRGTIN_NAMESPACE;
end;

function TACBrGTIN.IdentificaSchema(const AXML: String): TSchemaGTIN;
begin
  Result := schErro;
end;

function TACBrGTIN.GerarNomeArqSchema(const ALayOut: TLayOutGTIN;
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

function TACBrGTIN.GetConfiguracoes: TConfiguracoesGTIN;
begin
  Result := TConfiguracoesGTIN(FPConfiguracoes);
end;

procedure TACBrGTIN.SetConfiguracoes(AValue: TConfiguracoesGTIN);
begin
  FPConfiguracoes := AValue;
end;

function TACBrGTIN.LerVersaoDeParams(LayOutServico: TLayOutGTIN): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoGTINToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrGTIN.NomeServicoToNomeSchema(const NomeServico: String): String;
Var
  ok: Boolean;
  ALayout: TLayOutGTIN;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);

  if ok then
    Result := SchemaGTINToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBrGTIN.LerServicoDeParams(LayOutServico: TLayOutGTIN;
  var Versao: Double; var URL: String);
begin
  Versao := VersaoGTINToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';

  LerServicoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrGTIN.SetStatus(const stNewStatus: TStatusACBrGTIN);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;

    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrGTIN.Consultar(const aGTIN: String): Boolean;
begin
  WebServices.Consulta.GTIN := aGTIN;
  WebServices.Consulta.Executar;

  Result := True;
end;

end.
