{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{$I ACBr.inc}

unit pnfsNFSeW;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, pcnGerador, pnfsNFSe, pnfsConversao;

type

  TNFSeW = class;
  TGeradorOpcoes = class;

  { TNFSeWClass }

  TNFSeWClass = class
  private
  protected
    FpNFSeW: TNFSeW;

    FGerador: TGerador;
    FOpcoes: TGeradorOpcoes;

    FNFSe: TNFSe;
    FProvedor: TnfseProvedor;
    FAtributo: String;
    FPrefixo3: String;
    FPrefixo4: String;
    FIdentificador: String;
    FURL: String;
    FVersaoNFSe: TVersaoNFSe;
    FDefTipos: String;
    FServicoEnviar: String;
    FQuebradeLinha: String;
    FVersaoDados: String;

  public
    constructor Create(ANFSeW: TNFSeW); virtual;
    destructor  Destroy; Override;

    function ObterNomeArquivo: String; virtual;
    function GerarXml: Boolean; virtual;

    property Gerador: TGerador       read FGerador       write FGerador;
    property Opcoes: TGeradorOpcoes  read FOpcoes        write FOpcoes;

    property NFSe: TNFSe             read FNFSe          write FNFSe;
    property Provedor: TnfseProvedor read FProvedor      write FProvedor;
    property Atributo: String        read FAtributo      write FAtributo;
    property Prefixo3: String        read FPrefixo3      write FPrefixo3;
    property Prefixo4: String        read FPrefixo4      write FPrefixo4;
    property Identificador: String   read FIdentificador write FIdentificador;
    property URL: String             read FURL           write FURL;
    property VersaoNFSe: TVersaoNFSe read FVersaoNFSe    write FVersaoNFSe;
    property DefTipos: String        read FDefTipos      write FDefTipos;
    property ServicoEnviar: String   read FServicoEnviar write FServicoEnviar;
    property QuebradeLinha: String   read FQuebradeLinha write FQuebradeLinha;
    property VersaoDados: String     read FVersaoDados   write FVersaoDados;
  end;

  { TNFSeW }

  TNFSeW = class
  private
    FLayOutXML: TLayOutXML;
    FNFSeWClass: TNFSeWClass;
    FNFSe: TNFSe;

    procedure SetLayOutXML(ALayOutXML: TLayOutXML);

  public
    constructor Create(AOwner: TNFSe);
    procedure Clear;
    destructor Destroy; override;

    function GerarXml: Boolean;

    property LayOutXML: TLayOutXML   read FLayOutXML   write SetLayOutXML;
    property NFSeWClass: TNFSeWClass read FNFSeWClass;
    property NFSe: TNFSe             read FNFSe        write FNFSe;
  end;

 TGeradorOpcoes = class(TPersistent)
  private
    FAjustarTagNro: Boolean;
    FNormatizarMunicipios: Boolean;
    FGerarTagAssinatura: TnfseTagAssinatura;
    FPathArquivoMunicipios: String;
    FValidarInscricoes: Boolean;
    FValidarListaServicos: Boolean;
  published
    property AjustarTagNro: Boolean                 read FAjustarTagNro         write FAjustarTagNro;
    property NormatizarMunicipios: Boolean          read FNormatizarMunicipios  write FNormatizarMunicipios;
    property GerarTagAssinatura: TnfseTagAssinatura read FGerarTagAssinatura    write FGerarTagAssinatura;
    property PathArquivoMunicipios: String          read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: Boolean             read FValidarInscricoes     write FValidarInscricoes;
    property ValidarListaServicos: Boolean          read FValidarListaServicos  write FValidarListaServicos;
  end;

implementation

uses
  ACBrDFeException,
  pnfsNFSeW_ABRASFv1, pnfsNFSeW_ABRASFv2, pnfsNFSeW_EGoverneISS, pnfsNFSeW_EL,
  pnfsNFSeW_Equiplano, pnfsNFSeW_Infisc, pnfsNFSeW_ISSDSF, pnfsNFSeW_Governa,
  pnfsNFSeW_SP, pnfsNFSeW_CONAM, pnfsNFSeW_Agili, pnfsNFSeW_SMARAPD, pnfsNFSeW_IPM,
  pnfsNFSeW_AssessorPublico, pnfsNFSeW_WEBFISCO;

{ TNFSeW }

constructor TNFSeW.Create(AOwner: TNFSe);
begin
  inherited Create;

  FNFSe := AOwner;

  Clear;
end;

procedure TNFSeW.Clear;
begin
  FLayOutXML     := loNone;

  if Assigned(FNFSeWClass) then
    FNFSeWClass.Free;

  FNFSeWClass := TNFSeWClass.Create(Self);
end;

destructor TNFSeW.Destroy;
begin
  if Assigned(FNFSeWClass) then
    FreeAndNil(FNFSeWClass);

  inherited Destroy;
end;

function TNFSeW.GerarXml: Boolean;
begin
  Result := FNFSeWClass.GerarXml;
end;

procedure TNFSeW.SetLayOutXML(ALayOutXML: TLayOutXML);
begin
  if ALayOutXML = FLayOutXML then
    exit;

  if Assigned(FNFSeWClass) then
    FreeAndNil(FNFSeWClass);

  case ALayOutXML of
    loABRASFv1:    FNFSeWClass := TNFSeW_ABRASFv1.Create(Self);
    loABRASFv2:    FNFSeWClass := TNFSeW_ABRASFv2.Create(Self);
    loEGoverneISS: FNFSeWClass := TNFSeW_EGoverneISS.Create(Self);
    loEL:          FNFSeWClass := TNFSeW_EL.Create(Self);
    loEquiplano:   FNFSeWClass := TNFSeW_Equiplano.Create(Self);
    loGoverna:     FNFSeWClass := TNFSeW_Governa.Create(Self);
    loInfisc:      FNFSeWClass := TNFSeW_Infisc.Create(Self);
    loISSDSF:      FNFSeWClass := TNFSeW_ISSDSF.Create(Self);
    loSP:          FNFSeWClass := TNFSeW_SP.Create(Self);
    loCONAM:       FNFSeWClass := TNFSeW_CONAM.Create(Self);
    loAgili:       FNFSeWClass := TNFSeW_Agili.Create(Self);
    loSMARAPD:     FNFSeWClass := TNFSeW_SMARAPD.Create(Self);
    loIPM:         FNFSeWClass := TNFSeW_IPM.Create(Self);
    loAssessorPublico : FNFSeWClass := TNFSeW_AssesorPublico.Create(Self);
    loWEBFISCO:    FNFSeWClass := TNFSeW_WEBFISCO.Create(Self);
  else
    FNFSeWClass := TNFSeWClass.Create(Self);
  end;

  FNFSeWClass.FNFSe := FNFSe;

  FLayOutXML := ALayOutXML;
end;

{ TNFSeWClass }

constructor TNFSeWClass.Create(ANFSeW: TNFSeW);
begin
  FpNFSeW := ANFSeW;

  FGerador := TGerador.Create;
  FGerador.FIgnorarTagNivel := '|?xml version|NFSe xmlns|infNFSe versao|obsCont|obsFisco|';

  FOpcoes := TGeradorOpcoes.Create;
  FOpcoes.FAjustarTagNro        := True;
  FOpcoes.FNormatizarMunicipios := False;
  FOpcoes.FGerarTagAssinatura   := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes    := False;
  FOpcoes.FValidarListaServicos := False;

  FAtributo      := '';
  FPrefixo3      := '';
  FPrefixo4      := '';
  FIdentificador := '';
  FURL           := '';
  FVersaoNFSe    := ve100;
  FDefTipos      := '';
  FServicoEnviar := '';
  FQuebradeLinha := ';';
  FVersaoDados   := '1.0';
end;

function TNFSeWClass.ObterNomeArquivo: String;
begin
  Result := '';
  raise EACBrDFeException.Create(ClassName + '.ObterNomeArquivo, não implementado');
end;

destructor TNFSeWClass.Destroy;
begin
  FOpcoes.Free;
  FGerador.Free;
  inherited Destroy;
end;

function TNFSeWClass.GerarXml: Boolean;
begin
//  Result := False;
  raise EACBrDFeException.Create(ClassName + '.GerarXml, não implementado');
end;

end.
