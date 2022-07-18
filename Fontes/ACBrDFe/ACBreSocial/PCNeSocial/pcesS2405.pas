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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS2405;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase, pcnConversao, ACBrUtil.Strings, pcnConsts,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2405Collection = class;
  TS2405CollectionItem = class;
  TEvtCdBenefAlt = class;
  TIdeBenef = class;
  TAlteracao = class;
  TDadosBenef = class;
  
  TS2405Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2405CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2405CollectionItem);
  public
    function Add: TS2405CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2405CollectionItem;
    property Items[Index: Integer]: TS2405CollectionItem read GetItem write SetItem; default;
  end;

  TS2405CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtCdBenefAlt: TEvtCdBenefAlt;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtCdBenefAlt: TEvtCdBenefAlt read FEvtCdBenefAlt write FEvtCdBenefAlt;
  end;

  TIdeBenef = class(TObject)
  private
    FCpfBenef: string;
  public
    property CpfBenef: string read FCpfBenef write FCpfBenef;
  end;
  
  TAlteracao = class(TObject)
  private
    FDtAlteracao: TDateTime;
    FDadosBenef: TDadosBenef;
  public
    constructor Create;
    destructor Destroy; override;
    
    property DtAlteracao: TDateTime read FDtAlteracao write FDtAlteracao;
    property DadosBenef: TDadosBenef read FDadosBenef write FDadosBenef;
  end;
  
  TEvtCdBenefAlt = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;
    FAlteracao: TAlteracao;
    
    procedure GerarIdeBenef(pIdeBenef: TIdeBenef);
    procedure GerarAlteracao(pAlteracao: TAlteracao);
    procedure GerarDadosBenef(pDadosBenef: TDadosBenef);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeBenef: TIdeBenef read FIdeBenef write FIdeBenef;
    property Alteracao: TAlteracao read FAlteracao write FAlteracao;
  end;
  
  TDadosBenef = class(TObject)
  private
    FNmBenefic: string;
    FSexo: string;
    FRacaCor: integer;
    FEstCiv: integer;
    FIncFisMen: TpSimNao;
    FEndereco: TEndereco;
    FDependente: TDependenteCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    property nmBenefic: string read FNmBenefic write FNmBenefic;
    property sexo: string read FSexo write FSexo;
    property racaCor: integer read FRacaCor write FRacaCor;
    property estCiv: integer read FEstCiv write FEstCiv;
    property incFisMen: TpSimNao read FIncFisMen write FIncFisMen;
    property endereco: TEndereco read FEndereco write FEndereco;
    property dependente: TDependenteCollection read FDependente write FDependente;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2405Collection }

function TS2405Collection.Add: TS2405CollectionItem;
begin
  Result := Self.New;
end;

function TS2405Collection.GetItem(Index: Integer): TS2405CollectionItem;
begin
  Result := TS2405CollectionItem(inherited Items[Index]);
end;

procedure TS2405Collection.SetItem(Index: Integer; Value: TS2405CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2405Collection.New: TS2405CollectionItem;
begin
  Result := TS2405CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2405CollectionItem }

constructor TS2405CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento    := teS2405;
  FEvtCdBenefAlt := TEvtCdBenefAlt.Create(AOwner);
end;

destructor TS2405CollectionItem.Destroy;
begin
  FEvtCdBenefAlt.Free;

  inherited;
end;

{ TDadosBenef }

constructor TDadosBenef.Create;
begin
  inherited Create;
  FEndereco := TEndereco.Create;
  FDependente := TDependenteCollection.Create;
end;

destructor TDadosBenef.Destroy;
begin
  FEndereco.Free;
  FDependente.Free;
  inherited;
end;

{ TAlteracao }

constructor TAlteracao.Create;
begin
  inherited Create;
  FDadosBenef := TDadosBenef.Create;
end;

destructor TAlteracao.Destroy;
begin
  FDadosBenef.Free;
  inherited;
end;

{ TEvtCdBenefAlt }

constructor TEvtCdBenefAlt.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef      := TIdeBenef.Create;
  FAlteracao     := TAlteracao.Create;
end;

destructor TEvtCdBenefAlt.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;
  FAlteracao.Free;
  
  inherited;
end;

procedure TEvtCdBenefAlt.GerarIdeBenef(pIdeBenef: TIdeBenef);
begin
  Gerador.wGrupo('ideBenef');
  
  Gerador.wCampo(tcStr, '', 'cpfBenef',    11, 11, 1, pIdeBenef.cpfBenef);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtCdBenefAlt.GerarAlteracao(pAlteracao: TAlteracao);
begin
  Gerador.wGrupo('alteracao');

  Gerador.wCampo(tcDat, '', 'dtAlteracao', 10, 10, 1, pAlteracao.dtAlteracao);
  
  GerarDadosBenef(pAlteracao.DadosBenef);

  Gerador.wGrupo('/alteracao');
end;

procedure TEvtCdBenefAlt.GerarDadosBenef(pDadosBenef: TDadosBenef);
begin
  Gerador.wGrupo('dadosBenef');

  Gerador.wCampo(tcStr, '', 'nmBenefic',   70, 70, 1, pDadosBenef.nmBenefic);
  Gerador.wCampo(tcStr, '', 'sexo',         1,  1, 1, pDadosBenef.sexo);
  Gerador.wCampo(tcInt, '', 'racaCor',      1,  1, 1, pDadosBenef.racaCor);

  if ((pDadosBenef.EstCiv >= 1) and (pDadosBenef.EstCiv <= 5)) then
    Gerador.wCampo(tcInt, '', 'estCiv',     1,  1, 0, pDadosBenef.estCiv);

  Gerador.wCampo(tcStr, '', 'incFisMen',    1,  1, 1, eSSimNaoToStr(pDadosBenef.incFisMen));
  
  GerarEndereco(pDadosBenef.endereco, (pDadosBenef.endereco.exterior.paisResid <> ''));

  GerarDependente(pDadosBenef.dependente, true);
 
  Gerador.wGrupo('/dadosBenef');
end;

function TEvtCdBenefAlt.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCdBenefAlt');
    Gerador.wGrupo('evtCdBenefAlt Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeBenef(self.IdeBenef);
    GerarAlteracao(self.Alteracao);
    
    Gerador.wGrupo('/evtCdBenefAlt');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'EvtCdBenefAlt');

//    Validar(schEvtCdBenefAlt);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCdBenefAlt.LerArqIni(const AIniString: String): Boolean;
//var
//  INIRec: TMemIniFile;
//  Ok: Boolean;
//  sSecao: String;
begin
  Result := True;
end;

end.
