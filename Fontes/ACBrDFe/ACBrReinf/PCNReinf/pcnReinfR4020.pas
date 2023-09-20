{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
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

unit pcnReinfR4020;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.DateTime,
  pcnConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  {Classes específicas deste evento}

  TR4020Collection = class;
  TR4020CollectionItem = class;
  TevtRetPJ = class;
  TideEstab = class;
  TideBenef = class;
  TidePgtoCollection = class;
  TidePgtoCollectionItem = class;
  TinfoPgtoCollection = class;
  TinfoPgtoCollectionItem = class;
  Tretencoes = class;
  TinfoProcRetCollection = class;
  TinfoProcRetCollectionItem = class;
  TinfoProcJud = class;
  TdespProcJud = class;
  TideAdvCollection = class;
  TideAdvCollectionItem = class;
  TinfoPgtoExt = class;
  TendExt = class;

  { TR4020Collection }
  TR4020Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR4020CollectionItem;
    procedure SetItem(Index: Integer; Value: TR4020CollectionItem);
  public
    function Add: TR4020CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR4020CollectionItem;

    property Items[Index: Integer]: TR4020CollectionItem read GetItem write SetItem; default;
  end;

  { TR4020CollectionItem }
  TR4020CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtRetPJ: TevtRetPJ;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtRetPJ: TevtRetPJ read FevtRetPJ write FevtRetPJ;
  end;

  { TevtRetPJ }
  TevtRetPJ = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoComplContri: TinfoComplContri;
    FideEstab: TideEstab;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarideBenef;
    procedure GeraridePgto(Lista: TidePgtoCollection);
    procedure GerarinfoPgto(Lista: TinfoPgtoCollection);
    procedure Gerarretencoes(item: Tretencoes);
    procedure GerarinfoProcRet(Lista: TinfoProcRetCollection);
    procedure GerarinfoProcJud(item: TinfoProcJud);
    procedure GerardespProcJud(item: TdespProcJud);
    procedure GerarideAdv(Lista: TideAdvCollection);
    procedure GerarinfoPgtoExt(item: TinfoPgtoExt);
    procedure GerarendExt(item: TendExt);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property ideEstab: TideEstab read FideEstab write FideEstab;
  end;

  { TideEstab }
  TideEstab = class(TObject)
  private
    FtpInscEstab: TtpInsc;
    FnrInscEstab: string;

    FideBenef: TideBenef;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property ideBenef: TideBenef read FideBenef write FideBenef;
  end;

  { TideBenef }
  TideBenef = class(TObject)
  private
    FcnpjBenef: string;
    FnmBenef: string;
    FisenImun: TtpIsencaoImunidade;
    FideEvtAdic: string;
    FidePgto: TidePgtoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjBenef: string read FcnpjBenef write FcnpjBenef;
    property nmBenef: string read FnmBenef write FnmBenef;
    property isenImun: TtpIsencaoImunidade read FisenImun write FisenImun;
    property ideEvtAdic: string read FideEvtAdic write FideEvtAdic;
    property idePgto: TidePgtoCollection read FidePgto write FidePgto;
  end;

  { TidePgtoCollection }
  TidePgtoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TidePgtoCollectionItem;
    procedure SetItem(Index: Integer; Value: TidePgtoCollectionItem);
  public
    function Add: TidePgtoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TidePgtoCollectionItem;

    property Items[Index: Integer]: TidePgtoCollectionItem read GetItem write SetItem; default;
  end;

  { TidePgtoCollectionItem }
  TidePgtoCollectionItem = class(TObject)
  private
    FnatRend: string;
    Fobserv: string;
    FinfoPgto: TinfoPgtoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property natRend: string read FnatRend write FnatRend;
    property observ: string read Fobserv write Fobserv;
    property infoPgto: TinfoPgtoCollection read FinfoPgto write FinfoPgto;
  end;

  { TinfoPgtoCollection }
  TinfoPgtoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoPgtoCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoPgtoCollectionItem);
  public
    function Add: TinfoPgtoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoPgtoCollectionItem;

    property Items[Index: Integer]: TinfoPgtoCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoPgtoCollectionItem }
  TinfoPgtoCollectionItem = class(TObject)
  private
    FdtFG: TDateTime;
    FvlrBruto: double;
    FindFciScp: string;
    FnrInscFciScp: string;
    FpercSCP: double;
    FindJud: string;
    FpaisResidExt: string;
    FdtEscrCont: TDateTime;
    Fobserv: string;
    Fretencoes: Tretencoes;
    FinfoProcRet: TinfoProcRetCollection;
    FinfoProcJud: TinfoProcJud;
    FinfoPgtoExt: TinfoPgtoExt;
  public
    constructor Create;
    destructor Destroy; override;

    property dtFG: TDateTime read FdtFG write FdtFG;
    property vlrBruto: double read FvlrBruto write FvlrBruto;
    property indFciScp: string read FindFciScp write FindFciScp;
    property nrInscFciScp: string read FnrInscFciScp write FnrInscFciScp;
    property percSCP: double read FpercSCP write FpercSCP;
    property indJud: string read FindJud write FindJud;
    property paisResidExt: string read FpaisResidExt write FpaisResidExt;
    property dtEscrCont: TDateTime read FdtEscrCont write FdtEscrCont;
    property observ: string read Fobserv write Fobserv;
    property retencoes: Tretencoes read Fretencoes write Fretencoes;
    property infoProcRet: TinfoProcRetCollection read FinfoProcRet write FinfoProcRet;
    property infoProcJud: TinfoProcJud read FinfoProcJud write FinfoProcJud;
    property infoPgtoExt: TinfoPgtoExt read FinfoPgtoExt write FinfoPgtoExt;
  end;

  { Tretencoes }
  Tretencoes = class(TObject)
  private
    FvlrBaseIR: double;
    FvlrIR: double;
    FvlrBaseAgreg: double;
    FvlrAgreg: double;
    FvlrBaseCSLL: double;
    FvlrCSLL: double;
    FvlrBaseCofins: double;
    FvlrCofins: double;
    FvlrBasePP: double;
    FvlrPP: double;
  public
    property vlrBaseIR: double read FvlrBaseIR write FvlrBaseIR;
    property vlrIR: double read FvlrIR write FvlrIR;
    property vlrBaseAgreg: double read FvlrBaseAgreg write FvlrBaseAgreg;
    property vlrAgreg: double read FvlrAgreg write FvlrAgreg;
    property vlrBaseCSLL: double read FvlrBaseCSLL write FvlrBaseCSLL;
    property vlrCSLL: double read FvlrCSLL write FvlrCSLL;
    property vlrBaseCofins: double read FvlrBaseCofins write FvlrBaseCofins;
    property vlrCofins: double read FvlrCofins write FvlrCofins;
    property vlrBasePP: double read FvlrBasePP write FvlrBasePP;
    property vlrPP: double read FvlrPP write FvlrPP;
  end;

  { TinfoProcRetCollection }
  TinfoProcRetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetCollectionItem);
  public
    function Add: TinfoProcRetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcRetCollectionItem;

    property Items[Index: Integer]: TinfoProcRetCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoProcRetCollectionItem }
  TinfoProcRetCollectionItem = class(TObject)
  private
    FtpProcRet: TtpProc;
    FnrProcRet: string;
    FcodSusp: string;
    FvlrBaseSuspIR: double;
    FvlrNIR: double;
    FvlrDepIR: double;
    FvlrBaseSuspCSLL: double;
    FvlrNCSLL: double;
    FvlrDepCSLL: double;
    FvlrBaseSuspCofins: double;
    FvlrNCofins: double;
    FvlrDepCofins: double;
    FvlrBaseSuspPP: double;
    FvlrNPP: double;
    FvlrDepPP: double;
  public
    property tpProcRet: TtpProc read FtpProcRet write FtpProcRet;
    property nrProcRet: string read FnrProcRet write FnrProcRet;
    property codSusp: string read FcodSusp write FcodSusp;
    property vlrBaseSuspIR: double read FvlrBaseSuspIR write FvlrBaseSuspIR;
    property vlrNIR: double read FvlrNIR write FvlrNIR;
    property vlrDepIR: double read FvlrDepIR write FvlrDepIR;
    property vlrBaseSuspCSLL: double read FvlrBaseSuspCSLL write FvlrBaseSuspCSLL;
    property vlrNCSLL: double read FvlrNCSLL write FvlrNCSLL;
    property vlrDepCSLL: double read FvlrDepCSLL write FvlrDepCSLL;
    property vlrBaseSuspCofins: double read FvlrBaseSuspCofins write FvlrBaseSuspCofins;
    property vlrNCofins: double read FvlrNCofins write FvlrNCofins;
    property vlrDepCofins: double read FvlrDepCofins write FvlrDepCofins;
    property vlrBaseSuspPP: double read FvlrBaseSuspPP write FvlrBaseSuspPP;
    property vlrNPP: double read FvlrNPP write FvlrNPP;
    property vlrDepPP: double read FvlrDepPP write FvlrDepPP;
  end;

  { TinfoProcJud }
  TinfoProcJud = class(TObject)
  private
    FnrProc: string;
    FindOrigRec: TindOrigemRecursos;
    FcnpjOrigRecurso: string;
    Fdesc: string;
    FdespProcJud: TdespProcJud;
  public
    constructor Create;
    destructor Destroy; override;

    property nrProc: string read FnrProc write FnrProc;
    property indOrigRec: TindOrigemRecursos read FindOrigRec write FindOrigRec;
    property cnpjOrigRecurso: string read FcnpjOrigRecurso write FcnpjOrigRecurso;
    property desc: string read Fdesc write Fdesc;
    property despProcJud: TdespProcJud read FdespProcJud write FdespProcJud;
  end;

  { TdespProcJud }
  TdespProcJud = class(TObject)
  private
    FvlrDespCustas: double;
    FvlrDespAdvogados: double;
    FideAdv: TideAdvCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property vlrDespCustas: double read FvlrDespCustas write FvlrDespCustas;
    property vlrDespAdvogados: double read FvlrDespAdvogados write FvlrDespAdvogados;
    property ideAdv: TideAdvCollection read FideAdv write FideAdv;
  end;

  { TideAdvCollection }
  TideAdvCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideAdvCollectionItem;
    procedure SetItem(Index: Integer; Value: TideAdvCollectionItem);
  public
    function Add: TideAdvCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideAdvCollectionItem;

    property Items[Index: Integer]: TideAdvCollectionItem read GetItem write SetItem; default;
  end;

  { TideAdvCollectionItem }
  TideAdvCollectionItem = class(TObject)
  private
    FtpInscAdv: TtpInsc;
    FnrInscAdv: string;
    FvlrAdv: double;
  public
    property tpInscAdv: TtpInsc read FtpInscAdv write FtpInscAdv;
    property nrInscAdv: string read FnrInscAdv write FnrInscAdv;
    property vlrAdv: double read FvlrAdv write FvlrAdv;
  end;

  { TinfoPgtoExt }
  TinfoPgtoExt = class(TObject)
  private
    FindNIF: TindNIF;
    FnifBenef: string;
    FrelFontPg: string;
    FfrmTribut: string;
    FendExt: TendExt;
  public
    constructor Create;
    destructor Destroy; override;

    property indNIF: TindNIF read FindNIF write FindNIF;
    property nifBenef: string read FnifBenef write FnifBenef;
    property relFontPg: string read FrelFontPg write FrelFontPg;
    property frmTribut: string read FfrmTribut write FfrmTribut;
    property endExt: TendExt read FendExt write FendExt;
  end;

  { TendExt }
  TendExt = class(TObject)
  private
    FdscLograd: string;
    FnrLograd: string;
    Fcomplem: string;
    Fbairro: string;
    Fcidade: string;
    Festado: string;
    FcodPostal: string;
    Ftelef: string;
  public
    property dscLograd: string read FdscLograd write FdscLograd;
    property nrLograd: string read FnrLograd write FnrLograd;
    property complem: string read Fcomplem write Fcomplem;
    property bairro: string read Fbairro write Fbairro;
    property cidade: string read Fcidade write Fcidade;
    property estado: string read Festado write Festado;
    property codPostal: string read FcodPostal write FcodPostal;
    property telef: string read Ftelef write Ftelef;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR4020Collection }

function TR4020Collection.Add: TR4020CollectionItem;
begin
  Result := Self.New;
end;

function TR4020Collection.GetItem(Index: Integer): TR4020CollectionItem;
begin
  Result := TR4020CollectionItem(inherited Items[Index]);
end;

function TR4020Collection.New: TR4020CollectionItem;
begin
  Result := TR4020CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR4020Collection.SetItem(Index: Integer; Value: TR4020CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR4020CollectionItem }

constructor TR4020CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR4020;
  FevtRetPJ := TevtRetPJ.Create(AOwner);
end;

destructor TR4020CollectionItem.Destroy;
begin
  inherited;

  FevtRetPJ.Free;
end;

{ TevtRetPJ }

constructor TevtRetPJ.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri       := TideContri.Create;
  FIdeEvento       := TIdeEvento2.Create;
  FinfoComplContri := TinfoComplContri.Create;
  FideEstab        := TideEstab.Create;
end;

destructor TevtRetPJ.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoComplContri.Free;
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FideBenef := TideBenef.Create;
end;

destructor TideEstab.Destroy;
begin
  FideBenef.Free;

  inherited;
end;

{ TideBenef }

constructor TideBenef.Create;
begin
  FidePgto := TidePgtoCollection.Create;
end;

destructor TideBenef.Destroy;
begin
  FidePgto.Free;

  inherited;
end;

procedure TevtRetPJ.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.ideEstab.nrInscEstab);

  GerarideBenef;

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtRetPJ.GerarideBenef;
begin
  Gerador.wGrupo('ideBenef');

  with Self.ideEstab do
  begin
    Gerador.wCampo(tcStr, '', 'cnpjBenef', 14, 14, 0, ideBenef.cnpjBenef);
    Gerador.wCampo(tcStr, '', 'nmBenef',   1,  70, 0, ideBenef.nmBenef);
    Gerador.wCampo(tcStr, '', 'isenImun',  1,   1, 0, tpIsencaoImunidadeToStr(ideBenef.isenImun));
    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
      Gerador.wCampo(tcStr, '', 'ideEvtAdic',   1,  8, 0, ideBenef.ideEvtAdic);

    GeraridePgto(ideBenef.idePgto);
  end;

  Gerador.wGrupo('/ideBenef');
end;

procedure TevtRetPJ.GeraridePgto(Lista: TidePgtoCollection);
var
  item: TidePgtoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('idePgto');

    Gerador.wCampo(tcStr, '', 'natRend',  5,   5, 1, item.natRend);
    Gerador.wCampo(tcStr, '', 'observ',   1,  30, 0, item.observ);

    GerarinfoPgto(item.infoPgto);

    Gerador.wGrupo('/idePgto');
  end;

  if Lista.Count > 100 then
    Gerador.wAlerta('', 'idePgto', 'Identificação do rendimento', ERR_MSG_MAIOR_MAXIMO + '100');
end;

procedure TevtRetPJ.GerarinfoPgto(Lista: TinfoPgtoCollection);
var
  item: TinfoPgtoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoPgto');
    Gerador.wCampo(tcDat, '', 'dtFG',          10,  10,  1, item.dtFG);
    Gerador.wCampo(tcDe2, '', 'vlrBruto',       1,  14,  1, item.vlrBruto);
    Gerador.wCampo(tcStr, '', 'indFciScp',      1,   1,  0, item.indFciScp);
    Gerador.wCampo(tcStr, '', 'nrInscFciScp',  14,  14,  0, item.nrInscFciScp);
    Gerador.wCampo(tcDe1, '', 'percSCP',        1,   4,  0, item.percSCP);
    Gerador.wCampo(tcStr, '', 'indJud',         1,   1,  0, item.indJud);
    Gerador.wCampo(tcStr, '', 'paisResidExt',   1,   3,  0, item.paisResidExt);

    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
    begin
      Gerador.wCampo(tcDat, '', 'dtEscrCont', 1,  10, 0, item.dtEscrCont);
      Gerador.wCampo(tcStr, '', 'observ',     1, 200, 0, item.observ);
    end;

    Gerarretencoes(item.retencoes);
    GerarinfoProcRet(item.infoProcRet);
    GerarinfoProcJud(item.infoProcJud);
    GerarinfoPgtoExt(item.infoPgtoExt);

    Gerador.wGrupo('/infoPgto');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'infoPgto',
                    'Informações relativas ao rendimento pago/creditado',
                    ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtRetPJ.Gerarretencoes(item: Tretencoes);
begin
  if (item.vlrIR <> 0) or
     (item.vlrAgreg <> 0) or
     (item.vlrCSLL <> 0) or
     (item.vlrCSLL <> 0) or
     (item.vlrCofins <> 0) or
     (item.vlrPP <> 0) then
  begin
    Gerador.wGrupo('retencoes');
    Gerador.wCampo(tcDe2, '', 'vlrBaseIR',        1,  14,  0, item.vlrBaseIR);
    Gerador.wCampo(tcDe2, '', 'vlrIR',            1,  14,  0, item.vlrIR);
    Gerador.wCampo(tcDe2, '', 'vlrBaseAgreg',     1,  14,  0, item.vlrBaseAgreg);
    Gerador.wCampo(tcDe2, '', 'vlrAgreg',         1,  14,  0, item.vlrAgreg);
    Gerador.wCampo(tcDe2, '', 'vlrBaseCSLL',      1,  14,  0, item.vlrBaseCSLL);
    Gerador.wCampo(tcDe2, '', 'vlrCSLL',          1,  14,  0, item.vlrCSLL);
    Gerador.wCampo(tcDe2, '', 'vlrBaseCofins',    1,  14,  0, item.vlrBaseCofins);
    Gerador.wCampo(tcDe2, '', 'vlrCofins',        1,  14,  0, item.vlrCofins);
    Gerador.wCampo(tcDe2, '', 'vlrBasePP',        1,  14,  0, item.vlrBasePP);
    Gerador.wCampo(tcDe2, '', 'vlrPP',            1,  14,  0, item.vlrPP);
    Gerador.wGrupo('/retencoes');
  end;
end;

procedure TevtRetPJ.GerarinfoProcRet(Lista: TinfoProcRetCollection);
var
  item: TinfoProcRetCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcRet');
    Gerador.wCampo(tcStr, '', 'tpProcRet',            1,    1, 1, TpProcToStr(item.tpProcRet));
    Gerador.wCampo(tcStr, '', 'nrProcRet',            1,   21, 1, item.nrProcRet);
    Gerador.wCampo(tcStr, '', 'codSusp',              1,   14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrBaseSuspIR',        1,   14, 0, item.vlrBaseSuspIR);
    Gerador.wCampo(tcDe2, '', 'vlrNIR',               1,   14, 0, item.vlrNIR);
    Gerador.wCampo(tcDe2, '', 'vlrDepIR',             1,   14, 0, item.vlrDepIR);
    Gerador.wCampo(tcDe2, '', 'vlrBaseSuspCSLL',      1,   14, 0, item.vlrBaseSuspCSLL);
    Gerador.wCampo(tcDe2, '', 'vlrNCSLL',             1,   14, 0, item.vlrNCSLL);
    Gerador.wCampo(tcDe2, '', 'vlrDepCSLL',           1,   14, 0, item.vlrDepCSLL);
    Gerador.wCampo(tcDe2, '', 'vlrBaseSuspCofins',    1,   14, 0, item.vlrBaseSuspCofins);
    Gerador.wCampo(tcDe2, '', 'vlrNCofins',           1,   14, 0, item.vlrNCofins);
    Gerador.wCampo(tcDe2, '', 'vlrDepCofins',         1,   14, 0, item.vlrDepCofins);
    Gerador.wCampo(tcDe2, '', 'vlrBaseSuspPP',        1,   14, 0, item.vlrBaseSuspPP);
    Gerador.wCampo(tcDe2, '', 'vlrNPP',               1,   14, 0, item.vlrNPP);
    Gerador.wCampo(tcDe2, '', 'vlrDepPP',             1,   14, 0, item.vlrDepPP);
    Gerador.wGrupo('/infoProcRet');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProcRet',
                    'Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais',
                    ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TevtRetPJ.GerarinfoProcJud(item: TinfoProcJud);
begin
  if item.nrProc <> '' then
  begin
    Gerador.wGrupo('infoProcJud');
    Gerador.wCampo(tcStr, '', 'nrProc',            1,  21,  1, item.nrProc);
    Gerador.wCampo(tcStr, '', 'indOrigRec',        1,   1,  1, indOrigemRecursosToStr(item.indOrigRec));
    Gerador.wCampo(tcStr, '', 'cnpjOrigRecurso',  14,  14,  0, item.cnpjOrigRecurso);
    Gerador.wCampo(tcStr, '', 'desc',              1,  50,  0, item.desc);

    GerardespProcJud(item.despProcJud);

    Gerador.wGrupo('/infoProcJud');
  end;
end;

procedure TevtRetPJ.GerardespProcJud(item: TdespProcJud);
begin
  if item.vlrDespCustas > 0 then
  begin
    Gerador.wGrupo('despProcJud');
    Gerador.wCampo(tcDe2, '', 'vlrDespCustas',     1,  14,  1, item.vlrDespCustas);
    Gerador.wCampo(tcDe2, '', 'vlrDespAdvogados',  1,  14,  1, item.vlrDespAdvogados);

    GerarideAdv(item.ideAdv);

    Gerador.wGrupo('/despProcJud');
  end;
end;

procedure TevtRetPJ.GerarideAdv(Lista: TideAdvCollection);
var
  item: TideAdvCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideAdv');
    Gerador.wCampo(tcStr, '', 'tpInscAdv',  1,   1,  1, TpInscricaoToStr(item.tpInscAdv));
    Gerador.wCampo(tcStr, '', 'nrInscAdv', 11,  14,  1, item.nrInscAdv);
    Gerador.wCampo(tcDe2, '', 'vlrAdv',     1,  14,  0, item.vlrAdv);
    Gerador.wGrupo('/ideAdv');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'ideAdv', 'Identificação do advogado', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPJ.GerarinfoPgtoExt(item: TinfoPgtoExt);
begin
  if item.frmTribut <> '' then
  begin
    Gerador.wGrupo('infoPgtoExt');
    Gerador.wCampo(tcStr, '', 'indNIF',     1,   1,  1, indNIFToStr(item.indNIF));
    Gerador.wCampo(tcStr, '', 'nifBenef',   1,  30,  0, item.nifBenef);
    Gerador.wCampo(tcStr, '', 'relFontPg',  3,   3,  1, item.relFontPg);
    Gerador.wCampo(tcStr, '', 'frmTribut',  2,   2,  1, item.frmTribut);

    GerarendExt(item.endExt);

    Gerador.wGrupo('/infoPgtoExt');
  end;
end;

procedure TevtRetPJ.GerarendExt(item: TendExt);
begin
  // Nenhum campo é obrigatório, validados Logradouro e codigo postal
  if (item.dscLograd <> '') or
     (item.codPostal <> '') then
  begin
    Gerador.wGrupo('endExt');
    Gerador.wCampo(tcStr, '', 'dscLograd',    1,  80,  0, item.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',     1,  10,  0, item.nrLograd);
    Gerador.wCampo(tcStr, '', 'complem',      1,  30,  0, item.complem);
    Gerador.wCampo(tcStr, '', 'bairro',       1,  60,  0, item.bairro);
    Gerador.wCampo(tcStr, '', 'cidade',       1,  40,  0, item.cidade);
    Gerador.wCampo(tcStr, '', 'estado',       1,  40,  0, item.estado);
    Gerador.wCampo(tcStr, '', 'codPostal',    1,  12,  0, item.codPostal);
    Gerador.wCampo(tcStr, '', 'telef',        1,  15,  0, item.telef);
    Gerador.wGrupo('/endExt');
  end;
end;

function TevtRetPJ.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt4020PagtoBeneficiarioPJ');
    Gerador.wGrupo('evtRetPJ id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    GerarideEstab;

    Gerador.wGrupo('/evtRetPJ');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtRetPJ.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, I2, I3: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtRetPJ';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.TpInsc := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'infoComplContri';
      ideContri.infoComplContri.natJur := INIRec.ReadString(sSecao, 'natJur', EmptyStr);

      sSecao := 'ideEstab';
      ideEstab.tpInscEstab := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      ideEstab.nrInscEstab := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);

      with ideEstab do
      begin
        sSecao := 'ideBenef';
        ideBenef.cnpjBenef := INIRec.ReadString(sSecao, 'cnpjBenef', EmptyStr);
        ideBenef.nmBenef   := INIRec.ReadString(sSecao, 'nmBenef', EmptyStr);
        ideBenef.isenImun  := StrToTpIsencaoImunidade(Ok, INIRec.ReadString(sSecao, 'isenImun', EmptyStr));
        if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
          ideBenef.ideEvtAdic := INIRec.ReadString(sSecao, 'ideEvtAdic', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 100
          sSecao := 'idePgto' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'natRend', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideBenef.idePgto.New do
          begin
            natRend := sFim;
            observ  := INIRec.ReadString(sSecao, 'observ', '');

            I2 := 1;
            while true do
            begin
              // de 1 até 999
              sSecao := 'infoPgto' + IntToStrZero(I, 3) + IntToStrZero(I2, 3);
              sFim   := INIRec.ReadString(sSecao, 'dtFG', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoPgto.New do
              begin
                dtFG         := StringToDateTime(sFim);
                vlrBruto     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
                indFciScp    := INIRec.ReadString(sSecao, 'indFciScp', '');
                nrInscFciScp := INIRec.ReadString(sSecao, 'nrInscFciScp', '');
                percSCP      := StringToFloatDef(INIRec.ReadString(sSecao, 'percSCP', ''), 0);
                indJud       := INIRec.ReadString(sSecao, 'indJud', '');
                paisResidExt := INIRec.ReadString(sSecao, 'paisResidExt', '');

                if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
                begin
                  dtEscrCont := StringToDateTime(INIRec.ReadString(sSecao, 'dtEscrCont', ''));
                  observ     := INIRec.ReadString(sSecao, 'observ', '');
                end;

                sSecao := 'retencoes' + IntToStrZero(I, 3) +
                                        IntToStrZero(I2, 3);

                with retencoes do
                begin
                  vlrBaseIR     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseIR', ''), 0);
                  vlrIR         := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIR', ''), 0);
                  vlrBaseAgreg  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseAgreg', ''), 0);
                  vlrAgreg      := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAgreg', ''), 0);
                  vlrBaseCSLL   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseCSLL', ''), 0);
                  vlrCSLL       := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCSLL', ''), 0);
                  vlrBaseCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseCofins', ''), 0);
                  vlrCofins     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCofins', ''), 0);
                  vlrBasePP     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBasePP', ''), 0);
                  vlrPP         := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPP', ''), 0);
                end;

                I3 := 1;
                while true do
                begin
                  // de 1 até 50
                  sSecao := 'infoProcRet' + IntToStrZero(I, 3) +
                                            IntToStrZero(I2, 3) +
                                            IntToStrZero(I3, 3);

                  sFim := INIRec.ReadString(sSecao, 'tpProcRet', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoProcRet.New do
                  begin
                    tpProcRet         := StrToTpProc(Ok, sFim);
                    nrProcRet         := INIRec.ReadString(sSecao, 'nrProcRet', '');
                    codSusp           := INIRec.ReadString(sSecao, 'codSusp', '');
                    vlrBaseSuspIR     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseSuspIR', ''), 0);
                    vlrNIR            := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNIR', ''), 0);
                    vlrDepIR          := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepIR', ''), 0);
                    vlrBaseSuspCSLL   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseSuspCSLL', ''), 0);
                    vlrNCSLL          := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNCSLL', ''), 0);
                    vlrDepCSLL        := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepCSLL', ''), 0);
                    vlrBaseSuspCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseSuspCofins', ''), 0);
                    vlrNCofins        := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNCofins', ''), 0);
                    vlrDepCofins      := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepCofins', ''), 0);
                    vlrBaseSuspPP     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseSuspPP', ''), 0);
                    vlrNPP            := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNPP', ''), 0);
                    vlrDepPP          := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepPP', ''), 0);
                  end;

                  Inc(I3);
                end;

                sSecao := 'infoProcJud' + IntToStrZero(I, 3) +
                                          IntToStrZero(I2, 3);

                with infoProcJud do
                begin
                  nrProc          := INIRec.ReadString(sSecao, 'nrProc', '');
                  indOrigRec      := StrToindOrigemRecursos(Ok, INIRec.ReadString(sSecao, 'indOrigRec', ''));
                  cnpjOrigRecurso := INIRec.ReadString(sSecao, 'cnpjOrigRecurso', '');
                  desc            := INIRec.ReadString(sSecao, 'desc', '');

                  sSecao := 'despProcJud' + IntToStrZero(I, 3) +
                                            IntToStrZero(I2, 3);

                  with despProcJud do
                  begin
                    vlrDespCustas    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', ''),0);
                    vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', ''),0);

                    I3 := 1;
                    while true do
                    begin
                      // de 1 até 99
                      sSecao := 'ideAdv' + IntToStrZero(I, 3) +
                                           IntToStrZero(I2, 3) +
                                           IntToStrZero(I3, 3);

                      sFim := INIRec.ReadString(sSecao, 'tpInscAdv', 'FIM');

                      if (sFim = 'FIM') or (Length(sFim) <= 0) then
                        break;

                      with ideAdv.New do
                      begin
                        tpInscAdv := StrToTpInscricao(ok,sFim);
                        nrInscAdv := INIRec.ReadString(sSecao, 'nrInscAdv', '');
                        vlrAdv    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdv', ''), 0);
                      end;

                      Inc(I3);
                    end;
                  end;
                end;

                sSecao := 'infoPgtoExt' + IntToStrZero(I, 3) +
                                          IntToStrZero(I2, 3);

                with infoPgtoExt do
                begin
                  indNIF    := StrToindNIF(Ok, INIRec.ReadString(sSecao, 'indNIF', ''));
                  nifBenef  := INIRec.ReadString(sSecao, 'nifBenef', '');
                  relFontPg := INIRec.ReadString(sSecao, 'relFontPg', '');
                  frmTribut := INIRec.ReadString(sSecao, 'frmTribut', '');

                  sSecao := 'endExt' + IntToStrZero(I, 3) +
                                       IntToStrZero(I2, 3);

                  with endExt do
                  begin
                    dscLograd := INIRec.ReadString(sSecao, 'dscLograd', '');
                    nrLograd := INIRec.ReadString(sSecao, 'nrLograd', '');
                    complem := INIRec.ReadString(sSecao, 'complem', '');
                    bairro := INIRec.ReadString(sSecao, 'bairro', '');
                    cidade := INIRec.ReadString(sSecao, 'cidade', '');
                    estado := INIRec.ReadString(sSecao, 'estado', '');
                    codPostal := INIRec.ReadString(sSecao, 'codPostal', '');
                    telef := INIRec.ReadString(sSecao, 'telef', '');
                  end;
                end;
              end;
              Inc(I2);
            end;
          end;

          Inc(I);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TidePgtoCollection }

function TidePgtoCollection.Add: TidePgtoCollectionItem;
begin
  Result := Self.New;
end;

function TidePgtoCollection.GetItem(
  Index: Integer): TidePgtoCollectionItem;
begin
  Result := TidePgtoCollectionItem(inherited Items[Index]);
end;

function TidePgtoCollection.New: TidePgtoCollectionItem;
begin
  Result := TidePgtoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TidePgtoCollection.SetItem(Index: Integer;
  Value: TidePgtoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TidePgtoCollectionItem }

constructor TidePgtoCollectionItem.Create;
begin
  FinfoPgto := TinfoPgtoCollection.Create;
end;

destructor TidePgtoCollectionItem.Destroy;
begin
  FinfoPgto.Free;

  inherited;
end;

{ TinfoPgtoCollection }

function TinfoPgtoCollection.Add: TinfoPgtoCollectionItem;
begin
  Result := Self.New;
end;

function TinfoPgtoCollection.GetItem(
  Index: Integer): TinfoPgtoCollectionItem;
begin
  Result := TinfoPgtoCollectionItem(inherited Items[Index]);
end;

function TinfoPgtoCollection.New: TinfoPgtoCollectionItem;
begin
  Result := TinfoPgtoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoPgtoCollection.SetItem(Index: Integer;
  Value: TinfoPgtoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoPgtoCollectionItem }

constructor TinfoPgtoCollectionItem.Create;
begin
  Fretencoes := Tretencoes.Create;
  FinfoProcRet := TinfoProcRetCollection.Create;
  FinfoProcJud := TinfoProcJud.Create;
  FinfoPgtoExt := TinfoPgtoExt.Create;
end;

destructor TinfoPgtoCollectionItem.Destroy;
begin
  Fretencoes.Free;
  FinfoProcRet.Free;
  FinfoProcJud.Free;
  FinfoPgtoExt.Free;

  inherited;
end;

{ TinfoProcRetCollection }

function TinfoProcRetCollection.Add: TinfoProcRetCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcRetCollection.GetItem(
  Index: Integer): TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem(inherited Items[Index]);
end;

function TinfoProcRetCollection.New: TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcRetCollection.SetItem(Index: Integer;
  Value: TinfoProcRetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcJud }

constructor TinfoProcJud.Create;
begin
  FdespProcJud := TdespProcJud.Create;
end;

destructor TinfoProcJud.Destroy;
begin
  FdespProcJud.Free;

  inherited;
end;

{ TdespProcJud }

constructor TdespProcJud.Create;
begin
  FideAdv := TideAdvCollection.Create;
end;

destructor TdespProcJud.Destroy;
begin
  FideAdv.Free;

  inherited;
end;

{ TideAdvCollection }

function TideAdvCollection.Add: TideAdvCollectionItem;
begin
  Result := Self.New;
end;

function TideAdvCollection.GetItem(Index: Integer): TideAdvCollectionItem;
begin
  Result := TideAdvCollectionItem(inherited Items[Index]);
end;

function TideAdvCollection.New: TideAdvCollectionItem;
begin
  Result := TideAdvCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideAdvCollection.SetItem(Index: Integer;
  Value: TideAdvCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoPgtoExt }

constructor TinfoPgtoExt.Create;
begin
  FendExt := TendExt.Create;
end;

destructor TinfoPgtoExt.Destroy;
begin
  FendExt.Free;

  inherited;
end;

end.

