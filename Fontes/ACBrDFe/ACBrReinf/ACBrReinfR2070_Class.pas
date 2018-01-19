{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

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
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

unit ACBrReinfR2070_Class;

interface

uses Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs;

type
  TpgtoPFs = class;
  TideEstabs = class;
  TdetDeducoes = class;
  TrendIsentos = class;
  TinfoRRAs = class;
  TideAdvogados = class;
  TinfoProcJuds = class;
  TdespProcJud = class;
  TpgtoPJs = class;

  { TpgtoResidExt }
  TpgtoResidExt = class
  private
    FdtPagto         : TDateTime;
    FtpRendimento    : String;
    FformaTributacao : String;
    FvlrPgto         : double;
    FvlrRet          : double;
  public
    property dtPagto : TDateTime read FdtPagto write FdtPagto;
    property tpRendimento : String read FtpRendimento write FtpRendimento;
    property formaTributacao : String read FformaTributacao write FformaTributacao;
    property vlrPgto : double read FvlrPgto write FvlrPgto;
    property vlrRet : double read FvlrRet write FvlrRet;
  end;

  { TpgtoPJ }
  TpgtoPJ = class
  private
    FdtPagto           : TDateTime;
    FvlrRendTributavel : double;
    FvlrRet            : double;
    FinfoProcJuds      : TinfoProcJuds;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property dtPagto : TDateTime read FdtPagto write FdtPagto;
    property vlrRendTributavel : double read FvlrRendTributavel write FvlrRendTributavel;
    property vlrRet : double read FvlrRet write FvlrRet;
    property infoProcJuds : TinfoProcJuds read FinfoProcJuds;
  end;

  { TpgtoPJs }
  TpgtoPJs = class(TObjectList)
  private
    function GetItem(Index: Integer): TpgtoPJ;
    procedure SetItem(Index: Integer; const Value: TpgtoPJ);
  public
    function New: TpgtoPJ;

    property Items[Index: Integer]: TpgtoPJ read GetItem write SetItem;
  end;

  { TdepJudicial }
  TdepJudicial = class
  private
    FvlrDepJudicial : double;
  public
    property vlrDepJudicial : double read FvlrDepJudicial write FvlrDepJudicial;
  end;

  { TorigemRecursos }
  TorigemRecursos = class
  private
    FcnpjOrigemRecursos : String;
  public
    property cnpjOrigemRecursos : String read FcnpjOrigemRecursos write FcnpjOrigemRecursos;
  end;

  { TinfoProcJud }
  TinfoProcJud = class
  private
    FnrProcJud         : String;
    FcodSusp           : String;
    FindOrigemRecursos : TindOrigemRecursos;
    FdespProcJud       : TdespProcJud;
    ForigemRecursos    : TorigemRecursos;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property nrProcJud : String read FnrProcJud write FnrProcJud;
    property codSusp : String read FcodSusp write FcodSusp;
    property indOrigemRecursos : TindOrigemRecursos read FindOrigemRecursos write FindOrigemRecursos;
    property despProcJud : TdespProcJud read FdespProcJud write FdespProcJud;
    property origemRecursos : TorigemRecursos read ForigemRecursos write ForigemRecursos;
  end;

  { TinfoProcJuds }
  TinfoProcJuds = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoProcJud;
    procedure SetItem(Index: Integer; const Value: TinfoProcJud);
  public
    function New: TinfoProcJud;

    property Items[Index: Integer]: TinfoProcJud read GetItem write SetItem;
  end;

  { TideAdvogado }
  TideAdvogado = class
  private
    FtpInscAdvogado : tpTpInsc;
    FnrInscAdvogado : String;
    FvlrAdvogado    : double;
  public
    property tpInscAdvogado : tpTpInsc read FtpInscAdvogado write FtpInscAdvogado;
    property nrInscAdvogado : String read FnrInscAdvogado write FnrInscAdvogado;
    property vlrAdvogado : double read FvlrAdvogado write FvlrAdvogado;
  end;

  { TideAdvogados }
  TideAdvogados = class(TObjectList)
  private
    function GetItem(Index: Integer): TideAdvogado;
    procedure SetItem(Index: Integer; const Value: TideAdvogado);
  public
    function New: TideAdvogado;

    property Items[Index: Integer]: TideAdvogado read GetItem write SetItem;
  end;

  { TdespProcJud }
  TdespProcJud = class
  private
    FvlrDespCustas    : double;
    FvlrDespAdvogados : double;
    FideAdvogados     : TideAdvogados;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property vlrDespCustas : double read FvlrDespCustas write FvlrDespCustas;
    property vlrDespAdvogados : double read FvlrDespAdvogados write FvlrDespAdvogados;
    property ideAdvogados : TideAdvogados read FideAdvogados;
  end;

  { TinfoRRA }
  TinfoRRA = class
  private
    FtpProcRRA   : tpTpProc;
    FnrProcRRA   : String;
    FcodSusp     : String;
    FnatRRA      : String;
    FqtdMesesRRA : Integer;    FdespProcJud : TdespProcJud;  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property tpProcRRA : tpTpProc read FtpProcRRA write FtpProcRRA;
    property nrProcRRA : String read FnrProcRRA write FnrProcRRA;
    property codSusp : String read FcodSusp write FcodSusp;
    property natRRA : String read FnatRRA write FnatRRA;
    property qtdMesesRRA : Integer read FqtdMesesRRA write FqtdMesesRRA;    property despProcJud : TdespProcJud read FdespProcJud write FdespProcJud;  end;

  { TinfoRRAs }
  TinfoRRAs = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoRRA;
    procedure SetItem(Index: Integer; const Value: TinfoRRA);
  public
    function New: TinfoRRA;

    property Items[Index: Integer]: TinfoRRA read GetItem write SetItem;
  end;

  { TcompJud }
  TcompJud = class
  private
    FvlrCompAnoCalend : double;
    FvlrCompAnoAnt    : double;
  public
    property vlrCompAnoCalend : double read FvlrCompAnoCalend write FvlrCompAnoCalend;
    property vlrCompAnoAnt : double read FvlrCompAnoAnt write FvlrCompAnoAnt;
  end;

  { TdetCompet }
  TdetCompet = class
  private
    FindPerReferencia  : TindPerReferencia;
    FperRefPagto       : String;
    FvlrRendTributavel : double;
  public
    property indPerReferencia : TindPerReferencia read FindPerReferencia write FindPerReferencia;
    property perRefPagto : String read FperRefPagto write FperRefPagto;
    property vlrRendTributavel : double read FvlrRendTributavel write FvlrRendTributavel;
  end;

  { TdetCompets }
  TdetCompets = class(TObjectList)
  private
    function GetItem(Index: Integer): TdetCompet;
    procedure SetItem(Index: Integer; const Value: TdetCompet);
  public
    function New: TdetCompet;

    property Items[Index: Integer]: TdetCompet read GetItem write SetItem;
  end;

  { TrendIsento }
  TrendIsento = class
  private
    FtpIsencao      : TtpIsencao;
    FvlrIsento      : double;
    FdescRendimento : String;
  public
    property tpIsencao : TtpIsencao read FtpIsencao write FtpIsencao;
    property vlrIsento : double read FvlrIsento write FvlrIsento;
    property descRendimento : String read FdescRendimento write FdescRendimento;
  end;

  { TrendIsentos }
  TrendIsentos = class(TObjectList)
  private
    function GetItem(Index: Integer): TrendIsento;
    procedure SetItem(Index: Integer; const Value: TrendIsento);
  public
    function New: TrendIsento;

    property Items[Index: Integer]: TrendIsento read GetItem write SetItem;
  end;

  { TdetDeducao }
  TdetDeducao = class
  private
    FindTpDeducao : TindTpDeducao;
    FvlrDeducao   : double;
  public
    property indTpDeducao : TindTpDeducao read FindTpDeducao write FindTpDeducao;
    property vlrDeducao : double read FvlrDeducao write FvlrDeducao;
  end;

  { TdetDeducoes }
  TdetDeducoes = class(TObjectList)
  private
    function GetItem(Index: Integer): TdetDeducao;
    procedure SetItem(Index: Integer; const Value: TdetDeducao);
  public
    function New: TdetDeducao;

    property Items[Index: Integer]: TdetDeducao read GetItem write SetItem;
  end;

  { TpgtoPF }
  TpgtoPF = class
  private
    FdtPgto            : TDateTime;
    FindSuspExig       : String;
    FindDecTerceiro    : String;
    FvlrRendTributavel : double;
    FvlrIRRF           : double;
    FdetDeducoes       : TdetDeducoes;
    FrendIsentos       : TrendIsentos;
    FdetCompets        : TdetCompets;
    FcompJud           : TcompJud;
    FinfoRRAs          : TinfoRRAs;
    FinfoProcJuds      : TinfoProcJuds;
    FdepJudicial       : TdepJudicial;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property dtPgto : TDateTime read FdtPgto write FdtPgto;
    property indSuspExig : String read FindSuspExig write FindSuspExig;
    property indDecTerceiro : String read FindDecTerceiro write FindDecTerceiro;
    property vlrRendTributavel : double read FvlrRendTributavel write FvlrRendTributavel;
    property vlrIRRF : double read FvlrIRRF write FvlrIRRF;
    property detDeducoes : TdetDeducoes read FdetDeducoes;
    property rendIsentos : TrendIsentos read FrendIsentos;
    property detCompets : TdetCompets read FdetCompets;
    property compJud : TcompJud read FcompJud write FcompJud;
    property infoRRAs : TinfoRRAs read FinfoRRAs;
    property infoProcJuds : TinfoProcJuds read FinfoProcJuds;
    property depJudicial : TdepJudicial read FdepJudicial write FdepJudicial;
  end;

  { TpgtoPFs }
  TpgtoPFs = class(TObjectList)
  private
    function GetItem(Index: Integer): TpgtoPF;
    procedure SetItem(Index: Integer; const Value: TpgtoPF);
  public
    function New: TpgtoPF;

    property Items[Index: Integer]: TpgtoPF read GetItem write SetItem;
  end;

  { TpgtoResidBR }
  TpgtoResidBR = class
  private
    FpgtoPFs : TpgtoPFs;
    FpgtoPJs : TpgtoPJs;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property pgtoPFs : TpgtoPFs read FpgtoPFs;
    property pgtoPJs : TpgtoPJs read FpgtoPJs;
  end;

  { TideEstab }
  TideEstab = class
  private
    FtpInsc : tpTpInsc;
    FnrInsc : String;

    FpgtoResidBR  : TpgtoResidBR;
    FpgtoResidExt : TpgtoResidExt;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property tpInsc : tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc : String read FnrInsc write FnrInsc;
    property pgtoResidBR: TpgtoResidBR read FpgtoResidBR write FpgtoResidBR;
    property pgtoResidExt: TpgtoResidExt read FpgtoResidExt write FpgtoResidExt;
  end;

  { TideEstabs }
  TideEstabs = class(TObjectList)
  private
    function GetItem(Index: Integer): TideEstab;
    procedure SetItem(Index: Integer; const Value: TideEstab);
  public
    function New: TideEstab;

    property Items[Index: Integer]: TideEstab read GetItem write SetItem;
  end;

  { TinfoPgto }
  TinfoPgto = class
  private
    FideEstabs : TideEstabs;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ideEstabs: TideEstabs read FideEstabs;
  end;

  { TinfoMolestia }
  TinfoMolestia = class
  private
    FdtLaudo : TDateTime;
  public
    property dtLaudo : TDateTime read FdtLaudo write FdtLaudo;
  end;

  { TinfoFiscal }
  TinfoFiscal = class
  private
    FindNIF        : TindNIF;
    FnifBenef      : String;
    FrelFontePagad : String;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property indNIF        : TindNIF read FindNIF write FindNIF;
    property nifBenef      : String read FnifBenef write FnifBenef;
    property relFontePagad : String read FrelFontePagad write FrelFontePagad;
  end;

  { TinfoEnder }
  TinfoEnder = class
  private
    FpaisResid : String;
    FdscLograd : String;
    FnrLograd  : String;
    Fcomplem   : String;
    Fbairro    : String;
    Fcidade    : String;
    FcodPostal : String;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property paisResid : String read FpaisResid write FpaisResid;
    property dscLograd : String read FdscLograd write FdscLograd;
    property nrLograd  : String read FnrLograd write FnrLograd;
    property complem   : String read Fcomplem write Fcomplem;
    property bairro    : String read Fbairro write Fbairro;
    property cidade    : String read Fcidade write Fcidade;
    property codPostal : String read FcodPostal write FcodPostal;
  end;

  { TinfoResidExt }
  TinfoResidExt = class
  private
    FinfoEnder   : TinfoEnder;
    FinfoFiscal  : TinfoFiscal;
    FinfoMolestia: TinfoMolestia;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property infoEnder: TinfoEnder read FinfoEnder write FinfoEnder;
    property infoFiscal: TinfoFiscal read FinfoFiscal write FinfoFiscal;
    property infoMolestia: TinfoMolestia read FinfoMolestia write FinfoMolestia;
  end;

  { TideBenef }
  TideBenef = class
  private
    FcodPgto      : String;
    FtpInscBenef  : tpTpInsc;
    FnrInscBenef  : String;
    FnmRazaoBenef : String;
    FinfoResidExt : TinfoResidExt;
    FinfoPgto     : TinfoPgto;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property codPgto: String read FcodPgto write FcodPgto;
    property tpInscBenef: tpTpInsc read FtpInscBenef write FtpInscBenef;
    property nrInscBenef: String read FnrInscBenef write FnrInscBenef;
    property nmRazaoBenef: String read FnmRazaoBenef write FnmRazaoBenef;
    property infoResidExt : TinfoResidExt read FinfoResidExt write FinfoResidExt;
    property infoPgto: TinfoPgto read FinfoPgto write FinfoPgto;
  end;

implementation

{ TideBenef }

procedure TideBenef.AfterConstruction;
begin
  inherited;
  FinfoResidExt := TinfoResidExt.Create;
  FinfoPgto     := TinfoPgto.Create;
end;

procedure TideBenef.BeforeDestruction;
begin
  inherited;
  FinfoResidExt.Free;
  FinfoPgto.Free;
end;

{ TinfoResidExt }

procedure TinfoResidExt.AfterConstruction;
begin
  inherited;
  FinfoEnder := TinfoEnder.Create;
  FinfoFiscal := TinfoFiscal.Create;
  FinfoMolestia := TinfoMolestia.Create;
end;

procedure TinfoResidExt.BeforeDestruction;
begin
  inherited;
  FinfoEnder.Free;
  FinfoFiscal.Free;
  infoMolestia.Free;
end;

{ TinfoEnder }

procedure TinfoEnder.AfterConstruction;
begin
  inherited;

end;

procedure TinfoEnder.BeforeDestruction;
begin
  inherited;

end;

{ TinfoFiscal }

procedure TinfoFiscal.AfterConstruction;
begin
  inherited;

end;

procedure TinfoFiscal.BeforeDestruction;
begin
  inherited;

end;

{ TinfoPgto }

procedure TinfoPgto.AfterConstruction;
begin
  inherited;
  FideEstabs := TideEstabs.Create;
end;

procedure TinfoPgto.BeforeDestruction;
begin
  inherited;
  FideEstabs.Free;
end;

{ TideEstab }

procedure TideEstab.AfterConstruction;
begin
  inherited;
  pgtoResidBR  := TpgtoResidBR.Create;
  pgtoResidExt := TpgtoResidExt.Create;
end;

procedure TideEstab.BeforeDestruction;
begin
  inherited;
  pgtoResidBR.Free;
  pgtoResidExt.Free;
end;

{ TpgtoResidBR }

procedure TpgtoResidBR.AfterConstruction;
begin
  inherited;
  FpgtoPFs := TpgtoPFs.Create;
  FpgtoPJs := TpgtoPJs.Create;
end;

procedure TpgtoResidBR.BeforeDestruction;
begin
  inherited;
  FpgtoPFs.Free;
  FpgtoPJs.Free;
end;

{ TpgtoPF }

procedure TpgtoPF.AfterConstruction;
begin
  inherited;
  FdetDeducoes  := TdetDeducoes.Create;
  FrendIsentos  := TrendIsentos.Create;
  FdetCompets   := TdetCompets.Create;
  FcompJud      := TcompJud.Create;
  FinfoRRAs     := TinfoRRAs.Create;
  FinfoProcJuds := TinfoProcJuds.Create;
  FdepJudicial  := TdepJudicial.Create;
end;

procedure TpgtoPF.BeforeDestruction;
begin
  inherited;
  FdetDeducoes.Free;
  FrendIsentos.Free;
  FdetCompets.Free;
  FcompJud.Free;
  FinfoRRAs.Free;
  FinfoProcJuds.Free;
  FdepJudicial.Free;
end;

{ TideEstabs }

function TideEstabs.GetItem(Index: Integer): TideEstab;
begin
  Result := TideEstab(Inherited Items[Index]);
end;

function TideEstabs.New: TideEstab;
begin
  Result := TideEstab.Create;
  Add(Result);
end;

procedure TideEstabs.SetItem(Index: Integer; const Value: TideEstab);
begin
  Put(Index, Value);
end;

{ TpgtoPFs }

function TpgtoPFs.GetItem(Index: Integer): TpgtoPF;
begin
  Result := TpgtoPF(Inherited Items[Index]);
end;

function TpgtoPFs.New: TpgtoPF;
begin
  Result := TpgtoPF.Create;
  Add(Result);
end;

procedure TpgtoPFs.SetItem(Index: Integer; const Value: TpgtoPF);
begin
  Put(Index, Value);
end;

{ TdetDeducoes }

function TdetDeducoes.GetItem(Index: Integer): TdetDeducao;
begin
  Result := TdetDeducao(Inherited Items[Index]);
end;

function TdetDeducoes.New: TdetDeducao;
begin
  Result := TdetDeducao.Create;
  Add(Result);
end;

procedure TdetDeducoes.SetItem(Index: Integer; const Value: TdetDeducao);
begin
  Put(Index, Value);
end;

{ TrendIsentos }

function TrendIsentos.GetItem(Index: Integer): TrendIsento;
begin
  Result := TrendIsento(Inherited Items[Index]);
end;

function TrendIsentos.New: TrendIsento;
begin
  Result := TrendIsento.Create;
  Add(Result);
end;

procedure TrendIsentos.SetItem(Index: Integer; const Value: TrendIsento);
begin
  Put(Index, Value);
end;

{ TdetCompets }

function TdetCompets.GetItem(Index: Integer): TdetCompet;
begin
  Result := TdetCompet(Inherited Items[Index]);
end;

function TdetCompets.New: TdetCompet;
begin
  Result := TdetCompet.Create;
  Add(Result);
end;

procedure TdetCompets.SetItem(Index: Integer; const Value: TdetCompet);
begin
  Put(Index, Value);
end;

{ TinfoRRA }

procedure TinfoRRA.AfterConstruction;
begin
  inherited;
  FdespProcJud := TdespProcJud.Create;
end;

procedure TinfoRRA.BeforeDestruction;
begin
  inherited;
  FdespProcJud.Free;
end;

{ TinfoRRAs }

function TinfoRRAs.GetItem(Index: Integer): TinfoRRA;
begin
  Result := TinfoRRA(Inherited Items[Index]);
end;

function TinfoRRAs.New: TinfoRRA;
begin
  Result := TinfoRRA.Create;
  Add(Result);
end;

procedure TinfoRRAs.SetItem(Index: Integer; const Value: TinfoRRA);
begin
  Put(Index, Value);
end;

{ TdespProcJud }

procedure TdespProcJud.AfterConstruction;
begin
  inherited;
  FideAdvogados := TideAdvogados.Create;
end;

procedure TdespProcJud.BeforeDestruction;
begin
  inherited;
  FideAdvogados.Free;
end;

{ TideAdvogados }

function TideAdvogados.GetItem(Index: Integer): TideAdvogado;
begin
  Result := TideAdvogado(Inherited Items[Index]);
end;

function TideAdvogados.New: TideAdvogado;
begin
  Result := TideAdvogado.Create;
  Add(Result);
end;

procedure TideAdvogados.SetItem(Index: Integer; const Value: TideAdvogado);
begin
  Put(Index, Value);
end;

{ TinfoProcJud }

procedure TinfoProcJud.AfterConstruction;
begin
  inherited;
  FdespProcJud    := TdespProcJud.Create;
  ForigemRecursos := TorigemRecursos.Create;
end;

procedure TinfoProcJud.BeforeDestruction;
begin
  inherited;
  FdespProcJud.Free;
  ForigemRecursos.Free;
end;

{ TinfoProcJuds }

function TinfoProcJuds.GetItem(Index: Integer): TinfoProcJud;
begin
  Result := TinfoProcJud(Inherited Items[Index]);
end;

function TinfoProcJuds.New: TinfoProcJud;
begin
  Result := TinfoProcJud.Create;
  Add(Result);
end;

procedure TinfoProcJuds.SetItem(Index: Integer; const Value: TinfoProcJud);
begin
  Put(Index, Value);
end;

{ TpgtoPJ }

procedure TpgtoPJ.AfterConstruction;
begin
  inherited;
  FinfoProcJuds := TinfoProcJuds.Create;
end;

procedure TpgtoPJ.BeforeDestruction;
begin
  inherited;
  FinfoProcJuds.Free;
end;

{ TpgtoPJs }

function TpgtoPJs.GetItem(Index: Integer): TpgtoPJ;
begin
  Result := TpgtoPJ(Inherited Items[Index]);
end;

function TpgtoPJs.New: TpgtoPJ;
begin
  Result := TpgtoPJ.Create;
  Add(Result);
end;

procedure TpgtoPJs.SetItem(Index: Integer; const Value: TpgtoPJ);
begin
  Put(Index, Value);
end;

end.

