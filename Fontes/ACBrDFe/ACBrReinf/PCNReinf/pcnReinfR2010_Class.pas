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
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

unit pcnReinfR2010_Class;

interface

uses
  Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs, pcnReinfClasses;

type
  TideEstabObra = class;
  TidePrestServs = class;
  Tnfss = class;
  TinfoProcRetPrs = class;
  TinfoTpServs = class;

  { TinfoServTom }
  TinfoServTom = class
  private
    FidePeriodo: TIdePeriodo;
    FideEstabObra: TideEstabObra;
  public
    property IdePeriodo: TIdePeriodo read FidePeriodo write FidePeriodo;
    property ideEstabObra: TideEstabObra read FideEstabObra;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TideEstabObra }
  TideEstabObra = class
  private
    FtpInscEstab: tpTpInsc;
    FnrInscEstab: string;
    FindObra: TpindObra;
    FidePrestServs: TidePrestServs;
  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
    property tpInscEstab: tpTpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property indObra: TpindObra read FindObra write FindObra;
    property idePrestServs: TidePrestServs read FidePrestServs;
  end;

  { TideEstabObras }
  TideEstabObras = class(TObjectList)
  private
    function GetItem(Index: Integer): TideEstabObra;
    procedure SetItem(Index: Integer; const Value: TideEstabObra);
  public
    function New: TideEstabObra;
    property Items[Index: Integer]: TideEstabObra read GetItem write SetItem;
  end;

  { TideServico }
  TideServico = class
  private
    FvlrTotalBruto: Extended;
    FvlrTotalRetPrinc: Extended;
    FvlrTotalNRetPrinc: Extended;
    FvlrTotalRetAdic: Extended;
    FvlrTotalNRetAdic: Extended;
    FcodAnaCont: string;
    FvlrTotalBaseRet: Extended;
    Fnfss: Tnfss;
    FinfoProcRetPr: TinfoProcRetPrs;
    FinfoProcRetAd: TinfoProcRetPrs;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property vlrTotalBruto: Extended read FvlrTotalBruto write FvlrTotalBruto;
    property vlrTotalBaseRet: Extended read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Extended read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Extended read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Extended read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Extended read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
    property codAnaCont: string read FcodAnaCont write FcodAnaCont;
    property nfss: Tnfss read Fnfss write Fnfss;
    property infoProcRetPr: TinfoProcRetPrs read FinfoProcRetPr write FinfoProcRetPr;
    property infoProcRetAd: TinfoProcRetPrs read FinfoProcRetAd write FinfoProcRetAd;
  end;

  { TidePrestServ }
  TidePrestServ = class(TideServico)
  private
    FcnpjPrestador: string;
    FindCPRB: TpindCPRB;
  public
    property cnpjPrestador: string read FcnpjPrestador write FcnpjPrestador;
    property indCPRB: TpindCPRB read FindCPRB write FindCPRB;
  end;

  { TidePrestServs }
  TidePrestServs = class(TObjectList)
  private
    function GetItem(Index: Integer): TidePrestServ;
    procedure SetItem(Index: Integer; const Value: TidePrestServ);
  public
    function New: TidePrestServ;
    property Items[Index: Integer]: TidePrestServ read GetItem write SetItem;
  end;

  { Tnfs }
  Tnfs = class
  private
    FnumDocto: string;
    FdtEmissaoNF: TDateTime;
    Fserie: string;
    FvlrBruto: Extended;
    Fobs1: string;
    FinfoTpServs: TinfoTpServs;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property serie: string read Fserie write Fserie;
    property numDocto: string read FnumDocto write FnumDocto;
    property dtEmissaoNF: TDateTime read FdtEmissaoNF write FdtEmissaoNF;
    property vlrBruto: Extended read FvlrBruto write FvlrBruto;
    property obs: string read Fobs1 write Fobs1;
    property infoTpServs: TinfoTpServs read FinfoTpServs;
  end;

  { Tnfss }
  Tnfss = class(TObjectList)
  private
    function GetItem(Index: Integer): Tnfs;
    procedure SetItem(Index: Integer; const Value: Tnfs);
  public
    function New: Tnfs;
    property Items[Index: Integer]: Tnfs read GetItem write SetItem;
  end;

  { TinfoProcRetPr }
  TinfoProcRetPr = class
  private
    FtpProcRetPrinc: tpTpProc;
    FnrProcRetPrinc: string;
    FcodSuspPrinc: integer;
    FvalorPrinc: Extended;
  public
    property tpProcRetPrinc: tpTpProc read  FtpProcRetPrinc write FtpProcRetPrinc;
    property nrProcRetPrinc: string read FnrProcRetPrinc write FnrProcRetPrinc;
    property codSuspPrinc: integer read FcodSuspPrinc write FcodSuspPrinc;
    property valorPrinc: Extended read FvalorPrinc write FvalorPrinc;
  end;

  { TinfoProcRetPrs }
  TinfoProcRetPrs = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetPr;
    procedure SetItem(Index: Integer; const Value: TinfoProcRetPr);
  public
    function New: TinfoProcRetPr;
    property Items[Index: Integer]: TinfoProcRetPr read GetItem write SetItem;
  end;

  { TinfoTpServ }
  TinfoTpServ = class
  private
    FvlrRetencao: Extended;
    FvlrServicos20: Extended;
    FvlrMatEquip: Extended;
    FvlrServicos25: Extended;
    FvlrServicos15: Extended;
    FvlrAdicional: Extended;
    FvlrBaseRet: Extended;
    FvlrRetSub: Extended;
    FvlrDedAlim: Extended;
    FvlrDedTrans: Extended;
    FvlrNRetPrinc: Extended;
    FcodAtivEcon: string;
    FtpServico: string;
    FvlrNRetAdic: Extended;
  public
    property tpServico: string read FtpServico write FtpServico;
    property codAtivEcon: string read FcodAtivEcon write FcodAtivEcon;
    property vlrMatEquip: Extended read FvlrMatEquip write FvlrMatEquip;
    property vlrDedAlim: Extended read FvlrDedAlim write FvlrDedAlim;
    property vlrDedTrans: Extended read FvlrDedTrans write FvlrDedTrans;
    property vlrBaseRet: Extended read FvlrBaseRet write FvlrBaseRet;
    property vlrRetencao: Extended read FvlrRetencao write FvlrRetencao;
    property vlrRetSub: Extended read FvlrRetSub write FvlrRetSub;
    property vlrNRetPrinc: Extended read FvlrNRetPrinc write FvlrNRetPrinc;
    property vlrServicos15: Extended read FvlrServicos15 write FvlrServicos15;
    property vlrServicos20: Extended read FvlrServicos20 write FvlrServicos20;
    property vlrServicos25: Extended read FvlrServicos25 write FvlrServicos25;
    property vlrAdicional: Extended read FvlrAdicional write FvlrAdicional;
    property vlrNRetAdic: Extended read FvlrNRetAdic write FvlrNRetAdic;
  end;

  { TinfoTpServs }
  TinfoTpServs = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoTpServ;
    procedure SetItem(Index: Integer; const Value: TinfoTpServ);
  public
    function New: TinfoTpServ;
    property Items[Index: Integer]: TinfoTpServ read GetItem write SetItem;
  end;

implementation

{ TinfoServTom }

procedure TinfoServTom.AfterConstruction;
begin
  inherited;
  FidePeriodo := TIdePeriodo.Create;
  FideEstabObra := TideEstabObra.Create;
end;

procedure TinfoServTom.BeforeDestruction;
begin
  inherited;
  FidePeriodo.Free;
  FideEstabObra.Free;
end;

{ TideEstabObra }

procedure TideEstabObra.AfterConstruction;
begin
  inherited;
  FidePrestServs := TidePrestServs.Create;
end;

procedure TideEstabObra.BeforeDestruction;
begin
  inherited;
  FidePrestServs.Free;
end;

{ TideEstabObras }

function TideEstabObras.GetItem(Index: Integer): TideEstabObra;
begin
  Result := TideEstabObra(Inherited Items[Index]);
end;

function TideEstabObras.New: TideEstabObra;
begin
  Result := TideEstabObra.Create;
  Add(Result);
end;

procedure TideEstabObras.SetItem(Index: Integer; const Value: TideEstabObra);
begin
  Put(Index, Value);
end;

{ TidePrestServs }

function TidePrestServs.GetItem(Index: Integer): TidePrestServ;
begin
  Result := TidePrestServ(Inherited Items[Index]);
end;

function TidePrestServs.New: TidePrestServ;
begin
  Result := TidePrestServ.Create;
  Add(Result);
end;

procedure TidePrestServs.SetItem(Index: Integer; const Value: TidePrestServ);
begin
  Put(Index, Value);
end;

{ Tnfs }

procedure Tnfs.AfterConstruction;
begin
  inherited;
  FinfoTpServs := TinfoTpServs.Create;
end;

procedure Tnfs.BeforeDestruction;
begin
  inherited;
  FinfoTpServs.Free;
end;

{ Tnfss }

function Tnfss.GetItem(Index: Integer): Tnfs;
begin
  Result := Tnfs(Inherited Items[Index]);
end;

function Tnfss.New: Tnfs;
begin
  Result := Tnfs.Create;
  Add(Result);
end;

procedure Tnfss.SetItem(Index: Integer; const Value: Tnfs);
begin
  Put(Index, Value);
end;

{ TinfoProcRetPrs }

function TinfoProcRetPrs.GetItem(Index: Integer): TinfoProcRetPr;
begin
  Result := TinfoProcRetPr(Inherited Items[Index]);
end;

function TinfoProcRetPrs.New: TinfoProcRetPr;
begin
  Result := TinfoProcRetPr.Create;
  Add(Result);
end;

procedure TinfoProcRetPrs.SetItem(Index: Integer; const Value: TinfoProcRetPr);
begin
  Put(Index, Value);
end;

{ TideServico }

procedure TideServico.AfterConstruction;
begin
  inherited;
  Fnfss := Tnfss.Create;
  FinfoProcRetPr := TinfoProcRetPrs.Create;
  FinfoProcRetAd := TinfoProcRetPrs.Create;
end;

procedure TideServico.BeforeDestruction;
begin
  inherited;
  Fnfss.Free;
  FinfoProcRetPr.Free;
  FinfoProcRetAd.Free;
end;

{ TinfoTpServs }

function TinfoTpServs.GetItem(Index: Integer): TinfoTpServ;
begin
  Result := TinfoTpServ(Inherited Items[Index]);
end;

function TinfoTpServs.New: TinfoTpServ;
begin
  Result := TinfoTpServ.Create;
  Add(Result);
end;

procedure TinfoTpServs.SetItem(Index: Integer; const Value: TinfoTpServ);
begin
  Put(Index, Value);
end;

end.

