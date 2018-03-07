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

unit pcnReinfR2060_Class;

interface

uses
  Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs;

type
  TinfoProcs = class;
  TtipoAjustes = class;
  TtipoCods = class;

  { TinfoProc }
  TinfoProc = class
  private
    FvlrCPRBSusp: double;
    FtpProc: TtpProc;
    FnrProc: string;
    FcodSusp: string;
  public
    property vlrCPRBSusp: double read FvlrCPRBSusp write FvlrCPRBSusp;
    property tpProc: TtpProc read FtpProc write FtpProc;
    property nrProc: string read FnrProc write FnrProc;
    property codSusp: string read FcodSusp write FcodSusp;
  end;

  { TinfoProcs }
  TinfoProcs = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoProc;
    procedure SetItem(Index: Integer; const Value: TinfoProc);
  public
    function New: TinfoProc;
    
    property Items[Index: Integer]: TinfoProc read GetItem write SetItem;
  end;

  { TtipoAjuste }
  TtipoAjuste = class
  private
    FtpAjuste: TtpAjuste;
    FcodAjuste: TcodAjuste;
    FvlrAjuste: double;
    FdescAjuste: string;
    FdtAjuste: string;
  public
    property tpAjuste: TtpAjuste read FtpAjuste write FtpAjuste;
    property codAjuste: TcodAjuste read FcodAjuste write FcodAjuste;
    property vlrAjuste: double read FvlrAjuste write FvlrAjuste;
    property descAjuste: string read FdescAjuste write FdescAjuste;
    property dtAjuste: string read FdtAjuste write FdtAjuste;
  end;

  { TtipoAjustes }
  TtipoAjustes = class(TObjectList)
  private
    function GetItem(Index: Integer): TtipoAjuste;
    procedure SetItem(Index: Integer; const Value: TtipoAjuste);
  public
    function New: TtipoAjuste;

    property Items[Index: Integer]: TtipoAjuste read GetItem write SetItem;
  end;

  { TtipoCod }
  TtipoCod = class
  private
    FcodAtivEcon: string;
    FvlrRecBrutaAtiv: double;
    FvlrExcRecBruta: double;
    FvlrAdicRecBruta: double;
    FvlrBcCPRB: double;
    FvlrCPRBapur: double;
    FtipoAjustes: TtipoAjustes;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property codAtivEcon: string read FcodAtivEcon write FcodAtivEcon;
    property vlrRecBrutaAtiv: double read FvlrRecBrutaAtiv write FvlrRecBrutaAtiv;
    property vlrExcRecBruta: double read FvlrExcRecBruta write FvlrExcRecBruta;
    property vlrAdicRecBruta: double read FvlrAdicRecBruta write FvlrAdicRecBruta;
    property vlrBcCPRB: double read FvlrBcCPRB write FvlrBcCPRB;
    property vlrCPRBapur: double read FvlrCPRBapur write FvlrCPRBapur;
    property tipoAjustes: TtipoAjustes read FtipoAjustes;
  end;

  { TtipoCods }
  TtipoCods = class(TObjectList)
  private
    function GetItem(Index: Integer): TtipoCod;
    procedure SetItem(Index: Integer; const Value: TtipoCod);
  public
    function New: TtipoCod;

    property Items[Index: Integer]: TtipoCod read GetItem write SetItem;
  end;

  { TideEstab }
  TideEstab = class
  private
    FtpInscEstab: TtpInsc;
    FnrInscEstab: string;
    FvlrRecBrutaTotal: Double;
    FvlrCPApurTotal: Double;
    FvlrCPRBSuspTotal: Double;
    FtipoCods: TtipoCods;
    FinfoProcs: TinfoProcs;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property vlrRecBrutaTotal: Double read FvlrRecBrutaTotal write FvlrRecBrutaTotal;
    property vlrCPApurTotal: Double read FvlrCPApurTotal write FvlrCPApurTotal;
    property vlrCPRBSuspTotal: Double read FvlrCPRBSuspTotal write FvlrCPRBSuspTotal;
    property tipoCods: TtipoCods read FtipoCods;
    property infoProcs: TinfoProcs read FinfoProcs;
  end;

  { TinfoCPRB }
  TinfoCPRB = class
  private
    FideEstab: TideEstab;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ideEstab: TideEstab read FideEstab;
  end;

implementation

{ TinfoProcs }

function TinfoProcs.GetItem(Index: Integer): TinfoProc;
begin
  Result := TinfoProc(Inherited Items[Index]);
end;

function TinfoProcs.New: TinfoProc;
begin
  Result := TinfoProc.Create;
  Add(Result);
end;

procedure TinfoProcs.SetItem(Index: Integer; const Value: TinfoProc);
begin
  Put(Index, Value);
end;

{ TtipoAjustes }

function TtipoAjustes.GetItem(Index: Integer): TtipoAjuste;
begin
  Result := TtipoAjuste(Inherited Items[Index]);
end;

function TtipoAjustes.New: TtipoAjuste;
begin
  Result := TtipoAjuste.Create;
  Add(Result);
end;

procedure TtipoAjustes.SetItem(Index: Integer; const Value: TtipoAjuste);
begin
  Put(Index, Value);
end;

{ TideEstab }

procedure TideEstab.AfterConstruction;
begin
  inherited;
  FtipoCods := TtipoCods.Create;
  FinfoProcs := TinfoProcs.Create;
end;

procedure TideEstab.BeforeDestruction;
begin
  inherited;
  FtipoCods.Free;
  FinfoProcs.Free;
end;

{ TtipoCod }

procedure TtipoCod.AfterConstruction;
begin
  inherited;
  FtipoAjustes := TtipoAjustes.Create;
end;

procedure TtipoCod.BeforeDestruction;
begin
  inherited;
  FtipoAjustes.Free;
end;

{ TtipoCods }

function TtipoCods.GetItem(Index: Integer): TtipoCod;
begin
  Result := TtipoCod(Inherited Items[Index]);
end;

function TtipoCods.New: TtipoCod;
begin
  Result := TtipoCod.Create;
  Add(Result);
end;

procedure TtipoCods.SetItem(Index: Integer; const Value: TtipoCod);
begin
  Put(Index, Value);
end;

{ TinfoCPRB }

procedure TinfoCPRB.AfterConstruction;
begin
  inherited;
  FideEstab := TideEstab.Create;
end;

procedure TinfoCPRB.BeforeDestruction;
begin
  inherited;
  FideEstab.Free;
end;

end.

