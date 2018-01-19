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

unit ACBrReinfR1070_Class;

interface

uses Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs, ACBrReinfClasses;

type
  TIdeProcesso = class;
  TdadosProcJud = class;
  TinfoSusps = class;

  TInfoProcesso = class
  private
    FidePeriodo: TIdePeriodo;
    FideProcesso: TideProcesso;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property IdePeriodo: TIdePeriodo read FidePeriodo write FidePeriodo;
    property IdeProcesso: TideProcesso read FideProcesso write FideProcesso;
  end;

  TIdeProcesso = class
   private
    FTpProc : tpTpProc;
    FNrProc : string;
    FDadosProcJud: TDadosProcJud;
    FinfoSusps: TinfoSusps;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property tpProc: tpTpProc read FTpProc write FTpProc;
    property nrProc: string read FNrProc write FNrProc;
    property DadosProcJud: TDadosProcJud read FDadosProcJud;
    property infoSusps: TinfoSusps read FinfoSusps;
  end;

  TdadosProcJud = class(TPersistent)
   private
    FUfVara: string;
    FCodMunic: integer;
    FIdVara: string;
    FindAutoria: TTypeAutoria;
  public
    property UfVara: string read FUfVara write FUfVara;
    property codMunic: integer read FCodMunic write FCodMunic;
    property idVara: string read FIdVara write FIdVara;
    property indAutoria: TTypeAutoria read FindAutoria write FindAutoria;
  end;

  { TinfoSusp }
  TinfoSusp = class
  private
    FcodSusp: string;
    FIndDeposito: tpSimNao;
    FDTDecisao: TDate;
    FIndSusp: tpIndSusp;
  public
    property codSusp: string read FcodSusp write FcodSusp;
    property indSusp: tpIndSusp read FIndSusp write FIndSusp;
    property dtDecisao: TDate read FDTDecisao write FDTDecisao;
    property indDeposito: tpSimNao read FIndDeposito write FIndDeposito;
  end;

  { TinfoSusps }
  TinfoSusps = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoSusp;
    procedure SetItem(Index: Integer; const Value: TinfoSusp);
  public
    function New: TinfoSusp;
    property Items[Index: Integer]: TinfoSusp read GetItem write SetItem;
  end;


implementation

{ TInfoProcesso }

procedure TInfoProcesso.AfterConstruction;
begin
  inherited;
  FIdePeriodo := TIdePeriodo.Create;
  FideProcesso := TIdeProcesso.Create;
end;

procedure TInfoProcesso.BeforeDestruction;
begin
  inherited;
  FIdePeriodo.Free;
  FideProcesso.Free
end;

{ TIdeProcesso }

procedure TIdeProcesso.AfterConstruction;
begin
  inherited;
  FdadosProcJud := TdadosProcJud.Create;
  FinfoSusps := TinfoSusps.Create;
end;

procedure TIdeProcesso.BeforeDestruction;
begin
  inherited;
  FdadosProcJud.Free;
  FinfoSusps.Free;
end;

{ TinfoSusps }

function TinfoSusps.GetItem(Index: Integer): TinfoSusp;
begin
  Result := TinfoSusp(Inherited Items[Index]);
end;

function TinfoSusps.New: TinfoSusp;
begin
  Result := TinfoSusp.Create;
  Add(Result);
end;

procedure TinfoSusps.SetItem(Index: Integer; const Value: TinfoSusp);
begin
  Put(Index, Value);
end;

end.

