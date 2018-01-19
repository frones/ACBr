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

unit ACBrReinfR3010_Class;

interface

uses Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs;

type
  TideEstabs = class;
  Tboletins = class;
  TreceitaIngressoss = class;
  ToutrasReceitass = class;
  TinfoProcs = class;

  { TinfoProc }
  TinfoProc = class
  private
    FtpProc    : tpTpProc;
    FnrProc    : String;
    FcodSusp   : String;
    FvlrCPSusp : double;
  public
    property tpProc : tpTpProc read FtpProc write FtpProc;
    property nrProc : String read FnrProc write FnrProc;
    property codSusp : String read FcodSusp write FcodSusp;
    property vlrCPSusp : double read FvlrCPSusp write FvlrCPSusp;
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

  { TreceitaTotal }
  TreceitaTotal = class
  private
    FvlrReceitaTotal  : double;
    FvlrCP            : double;
    FvlrCPSuspTotal   : double;
    FvlrReceitaClubes : double;
    FvlrRetParc       : double;
    FinfoProcs        : TinfoProcs;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property vlrReceitaTotal : double read FvlrReceitaTotal write FvlrReceitaTotal;
    property vlrCP : double read FvlrCP write FvlrCP;
    property vlrCPSuspTotal : double read FvlrCPSuspTotal write FvlrCPSuspTotal;
    property vlrReceitaClubes : double read FvlrReceitaClubes write FvlrReceitaClubes;
    property vlrRetParc : double read FvlrRetParc write FvlrRetParc;
    property infoProcs : TinfoProcs read FinfoProcs;
  end;

  { ToutrasReceitas }
  ToutrasReceitas = class
  private
    FtpReceita   : TtpReceita;
    FvlrReceita  : double;
    FdescReceita : String;
  public
    property tpReceita : TtpReceita read FtpReceita write FtpReceita;
    property vlrReceita : double read FvlrReceita write FvlrReceita;
    property descReceita : String read FdescReceita write FdescReceita;
  end;

  { ToutrasReceitass }
  ToutrasReceitass = class(TObjectList)
  private
    function GetItem(Index: Integer): ToutrasReceitas;
    procedure SetItem(Index: Integer; const Value: ToutrasReceitas);
  public
    function New: ToutrasReceitas;

    property Items[Index: Integer]: ToutrasReceitas read GetItem write SetItem;
  end;

  { TreceitaIngressos }
  TreceitaIngressos = class
  private
    FtpIngresso       : TtpIngresso;
    FdescIngr         : String;
    FqtdeIngrVenda    : Integer;
    FqtdeIngrVendidos : Integer;
    FqtdeIngrDev      : Integer;
    FprecoIndiv       : double;
    FvlrTotal         : double;
  public
    property tpIngresso : TtpIngresso read FtpIngresso write FtpIngresso;
    property descIngr : String read FdescIngr write FdescIngr;
    property qtdeIngrVenda : Integer read FqtdeIngrVenda write FqtdeIngrVenda;
    property qtdeIngrVendidos : Integer read FqtdeIngrVendidos write FqtdeIngrVendidos;
    property qtdeIngrDev : Integer read FqtdeIngrDev write FqtdeIngrDev;
    property precoIndiv : double read FprecoIndiv write FprecoIndiv;
    property vlrTotal : double read FvlrTotal write FvlrTotal;
  end;

  { TreceitaIngressoss }
  TreceitaIngressoss = class(TObjectList)
  private
    function GetItem(Index: Integer): TreceitaIngressos;
    procedure SetItem(Index: Integer; const Value: TreceitaIngressos);
  public
    function New: TreceitaIngressos;

    property Items[Index: Integer]: TreceitaIngressos read GetItem write SetItem;
  end;

  { Tboletim }
  Tboletim = class
  private
    FnrBoletim         : String;
    FtpCompeticao      : TtpCompeticao;
    FcategEvento       : TcategEvento;
    FmodDesportiva     : String;
    FnomeCompeticao    : String;
    FcnpjMandante      : String;
    FcnpjVisitante     : String;
    FnomeVisitante     : String;
    FpracaDesportiva   : String;
    FcodMunic          : String;
    Fuf                : String;
    FqtdePagantes      : Integer;
    FqtdeNaoPagantes   : Integer;
    FreceitaIngressoss : TreceitaIngressoss;
    FoutrasReceitass   : ToutrasReceitass;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property nrBoletim : String read FnrBoletim write FnrBoletim;
    property tpCompeticao : TtpCompeticao read FtpCompeticao write FtpCompeticao;
    property categEvento : TcategEvento read FcategEvento write FcategEvento;
    property modDesportiva : String read FmodDesportiva write FmodDesportiva;
    property nomeCompeticao : String read FnomeCompeticao write FnomeCompeticao;
    property cnpjMandante : String read FcnpjMandante write FcnpjMandante;
    property cnpjVisitante : String read FcnpjVisitante write FcnpjVisitante;
    property nomeVisitante : String read FnomeVisitante write FnomeVisitante;
    property pracaDesportiva : String read FpracaDesportiva write FpracaDesportiva;
    property codMunic : String read FcodMunic write FcodMunic;
    property uf : String read Fuf write Fuf;
    property qtdePagantes : Integer read FqtdePagantes write FqtdePagantes;
    property qtdeNaoPagantes : Integer read FqtdeNaoPagantes write FqtdeNaoPagantes;
    property receitaIngressoss : TreceitaIngressoss read FreceitaIngressoss;
    property outrasReceitass : ToutrasReceitass read FoutrasReceitass;
  end;

  { Tboletins }
  Tboletins = class(TObjectList)
  private
    function GetItem(Index: Integer): Tboletim;
    procedure SetItem(Index: Integer; const Value: Tboletim);
  public
    function New: Tboletim;

    property Items[Index: Integer]: Tboletim read GetItem write SetItem;
  end;

  { TideEstab }
  TideEstab = class
  private
    FtpInscEstab  : tpTpInsc;
    FnrInscEstab  : String;

    Fboletins     : Tboletins;
    FreceitaTotal : TreceitaTotal;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property tpInscEstab : tpTpInsc read FtpInscEstab write FtpInscEstab;
    property nrInscEstab : String read FnrInscEstab write FnrInscEstab;
    property boletins : Tboletins read Fboletins;
    property receitaTotal : TreceitaTotal read FreceitaTotal write FreceitaTotal;
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


implementation

{ TideEstab }

procedure TideEstab.AfterConstruction;
begin
  inherited;
  Fboletins     := Tboletins.Create;
  FreceitaTotal := TreceitaTotal.Create;
end;

procedure TideEstab.BeforeDestruction;
begin
  inherited;
  Fboletins.Free;
  FreceitaTotal.Free;
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

{ Tboletim }

procedure Tboletim.AfterConstruction;
begin
  inherited;
  FreceitaIngressoss := TreceitaIngressoss.Create;
  FoutrasReceitass   := ToutrasReceitass.Create;
end;

procedure Tboletim.BeforeDestruction;
begin
  inherited;
  FreceitaIngressoss.Free;
  FoutrasReceitass.Free;
end;

{ Tboletins }

function Tboletins.GetItem(Index: Integer): Tboletim;
begin
  Result := Tboletim(Inherited Items[Index]);
end;

function Tboletins.New: Tboletim;
begin
  Result := Tboletim.Create;
  Add(Result);
end;

procedure Tboletins.SetItem(Index: Integer; const Value: Tboletim);
begin
  Put(Index, Value);
end;

{ TreceitaIngressoss }

function TreceitaIngressoss.GetItem(Index: Integer): TreceitaIngressos;
begin
  Result := TreceitaIngressos(Inherited Items[Index]);
end;

function TreceitaIngressoss.New: TreceitaIngressos;
begin
  Result := TreceitaIngressos.Create;
  Add(Result);
end;

procedure TreceitaIngressoss.SetItem(Index: Integer; const Value: TreceitaIngressos);
begin
  Put(Index, Value);
end;

{ ToutrasReceitass }

function ToutrasReceitass.GetItem(Index: Integer): ToutrasReceitas;
begin
  Result := ToutrasReceitas(Inherited Items[Index]);
end;

function ToutrasReceitass.New: ToutrasReceitas;
begin
  Result := ToutrasReceitas.Create;
  Add(Result);
end;

procedure ToutrasReceitass.SetItem(Index: Integer; const Value: ToutrasReceitas);
begin
  Put(Index, Value);
end;

{ TreceitaTotal }

procedure TreceitaTotal.AfterConstruction;
begin
  inherited;
  FinfoProcs := TinfoProcs.Create;
end;

procedure TreceitaTotal.BeforeDestruction;
begin
  inherited;
  FinfoProcs.Free;
end;

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

end.

