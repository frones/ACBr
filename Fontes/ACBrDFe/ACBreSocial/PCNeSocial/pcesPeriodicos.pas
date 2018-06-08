{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}
{$I ACBr.inc}

unit pcesPeriodicos;

interface

uses
  SysUtils, Classes, synautil,
  ACBrUtil, pcesConversaoeSocial,
  pcesS1200, pcesS1202, pcesS1207, pcesS1210,
  pcesS1250, pcesS1260, pcesS1270, pcesS1280, pcesS1295,
  pcesS1298, pcesS1299, pcesS1300;

type

  TPeriodicos = class(TComponent)
  private
    FS1200: TS1200Collection;
    FS1202: TS1202Collection;
    FS1207: TS1207Collection;
    FS1210: TS1210Collection;
    FS1250: TS1250Collection;
    FS1260: TS1260Collection;
    FS1270: TS1270Collection;
    FS1280: TS1280Collection;
    FS1295: TS1295Collection;
    FS1298: TS1298Collection;
    FS1299: TS1299Collection;
    FS1300: TS1300Collection;

    function GetCount: integer;
    procedure setS1200(const Value: TS1200Collection);
    procedure setS1202(const Value: TS1202Collection);
    procedure setS1207(const Value: TS1207Collection);
    procedure setS1210(const Value: TS1210Collection);
    procedure setS1250(const Value: TS1250Collection);
    procedure setS1260(const Value: TS1260Collection);
    procedure setS1270(const Value: TS1270Collection);
    procedure setS1280(const Value: TS1280Collection);
    procedure setS1295(const Value: TS1295Collection);
    procedure setS1298(const Value: TS1298Collection);
    procedure setS1299(const Value: TS1299Collection);
    procedure setS1300(const Value: TS1300Collection);

  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure GerarXMLs;
    procedure SaveToFiles;
    procedure Clear;
    function LoadFromString(AXMLString: String): Boolean;
    function LoadFromIni(AIniString: String): Boolean;

  published
    property Count: Integer read GetCount;
    property S1200: TS1200Collection read FS1200 write setS1200;
    property S1202: TS1202Collection read FS1202 write setS1202;
    property S1207: TS1207Collection read FS1207 write setS1207;
    property S1210: TS1210Collection read FS1210 write setS1210;
    property S1250: TS1250Collection read FS1250 write setS1250;
    property S1260: TS1260Collection read FS1260 write setS1260;
    property S1270: TS1270Collection read FS1270 write setS1270;
    property S1280: TS1280Collection read FS1280 write setS1280;
    property S1295: TS1295Collection read FS1295 write setS1295;
    property S1298: TS1298Collection read FS1298 write setS1298;
    property S1299: TS1299Collection read FS1299 write setS1299;
    property S1300: TS1300Collection read FS1300 write setS1300;

  end;

implementation

uses
  ACBreSocial;

{ TPeriodicos }

procedure TPeriodicos.Clear;
begin
  FS1200.Clear;
  FS1202.Clear;
  FS1207.Clear;
  FS1210.Clear;
  FS1250.Clear;
  FS1260.Clear;
  FS1270.Clear;
  FS1280.Clear;
  FS1295.Clear;
  FS1298.Clear;
  FS1299.Clear;
  FS1300.Clear;
end;

constructor TPeriodicos.Create(AOwner: TComponent);
begin
  inherited;

  FS1200 := TS1200Collection.Create(AOwner, TS1200CollectionItem);
  FS1202 := TS1202Collection.Create(AOwner, TS1202CollectionItem);
  FS1207 := TS1207Collection.Create(AOwner, TS1207CollectionItem);
  FS1210 := TS1210Collection.Create(AOwner, TS1210CollectionItem);
  FS1250 := TS1250Collection.Create(AOwner, TS1250CollectionItem);
  FS1260 := TS1260Collection.Create(AOwner, TS1260CollectionItem);
  FS1270 := TS1270Collection.Create(AOwner, TS1270CollectionItem);
  FS1280 := TS1280Collection.Create(AOwner, TS1280CollectionItem);
  FS1295 := TS1295Collection.Create(AOwner, TS1295CollectionItem);
  FS1298 := TS1298Collection.Create(AOwner, TS1298CollectionItem);
  FS1299 := TS1299Collection.Create(AOwner, TS1299CollectionItem);
  FS1300 := TS1300Collection.Create(AOwner, TS1300CollectionItem);
end;

destructor TPeriodicos.Destroy;
begin
  FS1200.Free;
  FS1202.Free;
  FS1207.Free;
  FS1210.Free;
  FS1250.Free;
  FS1260.Free;
  FS1270.Free;
  FS1280.Free;
  FS1295.Free;
  FS1298.Free;
  FS1299.Free;
  FS1300.Free;

  inherited;
end;

function TPeriodicos.GetCount: Integer;
begin
  Result := self.S1200.Count +
            self.S1202.Count +
            self.S1207.Count +
            self.S1210.Count +
            self.S1250.Count +
            self.S1260.Count +
            self.S1270.Count +
            self.S1280.Count +
            self.S1295.Count +
            self.S1298.Count +
            self.S1299.Count +
            self.S1300.Count;
end;

procedure TPeriodicos.GerarXMLs;
var
  i: Integer;
begin
  for I := 0 to Self.S1200.Count - 1 do
    Self.S1200.Items[i].evtRemun.GerarXML;

  for I := 0 to Self.S1202.Count - 1 do
    Self.S1202.Items[i].EvtRmnRPPS.GerarXML;

  for I := 0 to Self.S1207.Count - 1 do
    Self.S1207.Items[i].evtBenPrRP.GerarXML;

  for I := 0 to Self.S1210.Count - 1 do
    Self.S1210.Items[i].evtPgtos.GerarXML;

  for I := 0 to Self.S1250.Count - 1 do
    Self.S1250.Items[i].EvtAqProd.GerarXML;

  for I := 0 to Self.S1260.Count - 1 do
    Self.S1260.Items[i].EvtComProd.GerarXML;

  for I := 0 to Self.S1270.Count - 1 do
    Self.S1270.Items[i].EvtContratAvNP.GerarXML;

  for I := 0 to Self.S1280.Count - 1 do
    Self.S1280.Items[i].EvtInfoComplPer.GerarXML;

  for I := 0 to Self.S1295.Count - 1 do
    Self.S1295.Items[i].evtTotConting.GerarXML;

  for I := 0 to Self.S1298.Count - 1 do
    Self.S1298.Items[i].EvtReabreEvPer.GerarXML;

  for I := 0 to Self.S1299.Count - 1 do
    Self.S1299.Items[i].EvtFechaEvPer.GerarXML;

  for I := 0 to Self.S1300.Count - 1 do
    Self.S1300.Items[i].EvtContrSindPatr.GerarXML;
end;

procedure TPeriodicos.SaveToFiles;
var
  i: integer;
  Path, PathName: String;
begin
  with TACBreSocial(Self.Owner) do
    Path := PathWithDelim(Configuracoes.Arquivos.GetPatheSocial(Now, Configuracoes.Geral.IdEmpregador));

  for I := 0 to Self.S1200.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1200.Items[i].evtRemun.Id) + '-' +
     TipoEventoToStr(Self.S1200.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1200.Items[i].evtRemun.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1200;
      PathNome := PathName;
      XML := Self.S1200.Items[i].evtRemun.XML;
    end;
  end;

  for I := 0 to Self.S1202.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1202.Items[i].EvtRmnRPPS.Id) + '-' +
     TipoEventoToStr(Self.S1202.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1202.Items[i].EvtRmnRPPS.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1202;
      PathNome := PathName;
      XML := Self.S1202.Items[i].EvtRmnRPPS.XML;
    end;
  end;

  for I := 0 to Self.S1207.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1207.Items[i].evtBenPrRP.Id) + '-' +
     TipoEventoToStr(Self.S1207.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1207.Items[i].evtBenPrRP.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1207;
      PathNome := PathName;
      XML := Self.S1207.Items[i].evtBenPrRP.XML;
    end;
  end;

  for I := 0 to Self.S1210.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1210.Items[i].evtPgtos.Id) + '-' +
     TipoEventoToStr(Self.S1210.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1210.Items[i].evtPgtos.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1210;
      PathNome := PathName;
      XML := Self.S1210.Items[i].evtPgtos.XML;
    end;
  end;

  for I := 0 to Self.S1250.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1250.Items[i].EvtAqProd.Id) + '-' +
     TipoEventoToStr(Self.S1250.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1250.Items[i].EvtAqProd.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1250;
      PathNome := PathName;
      XML := Self.S1250.Items[i].EvtAqProd.XML;
    end;
  end;

  for I := 0 to Self.S1260.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1260.Items[i].EvtComProd.Id) + '-' +
     TipoEventoToStr(Self.S1260.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1260.Items[i].EvtComProd.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1260;
      PathNome := PathName;
      XML := Self.S1260.Items[i].EvtComProd.XML;
    end;
  end;

  for I := 0 to Self.S1270.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1270.Items[i].EvtContratAvNP.Id) + '-' +
     TipoEventoToStr(Self.S1270.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1270.Items[i].EvtContratAvNP.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1270;
      PathNome := PathName;
      XML := Self.S1270.Items[i].EvtContratAvNP.XML;
    end;
  end;

  for I := 0 to Self.S1280.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1280.Items[i].EvtInfoComplPer.Id) + '-' +
     TipoEventoToStr(Self.S1280.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1280.Items[i].EvtInfoComplPer.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1280;
      PathNome := PathName;
      XML := Self.S1280.Items[i].EvtInfoComplPer.XML;
    end;
  end;

  for I := 0 to Self.S1295.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1295.Items[i].evtTotConting.Id) + '-' +
     TipoEventoToStr(Self.S1295.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1295.Items[i].evtTotConting.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1295;
      PathNome := PathName;
      XML := Self.S1295.Items[i].evtTotConting.XML;
    end;
  end;

  for I := 0 to Self.S1298.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1298.Items[i].EvtReabreEvPer.Id) + '-' +
     TipoEventoToStr(Self.S1298.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1298.Items[i].EvtReabreEvPer.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1298;
      PathNome := PathName;
      XML := Self.S1298.Items[i].EvtReabreEvPer.XML;
    end;
  end;

  for I := 0 to Self.S1299.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1299.Items[i].EvtFechaEvPer.Id) + '-' +
     TipoEventoToStr(Self.S1299.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1299.Items[i].EvtFechaEvPer.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1299;
      PathNome := PathName;
      XML := Self.S1299.Items[i].EvtFechaEvPer.XML;
    end;
  end;

  for I := 0 to Self.S1300.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1300.Items[i].EvtContrSindPatr.Id) + '-' +
     TipoEventoToStr(Self.S1300.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1300.Items[i].EvtContrSindPatr.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teS1300;
      PathNome := PathName;
      XML := Self.S1300.Items[i].EvtContrSindPatr.XML;
    end;
  end;
end;

procedure TPeriodicos.setS1200(const Value: TS1200Collection);
begin
  FS1200.Assign(Value);
end;

procedure TPeriodicos.setS1202(const Value: TS1202Collection);
begin
  FS1202.Assign(Value);
end;

procedure TPeriodicos.setS1207(const Value: TS1207Collection);
begin
  FS1207.Assign(Value);
end;

procedure TPeriodicos.setS1210(const Value: TS1210Collection);
begin
  FS1210.Assign(Value);
end;

procedure TPeriodicos.setS1250(const Value: TS1250Collection);
begin
  FS1250.Assign(Value);
end;

procedure TPeriodicos.setS1260(const Value: TS1260Collection);
begin
  FS1260.Assign(Value);
end;

procedure TPeriodicos.setS1270(const Value: TS1270Collection);
begin
  FS1270.Assign(Value);
end;

procedure TPeriodicos.setS1280(const Value: TS1280Collection);
begin
  FS1280.Assign(Value);
end;

procedure TPeriodicos.setS1295(const Value: TS1295Collection);
begin
  FS1295.Assign(Value);
end;

procedure TPeriodicos.setS1298(const Value: TS1298Collection);
begin
  FS1298.Assign(Value);
end;

procedure TPeriodicos.setS1299(const Value: TS1299Collection);
begin
  FS1299.Assign(Value);
end;

procedure TPeriodicos.setS1300(const Value: TS1300Collection);
begin
  FS1300.Assign(Value);
end;

function TPeriodicos.LoadFromString(AXMLString: String): Boolean;
var
  Ok: Boolean;
begin
  case StringXMLToTipoEvento(Ok, AXMLString) of
    teS1200: Self.S1200.Add.evtRemun.XML := AXMLString;
    teS1202: Self.S1202.Add.EvtRmnRPPS.XML := AXMLString;
    teS1207: Self.S1207.Add.evtBenPrRP.XML := AXMLString;
    teS1210: Self.S1210.Add.evtPgtos.XML := AXMLString;
    teS1250: Self.S1250.Add.EvtAqProd.XML := AXMLString;
    teS1260: Self.S1260.Add.EvtComProd.XML := AXMLString;
    teS1270: Self.S1270.Add.EvtContratAvNP.XML := AXMLString;
    teS1280: Self.S1280.Add.EvtInfoComplPer.XML := AXMLString;
    teS1295: Self.S1295.Add.evtTotConting.XML := AXMLString;
    teS1298: Self.S1298.Add.EvtReabreEvPer.XML := AXMLString;
    teS1299: Self.S1299.Add.EvtFechaEvPer.XML := AXMLString;
    teS1300: Self.S1300.Add.EvtContrSindPatr.XML := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TPeriodicos.LoadFromIni(AIniString: String): Boolean;
var
  Ok: Boolean;
begin
  case StringINIToTipoEvento(Ok, AIniString) of
    teS1200: Self.S1200.Add.evtRemun.LerArqIni(AIniString);
    teS1202: Self.S1202.Add.EvtRmnRPPS.LerArqIni(AIniString);
    teS1207: Self.S1207.Add.evtBenPrRP.LerArqIni(AIniString);
    teS1210: Self.S1210.Add.evtPgtos.LerArqIni(AIniString);
    teS1250: Self.S1250.Add.EvtAqProd.LerArqIni(AIniString);
    teS1260: Self.S1260.Add.EvtComProd.LerArqIni(AIniString);
    teS1270: Self.S1270.Add.EvtContratAvNP.LerArqIni(AIniString);
    teS1280: Self.S1280.Add.EvtInfoComplPer.LerArqIni(AIniString);
    teS1295: Self.S1295.Add.evtTotConting.LerArqIni(AIniString);
    teS1298: Self.S1298.Add.EvtReabreEvPer.LerArqIni(AIniString);
    teS1299: Self.S1299.Add.EvtFechaEvPer.LerArqIni(AIniString);
    teS1300: Self.S1300.Add.EvtContrSindPatr.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.

