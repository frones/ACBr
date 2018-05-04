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

{$I ACBr.inc}

unit pcnEventosReinf;

interface

uses
  SysUtils, Classes, synautil,
  ACBrUtil, pcnConversaoReinf,
  pcnReinfR1000, pcnReinfR1070, pcnReinfR2010, pcnReinfR2020, pcnReinfR2030,
  pcnReinfR2040, pcnReinfR2050, pcnReinfR2060, pcnReinfR2070, pcnReinfR2098,
  pcnReinfR2099, pcnReinfR3010, pcnReinfR9000;

type

  TReinfEventos = class(TComponent)
  private
    FR1000: TR1000Collection;
    FR1070: TR1070Collection;
    FR2010: TR2010Collection;
    FR2020: TR2020Collection;
    FR2030: TR2030Collection;
    FR2040: TR2040Collection;
    FR2050: TR2050Collection;
    FR2060: TR2060Collection;
    FR2070: TR2070Collection;
    FR2098: TR2098Collection;
    FR2099: TR2099Collection;
    FR3010: TR3010Collection;
    FR9000: TR9000Collection;

    function GetCount: integer;
    procedure setR1000(const Value: TR1000Collection);
    procedure setR1070(const Value: TR1070Collection);
    procedure setR2010(const Value: TR2010Collection);
    procedure setR2020(const Value: TR2020Collection);
    procedure setR2030(const Value: TR2030Collection);
    procedure setR2040(const Value: TR2040Collection);
    procedure setR2050(const Value: TR2050Collection);
    procedure setR2060(const Value: TR2060Collection);
    procedure setR2070(const Value: TR2070Collection);
    procedure setR2098(const Value: TR2098Collection);
    procedure setR2099(const Value: TR2099Collection);
    procedure setR3010(const Value: TR3010Collection);
    procedure setR9000(const Value: TR9000Collection);
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
    property R1000: TR1000Collection read FR1000 write setR1000;
    property R1070: TR1070Collection read FR1070 write setR1070;
    property R2010: TR2010Collection read FR2010 write setR2010;
    property R2020: TR2020Collection read FR2020 write setR2020;
    property R2030: TR2030Collection read FR2030 write setR2030;
    property R2040: TR2040Collection read FR2040 write setR2040;
    property R2050: TR2050Collection read FR2050 write setR2050;
    property R2060: TR2060Collection read FR2060 write setR2060;
    property R2070: TR2070Collection read FR2070 write setR2070;
    property R2098: TR2098Collection read FR2098 write setR2098;
    property R2099: TR2099Collection read FR2099 write setR2099;
    property R3010: TR3010Collection read FR3010 write setR3010;
    property R9000: TR9000Collection read FR9000 write setR9000;
  end;

implementation

uses
  ACBrReinf;

{ TReinfEventos }

procedure TReinfEventos.Clear;
begin
  FR1000.Clear;
  FR1070.Clear;
  FR2010.Clear;
  FR2020.Clear;
  FR2030.Clear;
  FR2040.Clear;
  FR2050.Clear;
  FR2060.Clear;
  FR2070.Clear;
  FR2098.Clear;
  FR2099.Clear;
  FR3010.Clear;
  FR9000.Clear;
end;

constructor TReinfEventos.Create(AOwner: TComponent);
begin
  inherited;

  FR1000 := TR1000Collection.Create(AOwner, TR1000CollectionItem);
  FR1070 := TR1070Collection.Create(AOwner, TR1070CollectionItem);
  FR2010 := TR2010Collection.Create(AOwner, TR2010CollectionItem);
  FR2020 := TR2020Collection.Create(AOwner, TR2020CollectionItem);
  FR2030 := TR2030Collection.Create(AOwner, TR2030CollectionItem);
  FR2040 := TR2040Collection.Create(AOwner, TR2040CollectionItem);
  FR2050 := TR2050Collection.Create(AOwner, TR2050CollectionItem);
  FR2060 := TR2060Collection.Create(AOwner, TR2060CollectionItem);
  FR2070 := TR2070Collection.Create(AOwner, TR2070CollectionItem);
  FR2098 := TR2098Collection.Create(AOwner, TR2098CollectionItem);
  FR2099 := TR2099Collection.Create(AOwner, TR2099CollectionItem);
  FR3010 := TR3010Collection.Create(AOwner, TR3010CollectionItem);
  FR9000 := TR9000Collection.Create(AOwner, TR9000CollectionItem);
end;

destructor TReinfEventos.Destroy;
begin
  FR1000.Free;
  FR1070.Free;
  FR2010.Free;
  FR2020.Free;
  FR2030.Free;
  FR2040.Free;
  FR2050.Free;
  FR2060.Free;
  FR2070.Free;
  FR2098.Free;
  FR2099.Free;
  FR3010.Free;
  FR9000.Free;

  inherited;
end;

function TReinfEventos.GetCount: Integer;
begin
  Result := self.R1000.Count + self.R1070.Count + Self.R2010.Count +
            Self.R2020.Count + Self.R2030.Count + Self.R2040.Count +
            Self.R2050.Count + Self.R2060.Count + Self.R2070.Count +
            Self.R2098.Count + Self.R2099.Count + Self.R3010.Count +
            Self.R9000.Count;
end;

procedure TReinfEventos.GerarXMLs;
var
  i: Integer;
begin
  for i := 0 to Self.R1000.Count - 1 do
    Self.R1000.Items[i].evtInfoContri.GerarXML;

  for i := 0 to Self.R1070.Count - 1 do
    Self.R1070.Items[i].evtTabProcesso.GerarXML;

  for i := 0 to Self.R2010.Count - 1 do
    Self.R2010.Items[i].evtServTom.GerarXML;

  for i := 0 to Self.R2020.Count - 1 do
    Self.R2020.Items[i].evtServPrest.GerarXML;

  for i := 0 to Self.R2030.Count - 1 do
    Self.R2030.Items[i].evtAssocDespRec.GerarXML;

  for i := 0 to Self.R2040.Count - 1 do
    Self.R2040.Items[i].evtAssocDespRep.GerarXML;

  for i := 0 to Self.R2050.Count - 1 do
    Self.R2050.Items[i].evtComProd.GerarXML;

  for i := 0 to Self.R2060.Count - 1 do
    Self.R2060.Items[i].evtCPRB.GerarXML;

  for i := 0 to Self.R2070.Count - 1 do
    Self.R2070.Items[i].evtPgtosDivs.GerarXML;

  for i := 0 to Self.R2098.Count - 1 do
    Self.R2098.Items[i].evtReabreEvPer.GerarXML;

  for i := 0 to Self.R2099.Count - 1 do
    Self.R2099.Items[i].evtFechaEvPer.GerarXML;

  for i := 0 to Self.R3010.Count - 1 do
    Self.R3010.Items[i].evtEspDesportivo.GerarXML;

  for i := 0 to Self.R9000.Count - 1 do
    Self.R9000.Items[i].evtExclusao.GerarXML;
end;

procedure TReinfEventos.SaveToFiles;
var
  i: integer;
  Path, PathName: String;
begin
  with TACBrReinf(Self.Owner) do
  begin
    Path := Configuracoes.Arquivos.PathSalvar;
    if trim(Path) = '' then
      Path := PathWithDelim(Configuracoes.Arquivos.GetPathReinf(Now, Configuracoes.Geral.IdContribuinte));
  end;

  for i := 0 to Self.R1000.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R1000.Items[i].evtInfoContri.Id) + '-' +
     TipoEventoToStr(Self.R1000.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R1000.Items[i].evtInfoContri.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR1000;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R1070.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R1070.Items[i].evtTabProcesso.Id) + '-' +
     TipoEventoToStr(Self.R1070.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R1070.Items[i].evtTabProcesso.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR1070;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2010.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2010.Items[i].evtServTom.Id) + '-' +
     TipoEventoToStr(Self.R2010.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2010.Items[i].evtServTom.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2010;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2020.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2020.Items[i].evtServPrest.Id) + '-' +
     TipoEventoToStr(Self.R2020.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2020.Items[i].evtServPrest.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2020;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2030.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2030.Items[i].evtAssocDespRec.Id) + '-' +
     TipoEventoToStr(Self.R2030.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2030.Items[i].evtAssocDespRec.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2030;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2040.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2040.Items[i].evtAssocDespRep.Id) + '-' +
     TipoEventoToStr(Self.R2040.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2040.Items[i].evtAssocDespRep.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2040;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2050.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2050.Items[i].evtComProd.Id) + '-' +
     TipoEventoToStr(Self.R2050.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2050.Items[i].evtComProd.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2050;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2060.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2060.Items[i].evtCPRB.Id) + '-' +
     TipoEventoToStr(Self.R2060.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2060.Items[i].evtCPRB.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2060;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2070.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2070.Items[i].evtPgtosDivs.Id) + '-' +
     TipoEventoToStr(Self.R2070.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2070.Items[i].evtPgtosDivs.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2070;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2098.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2098.Items[i].evtReabreEvPer.Id) + '-' +
     TipoEventoToStr(Self.R2098.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2098.Items[i].evtReabreEvPer.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2098;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R2099.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2099.Items[i].evtFechaEvPer.Id) + '-' +
     TipoEventoToStr(Self.R2099.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2099.Items[i].evtFechaEvPer.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR2099;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R3010.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R3010.Items[i].evtEspDesportivo.Id) + '-' +
     TipoEventoToStr(Self.R3010.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R3010.Items[i].evtEspDesportivo.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR3010;
      PathNome := PathName;
    end;
  end;

  for i := 0 to Self.R9000.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R9000.Items[i].evtExclusao.Id) + '-' +
     TipoEventoToStr(Self.R9000.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R9000.Items[i].evtExclusao.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.Add do
    begin
      TipoEvento := teR9000;
      PathNome := PathName;
    end;
  end;
end;

procedure TReinfEventos.setR1000(const Value: TR1000Collection);
begin
  FR1000.Assign(Value);
end;

procedure TReinfEventos.setR1070(const Value: TR1070Collection);
begin
  FR1070.Assign(Value);
end;

procedure TReinfEventos.setR2010(const Value: TR2010Collection);
begin
  FR2010.Assign(Value);
end;

procedure TReinfEventos.setR2020(const Value: TR2020Collection);
begin
  FR2020.Assign(Value);
end;

procedure TReinfEventos.setR2030(const Value: TR2030Collection);
begin
  FR2030.Assign(Value);
end;

procedure TReinfEventos.setR2040(const Value: TR2040Collection);
begin
  FR2040.Assign(Value);
end;

procedure TReinfEventos.setR2050(const Value: TR2050Collection);
begin
  FR2050.Assign(Value);
end;

procedure TReinfEventos.setR2060(const Value: TR2060Collection);
begin
  FR2060.Assign(Value);
end;

procedure TReinfEventos.setR2070(const Value: TR2070Collection);
begin
  FR2070.Assign(Value);
end;

procedure TReinfEventos.setR2098(const Value: TR2098Collection);
begin
  FR2098.Assign(Value);
end;

procedure TReinfEventos.setR2099(const Value: TR2099Collection);
begin
  FR2099.Assign(Value);
end;

procedure TReinfEventos.setR3010(const Value: TR3010Collection);
begin
  FR3010.Assign(Value);
end;

procedure TReinfEventos.setR9000(const Value: TR9000Collection);
begin
  FR9000.Assign(Value);
end;

function TReinfEventos.LoadFromString(AXMLString: String): Boolean;
var
  Ok: Boolean;
begin
  case StrEventoToTipoEvento(Ok, AXMLString) of
    teR1000: Self.R1000.Add.evtInfoContri.XML    := AXMLString;
    teR1070: Self.R1070.Add.evtTabProcesso.XML   := AXMLString;
    teR2010: Self.R2010.Add.evtServTom.XML       := AXMLString;
    teR2020: Self.R2020.Add.evtServPrest.XML     := AXMLString;
    teR2030: Self.R2030.Add.evtAssocDespRec.XML  := AXMLString;
    teR2040: Self.R2040.Add.evtAssocDespRep.XML  := AXMLString;
    teR2050: Self.R2050.Add.evtComProd.XML       := AXMLString;
    teR2060: Self.R2060.Add.evtCPRB.XML          := AXMLString;
    teR2070: Self.R2070.Add.evtPgtosDivs.XML     := AXMLString;
    teR2098: Self.R2098.Add.evtReabreEvPer.XML   := AXMLString;
    teR2099: Self.R2099.Add.evtFechaEvPer.XML    := AXMLString;
    teR3010: Self.R3010.Add.evtEspDesportivo.XML := AXMLString;
    teR9000: Self.R9000.Add.evtExclusao.XML      := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TReinfEventos.LoadFromIni(AIniString: String): Boolean;
var
  Ok: Boolean;
begin
  case StringToTipoEvento(Ok, AIniString) of
    teR1000: Self.R1000.Add.evtInfoContri.LerArqIni(AIniString);
    teR1070: Self.R1070.Add.evtTabProcesso.LerArqIni(AIniString);
    teR2010: Self.R2010.Add.evtServTom.LerArqIni(AIniString);
    teR2020: Self.R2020.Add.evtServPrest.LerArqIni(AIniString);
    teR2030: Self.R2030.Add.evtAssocDespRec.LerArqIni(AIniString);
    teR2040: Self.R2040.Add.evtAssocDespRep.LerArqIni(AIniString);
    teR2050: Self.R2050.Add.evtComProd.LerArqIni(AIniString);
    teR2060: Self.R2060.Add.evtCPRB.LerArqIni(AIniString);
    teR2070: Self.R2070.Add.evtPgtosDivs.LerArqIni(AIniString);
    teR2098: Self.R2098.Add.evtReabreEvPer.LerArqIni(AIniString);
    teR2099: Self.R2099.Add.evtFechaEvPer.LerArqIni(AIniString);
    teR3010: Self.R3010.Add.evtEspDesportivo.LerArqIni(AIniString);
    teR9000: Self.R9000.Add.evtExclusao.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.
