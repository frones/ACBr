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

unit pcesTabelas;

interface

uses
  SysUtils, Classes, synautil,
  pcesConversaoeSocial,
  pcesS1010, pcesS1020, pcesS1030, pcesS1035, pcesS1040,
  pcesS1050, pcesS1070, pcesS1080, pcesS1060;

type

  TTabelas = class(TComponent)
  private
    FS1010: TS1010Collection;
    FS1020: TS1020Collection;
    FS1030: TS1030Collection;
    FS1035: TS1035Collection;
    FS1040: TS1040Collection;
    FS1050: TS1050Collection;
    FS1060: TS1060Collection;
    FS1070: TS1070Collection;
    FS1080: TS1080Collection;

    function GetCount: integer;
    procedure setS1010(const Value: TS1010Collection);
    procedure setS1020(const Value: TS1020Collection);
    procedure setS1030(const Value: TS1030Collection);
    procedure setS1035(const Value: TS1035Collection);
    procedure setS1040(const Value: TS1040Collection);
    procedure setS1050(const Value: TS1050Collection);
    procedure setS1060(const Value: TS1060Collection);
    procedure setS1070(const Value: TS1070Collection);
    procedure setS1080(const Value: TS1080Collection);

  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure Gerar;
    procedure Assinar;
    procedure Validar;
    procedure SaveToFiles;
    procedure Clear;
    function LoadFromString(const AXMLString: String): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

  published
    property Count: Integer read GetCount;
    property S1010: TS1010Collection read FS1010 write setS1010;
    property S1020: TS1020Collection read FS1020 write setS1020;
    property S1030: TS1030Collection read FS1030 write setS1030;
    property S1035: TS1035Collection read FS1035 write setS1035;
    property S1040: TS1040Collection read FS1040 write setS1040;
    property S1050: TS1050Collection read FS1050 write setS1050;
    property S1060: TS1060Collection read FS1060 write setS1060;
    property S1070: TS1070Collection read FS1070 write setS1070;
    property S1080: TS1080Collection read FS1080 write setS1080;

  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TTabelas }

procedure TTabelas.Clear;
begin
  FS1010.Clear;
  FS1020.Clear;
  FS1030.Clear;
  FS1035.Clear;
  FS1040.Clear;
  FS1050.Clear;
  FS1060.Clear;
  FS1070.Clear;
  FS1080.Clear;
end;

constructor TTabelas.Create(AOwner: TComponent);
begin
  inherited;
  FS1010 := TS1010Collection.Create(AOwner);
  FS1020 := TS1020Collection.Create(AOwner);
  FS1030 := TS1030Collection.Create(AOwner);
  FS1035 := TS1035Collection.Create(AOwner);
  FS1040 := TS1040Collection.Create(AOwner);
  FS1050 := TS1050Collection.Create(AOwner);
  FS1060 := TS1060Collection.Create(AOwner);
  FS1070 := TS1070Collection.Create(AOwner);
  FS1080 := TS1080Collection.Create(AOwner);
end;

destructor TTabelas.Destroy;
begin
  FS1010.Free;
  FS1020.Free;
  FS1030.Free;
  FS1035.Free;
  FS1040.Free;
  FS1050.Free;
  FS1060.Free;
  FS1070.Free;
  FS1080.Free;
  inherited;
end;

function TTabelas.GetCount: Integer;
begin
  Result := self.S1010.Count +
            self.S1020.Count +
            self.S1030.Count +
            self.S1035.Count +
            self.S1040.Count +
            self.S1050.Count +
            self.S1060.Count +
            self.S1070.Count +
            self.S1080.Count;
end;

procedure TTabelas.Gerar;
var
  i: Integer;
begin
  for I := 0 to Self.S1010.Count - 1 do
    Self.S1010.Items[i].EvtTabRubrica.GerarXML;

  for I := 0 to Self.S1020.Count - 1 do
    Self.S1020.Items[i].EvtTabLotacao.GerarXML;

  for I := 0 to Self.S1030.Count - 1 do
    Self.S1030.Items[i].EvtTabCargo.GerarXML;

  for I := 0 to Self.S1035.Count - 1 do
    Self.S1035.Items[i].evtTabCarreira.GerarXML;

  for I := 0 to Self.S1040.Count - 1 do
    Self.S1040.Items[i].EvtTabFuncao.GerarXML;

  for I := 0 to Self.S1050.Count - 1 do
    Self.S1050.Items[i].EvtTabHorContratual.GerarXML;

  for I := 0 to Self.S1060.Count - 1 do
    Self.S1060.Items[i].EvtTabAmbiente.GerarXML;

  for I := 0 to Self.S1070.Count - 1 do
    Self.S1070.Items[i].EvtTabProcesso.GerarXML;

  for I := 0 to Self.S1080.Count - 1 do
    Self.S1080.Items[i].EvtTabOperPortuario.GerarXML;
end;

procedure TTabelas.Assinar;
var
  i: Integer;
begin
  for I := 0 to Self.S1010.Count - 1 do
    Self.S1010.Items[i].EvtTabRubrica.XML :=
    Self.S1010.Items[i].EvtTabRubrica.Assinar(Self.S1010.Items[i].EvtTabRubrica.XML, 'evtTabRubrica');

  for I := 0 to Self.S1020.Count - 1 do
    Self.S1020.Items[i].EvtTabLotacao.XML :=
    Self.S1020.Items[i].EvtTabLotacao.Assinar(Self.S1020.Items[i].EvtTabLotacao.XML, 'evtTabLotacao');

  for I := 0 to Self.S1030.Count - 1 do
    Self.S1030.Items[i].EvtTabCargo.XML :=
    Self.S1030.Items[i].EvtTabCargo.Assinar(Self.S1030.Items[i].EvtTabCargo.XML, 'evtTabCargo');

  for I := 0 to Self.S1035.Count - 1 do
    Self.S1035.Items[i].evtTabCarreira.XML :=
    Self.S1035.Items[i].evtTabCarreira.Assinar(Self.S1035.Items[i].evtTabCarreira.XML, 'evtTabCarreira');

  for I := 0 to Self.S1040.Count - 1 do
    Self.S1040.Items[i].EvtTabFuncao.XML :=
    Self.S1040.Items[i].EvtTabFuncao.Assinar(Self.S1040.Items[i].EvtTabFuncao.XML, 'evtTabFuncao');

  for I := 0 to Self.S1050.Count - 1 do
    Self.S1050.Items[i].EvtTabHorContratual.XML :=
    Self.S1050.Items[i].EvtTabHorContratual.Assinar(Self.S1050.Items[i].EvtTabHorContratual.XML, 'evtTabHorTur');

  for I := 0 to Self.S1060.Count - 1 do
    Self.S1060.Items[i].EvtTabAmbiente.XML :=
    Self.S1060.Items[i].EvtTabAmbiente.Assinar(Self.S1060.Items[i].EvtTabAmbiente.XML, 'evtTabAmbiente');

  for I := 0 to Self.S1070.Count - 1 do
    Self.S1070.Items[i].EvtTabProcesso.XML :=
    Self.S1070.Items[i].EvtTabProcesso.Assinar(Self.S1070.Items[i].EvtTabProcesso.XML, 'evtTabProcesso');

  for I := 0 to Self.S1080.Count - 1 do
    Self.S1080.Items[i].EvtTabOperPortuario.XML :=
    Self.S1080.Items[i].EvtTabOperPortuario.Assinar(Self.S1080.Items[i].EvtTabOperPortuario.XML, 'evtTabOperPort');
end;

procedure TTabelas.Validar;
var
  i: Integer;
begin
  for I := 0 to Self.S1010.Count - 1 do
    Self.S1010.Items[i].EvtTabRubrica.Validar(schevtTabRubrica);

  for I := 0 to Self.S1020.Count - 1 do
    Self.S1020.Items[i].EvtTabLotacao.Validar(schevtTabLotacao);

  for I := 0 to Self.S1030.Count - 1 do
    Self.S1030.Items[i].EvtTabCargo.Validar(schevtTabCargo);

  for I := 0 to Self.S1035.Count - 1 do
    Self.S1035.Items[i].evtTabCarreira.Validar(schevtTabCarreira);

  for I := 0 to Self.S1040.Count - 1 do
    Self.S1040.Items[i].EvtTabFuncao.Validar(schevtTabFuncao);

  for I := 0 to Self.S1050.Count - 1 do
    Self.S1050.Items[i].EvtTabHorContratual.Validar(schevtTabHorTur);

  for I := 0 to Self.S1060.Count - 1 do
    Self.S1060.Items[i].EvtTabAmbiente.Validar(schevtTabAmbiente);

  for I := 0 to Self.S1070.Count - 1 do
    Self.S1070.Items[i].EvtTabProcesso.Validar(schevtTabProcesso);

  for I := 0 to Self.S1080.Count - 1 do
    Self.S1080.Items[i].EvtTabOperPortuario.Validar(schevtTabOperPort);
end;

procedure TTabelas.SaveToFiles;
var
  i: integer;
  Path, PathName: String;
begin
  with TACBreSocial(Self.Owner) do
    Path := PathWithDelim(Configuracoes.Arquivos.GetPatheSocial(Now, Configuracoes.Geral.IdEmpregador));

  for I := 0 to Self.S1010.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1010.Items[i].EvtTabRubrica.Id) + '-' +
     TipoEventoToStr(Self.S1010.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1010.Items[i].EvtTabRubrica.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1010;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1010.Items[i].EvtTabRubrica.Id);
      XML := Self.S1010.Items[i].EvtTabRubrica.XML;
    end;
  end;

  for I := 0 to Self.S1020.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1020.Items[i].EvtTabLotacao.Id) + '-' +
     TipoEventoToStr(Self.S1020.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1020.Items[i].EvtTabLotacao.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1020;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1020.Items[i].EvtTabLotacao.Id);
      XML := Self.S1020.Items[i].EvtTabLotacao.XML;
    end;
  end;

  for I := 0 to Self.S1030.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1030.Items[i].EvtTabCargo.Id) + '-' +
     TipoEventoToStr(Self.S1030.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1030.Items[i].EvtTabCargo.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1030;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1030.Items[i].EvtTabCargo.Id);
      XML := Self.S1030.Items[i].EvtTabCargo.XML;
    end;
  end;

  for I := 0 to Self.S1035.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1035.Items[i].evtTabCarreira.Id) + '-' +
     TipoEventoToStr(Self.S1035.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1035.Items[i].evtTabCarreira.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1035;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1035.Items[i].evtTabCarreira.Id);
      XML := Self.S1035.Items[i].evtTabCarreira.XML;
    end;
  end;

  for I := 0 to Self.S1040.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1040.Items[i].EvtTabFuncao.Id) + '-' +
     TipoEventoToStr(Self.S1040.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1040.Items[i].EvtTabFuncao.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1040;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1040.Items[i].EvtTabFuncao.Id);
      XML := Self.S1040.Items[i].EvtTabFuncao.XML;
    end;
  end;

  for I := 0 to Self.S1050.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1050.Items[i].EvtTabHorContratual.Id) + '-' +
     TipoEventoToStr(Self.S1050.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1050.Items[i].EvtTabHorContratual.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1050;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1050.Items[i].EvtTabHorContratual.Id);
      XML := Self.S1050.Items[i].EvtTabHorContratual.XML;
    end;
  end;

  for I := 0 to Self.S1060.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1060.Items[i].EvtTabAmbiente.Id) + '-' +
     TipoEventoToStr(Self.S1060.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1060.Items[i].EvtTabAmbiente.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1060;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1060.Items[i].EvtTabAmbiente.Id);
      XML := Self.S1060.Items[i].EvtTabAmbiente.XML;
    end;
  end;

  for I := 0 to Self.S1070.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1070.Items[i].EvtTabProcesso.Id) + '-' +
     TipoEventoToStr(Self.S1070.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1070.Items[i].EvtTabProcesso.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1070;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1070.Items[i].EvtTabProcesso.Id);
      XML := Self.S1070.Items[i].EvtTabProcesso.XML;
    end;
  end;

  for I := 0 to Self.S1080.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S1080.Items[i].EvtTabOperPortuario.Id) + '-' +
     TipoEventoToStr(Self.S1080.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S1080.Items[i].EvtTabOperPortuario.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS1080;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S1080.Items[i].EvtTabOperPortuario.Id);
      XML := Self.S1080.Items[i].EvtTabOperPortuario.XML;
    end;
  end;
end;

procedure TTabelas.setS1010(const Value: TS1010Collection);
begin
  FS1010.Assign(Value);
end;

procedure TTabelas.setS1020(const Value: TS1020Collection);
begin
  FS1020.Assign(Value);
end;

procedure TTabelas.setS1030(const Value: TS1030Collection);
begin
  FS1030.Assign(Value);
end;

procedure TTabelas.setS1035(const Value: TS1035Collection);
begin
  FS1035.Assign(Value);
end;

procedure TTabelas.setS1040(const Value: TS1040Collection);
begin
  FS1040.Assign(Value);
end;

procedure TTabelas.setS1050(const Value: TS1050Collection);
begin
  FS1050.Assign(Value);
end;

procedure TTabelas.setS1060(const Value: TS1060Collection);
begin
  FS1060.Assign(Value);
end;

procedure TTabelas.setS1070(const Value: TS1070Collection);
begin
  FS1070.Assign(Value);
end;

procedure TTabelas.setS1080(const Value: TS1080Collection);
begin
  FS1080.Assign(Value);
end;

function TTabelas.LoadFromString(const AXMLString: String): Boolean;
var
  Ok: Boolean;
  typVersaoDF : TVersaoeSocial;
begin
  typVersaoDF := TACBreSocial(Self.Owner).Configuracoes.Geral.VersaoDF;

  case StringXMLToTipoEvento(Ok, AXMLString, typVersaoDF) of
    teS1010:
    begin
      with Self.S1010.New do
      begin
        EvtTabRubrica.XML := AXMLString;
        EvtTabRubrica.LerXML;
      end;
    end;
    teS1020: Self.S1020.New.EvtTabLotacao.XML := AXMLString;
    teS1030: Self.S1030.New.EvtTabCargo.XML := AXMLString;
    teS1035: Self.S1035.New.evtTabCarreira.XML := AXMLString;
    teS1040: Self.S1040.New.EvtTabFuncao.XML := AXMLString;
    teS1050: Self.S1050.New.EvtTabHorContratual.XML := AXMLString;
    teS1060:
    begin
      with Self.S1060.New do
      begin
        EvtTabAmbiente.XML := AXMLString;
        EvtTabAmbiente.LerXML;
      end;
    end;
    teS1070: Self.S1070.New.EvtTabProcesso.XML := AXMLString;
    teS1080: Self.S1080.New.EvtTabOperPortuario.XML := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TTabelas.LoadFromIni(const AIniString: String): Boolean;
var
  Ok: Boolean;
  typVersaoDF : TVersaoeSocial;
begin
  typVersaoDF := TACBreSocial(Self.Owner).Configuracoes.Geral.VersaoDF;
  
  case StringINIToTipoEvento(Ok, AIniString, typVersaoDF) of
    teS1010: Self.S1010.New.EvtTabRubrica.LerArqIni(AIniString);
    teS1020: Self.S1020.New.EvtTabLotacao.LerArqIni(AIniString);
    teS1030: Self.S1030.New.EvtTabCargo.LerArqIni(AIniString);
    teS1035: Self.S1035.New.evtTabCarreira.LerArqIni(AIniString);
    teS1040: Self.S1040.New.EvtTabFuncao.LerArqIni(AIniString);
    teS1050: Self.S1050.New.EvtTabHorContratual.LerArqIni(AIniString);
    teS1060: Self.S1060.New.EvtTabAmbiente.LerArqIni(AIniString);
    teS1070: Self.S1070.New.EvtTabProcesso.LerArqIni(AIniString);
    teS1080: Self.S1080.New.EvtTabOperPortuario.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.
