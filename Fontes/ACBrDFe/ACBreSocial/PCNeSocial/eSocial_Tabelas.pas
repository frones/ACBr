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
******************************************************************************}
{$I ACBr.inc}

unit eSocial_Tabelas;

interface

uses
  SysUtils, Classes,
  ACBrUtil, eSocial_Conversao,
  eSocial_S1010, eSocial_S1020, eSocial_S1030, eSocial_S1035, eSocial_S1040,
  eSocial_S1050, eSocial_S1070, eSocial_S1080, eSocial_S1060;

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

    procedure GerarXMLs;
    procedure SaveToFiles;
    procedure Clear;

  published
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
  FS1010 := TS1010Collection.Create(AOwner, TS1010CollectionItem);
  FS1020 := TS1020Collection.Create(AOwner, TS1020CollectionItem);
  FS1030 := TS1030Collection.Create(AOwner, TS1030CollectionItem);
  FS1035 := TS1035Collection.Create(AOwner, TS1035CollectionItem);
  FS1040 := TS1040Collection.Create(AOwner, TS1040CollectionItem);
  FS1050 := TS1050Collection.Create(AOwner, TS1050CollectionItem);
  FS1060 := TS1060Collection.Create(AOwner, TS1060CollectionItem);
  FS1070 := TS1070Collection.Create(AOwner, TS1070CollectionItem);
  FS1080 := TS1080Collection.Create(AOwner, TS1080CollectionItem);
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

procedure TTabelas.GerarXMLs;
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

procedure TTabelas.SaveToFiles;
var
  i: integer;
  Path: String;
begin
  Path := TACBreSocial(Self.Owner).Configuracoes.Arquivos.PathSalvar;
  for I := 0 to Self.S1010.Count - 1 do
    Self.S1010.Items[i].EvtTabRubrica.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1010.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1020.Count - 1 do
    Self.S1020.Items[i].EvtTabLotacao.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1020.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1030.Count - 1 do
    Self.S1030.Items[i].EvtTabCargo.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1030.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1035.Count - 1 do
    Self.S1035.Items[i].evtTabCarreira.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1035.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1040.Count - 1 do
    Self.S1040.Items[i].EvtTabFuncao.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1040.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1050.Count - 1 do
    Self.S1050.Items[i].EvtTabHorContratual.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1050.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1060.Count - 1 do
    Self.S1060.Items[i].EvtTabAmbiente.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1060.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1070.Count - 1 do
    Self.S1070.Items[i].EvtTabProcesso.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1070.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1080.Count - 1 do
    Self.S1080.Items[i].EvtTabOperPortuario.SaveToFile(Path+'\'+TipoEventoToStr(Self.S1080.Items[i].TipoEvento)+'-'+IntToStr(i));
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

end.
