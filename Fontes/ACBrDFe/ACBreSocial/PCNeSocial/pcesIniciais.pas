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

unit pcesIniciais;

interface

uses
  SysUtils, Classes, synautil,
  ACBrUtil, pcesConversaoeSocial,
  pcesS1000, pcesS1005;

type

  TIniciais = class(TComponent)
  private
    FS1000: TS1000Collection;
    FS1005: TS1005Collection;

    function GetCount: integer;
    procedure setS1000(const Value: TS1000Collection);
    procedure setS1005(const Value: TS1005Collection);

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
    property S1000: TS1000Collection read FS1000 write setS1000;
    property S1005: TS1005Collection read FS1005 write setS1005;

  end;

implementation

uses
  ACBreSocial;

{ TIniciais }

procedure TIniciais.Clear;
begin
  FS1000.Clear;
  FS1005.Clear;
end;

constructor TIniciais.Create(AOwner: TComponent);
begin
  inherited;

  FS1000 := TS1000Collection.Create(AOwner, TS1000CollectionItem);
  FS1005 := TS1005Collection.Create(AOwner, TS1005CollectionItem);
end;

destructor TIniciais.Destroy;
begin
  FS1000.Free;
  FS1005.Free;

  inherited;
end;

function TIniciais.GetCount: Integer;
begin
  Result := self.S1000.Count +
            self.S1005.Count;
end;

procedure TIniciais.GerarXMLs;
var
  i: Integer;
begin
  for I := 0 to Self.S1000.Count - 1 do
    Self.S1000.Items[i].evtInfoEmpregador.GerarXML;

  for I := 0 to Self.S1005.Count - 1 do
    Self.S1005.Items[i].evtTabEstab.GerarXML;
end;

procedure TIniciais.SaveToFiles;
var
  i: integer;
  Path: String;
begin
  with TACBreSocial(Self.Owner) do
    Path := PathWithDelim(Configuracoes.Arquivos.GetPatheSocial(Now, Configuracoes.Geral.IdEmpregador));

  for I := 0 to Self.S1000.Count - 1 do
    Self.S1000.Items[i].evtInfoEmpregador.SaveToFile(Path + '\' +
     OnlyNumber(Self.S1000.Items[i].evtInfoEmpregador.Id) + '-' +
     TipoEventoToStr(Self.S1000.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S1005.Count - 1 do
    Self.S1005.Items[i].evtTabEstab.SaveToFile(Path +'\' +
     OnlyNumber(Self.S1005.Items[i].evtTabEstab.Id) + '-' +
     TipoEventoToStr(Self.S1005.Items[i].TipoEvento)+'-'+IntToStr(i));
end;

procedure TIniciais.setS1000(const Value: TS1000Collection);
begin
  FS1000.Assign(Value);
end;

procedure TIniciais.setS1005(const Value: TS1005Collection);
begin
  FS1005.Assign(Value);
end;

function TIniciais.LoadFromString(AXMLString: String): Boolean;
var
  Ok: Boolean;
begin
  case StrEventoToTipoEvento(Ok, AXMLString) of
    teS1000: Self.S1000.Add.evtInfoEmpregador.XML := AXMLString;
    teS1005: Self.S1005.Add.evtTabEstab.XML := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TIniciais.LoadFromIni(AIniString: String): Boolean;
var
  Ok: Boolean;
begin
  case StringToTipoEvento(Ok, AIniString) of
    teS1000: Self.S1000.Add.evtInfoEmpregador.LerArqIni(AIniString);
    teS1005: Self.S1005.Add.evtTabEstab.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.
