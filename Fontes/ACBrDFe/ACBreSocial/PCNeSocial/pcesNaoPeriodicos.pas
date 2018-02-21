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

unit pcesNaoPeriodicos;

interface

uses
  SysUtils, Classes, synautil,
  ACBrUtil, pcesConversaoeSocial,
  pcesS2190, pcesS2200, pcesS2220, pcesS2230, pcesS2240,
  pcesS2241, pcesS2205, pcesS2206, pcesS2210, pcesS2250,
  pcesS2260, pcesS2298, pcesS2299, pcesS2300, pcesS2306,
  pcesS2399, pcesS2400, pcesS3000;

type

  TNaoPeriodicos = class(TComponent)
  private
    FS2190: TS2190Collection;
    FS2200: TS2200Collection;
    FS2205: TS2205Collection;
    FS2206: TS2206Collection;
    FS2210: TS2210Collection;
    FS2220: TS2220Collection;
    FS2230: TS2230Collection;
    FS2240: TS2240Collection;
    FS2241: TS2241Collection;
    FS2250: TS2250Collection;
    FS2260: TS2260Collection;
    FS2298: TS2298Collection;
    FS2299: TS2299Collection;
    FS2300: TS2300Collection;
    FS2306: TS2306Collection;
    FS2399: TS2399Collection;
    FS2400: TS2400Collection;
    FS3000: TS3000Collection;

    function GetCount: integer;
    procedure setS2190(const Value: TS2190Collection);
    procedure setS2200(const Value: TS2200Collection);
    procedure setS2205(const Value: TS2205Collection);
    procedure setS2206(const Value: TS2206Collection);
    procedure setS2210(const Value: TS2210Collection);
    procedure setS2220(const Value: TS2220Collection);
    procedure setS2230(const Value: TS2230Collection);
    procedure setS2240(const Value: TS2240Collection);
    procedure setS2241(const Value: TS2241Collection);
    procedure setS2250(const Value: TS2250Collection);
    procedure setS2260(const Value: TS2260Collection);
    procedure setS2298(const Value: TS2298Collection);
    procedure setS2299(const Value: TS2299Collection);
    procedure setS2300(const Value: TS2300Collection);
    procedure setS2399(const Value: TS2399Collection);
    procedure setS2306(const Value: TS2306Collection);
    procedure setS2400(const Value: TS2400Collection);
    procedure setS3000(const Value: TS3000Collection);

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
    property S2190: TS2190Collection read FS2190 write setS2190;
    property S2200: TS2200Collection read FS2200 write setS2200;
    property S2205: TS2205Collection read FS2205 write setS2205;
    property S2206: TS2206Collection read FS2206 write setS2206;
    property S2210: TS2210Collection read FS2210 write setS2210;
    property S2220: TS2220Collection read FS2220 write setS2220;
    property S2230: TS2230Collection read FS2230 write setS2230;
    property S2240: TS2240Collection read FS2240 write setS2240;
    property S2241: TS2241Collection read FS2241 write setS2241;
    property S2250: TS2250Collection read FS2250 write setS2250;
    property S2260: TS2260Collection read FS2260 write setS2260;
    property S2298: TS2298Collection read FS2298 write setS2298;
    property S2299: TS2299Collection read FS2299 write setS2299;
    property S2300: TS2300Collection read FS2300 write setS2300;
    property S2306: TS2306Collection read FS2306 write setS2306;
    property S2399: TS2399Collection read FS2399 write setS2399;
    property S2400: TS2400Collection read FS2400 write setS2400;
    property S3000: TS3000Collection read FS3000 write setS3000;

  end;

implementation

uses
  ACBreSocial;

{ TNaoPeriodicos }

procedure TNaoPeriodicos.Clear;
begin
  FS2190.Clear;
  FS2200.Clear;
  FS2205.Clear;
  FS2206.Clear;
  FS2210.Clear;
  FS2220.Clear;
  FS2230.Clear;
  FS2240.Clear;
  FS2241.Clear;
  FS2250.Clear;
  FS2260.Clear;
  FS2298.Clear;
  FS2299.Clear;
  FS2300.Clear;
  FS2306.Clear;
  FS2399.Clear;
  FS2400.Clear;
  FS3000.Clear;
end;

constructor TNaoPeriodicos.Create(AOwner: TComponent);
begin
  inherited;

  FS2190 := TS2190Collection.Create(AOwner, TS2190CollectionItem);
  FS2200 := TS2200Collection.Create(AOwner, TS2200CollectionItem);
  FS2205 := TS2205Collection.Create(AOwner, TS2205CollectionItem);
  FS2206 := TS2206Collection.Create(AOwner, TS2206CollectionItem);
  FS2210 := TS2210Collection.Create(AOwner, TS2210CollectionItem);
  FS2220 := TS2220Collection.Create(AOwner, TS2220CollectionItem);
  FS2230 := TS2230Collection.Create(AOwner, TS2230CollectionItem);
  FS2240 := TS2240Collection.Create(AOwner, TS2240CollectionItem);
  FS2241 := TS2241Collection.Create(AOwner, TS2241CollectionItem);
  FS2250 := TS2250Collection.Create(AOwner, TS2250CollectionItem);
  FS2260 := TS2260Collection.Create(AOwner, TS2260CollectionItem);
  FS2298 := TS2298Collection.Create(AOwner, TS2298CollectionItem);
  FS2299 := TS2299Collection.Create(AOwner, TS2299CollectionItem);
  FS2300 := TS2300Collection.Create(AOwner, TS2300CollectionItem);
  FS2306 := TS2306Collection.Create(AOwner, TS2306CollectionItem);
  FS2399 := TS2399Collection.Create(AOwner, TS2399CollectionItem);
  FS2400 := TS2400Collection.Create(AOwner, TS2400CollectionItem);
  FS3000 := TS3000Collection.Create(AOwner, TS3000CollectionItem);
end;

destructor TNaoPeriodicos.Destroy;
begin
  FS2190.Free;
  FS2200.Free;
  FS2205.Free;
  FS2206.Free;
  FS2210.Free;
  FS2220.Free;
  FS2230.Free;
  FS2240.Free;
  FS2241.Free;
  FS2250.Free;
  FS2260.Free;
  FS2298.Free;
  FS2299.Free;
  FS2300.Free;
  FS2306.Free;
  FS2399.Free;
  FS2400.Free;
  FS3000.Free;

  inherited;
end;

function TNaoPeriodicos.GetCount: Integer;
begin
  Result := self.S2190.Count +
            self.S2200.Count +
            self.S2205.Count +
            self.S2206.Count +
            self.S2210.Count +
            self.S2220.Count +
            self.S2230.Count +
            self.S2240.Count +
            self.S2241.Count +
            self.S2250.Count +
            self.S2260.Count +
            self.S2298.Count +
            self.S2299.Count +
            self.S2300.Count +
            self.S2306.Count +
            self.S2399.Count +
            self.S2400.Count +
            self.S3000.Count;
end;

procedure TNaoPeriodicos.GerarXMLs;
var
  i: Integer;
begin
  for I := 0 to Self.S2190.Count - 1 do
    Self.S2190.Items[i].EvtAdmPrelim.GerarXML;

  for I := 0 to Self.S2200.Count - 1 do
    Self.S2200.Items[i].EvtAdmissao.GerarXML;

  for I := 0 to Self.S2205.Count - 1 do
    Self.S2205.Items[i].EvtAltCadastral.GerarXML;

  for I := 0 to Self.S2206.Count - 1 do
    Self.S2206.Items[i].EvtAltContratual.GerarXML;

  for I := 0 to Self.S2210.Count - 1 do
    Self.S2210.Items[i].EvtCAT.GerarXML;

  for I := 0 to Self.S2220.Count - 1 do
    Self.S2220.Items[i].EvtASO.GerarXML;

  for I := 0 to Self.S2230.Count - 1 do
    Self.S2230.Items[i].EvtAfastTemp.GerarXML;

  for I := 0 to Self.S2240.Count - 1 do
    Self.S2240.Items[i].EvtExpRisco.GerarXML;

  for I := 0 to Self.S2241.Count - 1 do
    Self.S2241.Items[i].EvtInsApo.GerarXML;

  for I := 0 to Self.S2250.Count - 1 do
    Self.S2250.Items[i].EvtAvPrevio.GerarXML;

  for I := 0 to Self.S2260.Count - 1 do
    Self.S2260.Items[i].EvtConvInterm.GerarXML;

  for I := 0 to Self.S2298.Count - 1 do
    Self.S2298.Items[i].EvtReintegr.GerarXML;

  for I := 0 to Self.S2299.Count - 1 do
    Self.S2299.Items[i].EvtDeslig.GerarXML;

  for I := 0 to Self.S2300.Count - 1 do
    Self.S2300.Items[i].EvtTSVInicio.GerarXML;

  for I := 0 to Self.S2306.Count - 1 do
    Self.S2306.Items[i].EvtTSVAltContr.GerarXML;

  for I := 0 to Self.S2399.Count - 1 do
      Self.S2399.Items[i].EvtTSVTermino.GerarXML;

  for I := 0 to Self.S2400.Count - 1 do
      Self.S2400.Items[i].EvtCdBenPrRP.GerarXML;

  for I := 0 to Self.S3000.Count - 1 do
    Self.S3000.Items[i].EvtExclusao.GerarXML;
end;

procedure TNaoPeriodicos.SaveToFiles;
var
  i: integer;
  Path: String;
begin
  with TACBreSocial(Self.Owner) do
    Path := PathWithDelim(Configuracoes.Arquivos.GetPatheSocial(Now, Configuracoes.Geral.IdEmpregador));

  for I := 0 to Self.S2190.Count - 1 do
    Self.S2190.Items[i].EvtAdmPrelim.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2190.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2200.Count - 1 do
    Self.S2200.Items[i].EvtAdmissao.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2200.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2205.Count - 1 do
    Self.S2205.Items[i].EvtAltCadastral.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2205.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2206.Count - 1 do
    Self.S2206.Items[i].EvtAltContratual.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2206.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2210.Count - 1 do
    Self.S2210.Items[i].EvtCAT.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2210.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2220.Count - 1 do
    Self.S2220.Items[i].EvtASO.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2220.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2230.Count - 1 do
    Self.S2230.Items[i].EvtAfastTemp.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2230.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2240.Count - 1 do
    Self.S2240.Items[i].EvtExpRisco.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2240.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2241.Count - 1 do
    Self.S2241.Items[i].EvtInsApo.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2241.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2250.Count - 1 do
    Self.S2250.Items[i].EvtAvPrevio.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2250.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2260.Count - 1 do
    Self.S2260.Items[i].EvtConvInterm.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2260.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2298.Count - 1 do
    Self.S2298.Items[i].EvtReintegr.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2298.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2299.Count - 1 do
    Self.S2299.Items[i].EvtDeslig.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2299.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2300.Count - 1 do
    Self.S2300.Items[i].EvtTSVInicio.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2300.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2306.Count - 1 do
      Self.S2306.Items[i].EvtTSVAltContr.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2306.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2399.Count - 1 do
    Self.S2399.Items[i].EvtTSVTermino.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2399.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S2400.Count - 1 do
    Self.S2400.Items[i].EvtCdBenPrRP.SaveToFile(Path+'\'+TipoEventoToStr(Self.S2400.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S3000.Count - 1 do
    Self.S3000.Items[i].EvtExclusao.SaveToFile(Path+'\'+TipoEventoToStr(Self.S3000.Items[i].TipoEvento)+'-'+IntToStr(i));
end;

procedure TNaoPeriodicos.setS2190(const Value: TS2190Collection);
begin
  FS2190.Assign(Value);
end;

procedure TNaoPeriodicos.setS2200(const Value: TS2200Collection);
begin
  FS2200.Assign(Value);
end;

procedure TNaoPeriodicos.setS2205(const Value: TS2205Collection);
begin
  FS2205.Assign(Value);
end;

procedure TNaoPeriodicos.setS2206(const Value: TS2206Collection);
begin
  FS2206.Assign(Value);
end;

procedure TNaoPeriodicos.setS2210(const Value: TS2210Collection);
begin
  FS2210.Assign(Value);
end;

procedure TNaoPeriodicos.setS2220(const Value: TS2220Collection);
begin
  FS2220.Assign(Value);
end;

procedure TNaoPeriodicos.setS2230(const Value: TS2230Collection);
begin
  FS2230.Assign(Value);
end;

procedure TNaoPeriodicos.setS2240(const Value: TS2240Collection);
begin
  FS2240.Assign(Value);
end;

procedure TNaoPeriodicos.setS2241(const Value: TS2241Collection);
begin
  FS2241.Assign(Value);
end;

procedure TNaoPeriodicos.setS2250(const Value: TS2250Collection);
begin
  FS2250.Assign(Value);
end;

procedure TNaoPeriodicos.setS2260(const Value: TS2260Collection);
begin
  FS2260.Assign(Value);
end;

procedure TNaoPeriodicos.setS2298(const Value: TS2298Collection);
begin
  FS2298.Assign(Value);
end;

procedure TNaoPeriodicos.setS2299(const Value: TS2299Collection);
begin
  FS2299.Assign(Value);
end;

procedure TNaoPeriodicos.setS2300(const Value: TS2300Collection);
begin
  FS2300.Assign(Value);
end;

procedure TNaoPeriodicos.setS2306(const Value: TS2306Collection);
begin
  FS2306.Assign(Value);
end;

procedure TNaoPeriodicos.setS2399(const Value: TS2399Collection);
begin
  FS2399.Assign(Value);
end;

procedure TNaoPeriodicos.setS2400(const Value: TS2400Collection);
begin
  FS2400.Assign(Value);
end;

procedure TNaoPeriodicos.setS3000(const Value: TS3000Collection);
begin
  FS3000.Assign(Value);
end;

function TNaoPeriodicos.LoadFromString(AXMLString: String): Boolean;
var
 Ok: Boolean;
begin
  case StrEventoToTipoEvento(Ok, AXMLString) of
    teS2190: Self.S2190.Add.EvtAdmPrelim.XML := AXMLString;
    teS2200: Self.S2200.Add.EvtAdmissao.XML := AXMLString;
    teS2205: Self.S2205.Add.EvtAltCadastral.XML := AXMLString;
    teS2206: Self.S2206.Add.EvtAltContratual.XML := AXMLString;
    teS2210: Self.S2210.Add.EvtCAT.XML := AXMLString;
    teS2220: Self.S2220.Add.EvtASO.XML := AXMLString;
    teS2230: Self.S2230.Add.EvtAfastTemp.XML := AXMLString;
    teS2240: Self.S2240.Add.EvtExpRisco.XML := AXMLString;
    teS2241: Self.S2241.Add.EvtInsApo.XML := AXMLString;
    teS2250: Self.S2250.Add.EvtAvPrevio.XML := AXMLString;
    teS2260: Self.S2260.Add.EvtConvInterm.XML := AXMLString;
    teS2298: Self.S2298.Add.EvtReintegr.XML := AXMLString;
    teS2299: Self.S2299.Add.EvtDeslig.XML := AXMLString;
    teS2300: Self.S2300.Add.EvtTSVInicio.XML := AXMLString;
    teS2306: Self.S2306.Add.EvtTSVAltContr.XML := AXMLString;
    teS2399: Self.S2399.Add.EvtTSVTermino.XML := AXMLString;
    teS2400: Self.S2400.Add.EvtCdBenPrRP.XML := AXMLString;
    teS3000: Self.S3000.Add.EvtExclusao.XML := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TNaoPeriodicos.LoadFromIni(AIniString: String): Boolean;
var
  Ok: Boolean;
begin
  case StrEventoToTipoEvento(Ok, AIniString) of
    teS2190: Self.S2190.Add.EvtAdmPrelim.LerArqIni(AIniString);
    teS2200: Self.S2200.Add.EvtAdmissao.LerArqIni(AIniString);
    teS2205: Self.S2205.Add.EvtAltCadastral.LerArqIni(AIniString);
    teS2206: Self.S2206.Add.EvtAltContratual.LerArqIni(AIniString);
    teS2210: Self.S2210.Add.EvtCAT.LerArqIni(AIniString);
    teS2220: Self.S2220.Add.EvtASO.LerArqIni(AIniString);
    teS2230: Self.S2230.Add.EvtAfastTemp.LerArqIni(AIniString);
    teS2240: Self.S2240.Add.EvtExpRisco.LerArqIni(AIniString);
    teS2241: Self.S2241.Add.EvtInsApo.LerArqIni(AIniString);
    teS2250: Self.S2250.Add.EvtAvPrevio.LerArqIni(AIniString);
    teS2260: Self.S2260.Add.EvtConvInterm.LerArqIni(AIniString);
    teS2298: Self.S2298.Add.EvtReintegr.LerArqIni(AIniString);
    teS2299: Self.S2299.Add.EvtDeslig.LerArqIni(AIniString);
    teS2300: Self.S2300.Add.EvtTSVInicio.LerArqIni(AIniString);
    teS2306: Self.S2306.Add.EvtTSVAltContr.LerArqIni(AIniString);
    teS2399: Self.S2399.Add.EvtTSVTermino.LerArqIni(AIniString);
    teS2400: Self.S2400.Add.EvtCdBenPrRP.LerArqIni(AIniString);
    teS3000: Self.S3000.Add.EvtExclusao.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.

