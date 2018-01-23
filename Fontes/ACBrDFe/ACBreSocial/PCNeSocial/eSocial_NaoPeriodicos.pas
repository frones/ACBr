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

unit eSocial_NaoPeriodicos;

interface

uses
  SysUtils, Classes,
  ACBrUtil, eSocial_Conversao,
  eSocial_S2190, eSocial_S2200, eSocial_S2220, eSocial_S2230, eSocial_S2240,
  eSocial_S2241, eSocial_S2205, eSocial_S2206, eSocial_S2210, eSocial_S2250,
  eSocial_S2298, eSocial_S2299, eSocial_S2300, eSocial_S2306, eSocial_S2399,
  eSocial_S2400, eSocial_S3000, eSocial_S4000, eSocial_S4999;

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
    FS2298: TS2298Collection;
    FS2299: TS2299Collection;
    FS2300: TS2300Collection;
    FS2306: TS2306Collection;
    FS2399: TS2399Collection;
    FS2400: TS2400Collection;
    FS3000: TS3000Collection;
    FS4000: TS4000Collection;
    FS4999: TS4999Collection;

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
    procedure setS2298(const Value: TS2298Collection);
    procedure setS2299(const Value: TS2299Collection);
    procedure setS2300(const Value: TS2300Collection);
    procedure setS2399(const Value: TS2399Collection);
    procedure setS2306(const Value: TS2306Collection);
    procedure setS2400(const Value: TS2400Collection);
    procedure setS3000(const Value: TS3000Collection);
    procedure setS4000(const Value: TS4000Collection);
    procedure setS4999(const Value: TS4999Collection);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure GerarXMLs;
    procedure SaveToFiles;
    procedure Clear;

  published
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
    property S2298: TS2298Collection read FS2298 write setS2298;
    property S2299: TS2299Collection read FS2299 write setS2299;
    property S2300: TS2300Collection read FS2300 write setS2300;
    property S2306: TS2306Collection read FS2306 write setS2306;
    property S2399: TS2399Collection read FS2399 write setS2399;
    property S2400: TS2400Collection read FS2400 write setS2400;
    property S3000: TS3000Collection read FS3000 write setS3000;
    property S4000: TS4000Collection read FS4000 write setS4000;
    property S4999: TS4999Collection read FS4999 write setS4999;
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
  FS2298.Clear;
  FS2299.Clear;
  FS2300.Clear;
  FS2306.Clear;
  FS2399.Clear;
  FS2400.Clear;
  FS3000.Clear;
  FS4000.Clear;
  FS4999.Clear;
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
  FS2298 := TS2298Collection.Create(AOwner, TS2298CollectionItem);
  FS2299 := TS2299Collection.Create(AOwner, TS2299CollectionItem);
  FS2300 := TS2300Collection.Create(AOwner, TS2300CollectionItem);
  FS2306 := TS2306Collection.Create(AOwner, TS2306CollectionItem);
  FS2399 := TS2399Collection.Create(AOwner, TS2399CollectionItem);
  FS2400 := TS2400Collection.Create(AOwner, TS2400CollectionItem);
  FS3000 := TS3000Collection.Create(AOwner, TS3000CollectionItem);
  FS4000 := TS4000Collection.Create(AOwner, TS4000CollectionItem);
  FS4999 := TS4999Collection.Create(AOwner, TS4999CollectionItem);
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
  FS2298.Free;
  FS2299.Free;
  FS2300.Free;
  FS2306.Free;
  FS2399.Free;
  FS2400.Free;
  FS3000.Free;
  FS4000.Free;
  FS4999.Free;

  inherited;
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

  for I := 0 to Self.S4000.Count - 1 do
    Self.S4000.Items[i].EvtSolicTotal.GerarXML;

  for I := 0 to Self.S4999.Count - 1 do
    Self.S4999.Items[i].EvtAdesao.GerarXML;
end;

procedure TNaoPeriodicos.SaveToFiles;
var
  i: integer;
  Path: String;
begin
  Path := TACBreSocial(Self.Owner).Configuracoes.Arquivos.PathSalvar;

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

  for I := 0 to Self.S4000.Count - 1 do
    Self.S4000.Items[i].EvtSolicTotal.SaveToFile(Path+'\'+TipoEventoToStr(Self.S4000.Items[i].TipoEvento)+'-'+IntToStr(i));

  for I := 0 to Self.S4999.Count - 1 do
    Self.S4999.Items[i].EvtAdesao.SaveToFile(Path+'\'+TipoEventoToStr(Self.S4999.Items[i].TipoEvento)+'-'+IntToStr(i));
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

procedure TNaoPeriodicos.setS4000(const Value: TS4000Collection);
begin
  FS4000.Assign(Value);
end;

procedure TNaoPeriodicos.setS4999(const Value: TS4999Collection);
begin
  FS4999.Assign(Value);
end;

end.

