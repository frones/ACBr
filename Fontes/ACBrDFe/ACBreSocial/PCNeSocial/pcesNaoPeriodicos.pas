{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
{                              Leivio Fontenele                                }
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

unit pcesNaoPeriodicos;

interface

uses
  SysUtils, Classes, synautil,
  pcesConversaoeSocial,
  pcesS2190, pcesS2200, pcesS2220, pcesS2230, pcesS2231,
  pcesS2240, pcesS2221, pcesS2205, pcesS2206, pcesS2210,
  pcesS2250, pcesS2260, pcesS2298, pcesS2299, pcesS2300,
  pcesS2306, pcesS2399, pcesS2400, pcesS3000, pcesS2245,
  pcesS2405, pcesS2410, pcesS2416, pcesS2418, pcesS2420,
  pcesS2500, pcesS2501, pcesS3500;

type

  TNaoPeriodicos = class(TComponent)
  private
    FS2190: TS2190Collection;
    FS2200: TS2200Collection;
    FS2205: TS2205Collection;
    FS2206: TS2206Collection;
    FS2210: TS2210Collection;
    FS2220: TS2220Collection;
    FS2221: TS2221Collection;
    FS2230: TS2230Collection;
    FS2231: TS2231Collection;
    FS2240: TS2240Collection;
    FS2245: TS2245Collection;
    FS2250: TS2250Collection;
    FS2260: TS2260Collection;
    FS2298: TS2298Collection;
    FS2299: TS2299Collection;
    FS2300: TS2300Collection;
    FS2306: TS2306Collection;
    FS2399: TS2399Collection;
    FS2400: TS2400Collection;
    FS2405: TS2405Collection;
    FS2410: TS2410Collection;
    FS2416: TS2416Collection;
    FS2418: TS2418Collection;
    FS2420: TS2420Collection;
    FS2500: TS2500Collection;
    FS2501: TS2501Collection;
    FS3000: TS3000Collection;
    FS3500: TS3500Collection;

    function GetCount: integer;
    procedure setS2190(const Value: TS2190Collection);
    procedure setS2200(const Value: TS2200Collection);
    procedure setS2205(const Value: TS2205Collection);
    procedure setS2206(const Value: TS2206Collection);
    procedure setS2210(const Value: TS2210Collection);
    procedure setS2220(const Value: TS2220Collection);
    procedure setS2221(const Value: TS2221Collection);
    procedure setS2230(const Value: TS2230Collection);
    procedure setS2231(const Value: TS2231Collection);
    procedure setS2240(const Value: TS2240Collection);
    procedure setS2245(const Value: TS2245Collection);
    procedure setS2250(const Value: TS2250Collection);
    procedure setS2260(const Value: TS2260Collection);
    procedure setS2298(const Value: TS2298Collection);
    procedure setS2299(const Value: TS2299Collection);
    procedure setS2300(const Value: TS2300Collection);
    procedure setS2399(const Value: TS2399Collection);
    procedure setS2306(const Value: TS2306Collection);
    procedure setS2400(const Value: TS2400Collection);
    procedure setS2405(const Value: TS2405Collection);
    procedure setS2410(const Value: TS2410Collection);
    procedure setS2416(const Value: TS2416Collection);
    procedure setS2418(const Value: TS2418Collection);
    procedure setS2420(const Value: TS2420Collection);
    procedure setS2500(const Value: TS2500Collection);
    procedure setS2501(const Value: TS2501Collection);
    procedure setS3000(const Value: TS3000Collection);
    procedure setS3500(const Value: TS3500Collection);

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
    property S2190: TS2190Collection read FS2190 write setS2190;
    property S2200: TS2200Collection read FS2200 write setS2200;
    property S2205: TS2205Collection read FS2205 write setS2205;
    property S2206: TS2206Collection read FS2206 write setS2206;
    property S2210: TS2210Collection read FS2210 write setS2210;
    property S2220: TS2220Collection read FS2220 write setS2220;
    property S2221: TS2221Collection read FS2221 write setS2221;
    property S2230: TS2230Collection read FS2230 write setS2230;
    property S2231: TS2231Collection read FS2231 write setS2231;
    property S2240: TS2240Collection read FS2240 write setS2240;
    property S2245: TS2245Collection read FS2245 write setS2245;
    property S2250: TS2250Collection read FS2250 write setS2250;
    property S2260: TS2260Collection read FS2260 write setS2260;
    property S2298: TS2298Collection read FS2298 write setS2298;
    property S2299: TS2299Collection read FS2299 write setS2299;
    property S2300: TS2300Collection read FS2300 write setS2300;
    property S2306: TS2306Collection read FS2306 write setS2306;
    property S2399: TS2399Collection read FS2399 write setS2399;
    property S2400: TS2400Collection read FS2400 write setS2400;
    property S2405: TS2405Collection read FS2405 write setS2405;
    property S2410: TS2410Collection read FS2410 write setS2410;
    property S2416: TS2416Collection read FS2416 write setS2416;
    property S2418: TS2418Collection read FS2418 write setS2418;
    property S2420: TS2420Collection read FS2420 write setS2420;
    property S2500: TS2500Collection read FS2500 write setS2500;
    property S2501: TS2501Collection read FS2501 write setS2501;
    property S3000: TS3000Collection read FS3000 write setS3000;
    property S3500: TS3500Collection read FS3500 write setS3500;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
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
  FS2221.Clear;
  FS2230.Clear;
  FS2231.Clear;
  FS2240.Clear;
  FS2245.Clear;
  FS2250.Clear;
  FS2260.Clear;
  FS2298.Clear;
  FS2299.Clear;
  FS2300.Clear;
  FS2306.Clear;
  FS2399.Clear;
  FS2400.Clear;
  FS2405.Clear;
  FS2410.Clear;
  FS2416.Clear;
  FS2418.Clear;
  FS2420.Clear;
  FS2500.Clear;
  FS2501.Clear;
  FS3000.Clear;
  FS3500.Clear;
end;

constructor TNaoPeriodicos.Create(AOwner: TComponent);
begin
  inherited;

  FS2190 := TS2190Collection.Create(AOwner);
  FS2200 := TS2200Collection.Create(AOwner);
  FS2205 := TS2205Collection.Create(AOwner);
  FS2206 := TS2206Collection.Create(AOwner);
  FS2210 := TS2210Collection.Create(AOwner);
  FS2220 := TS2220Collection.Create(AOwner);
  FS2221 := TS2221Collection.Create(AOwner);
  FS2230 := TS2230Collection.Create(AOwner);
  FS2231 := TS2231Collection.Create(AOwner);
  FS2240 := TS2240Collection.Create(AOwner);
  FS2245 := TS2245Collection.Create(AOwner);
  FS2250 := TS2250Collection.Create(AOwner);
  FS2260 := TS2260Collection.Create(AOwner);
  FS2298 := TS2298Collection.Create(AOwner);
  FS2299 := TS2299Collection.Create(AOwner);
  FS2300 := TS2300Collection.Create(AOwner);
  FS2306 := TS2306Collection.Create(AOwner);
  FS2399 := TS2399Collection.Create(AOwner);
  FS2400 := TS2400Collection.Create(AOwner);
  FS2405 := TS2405Collection.Create(AOwner);
  FS2410 := TS2410Collection.Create(AOwner);
  FS2416 := TS2416Collection.Create(AOwner);
  FS2418 := TS2418Collection.Create(AOwner);
  FS2420 := TS2420Collection.Create(AOwner);
  FS2500 := TS2500Collection.Create(AOwner);
  FS2501 := TS2501Collection.Create(AOwner);
  FS3000 := TS3000Collection.Create(AOwner);
  FS3500 := TS3500Collection.Create(AOwner);
end;

destructor TNaoPeriodicos.Destroy;
begin
  FS2190.Free;
  FS2200.Free;
  FS2205.Free;
  FS2206.Free;
  FS2210.Free;
  FS2220.Free;
  FS2221.Free;
  FS2230.Free;
  FS2231.Free;
  FS2240.Free;
  FS2245.Free;
  FS2250.Free;
  FS2260.Free;
  FS2298.Free;
  FS2299.Free;
  FS2300.Free;
  FS2306.Free;
  FS2399.Free;
  FS2400.Free;
  FS2405.Free;
  FS2410.Free;
  FS2416.Free;
  FS2418.Free;
  FS2420.Free;
  FS2500.Free;
  FS2501.Free;
  FS3000.Free;
  FS3500.Free;

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
            self.S2221.Count +
            self.S2230.Count +
            self.S2231.Count +
            self.S2240.Count +
            self.S2245.Count +
            self.S2250.Count +
            self.S2260.Count +
            self.S2298.Count +
            self.S2299.Count +
            self.S2300.Count +
            self.S2306.Count +
            self.S2399.Count +
            self.S2400.Count +
            self.S2405.Count +
            self.S2410.Count +
            self.S2416.Count +
            self.S2418.Count +
            self.S2420.Count +
            self.S2500.Count +
            self.S2501.Count +
            self.S3000.Count +
            self.S3500.Count;
end;

procedure TNaoPeriodicos.Gerar;
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
    Self.S2220.Items[i].evtMonit.GerarXML;

  for I := 0 to Self.S2221.Count - 1 do
    Self.S2221.Items[i].evtToxic.GerarXML;

  for I := 0 to Self.S2230.Count - 1 do
    Self.S2230.Items[i].EvtAfastTemp.GerarXML;

  for I := 0 to Self.S2231.Count - 1 do
    Self.S2231.Items[i].EvtCessao.GerarXML;

  for I := 0 to Self.S2240.Count - 1 do
    Self.S2240.Items[i].EvtExpRisco.GerarXML;

  for I := 0 to Self.S2245.Count - 1 do
    Self.S2245.Items[i].EvtTreiCap.GerarXML;

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
    Self.S2400.Items[i].EvtCdBenefIn.GerarXML;

  for I := 0 to Self.S2405.Count - 1 do
    Self.S2405.Items[i].EvtCdBenefAlt.GerarXML;

  for I := 0 to Self.S2410.Count - 1 do
    Self.S2410.Items[i].EvtCdBenIn.GerarXML;

  for I := 0 to Self.S2416.Count - 1 do
    Self.S2416.Items[i].EvtCdBenAlt.GerarXML;

  for I := 0 to Self.S2418.Count - 1 do
    Self.S2418.Items[i].EvtReativBen.GerarXML;

  for I := 0 to Self.S2420.Count - 1 do
    Self.S2420.Items[i].EvtCdBenTerm.GerarXML;

  for I := 0 to Self.S2500.Count - 1 do
    Self.S2500.Items[i].EvtProcTrab.GerarXML;

  for I := 0 to Self.S2501.Count - 1 do
    Self.S2501.Items[i].EvtContProc.GerarXML;

  for I := 0 to Self.S3000.Count - 1 do
    Self.S3000.Items[i].EvtExclusao.GerarXML;

  for I := 0 to Self.S3500.Count - 1 do
    Self.S3500.Items[i].EvtExcProcTrab.GerarXML;
end;

procedure TNaoPeriodicos.Assinar;
var
  i: Integer;
begin
  for I := 0 to Self.S2190.Count - 1 do
    Self.S2190.Items[i].EvtAdmPrelim.XML :=
    Self.S2190.Items[i].EvtAdmPrelim.Assinar(Self.S2190.Items[i].EvtAdmPrelim.XML, 'evtAdmPrelim');

  for I := 0 to Self.S2200.Count - 1 do
    Self.S2200.Items[i].EvtAdmissao.XML :=
    Self.S2200.Items[i].EvtAdmissao.Assinar(Self.S2200.Items[i].EvtAdmissao.XML, 'evtAdmissao');

  for I := 0 to Self.S2205.Count - 1 do
    Self.S2205.Items[i].EvtAltCadastral.XML :=
    Self.S2205.Items[i].EvtAltCadastral.Assinar(Self.S2205.Items[i].EvtAltCadastral.XML, 'evtAltCadastral');

  for I := 0 to Self.S2206.Count - 1 do
    Self.S2206.Items[i].EvtAltContratual.XML :=
    Self.S2206.Items[i].EvtAltContratual.Assinar(Self.S2206.Items[i].EvtAltContratual.XML, 'evtAltContratual');

  for I := 0 to Self.S2210.Count - 1 do
    Self.S2210.Items[i].EvtCAT.XML :=
    Self.S2210.Items[i].EvtCAT.Assinar(Self.S2210.Items[i].EvtCAT.XML, 'evtCAT');

  for I := 0 to Self.S2220.Count - 1 do
    Self.S2220.Items[i].evtMonit.XML :=
    Self.S2220.Items[i].evtMonit.Assinar(Self.S2220.Items[i].evtMonit.XML, 'evtMonit');

  for I := 0 to Self.S2221.Count - 1 do
    Self.S2221.Items[i].evtToxic.XML :=
    Self.S2221.Items[i].evtToxic.Assinar(Self.S2221.Items[i].evtToxic.XML, 'evtToxic');

  for I := 0 to Self.S2230.Count - 1 do
    Self.S2230.Items[i].EvtAfastTemp.XML :=
    Self.S2230.Items[i].EvtAfastTemp.Assinar(Self.S2230.Items[i].EvtAfastTemp.XML, 'evtAfastTemp');

  for I := 0 to Self.S2231.Count - 1 do
    Self.S2231.Items[i].EvtCessao.XML :=
    Self.S2231.Items[i].EvtCessao.Assinar(Self.S2231.Items[i].EvtCessao.XML, 'evtCessao');

  for I := 0 to Self.S2240.Count - 1 do
    Self.S2240.Items[i].EvtExpRisco.XML :=
    Self.S2240.Items[i].EvtExpRisco.Assinar(Self.S2240.Items[i].EvtExpRisco.XML, 'evtExpRisco');

  for I := 0 to Self.S2245.Count - 1 do
    Self.S2245.Items[i].EvtTreiCap.XML :=
    Self.S2245.Items[i].EvtTreiCap.Assinar(Self.S2245.Items[i].EvtTreiCap.XML, 'evtTreiCap');

  for I := 0 to Self.S2250.Count - 1 do
    Self.S2250.Items[i].EvtAvPrevio.XML :=
    Self.S2250.Items[i].EvtAvPrevio.Assinar(Self.S2250.Items[i].EvtAvPrevio.XML, 'evtAvPrevio');

  for I := 0 to Self.S2260.Count - 1 do
    Self.S2260.Items[i].EvtConvInterm.XML :=
    Self.S2260.Items[i].EvtConvInterm.Assinar(Self.S2260.Items[i].EvtConvInterm.XML, 'evtConvInterm');

  for I := 0 to Self.S2298.Count - 1 do
    Self.S2298.Items[i].EvtReintegr.XML :=
    Self.S2298.Items[i].EvtReintegr.Assinar(Self.S2298.Items[i].EvtReintegr.XML, 'evtReintegr');

  for I := 0 to Self.S2299.Count - 1 do
    Self.S2299.Items[i].EvtDeslig.XML :=
    Self.S2299.Items[i].EvtDeslig.Assinar(Self.S2299.Items[i].EvtDeslig.XML, 'evtDeslig');

  for I := 0 to Self.S2300.Count - 1 do
    Self.S2300.Items[i].EvtTSVInicio.XML :=
    Self.S2300.Items[i].EvtTSVInicio.Assinar(Self.S2300.Items[i].EvtTSVInicio.XML, 'evtTSVInicio');

  for I := 0 to Self.S2306.Count - 1 do
    Self.S2306.Items[i].EvtTSVAltContr.XML :=
    Self.S2306.Items[i].EvtTSVAltContr.Assinar(Self.S2306.Items[i].EvtTSVAltContr.XML, 'evtTSVAltContr');

  for I := 0 to Self.S2399.Count - 1 do
    Self.S2399.Items[i].EvtTSVTermino.XML :=
    Self.S2399.Items[i].EvtTSVTermino.Assinar(Self.S2399.Items[i].EvtTSVTermino.XML, 'evtTSVTermino');

  for I := 0 to Self.S2400.Count - 1 do
    Self.S2400.Items[i].EvtCdBenefIn.XML :=
    Self.S2400.Items[i].EvtCdBenefIn.Assinar(Self.S2400.Items[i].EvtCdBenefIn.XML, 'evtCdBenefIn');

  for I := 0 to Self.S2405.Count - 1 do
    Self.S2405.Items[i].EvtCdBenefAlt.XML :=
    Self.S2405.Items[i].EvtCdBenefAlt.Assinar(Self.S2405.Items[i].EvtCdBenefAlt.XML, 'evtCdBenefAlt');

  for I := 0 to Self.S2410.Count - 1 do
    Self.S2410.Items[i].EvtCdBenIn.XML :=
    Self.S2410.Items[i].EvtCdBenIn.Assinar(Self.S2410.Items[i].EvtCdBenIn.XML, 'evtCdBenIn');

  for I := 0 to Self.S2416.Count - 1 do
    Self.S2416.Items[i].EvtCdBenAlt.XML :=
    Self.S2416.Items[i].EvtCdBenAlt.Assinar(Self.S2416.Items[i].EvtCdBenAlt.XML, 'evtCdBenAlt');

  for I := 0 to Self.S2418.Count - 1 do
    Self.S2418.Items[i].EvtReativBen.XML :=
    Self.S2418.Items[i].EvtReativBen.Assinar(Self.S2418.Items[i].EvtReativBen.XML, 'evtReativBen');

  for I := 0 to Self.S2420.Count - 1 do
    Self.S2420.Items[i].EvtCdBenTerm.XML :=
    Self.S2420.Items[i].EvtCdBenTerm.Assinar(Self.S2420.Items[i].EvtCdBenTerm.XML, 'evtCdBenTerm');

  for I := 0 to Self.S2500.Count - 1 do
    Self.S2500.Items[i].EvtProcTrab.XML :=
    Self.S2500.Items[i].EvtProcTrab.Assinar(Self.S2500.Items[i].EvtProcTrab.XML, 'evtProcTrab');

  for I := 0 to Self.S2501.Count - 1 do
    Self.S2501.Items[i].EvtContProc.XML :=
    Self.S2501.Items[i].EvtContProc.Assinar(Self.S2501.Items[i].EvtContProc.XML, 'evtContProc');

  for I := 0 to Self.S3000.Count - 1 do
    Self.S3000.Items[i].EvtExclusao.XML :=
    Self.S3000.Items[i].EvtExclusao.Assinar(Self.S3000.Items[i].EvtExclusao.XML, 'evtExclusao');

  for I := 0 to Self.S3500.Count - 1 do
    Self.S3500.Items[i].EvtExcProcTrab.XML :=
    Self.S3500.Items[i].EvtExcProcTrab.Assinar(Self.S3500.Items[i].EvtExcProcTrab.XML, 'evtExcProcTrab');
end;

procedure TNaoPeriodicos.Validar;
var
  i: Integer;
begin
  for I := 0 to Self.S2190.Count - 1 do
    Self.S2190.Items[i].EvtAdmPrelim.Validar(schevtAdmPrelim);

  for I := 0 to Self.S2200.Count - 1 do
    Self.S2200.Items[i].EvtAdmissao.Validar(schevtAdmissao);

  for I := 0 to Self.S2205.Count - 1 do
    Self.S2205.Items[i].EvtAltCadastral.Validar(schevtAltCadastral);

  for I := 0 to Self.S2206.Count - 1 do
    Self.S2206.Items[i].EvtAltContratual.Validar(schevtAltContratual);

  for I := 0 to Self.S2210.Count - 1 do
    Self.S2210.Items[i].EvtCAT.Validar(schevtCAT);

  for I := 0 to Self.S2220.Count - 1 do
    Self.S2220.Items[i].evtMonit.Validar(schevtMonit);

  for I := 0 to Self.S2221.Count - 1 do
    Self.S2221.Items[i].evtToxic.Validar(schEvtToxic);

  for I := 0 to Self.S2230.Count - 1 do
    Self.S2230.Items[i].EvtAfastTemp.Validar(schevtAfastTemp);

  for I := 0 to Self.S2231.Count - 1 do
    Self.S2231.Items[i].EvtCessao.Validar(schevtCessao);

  for I := 0 to Self.S2240.Count - 1 do
    Self.S2240.Items[i].EvtExpRisco.Validar(schevtExpRisco);

  for I := 0 to Self.S2245.Count - 1 do
    Self.S2245.Items[i].EvtTreiCap.Validar(schEvtTreiCap);

  for I := 0 to Self.S2250.Count - 1 do
    Self.S2250.Items[i].EvtAvPrevio.Validar(schevtAvPrevio);

  for I := 0 to Self.S2260.Count - 1 do
    Self.S2260.Items[i].EvtConvInterm.Validar(schevtConvInterm);

  for I := 0 to Self.S2298.Count - 1 do
    Self.S2298.Items[i].EvtReintegr.Validar(schevtReintegr);

  for I := 0 to Self.S2299.Count - 1 do
    Self.S2299.Items[i].EvtDeslig.Validar(schevtDeslig);

  for I := 0 to Self.S2300.Count - 1 do
    Self.S2300.Items[i].EvtTSVInicio.Validar(schevtTSVInicio);

  for I := 0 to Self.S2306.Count - 1 do
    Self.S2306.Items[i].EvtTSVAltContr.Validar(schevtTSVAltContr);

  for I := 0 to Self.S2399.Count - 1 do
    Self.S2399.Items[i].EvtTSVTermino.Validar(schevtTSVTermino);

  for I := 0 to Self.S2400.Count - 1 do
    Self.S2400.Items[i].EvtCdBenefIn.Validar(schEvtCdBenefIn);

  for I := 0 to Self.S2405.Count - 1 do
    Self.S2405.Items[i].EvtCdBenefAlt.Validar(schEvtCdBenefAlt);

  for I := 0 to Self.S2410.Count - 1 do
    Self.S2410.Items[i].EvtCdBenIn.Validar(schEvtCdBenIn);

  for I := 0 to Self.S2416.Count - 1 do
    Self.S2416.Items[i].EvtCdBenAlt.Validar(schevtCdBenAlt);

  for I := 0 to Self.S2418.Count - 1 do
    Self.S2418.Items[i].EvtReativBen.Validar(schevtReativBen);

  for I := 0 to Self.S2420.Count - 1 do
    Self.S2420.Items[i].EvtCdBenTerm.Validar(schevtCdBenTerm);

  for I := 0 to Self.S2500.Count - 1 do
    Self.S2500.Items[i].EvtProcTrab.Validar(schevtProcTrab);

  for I := 0 to Self.S2501.Count - 1 do
    Self.S2501.Items[i].EvtContProc.Validar(schevtContProc);

  for I := 0 to Self.S3000.Count - 1 do
    Self.S3000.Items[i].EvtExclusao.Validar(schevtExclusao);

  for I := 0 to Self.S3500.Count - 1 do
    Self.S3500.Items[i].EvtExcProcTrab.Validar(schevtExcProcTrab);
end;

procedure TNaoPeriodicos.SaveToFiles;
var
  i: integer;
  Path, PathName: String;
begin
  with TACBreSocial(Self.Owner) do
    Path := PathWithDelim(Configuracoes.Arquivos.GetPatheSocial(Now, Configuracoes.Geral.IdEmpregador));

  for I := 0 to Self.S2190.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2190.Items[i].EvtAdmPrelim.Id) + '-' +
     TipoEventoToStr(Self.S2190.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2190.Items[i].EvtAdmPrelim.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2190;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2190.Items[i].EvtAdmPrelim.Id);
      XML := Self.S2190.Items[i].EvtAdmPrelim.XML;
    end;
  end;

  for I := 0 to Self.S2200.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2200.Items[i].EvtAdmissao.Id) + '-' +
     TipoEventoToStr(Self.S2200.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2200.Items[i].EvtAdmissao.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2200;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2200.Items[i].EvtAdmissao.Id);
      XML := Self.S2200.Items[i].EvtAdmissao.XML;
    end;
  end;

  for I := 0 to Self.S2205.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2205.Items[i].EvtAltCadastral.Id) + '-' +
     TipoEventoToStr(Self.S2205.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2205.Items[i].EvtAltCadastral.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2205;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2205.Items[i].EvtAltCadastral.Id);
      XML := Self.S2205.Items[i].EvtAltCadastral.XML;
    end;
  end;

  for I := 0 to Self.S2206.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2206.Items[i].EvtAltContratual.Id) + '-' +
     TipoEventoToStr(Self.S2206.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2206.Items[i].EvtAltContratual.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2206;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2206.Items[i].EvtAltContratual.Id);
      XML := Self.S2206.Items[i].EvtAltContratual.XML;
    end;
  end;

  for I := 0 to Self.S2210.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2210.Items[i].EvtCAT.Id) + '-' +
     TipoEventoToStr(Self.S2210.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2210.Items[i].EvtCAT.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2210;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2210.Items[i].EvtCAT.Id);
      XML := Self.S2210.Items[i].EvtCAT.XML;
    end;
  end;

  for I := 0 to Self.S2220.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2220.Items[i].evtMonit.Id) + '-' +
     TipoEventoToStr(Self.S2220.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2220.Items[i].evtMonit.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2220;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2220.Items[i].evtMonit.Id);
      XML := Self.S2220.Items[i].evtMonit.XML;
    end;
  end;

  for I := 0 to Self.S2221.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2221.Items[i].evtToxic.Id) + '-' +
     TipoEventoToStr(Self.S2221.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2221.Items[i].evtToxic.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2221;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2221.Items[i].evtToxic.Id);
      XML := Self.S2221.Items[i].evtToxic.XML;
    end;
  end;

  for I := 0 to Self.S2230.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2230.Items[i].EvtAfastTemp.Id) + '-' +
     TipoEventoToStr(Self.S2230.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2230.Items[i].EvtAfastTemp.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2230;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2230.Items[i].EvtAfastTemp.Id);
      XML := Self.S2230.Items[i].EvtAfastTemp.XML;
    end;
  end;

  for I := 0 to Self.S2231.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2231.Items[i].EvtCessao.Id) + '-' +
     TipoEventoToStr(Self.S2231.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2231.Items[i].EvtCessao.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2231;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2231.Items[i].EvtCessao.Id);
      XML := Self.S2231.Items[i].EvtCessao.XML;
    end;
  end;

  for I := 0 to Self.S2240.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2240.Items[i].EvtExpRisco.Id) + '-' +
     TipoEventoToStr(Self.S2240.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2240.Items[i].EvtExpRisco.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2240;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2240.Items[i].EvtExpRisco.Id);
      XML := Self.S2240.Items[i].EvtExpRisco.XML;
    end;
  end;

  for I := 0 to Self.S2245.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2245.Items[i].EvtTreiCap.Id) + '-' +
     TipoEventoToStr(Self.S2245.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2245.Items[i].EvtTreiCap.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2245;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2245.Items[i].EvtTreiCap.Id);
      XML := Self.S2245.Items[i].EvtTreiCap.XML;
    end;
  end;

  for I := 0 to Self.S2250.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2250.Items[i].EvtAvPrevio.Id) + '-' +
     TipoEventoToStr(Self.S2250.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2250.Items[i].EvtAvPrevio.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2250;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2250.Items[i].EvtAvPrevio.Id);
      XML := Self.S2250.Items[i].EvtAvPrevio.XML;
    end;
  end;

  for I := 0 to Self.S2260.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2260.Items[i].EvtConvInterm.Id) + '-' +
     TipoEventoToStr(Self.S2260.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2260.Items[i].EvtConvInterm.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2260;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2260.Items[i].EvtConvInterm.Id);
      XML := Self.S2260.Items[i].EvtConvInterm.XML;
    end;
  end;

  for I := 0 to Self.S2298.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2298.Items[i].EvtReintegr.Id) + '-' +
     TipoEventoToStr(Self.S2298.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2298.Items[i].EvtReintegr.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2298;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2298.Items[i].EvtReintegr.Id);
      XML := Self.S2298.Items[i].EvtReintegr.XML;
    end;
  end;

  for I := 0 to Self.S2299.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2299.Items[i].EvtDeslig.Id) + '-' +
     TipoEventoToStr(Self.S2299.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2299.Items[i].EvtDeslig.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2299;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2299.Items[i].EvtDeslig.Id);
      XML := Self.S2299.Items[i].EvtDeslig.XML;
    end;
  end;

  for I := 0 to Self.S2300.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2300.Items[i].EvtTSVInicio.Id) + '-' +
     TipoEventoToStr(Self.S2300.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2300.Items[i].EvtTSVInicio.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2300;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2300.Items[i].EvtTSVInicio.Id);
      XML := Self.S2300.Items[i].EvtTSVInicio.XML;
    end;
  end;

  for I := 0 to Self.S2306.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2306.Items[i].EvtTSVAltContr.Id) + '-' +
     TipoEventoToStr(Self.S2306.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2306.Items[i].EvtTSVAltContr.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2306;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2306.Items[i].EvtTSVAltContr.Id);
      XML := Self.S2306.Items[i].EvtTSVAltContr.XML;
    end;
  end;

  for I := 0 to Self.S2399.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2399.Items[i].EvtTSVTermino.Id) + '-' +
     TipoEventoToStr(Self.S2399.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2399.Items[i].EvtTSVTermino.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2399;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2399.Items[i].EvtTSVTermino.Id);
      XML := Self.S2399.Items[i].EvtTSVTermino.XML;
    end;
  end;

  for I := 0 to Self.S2400.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2400.Items[i].EvtCdBenefIn.Id) + '-' +
     TipoEventoToStr(Self.S2400.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2400.Items[i].EvtCdBenefIn.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2400;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2400.Items[i].EvtCdBenefIn.Id);
      XML := Self.S2400.Items[i].EvtCdBenefIn.XML;
    end;
  end;

  for I := 0 to Self.S2405.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2405.Items[i].EvtCdBenefAlt.Id) + '-' +
     TipoEventoToStr(Self.S2405.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2405.Items[i].EvtCdBenefAlt.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2405;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2405.Items[i].EvtCdBenefAlt.Id);
      XML := Self.S2405.Items[i].EvtCdBenefAlt.XML;
    end;
  end;

  for I := 0 to Self.S2410.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2410.Items[i].EvtCdBenIn.Id) + '-' +
     TipoEventoToStr(Self.S2410.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2410.Items[i].EvtCdBenIn.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2410;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2410.Items[i].EvtCdBenIn.Id);
      XML := Self.S2410.Items[i].EvtCdBenIn.XML;
    end;
  end;

  for I := 0 to Self.S2416.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2416.Items[i].EvtCdBenAlt.Id) + '-' +
     TipoEventoToStr(Self.S2416.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2416.Items[i].EvtCdBenAlt.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2416;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2416.Items[i].EvtCdBenAlt.Id);
      XML := Self.S2416.Items[i].EvtCdBenAlt.XML;
    end;
  end;

  for I := 0 to Self.S2418.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2418.Items[i].EvtReativBen.Id) + '-' +
     TipoEventoToStr(Self.S2418.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2418.Items[i].EvtReativBen.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2418;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2418.Items[i].EvtReativBen.Id);
      XML := Self.S2418.Items[i].EvtReativBen.XML;
    end;
  end;

  for I := 0 to Self.S2420.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2420.Items[i].EvtCdBenTerm.Id) + '-' +
     TipoEventoToStr(Self.S2420.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2420.Items[i].EvtCdBenTerm.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2420;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2420.Items[i].EvtCdBenTerm.Id);
      XML := Self.S2420.Items[i].EvtCdBenTerm.XML;
    end;
  end;

  for I := 0 to Self.S2500.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2500.Items[i].EvtProcTrab.Id) + '-' +
     TipoEventoToStr(Self.S2500.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2500.Items[i].EvtProcTrab.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2500;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2500.Items[i].EvtProcTrab.Id);
      XML := Self.S2500.Items[i].EvtProcTrab.XML;
    end;
  end;

  for I := 0 to Self.S2501.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S2501.Items[i].EvtContProc.Id) + '-' +
     TipoEventoToStr(Self.S2501.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S2501.Items[i].EvtContProc.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS2501;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S2501.Items[i].EvtContProc.Id);
      XML := Self.S2501.Items[i].EvtContProc.XML;
    end;
  end;

  for I := 0 to Self.S3000.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S3000.Items[i].EvtExclusao.Id) + '-' +
     TipoEventoToStr(Self.S3000.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S3000.Items[i].EvtExclusao.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS3000;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S3000.Items[i].EvtExclusao.Id);
      XML := Self.S3000.Items[i].EvtExclusao.XML;
    end;
  end;

  for I := 0 to Self.S3500.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.S3500.Items[i].EvtExcProcTrab.Id) + '-' +
     TipoEventoToStr(Self.S3500.Items[i].TipoEvento) + '-' + IntToStr(i);

    Self.S3500.Items[i].EvtExcProcTrab.SaveToFile(PathName);

    with TACBreSocial(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teS3500;
      PathNome := PathName;
      idEvento := OnlyNumber(Self.S3500.Items[i].EvtExcProcTrab.Id);
      XML := Self.S3500.Items[i].EvtExcProcTrab.XML;
    end;
  end;
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

procedure TNaoPeriodicos.setS2221(const Value: TS2221Collection);
begin
  FS2221.Assign(Value);
end;

procedure TNaoPeriodicos.setS2230(const Value: TS2230Collection);
begin
  FS2230.Assign(Value);
end;

procedure TNaoPeriodicos.setS2231(const Value: TS2231Collection);
begin
  FS2231.Assign(Value);
end;

procedure TNaoPeriodicos.setS2240(const Value: TS2240Collection);
begin
  FS2240.Assign(Value);
end;

procedure TNaoPeriodicos.setS2245(const Value: TS2245Collection);
begin
  FS2245.Assign(Value);
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

procedure TNaoPeriodicos.setS2405(const Value: TS2405Collection);
begin
  FS2405.Assign(Value);
end;

procedure TNaoPeriodicos.setS2410(const Value: TS2410Collection);
begin
  FS2410.Assign(Value);
end;

procedure TNaoPeriodicos.setS2416(const Value: TS2416Collection);
begin
  FS2416.Assign(Value);
end;

procedure TNaoPeriodicos.setS2418(const Value: TS2418Collection);
begin
  FS2418.Assign(Value);
end;

procedure TNaoPeriodicos.setS2420(const Value: TS2420Collection);
begin
  FS2420.Assign(Value);
end;

procedure TNaoPeriodicos.setS2500(const Value: TS2500Collection);
begin
  FS2500.Assign(Value);
end;

procedure TNaoPeriodicos.setS2501(const Value: TS2501Collection);
begin
  FS2501.Assign(Value);
end;

procedure TNaoPeriodicos.setS3000(const Value: TS3000Collection);
begin
  FS3000.Assign(Value);
end;

procedure TNaoPeriodicos.setS3500(const Value: TS3500Collection);
begin
  FS3500.Assign(Value);
end;

function TNaoPeriodicos.LoadFromString(const AXMLString: String): Boolean;
var
 Ok: Boolean;
 typVersaoDF : TVersaoeSocial;
begin
  typVersaoDF := TACBreSocial(Self.Owner).Configuracoes.Geral.VersaoDF;

  case StringXMLToTipoEvento(Ok, AXMLString, typVersaoDF) of
    teS2190: Self.S2190.New.EvtAdmPrelim.XML := AXMLString;
    teS2200:
    begin
      with Self.S2200.New do
      begin
        EvtAdmissao.XML := AXMLString;
        EvtAdmissao.LerXML;
      end;
    end;
    teS2205: Self.S2205.New.EvtAltCadastral.XML := AXMLString;
    teS2206: Self.S2206.New.EvtAltContratual.XML := AXMLString;
    teS2210:
    begin
      with Self.S2210.New do
      begin
        EvtCAT.XML := AXMLString;
        EvtCAT.LerXML;
      end;
    end;
    teS2220:
    begin
      with Self.S2220.New do
      begin
        EvtMonit.XML := AXMLString;
        EvtMonit.LerXML;
      end;
    end;
    teS2221: Self.S2221.New.EvtToxic.XML := AXMLString;
    teS2230: Self.S2230.New.EvtAfastTemp.XML := AXMLString;
    teS2231: Self.S2231.New.EvtCessao.XML := AXMLString;
    teS2240:
    begin
      with Self.S2240.New do
      begin
        EvtExpRisco.XML := AXMLString;
        EvtExpRisco.LerXML;
      end;
    end;
    teS2245: Self.S2245.New.EvtTreiCap.XML := AXMLString;
    teS2250: Self.S2250.New.EvtAvPrevio.XML := AXMLString;
    teS2260: Self.S2260.New.EvtConvInterm.XML := AXMLString;
    teS2298: Self.S2298.New.EvtReintegr.XML := AXMLString;
    teS2299: Self.S2299.New.EvtDeslig.XML := AXMLString;
    teS2300: Self.S2300.New.EvtTSVInicio.XML := AXMLString;
    teS2306: Self.S2306.New.EvtTSVAltContr.XML := AXMLString;
    teS2399: Self.S2399.New.EvtTSVTermino.XML := AXMLString;
    teS2400: Self.S2400.New.EvtCdBenefIn.XML := AXMLString;
    teS2405: Self.S2405.New.EvtCdBenefAlt.XML := AXMLString;
    teS2410: Self.S2410.New.EvtCdBenIn.XML := AXMLString;
    teS2416: Self.S2416.New.EvtCdBenAlt.XML := AXMLString;
    teS2418: Self.S2418.New.EvtReativBen.XML := AXMLString;
    teS2420: Self.S2420.New.EvtCdBenTerm.XML := AXMLString;
    teS2500: Self.S2500.New.EvtProcTrab.XML := AXMLString;
    teS2501: Self.S2501.New.EvtContProc.XML := AXMLString;
    teS3000: Self.S3000.New.EvtExclusao.XML := AXMLString;
    teS3500: Self.S3500.New.EvtExcProcTrab.XML := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TNaoPeriodicos.LoadFromIni(const AIniString: String): Boolean;
var
  Ok: Boolean;
  typVersaoDF : TVersaoeSocial;
begin
  typVersaoDF := TACBreSocial(Self.Owner).Configuracoes.Geral.VersaoDF;

  case StringINIToTipoEvento(Ok, AIniString, typVersaoDF) of
    teS2190: Self.S2190.New.EvtAdmPrelim.LerArqIni(AIniString);
    teS2200: Self.S2200.New.EvtAdmissao.LerArqIni(AIniString);
    teS2205: Self.S2205.New.EvtAltCadastral.LerArqIni(AIniString);
    teS2206: Self.S2206.New.EvtAltContratual.LerArqIni(AIniString);
    teS2210: Self.S2210.New.EvtCAT.LerArqIni(AIniString);
    teS2220: Self.S2220.New.evtMonit.LerArqIni(AIniString);
    teS2221: Self.S2221.New.evtToxic.LerArqIni(AIniString);
    teS2230: Self.S2230.New.EvtAfastTemp.LerArqIni(AIniString);
    teS2231: Self.S2231.New.EvtCessao.LerArqIni(AIniString);
    teS2240: Self.S2240.New.EvtExpRisco.LerArqIni(AIniString);
    teS2245: Self.S2245.New.EvtTreiCap.LerArqIni(AIniString);
    teS2250: Self.S2250.New.EvtAvPrevio.LerArqIni(AIniString);
    teS2260: Self.S2260.New.EvtConvInterm.LerArqIni(AIniString);
    teS2298: Self.S2298.New.EvtReintegr.LerArqIni(AIniString);
    teS2299: Self.S2299.New.EvtDeslig.LerArqIni(AIniString);
    teS2300: Self.S2300.New.EvtTSVInicio.LerArqIni(AIniString);
    teS2306: Self.S2306.New.EvtTSVAltContr.LerArqIni(AIniString);
    teS2399: Self.S2399.New.EvtTSVTermino.LerArqIni(AIniString);
    teS2400: Self.S2400.New.EvtCdBenefIn.LerArqIni(AIniString);
    teS2405: Self.S2405.New.EvtCdBenefAlt.LerArqIni(AIniString);
    teS2410: Self.S2410.New.EvtCdBenIn.LerArqIni(AIniString);
    teS2416: Self.S2416.New.EvtCdBenAlt.LerArqIni(AIniString);
    teS2418: Self.S2418.New.EvtReativBen.LerArqIni(AIniString);
    teS2420: Self.S2420.New.EvtCdBenTerm.LerArqIni(AIniString);
    teS2500: Self.S2500.New.EvtProcTrab.LerArqIni(AIniString);
    teS2501: Self.S2501.New.EvtContProc.LerArqIni(AIniString);
    teS3000: Self.S3000.New.EvtExclusao.LerArqIni(AIniString);
    teS3500: Self.S3500.New.EvtExcProcTrab.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.
