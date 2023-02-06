{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

{$I ACBr.inc}

unit pcnEventosReinf;

interface

uses
  SysUtils, Classes, synautil,
  ACBrUtil.Strings, ACBrUtil.FilesIO,
  pcnConversaoReinf,
  pcnReinfR1000, pcnReinfR1050, pcnReinfR1070, pcnReinfR2010, pcnReinfR2020,
  pcnReinfR2030,
  pcnReinfR2040, pcnReinfR2050, pcnReinfR2055, pcnReinfR2060, pcnReinfR2070,
  pcnReinfR2098, pcnReinfR2099, pcnReinfR3010, pcnReinfR4010, pcnReinfR4020,
  pcnReinfR4040, pcnReinfR4080, pcnReinfR4099, pcnReinfR9000;

type

  TReinfEventos = class(TComponent)
  private
    FR1000: TR1000Collection;
    FR1050: TR1050Collection;
    FR1070: TR1070Collection;
    FR2010: TR2010Collection;
    FR2020: TR2020Collection;
    FR2030: TR2030Collection;
    FR2040: TR2040Collection;
    FR2050: TR2050Collection;
    FR2055: TR2055Collection;
    FR2060: TR2060Collection;
    FR2070: TR2070Collection;
    FR2098: TR2098Collection;
    FR2099: TR2099Collection;
    FR3010: TR3010Collection;
    FR4010: TR4010Collection;
    FR4020: TR4020Collection;
    FR4040: TR4040Collection;
    FR4080: TR4080Collection;
    FR4099: TR4099Collection;
    FR9000: TR9000Collection;

    function GetCount: integer;
    procedure setR1000(const Value: TR1000Collection);
    procedure setR1050(const Value: TR1050Collection);
    procedure setR1070(const Value: TR1070Collection);
    procedure setR2010(const Value: TR2010Collection);
    procedure setR2020(const Value: TR2020Collection);
    procedure setR2030(const Value: TR2030Collection);
    procedure setR2040(const Value: TR2040Collection);
    procedure setR2050(const Value: TR2050Collection);
    procedure setR2055(const Value: TR2055Collection);
    procedure setR2060(const Value: TR2060Collection);
    procedure setR2070(const Value: TR2070Collection);
    procedure setR2098(const Value: TR2098Collection);
    procedure setR2099(const Value: TR2099Collection);
    procedure setR3010(const Value: TR3010Collection);
    procedure setR4010(const Value: TR4010Collection);
    procedure setR4020(const Value: TR4020Collection);
    procedure setR4040(const Value: TR4040Collection);
    procedure setR4080(const Value: TR4080Collection);
    procedure setR4099(const Value: TR4099Collection);
    procedure setR9000(const Value: TR9000Collection);
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
    property R1000: TR1000Collection read FR1000 write setR1000;
    property R1050: TR1050Collection read FR1050 write setR1050;
    property R1070: TR1070Collection read FR1070 write setR1070;
    property R2010: TR2010Collection read FR2010 write setR2010;
    property R2020: TR2020Collection read FR2020 write setR2020;
    property R2030: TR2030Collection read FR2030 write setR2030;
    property R2040: TR2040Collection read FR2040 write setR2040;
    property R2050: TR2050Collection read FR2050 write setR2050;
    property R2055: TR2055Collection read FR2055 write setR2055;
    property R2060: TR2060Collection read FR2060 write setR2060;
    property R2070: TR2070Collection read FR2070 write setR2070;
    property R2098: TR2098Collection read FR2098 write setR2098;
    property R2099: TR2099Collection read FR2099 write setR2099;
    property R3010: TR3010Collection read FR3010 write setR3010;
    property R4010: TR4010Collection read FR4010 write setR4010;
    property R4020: TR4020Collection read FR4020 write setR4020;
    property R4040: TR4040Collection read FR4040 write setR4040;
    property R4080: TR4080Collection read FR4080 write setR4080;
    property R4099: TR4099Collection read FR4099 write setR4099;
    property R9000: TR9000Collection read FR9000 write setR9000;
  end;

implementation

uses
  ACBrReinf;

{ TReinfEventos }

procedure TReinfEventos.Clear;
begin
  FR1000.Clear;
  FR1050.Clear;
  FR1070.Clear;
  FR2010.Clear;
  FR2020.Clear;
  FR2030.Clear;
  FR2040.Clear;
  FR2050.Clear;
  FR2055.Clear;
  FR2060.Clear;
  FR2070.Clear;
  FR2098.Clear;
  FR2099.Clear;
  FR3010.Clear;
  FR4010.Clear;
  FR4020.Clear;
  FR4040.Clear;
  FR4080.Clear;
  FR4099.Clear;
  FR9000.Clear;
end;

constructor TReinfEventos.Create(AOwner: TComponent);
begin
  inherited;

  FR1000 := TR1000Collection.Create(AOwner);
  FR1050 := TR1050Collection.Create(AOwner);
  FR1070 := TR1070Collection.Create(AOwner);
  FR2010 := TR2010Collection.Create(AOwner);
  FR2020 := TR2020Collection.Create(AOwner);
  FR2030 := TR2030Collection.Create(AOwner);
  FR2040 := TR2040Collection.Create(AOwner);
  FR2050 := TR2050Collection.Create(AOwner);
  FR2055 := TR2055Collection.Create(AOwner);
  FR2060 := TR2060Collection.Create(AOwner);
  FR2070 := TR2070Collection.Create(AOwner);
  FR2098 := TR2098Collection.Create(AOwner);
  FR2099 := TR2099Collection.Create(AOwner);
  FR3010 := TR3010Collection.Create(AOwner);
  FR4010 := TR4010Collection.Create(AOwner);
  FR4020 := TR4020Collection.Create(AOwner);
  FR4040 := TR4040Collection.Create(AOwner);
  FR4080 := TR4080Collection.Create(AOwner);
  FR4099 := TR4099Collection.Create(AOwner);
  FR9000 := TR9000Collection.Create(AOwner);
end;

destructor TReinfEventos.Destroy;
begin
  FR1000.Free;
  FR1050.Free;
  FR1070.Free;
  FR2010.Free;
  FR2020.Free;
  FR2030.Free;
  FR2040.Free;
  FR2050.Free;
  FR2055.Free;
  FR2060.Free;
  FR2070.Free;
  FR2098.Free;
  FR2099.Free;
  FR3010.Free;
  FR4010.Free;
  FR4020.Free;
  FR4040.Free;
  FR4080.Free;
  FR4099.Free;
  FR9000.Free;

  inherited;
end;

function TReinfEventos.GetCount: Integer;
begin
  Result := self.R1000.Count + self.R1050.Count + self.R1070.Count +
            Self.R2010.Count +
            Self.R2020.Count + Self.R2030.Count + Self.R2040.Count +
            Self.R2050.Count + Self.R2055.Count + Self.R2060.Count +
            Self.R2070.Count + Self.R2098.Count + Self.R2099.Count +
            Self.R3010.Count + Self.R4010.Count + Self.R4020.Count +
            Self.R4040.Count + Self.R4080.Count + Self.R4099.Count +
            Self.R9000.Count;
end;

procedure TReinfEventos.Gerar;
var
  i: Integer;
begin
  for i := 0 to Self.R1000.Count - 1 do
    Self.R1000.Items[i].evtInfoContri.GerarXML;

  for i := 0 to Self.R1050.Count - 1 do
    Self.R1050.Items[i].evtTabLig.GerarXML;

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

  for i := 0 to Self.R2055.Count - 1 do
    Self.R2055.Items[i].evtAqProd.GerarXML;

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

  for i := 0 to Self.R4010.Count - 1 do
    Self.R4010.Items[i].evtRetPF.GerarXML;

  for i := 0 to Self.R4020.Count - 1 do
    Self.R4020.Items[i].evtRetPJ.GerarXML;

  for i := 0 to Self.R4040.Count - 1 do
    Self.R4040.Items[i].evtBenefNId.GerarXML;

  for i := 0 to Self.R4080.Count - 1 do
    Self.R4080.Items[i].evtRetRec.GerarXML;

  for i := 0 to Self.R4099.Count - 1 do
    Self.R4099.Items[i].evtFech.GerarXML;

  for i := 0 to Self.R9000.Count - 1 do
    Self.R9000.Items[i].evtExclusao.GerarXML;
end;

procedure TReinfEventos.Assinar;
var
  i: Integer;
begin
  for i := 0 to Self.R1000.Count - 1 do
    Self.R1000.Items[i].evtInfoContri.XML :=
    Self.R1000.Items[i].evtInfoContri.Assinar(Self.R1000.Items[i].evtInfoContri.XML, 'evtInfoContri');

  for i := 0 to Self.R1050.Count - 1 do
    Self.R1050.Items[i].evtTabLig.XML :=
    Self.R1050.Items[i].evtTabLig.Assinar(Self.R1050.Items[i].evtTabLig.XML, 'evtTabLig');

  for i := 0 to Self.R1070.Count - 1 do
    Self.R1070.Items[i].evtTabProcesso.XML :=
    Self.R1070.Items[i].evtTabProcesso.Assinar(Self.R1070.Items[i].evtTabProcesso.XML, 'evtTabProcesso');

  for i := 0 to Self.R2010.Count - 1 do
    Self.R2010.Items[i].evtServTom.XML :=
    Self.R2010.Items[i].evtServTom.Assinar(Self.R2010.Items[i].evtServTom.XML, 'evtServTom');

  for i := 0 to Self.R2020.Count - 1 do
    Self.R2020.Items[i].evtServPrest.XML :=
    Self.R2020.Items[i].evtServPrest.Assinar(Self.R2020.Items[i].evtServPrest.XML, 'evtServPrest');

  for i := 0 to Self.R2030.Count - 1 do
    Self.R2030.Items[i].evtAssocDespRec.XML :=
    Self.R2030.Items[i].evtAssocDespRec.Assinar(Self.R2030.Items[i].evtAssocDespRec.XML, 'evtAssocDespRec');

  for i := 0 to Self.R2040.Count - 1 do
    Self.R2040.Items[i].evtAssocDespRep.XML :=
    Self.R2040.Items[i].evtAssocDespRep.Assinar(Self.R2040.Items[i].evtAssocDespRep.XML, 'evtAssocDespRep');

  for i := 0 to Self.R2050.Count - 1 do
    Self.R2050.Items[i].evtComProd.XML :=
    Self.R2050.Items[i].evtComProd.Assinar(Self.R2050.Items[i].evtComProd.XML, 'evtComProd');

  for i := 0 to Self.R2055.Count - 1 do
    Self.R2055.Items[i].evtAqProd.XML :=
    Self.R2055.Items[i].evtAqProd.Assinar(Self.R2055.Items[i].evtAqProd.XML, 'evtAqProd');

  for i := 0 to Self.R2060.Count - 1 do
    Self.R2060.Items[i].evtCPRB.XML :=
    Self.R2060.Items[i].evtCPRB.Assinar(Self.R2060.Items[i].evtCPRB.XML, 'evtCPRB');

  for i := 0 to Self.R2070.Count - 1 do
    Self.R2070.Items[i].evtPgtosDivs.XML :=
    Self.R2070.Items[i].evtPgtosDivs.Assinar(Self.R2070.Items[i].evtPgtosDivs.XML, 'evtPgtosDivs');

  for i := 0 to Self.R2098.Count - 1 do
    Self.R2098.Items[i].evtReabreEvPer.XML :=
    Self.R2098.Items[i].evtReabreEvPer.Assinar(Self.R2098.Items[i].evtReabreEvPer.XML, 'evtReabreEvPer');

  for i := 0 to Self.R2099.Count - 1 do
    Self.R2099.Items[i].evtFechaEvPer.XML :=
    Self.R2099.Items[i].evtFechaEvPer.Assinar(Self.R2099.Items[i].evtFechaEvPer.XML, 'evtFechaEvPer');

  for i := 0 to Self.R3010.Count - 1 do
    Self.R3010.Items[i].evtEspDesportivo.XML :=
    Self.R3010.Items[i].evtEspDesportivo.Assinar(Self.R3010.Items[i].evtEspDesportivo.XML, 'evtEspDesportivo');

  for i := 0 to Self.R4010.Count - 1 do
    Self.R4010.Items[i].evtRetPF.XML :=
    Self.R4010.Items[i].evtRetPF.Assinar(Self.R4010.Items[i].evtRetPF.XML, 'evtRetPF');

  for i := 0 to Self.R4020.Count - 1 do
    Self.R4020.Items[i].evtRetPJ.XML :=
    Self.R4020.Items[i].evtRetPJ.Assinar(Self.R4020.Items[i].evtRetPJ.XML, 'evtRetPJ');

  for i := 0 to Self.R4040.Count - 1 do
    Self.R4040.Items[i].evtBenefNId.XML :=
    Self.R4040.Items[i].evtBenefNId.Assinar(Self.R4040.Items[i].evtBenefNId.XML, 'evtBenefNId');

  for i := 0 to Self.R4080.Count - 1 do
    Self.R4080.Items[i].evtRetRec.XML :=
    Self.R4080.Items[i].evtRetRec.Assinar(Self.R4080.Items[i].evtRetRec.XML, 'evtRetRec');

  for i := 0 to Self.R4099.Count - 1 do
    Self.R4099.Items[i].evtFech.XML :=
    Self.R4099.Items[i].evtFech.Assinar(Self.R4099.Items[i].evtFech.XML, 'evtFech');

  for i := 0 to Self.R9000.Count - 1 do
    Self.R9000.Items[i].evtExclusao.XML :=
    Self.R9000.Items[i].evtExclusao.Assinar(Self.R9000.Items[i].evtExclusao.XML, 'evtExclusao');
end;

procedure TReinfEventos.Validar;
var
  i: Integer;
begin
  for i := 0 to Self.R1000.Count - 1 do
    Self.R1000.Items[i].evtInfoContri.Validar(schevtInfoContribuinte);

  for i := 0 to Self.R1050.Count - 1 do
    Self.R1050.Items[i].evtTabLig.Validar(schevt1050TabLig);

  for i := 0 to Self.R1070.Count - 1 do
    Self.R1070.Items[i].evtTabProcesso.Validar(schevtTabProcesso);

  for i := 0 to Self.R2010.Count - 1 do
    Self.R2010.Items[i].evtServTom.Validar(schevtTomadorServicos);

  for i := 0 to Self.R2020.Count - 1 do
    Self.R2020.Items[i].evtServPrest.Validar(schevtPrestadorServicos);

  for i := 0 to Self.R2030.Count - 1 do
    Self.R2030.Items[i].evtAssocDespRec.Validar(schevtRecursoRecebidoAssociacao);

  for i := 0 to Self.R2040.Count - 1 do
    Self.R2040.Items[i].evtAssocDespRep.Validar(schevtRecursoRepassadoAssociacao);

  for i := 0 to Self.R2050.Count - 1 do
    Self.R2050.Items[i].evtComProd.Validar(schevtInfoProdRural);

  for i := 0 to Self.R2055.Count - 1 do
    Self.R2055.Items[i].evtAqProd.Validar(schevtAquisicaoProdRural);

  for i := 0 to Self.R2060.Count - 1 do
    Self.R2060.Items[i].evtCPRB.Validar(schevtInfoCPRB);

  // Não encontra-se disponivel o schema para validação desse evento
  // somente da versão 1.01.01
//  for i := 0 to Self.R2070.Count - 1 do
//    Self.R2070.Items[i].evtPgtosDivs.Validar(schevtPgtosDivs);

  for i := 0 to Self.R2098.Count - 1 do
    Self.R2098.Items[i].evtReabreEvPer.Validar(schevtReabreEvPer);

  for i := 0 to Self.R2099.Count - 1 do
    Self.R2099.Items[i].evtFechaEvPer.Validar(schevtFechamento);

  for i := 0 to Self.R3010.Count - 1 do
    Self.R3010.Items[i].evtEspDesportivo.Validar(schevtEspDesportivo);

  for i := 0 to Self.R4010.Count - 1 do
    Self.R4010.Items[i].evtRetPF.Validar(schevt4010PagtoBeneficiarioPF);

  for i := 0 to Self.R4020.Count - 1 do
    Self.R4020.Items[i].evtRetPJ.Validar(schevt4020PagtoBeneficiarioPJ);

  for i := 0 to Self.R4040.Count - 1 do
    Self.R4040.Items[i].evtBenefNId.Validar(schevt4040PagtoBenefNaoIdentificado);

  for i := 0 to Self.R4080.Count - 1 do
    Self.R4080.Items[i].evtRetRec.Validar(schevt4080RetencaoRecebimento);

  for i := 0 to Self.R4099.Count - 1 do
    Self.R4099.Items[i].evtFech.Validar(schevt4099FechamentoDirf);

  for i := 0 to Self.R9000.Count - 1 do
    Self.R9000.Items[i].evtExclusao.Validar(schevtExclusao);
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

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR1000;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R1000.Items[i].evtInfoContri.Id);
      XML := Self.R1000.Items[i].evtInfoContri.XML;
    end;
  end;

  for i := 0 to Self.R1050.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R1050.Items[i].evtTabLig.Id) + '-' +
     TipoEventoToStr(Self.R1050.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R1050.Items[i].evtTabLig.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR1050;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R1050.Items[i].evtTabLig.Id);
      XML := Self.R1050.Items[i].evtTabLig.XML;
    end;
  end;

  for i := 0 to Self.R1070.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R1070.Items[i].evtTabProcesso.Id) + '-' +
     TipoEventoToStr(Self.R1070.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R1070.Items[i].evtTabProcesso.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR1070;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R1070.Items[i].evtTabProcesso.Id);
      XML := Self.R1070.Items[i].evtTabProcesso.XML;
    end;
  end;

  for i := 0 to Self.R2010.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2010.Items[i].evtServTom.Id) + '-' +
     TipoEventoToStr(Self.R2010.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2010.Items[i].evtServTom.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2010;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2010.Items[i].evtServTom.Id);
      XML := Self.R2010.Items[i].evtServTom.XML;
    end;
  end;

  for i := 0 to Self.R2020.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2020.Items[i].evtServPrest.Id) + '-' +
     TipoEventoToStr(Self.R2020.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2020.Items[i].evtServPrest.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2020;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2020.Items[i].evtServPrest.Id);
      XML := Self.R2020.Items[i].evtServPrest.XML;
    end;
  end;

  for i := 0 to Self.R2030.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2030.Items[i].evtAssocDespRec.Id) + '-' +
     TipoEventoToStr(Self.R2030.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2030.Items[i].evtAssocDespRec.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2030;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2030.Items[i].evtAssocDespRec.Id);
      XML := Self.R2030.Items[i].evtAssocDespRec.XML;
    end;
  end;

  for i := 0 to Self.R2040.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2040.Items[i].evtAssocDespRep.Id) + '-' +
     TipoEventoToStr(Self.R2040.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2040.Items[i].evtAssocDespRep.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2040;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2040.Items[i].evtAssocDespRep.Id);
      XML := Self.R2040.Items[i].evtAssocDespRep.XML;
    end;
  end;

  for i := 0 to Self.R2050.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2050.Items[i].evtComProd.Id) + '-' +
     TipoEventoToStr(Self.R2050.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2050.Items[i].evtComProd.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2050;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2050.Items[i].evtComProd.Id);
      XML := Self.R2050.Items[i].evtComProd.XML;
    end;
  end;

  for i := 0 to Self.R2055.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2055.Items[i].evtAqProd.Id) + '-' +
     TipoEventoToStr(Self.R2055.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2055.Items[i].evtAqProd.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2055;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2055.Items[i].evtAqProd.Id);
      XML := Self.R2055.Items[i].evtAqProd.XML;
    end;
  end;


  for i := 0 to Self.R2060.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2060.Items[i].evtCPRB.Id) + '-' +
     TipoEventoToStr(Self.R2060.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2060.Items[i].evtCPRB.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2060;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2060.Items[i].evtCPRB.Id);
      XML := Self.R2060.Items[i].evtCPRB.XML;
    end;
  end;

  for i := 0 to Self.R2070.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2070.Items[i].evtPgtosDivs.Id) + '-' +
     TipoEventoToStr(Self.R2070.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2070.Items[i].evtPgtosDivs.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2070;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2070.Items[i].evtPgtosDivs.Id);
      XML := Self.R2070.Items[i].evtPgtosDivs.XML;
    end;
  end;

  for i := 0 to Self.R2098.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2098.Items[i].evtReabreEvPer.Id) + '-' +
     TipoEventoToStr(Self.R2098.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2098.Items[i].evtReabreEvPer.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2098;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2098.Items[i].evtReabreEvPer.Id);
      XML := Self.R2098.Items[i].evtReabreEvPer.XML;
    end;
  end;

  for i := 0 to Self.R2099.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R2099.Items[i].evtFechaEvPer.Id) + '-' +
     TipoEventoToStr(Self.R2099.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R2099.Items[i].evtFechaEvPer.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR2099;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R2099.Items[i].evtFechaEvPer.Id);
      XML := Self.R2099.Items[i].evtFechaEvPer.XML;
    end;
  end;

  for i := 0 to Self.R3010.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R3010.Items[i].evtEspDesportivo.Id) + '-' +
     TipoEventoToStr(Self.R3010.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R3010.Items[i].evtEspDesportivo.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR3010;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R3010.Items[i].evtEspDesportivo.Id);
      XML := Self.R3010.Items[i].evtEspDesportivo.XML;
    end;
  end;

  for i := 0 to Self.R4010.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R4010.Items[i].evtRetPF.Id) + '-' +
     TipoEventoToStr(Self.R4010.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R4010.Items[i].evtRetPF.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR4010;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R4010.Items[i].evtRetPF.Id);
      XML := Self.R4010.Items[i].evtRetPF.XML;
    end;
  end;

  for i := 0 to Self.R4020.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R4020.Items[i].evtRetPJ.Id) + '-' +
     TipoEventoToStr(Self.R4020.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R4020.Items[i].evtRetPJ.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR4020;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R4020.Items[i].evtRetPJ.Id);
      XML := Self.R4020.Items[i].evtRetPJ.XML;
    end;
  end;

  for i := 0 to Self.R4040.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R4040.Items[i].evtBenefNId.Id) + '-' +
     TipoEventoToStr(Self.R4040.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R4040.Items[i].evtBenefNId.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR4040;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R4040.Items[i].evtBenefNId.Id);
      XML := Self.R4040.Items[i].evtBenefNId.XML;
    end;
  end;

  for i := 0 to Self.R4080.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R4080.Items[i].evtRetRec.Id) + '-' +
     TipoEventoToStr(Self.R4080.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R4080.Items[i].evtRetRec.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR4080;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R4080.Items[i].evtRetRec.Id);
      XML := Self.R4080.Items[i].evtRetRec.XML;
    end;
  end;

  for i := 0 to Self.R4099.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R4099.Items[i].evtFech.Id) + '-' +
     TipoEventoToStr(Self.R4099.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R4099.Items[i].evtFech.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR4099;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R4099.Items[i].evtFech.Id);
      XML := Self.R4099.Items[i].evtFech.XML;
    end;
  end;

  for i := 0 to Self.R9000.Count - 1 do
  begin
    PathName := Path + OnlyNumber(Self.R9000.Items[i].evtExclusao.Id) + '-' +
     TipoEventoToStr(Self.R9000.Items[i].TipoEvento)+'-'+IntToStr(i);

    Self.R9000.Items[i].evtExclusao.SaveToFile(PathName);

    with TACBrReinf(Self.Owner).Eventos.Gerados.New do
    begin
      TipoEvento := teR9000;
      PathNome := PathName;
      IdEvento := OnlyNumber(Self.R9000.Items[i].evtExclusao.Id);
      XML := Self.R9000.Items[i].evtExclusao.XML;
    end;
  end;
end;

procedure TReinfEventos.setR1000(const Value: TR1000Collection);
begin
  FR1000.Assign(Value);
end;

procedure TReinfEventos.setR1050(const Value: TR1050Collection);
begin
  FR1050.Assign(Value);
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

procedure TReinfEventos.setR2055(const Value: TR2055Collection);
begin
  FR2055.Assign(Value);
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

procedure TReinfEventos.setR4010(const Value: TR4010Collection);
begin
  FR4010.Assign(Value);
end;

procedure TReinfEventos.setR4020(const Value: TR4020Collection);
begin
  FR4020.Assign(Value);
end;

procedure TReinfEventos.setR4040(const Value: TR4040Collection);
begin
  FR4040.Assign(Value);
end;

procedure TReinfEventos.setR4080(const Value: TR4080Collection);
begin
  FR4080.Assign(Value);
end;

procedure TReinfEventos.setR4099(const Value: TR4099Collection);
begin
  FR4099.Assign(Value);
end;

procedure TReinfEventos.setR9000(const Value: TR9000Collection);
begin
  FR9000.Assign(Value);
end;

function TReinfEventos.LoadFromString(const AXMLString: String): Boolean;
var
  Ok: Boolean;
begin
  case StringXMLToTipoEvento(Ok, AXMLString) of
    teR1000: Self.R1000.New.evtInfoContri.XML    := AXMLString;
    teR1050: Self.R1050.New.evtTabLig.XML        := AXMLString;
    teR1070: Self.R1070.New.evtTabProcesso.XML   := AXMLString;
    teR2010: Self.R2010.New.evtServTom.XML       := AXMLString;
    teR2020: Self.R2020.New.evtServPrest.XML     := AXMLString;
    teR2030: Self.R2030.New.evtAssocDespRec.XML  := AXMLString;
    teR2040: Self.R2040.New.evtAssocDespRep.XML  := AXMLString;
    teR2050: Self.R2050.New.evtComProd.XML       := AXMLString;
    teR2055: Self.R2055.New.evtAqProd.XML        := AXMLString;
    teR2060: Self.R2060.New.evtCPRB.XML          := AXMLString;
    teR2070: Self.R2070.New.evtPgtosDivs.XML     := AXMLString;
    teR2098: Self.R2098.New.evtReabreEvPer.XML   := AXMLString;
    teR2099: Self.R2099.New.evtFechaEvPer.XML    := AXMLString;
    teR3010: Self.R3010.New.evtEspDesportivo.XML := AXMLString;
    teR4010: Self.R4010.New.evtRetPF.XML         := AXMLString;
    teR4020: Self.R4020.New.evtRetPJ.XML         := AXMLString;
    teR4040: Self.R4040.New.evtBenefNId.XML      := AXMLString;
    teR4080: Self.R4080.New.evtRetRec.XML        := AXMLString;
    teR4099: Self.R4099.New.evtFech.XML          := AXMLString;
    teR9000: Self.R9000.New.evtExclusao.XML      := AXMLString;
  end;

  Result := (GetCount > 0);
end;

function TReinfEventos.LoadFromIni(const AIniString: String): Boolean;
var
  Ok: Boolean;
begin
  case StringINIToTipoEvento(Ok, AIniString) of
    teR1000: Self.R1000.New.evtInfoContri.LerArqIni(AIniString);
    teR1050: Self.R1050.New.evtTabLig.LerArqIni(AIniString);
    teR1070: Self.R1070.New.evtTabProcesso.LerArqIni(AIniString);
    teR2010: Self.R2010.New.evtServTom.LerArqIni(AIniString);
    teR2020: Self.R2020.New.evtServPrest.LerArqIni(AIniString);
    teR2030: Self.R2030.New.evtAssocDespRec.LerArqIni(AIniString);
    teR2040: Self.R2040.New.evtAssocDespRep.LerArqIni(AIniString);
    teR2050: Self.R2050.New.evtComProd.LerArqIni(AIniString);
    teR2055: Self.R2055.New.evtAqProd.LerArqIni(AIniString);
    teR2060: Self.R2060.New.evtCPRB.LerArqIni(AIniString);
    teR2070: Self.R2070.New.evtPgtosDivs.LerArqIni(AIniString);
    teR2098: Self.R2098.New.evtReabreEvPer.LerArqIni(AIniString);
    teR2099: Self.R2099.New.evtFechaEvPer.LerArqIni(AIniString);
    teR3010: Self.R3010.New.evtEspDesportivo.LerArqIni(AIniString);
    teR4010: Self.R4010.New.evtRetPF.LerArqIni(AIniString);
    teR4020: Self.R4020.New.evtRetPJ.LerArqIni(AIniString);
    teR4040: Self.R4040.New.evtBenefNId.LerArqIni(AIniString);
    teR4080: Self.R4080.New.evtRetRec.LerArqIni(AIniString);
    teR4099: Self.R4099.New.evtFech.LerArqIni(AIniString);
    teR9000: Self.R9000.New.evtExclusao.LerArqIni(AIniString);
  end;

  Result := (GetCount > 0);
end;

end.
