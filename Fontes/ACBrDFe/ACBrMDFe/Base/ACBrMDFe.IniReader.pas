{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrMDFe.IniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrMDFe.Classes,
//  ACBrMDFe.Conversao;
  pmdfeConversaoMDFe;

type
  { TMDFeIniReader }

  TMDFeIniReader = class
  private
    FMDFe: TMDFe;
    FVersaoDF: TVersaoMDFe;
    FAmbiente: Integer;
    FtpEmis: Integer;

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_Carregamento(AINIRec: TMemIniFile; infMunCarrega: TinfMunCarregaCollection);
    procedure Ler_Percurso(AINIRec: TMemIniFile; infPercurso: TinfPercursoCollection);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: Temit);

    procedure Ler_ModalRodoviario(AINIRec: TMemIniFile; rodo: Trodo);
    procedure Ler_InfANTT(AINIRec: TMemIniFile; infANTT: TinfANTT);
    procedure Ler_InfCIOT(AINIRec: TMemIniFile; infCIOT: TinfCIOTCollection);
    procedure Ler_ValePedagio(AINIRec: TMemIniFile; valePed: TvalePed);
    procedure Ler_Dispositivo(AINIRec: TMemIniFile; disp: TdispCollection);
    procedure Ler_InfContratante(AINIRec: TMemIniFile; infContratante: TinfContratanteCollection);
    procedure Ler_InfPagamento(AINIRec: TMemIniFile; infPag: TinfPagCollection);
    procedure Ler_Componentes(AINIRec: TMemIniFile; Comp: TCompCollection; Idx: Integer);
    procedure Ler_InfPrazos(AINIRec: TMemIniFile; infPrazo: TInfPrazoCollection; Idx: Integer);
    procedure Ler_InfBanco(AINIRec: TMemIniFile; infBanc: TinfBanc; Idx: Integer);
    procedure Ler_VeiculoTracao(AINIRec: TMemIniFile; veicTracao: TveicTracao);
    procedure Ler_Motoristas(AINIRec: TMemIniFile; condutor: TcondutorCollection);
    procedure Ler_VeiculoReboque(AINIRec: TMemIniFile; veicReboque: TveicReboqueCollection);
    procedure Ler_LacresRodo(AINIRec: TMemIniFile; lacRodo: TlacRodoCollection);

    procedure Ler_ModalAereo(AINIRec: TMemIniFile; aereo: Taereo);

    procedure Ler_ModalAquaviario(AINIRec: TMemIniFile; aquav: Taquav);
    procedure Ler_InfTerminalCarregamento(AINIRec: TMemIniFile; infTermCarreg: TinfTermCarregCollection);
    procedure Ler_InfTerminalDescarregamento(AINIRec: TMemIniFile; infTermDescarreg: TinfTermDescarregCollection);
    procedure Ler_InfEmbComboio(AINIRec: TMemIniFile; infEmbComb: TinfEmbCombCollection);
    procedure Ler_InfUnidCargaVazia(AINIRec: TMemIniFile; infUnidCargaVazia: TinfUnidCargaVaziaCollection);
    procedure Ler_InfUnidTranspVazia(AINIRec: TMemIniFile; infUnidTranspVazia: TinfUnidTranspVaziaCollection);

    procedure Ler_ModalFerroviario(AINIRec: TMemIniFile; ferrov: Tferrov);
    procedure Ler_Vagoes(AINIRec: TMemIniFile; vag: TvagCollection);

    procedure Ler_Descarregamento(AINIRec: TMemIniFile; infMunDescarga: TinfMunDescargaCollection);
    procedure Ler_InfCTe(AINIRec: TMemIniFile; infCTe: TinfCTeCollection; Idx: Integer);
    procedure Ler_Peri(AINIRec: TMemIniFile; peri: TPeriCTeCollection; Idx1, Idx2: Integer);
    procedure Ler_InfNFePrestParcial(AINIRec: TMemIniFile; infNFePrestParcial: TinfNFePrestParcialCollection; Idx1, Idx2: Integer);
    procedure Ler_InfEntregaParcial(AINIRec: TMemIniFile; infEntregaParcial: TinfEntregaParcial; Idx1, Idx2: Integer);
    procedure Ler_InfUnidTransp(AINIRec: TMemIniFile; infUnidTransp: TinfUnidTranspCTeCollection; Idx1, Idx2: Integer);
    procedure Ler_LacUnidTransp(AINIRec: TMemIniFile; lacUnidTransp: TlacresCollection; Idx1, Idx2, Idx3: Integer);
    procedure Ler_InfUnidCarga(AINIRec: TMemIniFile; infUnidCarga: TinfUnidCargaCollection; Idx1, Idx2, Idx3: Integer);
    procedure Ler_LacUnidCarga(AINIRec: TMemIniFile; lacUnidCarga: TlacUnidCargaCollection; Idx1, Idx2, Idx3, Idx4: Integer);

    procedure Ler_InfNFe(AINIRec: TMemIniFile; infNFe: TinfNFeCollection; Idx: Integer);
    procedure Ler_InfMDFeTransp(AINIRec: TMemIniFile; infMDFeTransp: TinfMDFeTranspCollection; Idx: Integer);

    procedure Ler_Seguro(AINIRec: TMemIniFile; seg: TSegCollection);
    procedure Ler_Averbacao(AINIRec: TMemIniFile; aver: TAverCollection; Idx: Integer);

    procedure Ler_ProdutoPredominante(AINIRec: TMemIniFile; prodPred: TprodPred);
    procedure Ler_Totais(AINIRec: TMemIniFile; tot: Ttot);
    procedure Ler_Lacres(AINIRec: TMemIniFile; lacres: TlacresCollection);
    procedure Ler_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_InfAdicionais(AINIRec: TMemIniFile; infAdic: TinfAdic);
    procedure Ler_InfRespTecnico(AINIRec: TMemIniFile; infRespTec: TinfRespTec);

  public
    constructor Create(AOwner: TMDFe); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property MDFe: TMDFe read FMDFe write FMDFe;
    property VersaoDF: TVersaoMDFe read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrMDFe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcnConversao;

{ TMDFeIniReader }

constructor TMDFeIniReader.Create(AOwner: TMDFe);
begin
  inherited Create;

  FMDFe := AOwner;
end;

function TMDFeIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FMDFe.infMDFe.versao := StringToFloatDef(INIRec.ReadString('infMDFe', 'versao', VersaoMDFeToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FMDFe.Ide);
    Ler_Carregamento(INIRec, FMDFe.Ide.infMunCarrega);
    Ler_Percurso(INIRec, FMDFe.Ide.infPercurso);
    Ler_Emitente(INIRec, FMDFe.Emit);

    FMDFe.ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(FMDFe.Emit.enderEmit.UF));

    case FMDFe.Ide.modal of
      moRodoviario: Ler_ModalRodoviario(INIRec, FMDFe.rodo);
      moAereo: Ler_ModalAereo(INIRec, FMDFe.aereo);
      moAquaviario: Ler_ModalAquaviario(INIRec, FMDFe.aquav);
      moFerroviario: Ler_ModalFerroviario(INIRec, FMDFe.ferrov);
    end;

    Ler_Descarregamento(INIRec, FMDFe.infDoc.infMunDescarga);
    Ler_Seguro(INIRec, FMDFe.seg);
    Ler_ProdutoPredominante(INIRec, FMDFe.prodPred);
    Ler_Totais(INIRec, FMDFe.tot);
    Ler_Lacres(INIRec, FMDFe.lacres);
    Ler_AutorizadosXml(INIRec, FMDFe.autXML);
    Ler_InfAdicionais(INIRec, FMDFe.infAdic);
    Ler_InfRespTecnico(INIRec, FMDFe.infRespTec);
  finally
    INIRec.Free;
  end;
end;

procedure TMDFeIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
  OK: Boolean;
begin
  sSecao := 'ide';

  Ide.tpEmit  := StrToTpEmitente(OK, AINIRec.ReadString(sSecao, 'tpEmit', '1'));
  Ide.modelo  := AINIRec.ReadString(sSecao, 'mod', '58');
  Ide.serie   := AINIRec.ReadInteger(sSecao, 'serie', 1);
  Ide.nMDF    := AINIRec.ReadInteger(sSecao, 'nMDF', 0);
  Ide.cMDF    := AINIRec.ReadInteger(sSecao, 'cMDF', 0);
  Ide.modal   := StrToModal(OK, AINIRec.ReadString(sSecao, 'modal', '1'));
  Ide.dhEmi   := StringToDateTime(AINIRec.ReadString(sSecao, 'dhEmi', '0'));
  Ide.tpEmis  := StrToTpEmis(OK, AINIRec.ReadString(sSecao, 'tpEmis', IntToStr(tpEmis)));
  Ide.procEmi := StrToProcEmi(OK, AINIRec.ReadString(sSecao, 'procEmi', '0'));
  Ide.verProc := AINIRec.ReadString(sSecao, 'verProc', 'ACBrMDFe');
  Ide.UFIni   := AINIRec.ReadString(sSecao, 'UFIni', '');
  Ide.UFFim   := AINIRec.ReadString(sSecao, 'UFFim', '');
  Ide.tpTransp := StrToTTransportador(OK, AINIRec.ReadString(sSecao, 'tpTransp', '1'));
  Ide.dhIniViagem := StringToDateTime(AINIRec.ReadString(sSecao, 'dhIniViagem', '0'));
  Ide.indCanalVerde := StrToTIndicador(Ok, AINIRec.ReadString(sSecao, 'indCanalVerde', '0'));
  Ide.indCarregaPosterior := StrToTIndicador(Ok, AINIRec.ReadString(sSecao, 'indCarregaPosterior', '0'));
end;

procedure TMDFeIniReader.Ler_Carregamento(AINIRec: TMemIniFile;
  infMunCarrega: TinfMunCarregaCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfMunCarregaCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'CARR' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'xMunCarrega', 'FIM');

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := infMunCarrega.New;

    Item.cMunCarrega := AINIRec.ReadInteger(sSecao, 'cMunCarrega', 0);
    Item.xMunCarrega := sFim;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_Percurso(AINIRec: TMemIniFile;
  infPercurso: TinfPercursoCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'PERC' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'UFPer', 'FIM');

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    infPercurso.New.UFPer := sFim;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: Temit);
var
  sSecao: string;
begin
  sSecao := 'emit';

  Emit.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', AINIRec.ReadString(sSecao, 'CNPJ', ''));
  Emit.IE      := AINIRec.ReadString(sSecao, 'IE', '');
  Emit.xNome   := AINIRec.ReadString(sSecao, 'xNome', '');
  Emit.xFant   := AINIRec.ReadString(sSecao, 'xFant', '');

  Emit.enderEmit.xLgr    := AINIRec.ReadString(sSecao, 'xLgr', '');
  Emit.enderEmit.nro     := AINIRec.ReadString(sSecao, 'nro', '');
  Emit.enderEmit.xCpl    := AINIRec.ReadString(sSecao, 'xCpl', '');
  Emit.enderEmit.xBairro := AINIRec.ReadString(sSecao, 'xBairro', '');
  Emit.enderEmit.cMun    := AINIRec.ReadInteger(sSecao, 'cMun', 0);
  Emit.enderEmit.xMun    := AINIRec.ReadString(sSecao, 'xMun', '');
  Emit.enderEmit.CEP     := AINIRec.ReadInteger(sSecao, 'CEP', 0);
  Emit.enderEmit.UF      := AINIRec.ReadString(sSecao, 'UF', '');
  Emit.enderEmit.fone    := AINIRec.ReadString(sSecao, 'fone', '');
  Emit.enderEmit.email   := AINIRec.ReadString(sSecao, 'email', '');
end;

procedure TMDFeIniReader.Ler_ModalRodoviario(AINIRec: TMemIniFile; rodo: Trodo);
var
  sSecao: string;
begin
  sSecao := 'Rodo';

  rodo.codAgPorto := AINIRec.ReadString(sSecao, 'codAgPorto', '');

{
  GerarGrupo := (AINIRec.ReadString('infANTT', 'RNTRC', '') <> '') or
                AINIRec.SectionExists('infCIOT001') or
                AINIRec.SectionExists('valePed001') or
                AINIRec.SectionExists('infContratante001');
}
  Ler_InfANTT(AINIRec, rodo.infANTT);
  Ler_InfCIOT(AINIRec, rodo.infANTT.infCIOT);
  Ler_ValePedagio(AINIRec, rodo.infANTT.valePed);
  Ler_Dispositivo(AINIRec, rodo.infANTT.valePed.disp);
  Ler_InfContratante(AINIRec, rodo.infANTT.infContratante);
  Ler_InfPagamento(AINIRec, rodo.infANTT.infPag);

  Ler_VeiculoTracao(AINIRec, rodo.veicTracao);
  Ler_VeiculoReboque(AINIRec, rodo.veicReboque);
  Ler_LacresRodo(AINIRec, rodo.lacRodo);
end;

procedure TMDFeIniReader.Ler_InfANTT(AINIRec: TMemIniFile; infANTT: TinfANTT);
var
  sSecao: string;
begin
  sSecao := 'infANTT';

  infANTT.RNTRC := AINIRec.ReadString(sSecao, 'RNTRC', '');
end;

procedure TMDFeIniReader.Ler_InfCIOT(AINIRec: TMemIniFile;
  infCIOT: TinfCIOTCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfCIOTCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infCIOT' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infCIOT.New;

    Item.CIOT    := AINIRec.ReadString(sSecao, 'CIOT', '');
    Item.CNPJCPF := sFim;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_ValePedagio(AINIRec: TMemIniFile;
  valePed: TvalePed);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'valePed';
  valePed.categCombVeic := StrTocategCombVeic(OK, AINIRec.ReadString(sSecao, 'categCombVeic', ''));
end;

procedure TMDFeIniReader.Ler_Dispositivo(AINIRec: TMemIniFile;
  disp: TdispCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TdispCollectionItem;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'disp' + IntToStrZero(I, 3);

    if not AINIRec.SectionExists(sSecao) then
      sSecao := 'valePed' + IntToStrZero(I, 3);

    sFim   := AINIRec.ReadString(sSecao, 'CNPJForn', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := disp.New;

    Item.CNPJForn  := sFim;
    Item.CNPJPg    := AINIRec.ReadString(sSecao, 'CNPJPg', '');
    Item.nCompra   := AINIRec.ReadString(sSecao, 'nCompra', '');
    Item.vValePed  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vValePed', ''), 0 );
    Item.tpValePed := StrTotpValePed(OK, AINIRec.ReadString(sSecao, 'tpValePed', ''));

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfContratante(AINIRec: TMemIniFile;
  infContratante: TinfContratanteCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfContratanteCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infContratante' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'CNPJCPF', AINIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

    if sFim = 'FIM' then
      break;

    Item := infContratante.New;

    Item.CNPJCPF       := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    Item.idEstrangeiro := AINIRec.ReadString(sSecao, 'idEstrangeiro', '');
    Item.xNome         := AINIRec.ReadString(sSecao, 'xNome', '');

    Item.infContrato.NroContrato := AINIRec.ReadString(sSecao, 'NroContrato', '');
    Item.infContrato.vContratoGlobal := StringToFloatDef(AINIRec.ReadString(sSecao, 'vContratoGlobal', ''), 0 );

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfPagamento(AINIRec: TMemIniFile;
  infPag: TinfPagCollection);
var
  I: Integer;
  sSecao, sFim: string;
  Item: TinfPagCollectionItem;
  Ok: Boolean;
begin
  sSecao := 'infPag001';

  if AINIRec.SectionExists(sSecao) then
  begin
    I := 1;
    while true do
    begin
      sSecao := 'infPag' + IntToStrZero(I, 3);
      sFim   := AINIRec.ReadString(sSecao, 'CNPJCPF', AINIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

      if sFim = 'FIM' then
        break;

      Item := infPag.New;

      Item.xNome         := AINIRec.ReadString(sSecao, 'xNome', '');
      Item.idEstrangeiro := AINIRec.ReadString(sSecao, 'idEstrangeiro', '');

      if Item.idEstrangeiro = '' then
        Item.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');

      Item.vContrato     := StringToFloatDef(AINIRec.ReadString(sSecao, 'vContrato', ''), 0 );
      Item.indAltoDesemp := StrToindAltoDesemp(ok, AINIRec.ReadString(sSecao, 'indAltoDesemp', ''));
      Item.indPag        := StrToTIndPag(ok, AINIRec.ReadString(sSecao, 'indPag', '0'));
      Item.vAdiant       := StringToFloatDef(AINIRec.ReadString(sSecao, 'vAdiant', ''), 0 );

      Item.indAntecipaAdiant := StrToTIndicador(ok, AINIRec.ReadString(sSecao, 'indAntecipaAdiant', '0'));
      Item.tpAntecip := StrTotpAntecip(ok, AINIRec.ReadString(sSecao, 'tpAntecip', ''));

      Ler_Componentes(AINIRec, Item.Comp, i);

      if Item.indPag = ipPrazo then
      begin
        Ler_InfPrazos(AINIRec, Item.infPrazo, i);
      end;

      Ler_InfBanco(AINIRec, Item.infBanc, i);

      Inc(I);
    end;
  end;
end;

procedure TMDFeIniReader.Ler_Componentes(AINIRec: TMemIniFile;
  Comp: TCompCollection; Idx: Integer);
var
  I: Integer;
  sSecao, sFim: string;
  Item: TCompCollectionItem;
  Ok: Boolean;
begin
  i := 1;
  while true do
  begin
    sSecao := 'Comp' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim   := AINIRec.ReadString(sSecao, 'vComp', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := Comp.New;

    Item.tpComp := StrToTComp(ok, AINIRec.ReadString(sSecao, 'tpComp', '01'));
    Item.vComp  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vComp', ''), 0 );
    Item.xComp  := AINIRec.ReadString(sSecao, 'xComp', '');

    Inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfPrazos(AINIRec: TMemIniFile;
  infPrazo: TInfPrazoCollection; Idx: Integer);
var
  I: Integer;
  sSecao, sFim: string;
  Item: TInfPrazoCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infPrazo' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim   := AINIRec.ReadString(sSecao, 'vParcela', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infPrazo.New;

    Item.nParcela := AINIRec.ReadInteger(sSecao, 'nParcela', 1);
    Item.dVenc    := StringToDateTime(AINIRec.ReadString(sSecao, 'dVenc', '0'));
    Item.vParcela := StringToFloatDef(AINIRec.ReadString(sSecao, 'vParcela', ''), 0 );

    Inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfBanco(AINIRec: TMemIniFile; infBanc: TinfBanc;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'infBanc' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    infBanc.PIX := AINIRec.ReadString(sSecao, 'PIX', '');

    if infBanc.PIX = '' then
    begin
      infBanc.CNPJIPEF := AINIRec.ReadString(sSecao, 'CNPJIPEF', '');

      if infBanc.CNPJIPEF = '' then
      begin
        infBanc.codBanco   := AINIRec.ReadString(sSecao, 'codBanco', '');
        infBanc.codAgencia := AINIRec.ReadString(sSecao, 'codAgencia', '');
      end;
    end;
  end;
end;

procedure TMDFeIniReader.Ler_VeiculoTracao(AINIRec: TMemIniFile;
  veicTracao: TveicTracao);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'veicTracao';

  if AINIRec.ReadString(sSecao, 'placa', '') <> '' then
  begin
    veicTracao.cInt    := AINIRec.ReadString(sSecao, 'cInt', '');
    veicTracao.placa   := AINIRec.ReadString(sSecao, 'placa', '');
    veicTracao.RENAVAM := AINIRec.ReadString(sSecao, 'RENAVAM', '');
    veicTracao.tara    := AINIRec.ReadInteger(sSecao, 'tara', 0);
    veicTracao.capKG   := AINIRec.ReadInteger(sSecao, 'capKG', 0);
    veicTracao.capM3   := AINIRec.ReadInteger(sSecao, 'capM3', 0);
    veicTracao.tpRod   := StrToTpRodado(OK, AINIRec.ReadString(sSecao, 'tpRod', '01'));
    veicTracao.tpCar   := StrToTpCarroceria(OK, AINIRec.ReadString(sSecao, 'tpCar', '00'));
    veicTracao.UF      := AINIRec.ReadString(sSecao, 'UF', '');
  end;

  if AINIRec.ReadString(sSecao, 'CNPJCPF', '') <> '' then
  begin
    veicTracao.prop.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    veicTracao.prop.RNTRC   := AINIRec.ReadString(sSecao, 'RNTRC', '');
    veicTracao.prop.xNome   := AINIRec.ReadString(sSecao, 'xNome', '');
    veicTracao.prop.IE      := AINIRec.ReadString(sSecao, 'IE', 'ISENTO');
    veicTracao.prop.UF      := AINIRec.ReadString(sSecao, 'UFProp', '');
    veicTracao.prop.tpProp  := StrToTpProp(OK, AINIRec.ReadString(sSecao, 'tpProp', '0'));
  end;

  Ler_Motoristas(AINIRec, veicTracao.condutor);
end;

procedure TMDFeIniReader.Ler_Motoristas(AINIRec: TMemIniFile;
  condutor: TcondutorCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TcondutorCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'moto' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'xNome', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := condutor.New;

    Item.xNome := sFim;
    Item.CPF   := AINIRec.ReadString(sSecao, 'CPF', '');

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_VeiculoReboque(AINIRec: TMemIniFile;
  veicReboque: TveicReboqueCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TveicReboqueCollectionItem;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'reboque' + IntToStrZero(I, 2);
    sFim   := AINIRec.ReadString(sSecao, 'placa', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := veicReboque.New;

    Item.cInt    := AINIRec.ReadString(sSecao, 'cInt', '');
    Item.placa   := sFim;
    Item.RENAVAM := AINIRec.ReadString(sSecao, 'RENAVAM', '');
    Item.tara    := AINIRec.ReadInteger(sSecao, 'tara', 0);
    Item.capKG   := AINIRec.ReadInteger(sSecao, 'capKG', 0);
    Item.capM3   := AINIRec.ReadInteger(sSecao, 'capM3', 0);
    Item.tpCar   := StrToTpCarroceria(OK, AINIRec.ReadString(sSecao, 'tpCar', '00'));
    Item.UF      := AINIRec.ReadString(sSecao, 'UF', '');

    if AINIRec.ReadString(sSecao, 'CNPJCPF', '') <> '' then
    begin
      Item.prop.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
      Item.prop.RNTRC   := AINIRec.ReadString(sSecao, 'RNTRC', '');
      Item.prop.xNome   := AINIRec.ReadString(sSecao, 'xNome', '');
      Item.prop.IE      := AINIRec.ReadString(sSecao, 'IE', '');
      Item.prop.UF      := AINIRec.ReadString(sSecao, 'UFProp', '');
      Item.prop.tpProp  := StrToTpProp(OK, AINIRec.ReadString(sSecao, 'tpProp', '0'));
    end;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_LacresRodo(AINIRec: TMemIniFile;
  lacRodo: TlacRodoCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'lacRodo' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'nLacre', 'FIM');

    if sFim = 'FIM' then
      break;

    lacRodo.New.nLacre := sFim;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_ModalAereo(AINIRec: TMemIniFile; aereo: Taereo);
var
  sSecao: string;
begin
  sSecao := 'aereo';

  Aereo.nac := AINIRec.ReadString(sSecao, 'nac', '');
  if (Aereo.nac <> '') then
  begin
    Aereo.matr    := AINIRec.ReadString(sSecao, 'matr', '');
    Aereo.nVoo    := AINIRec.ReadString(sSecao, 'nVoo', '');
    Aereo.cAerEmb := AINIRec.ReadString(sSecao, 'cAerEmb', '');
    Aereo.cAerDes := AINIRec.ReadString(sSecao, 'cAerDes', '');
    Aereo.dVoo    := StringToDateTime(AINIRec.ReadString(sSecao, 'dVoo', '0'));
  end;
end;

procedure TMDFeIniReader.Ler_ModalAquaviario(AINIRec: TMemIniFile;
  aquav: Taquav);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'aquav';

  Aquav.CNPJAgeNav  := AINIRec.ReadString(sSecao, 'CNPJAgeNav', '');
  Aquav.irin        := AINIRec.ReadString(sSecao, 'irin', '');

  if ( (Aquav.CNPJAgeNav  <> '') or (Aquav.irin <> '') ) then
  begin
    Aquav.tpEmb    := AINIRec.ReadString(sSecao, 'tpEmb', '');
    Aquav.cEmbar   := AINIRec.ReadString(sSecao, 'cEmbar', '');
    Aquav.xEmbar   := AINIRec.ReadString(sSecao, 'xEmbar', '');
    Aquav.nViagem  := AINIRec.ReadString(sSecao, 'nViag', '');
    Aquav.cPrtEmb  := AINIRec.ReadString(sSecao, 'cPrtEmb', '');
    Aquav.cPrtDest := AINIRec.ReadString(sSecao, 'cPrtDest', '');
    Aquav.MMSI := AINIRec.ReadString(sSecao, 'MMSI', '');

    //Campos MDF-e 3.0
    Aquav.prtTrans := AINIRec.ReadString(sSecao, 'prtTrans', '');
    Aquav.tpNav    := StrToTpNavegacao(OK, AINIRec.ReadString(sSecao, 'tpNav', '0') );

    Ler_InfTerminalCarregamento(AINIRec, aquav.infTermCarreg);
    Ler_InfTerminalDescarregamento(AINIRec, aquav.infTermDescarreg);
    Ler_InfEmbComboio(AINIRec, aquav.infEmbComb);
    Ler_InfUnidCargaVazia(AINIRec, aquav.infUnidCargaVazia);
    Ler_InfUnidTranspVazia(AINIRec, aquav.infUnidTranspVazia);
  end;
end;

procedure TMDFeIniReader.Ler_InfTerminalCarregamento(AINIRec: TMemIniFile;
  infTermCarreg: TinfTermCarregCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfTermCarregCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infTermCarreg' + IntToStrZero(I, 1);
    sFim   := AINIRec.ReadString(sSecao, 'cTermCarreg', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infTermCarreg.New;

    Item.cTermCarreg := sFim;
    Item.xTermCarreg := AINIRec.ReadString(sSecao, 'xTermCarreg', '');

    inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfTerminalDescarregamento(AINIRec: TMemIniFile;
  infTermDescarreg: TinfTermDescarregCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfTermDescarregCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infTermDescarreg' + IntToStrZero(I, 1);
    sFim   := AINIRec.ReadString(sSecao, 'cTermDescarreg', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infTermDescarreg.New;

    Item.cTermDescarreg := sFim;
    Item.xTermDescarreg := AINIRec.ReadString(sSecao, 'xTermDescarreg', '');

    inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfEmbComboio(AINIRec: TMemIniFile;
  infEmbComb: TinfEmbCombCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfEmbCombCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infEmbComb' + IntToStrZero(I, 2);
    sFim   := AINIRec.ReadString(sSecao, 'cEmbComb', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infEmbComb.New;

    Item.cEmbComb := sFim;
    Item.xBalsa   :=  AINIRec.ReadString(sSecao, 'xBalsa', '');

    inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfUnidCargaVazia(AINIRec: TMemIniFile;
  infUnidCargaVazia: TinfUnidCargaVaziaCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfUnidCargaVaziaCollectionItem;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infUnidCargaVazia' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'idUnidCargaVazia', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infUnidCargaVazia.New;

    Item.idUnidCargaVazia := sFim;
    Item.tpUnidCargaVazia := StrToUnidCarga(OK, AINIRec.ReadString(sSecao, 'tpUnidCargaVazia', '1'));

    inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfUnidTranspVazia(AINIRec: TMemIniFile;
  infUnidTranspVazia: TinfUnidTranspVaziaCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfUnidTranspVaziaCollectionItem;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infUnidTranspVazia' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'idUnidTranspVazia', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infUnidTranspVazia.New;

    Item.idUnidTranspVazia := sFim;
    Item.tpUnidTranspVazia := StrToUnidTransp (OK, AINIRec.ReadString(sSecao, 'tpUnidTranspVazia', '1'));

    inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_ModalFerroviario(AINIRec: TMemIniFile;
  ferrov: Tferrov);
var
  sSecao: string;
begin
  sSecao := 'ferrov';

  Ferrov.xPref  := AINIRec.ReadString(sSecao, 'xPref', '');
  if (Ferrov.xPref <> '') then
  begin
    Ferrov.dhTrem := StringToDateTime(AINIRec.ReadString(sSecao, 'dhTrem', '0'));
    Ferrov.xOri   := AINIRec.ReadString(sSecao, 'xOri', '');
    Ferrov.xDest  := AINIRec.ReadString(sSecao, 'xDest', '');
    Ferrov.qVag   := AINIRec.ReadInteger(sSecao, 'qVag', 0);

    Ler_Vagoes(AINIRec, Ferrov.vag);
  end;
end;

procedure TMDFeIniReader.Ler_Vagoes(AINIRec: TMemIniFile; vag: TvagCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TvagCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'vag' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'serie', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := vag.New;

    Item.serie := sFim;
    Item.nVag  := AINIRec.ReadInteger(sSecao, 'nVag', 0);
    Item.nSeq  := AINIRec.ReadInteger(sSecao, 'nSeq', 0);
    Item.TU    := StringToFloatDef(AINIRec.ReadString(sSecao, 'TU', ''), 0);

    //Campos MDF-e 3.0
    Item.pesoBC := StringToFloatDef( AINIRec.ReadString(sSecao, 'pesoBC', ''), 0);
    Item.pesoR  := StringToFloatDef( AINIRec.ReadString(sSecao, 'pesoR', ''), 0);
    Item.tpVag  := AINIRec.ReadString(sSecao, 'tpVag', '');

    inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_Descarregamento(AINIRec: TMemIniFile;
  infMunDescarga: TinfMunDescargaCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfMunDescargaCollectionItem;
begin
  I := 1;
  while true do
  begin
    sSecao := 'DESC' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'xMunDescarga', 'FIM');

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := infMunDescarga.New;

    Item.cMunDescarga := AINIRec.ReadInteger(sSecao, 'cMunDescarga', 0);
    Item.xMunDescarga := sFim;

    Ler_InfCTe(AINIRec, Item.infCTe, I);
    Ler_InfNFe(AINIRec, Item.infNFe, I);
    Ler_InfMDFeTransp(AINIRec, Item.infMDFeTransp, I);

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfCTe(AINIRec: TMemIniFile;
  infCTe: TinfCTeCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfCTeCollectionItem;
  Ok: Boolean;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infCTe' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim   := AINIRec.ReadString(sSecao, 'chCTe', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infCTe.New;

    Item.chCTe       := sFim;
    Item.SegCodBarra := AINIRec.ReadString(sSecao, 'SegCodBarra', '');
    Item.indReentrega:= AINIRec.ReadString(sSecao, 'indReentrega', '');
    Item.indPrestacaoParcial:= StrToTIndicadorEx(OK, AINIRec.ReadString(sSecao, 'indPrestacaoParcial', ''));

    Ler_Peri(AINIRec, Item.peri, Idx, i);
    Ler_InfNFePrestParcial(AINIRec, Item.infNFePrestParcial, Idx, i);
    Ler_InfEntregaParcial(AINIRec, Item.infEntregaParcial, Idx, i);
    Ler_InfUnidTransp(AINIRec, Item.infUnidTransp, Idx, i);

    Inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_Peri(AINIRec: TMemIniFile;
  peri: TPeriCTeCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TPeriCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'peri'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+IntToStrZero(i,3);
    sFim   := AINIRec.ReadString(sSecao,'nONU','FIM');

    if sFim = 'FIM' then
      break;

    Item := peri.New;

    Item.nONU      := sFim;
    Item.xNomeAE   := AINIRec.ReadString(sSecao,'xNomeAE','');
    Item.xClaRisco := AINIRec.ReadString(sSecao,'xClaRisco','');
    Item.grEmb     := AINIRec.ReadString(sSecao,'grEmb','');
    Item.qTotProd  := AINIRec.ReadString(sSecao,'qTotProd','');
    Item.qVolTipo  := AINIRec.ReadString(sSecao,'qVolTipo','');

    inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfNFePrestParcial(AINIRec: TMemIniFile;
  infNFePrestParcial: TinfNFePrestParcialCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfNFePrestParcialCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infNFePrestParcial'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+IntToStrZero(i,3);
    sFim   := AINIRec.ReadString(sSecao,'chNFe','FIM');

    if sFim = 'FIM' then
      break;

    Item := infNFePrestParcial.New;

    Item.chNFe := sFim;

    inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfEntregaParcial(AINIRec: TMemIniFile;
  infEntregaParcial: TinfEntregaParcial; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'infEntregaParcial'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3);

  if AINIRec.SectionExists(sSecao) then
  begin
    infEntregaParcial.qtdTotal   := StringToFloatDef(AINIRec.ReadString(sSecao, 'qtdTotal', ''), 0);
    infEntregaParcial.qtdParcial := StringToFloatDef(AINIRec.ReadString(sSecao, 'qtdParcial', ''), 0);
  end;
end;

procedure TMDFeIniReader.Ler_InfUnidTransp(AINIRec: TMemIniFile;
  infUnidTransp: TinfUnidTranspCTeCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfUnidTranspCollectionItem;
  Ok: Boolean;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infUnidTransp'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+IntToStrZero(i,3);
    sFim   := AINIRec.ReadString(sSecao,'idUnidTransp','FIM');

    if sFim = 'FIM' then
      break;

    Item := infUnidTransp.New;

    Item.tpUnidTransp := StrToUnidTransp(OK,AINIRec.ReadString(sSecao,'tpUnidTransp','1'));
    Item.idUnidTransp := sFim;
    Item.qtdRat       := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

    Ler_LacUnidTransp(AINIRec, Item.lacUnidTransp, Idx1, Idx2, i);
    Ler_InfUnidCarga(AINIRec, Item.infUnidCarga, Idx1, Idx2, i);

    inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_LacUnidTransp(AINIRec: TMemIniFile;
  lacUnidTransp: TlacresCollection; Idx1, Idx2, Idx3: Integer);
var
  i: Integer;
  sSecao, sFim: string;
begin
  i := 1;
  while true do
  begin
    sSecao := 'lacUnidTransp'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+
                              IntToStrZero(Idx3,3)+IntToStrZero(i,3);
    sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');

    if sFim = 'FIM' then
      break;

    lacUnidTransp.New.nLacre := sFim;

    inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfUnidCarga(AINIRec: TMemIniFile;
  infUnidCarga: TinfUnidCargaCollection; Idx1, Idx2, Idx3: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfUnidCargaCollectionItem;
  Ok: Boolean;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infUnidCarga'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+
                             IntToStrZero(Idx3,3)+IntToStrZero(i,3);
    sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');

    if sFim = 'FIM' then
      break;

    Item := infUnidCarga.New;

    Item.tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
    Item.idUnidCarga := sFim;
    Item.qtdRat      := StringToFloatDef( AINIRec.ReadString(sSecao,'qtdRat',''),0);

    Ler_LacUnidCarga(AINIRec, Item.lacUnidCarga, Idx1, Idx2, Idx3, i);

    inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_LacUnidCarga(AINIRec: TMemIniFile;
  lacUnidCarga: TlacUnidCargaCollection; Idx1, Idx2, Idx3, Idx4: Integer);
var
  i: Integer;
  sSecao, sFim: string;
begin
  i := 1;
  while true do
  begin
    sSecao := 'lacUnidCarga'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+
                    IntToStrZero(Idx3,3)+IntToStrZero(Idx4,3)+IntToStrZero(i,3);
    sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');

    if sFim = 'FIM' then
      break;

    lacUnidCarga.New.nLacre := sFim;

    inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfNFe(AINIRec: TMemIniFile;
  infNFe: TinfNFeCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfNFeCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infNFe' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim   := AINIRec.ReadString(sSecao, 'chNFe', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infNFe.New;

    Item.chNFe        := sFim;
    Item.SegCodBarra  := AINIRec.ReadString(sSecao, 'SegCodBarra', '');
    Item.indReentrega := AINIRec.ReadString(sSecao, 'indReentrega', '');

    Ler_Peri(AINIRec, Item.peri, Idx, i);
    Ler_InfUnidTransp(AINIRec, Item.infUnidTransp, Idx, i);

    Inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_InfMDFeTransp(AINIRec: TMemIniFile;
  infMDFeTransp: TinfMDFeTranspCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TinfMDFeTranspCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'infMDFeTransp' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim   := AINIRec.ReadString(sSecao, 'chMDFe', 'FIM');

    if sFim = 'FIM' then
      break;

    Item := infMDFeTransp.New;

    Item.chMDFe := sFim;
    Item.indReentrega:= AINIRec.ReadString(sSecao, 'indReentrega', '');

    Ler_Peri(AINIRec, Item.peri, Idx, i);
    Ler_InfUnidTransp(AINIRec, Item.infUnidTransp, Idx, i);

    Inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_Seguro(AINIRec: TMemIniFile; seg: TSegCollection);
var
  i: Integer;
  sSecao: string;
  Item: TSegCollectionItem;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'seg' + IntToStrZero(I, 3);

    if (AINIRec.ReadString(sSecao, 'xSeg', '') = '') and
       (AINIRec.ReadString(sSecao, 'nApol', '') = '') and
       not AINIRec.SectionExists('aver' + IntToStrZero(I, 3) + '001') then
      Break;

    Item := seg.New;

    Item.respSeg :=  StrToRspSeguroMDFe(OK, AINIRec.ReadString(sSecao, 'respSeg', '1'));
    Item.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    Item.xSeg    := AINIRec.ReadString(sSecao, 'xSeg', '');
    Item.CNPJ    := AINIRec.ReadString(sSecao, 'CNPJ', '');
    Item.nApol   := AINIRec.ReadString(sSecao, 'nApol', '');

    Ler_Averbacao(AINIRec, Item.aver, I);

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_Averbacao(AINIRec: TMemIniFile;
  aver: TAverCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
begin
  i := 1;
  while true do
  begin
    sSecao := 'aver' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim   := AINIRec.ReadString(sSecao, 'nAver', 'FIM');

    if sFim = 'FIM' then
      break;

    aver.New.nAver := sFim;

    Inc(i);
  end;
end;

procedure TMDFeIniReader.Ler_ProdutoPredominante(AINIRec: TMemIniFile;
  prodPred: TprodPred);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'prodPred';

  if AINIRec.SectionExists(sSecao) then
  begin
    prodPred.tpCarga := StrToTCarga(ok, AINIRec.ReadString(sSecao, 'tpCarga', '01'));
    prodPred.xProd   := AINIRec.ReadString(sSecao, 'xProd', '');
    prodPred.cEAN    := AINIRec.ReadString(sSecao, 'cEAN', '');
    prodPred.NCM     := AINIRec.ReadString(sSecao, 'NCM', '');

    sSecao := 'infLocalCarrega';

    if AINIRec.SectionExists(sSecao) then
    begin
      prodPred.infLocalCarrega.CEP       := AINIRec.ReadInteger(sSecao, 'CEP', 0);
      prodPred.infLocalCarrega.latitude  := StringToFloatDef( AINIRec.ReadString(sSecao,'latitude',''),0);
      prodPred.infLocalCarrega.longitude := StringToFloatDef( AINIRec.ReadString(sSecao,'longitude',''),0);
    end;

    sSecao := 'infLocalDescarrega';

    if AINIRec.SectionExists(sSecao) then
    begin
      prodPred.infLocalDescarrega.CEP       := AINIRec.ReadInteger(sSecao, 'CEP', 0);
      prodPred.infLocalDescarrega.latitude  := StringToFloatDef( AINIRec.ReadString(sSecao,'latitude',''),0);
      prodPred.infLocalDescarrega.longitude := StringToFloatDef( AINIRec.ReadString(sSecao,'longitude',''),0);
    end;
  end;
end;

procedure TMDFeIniReader.Ler_Totais(AINIRec: TMemIniFile; tot: Ttot);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'tot';
  tot.qCTe   := AINIRec.ReadInteger(sSecao, 'qCTe', 0);
  tot.qCT    := AINIRec.ReadInteger(sSecao, 'qCT', 0);
  tot.qNFe   := AINIRec.ReadInteger(sSecao, 'qNFe', 0);
  tot.qNF    := AINIRec.ReadInteger(sSecao, 'qNF', 0);
  tot.qMDFe  := AINIRec.ReadInteger(sSecao, 'qMDFe', 0);
  tot.vCarga := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCarga', ''), 0);
  tot.cUnid  := StrToUnidMed(OK, AINIRec.ReadString(sSecao, 'cUnid', '01'));
  tot.qCarga := StringToFloatDef(AINIRec.ReadString(sSecao, 'qCarga', ''), 0);
end;

procedure TMDFeIniReader.Ler_Lacres(AINIRec: TMemIniFile;
  lacres: TlacresCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'lacres' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'nLacre', 'FIM');

    if sFim = 'FIM' then
      break;

    lacres.New.nLacre := sFim;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  I: Integer;
  sSecao, sFim: String;
begin
  I := 1 ;
  while true do
  begin
    sSecao := 'autXML' + IntToStrZero(I,2) ;
    sFim   := OnlyNumber(AINIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break ;

    autXML.New.CNPJCPF := sFim;

    Inc(I);
  end;
end;

procedure TMDFeIniReader.Ler_InfAdicionais(AINIRec: TMemIniFile;
  infAdic: TinfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';

  infAdic.infCpl     := AINIRec.ReadString(sSecao, 'infCpl', '');
  infAdic.infAdFisco := AINIRec.ReadString(sSecao, 'infAdFisco', '');
end;

procedure TMDFeIniReader.Ler_InfRespTecnico(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  sSecao := 'infRespTec';
  if AINIRec.SectionExists(sSecao) then
  begin
    infRespTec.CNPJ     := AINIRec.ReadString(sSecao, 'CNPJ', '');
    infRespTec.xContato := AINIRec.ReadString(sSecao, 'xContato', '');
    infRespTec.email    := AINIRec.ReadString(sSecao, 'email', '');
    infRespTec.fone     := AINIRec.ReadString(sSecao, 'fone', '');
  end;
end;

end.
