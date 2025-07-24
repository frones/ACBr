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

unit ACBrNFComIniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrNFComClass,
  ACBrNFComConversao;

type
  { TNFComIniReader }

  TNFComIniReader = class
  private
    FNFCom: TNFCom;
    FVersaoDF: TVersaoNFCom;
    FAmbiente: Integer;
    FtpEmis: Integer;

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Ler_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
    procedure Ler_Assinante(AINIRec: TMemIniFile; assinante: Tassinante);
    procedure Ler_TermAdic(AINIRec: TMemIniFile; TermAdic: TTermAdicCollection);
    procedure Ler_gSub(AINIRec: TMemIniFile; gSub: TgSub);
    procedure Ler_gCofat(AINIRec: TMemIniFile; gCofat: TgCofat);
    procedure Ler_Det(AINIRec: TMemIniFile; Det: TDetCollection);
    procedure Ler_ICMS(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_ICMSUFDest(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_PIS(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_COFINS(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_FUST(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_FUNTTEL(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_RetTrib(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_gProcRef(AINIRec: TMemIniFile; gProcRef: TgProcRef; Idx: Integer);
    procedure Ler_gProc(AINIRec: TMemIniFile; gProc: TgProcCollection; Idx: Integer);
    procedure Ler_gRessarc(AINIRec: TMemIniFile; gRessarc: TgRessarc; Idx: Integer);
    procedure Ler_Total(AINIRec: TMemIniFile; Total: TTotal);
    procedure Ler_gFidelidade(AINIRec: TMemIniFile; gFidelidade: TgFidelidade);
    procedure Ler_gFat(AINIRec: TMemIniFile; gFat: TgFat);
    procedure Ler_gFatCentral(AINIRec: TMemIniFile; gFatCentral: TgFatCentral);
    procedure Ler_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Ler_InfRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);

    // Reforma Tributária
    procedure Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS; Idx: Integer);
    procedure Ler_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores; Idx: Integer);
    procedure Ler_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores; Idx: Integer);
    procedure Ler_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores; Idx: Integer);

    procedure Ler_gTribReg(AINIRec: TMemIniFile; gTribRegular: TgTribRegular; Idx: Integer);
    procedure Ler_gIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres; Idx: Integer);
    procedure Ler_gCBSCredPres(AINIRec: TMemIniFile; gCBSCredPres: TgIBSCBSCredPres; Idx: Integer);
    procedure Ler_gTribCompraGov(AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov; Idx: Integer);

    procedure Ler_IBSCBSTot(AINIRec: TMemIniFile; IBSCBSTot: TIBSCBSTot);
    procedure Ler_IBSCBSTot_gIBS(AINIRec: TMemIniFile; gIBS: TgIBS);
    procedure Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile; gIBSUFTot: TgIBSUFTot);
    procedure Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile; gIBSMunTot: TgIBSMunTot);
    procedure Ler_IBSCBSTot_gCBS(AINIRec: TMemIniFile; gCBS: TgCBS);
  public
    constructor Create(AOwner: TNFCom); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property NFCom: TNFCom read FNFCom write FNFCom;
    property VersaoDF: TVersaoNFCom read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrDFe.Conversao,
  ACBrNFCom,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TNFComIniReader }

constructor TNFComIniReader.Create(AOwner: TNFCom);
begin
  inherited Create;

  FNFCom := AOwner;
end;

function TNFComIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FNFCom.infNFCom.versao := StringToFloatDef(INIRec.ReadString('infNFCom', 'versao', VersaoNFComToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FNFCom.Ide);
    Ler_Emitente(INIRec, FNFCom.Emit);

    FNFCom.ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(FNFCom.Emit.enderEmit.UF));
    FNFCom.Ide.cMunFG := INIRec.ReadInteger('ide', 'cMunFG', FNFCom.Emit.EnderEmit.cMun);

    Ler_Destinatario(INIRec, FNFCom.Dest);
    Ler_Assinante(INIRec, FNFCom.assinante);
    Ler_TermAdic(INIRec, FNFCom.assinante.TermAdic);
    Ler_gSub(INIRec, FNFCom.gSub);
    Ler_gCofat(INIRec, FNFCom.gCofat);
    Ler_Det(INIRec, FNFCom.Det);
    Ler_Total(INIRec, FNFCom.Total);
    Ler_gFidelidade(INIRec, FNFCom.gFidelidade);
    Ler_gFat(INIRec, FNFCom.gFat);
    Ler_gFatCentral(INIRec, FNFCom.gFatCentral);
    Ler_AutorizadosXml(INIRec, FNFCom.autXML);
    Ler_InfAdic(INIRec, FNFCom.InfAdic);
    Ler_InfRespTec(INIRec, FNFCom.infRespTec);
  finally
    INIRec.Free;
  end;
end;

procedure TNFComIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'ide';
  Ide.tpAmb := StrToTipoAmbiente(OK, AINIRec.ReadString(sSecao, 'tpAmb', IntToStr(Ambiente)));
  Ide.modelo := AINIRec.ReadInteger(sSecao, 'Modelo', 62);
  Ide.serie := AINIRec.ReadInteger(sSecao, 'Serie', 1);
  Ide.nNF := AINIRec.ReadInteger(sSecao, 'nNF', 0);
  Ide.cNF := AINIRec.ReadInteger(sSecao, 'cNF', 0);
  Ide.dhEmi := StringToDateTime(AINIRec.ReadString(sSecao, 'dhEmi', '0'));
  Ide.tpEmis := StrToTipoEmissao(OK, AINIRec.ReadString(sSecao, 'tpEmis', IntToStr(tpEmis)));
  Ide.nSiteAutoriz := StrToSiteAutorizator(AINIRec.ReadString(sSecao, 'nSiteAutoriz', '0'));
  Ide.finNFCom := StrToFinNFCom(AINIRec.ReadString(sSecao, 'finNFCom', '0'));
  Ide.tpFat := StrToTipoFaturamento(AINIRec.ReadString(sSecao, 'tpFat', '0'));
  Ide.verProc := AINIRec.ReadString(sSecao, 'verProc', 'ACBrNFCom');
  Ide.indPrePago := StrToTIndicador(AINIRec.ReadString(sSecao, 'indPrePago', '0'));
  Ide.indCessaoMeiosRede := StrToTIndicador(AINIRec.ReadString(sSecao, 'indCessaoMeiosRede', '0'));
  Ide.indNotaEntrada := StrToTIndicador(AINIRec.ReadString(sSecao, 'indNotaEntrada', '0'));
  Ide.dhCont := StringToDateTime(AINIRec.ReadString(sSecao, 'dhCont', '0'));
  Ide.xJust := AINIRec.ReadString(sSecao, 'xJust', '');

  // Reforma Tributária
  Ide.gCompraGov.pRedutor := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedutor', ''), 0);

  if Ide.gCompraGov.pRedutor > 0 then
    Ide.gCompraGov.tpEnteGov := StrTotpEnteGov(AINIRec.ReadString(sSecao, 'tpEnteGov', ''));
end;

procedure TNFComIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  sSecao: string;
begin
  sSecao := 'emit';
  Emit.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  Emit.IE := AINIRec.ReadString(sSecao, 'IE', '');
  Emit.IEUFDest := AINIRec.ReadString(sSecao, 'IEUFDest', '');
  Emit.CRT := StrToCRT(AINIRec.ReadString(sSecao, 'CRT', '3'));
  Emit.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
  Emit.xFant := AINIRec.ReadString(sSecao, 'xFant', '');
  // Endereço do Emitente
  Emit.EnderEmit.xLgr := AINIRec.ReadString(sSecao, 'xLgr', '');
  Emit.EnderEmit.nro := AINIRec.ReadString(sSecao, 'nro', '');
  Emit.EnderEmit.xCpl := AINIRec.ReadString(sSecao, 'xCpl', '');
  Emit.EnderEmit.xBairro := AINIRec.ReadString(sSecao, 'xBairro', '');
  Emit.EnderEmit.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
  Emit.EnderEmit.xMun := AINIRec.ReadString(sSecao, 'xMun', '');
  Emit.EnderEmit.CEP := AINIRec.ReadInteger(sSecao, 'CEP', 0);
  Emit.EnderEmit.UF := AINIRec.ReadString(sSecao, 'UF', '');
  Emit.EnderEmit.fone := AINIRec.ReadString(sSecao, 'fone', '');
  Emit.EnderEmit.email := AINIRec.ReadString(sSecao, 'email', '');
end;

procedure TNFComIniReader.Ler_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
var
  sSecao: string;
begin
  sSecao := 'dest';
  Dest.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
  Dest.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
  Dest.idOutros := AINIRec.ReadString(sSecao, 'idOutros','');
  Dest.indIEDest := StrToindIEDest(AINIRec.ReadString(sSecao, 'indIEDest', '1'));
  Dest.IE := AINIRec.ReadString(sSecao, 'IE', '');
  Dest.IM := AINIRec.ReadString(sSecao, 'IM', '');
  // Endereço do Destinatario
  Dest.EnderDest.xLgr := AINIRec.ReadString(sSecao, 'xLgr', '');
  Dest.EnderDest.nro := AINIRec.ReadString(sSecao, 'nro', '');
  Dest.EnderDest.xCpl := AINIRec.ReadString(sSecao, 'xCpl', '');
  Dest.EnderDest.xBairro := AINIRec.ReadString(sSecao, 'xBairro', '');
  Dest.EnderDest.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
  Dest.EnderDest.xMun := AINIRec.ReadString(sSecao, 'xMun', '');
  Dest.EnderDest.CEP := AINIRec.ReadInteger(sSecao, 'CEP', 0);
  Dest.EnderDest.UF := AINIRec.ReadString(sSecao, 'UF', '');
  Dest.EnderDest.fone := AINIRec.ReadString(sSecao, 'fone', '');
  Dest.EnderDest.email := AINIRec.ReadString(sSecao, 'email', '');
end;

procedure TNFComIniReader.Ler_Assinante(AINIRec: TMemIniFile;
  assinante: Tassinante);
var
  sSecao: string;
begin
  sSecao := 'assinante';
  assinante.iCodAssinante := AINIRec.ReadString(sSecao, 'iCodAssinante', '');
  assinante.tpAssinante := StrTotpAssinante(AINIRec.ReadString(sSecao, 'tpAssinante', '1'));
  assinante.tpServUtil := StrTotpServUtil(AINIRec.ReadString(sSecao, 'tpServUtil', '1'));
  assinante.nContrato := AINIRec.ReadString(sSecao, 'nContrato', '');
  assinante.dContratoIni := StringToDateTime(AINIRec.ReadString(sSecao, 'dContratoIni', '0'));
  assinante.dContratoFim := StringToDateTime(AINIRec.ReadString(sSecao, 'dContratoFim', '0'));
  assinante.NroTermPrinc := AINIRec.ReadString(sSecao, 'NroTermPrinc', '');
  assinante.cUFPrinc := AINIRec.ReadInteger(sSecao, 'cUFPrinc', 0);
end;

procedure TNFComIniReader.Ler_TermAdic(AINIRec: TMemIniFile;
  TermAdic: TTermAdicCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TTermAdicCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'TermAdic' + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'NroTermAdic', 'FIM');
    if sFim = 'FIM' then
      break;

    Item := TermAdic.New;

    Item.NroTermAdic := sFim;
    Item.cUFAdic := AINIRec.ReadInteger(sSecao, 'cUFAdic', 0);

    Inc(i);
  end;
end;

procedure TNFComIniReader.Ler_gSub(AINIRec: TMemIniFile; gSub: TgSub);
var
  sSecao: string;
begin
  sSecao := 'gSub';
  gSub.chNFCom := AINIRec.ReadString(sSecao, 'chNFCom', '');
  gSub.motSub := StrToMotSub(AINIRec.ReadString(sSecao, 'motSub', '1'));
  gSub.gNF.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  gSub.gNF.Modelo := AINIRec.ReadInteger(sSecao, 'Modelo', 0);
  gSub.gNF.Serie := AINIRec.ReadString(sSecao, 'Serie', '');
  gSub.gNF.nNF := AINIRec.ReadInteger(sSecao, 'nNF', 0);
  gSub.gNF.CompetEmis := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetEmis', '0'));
  gSub.gNF.hash115 := AINIRec.ReadString(sSecao, 'hash115', '');
end;

procedure TNFComIniReader.Ler_gCofat(AINIRec: TMemIniFile; gCofat: TgCofat);
var
  sSecao: string;
begin
  sSecao := 'gCofat';
  gCofat.chNFComLocal := AINIRec.ReadString(sSecao, 'chNFComLocal', '');
  gCofat.gNF.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  gCofat.gNF.Modelo := AINIRec.ReadInteger(sSecao, 'Modelo', 0);
  gCofat.gNF.Serie := AINIRec.ReadString(sSecao, 'Serie', '');
  gCofat.gNF.nNF := AINIRec.ReadInteger(sSecao, 'nNF', 0);
  gCofat.gNF.CompetEmis := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetEmis', '0'));
  gCofat.gNF.hash115 := AINIRec.ReadString(sSecao, 'hash115', '');
end;

procedure TNFComIniReader.Ler_Det(AINIRec: TMemIniFile; Det: TDetCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TDetCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'det' + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'nItem', 'FIM');
    if sFim = 'FIM' then
      break;

    Item := Det.New;

    Item.nItem := StrToIntDef(sFim, 0);
    Item.chNFComAnt := AINIRec.ReadString(sSecao, 'chNFComAnt', '');
    Item.nItemAnt := AINIRec.ReadInteger(sSecao, 'nItemAnt', 0);
    Item.infAdProd := AINIRec.ReadString(sSecao, 'infAdProd', '');
    Item.indNFComAntPapelFatCentral := StrToTIndicador(AINIRec.ReadString(sSecao, 'indNFComAntPapelFatCentral', '0'));

    Item.Prod.cProd := AINIRec.ReadString(sSecao, 'cProd', '');
    Item.Prod.xProd := AINIRec.ReadString(sSecao, 'xProd', '');
    Item.Prod.cClass := AINIRec.ReadString(sSecao, 'cClass', '');
    Item.Prod.CFOP := AINIRec.ReadInteger(sSecao, 'CFOP', 0);
    Item.Prod.CNPJLD := AINIRec.ReadString(sSecao, 'CNPJLD', '');
    Item.Prod.uMed:= StrTouMed(AINIRec.ReadString(sSecao, 'uMed', ''));
    Item.Prod.qFaturada := StringToFloatDef(AINIRec.ReadString(sSecao, 'qFaturada', ''), 0);
    Item.Prod.vItem := StringToFloatDef(AINIRec.ReadString(sSecao, 'vItem', ''), 0);
    Item.Prod.vDesc := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDesc', ''), 0);
    Item.Prod.vOutro := StringToFloatDef(AINIRec.ReadString(sSecao, 'vOutro', ''), 0);
    Item.Prod.vProd := StringToFloatDef(AINIRec.ReadString(sSecao, 'vProd', ''), 0);
    Item.Prod.dExpiracao := StringToDateTime(AINIRec.ReadString(sSecao, 'dExpiracao', '0'));
    Item.Prod.indDevolucao := StrToTIndicador(AINIRec.ReadString(sSecao, 'indDevolucao', '0'));

    Ler_ICMS(AINIRec, Item.Imposto, i);
    Ler_ICMSUFDest(AINIRec, Item.Imposto, i);
    Ler_PIS(AINIRec, Item.Imposto, i);
    Ler_COFINS(AINIRec, Item.Imposto, i);
    Ler_FUST(AINIRec, Item.Imposto, i);
    Ler_FUNTTEL(AINIRec, Item.Imposto, i);
    Ler_RetTrib(AINIRec, Item.Imposto, i);

    // Reforma Tributária
    Ler_IBSCBS(AINIRec, Item.Imposto.IBSCBS, i);

    Ler_gProcRef(AINIRec, Item.gProcRef, i);
    Ler_gRessarc(AINIRec, Item.gRessarc, i);

    Inc(i);
  end;
end;

procedure TNFComIniReader.Ler_ICMS(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'ICMS' + IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    Imposto.indSemCST := StrToTIndicador(AINIRec.ReadString(sSecao, 'indSemCST', '0'));

    Imposto.ICMS.CST := StrToCSTICMS(AINIRec.ReadString(sSecao, 'CST', '00'));
    Imposto.ICMS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBC', ''), 0);
    Imposto.ICMS.pICMS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMS', ''), 0);
    Imposto.ICMS.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMS', ''), 0);
    Imposto.ICMS.pFCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'pFCP', ''), 0);
    Imposto.ICMS.vFCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCP', ''), 0);
    Imposto.ICMS.pRedBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBC', ''), 0);
    Imposto.ICMS.vICMSDeson := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSDeson', ''), 0);
    Imposto.ICMS.cBenef := AINIRec.ReadString(sSecao,'cBenef', '');
  end;
end;

procedure TNFComIniReader.Ler_ICMSUFDest(AINIRec: TMemIniFile;
  Imposto: TImposto; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TICMSUFDestCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'ICMSUFDest' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'vBCUFDest', 'FIM');
    if sFim = 'FIM' then
      break;

    Item := Imposto.ICMSUFDest.New;

    Item.cUFDest := AINIRec.ReadInteger(sSecao, 'cUFDest', 0);
    Item.vBCUFDest := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCUFDest', ''), 0);
    Item.pFCPUFDest := StringToFloatDef(AINIRec.ReadString(sSecao, 'pFCPUFDest', ''), 0);
    Item.pICMSUFDest := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSUFDest', ''), 0);
    Item.pICMSInter := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSInter', ''), 0);
    Item.vFCPUFDest := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCPUFDest', ''), 0);
    Item.vICMSUFDest := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSUFDest', ''), 0);
    Item.vICMSUFEmi := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSUFEmi', ''), 0);
    Item.cBenefUFDest := AINIRec.ReadString(sSecao, 'cBenefUFDest', '');

    Inc(i);
  end;
end;

procedure TNFComIniReader.Ler_PIS(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'PIS' + IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    Imposto.PIS.CST := StrToCSTPIS(sFim);
    Imposto.PIS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
    Imposto.PIS.pPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'pPIS', ''), 0);
    Imposto.PIS.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vPIS', ''), 0);
  end;
end;

procedure TNFComIniReader.Ler_COFINS(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'COFINS'+ IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    Imposto.COFINS.CST := StrToCSTCOFINS(sFim);
    Imposto.COFINS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
    Imposto.COFINS.pCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'pCOFINS', ''), 0);
    Imposto.COFINS.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINS', ''), 0);
  end;
end;

procedure TNFComIniReader.Ler_FUST(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'FUST'+ IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'vBC', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    Imposto.FUST.vBC := StringToFloatDef(sFim, 0);
    Imposto.FUST.pFUST := StringToFloatDef(AINIRec.ReadString(sSecao,'pFUST', ''), 0);
    Imposto.FUST.vFUST := StringToFloatDef(AINIRec.ReadString(sSecao,'vFUST', ''), 0);
  end;
end;

procedure TNFComIniReader.Ler_FUNTTEL(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'FUNTTEL'+ IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'vBC', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    Imposto.FUNTTEL.vBC := StringToFloatDef(sFim, 0);
    Imposto.FUNTTEL.pFUNTTEL := StringToFloatDef(AINIRec.ReadString(sSecao,'pFUNTTEL', ''), 0);
    Imposto.FUNTTEL.vFUNTTEL := StringToFloatDef(AINIRec.ReadString(sSecao,'vFUNTTEL', ''), 0);
  end;
end;

procedure TNFComIniReader.Ler_RetTrib(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'retTrib'+ IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'vRetPIS', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    Imposto.retTrib.vRetPIS := StringToFloatDef(sFim, 0);
    Imposto.retTrib.vRetCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCOFINS', ''), 0);
    Imposto.retTrib.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCSLL', ''), 0);
    Imposto.retTrib.vBCIRRF := StringToFloatDef(AINIRec.ReadString(sSecao,'vBCIRRF', ''), 0);
    Imposto.retTrib.vIRRF := StringToFloatDef(AINIRec.ReadString(sSecao,'vIRRF', ''), 0);
  end;
end;

procedure TNFComIniReader.Ler_gProcRef(AINIRec: TMemIniFile;
  gProcRef: TgProcRef; Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'gProcRef' + IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'vItem', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    gProcRef.vItem := StringToFloatDef(AINIRec.ReadString(sSecao,'vItem', ''), 0);
    gProcRef.qFaturada := AINIRec.ReadInteger(sSecao,'qFaturada', 0);
    gProcRef.vProd := StringToFloatDef(AINIRec.ReadString(sSecao,'vProd', ''), 0);
    gProcRef.vDesc := StringToFloatDef(AINIRec.ReadString(sSecao,'vDesc', ''), 0);
    gProcRef.vOutro := StringToFloatDef(AINIRec.ReadString(sSecao,'vOutro', ''), 0);
    gProcRef.indDevolucao := StrToTIndicador(AINIRec.ReadString(sSecao, 'indDevolucao', '0'));
    gProcRef.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
    gProcRef.pICMS := StringToFloatDef(AINIRec.ReadString(sSecao,'pICMS', ''), 0);
    gProcRef.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMS', ''), 0);
    gProcRef.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vPIS', ''), 0);
    gProcRef.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINS', ''), 0);

    Ler_gProc(AINIRec, gProcRef.gProc, Idx);
  end;
end;

procedure TNFComIniReader.Ler_gProc(AINIRec: TMemIniFile;
  gProc: TgProcCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgProcCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gProc' + IntToStrZero(Idx, 3) + IntToStrZero(i, 2);
    sFim := AINIRec.ReadString(sSecao, 'nProcesso', 'FIM');
    if sFim = 'FIM' then
      break;

    Item := gProc.New;

    Item.tpProc := StrTotpProc(AINIRec.ReadString(sSecao, 'tpProc', '0'));
    Item.nProcesso := sFim;

    Inc(i);
  end;
end;

procedure TNFComIniReader.Ler_gRessarc(AINIRec: TMemIniFile;
  gRessarc: TgRessarc; Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'gRessarc' + IntToStrZero(Idx, 3);
  sFim := AINIRec.ReadString(sSecao, 'dRef', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    gRessarc.tpRessarc := StrTotpRessarc(AINIRec.ReadString(sSecao, 'tpRessarc', '0'));
    gRessarc.dRef := StringToDateTime(AINIRec.ReadString(sSecao, 'dRef', '0'));
    gRessarc.nProcesso := AINIRec.ReadString(sSecao,'nProcesso', '');
    gRessarc.nProtReclama := AINIRec.ReadString(sSecao,'nProtReclama', '');
    gRessarc.xObs := AINIRec.ReadString(sSecao,'xObs', '');
  end;
end;

procedure TNFComIniReader.Ler_Total(AINIRec: TMemIniFile; Total: TTotal);
var
  sSecao: string;
begin
  sSecao := 'total';
  Total.vProd := StringToFloatDef(AINIRec.ReadString(sSecao,'vProd', ''), 0);
  Total.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
  Total.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMS', ''), 0);
  Total.vICMSDeson := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMSDeson', ''), 0);
  Total.vFCP := StringToFloatDef(AINIRec.ReadString(sSecao,'vFCP', ''), 0);
  Total.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINS', ''), 0);
  Total.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vPIS', ''), 0);
  Total.vFUNTTEL := StringToFloatDef(AINIRec.ReadString(sSecao,'vFUNTTEL', ''), 0);
  Total.vFUST := StringToFloatDef(AINIRec.ReadString(sSecao,'vFUST', ''), 0);
  Total.vRetPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetPIS', ''), 0);
  Total.vRetCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCOFINS', ''), 0);
  Total.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCSLL', ''), 0);
  Total.vIRRF := StringToFloatDef(AINIRec.ReadString(sSecao,'vIRRF', ''), 0);
  Total.vDesc := StringToFloatDef(AINIRec.ReadString(sSecao,'vDesc', ''), 0);
  Total.vOutro := StringToFloatDef(AINIRec.ReadString(sSecao,'vOutro', ''), 0);
  Total.vNF := StringToFloatDef(AINIRec.ReadString(sSecao,'vNF', ''), 0);
  Total.vTotDFe := StringToFloatDef(AINIRec.ReadString(sSecao,'vTotDFe', ''), 0);

  // Reforma Tributária
  Ler_IBSCBSTot(AINIRec, total.IBSCBSTot);
end;

procedure TNFComIniReader.Ler_gFidelidade(AINIRec: TMemIniFile;
  gFidelidade: TgFidelidade);
var
  sSecao: string;
begin
  sSecao := 'gFidelidade';
  gFidelidade.qtdSaldoPts := AINIRec.ReadString(sSecao, 'qtdSaldoPts', '');
  gFidelidade.dRefSaldoPts := StringToDateTime(AINIRec.ReadString(sSecao, 'dRefSaldoPts', '0'));
  gFidelidade.qtdPtsResg := AINIRec.ReadString(sSecao, 'qtdPtsResg', '');
  gFidelidade.dRefResgPts := StringToDateTime(AINIRec.ReadString(sSecao, 'dRefResgPts', '0'));
end;

procedure TNFComIniReader.Ler_gFat(AINIRec: TMemIniFile; gFat: TgFat);
var
  sSecao: string;
begin
  sSecao := 'gFat';
  gFat.CompetFat := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetFat', '0'));
  gFat.dVencFat := StringToDateTime(AINIRec.ReadString(sSecao, 'dVencFat', '0'));
  gFat.dPerUsoIni := StringToDateTime(AINIRec.ReadString(sSecao, 'dPerUsoIni', '0'));
  gFat.dPerUsoFim := StringToDateTime(AINIRec.ReadString(sSecao, 'dPerUsoFim', '0'));
  gFat.codBarras := AINIRec.ReadString(sSecao, 'codBarras', '');
  gFat.codDebAuto := AINIRec.ReadString(sSecao, 'codDebAuto', '');
  gFat.codBanco := AINIRec.ReadString(sSecao, 'codBanco', '');
  gFat.codAgencia := AINIRec.ReadString(sSecao, 'codAgencia', '');
  // Endereço do Destinatario
  gFat.enderCorresp.xLgr := AINIRec.ReadString(sSecao, 'xLgr', '');
  gFat.enderCorresp.nro := AINIRec.ReadString(sSecao, 'nro', '');
  gFat.enderCorresp.xCpl := AINIRec.ReadString(sSecao, 'xCpl', '');
  gFat.enderCorresp.xBairro := AINIRec.ReadString(sSecao, 'xBairro', '');
  gFat.enderCorresp.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
  gFat.enderCorresp.xMun := AINIRec.ReadString(sSecao, 'xMun', '');
  gFat.enderCorresp.CEP := AINIRec.ReadInteger(sSecao, 'CEP', 0);
  gFat.enderCorresp.UF := AINIRec.ReadString(sSecao, 'UF', '');
  gFat.enderCorresp.fone := AINIRec.ReadString(sSecao, 'fone', '');
  gFat.enderCorresp.email := AINIRec.ReadString(sSecao, 'email', '');
  // Chave PIX
  gFat.gPIX.urlQRCodePIX := AINIRec.ReadString(sSecao, 'urlQRCodePIX', '');
end;

procedure TNFComIniReader.Ler_gFatCentral(AINIRec: TMemIniFile;
  gFatCentral: TgFatCentral);
var
  sSecao: string;
begin
  sSecao := 'gFatCentral';
  gFatCentral.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  gFatCentral.cUF := AINIRec.ReadInteger(sSecao, 'cUF', 0);
end;

procedure TNFComIniReader.Ler_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  i := 1;
  while true do
  begin
    sSecao := 'autXML' + IntToStrZero(i, 2);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    autXML.New.CNPJCPF := sFim;

    Inc(i);
  end;
end;

procedure TNFComIniReader.Ler_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';
  InfAdic.infAdFisco := AINIRec.ReadString(sSecao, 'infAdFisco', '');
  // Vai ser alterado pois é uma lista
  InfAdic.infCpl := AINIRec.ReadString(sSecao,'infCpl', '');
end;

procedure TNFComIniReader.Ler_InfRespTec(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  sSecao := 'infRespTec';
  if AINIRec.SectionExists(sSecao) then
  begin
    infRespTec.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
    infRespTec.xContato := AINIRec.ReadString(sSecao, 'xContato', '');
    infRespTec.email := AINIRec.ReadString(sSecao, 'email', '');
    infRespTec.fone := AINIRec.ReadString(sSecao, 'fone', '');
  end;
end;

// Reforma Tributária
procedure TNFComIniReader.Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'IBSCBS' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBS.CST := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CST', '000'));
    IBSCBS.cClassTrib := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTrib', '000001'));

    Ler_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS, Idx);
  end;
end;

procedure TNFComIniReader.Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCBS.vBC := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC','') ,0);
    gIBSCBS.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vIBS','') ,0);

    Ler_gIBSUF(AINIRec, gIBSCBS.gIBSUF, Idx);
    Ler_gIBSMun(AINIRec, gIBSCBS.gIBSMun, Idx);
    Ler_gCBS(AINIRec, gIBSCBS.gCBS, Idx);
    Ler_gTribReg(AINIRec, gIBSCBS.gTribRegular, Idx);
    Ler_gIBSCredPres(AINIRec, gIBSCBS.gIBSCredPres, Idx);
    Ler_gCBSCredPres(AINIRec, gIBSCBS.gCBSCredPres, Idx);
    Ler_gTribCompraGov(AINIRec, gIBSCBS.gTribCompraGov, Idx);
  end;
end;

procedure TNFComIniReader.Ler_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSUF.pIBS := StringToFloatDef( AINIRec.ReadString(sSecao,'pIBSUF','') ,0);
    gIBSUF.vIBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vIBSUF','') ,0);

    gIBSUF.gDif.pDif := StringToFloatDef( AINIRec.ReadString(sSecao,'pDif','') ,0);
    gIBSUF.gDif.vDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vDif','') ,0);

    gIBSUF.gDevTrib.vDevTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vDevTrib','') ,0);

    gIBSUF.gRed.pRedAliq := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedAliq','') ,0);
    gIBSUF.gRed.pAliqEfet := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfet','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSMun.pIBS := StringToFloatDef( AINIRec.ReadString(sSecao,'pIBSMun','') ,0);
    gIBSMun.vIBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vIBSMun','') ,0);

    gIBSMun.gDif.pDif := StringToFloatDef( AINIRec.ReadString(sSecao,'pDif','') ,0);
    gIBSMun.gDif.vDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vDif','') ,0);

    gIBSMun.gDevTrib.vDevTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vDevTrib','') ,0);

    gIBSMun.gRed.pRedAliq := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedAliq','') ,0);
    gIBSMun.gRed.pAliqEfet := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfet','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBS' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBS.pCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'pCBS','') ,0);
    gCBS.vCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vCBS','') ,0);

    gCBS.gDif.pDif := StringToFloatDef( AINIRec.ReadString(sSecao,'pDif','') ,0);
    gCBS.gDif.vDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vDif','') ,0);

    gCBS.gDevTrib.vDevTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vDevTrib','') ,0);

    gCBS.gRed.pRedAliq := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedAliq','') ,0);
    gCBS.gRed.pAliqEfet := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfet','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_gTribReg(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular' + IntToStrZero(Idx, 3);

  gTribRegular.CSTReg := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CSTReg', '000'));
  gTribRegular.cClassTribReg := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTribReg', '000001'));
  gTribRegular.pAliqEfetRegIBSUF := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfetRegIBSUF','') ,0);
  gTribRegular.vTribRegIBSUF := StringToFloatDef( AINIRec.ReadString(sSecao,'vTribRegIBSUF','') ,0);
  gTribRegular.pAliqEfetRegIBSMun := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfetRegIBSMun','') ,0);
  gTribRegular.vTribRegIBSMun := StringToFloatDef( AINIRec.ReadString(sSecao,'vTribRegIBSMun','') ,0);
  gTribRegular.pAliqEfetRegCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfetRegCBS','') ,0);
  gTribRegular.vTribRegCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vTribRegCBS','') ,0);
end;

procedure TNFComIniReader.Ler_gIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCredPres' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    gIBSCredPres.pCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'pCredPres','') ,0);
    gIBSCredPres.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gIBSCredPres.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_gCBSCredPres(AINIRec: TMemIniFile; gCBSCredPres: TgIBSCBSCredPres; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBSCredPres' + IntToStrZero(Idx, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    gCBSCredPres.pCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'pCredPres','') ,0);
    gCBSCredPres.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gCBSCredPres.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_gTribCompraGov(AINIRec: TMemIniFile;
  gTribCompraGov: TgTribCompraGov; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gTribCompraGov.pAliqIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqIBSUF','') ,0);
    gTribCompraGov.vTribIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribIBSUF','') ,0);
    gTribCompraGov.pAliqIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqIBSMun','') ,0);
    gTribCompraGov.vTribIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribIBSMun','') ,0);
    gTribCompraGov.pAliqCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqCBS','') ,0);
    gTribCompraGov.vTribCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribCBS','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_IBSCBSTot(AINIRec: TMemIniFile;
  IBSCBSTot: TIBSCBSTot);
var
  sSecao: string;
begin
  sSecao := 'IBSCBSTot';
  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBSTot.vBCIBSCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCIBSCBS','') ,0);

    Ler_IBSCBSTot_gIBS(AINIRec, IBSCBSTot.gIBS);
    Ler_IBSCBSTot_gCBS(AINIRec, IBSCBSTot.gCBS);
  end;
end;

procedure TNFComIniReader.Ler_IBSCBSTot_gIBS(AINIRec: TMemIniFile;
  gIBS: TgIBS);
var
  sSecao: string;
begin
  sSecao := 'gIBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBS.vIBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vIBS','') ,0);
    gIBS.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gIBS.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);

    Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec, gIBS.gIBSUFTot);
    Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec, gIBS.gIBSMunTot);
  end;
end;

procedure TNFComIniReader.Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile;
  gIBSUFTot: TgIBSUFTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSUFTot';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSUFTot.vDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vDif','') ,0);
    gIBSUFTot.vDevTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vDevTrib','') ,0);
    gIBSUFTot.vIBSUF := StringToFloatDef( AINIRec.ReadString(sSecao,'vIBSUF','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile;
  gIBSMunTot: TgIBSMunTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSMunTot';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSMunTot.vDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vDif','') ,0);
    gIBSMunTot.vDevTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vDevTrib','') ,0);
    gIBSMunTot.vIBSMun := StringToFloatDef( AINIRec.ReadString(sSecao,'vIBSMun','') ,0);
  end;
end;

procedure TNFComIniReader.Ler_IBSCBSTot_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBS);
var
  sSecao: string;
begin
  sSecao := 'gCBSTot';
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBS.vDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vDif','') ,0);
    gCBS.vDevTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vDevTrib','') ,0);
    gCBS.vCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vCBS','') ,0);
    gCBS.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gCBS.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

end.
