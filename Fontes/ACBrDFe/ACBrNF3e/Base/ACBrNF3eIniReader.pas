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

unit ACBrNF3eIniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrNF3eClass,
  ACBrNF3eConversao,
  ACBrDFeComum.Proc;

type
  { TNF3eIniReader }

  TNF3eIniReader = class
  private
    FNF3e: TNF3e;
    FVersaoDF: TVersaoNF3e;
    FAmbiente: Integer;
    FtpEmis: Integer;

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Ler_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
    procedure Ler_Acessante(AINIRec: TMemIniFile; acessante: Tacessante);
    procedure Ler_gSub(AINIRec: TMemIniFile; gSub: TgSub);
    procedure Ler_gJudic(AINIRec: TMemIniFile; gJudic: TgJudic);
    procedure Ler_gGrContrat(AINIRec: TMemIniFile; gGrContrat: TgGrContratCollection);
    procedure Ler_gMed(AINIRec: TMemIniFile; gMed: TgMedCollection);
    procedure Ler_gSCEE(AINIRec: TMemIniFile; gSCEE: TgSCEE);
    procedure Ler_gConsumidor(AINIRec: TMemIniFile; gConsumidor: TgConsumidorCollection);
    procedure Ler_EnerAloc(AINIRec: TMemIniFile; enerAlocLista: TenerAlocCollection; Idx: Integer);
    procedure Ler_EnerInjet(AINIRec: TMemIniFile; enerInjetLista: TenerInjetCollection; Idx: Integer);
    procedure Ler_gSaldoCred(AINIRec: TMemIniFile; gSaldoCred: TgSaldoCredCollection);
    procedure Ler_gTipoSaldo(AINIRec: TMemIniFile; gTipoSaldo: TgTipoSaldoCollection);
    procedure Ler_gTipoSaldogSaldoCred(AINIRec: TMemIniFile; gSaldoCred: TgSaldoCredCollection; Idx: Integer);
    procedure Ler_NFDet(AINIRec: TMemIniFile; NFDet: TNFDetCollection);
    procedure Ler_det(AINIRec: TMemIniFile; Det: TDetCollection; Idx: Integer);
    procedure Ler_DetItem(AINIRec: TMemIniFile; detItem: TdetItem; Idx1, Idx2: Integer);
    procedure Ler_gTarif(AINIRec: TMemIniFile; gTarif: TgTarifCollection; Idx1, Idx2: Integer);
    procedure Ler_gAdBand(AINIRec: TMemIniFile; gAdBand: TgAdBandCollection; Idx1, Idx2: Integer);
    procedure Ler_Prod(AINIRec: TMemIniFile; Prod: TProd; Idx1, Idx2: Integer);
    procedure Ler_Imposto(AINIRec: TMemIniFile; Imposto: TImposto; Idx1, Idx2: Integer);
    procedure Ler_ICMS(AINIRec: TMemIniFile; ICMS: TICMS; Idx1, Idx2: Integer);
    procedure Ler_PIS(AINIRec: TMemIniFile; PIS: TPIS; Idx1, Idx2: Integer);
    procedure Ler_PISEfet(AINIRec: TMemIniFile; PISEfet: TPISEfet; Idx1, Idx2: Integer);
    procedure Ler_COFINS(AINIRec: TMemIniFile; COFINS: TCOFINS; Idx1, Idx2: Integer);
    procedure Ler_COFINSEfet(AINIRec: TMemIniFile; COFINSEfet: TCOFINSEfet; Idx1, Idx2: Integer);
    procedure Ler_RetTrib(AINIRec: TMemIniFile; retTrib: TretTrib; Idx1, Idx2: Integer);
    procedure Ler_gProcRef(AINIRec: TMemIniFile; gProcRef: TgProcRef; Idx1, Idx2: Integer);
    procedure Ler_gProc(AINIRec: TMemIniFile; gProc: TgProcCollection; Idx1, Idx2: Integer);
    procedure Ler_gContab(AINIRec: TMemIniFile; gContab: TgContabCollection; Idx1, Idx2: Integer);
    procedure Ler_Total(AINIRec: TMemIniFile; Total: TTotal);
    procedure Ler_gFat(AINIRec: TMemIniFile; gFat: TgFat);
    procedure Ler_gANEEL(AINIRec: TMemIniFile; gANEEL: TgANEEL);
    procedure Ler_gHistFat(AINIRec: TMemIniFile; gHistFat: TgHistFatCollection);
    procedure Ler_gGrandFat(AINIRec: TMemIniFile; gGrandFat: TgGrandFatCollection; Idx: Integer);
    procedure Ler_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Ler_InfRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Ler_InfNF3eSupl(AINIRec: TMemIniFile; infNF3eSupl: TinfNF3eSupl);
    procedure Ler_ProcessamentoNF3e(AINIRec: TMemIniFile; procNF3e: TProcDFe);

    // Reforma Tributária
    procedure Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx1, Idx2: Integer);
    procedure Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS; Idx1, Idx2: Integer);
    procedure Ler_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores; Idx1, Idx2: Integer);
    procedure Ler_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores; Idx1, Idx2: Integer);
    procedure Ler_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores; Idx1, Idx2: Integer);

    procedure Ler_gTribReg(AINIRec: TMemIniFile; gTribRegular: TgTribRegular; Idx1, Idx2: Integer);
    procedure Ler_gIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres; Idx1, Idx2: Integer);
    procedure Ler_gCBSCredPres(AINIRec: TMemIniFile; gCBSCredPres: TgIBSCBSCredPres; Idx1, Idx2: Integer);
    procedure Ler_gTribCompraGov(AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov; Idx1, Idx2: Integer);

    procedure Ler_IBSCBSTot(AINIRec: TMemIniFile; IBSCBSTot: TIBSCBSTot);
    procedure Ler_IBSCBSTot_gIBS(AINIRec: TMemIniFile; gIBS: TgIBS);
    procedure Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile; gIBSUFTot: TgIBSUFTot);
    procedure Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile; gIBSMunTot: TgIBSMunTot);
    procedure Ler_IBSCBSTot_gCBS(AINIRec: TMemIniFile; gCBS: TgCBS);
  public
    constructor Create(AOwner: TNF3e); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property NF3e: TNF3e read FNF3e write FNF3e;
    property VersaoDF: TVersaoNF3e read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrDFe.Conversao,
  ACBrNF3e,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TNF3eIniReader }

constructor TNF3eIniReader.Create(AOwner: TNF3e);
begin
  inherited Create;

  FNF3e := AOwner;
end;

function TNF3eIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FNF3e.infNF3e.versao := StringToFloatDef(INIRec.ReadString('infNF3e', 'versao', VersaoNF3eToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FNF3e.Ide);
    Ler_Emitente(INIRec, FNF3e.Emit);

    FNF3e.ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(FNF3e.Emit.enderEmit.UF));
    FNF3e.Ide.cMunFG := INIRec.ReadInteger('ide', 'cMunFG', FNF3e.Emit.EnderEmit.cMun);

    Ler_Destinatario(INIRec, FNF3e.Dest);
    Ler_Acessante(INIRec, FNF3e.acessante);
    Ler_gSub(INIRec, FNF3e.gSub);
    Ler_gJudic(INIRec, FNF3e.gJudic);
    Ler_gGrContrat(INIRec, FNF3e.gGrContrat);
    Ler_gMed(INIRec, FNF3e.gMed);
    Ler_gSCEE(INIRec, FNF3e.gSCEE);
    Ler_gConsumidor(INIRec, FNF3e.gSCEE.gConsumidor);
    Ler_gSaldoCred(INIRec, FNF3e.gSCEE.gSaldoCred);
    Ler_gTipoSaldo(INIRec, FNF3e.gSCEE.gTipoSaldo);
    Ler_NFDet(INIRec, FNF3e.NFDet);
    Ler_Total(INIRec, FNF3e.Total);
    Ler_gFat(INIRec, FNF3e.gFat);
    Ler_gANEEL(INIRec, FNF3e.gANEEL);
    Ler_AutorizadosXml(INIRec, FNF3e.autXML);
    Ler_InfAdic(INIRec, FNF3e.InfAdic);
    Ler_InfRespTec(INIRec, FNF3e.infRespTec);
    Ler_InfNF3eSupl(INIRec, FNF3e.infNF3eSupl);

    Ler_ProcessamentoNF3e(INIRec, FNF3e.procNF3e);
  finally
    INIRec.Free;
  end;
end;

procedure TNF3eIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'ide';
  Ide.tpAmb   := StrToTipoAmbiente(OK, AINIRec.ReadString(sSecao, 'tpAmb', IntToStr(Ambiente)));
  Ide.modelo := AINIRec.ReadInteger(sSecao, 'Modelo', 62);
  Ide.serie := AINIRec.ReadInteger(sSecao, 'Serie', 1);
  Ide.nNF := AINIRec.ReadInteger(sSecao, 'nNF', 0);
  Ide.cNF := AINIRec.ReadInteger(sSecao, 'cNF', 0);
  Ide.dhEmi := StringToDateTime(AINIRec.ReadString(sSecao, 'dhEmi', '0'));
  Ide.tpEmis  := StrToTipoEmissao(OK, AINIRec.ReadString(sSecao, 'tpEmis', IntToStr(tpEmis)));
  Ide.nSiteAutoriz := StrToSiteAutorizator(AINIRec.ReadString(sSecao, 'nSiteAutoriz', '0'));
  Ide.finNF3e := StrToFinNF3e(AINIRec.ReadString(sSecao, 'finNF3e', '0'));
  Ide.verProc := AINIRec.ReadString(sSecao, 'verProc', 'ACBrNFCom');
  Ide.dhCont := StringToDateTime(AINIRec.ReadString(sSecao, 'dhCont', '0'));
  Ide.xJust := AINIRec.ReadString(sSecao, 'xJust', '');

  // Reforma Tributária
  Ide.gCompraGov.pRedutor := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedutor', ''), 0);

  if Ide.gCompraGov.pRedutor > 0 then
    Ide.gCompraGov.tpEnteGov := StrTotpEnteGov(AINIRec.ReadString(sSecao, 'tpEnteGov', ''));
end;

procedure TNF3eIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  sSecao: string;
begin
  sSecao := 'emit';
  Emit.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  Emit.IE := AINIRec.ReadString(sSecao, 'IE', '');
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

procedure TNF3eIniReader.Ler_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
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
  Dest.cNIS := AINIRec.ReadString(sSecao, 'cNIS', '');
  Dest.NB := AINIRec.ReadString(sSecao, 'NB', '');
  Dest.xNomeAdicional := AINIRec.ReadString(sSecao, 'xNomeAdicional', '');
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

procedure TNF3eIniReader.Ler_Acessante(AINIRec: TMemIniFile;
  acessante: Tacessante);
var
  sSecao: string;
begin
  sSecao := 'acessante';
  acessante.idAcesso := AINIRec.ReadString(sSecao, 'idAcesso', '');
  acessante.idCodCliente := AINIRec.ReadString(sSecao, 'idCodCliente', '');
  acessante.tpAcesso := StrTotpAcesso(AINIRec.ReadString(sSecao, 'tpAcesso', '0'));
  acessante.xNomeUC := AINIRec.ReadString(sSecao, 'xNomeUC', '');
  acessante.tpClasse := StrTotpClasse(AINIRec.ReadString(sSecao, 'tpClasse', '01'));
  acessante.tpSubClasse := StrTotpSubClasse(AINIRec.ReadString(sSecao, 'tpSubClasse', '01'));
  acessante.tpFase := StrTotpFase(AINIRec.ReadString(sSecao, 'tpFase', '1'));
  acessante.tpGrpTensao := StrTotpGrpTensao(AINIRec.ReadString(sSecao, 'tpGrpTensao', '01'));
  acessante.tpModTar := StrTotpModTar(AINIRec.ReadString(sSecao, 'tpModTar', '01'));
  acessante.latGPS := AINIRec.ReadString(sSecao, 'latGPS', '');
  acessante.longGPS := AINIRec.ReadString(sSecao, 'longGPS', '');
  acessante.codRoteiroLeitura := AINIRec.ReadString(sSecao, 'codRoteiroLeitura', '');
end;

procedure TNF3eIniReader.Ler_gSub(AINIRec: TMemIniFile; gSub: TgSub);
var
  sSecao: string;
begin
  sSecao := 'gSub';
  gSub.chNF3e := AINIRec.ReadString(sSecao, 'chNF3e', '');
  gSub.motSub := StrToMotSub(AINIRec.ReadString(sSecao, 'motSub', '1'));
  gSub.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  gSub.Serie := AINIRec.ReadString(sSecao, 'Serie', '');
  gSub.nNF := AINIRec.ReadInteger(sSecao, 'nNF', 0);
  gSub.CompetEmis := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetEmis', '0'));
  gSub.CompetApur := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetApur', '0'));
  gSub.hash115 := AINIRec.ReadString(sSecao, 'hash115', '');
end;

procedure TNF3eIniReader.Ler_gJudic(AINIRec: TMemIniFile; gJudic: TgJudic);
var
  sSecao: string;
begin
  sSecao := 'gJudic';
  gJudic.chNF3e := AINIRec.ReadString(sSecao, 'chNF3e', '');
end;

procedure TNF3eIniReader.Ler_gGrContrat(AINIRec: TMemIniFile;
  gGrContrat: TgGrContratCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgGrContratCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gGrContrat' + IntToStrZero(i, 2);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'qUnidContrat', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := gGrContrat.New;

    Item.nContrat := AINIRec.ReadInteger(sSecao, 'nContrat', 0);
    Item.tpGrContrat := StrTotpGrContrat(AINIRec.ReadString(sSecao, 'tpGrContrat', '1'));
    Item.tpPosTar := StrTotpPosTar(AINIRec.ReadString(sSecao, 'tpPosTar', '0'));
    Item.qUnidContrat := StringToFloatDef(sFim, 0);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gMed(AINIRec: TMemIniFile; gMed: TgMedCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgMedCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gMed' + IntToStrZero(i, 2);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'idMedidor', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := gMed.New;

    Item.nMed := AINIRec.ReadInteger(sSecao, 'nMed', 0);
    Item.idMedidor := sFim;
    Item.dMedAnt := StringToDateTime(AINIRec.ReadString(sSecao, 'dMedAnt', '0'));
    Item.dMedAtu := StringToDateTime(AINIRec.ReadString(sSecao, 'dMedAtu', '0'));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gSCEE(AINIRec: TMemIniFile; gSCEE: TgSCEE);
var
  sSecao: string;
begin
  sSecao := 'gSCEE';
  gSCEE.tpPartComp := StrTotpPartComp(AINIRec.ReadString(sSecao, 'tpPartComp', '1'));
end;

procedure TNF3eIniReader.Ler_gConsumidor(AINIRec: TMemIniFile;
  gConsumidor: TgConsumidorCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgConsumidorCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gConsumidor' + IntToStrZero(i, 3);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'vPotInst', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := gConsumidor.New;

    Item.idAcessGer := AINIRec.ReadString(sSecao, 'idAcessGer', '');
    Item.vPotInst := StringToFloatDef(sFim, 0);
    Item.tpFonteEnergia := StrTotpFonteEnergia(AINIRec.ReadString(sSecao, 'tpFonteEnergia', ''));

    Ler_EnerAloc(AINIRec, Item.enerAlocLista, i);
    Ler_EnerInjet(AINIRec, Item.enerInjetLista, i);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_EnerAloc(AINIRec: TMemIniFile;
  enerAlocLista: TenerAlocCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TenerAlocCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'EnerAloc' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'enerAloc', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := enerAlocLista.New;

    Item.enerAloc := StringToFloatDef(AINIRec.ReadString(sSecao, 'enerAloc', ''), 0);
    Item.tpPosTar := StrTotpPosTar(AINIRec.ReadString(sSecao, 'tpPosTar', ''));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_EnerInjet(AINIRec: TMemIniFile;
  enerInjetLista: TenerInjetCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TenerInjetCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'EnerInjet' + IntToStrZero(Idx, 3) + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'enerInjet', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := enerInjetLista.New;

    Item.enerInjet := StringToFloatDef(AINIRec.ReadString(sSecao, 'enerInjet', ''), 0);
    Item.tpPosTarInjet := StrTotpPosTar(AINIRec.ReadString(sSecao, 'tpPosTarInjet', ''));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gSaldoCred(AINIRec: TMemIniFile;
  gSaldoCred: TgSaldoCredCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgSaldoCredCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gSaldoCred' + IntToStrZero(i, 1);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'vSaldAnt', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := gSaldoCred.New;

    Item.tpPosTar := StrTotpPosTar(AINIRec.ReadString(sSecao, 'tpPosTar', ''));
    Item.vSaldAnt := StringToFloatDef(sFim, 0);
    Item.vCredExpirado := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredExpirado', ''), 0);
    Item.vSaldAtual := StringToFloatDef(AINIRec.ReadString(sSecao, 'vSaldAtual', ''), 0);
    Item.vCredExpirar := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredExpirar', ''), 0);
    Item.CompetExpirar := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetExpirar', '0'));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gTipoSaldo(AINIRec: TMemIniFile;
  gTipoSaldo: TgTipoSaldoCollection);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgTipoSaldoCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gTipoSaldo' + IntToStrZero(i, 2);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'nTipoSaldo', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := gTipoSaldo.New;

    Ler_gTipoSaldogSaldoCred(AINIRec, Item.gSaldoCred, I);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gTipoSaldogSaldoCred(AINIRec: TMemIniFile;
  gSaldoCred: TgSaldoCredCollection; Idx: Integer);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TgSaldoCredCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gSaldoCred' + IntToStrZero(Idx, 2) + IntToStrZero(i, 1);
    sFim := OnlyNumber(AINIRec.ReadString(sSecao, 'vSaldAnt', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    Item := gSaldoCred.New;

    Item.tpPosTar := StrTotpPosTar(AINIRec.ReadString(sSecao, 'tpPosTar', ''));
    Item.vSaldAnt := StringToFloatDef(sFim, 0);
    Item.vCredExpirado := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredExpirado', ''), 0);
    Item.vSaldAtual := StringToFloatDef(AINIRec.ReadString(sSecao, 'vSaldAtual', ''), 0);
    Item.vCredExpirar := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredExpirar', ''), 0);
    Item.CompetExpirar := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetExpirar', '0'));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_NFDet(AINIRec: TMemIniFile;
  NFDet: TNFDetCollection);
var
  i: Integer;
  sSecao: string;
  Item: TNFDetCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'NFdet' + IntToStrZero(i, 2);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := NFDet.New;

    Item.chNF3eAnt := AINIRec.ReadString(sSecao, 'chNF3eAnt', '');
    Item.mod6HashAnt := AINIRec.ReadString(sSecao, 'mod6HashAnt', '');

    Ler_det(AINIRec, Item.Det, I);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_det(AINIRec: TMemIniFile; Det: TDetCollection;
  Idx: Integer);
var
  i: Integer;
  sSecao: string;
  Item: TDetCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'Det' + IntToStrZero(Idx, 2) + IntToStrZero(i, 3);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := Det.New;

    Item.nItem := AINIRec.ReadInteger(sSecao, 'nItem', 0);

    Item.gAjusteNF3eAnt.tpAjuste:= StrTotpAjuste(AINIRec.ReadString(sSecao, 'tpAjuste', '0'));
    Item.gAjusteNF3eAnt.motAjuste:= StrToMotAjuste(AINIRec.ReadString(sSecao, 'motAjuste', '1'));

    Item.detItemAnt.nItemAnt := AINIRec.ReadInteger(sSecao, 'nItemAnt', 0);
    Item.detItemAnt.vItem := StringToFloatDef(AINIRec.ReadString(sSecao, 'vItem', ''), 0);
    Item.detItemAnt.qFaturada := StringToFloatDef(AINIRec.ReadString(sSecao, 'qFaturada', ''), 0);
    Item.detItemAnt.vProd := StringToFloatDef(AINIRec.ReadString(sSecao, 'vProd', ''), 0);
    Item.detItemAnt.cClass := AINIRec.ReadInteger(sSecao, 'cClass', 0);
    Item.detItemAnt.vBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBC', ''), 0);
    Item.detItemAnt.pICMS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMS', ''), 0);
    Item.detItemAnt.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMS', ''), 0);
    Item.detItemAnt.vFCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCP', ''), 0);
    Item.detItemAnt.vBCST := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCST', ''), 0);
    Item.detItemAnt.vICMSST := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSST', ''), 0);
    Item.detItemAnt.vFCPST := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCPST', ''), 0);
    Item.detItemAnt.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vPIS', ''), 0);
    Item.detItemAnt.vPISEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vPISEfet', ''), 0);
    Item.detItemAnt.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCOFINS', ''), 0);
    Item.detItemAnt.vCOFINSEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCOFINSEfet', ''), 0);
    Item.detItemAnt.indDevolucao := StrToTIndicador(AINIRec.ReadString(sSecao, 'indDevolucao', '0'));

    Item.detItemAnt.retTrib.vRetPIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetPIS', ''), 0);
    Item.detItemAnt.retTrib.vRetCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetCOFINS', ''), 0);
    Item.detItemAnt.retTrib.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetCSLL', ''), 0);
    Item.detItemAnt.retTrib.vBCIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCIRRF', ''), 0);
    Item.detItemAnt.retTrib.vIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIRRF', ''), 0);

    Ler_DetItem(AINIRec, Item.detItem, Idx, I);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_DetItem(AINIRec: TMemIniFile; detItem: TdetItem;
  Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'detItem' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);

  detItem.nItemAnt := AINIRec.ReadInteger(sSecao, 'nItemAnt', 0);
  detItem.infAdProd := AINIRec.ReadString(sSecao, 'infAdProd', '');

  Ler_gTarif(AINIRec, detItem.gTarif, Idx1, Idx2);
  Ler_gAdBand(AINIRec, detItem.gAdBand, Idx1, Idx2);
  Ler_Prod(AINIRec, detItem.Prod, Idx1, Idx2);
  Ler_Imposto(AINIRec, detItem.Imposto, Idx1, Idx2);
  Ler_gProcRef(AINIRec, detItem.gProcRef, Idx1, Idx2);
  Ler_gContab(AINIRec, detItem.gContab, Idx1, Idx2);
end;

procedure TNF3eIniReader.Ler_gTarif(AINIRec: TMemIniFile;
  gTarif: TgTarifCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
  Item: TgTarifCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gTarif' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3) + IntToStrZero(i, 1);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := gTarif.New;

    Item.dIniTarif := StringToDateTime(AINIRec.ReadString(sSecao, 'dIniTarif', '0'));
    Item.dFimTarif := StringToDateTime(AINIRec.ReadString(sSecao, 'dFimTarif', '0'));
    Item.tpAto := StrTotpAto(AINIRec.ReadString(sSecao, 'tpAto', ''));
    Item.nAto := AINIRec.ReadString(sSecao, 'nAto', '');
    Item.anoAto := AINIRec.ReadInteger(sSecao, 'anoAto', 0);
    Item.tpTarif := StrTotpTarif(AINIRec.ReadString(sSecao, 'tpTarif', ''));
    Item.cPosTarif := StrTocPosTarif(AINIRec.ReadString(sSecao, 'cPosTarif', ''));
    Item.uMed := StrTouMed(AINIRec.ReadString(sSecao, 'uMed', ''));
    Item.vTarifHom := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTarifHom', ''), 0);
    Item.vTarifAplic := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTarifAplic', ''), 0);
    Item.motDifTarif := StrTomotDifTarif(AINIRec.ReadString(sSecao, 'motDifTarif', ''));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gAdBand(AINIRec: TMemIniFile;
  gAdBand: TgAdBandCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
  Item: TgAdBandCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gAdBand' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3) + IntToStrZero(i, 1);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := gAdBand.New;

    Item.dIniAdBand := StringToDateTime(AINIRec.ReadString(sSecao, 'dIniAdBand', '0'));
    Item.dFimAdBand := StringToDateTime(AINIRec.ReadString(sSecao, 'dFimAdBand', '0'));
    Item.tpBand := StrTotpBand(AINIRec.ReadString(sSecao, 'tpBand', ''));
    Item.vAdBand := StringToFloatDef(AINIRec.ReadString(sSecao, 'vAdBand', ''), 0);
    Item.vAdBandAplic := StringToFloatDef(AINIRec.ReadString(sSecao, 'vAdBandAplic', ''), 0);
    Item.motDifBand := StrTomotDifBand(AINIRec.ReadString(sSecao, 'motDifBand', ''));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_Prod(AINIRec: TMemIniFile; Prod: TProd; Idx1,
  Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'Prod' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    Prod.indOrigemQtd := StrToindOrigemQtd(AINIRec.ReadString(sSecao, 'indOrigemQtd', ''));
    Prod.cProd := AINIRec.ReadString(sSecao, 'cProd', '');
    Prod.xProd := AINIRec.ReadString(sSecao, 'xProd', '');
    Prod.cClass := AINIRec.ReadInteger(sSecao, 'cClass', 0);
    Prod.CFOP := AINIRec.ReadInteger(sSecao, 'CFOP', 0);
    Prod.uMed := StrTouMedFat(AINIRec.ReadString(sSecao, 'uMedProd', ''));
    Prod.qFaturada := AINIRec.ReadInteger(sSecao, 'qFaturada', 0);
    Prod.vItem := StringToFloatDef(AINIRec.ReadString(sSecao, 'vItem', ''), 0);
    Prod.vProd := StringToFloatDef(AINIRec.ReadString(sSecao, 'vProd', ''), 0);
    Prod.indDevolucao := StrToTIndicador(AINIRec.ReadString(sSecao, 'indDevolucao', ''));
    Prod.indPrecoACL := StrToTIndicador(AINIRec.ReadString(sSecao, 'indPrecoACL', ''));

    Prod.gMedicao.nMed := AINIRec.ReadInteger(sSecao, 'nMed', 0);
    Prod.gMedicao.nContrat := AINIRec.ReadInteger(sSecao, 'nContrat', 0);
    Prod.gMedicao.tpGrMed := StrTotpGrMed(AINIRec.ReadString(sSecao, 'tpGrMed', ''));
    Prod.gMedicao.cPosTarif := StrTocPosTarif(AINIRec.ReadString(sSecao, 'cPosTarif', ''));
    Prod.gMedicao.uMed := StrTouMedFat(AINIRec.ReadString(sSecao, 'uMedMedicao', ''));
    Prod.gMedicao.vMedAnt := StringToFloatDef(AINIRec.ReadString(sSecao, 'vMedAnt', ''), 0);
    Prod.gMedicao.vMedAtu := StringToFloatDef(AINIRec.ReadString(sSecao, 'vMedAtu', ''), 0);
    Prod.gMedicao.vConst := StringToFloatDef(AINIRec.ReadString(sSecao, 'vConst', ''), 0);
    Prod.gMedicao.vMed := StringToFloatDef(AINIRec.ReadString(sSecao, 'vMed', ''), 0);
    Prod.gMedicao.pPerdaTran := StringToFloatDef(AINIRec.ReadString(sSecao, 'pPerdaTran', ''), 0);
    Prod.gMedicao.vMedPerdaTran := StringToFloatDef(AINIRec.ReadString(sSecao, 'vMedPerdaTran', ''), 0);
    Prod.gMedicao.vMedPerdaTec := StringToFloatDef(AINIRec.ReadString(sSecao, 'vMedPerdaTec', ''), 0);
    Prod.gMedicao.tpMotNaoLeitura := StrTotpMotNaoLeitura(AINIRec.ReadString(sSecao, 'tpMotNaoLeitura', ''));
  end;
end;

procedure TNF3eIniReader.Ler_Imposto(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx1, Idx2: Integer);
begin
  Ler_ICMS(AINIRec, Imposto.ICMS, Idx1, Idx2);
  Ler_PIS(AINIRec, Imposto.PIS, Idx1, Idx2);
  Ler_PISEfet(AINIRec, Imposto.PISEfet, Idx1, Idx2);
  Ler_COFINS(AINIRec, Imposto.COFINS, Idx1, Idx2);
  Ler_COFINSEfet(AINIRec, Imposto.COFINSEfet, Idx1, Idx2);
  Ler_RetTrib(AINIRec, Imposto.retTrib, Idx1, Idx2);
  // Reforma Tributária
  Ler_IBSCBS(AINIRec, Imposto.IBSCBS, Idx1, Idx2);
end;

procedure TNF3eIniReader.Ler_ICMS(AINIRec: TMemIniFile; ICMS: TICMS; Idx1,
  Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'ICMS' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    ICMS.indSemCST := StrToTIndicador(AINIRec.ReadString(sSecao, 'indSemCST', '0'));

    ICMS.CST := StrToCSTICMS(AINIRec.ReadString(sSecao, 'CST', '00'));
    ICMS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBC', ''), 0);
    ICMS.pICMS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMS', ''), 0);
    ICMS.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMS', ''), 0);
    ICMS.pFCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'pFCP', ''), 0);
    ICMS.vFCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCP', ''), 0);
    ICMS.vBCST := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCST', ''), 0);
    ICMS.pICMSST := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSST', ''), 0);
    ICMS.vICMSST := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSST', ''), 0);
    ICMS.pFCPST := StringToFloatDef(AINIRec.ReadString(sSecao, 'pFCPST', ''), 0);
    ICMS.vFCPST := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCPST', ''), 0);
    ICMS.pRedBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBC', ''), 0);
    ICMS.vICMSDeson := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSDeson', ''), 0);
    ICMS.cBenef := AINIRec.ReadString(sSecao,'cBenef', '');
    ICMS.vBCSTRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCSTRet', ''), 0);
    ICMS.pICMSSTRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSSTRet', ''), 0);
    ICMS.vICMSSubstituto := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSSubstituto', ''), 0);
    ICMS.vICMSSTRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSSTRet', ''), 0);
    ICMS.vBCFCPSTRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCFCPSTRet', ''), 0);
    ICMS.pFCPSTRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pFCPSTRet', ''), 0);
    ICMS.vFCPSTRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCPSTRet', ''), 0);
    ICMS.pRedBCEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBCEfet', ''), 0);
    ICMS.vBCEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCEfet', ''), 0);
    ICMS.pICMSEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSEfet', ''), 0);
    ICMS.vICMSEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSEfet', ''), 0);
  end;
end;

procedure TNF3eIniReader.Ler_PIS(AINIRec: TMemIniFile; PIS: TPIS; Idx1,
  Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'PIS' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    PIS.CST := StrToCSTPIS(sFim);
    PIS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
    PIS.pPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'pPIS', ''), 0);
    PIS.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vPIS', ''), 0);
  end;
end;

procedure TNF3eIniReader.Ler_PISEfet(AINIRec: TMemIniFile; PISEfet: TPISEfet;
  Idx1, Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'PISEfet' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'vBCPISEfet', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    PISEfet.vBCPISEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vBCPISEfet', ''), 0);
    PISEfet.pPISEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'pPISEfet', ''), 0);
    PISEfet.vPISEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vPISEfet', ''), 0);
  end;
end;

procedure TNF3eIniReader.Ler_COFINS(AINIRec: TMemIniFile; COFINS: TCOFINS; Idx1,
  Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'COFINS' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    COFINS.CST := StrToCSTCOFINS(sFim);
    COFINS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
    COFINS.pCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'pCOFINS', ''), 0);
    COFINS.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINS', ''), 0);
  end;
end;

procedure TNF3eIniReader.Ler_COFINSEfet(AINIRec: TMemIniFile;
  COFINSEfet: TCOFINSEfet; Idx1, Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'COFINSEfet' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'vBCCOFINSEfet', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    COFINSEfet.vBCCOFINSEfet := StringToFloatDef(sFim, 0);
    COFINSEfet.pCOFINSEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'pCOFINSEfet', ''), 0);
    COFINSEfet.vCOFINSEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINSEfet', ''), 0);
  end;
end;

procedure TNF3eIniReader.Ler_RetTrib(AINIRec: TMemIniFile; retTrib: TretTrib;
  Idx1, Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'retTrib' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'vRetPIS', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    retTrib.vRetPIS := StringToFloatDef(sFim, 0);
    retTrib.vRetCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCOFINS', ''), 0);
    retTrib.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCSLL', ''), 0);
    retTrib.vBCIRRF := StringToFloatDef(AINIRec.ReadString(sSecao,'vBCIRRF', ''), 0);
    retTrib.vIRRF := StringToFloatDef(AINIRec.ReadString(sSecao,'vIRRF', ''), 0);
  end;
end;

procedure TNF3eIniReader.Ler_gProcRef(AINIRec: TMemIniFile; gProcRef: TgProcRef;
  Idx1, Idx2: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'gProcRef' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  sFim := AINIRec.ReadString(sSecao, 'vItem', 'FIM');

  if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
  begin
    gProcRef.vItem := StringToFloatDef(sFim, 0);
    gProcRef.qFaturada := AINIRec.ReadInteger(sSecao,'qFaturada', 0);
    gProcRef.vProd := StringToFloatDef(AINIRec.ReadString(sSecao,'vProd', ''), 0);
    gProcRef.indDevolucao := StrToTIndicador(AINIRec.ReadString(sSecao,'indDevolucao', ''));
    gProcRef.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
    gProcRef.pICMS := StringToFloatDef(AINIRec.ReadString(sSecao,'pICMS', ''), 0);
    gProcRef.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMS', ''), 0);
    gProcRef.pFCP := StringToFloatDef(AINIRec.ReadString(sSecao,'pFCP', ''), 0);
    gProcRef.vFCP := StringToFloatDef(AINIRec.ReadString(sSecao,'vFCP', ''), 0);
    gProcRef.vBCST := StringToFloatDef(AINIRec.ReadString(sSecao,'vBCST', ''), 0);
    gProcRef.pICMSST := StringToFloatDef(AINIRec.ReadString(sSecao,'pICMSST', ''), 0);
    gProcRef.vICMSST := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMSST', ''), 0);
    gProcRef.pFCPST := StringToFloatDef(AINIRec.ReadString(sSecao,'pFCPST', ''), 0);
    gProcRef.vFCPST := StringToFloatDef(AINIRec.ReadString(sSecao,'vFCPST', ''), 0);
    gProcRef.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vPIS', ''), 0);
    gProcRef.vPISEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vPISEfet', ''), 0);
    gProcRef.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINS', ''), 0);
    gProcRef.vCOFINSEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINSEfet', ''), 0);

    Ler_gProc(AINIRec, gProcRef.gProc, Idx1, Idx2);
  end;
end;

procedure TNF3eIniReader.Ler_gProc(AINIRec: TMemIniFile;
  gProc: TgProcCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
  Item: TgProcCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gProc' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3) + IntToStrZero(i, 1);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := gProc.New;

    Item.tpProc := StrTotpProc(AINIRec.ReadString(sSecao, 'tpProc', ''));
    Item.nProcesso := AINIRec.ReadString(sSecao, 'nProcesso', '');

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gContab(AINIRec: TMemIniFile;
  gContab: TgContabCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
  Item: TgContabCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gContab' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3) + IntToStrZero(i, 1);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := gContab.New;

    Item.cContab := AINIRec.ReadString(sSecao, 'cContab', '');
    Item.xContab := AINIRec.ReadString(sSecao, 'xContab', '');
    Item.vContab := StringToFloatDef(AINIRec.ReadString(sSecao,'vContab', ''), 0);
    Item.tpLanc := StrTotpLanc(AINIRec.ReadString(sSecao, 'tpLanc', ''));

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_Total(AINIRec: TMemIniFile; Total: TTotal);
var
  sSecao: string;
begin
  sSecao := 'total';
  Total.vProd := StringToFloatDef(AINIRec.ReadString(sSecao,'vProd', ''), 0);
  Total.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC', ''), 0);
  Total.vICMS := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMS', ''), 0);
  Total.vICMSDeson := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMSDeson', ''), 0);
  Total.vFCP := StringToFloatDef(AINIRec.ReadString(sSecao,'vFCP', ''), 0);
  Total.vBCST := StringToFloatDef(AINIRec.ReadString(sSecao,'vBCST', ''), 0);
  Total.vST := StringToFloatDef(AINIRec.ReadString(sSecao,'vST', ''), 0);
  Total.vFCPST := StringToFloatDef(AINIRec.ReadString(sSecao,'vFCPST', ''), 0);
  Total.vRetPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetPIS', ''), 0);
  Total.vRetCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCOFINS', ''), 0);
  Total.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao,'vRetCSLL', ''), 0);
  Total.vIRRF := StringToFloatDef(AINIRec.ReadString(sSecao,'vIRRF', ''), 0);
  Total.vCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINS', ''), 0);
  Total.vCOFINSEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vCOFINSEfet', ''), 0);
  Total.vPIS := StringToFloatDef(AINIRec.ReadString(sSecao,'vPIS', ''), 0);
  Total.vPISEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'vPISEfet', ''), 0);
  Total.vNF := StringToFloatDef(AINIRec.ReadString(sSecao,'vNF', ''), 0);
  Total.vTotDFe := StringToFloatDef(AINIRec.ReadString(sSecao,'vTotDFe', ''), 0);

  // Reforma Tributária
  Ler_IBSCBSTot(AINIRec, total.IBSCBSTot);
end;

procedure TNF3eIniReader.Ler_gFat(AINIRec: TMemIniFile; gFat: TgFat);
var
  sSecao: string;
begin
  sSecao := 'gFat';
  gFat.CompetFat := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetFat', '0'));
  gFat.dVencFat := StringToDateTime(AINIRec.ReadString(sSecao, 'dVencFat', '0'));
  gFat.dApresFat := StringToDateTime(AINIRec.ReadString(sSecao, 'dApresFat', '0'));
  gFat.dProxLeitura := StringToDateTime(AINIRec.ReadString(sSecao, 'dProxLeitura', '0'));
  gFat.nFat := AINIRec.ReadString(sSecao, 'nFat', '');
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

procedure TNF3eIniReader.Ler_gANEEL(AINIRec: TMemIniFile; gANEEL: TgANEEL);
begin
  Ler_gHistFat(AINIRec, gANEEL.gHistFat);
end;

procedure TNF3eIniReader.Ler_gHistFat(AINIRec: TMemIniFile;
  gHistFat: TgHistFatCollection);
var
  i: Integer;
  sSecao: string;
  Item: TgHistFatCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gHistFat' + IntToStrZero(i, 1);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := gHistFat.New;

    Item.xGrandFat := AINIRec.ReadString(sSecao, 'xGrandFat', '');

    Ler_gGrandFat(AINIRec, Item.gGrandFat, i);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_gGrandFat(AINIRec: TMemIniFile;
  gGrandFat: TgGrandFatCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
  Item: TgGrandFatCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'gGrandFat' + IntToStrZero(Idx, 1) + IntToStrZero(i, 2);
    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := gGrandFat.New;

    Item.CompetFat := StringToDateTime(AINIRec.ReadString(sSecao, 'CompetFat', '0'));
    Item.vFat := StringToFloatDef(AINIRec.ReadString(sSecao,'vFat', ''), 0);
    Item.uMed := StrTouMedFat(AINIRec.ReadString(sSecao, 'uMed', ''));
    Item.qtdDias := AINIRec.ReadInteger(sSecao,'qtdDias', 0);

    Inc(i);
  end;
end;

procedure TNF3eIniReader.Ler_AutorizadosXml(AINIRec: TMemIniFile;
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

procedure TNF3eIniReader.Ler_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';
  InfAdic.infAdFisco := AINIRec.ReadString(sSecao, 'infAdFisco', '');
  // Vai ser alterado pois é uma lista
  InfAdic.infCpl := AINIRec.ReadString(sSecao,'infCpl', '');
end;

procedure TNF3eIniReader.Ler_InfRespTec(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  sSecao := 'infRespTec';
  if AINIRec.SectionExists(sSecao) then
  begin
    with infRespTec do
    begin
      CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
      xContato := AINIRec.ReadString(sSecao, 'xContato', '');
      email := AINIRec.ReadString(sSecao, 'email', '');
      fone := AINIRec.ReadString(sSecao, 'fone', '');
    end;
  end;
end;

procedure TNF3eIniReader.Ler_InfNF3eSupl(AINIRec: TMemIniFile;
  infNF3eSupl: TinfNF3eSupl);
var
  sSecao: string;
begin
  sSecao := 'infNF3eSupl';
  if AINIRec.SectionExists(sSecao) then
  begin
    infNF3eSupl.qrCodNF3e := AINIRec.ReadString(sSecao, 'qrCodNF3e', '');
  end;
end;

procedure TNF3eIniReader.Ler_ProcessamentoNF3e(AINIRec: TMemIniFile;
  procNF3e: TProcDFe);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'procNF3e';
  if AINIRec.SectionExists(sSecao) then
  begin
    procNF3e.tpAmb := StrToTipoAmbiente(ok, AINIRec.ReadString(sSecao, 'tpAmb', ''));
    procNF3e.verAplic := AINIRec.ReadString(sSecao, 'verAplic', '');
    procNF3e.chDFe := AINIRec.ReadString(sSecao, 'chNF3e', '');
    procNF3e.dhRecbto := AINIRec.ReadDateTime(sSecao, 'dhRecbto', 0);
    procNF3e.nProt := AINIRec.ReadString(sSecao, 'nProt', '');
    procNF3e.digVal := AINIRec.ReadString(sSecao, 'digVal', '');
    procNF3e.cStat := AINIRec.ReadInteger(sSecao, 'cStat', 0);
    procNF3e.xMotivo := AINIRec.ReadString(sSecao, 'xMotivo', '');
    procNF3e.cMsg := AINIRec.ReadInteger(sSecao, 'cMsg', 0);
    procNF3e.xMsg := AINIRec.ReadString(sSecao, 'xMsg', '');
  end;
end;

// Reforma Tributária
procedure TNF3eIniReader.Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'IBSCBS' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBS.CST := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CST', '000'));
    IBSCBS.cClassTrib := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTrib', '000001'));

    Ler_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS, Idx1, Idx2);
  end;
end;

procedure TNF3eIniReader.Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCBS.vBC := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC','') ,0);
    gIBSCBS.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vIBS','') ,0);

    Ler_gIBSUF(AINIRec, gIBSCBS.gIBSUF, Idx1, Idx2);
    Ler_gIBSMun(AINIRec, gIBSCBS.gIBSMun, Idx1, Idx2);
    Ler_gCBS(AINIRec, gIBSCBS.gCBS, Idx1, Idx2);
    Ler_gTribReg(AINIRec, gIBSCBS.gTribRegular, Idx1, Idx2);
    Ler_gIBSCredPres(AINIRec, gIBSCBS.gIBSCredPres, Idx1, Idx2);
    Ler_gCBSCredPres(AINIRec, gIBSCBS.gCBSCredPres, Idx1, Idx2);
    Ler_gTribCompraGov(AINIRec, gIBSCBS.gTribCompraGov, Idx1, Idx2);
  end;
end;

procedure TNF3eIniReader.Ler_gIBSUF(AINIRec: TMemIniFile;
  gIBSUF: TgIBSUFValores; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
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

procedure TNF3eIniReader.Ler_gIBSMun(AINIRec: TMemIniFile;
  gIBSMun: TgIBSMunValores; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
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

procedure TNF3eIniReader.Ler_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBS' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
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

procedure TNF3eIniReader.Ler_gTribReg(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);

  gTribRegular.CSTReg := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CSTReg', '000'));
  gTribRegular.cClassTribReg := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTribReg', '000001'));
  gTribRegular.pAliqEfetRegIBSUF := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfetRegIBSUF','') ,0);
  gTribRegular.vTribRegIBSUF := StringToFloatDef( AINIRec.ReadString(sSecao,'vTribRegIBSUF','') ,0);
  gTribRegular.pAliqEfetRegIBSMun := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfetRegIBSMun','') ,0);
  gTribRegular.vTribRegIBSMun := StringToFloatDef( AINIRec.ReadString(sSecao,'vTribRegIBSMun','') ,0);
  gTribRegular.pAliqEfetRegCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'pAliqEfetRegCBS','') ,0);
  gTribRegular.vTribRegCBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vTribRegCBS','') ,0);
end;

procedure TNF3eIniReader.Ler_gIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCredPres' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    gIBSCredPres.pCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'pCredPres','') ,0);
    gIBSCredPres.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gIBSCredPres.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

procedure TNF3eIniReader.Ler_gCBSCredPres(AINIRec: TMemIniFile; gCBSCredPres: TgIBSCBSCredPres; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBSCredPres' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    gCBSCredPres.pCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'pCredPres','') ,0);
    gCBSCredPres.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gCBSCredPres.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

procedure TNF3eIniReader.Ler_gTribCompraGov(AINIRec: TMemIniFile;
  gTribCompraGov: TgTribCompraGov; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov' + IntToStrZero(Idx1, 2) + IntToStrZero(Idx2, 3);

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

procedure TNF3eIniReader.Ler_IBSCBSTot(AINIRec: TMemIniFile;
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

procedure TNF3eIniReader.Ler_IBSCBSTot_gIBS(AINIRec: TMemIniFile;
  gIBS: TgIBS);
var
  sSecao: string;
begin
  sSecao := 'gIBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBS.vCredPres := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gIBS.vCredPresCondSus := StringToFloatDef( AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
    gIBS.vIBS := StringToFloatDef( AINIRec.ReadString(sSecao,'vIBS','') ,0);

    Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec, gIBS.gIBSUFTot);
    Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec, gIBS.gIBSMunTot);
  end;
end;

procedure TNF3eIniReader.Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile;
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

procedure TNF3eIniReader.Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile;
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

procedure TNF3eIniReader.Ler_IBSCBSTot_gCBS(AINIRec: TMemIniFile;
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
