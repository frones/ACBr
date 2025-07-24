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

unit ACBrNFComIniWriter;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrDFeComum.Proc,
  ACBrNFComClass,
  ACBrNFComConversao;

type
  { TNFComIniWriter }

  TNFComIniWriter = class
  private
    FNFCom: TNFCom;

    procedure Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Gerar_Assinante(AINIRec: TMemIniFile; assinante: Tassinante);
    procedure Gerar_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
    procedure Gerar_TermAdic(AINIRec: TMemIniFile; TermAdic: TTermAdicCollection);
    procedure Gerar_gSub(AINIRec: TMemIniFile; gSub: TgSub);
    procedure Gerar_gCofat(AINIRec: TMemIniFile; gCofat: TgCofat);
    procedure Gerar_Det(AINIRec: TMemIniFile; Det: TDetCollection);
    procedure Gerar_ICMS(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_ICMSUFDest(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_PIS(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_COFINS(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_FUST(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_FUNTTEL(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_RetTrib(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_gProcRef(AINIRec: TMemIniFile; gProcRef: TgProcRef; Idx: Integer);
    procedure Gerar_gProc(AINIRec: TMemIniFile; gProc: TgProcCollection; Idx: Integer);
    procedure Gerar_gRessarc(AINIRec: TMemIniFile; gRessarc: TgRessarc; Idx: Integer);
    procedure Gerar_Total(AINIRec: TMemIniFile; Total: TTotal);
    procedure Gerar_gFidelidade(AINIRec: TMemIniFile; gFidelidade: TgFidelidade);
    procedure Gerar_gFat(AINIRec: TMemIniFile; gFat: TgFat);
    procedure Gerar_gFatCentral(AINIRec: TMemIniFile; gFatCentral: TgFatCentral);
    procedure Gerar_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Gerar_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Gerar_InfRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Gerar_ProcNFCom(AINIRec: TMemIniFile; procNFCom: TProcDFe);

    // Reforma Tributária
    procedure Gerar_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS; Idx: Integer);

    procedure Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores; Idx: Integer);

    procedure Gerar_IBSCBS_gIBSCBS_gTribReg(AINIRec: TMemIniFile; gTribRegular: TgTribRegular;
      Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres;
      const Grupo: string; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov;
      Idx: Integer);

    procedure Gerar_IBSCBSTot(AINIRec: TMemIniFile; IBSCBSTot: TIBSCBSTot);
    procedure Gerar_IBSCBSTot_gIBS(AINIRec: TMemIniFile; gIBS: TgIBS);
    procedure Gerar_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile; gIBSUFTot: TgIBSUFTot);
    procedure Gerar_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile; gIBSMunTot: TgIBSMunTot);

    procedure Gerar_IBSCBSTot_gCBS(AINIRec: TMemIniFile; gCBS: TgCBS);
  public
    constructor Create(AOwner: TNFCom); reintroduce;

    function GravarIni: string;

    property NFCom: TNFCom read FNFCom write FNFCom;
  end;


implementation

uses
  ACBrXmlBase,
  ACBrDFeUtil,
  ACBrDFe.Conversao,
  ACBrNFCom,
  ACBrUtil.Base;

{ TNFComIniWriter }

constructor TNFComIniWriter.Create(AOwner: TNFCom);
begin
  inherited Create;

  FNFCom := AOwner;
end;

function TNFComIniWriter.GravarIni: string;
var
  INIRec: TMemIniFile;
  IniNFCom: TStringList;
begin
  Result := '';

  if not ValidarChave(FNFCom.infNFCom.ID) then
    raise EACBrNFComException.Create('NFCom Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    INIRec.WriteString('infNFCom', 'ID', FNFCom.infNFCom.ID);
    INIRec.WriteString('infNFCom', 'versao', VersaoNFComToStr(DblToVersaoNFCom(FNFCom.infNFCom.versao)));

    Gerar_Identificacao(INIRec, FNFCom.Ide);
    Gerar_Emitente(INIRec, FNFCom.Emit);
    Gerar_Destinatario(INIRec, FNFCom.Dest);
    Gerar_Assinante(INIRec, FNFCom.assinante);
    Gerar_TermAdic(INIRec, FNFCom.assinante.TermAdic);
    Gerar_gSub(INIRec, FNFCom.gSub);
    Gerar_gCofat(INIRec, FNFCom.gCofat);
    Gerar_Det(INIRec, FNFCom.Det);
    Gerar_Total(INIRec, FNFCom.Total);
    Gerar_gFidelidade(INIRec, FNFCom.gFidelidade);
    Gerar_gFat(INIRec, FNFCom.gFat);
    Gerar_gFatCentral(INIRec, FNFCom.gFatCentral);
    Gerar_AutorizadosXml(INIRec, FNFCom.autXML);
    Gerar_InfAdic(INIRec, FNFCom.InfAdic);
    Gerar_InfRespTec(INIRec, FNFCom.infRespTec);
    Gerar_ProcNFCom(INIRec, FNFCom.procNFCom);

    IniNFCom := TStringList.Create;
    try
      INIRec.GetStrings(IniNFCom);
      Result := StringReplace(IniNFCom.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniNFCom.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

procedure TNFComIniWriter.Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
begin
  sSecao := 'ide';
  AINIRec.WriteInteger(sSecao, 'cUF', Ide.cUF);
  AINIRec.WriteString(sSecao, 'tpAmb', TipoAmbienteToStr(Ide.tpAmb));
  AINIRec.WriteInteger(sSecao, 'Modelo', Ide.modelo);
  AINIRec.WriteInteger(sSecao, 'Serie', Ide.serie);
  AINIRec.WriteInteger(sSecao, 'nNF', Ide.nNF);
  AINIRec.WriteInteger(sSecao, 'cNF', Ide.cNF);
  AINIRec.WriteString(sSecao, 'dhEmi', DateTimeToStr(Ide.dhEmi));
  AINIRec.WriteString(sSecao, 'tpEmis', TipoEmissaoToStr(Ide.tpEmis));
  AINIRec.WriteString(sSecao, 'nSiteAutoriz', SiteAutorizadorToStr(Ide.nSiteAutoriz));
  AINIRec.WriteInteger(sSecao, 'cMunFG', Ide.cMunFG);
  AINIRec.WriteString(sSecao, 'finNFCom', FinNFComToStr(Ide.finNFCom));
  AINIRec.WriteString(sSecao, 'tpFat', TipoFaturamentoToStr(Ide.tpFat));
  AINIRec.WriteString(sSecao, 'verProc', Ide.verProc);
  AINIRec.WriteString(sSecao, 'indPrePago', TIndicadorToStr(Ide.indPrePago));
  AINIRec.WriteString(sSecao, 'indCessaoMeiosRede', TIndicadorToStr(Ide.indCessaoMeiosRede));
  AINIRec.WriteString(sSecao, 'indNotaEntrada', TIndicadorToStr(Ide.indNotaEntrada));
  AINIRec.WriteString(sSecao, 'dhCont', DateToStr(Ide.dhCont));
  AINIRec.WriteString(sSecao, 'xJust', Ide.xJust);

  // Reforma Tritutaria
  if Ide.gCompraGov.pRedutor > 0 then
  begin
    AINIRec.WriteString(sSecao, 'tpEnteGov', tpEnteGovToStr(Ide.gCompraGov.tpEnteGov));
    AINIRec.WriteFloat(sSecao, 'pRedutor', Ide.gCompraGov.pRedutor);
  end;
end;

procedure TNFComIniWriter.Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  sSecao: string;
begin
  sSecao := 'emit';
  AINIRec.WriteString(sSecao, 'CNPJ', Emit.CNPJ);
  AINIRec.WriteString(sSecao, 'IE', Emit.IE);
  AINIRec.WriteString(sSecao, 'IEUFDest', Emit.IEUFDest);
  AINIRec.WriteString(sSecao, 'CRT', CRTToStr(Emit.CRT));
  AINIRec.WriteString(sSecao, 'xNome', Emit.xNome);
  AINIRec.WriteString(sSecao, 'xFant', Emit.xFant);
  // Endereço do Emitente
  AINIRec.WriteString(sSecao, 'xLgr', Emit.EnderEmit.xLgr);
  AINIRec.WriteString(sSecao, 'nro', Emit.EnderEmit.nro);
  AINIRec.WriteString(sSecao, 'xCpl', Emit.EnderEmit.xCpl);
  AINIRec.WriteString(sSecao, 'xBairro', Emit.EnderEmit.xBairro);
  AINIRec.WriteInteger(sSecao, 'cMun', Emit.EnderEmit.cMun);
  AINIRec.WriteString(sSecao, 'xMun', Emit.EnderEmit.xMun);
  AINIRec.WriteInteger(sSecao, 'CEP', Emit.EnderEmit.CEP);
  AINIRec.WriteString(sSecao, 'UF', Emit.EnderEmit.UF);
  AINIRec.WriteString(sSecao, 'fone', Emit.EnderEmit.fone);
  AINIRec.WriteString(sSecao, 'email', Emit.EnderEmit.email);
end;

procedure TNFComIniWriter.Gerar_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
var
  sSecao: string;
begin
  sSecao := 'dest';
  AINIRec.WriteString(sSecao, 'xNome', Dest.xNome);
  AINIRec.WriteString(sSecao, 'CNPJCPF', Dest.CNPJCPF);
  AINIRec.WriteString(sSecao, 'idOutros', Dest.idOutros);
  AINIRec.WriteString(sSecao, 'indIEDest', indIEDestToStr(Dest.indIEDest));
  AINIRec.WriteString(sSecao, 'IE', Dest.IE);
  AINIRec.WriteString(sSecao, 'IM', Dest.IM);
  // Endereço do Destinatario
  AINIRec.WriteString(sSecao, 'xLgr', Dest.EnderDest.xLgr);
  AINIRec.WriteString(sSecao, 'nro', Dest.EnderDest.nro);
  AINIRec.WriteString(sSecao, 'xCpl', Dest.EnderDest.xCpl);
  AINIRec.WriteString(sSecao, 'xBairro', Dest.EnderDest.xBairro);
  AINIRec.WriteInteger(sSecao, 'cMun', Dest.EnderDest.cMun);
  AINIRec.WriteString(sSecao, 'xMun', Dest.EnderDest.xMun);
  AINIRec.WriteInteger(sSecao, 'CEP', Dest.EnderDest.CEP);
  AINIRec.WriteString(sSecao, 'UF', Dest.EnderDest.UF);
  AINIRec.WriteString(sSecao, 'fone', Dest.EnderDest.fone);
  AINIRec.WriteString(sSecao, 'email', Dest.EnderDest.email);
end;

procedure TNFComIniWriter.Gerar_Assinante(AINIRec: TMemIniFile;
  assinante: Tassinante);
var
  sSecao: string;
begin
  sSecao := 'assinante';
  AINIRec.WriteString(sSecao, 'iCodAssinante', assinante.iCodAssinante);
  AINIRec.WriteString(sSecao, 'tpAssinante', tpAssinanteToStr(assinante.tpAssinante));
  AINIRec.WriteString(sSecao, 'tpServUtil', tpServUtilToStr(assinante.tpServUtil));
  AINIRec.WriteString(sSecao, 'nContrato', assinante.nContrato);
  AINIRec.WriteString(sSecao, 'dContratoIni', DateTimeToStr(assinante.dContratoIni));
  AINIRec.WriteString(sSecao, 'dContratoFim', DateTimeToStr(assinante.dContratoFim));
  AINIRec.WriteString(sSecao, 'NroTermPrinc', assinante.NroTermPrinc);
  AINIRec.WriteInteger(sSecao, 'cUFPrinc', assinante.cUFPrinc);
end;

procedure TNFComIniWriter.Gerar_TermAdic(AINIRec: TMemIniFile;
  TermAdic: TTermAdicCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to TermAdic.Count - 1 do
  begin
    sSecao := 'TermAdic' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'NroTermAdic', TermAdic.Items[i].NroTermAdic);
    AINIRec.WriteInteger(sSecao, 'cUFAdic', TermAdic.Items[i].cUFAdic);
  end;
end;

procedure TNFComIniWriter.Gerar_gSub(AINIRec: TMemIniFile; gSub: TgSub);
var
  sSecao: string;
begin
  sSecao := 'gSub';
  AINIRec.WriteString(sSecao, 'chNFCom', gSub.chNFCom);
  AINIRec.WriteString(sSecao, 'motSub', MotSubToStr(gSub.motSub));
  AINIRec.WriteString(sSecao, 'CNPJ', gSub.gNF.CNPJ);
  AINIRec.WriteInteger(sSecao, 'Modelo', gSub.gNF.Modelo);
  AINIRec.WriteString(sSecao, 'Serie', gSub.gNF.Serie);
  AINIRec.WriteInteger(sSecao, 'nNF', gSub.gNF.nNF);
  AINIRec.WriteString(sSecao, 'CompetEmis', DateTimeToStr(gSub.gNF.CompetEmis));
  AINIRec.WriteString(sSecao, 'hash115', gSub.gNF.hash115);
end;

procedure TNFComIniWriter.Gerar_gCofat(AINIRec: TMemIniFile; gCofat: TgCofat);
var
  sSecao: string;
begin
  sSecao := 'gCofat';
  AINIRec.WriteString(sSecao, 'chNFComLocal', gCofat.chNFComLocal);
  AINIRec.WriteString(sSecao, 'CNPJ', gCofat.gNF.CNPJ);
  AINIRec.WriteInteger(sSecao, 'Modelo', gCofat.gNF.Modelo);
  AINIRec.WriteString(sSecao, 'Serie', gCofat.gNF.Serie);
  AINIRec.WriteInteger(sSecao, 'nNF', gCofat.gNF.nNF);
  AINIRec.WriteString(sSecao, 'CompetEmis', DateTimeToStr(gCofat.gNF.CompetEmis));
  AINIRec.WriteString(sSecao, 'hash115', gCofat.gNF.hash115);
end;

procedure TNFComIniWriter.Gerar_Det(AINIRec: TMemIniFile; Det: TDetCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to Det.Count - 1 do
  begin
    sSecao := 'det' + IntToStrZero(i + 1, 3);

    AINIRec.WriteInteger(sSecao, 'nItem', Det[i].nItem);
    AINIRec.WriteString(sSecao, 'chNFComAnt', Det[i].chNFComAnt);
    AINIRec.WriteInteger(sSecao, 'nItemAnt', Det[i].nItemAnt);
    AINIRec.WriteString(sSecao, 'infAdProd', Det[i].infAdProd);
    AINIRec.WriteString(sSecao, 'indNFComAntPapelFatCentral',TIndicadorToStr(Det[i].indNFComAntPapelFatCentral));

    // Informações do produto
    AINIRec.WriteString(sSecao, 'cProd', Det[i].Prod.cProd);
    AINIRec.WriteString(sSecao, 'xProd', Det[i].Prod.xProd);
    AINIRec.WriteString(sSecao, 'cClass', Det[i].Prod.cClass);
    AINIRec.WriteInteger(sSecao, 'CFOP', Det[i].Prod.CFOP);
    AINIRec.WriteString(sSecao, 'CNPJLD', Det[i].Prod.CNPJLD);
    AINIRec.WriteString(sSecao, 'uMed', uMedToStr(Det[i].Prod.uMed));
    AINIRec.WriteFloat(sSecao, 'qFaturada', Det[i].Prod.qFaturada);
    AINIRec.WriteFloat(sSecao, 'vItem', Det[i].Prod.vItem);
    AINIRec.WriteFloat(sSecao, 'vDesc', Det[i].Prod.vDesc);
    AINIRec.WriteFloat(sSecao, 'vOutro', Det[i].Prod.vOutro);
    AINIRec.WriteFloat(sSecao, 'vProd', Det[i].Prod.vProd);
    AINIRec.WriteString(sSecao, 'dExpiracao', DateTimeToStr(Det[i].Prod.dExpiracao));
    AINIRec.WriteString(sSecao, 'indDevolucao', TIndicadorToStr(Det[i].Prod.indDevolucao));

    Gerar_ICMS(AINIRec, Det[i].Imposto, i);
    Gerar_ICMSUFDest(AINIRec, Det[i].Imposto, i);
    Gerar_PIS(AINIRec, Det[i].Imposto, i);
    Gerar_COFINS(AINIRec, Det[i].Imposto, i);
    Gerar_FUST(AINIRec, Det[i].Imposto, i);
    Gerar_FUNTTEL(AINIRec, Det[i].Imposto, i);
    Gerar_RetTrib(AINIRec, Det[i].Imposto, i);

    // Reforma Tributária
    Gerar_IBSCBS(AINIRec, Det[i].Imposto.IBSCBS, i);

    Gerar_gProcRef(AINIRec, Det[i].gProcRef, i);
    Gerar_gProc(AINIRec, Det[i].gProcRef.gProc, i);
    Gerar_gRessarc(AINIRec, Det[i].gRessarc, i);
  end;
end;

procedure TNFComIniWriter.Gerar_ICMS(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'ICMS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'indSemCST', TIndicadorToStr(Imposto.indSemCST));

  AINIRec.WriteString(sSecao, 'CST', CSTICMSToStr(Imposto.ICMS.CST));
  AINIRec.WriteFloat(sSecao, 'vBC', Imposto.ICMS.vBC);
  AINIRec.WriteFloat(sSecao, 'pICMS', Imposto.ICMS.pICMS);
  AINIRec.WriteFloat(sSecao, 'vICMS', Imposto.ICMS.vICMS);
  AINIRec.WriteFloat(sSecao, 'pFCP', Imposto.ICMS.pFCP);
  AINIRec.WriteFloat(sSecao, 'vFCP', Imposto.ICMS.vFCP);
  AINIRec.WriteFloat(sSecao, 'pRedBC', Imposto.ICMS.pRedBC);
  AINIRec.WriteFloat(sSecao, 'vICMSDeson', Imposto.ICMS.vICMSDeson);
  AINIRec.WriteString(sSecao, 'cBenef', Imposto.ICMS.cBenef);
end;

procedure TNFComIniWriter.Gerar_ICMSUFDest(AINIRec: TMemIniFile;
  Imposto: TImposto; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to Imposto.ICMSUFDest.Count - 1 do
  begin
    sSecao := 'ICMSUFDest' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteInteger(sSecao, 'cUFDest', Imposto.ICMSUFDest[i].cUFDest);
    AINIRec.WriteFloat(sSecao, 'vBCUFDest', Imposto.ICMSUFDest[i].vBCUFDest);
    AINIRec.WriteFloat(sSecao, 'pFCPUFDest', Imposto.ICMSUFDest[i].pFCPUFDest);
    AINIRec.WriteFloat(sSecao, 'pICMSUFDest', Imposto.ICMSUFDest[i].pICMSUFDest);
    AINIRec.WriteFloat(sSecao, 'pICMSInter', Imposto.ICMSUFDest[i].pICMSInter);
    AINIRec.WriteFloat(sSecao, 'vFCPUFDest', Imposto.ICMSUFDest[i].vFCPUFDest);
    AINIRec.WriteFloat(sSecao, 'vICMSUFDest', Imposto.ICMSUFDest[i].vICMSUFDest);
    AINIRec.WriteFloat(sSecao, 'vICMSUFEmi', Imposto.ICMSUFDest[i].vICMSUFEmi);
    AINIRec.WriteString(sSecao, 'cBenefUFDest', Imposto.ICMSUFDest[i].cBenefUFDest);
  end;
end;

procedure TNFComIniWriter.Gerar_PIS(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'PIS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'CST', CSTPISToStr(Imposto.PIS.CST));
  AINIRec.WriteFloat(sSecao, 'vBC', Imposto.PIS.vBC);
  AINIRec.WriteFloat(sSecao, 'pPIS', Imposto.PIS.pPIS);
  AINIRec.WriteFloat(sSecao, 'vPIS', Imposto.PIS.vPIS);
end;

procedure TNFComIniWriter.Gerar_COFINS(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'COFINS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'CST', CSTCOFINSToStr(Imposto.COFINS.CST));
  AINIRec.WriteFloat(sSecao, 'vBC', Imposto.COFINS.vBC);
  AINIRec.WriteFloat(sSecao, 'pCOFINS', Imposto.COFINS.pCOFINS);
  AINIRec.WriteFloat(sSecao, 'vCOFINS', Imposto.COFINS.vCOFINS);
end;

procedure TNFComIniWriter.Gerar_FUST(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'FUST' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'vBC', Imposto.FUST.vBC);
  AINIRec.WriteFloat(sSecao, 'pFUST', Imposto.FUST.pFUST);
  AINIRec.WriteFloat(sSecao, 'vFUST', Imposto.FUST.vFUST);
end;

procedure TNFComIniWriter.Gerar_FUNTTEL(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'FUNTTEL' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'vBC', Imposto.FUNTTEL.vBC);
  AINIRec.WriteFloat(sSecao, 'pFUNTTEL', Imposto.FUNTTEL.pFUNTTEL);
  AINIRec.WriteFloat(sSecao, 'vFUNTTEL', Imposto.FUNTTEL.vFUNTTEL);
end;

procedure TNFComIniWriter.Gerar_RetTrib(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'retTrib' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'vRetPIS', Imposto.retTrib.vRetPIS);
  AINIRec.WriteFloat(sSecao, 'vRetCOFINS', Imposto.retTrib.vRetCOFINS);
  AINIRec.WriteFloat(sSecao, 'vRetCSLL', Imposto.retTrib.vRetCSLL);
  AINIRec.WriteFloat(sSecao, 'vBCIRRF', Imposto.retTrib.vBCIRRF);
  AINIRec.WriteFloat(sSecao, 'vIRRF', Imposto.retTrib.vIRRF);
end;

procedure TNFComIniWriter.Gerar_gProcRef(AINIRec: TMemIniFile;
  gProcRef: TgProcRef; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gProcRef' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'vItem', gProcRef.vItem);
  AINIRec.WriteInteger(sSecao, 'qFaturada', gProcRef.qFaturada);
  AINIRec.WriteFloat(sSecao, 'vProd', gProcRef.vProd);
  AINIRec.WriteFloat(sSecao, 'vDesc', gProcRef.vDesc);
  AINIRec.WriteFloat(sSecao, 'vOutro', gProcRef.vOutro);
  AINIRec.WriteString(sSecao, 'indDevolucao', TIndicadorToStr(gProcRef.indDevolucao));
  AINIRec.WriteFloat(sSecao, 'vBC', gProcRef.vBC);
  AINIRec.WriteFloat(sSecao, 'pICMS', gProcRef.pICMS);
  AINIRec.WriteFloat(sSecao, 'vICMS', gProcRef.vICMS);
  AINIRec.WriteFloat(sSecao, 'vPIS', gProcRef.vPIS);
  AINIRec.WriteFloat(sSecao, 'vCOFINS', gProcRef.vCOFINS);
end;

procedure TNFComIniWriter.Gerar_gProc(AINIRec: TMemIniFile;
  gProc: TgProcCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to gProc.Count - 1 do
  begin
    sSecao := 'gProc' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 2);

    AINIRec.WriteString(sSecao, 'tpProc', tpProcToStr(gProc[i].tpProc));
    AINIRec.WriteString(sSecao, 'nProcesso', gProc[i].nProcesso);
  end;
end;

procedure TNFComIniWriter.Gerar_gRessarc(AINIRec: TMemIniFile;
  gRessarc: TgRessarc; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gRessarc' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'tpRessarc', tpRessarcToStr(gRessarc.tpRessarc));
  AINIRec.WriteString(sSecao, 'dRef', DateTimeToStr(gRessarc.dRef));
  AINIRec.WriteString(sSecao, 'nProcesso', gRessarc.nProcesso);
  AINIRec.WriteString(sSecao, 'nProtReclama', gRessarc.nProtReclama);
  AINIRec.WriteString(sSecao, 'xObs', gRessarc.xObs);
end;

procedure TNFComIniWriter.Gerar_Total(AINIRec: TMemIniFile; Total: TTotal);
var
  sSecao: string;
begin
  sSecao := 'total';
  AINIRec.WriteFloat(sSecao, 'vProd', Total.vProd);
  AINIRec.WriteFloat(sSecao, 'vBC', Total.vBC);
  AINIRec.WriteFloat(sSecao, 'vICMS', Total.vICMS);
  AINIRec.WriteFloat(sSecao, 'vICMSDeson', Total.vICMSDeson);
  AINIRec.WriteFloat(sSecao, 'vFCP', Total.vFCP);
  AINIRec.WriteFloat(sSecao, 'vCOFINS', Total.vCOFINS);
  AINIRec.WriteFloat(sSecao, 'vPIS', Total.vPIS);
  AINIRec.WriteFloat(sSecao, 'vFUNTTEL', Total.vFUNTTEL);
  AINIRec.WriteFloat(sSecao, 'vFUST', Total.vFUST);
  AINIRec.WriteFloat(sSecao, 'vRetPIS', Total.vRetPIS);
  AINIRec.WriteFloat(sSecao, 'vRetCOFINS', Total.vRetCOFINS);
  AINIRec.WriteFloat(sSecao, 'vRetCSLL', Total.vRetCSLL);
  AINIRec.WriteFloat(sSecao, 'vIRRF', Total.vIRRF);
  AINIRec.WriteFloat(sSecao, 'vDesc', Total.vDesc);
  AINIRec.WriteFloat(sSecao, 'vOutro', Total.vOutro);
  AINIRec.WriteFloat(sSecao, 'vNF', Total.vNF);
  AINIRec.WriteFloat(sSecao, 'vTotDFe', Total.vTotDFe);

  // Reforma Tributária
  if NFCom.Total.IBSCBSTot.vBCIBSCBS > 0 then
    Gerar_IBSCBSTot(AINIRec, NFCom.Total.IBSCBSTot);
end;

procedure TNFComIniWriter.Gerar_gFidelidade(AINIRec: TMemIniFile;
  gFidelidade: TgFidelidade);
var
  sSecao: string;
begin
  sSecao := 'gFidelidade';
  AINIRec.WriteString(sSecao, 'qtdSaldoPts', gFidelidade.qtdSaldoPts);
  AINIRec.WriteString(sSecao, 'dRefSaldoPts', DateTimeToStr(gFidelidade.dRefSaldoPts));
  AINIRec.WriteString(sSecao, 'qtdPtsResg', gFidelidade.qtdPtsResg);
  AINIRec.WriteString(sSecao, 'dRefResgPts', DateTimeToStr(gFidelidade.dRefResgPts));
end;

procedure TNFComIniWriter.Gerar_gFat(AINIRec: TMemIniFile; gFat: TgFat);
var
  sSecao: string;
begin
  sSecao := 'gFat';
  AINIRec.WriteString(sSecao, 'CompetFat', DateTimeToStr(gFat.CompetFat));
  AINIRec.WriteString(sSecao, 'dVencFat', DateTimeToStr(gFat.dVencFat));
  AINIRec.WriteString(sSecao, 'dPerUsoIni', DateTimeToStr(gFat.dPerUsoIni));
  AINIRec.WriteString(sSecao, 'dPerUsoFim', DateTimeToStr(gFat.dPerUsoFim));
  AINIRec.WriteString(sSecao, 'codBarras', gFat.codBarras);
  AINIRec.WriteString(sSecao, 'codDebAuto', gFat.codDebAuto);
  AINIRec.WriteString(sSecao, 'codBanco', gFat.codBanco);
  AINIRec.WriteString(sSecao, 'codAgencia', gFat.codAgencia);
  // Endereço do Destinatario
  AINIRec.WriteString(sSecao, 'xLgr', gFat.enderCorresp.xLgr);
  AINIRec.WriteString(sSecao, 'nro', gFat.enderCorresp.nro);
  AINIRec.WriteString(sSecao, 'xCpl', gFat.enderCorresp.xCpl);
  AINIRec.WriteString(sSecao, 'xBairro', gFat.enderCorresp.xBairro);
  AINIRec.WriteInteger(sSecao, 'cMun', gFat.enderCorresp.cMun);
  AINIRec.WriteString(sSecao, 'xMun', gFat.enderCorresp.xMun);
  AINIRec.WriteInteger(sSecao, 'CEP', gFat.enderCorresp.CEP);
  AINIRec.WriteString(sSecao, 'UF', gFat.enderCorresp.UF);
  AINIRec.WriteString(sSecao, 'fone', gFat.enderCorresp.fone);
  AINIRec.WriteString(sSecao, 'email', gFat.enderCorresp.email);
  // Chave PIX
  AINIRec.WriteString(sSecao, 'urlQRCodePIX', gFat.gPIX.urlQRCodePIX);
end;

procedure TNFComIniWriter.Gerar_gFatCentral(AINIRec: TMemIniFile;
  gFatCentral: TgFatCentral);
var
  sSecao: string;
begin
  sSecao := 'gFatCentral';
  AINIRec.WriteString(sSecao, 'CNPJ', gFatCentral.CNPJ);
  AINIRec.WriteInteger(sSecao, 'cUF', gFatCentral.cUF);
end;

procedure TNFComIniWriter.Gerar_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to autXML.Count - 1 do
  begin
    sSecao := 'autXML' + IntToStrZero(i + 1, 2);

    AINIRec.WriteString(sSecao, 'CNPJCPF', autXML[i].CNPJCPF);
  end;
end;

procedure TNFComIniWriter.Gerar_InfAdic(AINIRec: TMemIniFile;
  InfAdic: TInfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';
  AINIRec.WriteString(sSecao, 'infAdFisco', InfAdic.infAdFisco);
  // Vai ser alterado pois é uma lista
  AINIRec.WriteString(sSecao, 'infCpl', InfAdic.infCpl);
end;

procedure TNFComIniWriter.Gerar_InfRespTec(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  sSecao := 'infRespTec';
  AINIRec.WriteString(sSecao, 'CNPJ', infRespTec.CNPJ);
  AINIRec.WriteString(sSecao, 'xContato', infRespTec.xContato);
  AINIRec.WriteString(sSecao, 'email', infRespTec.email);
  AINIRec.WriteString(sSecao, 'fone', infRespTec.fone);
end;

procedure TNFComIniWriter.Gerar_ProcNFCom(AINIRec: TMemIniFile;
  procNFCom: TProcDFe);
var
  sSecao: string;
begin
  sSecao := 'procNFCom';
  AINIRec.WriteString(sSecao, 'tpAmb', TipoAmbienteToStr(procNFCom.tpAmb));
  AINIRec.WriteString(sSecao, 'verAplic', procNFCom.verAplic);
  AINIRec.WriteString(sSecao, 'chNFCom', procNFCom.chDFe);
  AINIRec.WriteString(sSecao, 'dhRecbto', DateTimeToStr(procNFCom.dhRecbto));
  AINIRec.WriteString(sSecao, 'nProt', procNFCom.nProt);
  AINIRec.WriteString(sSecao, 'digVal', procNFCom.digVal);
  AINIRec.WriteString(sSecao, 'cStat', IntToStr(procNFCom.cStat));
  AINIRec.WriteString(sSecao, 'xMotivo', procNFCom.xMotivo);
end;

// Reforma Tributária
procedure TNFComIniWriter.Gerar_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  if (IBSCBS.gIBSCBS.vBC > 0) then
  begin
    sSecao := 'IBSCBS' + IntToStrZero(Idx + 1, 3);

    AINIRec.WriteString(sSecao, 'CST', CSTIBSCBSToStr(IBSCBS.CST));
    AINIRec.WriteString(sSecao, 'cClassTrib', cClassTribToStr(IBSCBS.cClassTrib));

    if IBSCBS.gIBSCBS.vBC > 0 then
      Gerar_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS, Idx);
  end;
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS' + IntToStrZero(Idx + 1, 3);;

  AINIRec.WriteFloat(sSecao, 'vBC', gIBSCBS.vBC);
  AINIRec.WriteFloat(sSecao, 'vIBS', gIBSCBS.vIBS);

  Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec, gIBSCBS.gIBSUF, Idx);
  Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec, gIBSCBS.gIBSMun, Idx);
  Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec, gIBSCBS.gCBS, Idx);

  if gIBSCBS.gTribRegular.pAliqEfetRegIBSUF > 0 then
    Gerar_IBSCBS_gIBSCBS_gTribReg(AINIRec, gIBSCBS.gTribRegular, Idx);

  if gIBSCBS.gIBSCredPres.pCredPres > 0 then
    Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, gIBSCBS.gIBSCredPres, 'gIBSCredPres', Idx);

  if gIBSCBS.gCBSCredPres.pCredPres > 0 then
    Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, gIBSCBS.gCBSCredPres, 'gCBSCredPres', Idx);

  if gIBSCBS.gTribCompraGov.pAliqIBSUF > 0 then
    Gerar_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec, gIBSCBS.gTribCompraGov, Idx);
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile;
  gIBSUF: TgIBSUFValores; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF' + IntToStrZero(Idx + 1, 3);;

  AINIRec.WriteFloat(sSecao, 'pIBSUF', gIBSUF.pIBS);
  AINIRec.WriteFloat(sSecao, 'vIBSUF', gIBSUF.vIBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gIBSUF.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gIBSUF.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSUF.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gIBSUF.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gIBSUF.gRed.pAliqEfet);
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile;
  gIBSMun: TgIBSMunValores; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun' + IntToStrZero(Idx + 1, 3);;

  AINIRec.WriteFloat(sSecao, 'pIBSMun', gIBSMun.pIBS);
  AINIRec.WriteFloat(sSecao, 'vIBSMun', gIBSMun.vIBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gIBSMun.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gIBSMun.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSMun.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gIBSMun.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gIBSMun.gRed.pAliqEfet);
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBSValores; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBS' + IntToStrZero(Idx + 1, 3);;

  AINIRec.WriteFloat(sSecao, 'pCBS', gCBS.pCBS);
  AINIRec.WriteFloat(sSecao, 'vCBS', gCBS.vCBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gCBS.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gCBS.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gCBS.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gCBS.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gCBS.gRed.pAliqEfet);
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS_gTribReg(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular' + IntToStrZero(Idx + 1, 3);;

  AINIRec.WriteString(sSecao, 'CSTReg', CSTIBSCBSToStr(gTribRegular.CSTReg));
  AINIRec.WriteString(sSecao, 'cClassTribReg', cClassTribToStr(gTribRegular.cClassTribReg));
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegIBSUF', gTribRegular.pAliqEfetRegIBSUF);
  AINIRec.WriteFloat(sSecao, 'vTribRegIBSUF', gTribRegular.vTribRegIBSUF);
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegIBSMun', gTribRegular.pAliqEfetRegIBSMun);
  AINIRec.WriteFloat(sSecao, 'vTribRegIBSMun', gTribRegular.vTribRegIBSMun);
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegCBS', gTribRegular.pAliqEfetRegCBS);
  AINIRec.WriteFloat(sSecao, 'vTribRegCBS', gTribRegular.vTribRegCBS);
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec: TMemIniFile;
  gIBSCredPres: TgIBSCBSCredPres; const Grupo: string; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := Grupo + IntToStrZero(Idx + 1, 3);;

  AINIRec.WriteString(sSecao, 'cCredPres', cCredPresToStr(gIBSCredPres.cCredPres));
  AINIRec.WriteFloat(sSecao, 'pCredPres', gIBSCredPres.pCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPres', gIBSCredPres.vCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPresCondSus', gIBSCredPres.vCredPresCondSus);
end;

procedure TNFComIniWriter.Gerar_IBSCBS_gIBSCBS_gTribCompraGov(
  AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov' + IntToStrZero(Idx, 3);

  AINIRec.WriteFloat(sSecao, 'pAliqIBSUF', gTribCompraGov.pAliqIBSUF);
  AINIRec.WriteFloat(sSecao, 'vTribIBSUF', gTribCompraGov.vTribIBSUF);
  AINIRec.WriteFloat(sSecao, 'pAliqIBSMun', gTribCompraGov.pAliqIBSMun);
  AINIRec.WriteFloat(sSecao, 'vTribIBSMun', gTribCompraGov.vTribIBSMun);
  AINIRec.WriteFloat(sSecao, 'pAliqCBS', gTribCompraGov.pAliqCBS);
  AINIRec.WriteFloat(sSecao, 'vTribCBS', gTribCompraGov.vTribCBS);
end;

procedure TNFComIniWriter.Gerar_IBSCBSTot(AINIRec: TMemIniFile; IBSCBSTot: TIBSCBSTot);
var
  sSecao: string;
begin
  sSecao := 'IBSCBSTot';

  AINIRec.WriteFloat(sSecao, 'vBCIBSCBS', IBSCBSTot.vBCIBSCBS);

  Gerar_IBSCBSTot_gIBS(AINIRec, IBSCBSTot.gIBS);
  Gerar_IBSCBSTot_gCBS(AINIRec, IBSCBSTot.gCBS);
end;

procedure TNFComIniWriter.Gerar_IBSCBSTot_gIBS(AINIRec: TMemIniFile; gIBS: TgIBS);
var
  sSecao: string;
begin
  sSecao := 'gIBS';

  AINIRec.WriteFloat(sSecao, 'vIBS', gIBS.vIBS);
  AINIRec.WriteFloat(sSecao, 'vCredPres', gIBS.vCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPresCondSus', gIBS.vCredPresCondSus);

  Gerar_IBSCBSTot_gIBS_gIBSUFTot(AINIRec, gIBS.gIBSUFTot);
  Gerar_IBSCBSTot_gIBS_gIBSMunTot(AINIRec, gIBS.gIBSMunTot);
end;

procedure TNFComIniWriter.Gerar_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile;
  gIBSUFTot: TgIBSUFTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSUFTot';

  AINIRec.WriteFloat(sSecao, 'vDif', gIBSUFTot.vDif);
  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSUFTot.vDevTrib);
  AINIRec.WriteFloat(sSecao, 'vIBSUF', gIBSUFTot.vIBSUF);
end;

procedure TNFComIniWriter.Gerar_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile;
  gIBSMunTot: TgIBSMunTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSMunTot';

  AINIRec.WriteFloat(sSecao, 'vDif', gIBSMunTot.vDif);
  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSMunTot.vDevTrib);
  AINIRec.WriteFloat(sSecao, 'vIBSMun', gIBSMunTot.vIBSMun);
end;

procedure TNFComIniWriter.Gerar_IBSCBSTot_gCBS(AINIRec: TMemIniFile; gCBS: TgCBS);
var
  sSecao: string;
begin
  sSecao := 'gCBSTot';

  AINIRec.WriteFloat(sSecao, 'vDif', gCBS.vDif);
  AINIRec.WriteFloat(sSecao, 'vDevTrib', gCBS.vDevTrib);
  AINIRec.WriteFloat(sSecao, 'vCBS', gCBS.vCBS);
  AINIRec.WriteFloat(sSecao, 'vCredPres', gCBS.vCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPresCondSus', gCBS.vCredPresCondSus);
end;

end.
