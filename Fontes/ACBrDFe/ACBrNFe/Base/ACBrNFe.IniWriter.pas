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

unit ACBrNFe.IniWriter;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrNFe.Classes,
  pcnProcNFe;

type
  { TNFeIniWriter }

  TNFeIniWriter = class
  private
    FNFe: TNFe;

    procedure Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Gerar_NFReferenciada(AINIRec: TMemIniFile; NFref: TNFrefCollection);
    procedure Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Gerar_Avulsa(AINIRec: TMemIniFile; Avulsa: TAvulsa);
    procedure Gerar_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
    procedure Gerar_Retirada(AINIRec: TMemIniFile; Retirada: TRetirada);
    procedure Gerar_Entrega(AINIRec: TMemIniFile; Entrega: TEntrega);
    procedure Gerar_Detalhamento(AINIRec: TMemIniFile; Det: TDetCollection);
    procedure Gerar_Produto(AINIRec: TMemIniFile; Prod: TProd; Idx: Integer);
    procedure Gerar_NVE(AINIRec: TMemIniFile; NVE: TNVECollection; Idx: Integer);
    procedure Gerar_Rastro(AINIRec: TMemIniFile; rastro: TRastroCollection; Idx: Integer);
    procedure Gerar_DI(AINIRec: TMemIniFile; DI: TDICollection; Idx: Integer);
    procedure Gerar_DetExportacao(AINIRec: TMemIniFile; detExport: TdetExportCollection; Idx: Integer);
    procedure Gerar_Veiculo(AINIRec: TMemIniFile; veicProd: TveicProd; Idx: Integer);
    procedure Gerar_Medicamento(AINIRec: TMemIniFile; med: TMedCollection; Idx: Integer);
    procedure Gerar_Arma(AINIRec: TMemIniFile; arma: TArmaCollection; Idx: Integer);
    procedure Gerar_Combustivel(AINIRec: TMemIniFile; comb: TComb; Idx: Integer);
    procedure Gerar_OrigemCombustivel(AINIRec: TMemIniFile; origComb: TorigCombCollection; Idx: Integer);
    procedure Gerar_CIDE(AINIRec: TMemIniFile; CIDE: TCIDE; Idx: Integer);
    procedure Gerar_Encerrante(AINIRec: TMemIniFile; encerrante: Tencerrante; Idx: Integer);
    procedure Gerar_ICMSCombustivel(AINIRec: TMemIniFile; ICMS: TICMSComb; Idx: Integer);
    procedure Gerar_ICMSInterCombustivel(AINIRec: TMemIniFile; ICMSInter: TICMSInter; Idx: Integer);
    procedure Gerar_ICMSConsCombustivel(AINIRec: TMemIniFile; ICMSCons: TICMSCons; Idx: Integer);
    procedure Gerar_CreditoPresumido(AINIRec: TMemIniFile; CredPresumido: TCredPresumidoCollection; Idx: Integer);
    procedure Gerar_Imposto(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Gerar_ImpostoICMS(AINIRec: TMemIniFile; ICMS: TICMS; Idx: Integer);
    procedure Gerar_ImpostoICMSUFDest(AINIRec: TMemIniFile; ICMSUFDest: TICMSUFDest; Idx: Integer);
    procedure Gerar_ImpostoIPI(AINIRec: TMemIniFile; IPI: TIPI; Idx: Integer);
    procedure Gerar_ImpostoII(AINIRec: TMemIniFile; II: TII; Idx: Integer);
    procedure Gerar_ImpostoPIS(AINIRec: TMemIniFile; PIS: TPIS; Idx: Integer);
    procedure Gerar_ImpostoPISST(AINIRec: TMemIniFile; PISST: TPISST; Idx: Integer);
    procedure Gerar_ImpostoCOFINS(AINIRec: TMemIniFile; COFINS: TCOFINS; Idx: Integer);
    procedure Gerar_ImpostoCOFINSST(AINIRec: TMemIniFile; COFINSST: TCOFINSST; Idx: Integer);
    procedure Gerar_ImpostoISSQN(AINIRec: TMemIniFile; ISSQN: TISSQN; Idx: Integer);

    procedure Gerar_ObsContribuinte(AINIRec: TMemIniFile; obsCont: TobsItem; Idx: Integer);
    procedure Gerar_ObsFisco(AINIRec: TMemIniFile; obsFisco: TobsItem; Idx: Integer);

    procedure Gerar_Total(AINIRec: TMemIniFile; Total: TTotal);
    procedure Gerar_ISSQNtot(AINIRec: TMemIniFile; ISSQNtot: TISSQNtot);
    procedure Gerar_RetTributo(AINIRec: TMemIniFile; retTrib: TretTrib);

    procedure Gerar_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Gerar_Transportador(AINIRec: TMemIniFile; Transp: TTransp);
    procedure Gerar_Reboque(AINIRec: TMemIniFile; Reboque: TReboqueCollection);
    procedure Gerar_Volume(AINIRec: TMemIniFile; vol: TVolCollection);
    procedure Gerar_Fatura(AINIRec: TMemIniFile; Fat: TFat);
    procedure Gerar_Duplicata(AINIRec: TMemIniFile; Dup: TDupCollection);
    procedure Gerar_Pagamento(AINIRec: TMemIniFile; pag: TpagCollection);
    procedure Gerar_InfIntermediario(AINIRec: TMemIniFile; infIntermed: TinfIntermed);
    procedure Gerar_DadosAdicionais(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Gerar_InfAdicionaisContribuinte(AINIRec: TMemIniFile; obsCont: TobsContCollection);
    procedure Gerar_InfAdicionaisFisco(AINIRec: TMemIniFile; obsFisco: TobsFiscoCollection);
    procedure Gerar_ProcRef(AINIRec: TMemIniFile; procRef: TprocRefCollection);
    procedure Gerar_Exporta(AINIRec: TMemIniFile; exporta: TExporta);
    procedure Gerar_Compra(AINIRec: TMemIniFile; compra: TCompra);
    procedure Gerar_Cana(AINIRec: TMemIniFile; cana: Tcana);
    procedure Gerar_ForDia(AINIRec: TMemIniFile; fordia: TForDiaCollection);
    procedure Gerar_Deducao(AINIRec: TMemIniFile; deduc: TDeducCollection);
    procedure Gerar_infNFeSupl(AINIRec: TMemIniFile; infNFeSupl: TinfNFeSupl);
    procedure Gerar_infRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Gerar_Defensivo(AINIRec: TMemIniFile; defensivo: TdefensivoCollection);
    procedure Gerar_GuiaTransito(AINIRec: TMemIniFile; guiaTransito: TguiaTransito);
    procedure Gerar_ProcNFe(AINIRec: TMemIniFile; procNFe: TProcNFe);

    // Reforma Tributária
    procedure Gerar_gPagAntecipado(AINIRec: TMemIniFile; gPagAntecipado: TgPagAntecipadoCollection);

    procedure Gerar_ISel(AINIRec: TMemIniFile; ISel: TgIS; Idx: Integer);
    procedure Gerar_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; IBSCBS: TgIBSCBS; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBSMono(AINIRec: TMemIniFile; IBSCBSMono: TgIBSCBSMono; Idx: Integer);
    procedure Gerar_IBSCBS_gTransfCred(AINIRec: TMemIniFile; gTransfCred: TgTransfCred; Idx: Integer);
    procedure Gerar_IBSCBS_gCredPresIBSZFM(AINIRec: TMemIniFile; gCredPresIBSZFM: TCredPresIBSZFM; Idx: Integer);

    procedure Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUF; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMun; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile; gCBS: TgCBS; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gTribRegular(AINIRec: TMemIniFile;
      gTribRegular: TgTribRegular; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec: TMemIniFile;
      IBSCredPres: TgIBSCBSCredPres; const Grupo: string; Idx: Integer);
    procedure Gerar_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec: TMemIniFile;
      gTribCompraGov: TgTribCompraGov; Idx: Integer);

    procedure Gerar_Det_DFeReferenciado(AINIRec: TMemIniFile;
      DFeReferenciado: TDFeReferenciado; Idx: Integer);

    procedure Gerar_ISTot(AINIRec: TMemIniFile; ISTot: TISTot);
    procedure Gerar_IBSCBSTot(AINIRec: TMemIniFile; IBSCBSTot: TIBSCBSTot);

    procedure Gerar_IBSCBSTot_gIBS(AINIRec: TMemIniFile; IBS: TgIBSTot);
    procedure Gerar_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile; gIBSUFTot: TgIBSUFTot);
    procedure Gerar_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile; gIBSMunTot: TgIBSMunTot);

    procedure Gerar_IBSCBSTot_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSTot);
    procedure Gerar_IBSCBSTot_gMono(AINIRec: TMemIniFile; gMono: TgMono);
  public
    constructor Create(AOwner: TNFe); reintroduce;

    function GravarIni: string;

    property NFe: TNFe read FNFe write FNFe;
  end;


implementation

uses
  ACBrXmlBase,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrNFe,
  ACBrDFe.Conversao,
  pcnConversao,
  pcnConversaoNFe;

{ TNFeIniWriter }

constructor TNFeIniWriter.Create(AOwner: TNFe);
begin
  inherited Create;

  FNFe := AOwner;
end;

function TNFeIniWriter.GravarIni: string;
var
  INIRec: TMemIniFile;
  IniNFe: TStringList;
begin
  Result := '';

  if not ValidarChave(FNFe.infNFe.ID) then
    raise EACBrNFeException.Create('NFe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    INIRec.WriteString('infNFe', 'ID', FNFe.infNFe.ID);
    INIRec.WriteString('infNFe', 'Versao', FloatToStr(FNFe.infNFe.Versao));

    Gerar_Identificacao(INIRec, FNFe.Ide);
    Gerar_NFReferenciada(INIRec, FNFe.Ide.NFref);
    Gerar_Emitente(INIRec, FNFe.Emit);
    Gerar_Avulsa(INIRec, FNFe.Avulsa);
    Gerar_Destinatario(INIRec, FNFe.Dest);
    Gerar_Retirada(INIRec, FNFe.Retirada);
    Gerar_Entrega(INIRec, FNFe.Entrega);
    Gerar_Detalhamento(INIRec, FNFe.Det);
    Gerar_Total(INIRec, FNFe.Total);
    Gerar_ISSQNtot(INIRec, FNFe.Total.ISSQNtot);
    Gerar_RetTributo(INIRec, FNFe.Total.retTrib);

    Gerar_AutorizadosXml(INIRec, FNFe.autXML);
    Gerar_Transportador(INIRec, FNFe.Transp);
    Gerar_Reboque(INIRec, FNFe.Transp.Reboque);
    Gerar_Volume(INIRec, FNFe.Transp.vol);
    Gerar_Fatura(INIRec, FNFe.Cobr.Fat);
    Gerar_Duplicata(INIRec, FNFe.Cobr.Dup);
    Gerar_Pagamento(INIRec, FNFe.pag);
    Gerar_InfIntermediario(INIRec, FNFe.infIntermed);
    Gerar_DadosAdicionais(INIRec, FNFe.InfAdic);
    Gerar_InfAdicionaisContribuinte(INIRec, FNFe.InfAdic.obsCont);
    Gerar_InfAdicionaisFisco(INIRec, FNFe.InfAdic.obsFisco);
    Gerar_ProcRef(INIRec, FNFe.InfAdic.procRef);
    Gerar_Exporta(INIRec, FNFe.exporta);
    Gerar_Compra(INIRec, FNFe.compra);
    Gerar_Cana(INIRec, FNFe.cana);
    Gerar_ForDia(INIRec, FNFe.cana.fordia);
    Gerar_Deducao(INIRec, FNFe.cana.deduc);
    Gerar_infNFeSupl(INIRec, FNFe.infNFeSupl);
    Gerar_infRespTec(INIRec, FNFe.infRespTec);
    Gerar_Defensivo(INIRec, FNFe.agropecuario.defensivo);
    Gerar_GuiaTransito(INIRec, FNFe.agropecuario.guiaTransito);

    // Ultimo a ser gerado
    Gerar_ProcNFe(INIRec, FNFe.procNFe);

    IniNFe := TStringList.Create;
    try
      INIRec.GetStrings(IniNFe);
      Result := StringReplace(IniNFe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniNFe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

procedure TNFeIniWriter.Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
begin
  AINIRec.WriteInteger('Identificacao', 'cUF', Ide.cUF);
  AINIRec.WriteInteger('Identificacao', 'cNF', Ide.cNF);
  AINIRec.WriteString('Identificacao', 'natOp', Ide.natOp);
  AINIRec.WriteString('Identificacao', 'indPag', IndpagToStrEX(Ide.indPag));
  AINIRec.WriteInteger('Identificacao', 'Modelo', Ide.modelo);
  AINIRec.WriteInteger('Identificacao', 'Serie', Ide.serie);
  AINIRec.WriteInteger('Identificacao', 'nNF', Ide.nNF);
  AINIRec.WriteString('Identificacao', 'dhEmi', DateTimeToStr(Ide.dEmi));
  AINIRec.WriteString('Identificacao', 'dhSaiEnt', DateTimeToStr(Ide.dSaiEnt));
  AINIRec.WriteString('Identificacao', 'tpNF', tpNFToStr(Ide.tpNF));
  AINIRec.WriteString('Identificacao', 'idDest',
    DestinoOperacaoToStr(TpcnDestinoOperacao(Ide.idDest)));
  AINIRec.WriteInteger('Identificacao', 'cMunFG', Ide.cMunFG);
  AINIRec.WriteString('Identificacao', 'tpAmb', TpAmbToStr(Ide.tpAmb));
  AINIRec.WriteString('Identificacao', 'tpImp', TpImpToStr(Ide.tpImp));
  AINIRec.WriteString('Identificacao', 'tpemis', TpEmisToStr(Ide.tpemis));
  AINIRec.WriteString('Identificacao', 'finNFe', FinNFeToStr(Ide.finNFe));
  AINIRec.WriteString('Identificacao', 'indFinal', ConsumidorFinalToStr(
    TpcnConsumidorFinal(Ide.indFinal)));
  AINIRec.WriteString('Identificacao', 'indPres',
    PresencaCompradorToStr(TpcnPresencaComprador(Ide.indPres)));
  AINIRec.WriteString('Identificacao', 'indIntermed',
    indIntermedToStr(TindIntermed(Ide.indIntermed)));
  AINIRec.WriteString('Identificacao', 'procEmi', procEmiToStr(Ide.procEmi));
  AINIRec.WriteString('Identificacao', 'verProc', Ide.verProc);
  AINIRec.WriteString('Identificacao', 'dhCont', FormatDateTimeBr(Ide.dhCont));
  AINIRec.WriteString('Identificacao', 'xJust', Ide.xJust);

  // Reforma Tributária
  AINIRec.WriteInteger('Identificacao', 'cMunFGIBS', Ide.cMunFGIBS);
  AINIRec.WriteString('Identificacao', 'tpNFDebito', tpNFDebitoToStr(Ide.tpNFDebito));
  AINIRec.WriteString('Identificacao', 'tpNFCredito', tpNFCreditoToStr(Ide.tpNFCredito));

  if Ide.gCompraGov.pRedutor > 0 then
  begin
    AINIRec.WriteString('Identificacao', 'tpEnteGov', tpEnteGovToStr(Ide.gCompraGov.tpEnteGov));
    AINIRec.WriteFloat('Identificacao', 'pRedutor', Ide.gCompraGov.pRedutor);
    AINIRec.WriteString('Identificacao', 'tpOperGov', tpOperGovToStr(Ide.gCompraGov.tpOperGov));
  end;

  Gerar_gPagAntecipado(AINIRec, Ide.gPagAntecipado);
end;

procedure TNFeIniWriter.Gerar_NFReferenciada(AINIRec: TMemIniFile;
  NFref: TNFrefCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to NFref.Count - 1 do
  begin
    with NFref[I] do
    begin
      sSecao := 'NFRef' + IntToStrZero(I + 1, 3);
      if trim(refNFe) <> '' then
      begin
        AINIRec.WriteString(sSecao, 'Tipo', 'NFe');
        AINIRec.WriteString(sSecao, 'refNFe', refNFe);
      end
      else if trim(refNFeSig) <> '' then
      begin
        AINIRec.WriteString(sSecao, 'Tipo', 'NFe');
        AINIRec.WriteString(sSecao, 'refNFeSig', refNFeSig);
      end
      else if trim(RefNF.CNPJ) <> '' then
      begin
        AINIRec.WriteString(sSecao, 'Tipo', 'NF');
        AINIRec.WriteInteger(sSecao, 'cUF', RefNF.cUF);
        AINIRec.WriteString(sSecao, 'AAMM', RefNF.AAMM);
        AINIRec.WriteString(sSecao, 'CNPJ', RefNF.CNPJ);
        AINIRec.WriteInteger(sSecao, 'Modelo', RefNF.modelo);
        AINIRec.WriteInteger(sSecao, 'Serie', RefNF.serie);
        AINIRec.WriteInteger(sSecao, 'nNF', RefNF.nNF);
      end
      else if trim(RefNFP.CNPJCPF) <> '' then
      begin
        AINIRec.WriteString(sSecao, 'Tipo', 'NFP');
        AINIRec.WriteInteger(sSecao, 'cUF', RefNFP.cUF);
        AINIRec.WriteString(sSecao, 'AAMM', RefNFP.AAMM);
        AINIRec.WriteString(sSecao, 'CNPJ', RefNFP.CNPJCPF);
        AINIRec.WriteString(sSecao, 'IE', RefNFP.IE);
        AINIRec.WriteString(sSecao, 'Modelo', RefNFP.modelo);
        AINIRec.WriteInteger(sSecao, 'Serie', RefNFP.serie);
        AINIRec.WriteInteger(sSecao, 'nNF', RefNFP.nNF);
      end
      else if trim(refCTe) <> '' then
      begin
        AINIRec.WriteString(sSecao, 'Tipo', 'CTe');
        AINIRec.WriteString(sSecao, 'refCTe', refCTe);
      end
      else if trim(RefECF.nCOO) <> '' then
      begin
        AINIRec.WriteString(sSecao, 'Tipo', 'ECF');
        AINIRec.WriteString(sSecao, 'modelo', ECFModRefToStr(RefECF.modelo));
        AINIRec.WriteString(sSecao, 'nECF', RefECF.nECF);
        AINIRec.WriteString(sSecao, 'nCOO', RefECF.nCOO);
      end;
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
begin
  AINIRec.WriteString('Emitente', 'CNPJCPF', Emit.CNPJCPF);
  AINIRec.WriteString('Emitente', 'xNome', Emit.xNome);
  AINIRec.WriteString('Emitente', 'xFant', Emit.xFant);
  AINIRec.WriteString('Emitente', 'IE', Emit.IE);
  AINIRec.WriteString('Emitente', 'IEST', Emit.IEST);
  AINIRec.WriteString('Emitente', 'IM', Emit.IM);
  AINIRec.WriteString('Emitente', 'CNAE', Emit.CNAE);
  AINIRec.WriteString('Emitente', 'CRT', CRTToStr(Emit.CRT));
  AINIRec.WriteString('Emitente', 'xLgr', Emit.EnderEmit.xLgr);
  AINIRec.WriteString('Emitente', 'nro', Emit.EnderEmit.nro);
  AINIRec.WriteString('Emitente', 'xCpl', Emit.EnderEmit.xCpl);
  AINIRec.WriteString('Emitente', 'xBairro', Emit.EnderEmit.xBairro);
  AINIRec.WriteInteger('Emitente', 'cMun', Emit.EnderEmit.cMun);
  AINIRec.WriteString('Emitente', 'xMun', Emit.EnderEmit.xMun);
  AINIRec.WriteString('Emitente', 'UF', Emit.EnderEmit.UF);
  AINIRec.WriteInteger('Emitente', 'CEP', Emit.EnderEmit.CEP);
  AINIRec.WriteInteger('Emitente', 'cPais', Emit.EnderEmit.cPais);
  AINIRec.WriteString('Emitente', 'xPais', Emit.EnderEmit.xPais);
  AINIRec.WriteString('Emitente', 'Fone', Emit.EnderEmit.fone);
end;

procedure TNFeIniWriter.Gerar_Avulsa(AINIRec: TMemIniFile; Avulsa: TAvulsa);
begin
  if Avulsa.CNPJ <> '' then
  begin
    AINIRec.WriteString('Avulsa', 'CNPJ', Avulsa.CNPJ);
    AINIRec.WriteString('Avulsa', 'xOrgao', Avulsa.xOrgao);
    AINIRec.WriteString('Avulsa', 'matr', Avulsa.matr);
    AINIRec.WriteString('Avulsa', 'xAgente', Avulsa.xAgente);
    AINIRec.WriteString('Avulsa', 'fone', Avulsa.fone);
    AINIRec.WriteString('Avulsa', 'UF', Avulsa.UF);
    AINIRec.WriteString('Avulsa', 'nDAR', Avulsa.nDAR);
    AINIRec.WriteString('Avulsa', 'dEmi', DateToStr(Avulsa.dEmi));
    AINIRec.WriteFloat('Avulsa', 'vDAR', Avulsa.vDAR);
    AINIRec.WriteString('Avulsa', 'repEmi', Avulsa.repEmi);
    AINIRec.WriteString('Avulsa', 'dPag', DateToStr(Avulsa.dPag));
  end;
end;

procedure TNFeIniWriter.Gerar_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
begin
  if (Dest.idEstrangeiro <> EmptyStr) then
    AINIRec.WriteString('Destinatario', 'idEstrangeiro', Dest.idEstrangeiro);

  AINIRec.WriteString('Destinatario', 'CNPJCPF', Dest.CNPJCPF);
  AINIRec.WriteString('Destinatario', 'xNome', Dest.xNome);
  AINIRec.WriteString('Destinatario', 'indIEDest', indIEDestToStr(Dest.indIEDest));
  AINIRec.WriteString('Destinatario', 'IE', Dest.IE);
  AINIRec.WriteString('Destinatario', 'ISUF', Dest.ISUF);
  AINIRec.WriteString('Destinatario', 'IM', Dest.IM);
  AINIRec.WriteString('Destinatario', 'Email', Dest.Email);
  AINIRec.WriteString('Destinatario', 'xLgr', Dest.EnderDest.xLgr);
  AINIRec.WriteString('Destinatario', 'nro', Dest.EnderDest.nro);
  AINIRec.WriteString('Destinatario', 'xCpl', Dest.EnderDest.xCpl);
  AINIRec.WriteString('Destinatario', 'xBairro', Dest.EnderDest.xBairro);
  AINIRec.WriteInteger('Destinatario', 'cMun', Dest.EnderDest.cMun);
  AINIRec.WriteString('Destinatario', 'xMun', Dest.EnderDest.xMun);
  AINIRec.WriteString('Destinatario', 'UF', Dest.EnderDest.UF);
  AINIRec.WriteInteger('Destinatario', 'CEP', Dest.EnderDest.CEP);
  AINIRec.WriteInteger('Destinatario', 'cPais', Dest.EnderDest.cPais);
  AINIRec.WriteString('Destinatario', 'xPais', Dest.EnderDest.xPais);
  AINIRec.WriteString('Destinatario', 'Fone', Dest.EnderDest.Fone);
end;

procedure TNFeIniWriter.Gerar_Retirada(AINIRec: TMemIniFile;
  Retirada: TRetirada);
begin
  if Retirada.CNPJCPF <> '' then
  begin
    AINIRec.WriteString('Retirada', 'CNPJCPF', Retirada.CNPJCPF);
    AINIRec.WriteString('Retirada', 'xLgr', Retirada.xLgr);
    AINIRec.WriteString('Retirada', 'nro', Retirada.nro);
    AINIRec.WriteString('Retirada', 'xCpl', Retirada.xCpl);
    AINIRec.WriteString('Retirada', 'xBairro', Retirada.xBairro);
    AINIRec.WriteInteger('Retirada', 'cMun', Retirada.cMun);
    AINIRec.WriteString('Retirada', 'xMun', Retirada.xMun);
    AINIRec.WriteString('Retirada', 'UF', Retirada.UF);
  end;
end;

procedure TNFeIniWriter.Gerar_Entrega(AINIRec: TMemIniFile; Entrega: TEntrega);
begin
  if Entrega.CNPJCPF <> '' then
  begin
    AINIRec.WriteString('Entrega', 'CNPJCPF', Entrega.CNPJCPF);
    AINIRec.WriteString('Entrega', 'xLgr', Entrega.xLgr);
    AINIRec.WriteString('Entrega', 'nro', Entrega.nro);
    AINIRec.WriteString('Entrega', 'xCpl', Entrega.xCpl);
    AINIRec.WriteString('Entrega', 'xBairro', Entrega.xBairro);
    AINIRec.WriteInteger('Entrega', 'cMun', Entrega.cMun);
    AINIRec.WriteString('Entrega', 'xMun', Entrega.xMun);
    AINIRec.WriteString('Entrega', 'UF', Entrega.UF);
  end;
end;

procedure TNFeIniWriter.Gerar_Detalhamento(AINIRec: TMemIniFile;
  Det: TDetCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to Det.Count - 1 do
  begin
    with Det[I] do
    begin
      Gerar_Produto(AINIRec, Prod, I);

      sSecao := 'Produto' + IntToStrZero(I + 1, 3);
      AINIRec.WriteString(sSecao, 'infAdProd', infAdProd);

      // os dois campos abaixo aparecem dentro da seção impostoDevol
      AINIRec.WriteFloat(sSecao, 'pDevol', pDevol);
      AINIRec.WriteFloat(sSecao, 'vIPIDevol', vIPIDevol);

      AINIRec.WriteFloat(sSecao, 'vTotTrib', Imposto.vTotTrib);

      // Reforma Tributária
      AINIRec.WriteFloat(sSecao, 'vItem', vItem);

      Gerar_NVE(AINIRec, Prod.NVE, I);
      Gerar_Rastro(AINIRec, Prod.rastro, I);
      Gerar_DI(AINIRec, Prod.DI, I);
      Gerar_DetExportacao(AINIRec, Prod.detExport, I);

      if (pDevol > 0) then
      begin
        sSecao := 'impostoDevol' + IntToStrZero(I + 1, 3);
        AINIRec.WriteFloat(sSecao, 'pDevol', pDevol);
        AINIRec.WriteFloat(sSecao, 'vIPIDevol', vIPIDevol);
      end;

      Gerar_Veiculo(AINIRec, Prod.veicProd, I);
      Gerar_Medicamento(AINIRec, Prod.med, I);
      Gerar_Arma(AINIRec, Prod.arma, I);
      Gerar_Combustivel(AINIRec, Prod.comb, I);
      Gerar_OrigemCombustivel(AINIRec, Prod.comb.origComb, I);
      Gerar_CIDE(AINIRec, Prod.comb.CIDE, I);
      Gerar_Encerrante(AINIRec, Prod.comb.encerrante, I);
      Gerar_ICMSCombustivel(AINIRec, Prod.comb.ICMS, I);
      Gerar_ICMSInterCombustivel(AINIRec, Prod.comb.ICMSInter, I);
      Gerar_ICMSConsCombustivel(AINIRec, Prod.comb.ICMSCons, I);
      Gerar_CreditoPresumido(AINIRec, Prod.CredPresumido, I);

      Gerar_Imposto(AINIRec, Imposto, I);

      Gerar_ObsContribuinte(AINIRec, obsCont, I);
      Gerar_ObsFisco(AINIRec, obsFisco, I);

      // Reforma Tributária
      Gerar_Det_DFeReferenciado(AINIRec, DFeReferenciado, I);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Produto(AINIRec: TMemIniFile; Prod: TProd;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'Produto' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteInteger(sSecao, 'nItem', Prod.nItem);
  AINIRec.WriteString(sSecao, 'cProd', Prod.cProd);
  AINIRec.WriteString(sSecao, 'cEAN', Prod.cEAN);
  AINIRec.WriteString(sSecao, 'cBarra', Prod.cBarra);
  AINIRec.WriteString(sSecao, 'xProd', Prod.xProd);
  AINIRec.WriteString(sSecao, 'NCM', Prod.NCM);
  AINIRec.WriteString(sSecao, 'CEST', Prod.CEST);
  AINIRec.WriteString(sSecao, 'indEscala', IndEscalaToStr(Prod.indEscala));
  AINIRec.WriteString(sSecao, 'CNPJFab', Prod.CNPJFab);
  AINIRec.WriteString(sSecao, 'cBenef', Prod.cBenef);
  AINIRec.WriteString(sSecao, 'EXTIPI', Prod.EXTIPI);
  AINIRec.WriteString(sSecao, 'CFOP', Prod.CFOP);
  AINIRec.WriteString(sSecao, 'uCom', Prod.uCom);
  AINIRec.WriteFloat(sSecao, 'qCom', Prod.qCom);
  AINIRec.WriteFloat(sSecao, 'vUnCom', Prod.vUnCom);
  AINIRec.WriteFloat(sSecao, 'vProd', Prod.vProd);
  AINIRec.WriteString(sSecao, 'cEANTrib', Prod.cEANTrib);
  AINIRec.WriteString(sSecao, 'cBarraTrib', Prod.cBarraTrib);
  AINIRec.WriteString(sSecao, 'uTrib', Prod.uTrib);
  AINIRec.WriteFloat(sSecao, 'qTrib', Prod.qTrib);
  AINIRec.WriteFloat(sSecao, 'vUnTrib', Prod.vUnTrib);
  AINIRec.WriteFloat(sSecao, 'vFrete', Prod.vFrete);
  AINIRec.WriteFloat(sSecao, 'vSeg', Prod.vSeg);
  AINIRec.WriteFloat(sSecao, 'vDesc', Prod.vDesc);
  AINIRec.WriteFloat(sSecao, 'vOutro', Prod.vOutro);
  AINIRec.WriteString(sSecao, 'IndTot', indTotToStr(Prod.IndTot));
  AINIRec.WriteString(sSecao, 'xPed', Prod.xPed);
  AINIRec.WriteString(sSecao, 'nItemPed', Prod.nItemPed);
  AINIRec.WriteString(sSecao, 'nFCI', Prod.nFCI);
  AINIRec.WriteString(sSecao, 'nRECOPI', Prod.nRECOPI);
  // Reforma Tributária
  AINIRec.WriteString(sSecao, 'indBemMovelUsado', TIndicadorExToStr(Prod.indBemMovelUsado));
end;

procedure TNFeIniWriter.Gerar_NVE(AINIRec: TMemIniFile; NVE: TNVECollection;
  Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to NVE.Count - 1 do
  begin
    if NVE[J].NVE <> '' then
    begin
      with NVE[J] do
      begin
        sSecao := 'NVE' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3);
        AINIRec.WriteString(sSecao, 'NVE', NVE);
      end;
    end
    else
      Break;
  end;
end;

procedure TNFeIniWriter.Gerar_Rastro(AINIRec: TMemIniFile;
  rastro: TRastroCollection; Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to rastro.Count - 1 do
  begin
    if rastro[J].nLote <> '' then
    begin
      with rastro[J] do
      begin
        sSecao := 'Rastro' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3);
        AINIRec.WriteString(sSecao, 'nLote', nLote);
        AINIRec.WriteFloat(sSecao, 'qLote', qLote);
        AINIRec.WriteDateTime(sSecao, 'dFab', dFab);
        AINIRec.WriteDateTime(sSecao, 'dVal', dVal);
        AINIRec.WriteString(sSecao, 'cAgreg', cAgreg);
      end;
    end
    else
      Break;
  end;
end;

procedure TNFeIniWriter.Gerar_DI(AINIRec: TMemIniFile; DI: TDICollection;
  Idx: Integer);
var
  J, K: Integer;
  sSecao: string;
begin
  for J := 0 to DI.Count - 1 do
  begin
    if DI[j].nDi <> '' then
    begin
      with DI[j] do
      begin
        sSecao := 'DI' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3);
        AINIRec.WriteString(sSecao, 'nDi', nDi);
        AINIRec.WriteString(sSecao, 'dDi', DateToStr(dDi));
        AINIRec.WriteString(sSecao, 'xLocDesemb', xLocDesemb);
        AINIRec.WriteString(sSecao, 'UFDesemb', UFDesemb);
        AINIRec.WriteString(sSecao, 'dDesemb', DateToStr(dDesemb));
        AINIRec.WriteString(sSecao, 'cExportador', cExportador);

        if (TipoViaTranspToStr(tpViaTransp) <> '') then
        begin
          AINIRec.WriteString(sSecao, 'tpViaTransp',
            TipoViaTranspToStr(tpViaTransp));
          if (tpViaTransp = tvMaritima) then
            AINIRec.WriteFloat(sSecao, 'vAFRMM', vAFRMM);
        end;

        if (TipoIntermedioToStr(tpIntermedio) <> '') then
        begin
          AINIRec.WriteString(sSecao, 'tpIntermedio',
            TipoIntermedioToStr(tpIntermedio));
          if not (tpIntermedio = tiContaPropria) then
          begin
            AINIRec.WriteString(sSecao, 'CNPJ', CNPJ);
            AINIRec.WriteString(sSecao, 'UFTerceiro', UFTerceiro);
          end;
        end;

        for K := 0 to adi.Count - 1 do
        begin
          sSecao :=
            'LADI' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3) + IntToStrZero(K + 1, 3);
          AINIRec.WriteInteger(sSecao, 'nAdicao', adi[K].nAdicao);
          AINIRec.WriteInteger(sSecao, 'nSeqAdi', adi[K].nSeqAdi);
          AINIRec.WriteString(sSecao, 'cFabricante', adi[K].cFabricante);
          AINIRec.WriteFloat(sSecao, 'vDescDI', adi[K].vDescDI);
          AINIRec.WriteString(sSecao, 'nDraw', adi[K].nDraw);
        end;
      end;
    end
    else
      Break;
  end;
end;

procedure TNFeIniWriter.Gerar_DetExportacao(AINIRec: TMemIniFile;
  detExport: TdetExportCollection; Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to detExport.Count - 1 do
  begin
    if detExport[j].nDraw <> '' then
    begin
      with detExport[j] do
      begin
        sSecao := 'detExport' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3);
        AINIRec.WriteString(sSecao, 'nDraw', nDraw);
        AINIRec.WriteString(sSecao, 'nRe', nRE);
        AINIRec.WriteString(sSecao, 'chNFe', chNFe);
        AINIRec.WriteFloat(sSecao, 'qExport', qExport);
      end;
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Veiculo(AINIRec: TMemIniFile; veicProd: TveicProd;
  Idx: Integer);
var
  sSecao: string;
begin
  if veicProd.chassi <> '' then
  begin
    sSecao := 'Veiculo' + IntToStrZero(Idx + 1, 3);
    with veicProd do
    begin
      AINIRec.WriteString(sSecao, 'tpOP', tpOPToStr(tpOP));
      AINIRec.WriteString(sSecao, 'Chassi', chassi);
      AINIRec.WriteString(sSecao, 'cCor', cCor);
      AINIRec.WriteString(sSecao, 'xCor', xCor);
      AINIRec.WriteString(sSecao, 'pot', pot);
      AINIRec.WriteString(sSecao, 'Cilin', Cilin);
      AINIRec.WriteString(sSecao, 'pesoL', pesoL);
      AINIRec.WriteString(sSecao, 'pesoB', pesoB);
      AINIRec.WriteString(sSecao, 'nSerie', nSerie);
      AINIRec.WriteString(sSecao, 'tpComb', tpComb);
      AINIRec.WriteString(sSecao, 'nMotor', nMotor);
      AINIRec.WriteString(sSecao, 'CMT', CMT);
      AINIRec.WriteString(sSecao, 'dist', dist);
      AINIRec.WriteInteger(sSecao, 'anoMod', anoMod);
      AINIRec.WriteInteger(sSecao, 'anoFab', anoFab);
      AINIRec.WriteString(sSecao, 'tpPint', tpPint);
      AINIRec.WriteInteger(sSecao, 'tpVeic', tpVeic);
      AINIRec.WriteInteger(sSecao, 'espVeic', espVeic);
      AINIRec.WriteString(sSecao, 'VIN', VIN);
      AINIRec.WriteString(sSecao, 'condVeic', condVeicToStr(condVeic));
      AINIRec.WriteString(sSecao, 'cMod', cMod);
      AINIRec.WriteString(sSecao, 'cCorDENATRAN', cCorDENATRAN);
      AINIRec.WriteInteger(sSecao, 'lota', lota);
      AINIRec.WriteInteger(sSecao, 'tpRest', tpRest);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Medicamento(AINIRec: TMemIniFile;
  med: TMedCollection; Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to med.Count - 1 do
  begin
    sSecao := 'Medicamento' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3);
    with med[J] do
    begin
      if NFe.infNFe.Versao >= 4 then
      begin
        AINIRec.WriteString(sSecao, 'cProdANVISA', cProdANVISA);
        AINIRec.WriteString(sSecao, 'xMotivoIsencao', xMotivoIsencao);
      end;

      if NFe.infNFe.Versao < 4 then
      begin
        AINIRec.WriteString(sSecao, 'nLote', nLote);
        AINIRec.WriteFloat(sSecao, 'qLote', qLote);
        AINIRec.WriteString(sSecao, 'dFab', DateToStr(dFab));
        AINIRec.WriteString(sSecao, 'dVal', DateToStr(dVal));
      end;

      AINIRec.WriteFloat(sSecao, 'vPMC', vPMC);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Arma(AINIRec: TMemIniFile; arma: TArmaCollection;
  Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to arma.Count - 1 do
  begin
    sSecao := 'Arma' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 3);
    with arma[J] do
    begin
      AINIRec.WriteString(sSecao, 'tpArma', tpArmaToStr(tpArma));
      AINIRec.WriteString(sSecao, 'nSerie', nSerie);
      AINIRec.WriteString(sSecao, 'nCano', nCano);
      AINIRec.WriteString(sSecao, 'descr', descr);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Combustivel(AINIRec: TMemIniFile; comb: TComb;
  Idx: Integer);
var
  sSecao: string;
begin
  if (comb.cProdANP > 0) then
  begin
    sSecao := 'Combustivel' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteInteger(sSecao, 'cProdANP', comb.cProdANP);
    AINIRec.WriteFloat(sSecao, 'pMixGN', comb.pMixGN);
    AINIRec.WriteString(sSecao, 'descANP', comb.descANP);
    AINIRec.WriteFloat(sSecao, 'pGLP', comb.pGLP);
    AINIRec.WriteFloat(sSecao, 'pGNn', comb.pGNn);
    AINIRec.WriteFloat(sSecao, 'pGNi', comb.pGNi);
    AINIRec.WriteFloat(sSecao, 'vPart', comb.vPart);
    AINIRec.WriteString(sSecao, 'CODIF', comb.CODIF);
    AINIRec.WriteFloat(sSecao, 'qTemp', comb.qTemp);
    AINIRec.WriteString(sSecao, 'UFCons', comb.UFcons);
    AINIRec.WriteFloat(sSecao, 'pBio', comb.pBio);
  end;
end;

procedure TNFeIniWriter.Gerar_OrigemCombustivel(AINIRec: TMemIniFile;
  origComb: TorigCombCollection; Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to origComb.Count - 1 do
  begin
    sSecao := 'origComb' + IntToStrZero(Idx + 1 , 3) + IntToStrZero(J + 1, 2);
    AINIRec.WriteString(sSecao, 'indImport', indImportToStr(origComb[J].indImport));
    AINIRec.WriteInteger(sSecao, 'cUFOrig', origComb[J].cUFOrig);
    AINIRec.WriteFloat(sSecao, 'pOrig', origComb[J].pOrig);
  end;
end;

procedure TNFeIniWriter.Gerar_CIDE(AINIRec: TMemIniFile; CIDE: TCIDE;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'CIDE' + IntToStrZero(Idx + 1, 3);
  AINIRec.WriteFloat(sSecao, 'qBCprod', CIDE.qBCprod);
  AINIRec.WriteFloat(sSecao, 'vAliqProd', CIDE.vAliqProd);
  AINIRec.WriteFloat(sSecao, 'vCIDE', CIDE.vCIDE);
end;

procedure TNFeIniWriter.Gerar_Encerrante(AINIRec: TMemIniFile;
  encerrante: Tencerrante; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'encerrante' + IntToStrZero(Idx + 1, 3);
  AINIRec.WriteInteger(sSecao, 'nBico', encerrante.nBico);
  AINIRec.WriteInteger(sSecao, 'nBomba', encerrante.nBomba);
  AINIRec.WriteInteger(sSecao, 'nTanque', encerrante.nTanque);
  AINIRec.WriteFloat(sSecao, 'vEncIni', encerrante.vEncIni);
  AINIRec.WriteFloat(sSecao, 'vEncFin', encerrante.vEncFin);
end;

procedure TNFeIniWriter.Gerar_ICMSCombustivel(AINIRec: TMemIniFile;
  ICMS: TICMSComb; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'ICMSComb' + IntToStrZero(Idx + 1, 3);
  AINIRec.WriteFloat(sSecao, 'vBCICMS', ICMS.vBCICMS);
  AINIRec.WriteFloat(sSecao, 'vICMS', ICMS.vICMS);
  AINIRec.WriteFloat(sSecao, 'vBCICMSST', ICMS.vBCICMSST);
  AINIRec.WriteFloat(sSecao, 'vICMSST', ICMS.vICMSST);
end;

procedure TNFeIniWriter.Gerar_ICMSInterCombustivel(AINIRec: TMemIniFile;
  ICMSInter: TICMSInter; Idx: Integer);
var
  sSecao: string;
begin
  if (ICMSInter.vBCICMSSTDest > 0) then
  begin
    sSecao := 'ICMSInter' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteFloat(sSecao, 'vBCICMSSTDest', ICMSInter.vBCICMSSTDest);
    AINIRec.WriteFloat(sSecao, 'vICMSSTDest', ICMSInter.vICMSSTDest);
  end;
end;

procedure TNFeIniWriter.Gerar_ICMSConsCombustivel(AINIRec: TMemIniFile;
  ICMSCons: TICMSCons; Idx: Integer);
var
  sSecao: string;
begin
  if (ICMSCons.vBCICMSSTCons > 0) then
  begin
    sSecao := 'ICMSCons' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteFloat(sSecao, 'vBCICMSSTCons', ICMSCons.vBCICMSSTCons);
    AINIRec.WriteFloat(sSecao, 'vICMSSTCons', ICMSCons.vICMSSTCons);
    AINIRec.WriteString(sSecao, 'UFCons', ICMSCons.UFcons);
  end;
end;

procedure TNFeIniWriter.Gerar_CreditoPresumido(AINIRec: TMemIniFile;
  CredPresumido: TCredPresumidoCollection; Idx: Integer);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to CredPresumido.Count - 1 do
  begin
    sSecao := 'gCred' + IntToStrZero(Idx + 1, 3) + IntToStrZero(J + 1, 1);
    with CredPresumido[J] do
    begin
      AINIRec.WriteString(sSecao, 'cCredPresumido', cCredPresumido);
      AINIRec.WriteFloat(sSecao, 'pCredPresumido', pCredPresumido);
      AINIRec.WriteFloat(sSecao, 'vCredPresumido', vCredPresumido);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Imposto(AINIRec: TMemIniFile; Imposto: TImposto;
  Idx: Integer);
begin
  Gerar_ImpostoICMS(AINIRec, Imposto.ICMS, Idx);
  Gerar_ImpostoICMSUFDest(AINIRec, Imposto.ICMSUFDest, Idx);
  Gerar_ImpostoIPI(AINIRec, Imposto.IPI, Idx);
  Gerar_ImpostoII(AINIRec, Imposto.II, Idx);
  Gerar_ImpostoPIS(AINIRec, Imposto.PIS, Idx);
  Gerar_ImpostoPISST(AINIRec, Imposto.PISST, Idx);
  Gerar_ImpostoCOFINS(AINIRec, Imposto.COFINS, Idx);
  Gerar_ImpostoCOFINSST(AINIRec, Imposto.COFINSST, Idx);
  Gerar_ImpostoISSQN(AINIRec, Imposto.ISSQN, Idx);

  // Reforma Tributária
  if Imposto.ISel.vBCIS > 0 then
    Gerar_ISel(AINIRec, Imposto.ISel, Idx);

  Gerar_IBSCBS(AINIRec, Imposto.IBSCBS, Idx);
end;

procedure TNFeIniWriter.Gerar_ImpostoICMS(AINIRec: TMemIniFile; ICMS: TICMS;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'ICMS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'orig', OrigToStr(ICMS.orig));
  AINIRec.WriteString(sSecao, 'CST', CSTICMSToStr(ICMS.CST));
  AINIRec.WriteString(sSecao, 'CSOSN', CSOSNIcmsToStr(ICMS.CSOSN));
  AINIRec.WriteString(sSecao, 'modBC', modBCToStr(ICMS.modBC));
  AINIRec.WriteFloat(sSecao, 'pRedBC', ICMS.pRedBC);
  AINIRec.WriteString(sSecao, 'cBenefRBC', ICMS.cBenefRBC);
  AINIRec.WriteFloat(sSecao, 'vBC', ICMS.vBC);
  AINIRec.WriteFloat(sSecao, 'pICMS', ICMS.pICMS);
  AINIRec.WriteFloat(sSecao, 'vICMS', ICMS.vICMS);
  AINIRec.WriteFloat(sSecao, 'vBCFCP', ICMS.vBCFCP);
  AINIRec.WriteFloat(sSecao, 'pFCP', ICMS.pFCP);
  AINIRec.WriteFloat(sSecao, 'vFCP', ICMS.vFCP);
  AINIRec.WriteString(sSecao, 'modBCST', modBCSTToStr(ICMS.modBCST));
  AINIRec.WriteFloat(sSecao, 'pMVAST', ICMS.pMVAST);
  AINIRec.WriteFloat(sSecao, 'pRedBCST', ICMS.pRedBCST);
  AINIRec.WriteFloat(sSecao, 'vBCST', ICMS.vBCST);
  AINIRec.WriteFloat(sSecao, 'pICMSST', ICMS.pICMSST);
  AINIRec.WriteFloat(sSecao, 'vICMSST', ICMS.vICMSST);
  AINIRec.WriteFloat(sSecao, 'vBCFCPST', ICMS.vBCFCPST);
  AINIRec.WriteFloat(sSecao, 'pFCPST', ICMS.pFCPST);
  AINIRec.WriteFloat(sSecao, 'vFCPST', ICMS.vFCPST);
  AINIRec.WriteString(sSecao, 'UFST', ICMS.UFST);
  AINIRec.WriteFloat(sSecao, 'pBCOp', ICMS.pBCOp);
  AINIRec.WriteFloat(sSecao, 'vBCSTRet', ICMS.vBCSTRet);
  AINIRec.WriteFloat(sSecao, 'pST', ICMS.pST);
  AINIRec.WriteFloat(sSecao, 'vICMSSTRet', ICMS.vICMSSTRet);
  AINIRec.WriteFloat(sSecao, 'vBCFCPSTRet', ICMS.vBCFCPSTRet);
  AINIRec.WriteFloat(sSecao, 'pFCPSTRet', ICMS.pFCPSTRet);
  AINIRec.WriteFloat(sSecao, 'vFCPSTRet', ICMS.vFCPSTRet);
  AINIRec.WriteString(sSecao, 'motDesICMS', motDesICMSToStr(ICMS.motDesICMS));
  AINIRec.WriteFloat(sSecao, 'pCredSN', ICMS.pCredSN);
  AINIRec.WriteFloat(sSecao, 'vCredICMSSN', ICMS.vCredICMSSN);
  AINIRec.WriteFloat(sSecao, 'vBCSTDest', ICMS.vBCSTDest);
  AINIRec.WriteFloat(sSecao, 'vICMSSTDest', ICMS.vICMSSTDest);
  AINIRec.WriteFloat(sSecao, 'vICMSDeson', ICMS.vICMSDeson);
  AINIRec.WriteFloat(sSecao, 'vICMSOp', ICMS.vICMSOp);
  AINIRec.WriteFloat(sSecao, 'pDif', ICMS.pDif);
  AINIRec.WriteFloat(sSecao, 'vICMSDif', ICMS.vICMSDif);

  AINIRec.WriteFloat(sSecao, 'pRedBCEfet', ICMS.pRedBCEfet);
  AINIRec.WriteFloat(sSecao, 'vBCEfet', ICMS.vBCEfet);
  AINIRec.WriteFloat(sSecao, 'pICMSEfet', ICMS.pICMSEfet);
  AINIRec.WriteFloat(sSecao, 'vICMSEfet', ICMS.vICMSEfet);

  AINIRec.WriteFloat(sSecao, 'vICMSSubstituto', ICMS.vICMSSubstituto);

  AINIRec.WriteFloat(sSecao, 'vICMSSTDeson', ICMS.vICMSSTDeson);
  AINIRec.WriteString(sSecao, 'motDesICMSST', motDesICMSToStr(ICMS.motDesICMSST));
  AINIRec.WriteFloat(sSecao, 'pFCPDif', ICMS.pFCPDif);
  AINIRec.WriteFloat(sSecao, 'vFCPDif', ICMS.vFCPDif);
  AINIRec.WriteFloat(sSecao, 'vFCPEfet', ICMS.vFCPEfet);

  AINIRec.WriteFloat(sSecao, 'adRemICMS', ICMS.adRemICMS);
  AINIRec.WriteFloat(sSecao, 'vICMSMono', ICMS.vICMSMono);
  AINIRec.WriteFloat(sSecao, 'adRemICMSReten', ICMS.adRemICMSReten);
  AINIRec.WriteFloat(sSecao, 'vICMSMonoReten', ICMS.vICMSMonoReten);
  AINIRec.WriteFloat(sSecao, 'vICMSMonoDif', ICMS.vICMSMonoDif);
  AINIRec.WriteFloat(sSecao, 'adRemICMSRet', ICMS.adRemICMSRet);
  AINIRec.WriteFloat(sSecao, 'vICMSMonoRet', ICMS.vICMSMonoRet);

  AINIRec.WriteFloat(sSecao, 'qBCMono', ICMS.qBCMono);
  AINIRec.WriteFloat(sSecao, 'qBCMonoReten', ICMS.qBCMonoReten);
  AINIRec.WriteFloat(sSecao, 'pRedAdRem', ICMS.pRedAdRem);
  AINIRec.WriteString(sSecao, 'motRedAdRem', motRedAdRemToStr(ICMS.motRedAdRem));
  AINIRec.WriteFloat(sSecao, 'qBCMonoRet', ICMS.qBCMonoRet);
  AINIRec.WriteFloat(sSecao, 'vICMSMonoOp', ICMS.vICMSMonoOp);
  AINIRec.WriteString(sSecao, 'indDeduzDeson', TIndicadorExToStr(ICMS.indDeduzDeson));
end;

procedure TNFeIniWriter.Gerar_ImpostoICMSUFDest(AINIRec: TMemIniFile;
  ICMSUFDest: TICMSUFDest; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'ICMSUFDEST' + IntToStrZero(Idx + 1, 3);
  AINIRec.WriteFloat(sSecao, 'vBCUFDest', ICMSUFDest.vBCUFDest);
  AINIRec.WriteFloat(sSecao, 'vBCFCPUFDest', ICMSUFDest.vBCFCPUFDest);
  AINIRec.WriteFloat(sSecao, 'pICMSUFDest', ICMSUFDest.pICMSUFDest);
  AINIRec.WriteFloat(sSecao, 'pICMSInter', ICMSUFDest.pICMSInter);
  AINIRec.WriteFloat(sSecao, 'pICMSInterPart', ICMSUFDest.pICMSInterPart);
  AINIRec.WriteFloat(sSecao, 'vICMSUFDest', ICMSUFDest.vICMSUFDest);
  AINIRec.WriteFloat(sSecao, 'vICMSUFRemet', ICMSUFDest.vICMSUFRemet);
  AINIRec.WriteFloat(sSecao, 'pFCPUFDest', ICMSUFDest.pFCPUFDest);
  AINIRec.WriteFloat(sSecao, 'vFCPUFDest', ICMSUFDest.vFCPUFDest);
end;

procedure TNFeIniWriter.Gerar_ImpostoIPI(AINIRec: TMemIniFile; IPI: TIPI;
  Idx: Integer);
var
  sSecao: string;
begin
  if (IPI.cEnq <> '') then
  begin
    sSecao := 'IPI' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteString(sSecao, 'CST', CSTIPIToStr(IPI.CST));
    AINIRec.WriteString(sSecao, 'cEnq', IPI.cEnq);
    AINIRec.WriteString(sSecao, 'clEnq', IPI.clEnq);
    AINIRec.WriteString(sSecao, 'CNPJProd', IPI.CNPJProd);
    AINIRec.WriteString(sSecao, 'cSelo', IPI.cSelo);
    AINIRec.WriteInteger(sSecao, 'qSelo', IPI.qSelo);
    AINIRec.WriteFloat(sSecao, 'vBC', IPI.vBC);
    AINIRec.WriteFloat(sSecao, 'qUnid', IPI.qUnid);
    AINIRec.WriteFloat(sSecao, 'vUnid', IPI.vUnid);
    AINIRec.WriteFloat(sSecao, 'pIPI', IPI.pIPI);
    AINIRec.WriteFloat(sSecao, 'vIPI', IPI.vIPI);
  end;
end;

procedure TNFeIniWriter.Gerar_ImpostoII(AINIRec: TMemIniFile; II: TII;
  Idx: Integer);
var
  sSecao: string;
begin
  if (II.vBc > 0) then
  begin
    sSecao := 'II' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteFloat(sSecao, 'vBc', II.vBc);
    AINIRec.WriteFloat(sSecao, 'vDespAdu', II.vDespAdu);
    AINIRec.WriteFloat(sSecao, 'vII', II.vII);
    AINIRec.WriteFloat(sSecao, 'vIOF', II.vIOF);
  end;
end;

procedure TNFeIniWriter.Gerar_ImpostoPIS(AINIRec: TMemIniFile; PIS: TPIS;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'PIS' + IntToStrZero(Idx + 1, 3);
  AINIRec.WriteString(sSecao, 'CST', CSTPISToStr(PIS.CST));
  case PIS.CST of
    pis01, pis02:
      begin
        AINIRec.WriteFloat(sSecao, 'vBC', PIS.vBC);
        AINIRec.WriteFloat(sSecao, 'pPIS', PIS.pPIS);
        AINIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
      end;

    pis03:
      begin
        AINIRec.WriteFloat(sSecao, 'qBCProd', PIS.qBCProd);
        AINIRec.WriteFloat(sSecao, 'vAliqProd', PIS.vAliqProd);
        AINIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
      end;

    pis99:
      begin
        AINIRec.WriteFloat(sSecao, 'vBC', PIS.vBC);
        AINIRec.WriteFloat(sSecao, 'pPIS', PIS.pPIS);
        AINIRec.WriteFloat(sSecao, 'qBCProd', PIS.qBCProd);
        AINIRec.WriteFloat(sSecao, 'vAliqProd', PIS.vAliqProd);
        AINIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
      end;
  end;
end;

procedure TNFeIniWriter.Gerar_ImpostoPISST(AINIRec: TMemIniFile; PISST: TPISST;
  Idx: Integer);
var
  sSecao: string;
begin
  if (PISST.vBc > 0) then
  begin
    sSecao := 'PISST' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteFloat(sSecao, 'vBc', PISST.vBc);
    AINIRec.WriteFloat(sSecao, 'pPis', PISST.pPis);
    AINIRec.WriteFloat(sSecao, 'qBCProd', PISST.qBCProd);
    AINIRec.WriteFloat(sSecao, 'vAliqProd', PISST.vAliqProd);
    AINIRec.WriteFloat(sSecao, 'vPIS', PISST.vPIS);
    AINIRec.WriteString(sSecao, 'indSomaPISST', indSomaPISSTToStr(PISST.indSomaPISST));
  end;
end;

procedure TNFeIniWriter.Gerar_ImpostoCOFINS(AINIRec: TMemIniFile;
  COFINS: TCOFINS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'COFINS' + IntToStrZero(Idx + 1, 3);
  AINIRec.WriteString(sSecao, 'CST', CSTCOFINSToStr(COFINS.CST));
  case COFINS.CST of
    cof01, cof02:
      begin
        AINIRec.WriteFloat(sSecao, 'vBC', COFINS.vBC);
        AINIRec.WriteFloat(sSecao, 'pCOFINS', COFINS.pCOFINS);
        AINIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
      end;

    cof03:
      begin
        AINIRec.WriteFloat(sSecao, 'qBCProd', COFINS.qBCProd);
        AINIRec.WriteFloat(sSecao, 'vAliqProd', COFINS.vAliqProd);
        AINIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
      end;

    cof99:
      begin
        AINIRec.WriteFloat(sSecao, 'vBC', COFINS.vBC);
        AINIRec.WriteFloat(sSecao, 'pCOFINS', COFINS.pCOFINS);
        AINIRec.WriteFloat(sSecao, 'qBCProd', COFINS.qBCProd);
        AINIRec.WriteFloat(sSecao, 'vAliqProd', COFINS.vAliqProd);
        AINIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
      end;
  end;
end;

procedure TNFeIniWriter.Gerar_ImpostoCOFINSST(AINIRec: TMemIniFile;
  COFINSST: TCOFINSST; Idx: Integer);
var
  sSecao: string;
begin
  if (COFINSST.vBC > 0) then
  begin
    sSecao := 'COFINSST' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteFloat(sSecao, 'vBC', COFINSST.vBC);
    AINIRec.WriteFloat(sSecao, 'pCOFINS', COFINSST.pCOFINS);
    AINIRec.WriteFloat(sSecao, 'qBCProd', COFINSST.qBCProd);
    AINIRec.WriteFloat(sSecao, 'vAliqProd', COFINSST.vAliqProd);
    AINIRec.WriteFloat(sSecao, 'vCOFINS', COFINSST.vCOFINS);
    AINIRec.WriteString(sSecao, 'indSomaCOFINSST', indSomaCOFINSSTToStr(COFINSST.indSomaCOFINSST));
  end;
end;

procedure TNFeIniWriter.Gerar_ImpostoISSQN(AINIRec: TMemIniFile; ISSQN: TISSQN;
  Idx: Integer);
var
  sSecao: string;
begin
  if (ISSQN.vBC > 0) then
  begin
    sSecao := 'ISSQN' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteFloat(sSecao, 'vBC', ISSQN.vBC);
    AINIRec.WriteFloat(sSecao, 'vAliq', ISSQN.vAliq);
    AINIRec.WriteFloat(sSecao, 'vISSQN', ISSQN.vISSQN);
    AINIRec.WriteInteger(sSecao, 'cMunFG', ISSQN.cMunFG);
    AINIRec.WriteString(sSecao, 'cListServ', ISSQN.cListServ);
    AINIRec.WriteString(sSecao, 'cSitTrib', ISSQNcSitTribToStr(ISSQN.cSitTrib));
    AINIRec.WriteFloat(sSecao, 'vDeducao', ISSQN.vDeducao);
    AINIRec.WriteFloat(sSecao, 'vOutro', ISSQN.vOutro);
    AINIRec.WriteFloat(sSecao, 'vDescIncond', ISSQN.vDescIncond);
    AINIRec.WriteFloat(sSecao, 'vDescCond', ISSQN.vDescCond);
    AINIRec.WriteFloat(sSecao, 'vISSRet', ISSQN.vISSRet);
    AINIRec.WriteString(sSecao, 'indISS', indISSToStr(ISSQN.indISS));
    AINIRec.Writestring(sSecao, 'cServico', ISSQN.cServico);
    AINIRec.WriteInteger(sSecao, 'cMun', ISSQN.cMun);
    AINIRec.WriteInteger(sSecao, 'cPais', ISSQN.cPais);
    AINIRec.WriteString(sSecao, 'nProcesso', ISSQN.nProcesso);
    AINIRec.WriteString(sSecao, 'indIncentivo', indIncentivoToStr(ISSQN.indIncentivo));
  end;
end;

procedure TNFeIniWriter.Gerar_ObsContribuinte(AINIRec: TMemIniFile;
  obsCont: TobsItem; Idx: Integer);
var
  sSecao: string;
begin
  if (obsCont.xTexto <> '') then
  begin
    sSecao := 'obsContItem' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteString(sSecao, 'xCampo', obsCont.xCampo);
    AINIRec.WriteString(sSecao, 'xTexto', obsCont.xTexto);
  end;
end;

procedure TNFeIniWriter.Gerar_ObsFisco(AINIRec: TMemIniFile; obsFisco: TobsItem;
  Idx: Integer);
var
  sSecao: string;
begin
  if (obsFisco.xTexto <> '') then
  begin
    sSecao := 'obsFiscoItem' + IntToStrZero(Idx + 1, 3);
    AINIRec.WriteString(sSecao, 'xCampo', obsFisco.xCampo);
    AINIRec.WriteString(sSecao, 'xTexto', obsFisco.xTexto);
  end;
end;

procedure TNFeIniWriter.Gerar_Total(AINIRec: TMemIniFile; Total: TTotal);
begin
  AINIRec.WriteFloat('Total', 'vBC', Total.ICMSTot.vBC);
  AINIRec.WriteFloat('Total', 'vICMS', Total.ICMSTot.vICMS);
  AINIRec.WriteFloat('Total', 'vICMSDeson', Total.ICMSTot.vICMSDeson);
  AINIRec.WriteFloat('Total', 'vFCP', Total.ICMSTot.vFCP);
  AINIRec.WriteFloat('Total', 'vICMSUFDest', Total.ICMSTot.vICMSUFDest);
  AINIRec.WriteFloat('Total', 'vICMSUFRemet', Total.ICMSTot.vICMSUFRemet);
  AINIRec.WriteFloat('Total', 'vFCPUFDest', Total.ICMSTot.vFCPUFDest);
  AINIRec.WriteFloat('Total', 'vBCST', Total.ICMSTot.vBCST);
  AINIRec.WriteFloat('Total', 'vST', Total.ICMSTot.vST);
  AINIRec.WriteFloat('Total', 'vFCPST', Total.ICMSTot.vFCPST);
  AINIRec.WriteFloat('Total', 'vFCPSTRet', Total.ICMSTot.vFCPSTRet);
  AINIRec.WriteFloat('Total', 'qBCMono', Total.ICMSTot.qBCMono);
  AINIRec.WriteFloat('Total', 'vICMSMono', Total.ICMSTot.vICMSMono);
  AINIRec.WriteFloat('Total', 'qBCMonoReten', Total.ICMSTot.qBCMonoReten);
  AINIRec.WriteFloat('Total', 'vICMSMonoReten', Total.ICMSTot.vICMSMonoReten);
  AINIRec.WriteFloat('Total', 'qBCMonoRet', Total.ICMSTot.qBCMonoRet);
  AINIRec.WriteFloat('Total', 'vICMSMonoRet', Total.ICMSTot.vICMSMonoRet);
  AINIRec.WriteFloat('Total', 'vProd', Total.ICMSTot.vProd);
  AINIRec.WriteFloat('Total', 'vFrete', Total.ICMSTot.vFrete);
  AINIRec.WriteFloat('Total', 'vSeg', Total.ICMSTot.vSeg);
  AINIRec.WriteFloat('Total', 'vDesc', Total.ICMSTot.vDesc);
  AINIRec.WriteFloat('Total', 'vII', Total.ICMSTot.vII);
  AINIRec.WriteFloat('Total', 'vIPI', Total.ICMSTot.vIPI);
  AINIRec.WriteFloat('Total', 'vIPIDevol', Total.ICMSTot.vIPIDevol);
  AINIRec.WriteFloat('Total', 'vPIS', Total.ICMSTot.vPIS);
  AINIRec.WriteFloat('Total', 'vCOFINS', Total.ICMSTot.vCOFINS);
  AINIRec.WriteFloat('Total', 'vOutro', Total.ICMSTot.vOutro);
  AINIRec.WriteFloat('Total', 'vNF', Total.ICMSTot.vNF);
  AINIRec.WriteFloat('Total', 'vTotTrib', Total.ICMSTot.vTotTrib);
  AINIRec.WriteFloat('Total', 'vNFTot', Total.vNFTot);

  // Reforma Tributária
  Gerar_ISTot(AINIRec, Total.ISTot);
  Gerar_IBSCBSTot(AINIRec, Total.IBSCBSTot);
end;

procedure TNFeIniWriter.Gerar_ISSQNtot(AINIRec: TMemIniFile;
  ISSQNtot: TISSQNtot);
begin
  AINIRec.WriteFloat('ISSQNtot', 'vServ', ISSQNtot.vServ);
  AINIRec.WriteFloat('ISSQNtot', 'vBC', ISSQNTot.vBC);
  AINIRec.WriteFloat('ISSQNtot', 'vISS', ISSQNTot.vISS);
  AINIRec.WriteFloat('ISSQNtot', 'vPIS', ISSQNTot.vPIS);
  AINIRec.WriteFloat('ISSQNtot', 'vCOFINS', ISSQNTot.vCOFINS);
  AINIRec.WriteDateTime('ISSQNtot', 'dCompet', ISSQNTot.dCompet);
  AINIRec.WriteFloat('ISSQNtot', 'vDeducao', ISSQNTot.vDeducao);
  AINIRec.WriteFloat('ISSQNtot', 'vOutro', ISSQNTot.vOutro);
  AINIRec.WriteFloat('ISSQNtot', 'vDescIncond', ISSQNTot.vDescIncond);
  AINIRec.WriteFloat('ISSQNtot', 'vDescCond', ISSQNTot.vDescCond);
  AINIRec.WriteFloat('ISSQNtot', 'vISSRet', ISSQNTot.vISSRet);
  AINIRec.WriteString('ISSQNtot', 'cRegTrib', RegTribISSQNToStr(ISSQNTot.cRegTrib));
end;

procedure TNFeIniWriter.Gerar_RetTributo(AINIRec: TMemIniFile;
  retTrib: TretTrib);
begin
  AINIRec.WriteFloat('retTrib', 'vRetPIS', retTrib.vRetPIS);
  AINIRec.WriteFloat('retTrib', 'vRetCOFINS', retTrib.vRetCOFINS);
  AINIRec.WriteFloat('retTrib', 'vRetCSLL', retTrib.vRetCSLL);
  AINIRec.WriteFloat('retTrib', 'vBCIRRF', retTrib.vBCIRRF);
  AINIRec.WriteFloat('retTrib', 'vIRRF', retTrib.vIRRF);
  AINIRec.WriteFloat('retTrib', 'vBCRetPrev', retTrib.vBCRetPrev);
  AINIRec.WriteFloat('retTrib', 'vRetPrev', retTrib.vRetPrev);
end;

procedure TNFeIniWriter.Gerar_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to autXML.Count - 1 do
  begin
    sSecao := 'autXML' + IntToStrZero(J + 1, 2);

    AINIRec.WriteString(sSecao, 'CNPJCPF', autXML[J].CNPJCPF);
  end;
end;

procedure TNFeIniWriter.Gerar_Transportador(AINIRec: TMemIniFile;
  Transp: TTransp);
begin
  AINIRec.WriteString('Transportador', 'modFrete', modFreteToStr(Transp.modFrete));
  AINIRec.WriteString('Transportador', 'CNPJCPF', Transp.Transporta.CNPJCPF);
  AINIRec.WriteString('Transportador', 'xNome', Transp.Transporta.xNome);
  AINIRec.WriteString('Transportador', 'IE', Transp.Transporta.IE);
  AINIRec.WriteString('Transportador', 'xEnder', Transp.Transporta.xEnder);
  AINIRec.WriteString('Transportador', 'xMun', Transp.Transporta.xMun);
  AINIRec.WriteString('Transportador', 'UF', Transp.Transporta.UF);
  AINIRec.WriteFloat('Transportador', 'vServ', Transp.retTransp.vServ);
  AINIRec.WriteFloat('Transportador', 'vBCRet', Transp.retTransp.vBCRet);
  AINIRec.WriteFloat('Transportador', 'pICMSRet', Transp.retTransp.pICMSRet);
  AINIRec.WriteFloat('Transportador', 'vICMSRet', Transp.retTransp.vICMSRet);
  AINIRec.WriteString('Transportador', 'CFOP', Transp.retTransp.CFOP);
  AINIRec.WriteInteger('Transportador', 'cMunFG', Transp.retTransp.cMunFG);
  AINIRec.WriteString('Transportador', 'Placa', Transp.veicTransp.placa);
  AINIRec.WriteString('Transportador', 'UFPlaca', Transp.veicTransp.UF);
  AINIRec.WriteString('Transportador', 'RNTC', Transp.veicTransp.RNTC);
  AINIRec.WriteString('Transportador', 'vagao', Transp.vagao);
  AINIRec.WriteString('Transportador', 'balsa', Transp.balsa);
end;

procedure TNFeIniWriter.Gerar_Reboque(AINIRec: TMemIniFile;
  Reboque: TReboqueCollection);
var
  J: Integer;
  sSecao: string;
begin
  for J := 0 to Reboque.Count - 1 do
  begin
    sSecao := 'Reboque' + IntToStrZero(J + 1, 3);

    AINIRec.WriteString(sSecao, 'placa', Reboque[J].placa);
    AINIRec.WriteString(sSecao, 'UF', Reboque[J].UF);
    AINIRec.WriteString(sSecao, 'RNTC', Reboque[J].RNTC);
  end;
end;

procedure TNFeIniWriter.Gerar_Volume(AINIRec: TMemIniFile; vol: TVolCollection);
var
  I, J: Integer;
  sSecao: string;
begin
  for I := 0 to Vol.Count - 1 do
  begin
    sSecao := 'Volume' + IntToStrZero(I + 1, 3);

    AINIRec.WriteInteger(sSecao, 'qVol', Vol[I].qVol);
    AINIRec.WriteString(sSecao, 'esp', Vol[I].esp);
    AINIRec.WriteString(sSecao, 'marca', Vol[I].marca);
    AINIRec.WriteString(sSecao, 'nVol', Vol[I].nVol);
    AINIRec.WriteFloat(sSecao, 'pesoL', Vol[I].pesoL);
    AINIRec.WriteFloat(sSecao, 'pesoB', Vol[I].pesoB);

    for J := 0 to Vol[I].Lacres.Count - 1 do
    begin
      sSecao := 'Lacre' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
      AINIRec.WriteString(sSecao, 'nLacre', Vol[I].Lacres[J].nLacre);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_Fatura(AINIRec: TMemIniFile; Fat: TFat);
begin
  AINIRec.WriteString('Fatura', 'nFat', Fat.nFat);
  AINIRec.WriteFloat('Fatura', 'vOrig', Fat.vOrig);
  AINIRec.WriteFloat('Fatura', 'vDesc', Fat.vDesc);
  AINIRec.WriteFloat('Fatura', 'vLiq', Fat.vLiq);
end;

procedure TNFeIniWriter.Gerar_Duplicata(AINIRec: TMemIniFile;
  Dup: TDupCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to Dup.Count - 1 do
  begin
    sSecao := 'Duplicata' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'nDup', Dup[I].nDup);
    AINIRec.WriteString(sSecao, 'dVenc', DateToStr(Dup[I].dVenc));
    AINIRec.WriteFloat(sSecao, 'vDup', Dup[I].vDup);
  end;
end;

procedure TNFeIniWriter.Gerar_Pagamento(AINIRec: TMemIniFile;
  pag: TpagCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to pag.Count - 1 do
  begin
    sSecao := 'pag' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'tPag', FormaPagamentoToStr(pag[I].tPag));
    AINIRec.WriteString(sSecao, 'xPag', pag[I].xPag);
    AINIRec.WriteFloat(sSecao, 'vPag', pag[I].vPag);
    AINIRec.WriteString(sSecao, 'dPag', DateToStr(pag[I].dPag));
    AINIRec.WriteString(sSecao, 'CNPJPag', pag[I].CNPJPag);
    AINIRec.WriteString(sSecao, 'UFPag', pag[I].UFPag);

    AINIRec.WriteString(sSecao, 'indPag', IndpagToStrEX(pag[I].indPag));
    AINIRec.WriteString(sSecao, 'tpIntegra', tpIntegraToStr(pag[I].tpIntegra));
    AINIRec.WriteString(sSecao, 'CNPJ', pag[I].CNPJ);
    AINIRec.WriteString(sSecao, 'tBand', BandeiraCartaoToStr(pag[I].tBand));
    AINIRec.WriteString(sSecao, 'cAut', pag[I].cAut);
    AINIRec.WriteString(sSecao, 'CNPJReceb', pag[I].CNPJReceb);
    AINIRec.WriteString(sSecao, 'idTermPag', pag[I].idTermPag);
  end;

  AINIRec.WriteFloat(sSecao, 'vTroco', pag.vTroco);
end;

procedure TNFeIniWriter.Gerar_InfIntermediario(AINIRec: TMemIniFile;
  infIntermed: TinfIntermed);
begin
  AINIRec.WriteString('infIntermed', 'CNPJ', infIntermed.CNPJ);
  AINIRec.WriteString('infIntermed', 'idCadIntTran', infIntermed.idCadIntTran);
end;

procedure TNFeIniWriter.Gerar_DadosAdicionais(AINIRec: TMemIniFile;
  InfAdic: TInfAdic);
begin
  AINIRec.WriteString('DadosAdicionais', 'infAdFisco', InfAdic.infAdFisco);
  AINIRec.WriteString('DadosAdicionais', 'infCpl', InfAdic.infCpl);
end;

procedure TNFeIniWriter.Gerar_InfAdicionaisContribuinte(AINIRec: TMemIniFile;
  obsCont: TobsContCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to obsCont.Count - 1 do
  begin
    sSecao := 'InfAdic' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'xCampo', obsCont[I].xCampo);
    AINIRec.WriteString(sSecao, 'xTexto', obsCont[I].xTexto);
  end;
end;

procedure TNFeIniWriter.Gerar_InfAdicionaisFisco(AINIRec: TMemIniFile;
  obsFisco: TobsFiscoCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to obsFisco.Count - 1 do
  begin
    sSecao := 'ObsFisco' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'xCampo', obsFisco[I].xCampo);
    AINIRec.WriteString(sSecao, 'xTexto', obsFisco[I].xTexto);
  end;
end;

procedure TNFeIniWriter.Gerar_ProcRef(AINIRec: TMemIniFile;
  procRef: TprocRefCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to procRef.Count - 1 do
  begin
    sSecao := 'procRef' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'nProc', procRef[I].nProc);
    AINIRec.WriteString(sSecao, 'indProc', indProcToStr(procRef[I].indProc));
    AINIRec.WriteString(sSecao, 'tpAto', tpAtoToStr(procRef[I].tpAto));
  end;
end;

procedure TNFeIniWriter.Gerar_Exporta(AINIRec: TMemIniFile; exporta: TExporta);
begin
  if (exporta.UFembarq <> '') or (exporta.UFSaidaPais <> '') then
  begin
    AINIRec.WriteString('Exporta', 'UFembarq', exporta.UFembarq);
    AINIRec.WriteString('Exporta', 'xLocEmbarq', exporta.xLocEmbarq);

    AINIRec.WriteString('Exporta', 'UFSaidaPais', exporta.UFSaidaPais);
    AINIRec.WriteString('Exporta', 'xLocExporta', exporta.xLocExporta);
    AINIRec.WriteString('Exporta', 'xLocDespacho', exporta.xLocDespacho);
  end;
end;

procedure TNFeIniWriter.Gerar_Compra(AINIRec: TMemIniFile; compra: TCompra);
begin
  if (compra.xNEmp <> '') then
  begin
    AINIRec.WriteString('Compra', 'xNEmp', compra.xNEmp);
    AINIRec.WriteString('Compra', 'xPed', compra.xPed);
    AINIRec.WriteString('Compra', 'xCont', compra.xCont);
  end;
end;

procedure TNFeIniWriter.Gerar_Cana(AINIRec: TMemIniFile; cana: Tcana);
begin
  AINIRec.WriteString('cana', 'safra', cana.safra);
  AINIRec.WriteString('cana', 'ref', cana.ref);
  AINIRec.WriteFloat('cana', 'qTotMes', cana.qTotMes);
  AINIRec.WriteFloat('cana', 'qTotAnt', cana.qTotAnt);
  AINIRec.WriteFloat('cana', 'qTotGer', cana.qTotGer);
  AINIRec.WriteFloat('cana', 'vFor', cana.vFor);
  AINIRec.WriteFloat('cana', 'vTotDed', cana.vTotDed);
  AINIRec.WriteFloat('cana', 'vLiqFor', cana.vLiqFor);
end;

procedure TNFeIniWriter.Gerar_ForDia(AINIRec: TMemIniFile;
  fordia: TForDiaCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to fordia.Count - 1 do
  begin
    sSecao := 'forDia' + IntToStrZero(I + 1, 3);

    AINIRec.WriteInteger(sSecao, 'dia', fordia[I].dia);
    AINIRec.WriteFloat(sSecao, 'qtde', fordia[I].qtde);
  end;
end;

procedure TNFeIniWriter.Gerar_Deducao(AINIRec: TMemIniFile;
  deduc: TDeducCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to deduc.Count - 1 do
  begin
    sSecao := 'deduc' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'xDed', deduc[I].xDed);
    AINIRec.WriteFloat(sSecao, 'vDed', deduc[I].vDed);
  end;
end;

procedure TNFeIniWriter.Gerar_infNFeSupl(AINIRec: TMemIniFile;
  infNFeSupl: TinfNFeSupl);
begin
  AINIRec.WriteString('infNFeSupl', 'qrCode', infNFeSupl.qrCode);
  AINIRec.WriteString('infNFeSupl', 'urlChave', infNFeSupl.urlChave);
end;

procedure TNFeIniWriter.Gerar_infRespTec(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
begin
  AINIRec.WriteString('infRespTec', 'CNPJ', infRespTec.CNPJ);
  AINIRec.WriteString('infRespTec', 'xContato', infRespTec.xContato);
  AINIRec.WriteString('infRespTec', 'email', infRespTec.email);
  AINIRec.WriteString('infRespTec', 'fone', infRespTec.fone);
end;

procedure TNFeIniWriter.Gerar_Defensivo(AINIRec: TMemIniFile;
  defensivo: TdefensivoCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to defensivo.Count - 1 do
  begin
    sSecao := 'defensivo' + IntToStrZero(I + 1, 2);

    AINIRec.WriteString(sSecao, 'nReceituario', defensivo[I].nReceituario);
    AINIRec.WriteString(sSecao, 'CPFRespTec', defensivo[I].CPFRespTec);
  end;
end;

procedure TNFeIniWriter.Gerar_GuiaTransito(AINIRec: TMemIniFile;
  guiaTransito: TguiaTransito);
begin
  if guiaTransito.tpGuia <> tpgNenhum then
  begin
    AINIRec.WriteString('guiaTransito', 'UFGuia', guiaTransito.UFGuia);
    AINIRec.WriteString('guiaTransito', 'tpGuia', TtpGuiaToStr(guiaTransito.tpGuia));
    AINIRec.WriteString('guiaTransito', 'serieGuia', guiaTransito.serieGuia);
    AINIRec.WriteString('guiaTransito', 'nGuia', guiaTransito.nGuia);
  end;
end;

procedure TNFeIniWriter.Gerar_ProcNFe(AINIRec: TMemIniFile; procNFe: TProcNFe);
begin
  if procNFe.nProt <> '' then
  begin
    AINIRec.WriteString('procNFe', 'tpAmb', TpAmbToStr(procNFe.tpAmb));
    AINIRec.WriteString('procNFe', 'verAplic', procNFe.verAplic);
    AINIRec.WriteString('procNFe', 'chNFe', procNFe.chNFe);
    AINIRec.WriteString('procNFe', 'dhRecbto', DateTimeToStr(procNFe.dhRecbto));
    AINIRec.WriteString('procNFe', 'nProt', procNFe.nProt);
    AINIRec.WriteString('procNFe', 'digVal', procNFe.digVal);
    AINIRec.WriteString('procNFe', 'cStat', IntToStr(procNFe.cStat));
    AINIRec.WriteString('procNFe', 'xMotivo', procNFe.xMotivo);
  end;
end;

// Reforma Tributária
procedure TNFeIniWriter.Gerar_gPagAntecipado(AINIRec: TMemIniFile;
  gPagAntecipado: TgPagAntecipadoCollection);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to gPagAntecipado.Count - 1 do
  begin
    with gPagAntecipado[I] do
    begin
      sSecao := 'gPagAntecipado' + IntToStrZero(I + 1, 2);
      AINIRec.WriteString(sSecao, 'refNFe', refNFe);
    end;
  end;
end;

procedure TNFeIniWriter.Gerar_ISel(AINIRec: TMemIniFile; ISel: TgIS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'IS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'CSTIS', CSTISToStr(ISel.CSTIS));
  AINIRec.WriteString(sSecao, 'cClassTribIS', cClassTribISToStr(ISel.cClassTribIS));
  AINIRec.WriteFloat(sSecao, 'vBCIS', ISel.vBCIS);
  AINIRec.WriteFloat(sSecao, 'pIS', ISel.pIS);
  AINIRec.WriteFloat(sSecao, 'pISEspec', ISel.pISEspec);
  AINIRec.WriteString(sSecao, 'uTrib', ISel.uTrib);
  AINIRec.WriteFloat(sSecao, 'qTrib', ISel.qTrib);
  AINIRec.WriteFloat(sSecao, 'vIS', ISel.vIS);
end;

procedure TNFeIniWriter.Gerar_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'IBSCBS' + IntToStrZero(Idx + 1, 3);

  if (IBSCBS.gIBSCBS.vBC > 0) or (IBSCBS.gIBSCBSMono.adRemIBS > 0) or
     (IBSCBS.gTransfCred.vIBS > 0) or (IBSCBS.gTransfCred.vCBS > 0) then
  begin
    AINIRec.WriteString(sSecao, 'CST', CSTIBSCBSToStr(IBSCBS.CST));
    AINIRec.WriteString(sSecao, 'cClassTrib', cClassTribToStr(IBSCBS.cClassTrib));

    if IBSCBS.gIBSCBS.vBC > 0 then
      Gerar_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS, Idx)
    else
    if IBSCBS.gIBSCBSMono.adRemIBS > 0 then
      Gerar_IBSCBS_gIBSCBSMono(AINIRec, IBSCBS.gIBSCBSMono, Idx)
    else
    if (NFe.Ide.modelo = 55) and (IBSCBS.CST = cst800) then
      Gerar_IBSCBS_gTransfCred(AINIRec, IBSCBS.gTransfCred, Idx);

    if (NFe.Ide.modelo = 55) and (IBSCBS.gCredPresIBSZFM.tpCredPresIBSZFM <> tcpNenhum) then
      Gerar_IBSCBS_gCredPresIBSZFM(AINIRec, IBSCBS.gCredPresIBSZFM, Idx);
  end;
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS(AINIRec: TMemIniFile;
  IBSCBS: TgIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'vBC', IBSCBS.vBC);
  AINIRec.WriteFloat(sSecao, 'vIBS', IBSCBS.vIBS);

  Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec, IBSCBS.gIBSUF, Idx);
  Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec, IBSCBS.gIBSMun, Idx);
  Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec, IBSCBS.gCBS, Idx);

  if IBSCBS.gTribRegular.pAliqEfetRegIBSUF > 0 then
    Gerar_IBSCBS_gIBSCBS_gTribRegular(AINIRec, IBSCBS.gTribRegular, Idx);

  if IBSCBS.gIBSCredPres.pCredPres > 0 then
    Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, IBSCBS.gIBSCredPres, 'gIBSCredPres', Idx);

  if IBSCBS.gCBSCredPres.pCredPres > 0 then
    Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, IBSCBS.gCBSCredPres, 'gCBSCredPres', Idx);

  if IBSCBS.gTribCompraGov.pAliqIBSUF > 0 then
    Gerar_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec, IBSCBS.gTribCompraGov, Idx);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile;
  gIBSUF: TgIBSUF; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'pIBSUF', gIBSUF.pIBSUF);
  AINIRec.WriteFloat(sSecao, 'vIBSUF', gIBSUF.vIBSUF);

  AINIRec.WriteFloat(sSecao, 'pDif', gIBSUF.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gIBSUF.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSUF.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gIBSUF.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gIBSUF.gRed.pAliqEfet);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile;
  gIBSMun: TgIBSMun; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'pIBSMun', gIBSMun.pIBSMun);
  AINIRec.WriteFloat(sSecao, 'vIBSMun', gIBSMun.vIBSMun);

  AINIRec.WriteFloat(sSecao, 'pDif', gIBSMun.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gIBSMun.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSMun.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gIBSMun.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gIBSMun.gRed.pAliqEfet);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBS' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'pCBS', gCBS.pCBS);
  AINIRec.WriteFloat(sSecao, 'vCBS', gCBS.vCBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gCBS.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gCBS.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gCBS.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gCBS.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gCBS.gRed.pAliqEfet);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS_gTribRegular(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'CSTReg', CSTIBSCBSToStr(gTribRegular.CSTReg));
  AINIRec.WriteString(sSecao, 'cClassTribReg', cClassTribToStr(gTribRegular.cClassTribReg));
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegIBSUF', gTribRegular.pAliqEfetRegIBSUF);
  AINIRec.WriteFloat(sSecao, 'vTribRegIBSUF', gTribRegular.vTribRegIBSUF);
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegIBSMun', gTribRegular.pAliqEfetRegIBSMun);
  AINIRec.WriteFloat(sSecao, 'vTribRegIBSMun', gTribRegular.vTribRegIBSMun);
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegCBS', gTribRegular.pAliqEfetRegCBS);
  AINIRec.WriteFloat(sSecao, 'vTribRegCBS', gTribRegular.vTribRegCBS);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(
  AINIRec: TMemIniFile; IBSCredPres: TgIBSCBSCredPres; const Grupo: string;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := Grupo + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'cCredPres', cCredPresToStr(IBSCredPres.cCredPres));
  AINIRec.WriteFloat(sSecao, 'pCredPres', IBSCredPres.pCredPres);

  if IBSCredPres.vCredPres > 0 then
    AINIRec.WriteFloat(sSecao, 'vCredPres', IBSCredPres.vCredPres)
  else
    AINIRec.WriteFloat(sSecao, 'vCredPresCondSus', IBSCredPres.vCredPresCondSus);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBS_gTribCompraGov(
  AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'pAliqIBSUF', gTribCompraGov.pAliqIBSUF);
  AINIRec.WriteFloat(sSecao, 'vTribIBSUF', gTribCompraGov.vTribIBSUF);
  AINIRec.WriteFloat(sSecao, 'pAliqIBSMun', gTribCompraGov.pAliqIBSMun);
  AINIRec.WriteFloat(sSecao, 'vTribIBSMun', gTribCompraGov.vTribIBSMun);
  AINIRec.WriteFloat(sSecao, 'pAliqCBS', gTribCompraGov.pAliqCBS);
  AINIRec.WriteFloat(sSecao, 'vTribCBS', gTribCompraGov.vTribCBS);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gIBSCBSMono(AINIRec: TMemIniFile;
  IBSCBSMono: TgIBSCBSMono; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBSMono' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'qBCMono', IBSCBSMono.qBCMono);
  AINIRec.WriteFloat(sSecao, 'adRemIBS', IBSCBSMono.adRemIBS);
  AINIRec.WriteFloat(sSecao, 'adRemCBS', IBSCBSMono.adRemCBS);
  AINIRec.WriteFloat(sSecao, 'vIBSMono', IBSCBSMono.vIBSMono);
  AINIRec.WriteFloat(sSecao, 'vCBSMono', IBSCBSMono.vCBSMono);
  AINIRec.WriteFloat(sSecao, 'qBCMonoReten', IBSCBSMono.qBCMonoReten);
  AINIRec.WriteFloat(sSecao, 'adRemIBSReten', IBSCBSMono.adRemIBSReten);
  AINIRec.WriteFloat(sSecao, 'vIBSMonoReten', IBSCBSMono.vIBSMonoReten);
  AINIRec.WriteFloat(sSecao, 'adRemCBSReten', IBSCBSMono.adRemCBSReten);
  AINIRec.WriteFloat(sSecao, 'vCBSMonoReten', IBSCBSMono.vCBSMonoReten);
  AINIRec.WriteFloat(sSecao, 'qBCMonoRet', IBSCBSMono.qBCMonoRet);
  AINIRec.WriteFloat(sSecao, 'adRemIBSRet', IBSCBSMono.adRemIBSRet);
  AINIRec.WriteFloat(sSecao, 'vIBSMonoRet', IBSCBSMono.vIBSMonoRet);
  AINIRec.WriteFloat(sSecao, 'adRemCBSRet', IBSCBSMono.adRemCBSRet);
  AINIRec.WriteFloat(sSecao, 'vCBSMonoRet', IBSCBSMono.vCBSMonoRet);
  AINIRec.WriteFloat(sSecao, 'pDifIBS', IBSCBSMono.pDifIBS);
  AINIRec.WriteFloat(sSecao, 'vIBSMonoDif', IBSCBSMono.vIBSMonoDif);
  AINIRec.WriteFloat(sSecao, 'pDifCBS', IBSCBSMono.pDifCBS);
  AINIRec.WriteFloat(sSecao, 'vCBSMonoDif', IBSCBSMono.vCBSMonoDif);
  AINIRec.WriteFloat(sSecao, 'vTotIBSMonoItem', IBSCBSMono.vTotIBSMonoItem);
  AINIRec.WriteFloat(sSecao, 'vTotCBSMonoItem', IBSCBSMono.vTotCBSMonoItem);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gTransfCred(AINIRec: TMemIniFile;
  gTransfCred: TgTransfCred; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTransfCred' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteFloat(sSecao, 'vIBS', gTransfCred.vIBS);
  AINIRec.WriteFloat(sSecao, 'vCBS', gTransfCred.vCBS);
end;

procedure TNFeIniWriter.Gerar_IBSCBS_gCredPresIBSZFM(AINIRec: TMemIniFile;
  gCredPresIBSZFM: TCredPresIBSZFM; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCredPresIBSZFM' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'tpCredPresIBSZFM', TpCredPresIBSZFMToStr(gCredPresIBSZFM.tpCredPresIBSZFM));
  AINIRec.WriteFloat(sSecao, 'vCredPresIBSZFM', gCredPresIBSZFM.vCredPresIBSZFM);
end;

procedure TNFeIniWriter.Gerar_Det_DFeReferenciado(AINIRec: TMemIniFile;
  DFeReferenciado: TDFeReferenciado; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'DFeReferenciado' + IntToStrZero(Idx + 1, 3);

  AINIRec.WriteString(sSecao, 'chaveAcesso', DFeReferenciado.chaveAcesso);
  AINIRec.WriteInteger(sSecao, 'nItem', DFeReferenciado.nItem);
end;

procedure TNFeIniWriter.Gerar_ISTot(AINIRec: TMemIniFile; ISTot: TISTot);
var
  sSecao: string;
begin
  sSecao := 'ISTot';

  if ISTot.vIS > 0 then
    AINIRec.WriteFloat(sSecao, 'vIS', ISTot.vIS);
end;

procedure TNFeIniWriter.Gerar_IBSCBSTot(AINIRec: TMemIniFile;
  IBSCBSTot: TIBSCBSTot);
var
  sSecao: string;
begin
  sSecao := 'IBSCBSTot';

  if IBSCBSTot.vBCIBSCBS > 0 then
  begin
    AINIRec.WriteFloat(sSecao, 'vBCIBSCBS', IBSCBSTot.vBCIBSCBS);

    if IBSCBSTot.gIBS.vIBS > 0 then
      Gerar_IBSCBSTot_gIBS(AINIRec, IBSCBSTot.gIBS);

    if IBSCBSTot.gCBS.vDif > 0 then
      Gerar_IBSCBSTot_gCBS(AINIRec, IBSCBSTot.gCBS);

    if IBSCBSTot.gMono.vIBSMono > 0 then
      Gerar_IBSCBSTot_gMono(AINIRec, IBSCBSTot.gMono);
  end;
end;

procedure TNFeIniWriter.Gerar_IBSCBSTot_gIBS(AINIRec: TMemIniFile;
  IBS: TgIBSTot);
var
  sSecao: string;
begin
  sSecao := 'gIBS';

  AINIRec.WriteFloat(sSecao, 'vIBS', IBS.vIBS);
  AINIRec.WriteFloat(sSecao, 'vCredPres', IBS.vCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPresCondSus', IBS.vCredPresCondSus);

  Gerar_IBSCBSTot_gIBS_gIBSUFTot(AINIRec, IBS.gIBSUFTot);
  Gerar_IBSCBSTot_gIBS_gIBSMunTot(AINIRec, IBS.gIBSMunTot);
end;

procedure TNFeIniWriter.Gerar_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile;
  gIBSUFTot: TgIBSUFTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSUFTot';

  AINIRec.WriteFloat(sSecao, 'vDif', gIBSUFTot.vDif);
  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSUFTot.vDevTrib);
  AINIRec.WriteFloat(sSecao, 'vIBSUF', gIBSUFTot.vIBSUF);
end;

procedure TNFeIniWriter.Gerar_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile;
  gIBSMunTot: TgIBSMunTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSMunTot';

  AINIRec.WriteFloat(sSecao, 'vDif', gIBSMunTot.vDif);
  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSMunTot.vDevTrib);
  AINIRec.WriteFloat(sSecao, 'vIBSMun', gIBSMunTot.vIBSMun);
end;

procedure TNFeIniWriter.Gerar_IBSCBSTot_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBSTot);
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

procedure TNFeIniWriter.Gerar_IBSCBSTot_gMono(AINIRec: TMemIniFile;
  gMono: TgMono);
var
  sSecao: string;
begin
  sSecao := 'gMono';

  AINIRec.WriteFloat(sSecao, 'vIBSMono', gMono.vIBSMono);
  AINIRec.WriteFloat(sSecao, 'vCBSMono', gMono.vCBSMono);
  AINIRec.WriteFloat(sSecao, 'vIBSMonoReten', gMono.vIBSMonoReten);
  AINIRec.WriteFloat(sSecao, 'vCBSMonoReten', gMono.vCBSMonoReten);
  AINIRec.WriteFloat(sSecao, 'vIBSMonoRet', gMono.vIBSMonoRet);
  AINIRec.WriteFloat(sSecao, 'vCBSMonoRet', gMono.vCBSMonoRet);
end;

end.
