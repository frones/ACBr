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

unit ACBrNFe.IniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrNFe.Classes,
  pcnConversao,
  pcnConversaoNFe;

type
  { TNFeIniReader }

  TNFeIniReader = class
  private
    FNFe: TNFe;
    FVersaoDF: TpcnVersaoDF;
    FAmbiente: Integer;
    FtpEmis: Integer;

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_NFReferencia(AINIRec: TMemIniFile; NFref: TNFrefCollection);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Ler_Avulsa(AINIRec: TMemIniFile; Avulsa: TAvulsa);
    procedure Ler_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
    procedure Ler_Retirada(AINIRec: TMemIniFile; Retirada: TRetirada);
    procedure Ler_Entrega(AINIRec: TMemIniFile; Entrega: TEntrega);
    procedure Ler_AutorizadosXML(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_Produto(AINIRec: TMemIniFile; Det: TDetCollection);
    procedure Ler_ProdutogCred(AINIRec: TMemIniFile; CredPresumido: TCredPresumidoCollection; Idx: Integer);
    procedure Ler_ProdutoNVE(AINIRec: TMemIniFile; NVE: TNVECollection; Idx: Integer);
    procedure Ler_ProdutoRastro(AINIRec: TMemIniFile; rastro: TRastroCollection; Idx: Integer);
    procedure Ler_ProdutoDI(AINIRec: TMemIniFile; DI: TDICollection; Idx: Integer);
    procedure Ler_ProdutoADI(AINIRec: TMemIniFile; adi: TAdiCollection; Idx1, Idx2: Integer);

    procedure Ler_ProdutoDetExport(AINIRec: TMemIniFile; detExport: TdetExportCollection; Idx: Integer);
    procedure Ler_ProdutoVeiculo(AINIRec: TMemIniFile; veicProd: TveicProd; Idx: Integer);
    procedure Ler_ProdutoMedicamento(AINIRec: TMemIniFile; med: TMedCollection; Idx: Integer);
    procedure Ler_ProdutoArma(AINIRec: TMemIniFile; arma: TArmaCollection; Idx: Integer);
    procedure Ler_ProdutoCombustivel(AINIRec: TMemIniFile; comb: TComb; Idx: Integer);
    procedure Ler_ProdutoCombustivelOrigemComb(AINIRec: TMemIniFile; origComb: TorigCombCollection; Idx: Integer);
    procedure Ler_ProdutoCombustivelCIDE(AINIRec: TMemIniFile; CIDE: TCIDE; Idx: Integer);
    procedure Ler_ProdutoCombustivelEncerrante(AINIRec: TMemIniFile; encerrante: Tencerrante; Idx: Integer);
    procedure Ler_ProdutoCombustivelICMSComb(AINIRec: TMemIniFile; ICMS: TICMSComb; Idx: Integer);
    procedure Ler_ProdutoCombustivelICMSInter(AINIRec: TMemIniFile; ICMSInter: TICMSInter; Idx: Integer);
    procedure Ler_ProdutoCombustivelICMSCons(AINIRec: TMemIniFile; ICMSCons: TICMSCons; Idx: Integer);

    procedure Ler_ProdutoImposto(AINIRec: TMemIniFile; Imposto: TImposto; Idx: Integer);
    procedure Ler_ImpostoICMS(AINIRec: TMemIniFile; ICMS: TICMS; Idx: Integer);
    procedure Ler_ImpostoICMSUFDest(AINIRec: TMemIniFile; ICMSUFDest: TICMSUFDest; Idx: Integer);
    procedure Ler_ImpostoIPI(AINIRec: TMemIniFile; IPI: TIPI; Idx: Integer);
    procedure Ler_ImpostoII(AINIRec: TMemIniFile; II: TII; Idx: Integer);
    procedure Ler_ImpostoPIS(AINIRec: TMemIniFile; PIS: TPIS; Idx: Integer);
    procedure Ler_ImpostoPISST(AINIRec: TMemIniFile; PISST: TPISST; Idx: Integer);
    procedure Ler_ImpostoCOFINS(AINIRec: TMemIniFile; COFINS: TCOFINS; Idx: Integer);
    procedure Ler_ImpostoCOFINSST(AINIRec: TMemIniFile; COFINSST: TCOFINSST; Idx: Integer);
    procedure Ler_ImpostoISSQN(AINIRec: TMemIniFile; ISSQN: TISSQN; Idx: Integer);

    procedure Ler_ProdutoObsContItem(AINIRec: TMemIniFile; obsCont: TobsItem; Idx: Integer);
    procedure Ler_ProdutoObsFiscoItem(AINIRec: TMemIniFile; obsFisco: TobsItem; Idx: Integer);

    procedure Ler_Total(AINIRec: TMemIniFile; ICMSTot: TICMSTot);
    procedure Ler_TotalISSQNTot(AINIRec: TMemIniFile; ISSQNtot: TISSQNtot);
    procedure Ler_TotalRetTributavel(AINIRec: TMemIniFile; retTrib: TretTrib);
    procedure Ler_Transportador(AINIRec: TMemIniFile; Transp: TTransp);
    procedure Ler_Fatura(AINIRec: TMemIniFile; Fat: TFat);
    procedure Ler_Duplicata(AINIRec: TMemIniFile; Dup: TDupCollection);
    procedure Ler_Pagamento(AINIRec: TMemIniFile; pag: TpagCollection);
    procedure Ler_InfIntermed(AINIRec: TMemIniFile; infIntermed: TinfIntermed);
    procedure Ler_DadosAdicionais(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Ler_ObsContribuinte(AINIRec: TMemIniFile; obsCont: TobsContCollection);
    procedure Ler_ObsFisco(AINIRec: TMemIniFile; obsFisco: TobsFiscoCollection);
    procedure Ler_ProcessoReferenciado(AINIRec: TMemIniFile; procRef: TprocRefCollection);
    procedure Ler_Exportacao(AINIRec: TMemIniFile; exporta: TExporta);
    procedure Ler_Compra(AINIRec: TMemIniFile; compra: TCompra);
    procedure Ler_Cana(AINIRec: TMemIniFile; cana: Tcana);
    procedure Ler_CanaForDia(AINIRec: TMemIniFile; fordia: TForDiaCollection);
    procedure Ler_CanaDeducao(AINIRec: TMemIniFile; deduc: TDeducCollection);
    procedure Ler_InfNFeSupl(AINIRec: TMemIniFile; infNFeSupl: TinfNFeSupl);
    procedure Ler_InfRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Ler_Defensivo(AINIRec: TMemIniFile; defensivo: TdefensivoCollection);
    procedure Ler_GuiaTransito(AINIRec: TMemIniFile; guiaTransito: TguiaTransito);

    // Reforma Tributária
    procedure Ler_gPagAntecipado(AINIRec: TMemIniFile);

    procedure Ler_ISel(AINIRec: TMemIniFile; ISel: TgIS; Idx: Integer);
    procedure Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; IBSCBS: TgIBSCBS; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBSMono(AINIRec: TMemIniFile; IBSCBSMono: TgIBSCBSMono; Idx: Integer);
    procedure Ler_IBSCBS_gTransfCred(AINIRec: TMemIniFile; gTransfCred: TgTransfCred; Idx: Integer);
    procedure Ler_IBSCBS_gCredPresIBSZFM(AINIRec: TMemIniFile; gCredPresIBSZFM: TCredPresIBSZFM; Idx: Integer);

    procedure Ler_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUF; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMun; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile; gCBS: TgCBS; Idx: Integer);

    procedure Ler_IBSCBS_gIBSCBS_gTribRegular(AINIRec: TMemIniFile;
      gTribRegular: TgTribRegular; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec: TMemIniFile;
      IBSCredPres: TgIBSCBSCredPres; const Grupo: string; Idx: Integer);
    procedure Ler_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec: TMemIniFile;
      gTribCompraGov: TgTribCompraGov; Idx: Integer);

    procedure Ler_Det_DFeReferenciado(AINIRec: TMemIniFile;
      DFeReferenciado: TDFeReferenciado; Idx: Integer);

    procedure Ler_ISTot(AINIRec: TMemIniFile; ISTot: TISTot);
    procedure Ler_IBSCBSTot(AINIRec: TMemIniFile; IBSCBSTot: TIBSCBSTot);
    procedure Ler_IBSCBSTot_gIBS(AINIRec: TMemIniFile; IBS: TgIBSTot);
    procedure Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile; gIBSUFTot: TgIBSUFTot);
    procedure Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile; gIBSMunTot: TgIBSMunTot);
    procedure Ler_IBSCBSTot_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSTot);
    procedure Ler_IBSCBSTot_gMono(AINIRec: TMemIniFile; gMono: TgMono);
  public
    constructor Create(AOwner: TNFe); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property NFe: TNFe read FNFe write FNFe;
    property VersaoDF: TpcnVersaoDF read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  StrUtils,
  ACBrDFe.Conversao,
  ACBrXmlBase,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TNFeIniReader }

constructor TNFeIniReader.Create(AOwner: TNFe);
begin
  inherited Create;

  FNFe := AOwner;
end;

function TNFeIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FNFe.infNFe.versao := StringToFloatDef(INIRec.ReadString('infNFe', 'versao', VersaoDFToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FNFe.Ide);
    Ler_NFReferencia(INIRec, FNFe.Ide.NFref);
    Ler_Emitente(INIRec, FNFe.Emit);
    Ler_Avulsa(INIRec, FNFe.Avulsa);
    Ler_Destinatario(INIRec, FNFe.Dest);
    Ler_Retirada(INIRec, FNFe.Retirada);
    Ler_Entrega(INIRec, FNFe.Entrega);
    Ler_AutorizadosXML(INIRec, FNFe.autXML);
    Ler_Produto(INIRec, FNFe.Det);
    Ler_Total(INIRec, FNFe.Total.ICMSTot);
    Ler_TotalISSQNTot(INIRec, FNFe.Total.ISSQNtot);
    Ler_TotalRetTributavel(INIRec, FNFe.Total.retTrib);
    Ler_Transportador(INIRec, FNFe.Transp);
    Ler_Fatura(INIRec, FNFe.Cobr.Fat);
    Ler_Duplicata(INIRec, FNFe.Cobr.Dup);
    Ler_Pagamento(INIRec, FNFe.pag);
    Ler_InfIntermed(INIRec, FNFe.infIntermed);
    Ler_DadosAdicionais(INIRec, FNFe.InfAdic);
    Ler_ObsContribuinte(INIRec, FNFe.InfAdic.obsCont);
    Ler_ObsFisco(INIRec, FNFe.InfAdic.obsFisco);
    Ler_ProcessoReferenciado(INIRec, FNFe.InfAdic.procRef);
    Ler_Exportacao(INIRec, FNFe.exporta);
    Ler_Compra(INIRec, FNFe.compra);
    Ler_Cana(INIRec, FNFe.cana);
    Ler_CanaForDia(INIRec, FNFe.cana.fordia);
    Ler_CanaDeducao(INIRec, FNFe.cana.deduc);
    Ler_InfNFeSupl(INIRec, FNFe.infNFeSupl);
    Ler_InfRespTec(INIRec, FNFe.infRespTec);
    Ler_Defensivo(INIRec, FNFe.agropecuario.defensivo);
    Ler_GuiaTransito(INIRec, FNFe.agropecuario.guiaTransito);
  finally
    INIRec.Free;
  end;
end;

procedure TNFeIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  OK: Boolean;
  sSecao: string;
begin
  sSecao := IfThen( AINIRec.SectionExists('Identificacao'), 'Identificacao', 'ide');

  Ide.cNF     := AINIRec.ReadInteger( sSecao,'cNF' ,AINIRec.ReadInteger( sSecao,'Codigo' ,0));
  Ide.natOp   := AINIRec.ReadString(  sSecao,'natOp' ,AINIRec.ReadString(  sSecao,'NaturezaOperacao' ,''));
  Ide.indPag  := StrToIndpagEX(AINIRec.ReadString( sSecao,'indPag',AINIRec.ReadString( sSecao,'FormaPag','0')));
  Ide.modelo  := AINIRec.ReadInteger( sSecao,'mod' ,AINIRec.ReadInteger( sSecao,'Modelo' ,65));
  Ide.serie   := AINIRec.ReadInteger( sSecao,'Serie'  ,1);
  Ide.nNF     := AINIRec.ReadInteger( sSecao,'nNF' ,AINIRec.ReadInteger( sSecao,'Numero' ,0));
  Ide.dEmi    := StringToDateTime(AINIRec.ReadString( sSecao,'dhEmi',AINIRec.ReadString( sSecao,'dEmi',AINIRec.ReadString( sSecao,'Emissao','0'))));
  Ide.dSaiEnt := StringToDateTime(AINIRec.ReadString( sSecao,'dhSaiEnt'  ,AINIRec.ReadString( sSecao,'dSaiEnt'  ,AINIRec.ReadString( sSecao,'Saida'  ,'0'))));
  Ide.hSaiEnt := StringToDateTime(AINIRec.ReadString( sSecao,'hSaiEnt','0'));
  Ide.tpNF    := StrToTpNF(OK,AINIRec.ReadString( sSecao,'tpNF',AINIRec.ReadString( sSecao,'Tipo','1')));
  Ide.idDest  := StrToDestinoOperacao(OK,AINIRec.ReadString( sSecao,'idDest','1'));
  Ide.tpImp   := StrToTpImp(  OK, AINIRec.ReadString( sSecao,'tpImp','1'));
  Ide.tpEmis  := StrToTpEmis(OK, AINIRec.ReadString(sSecao, 'tpEmis', IntToStr(tpEmis)));
  Ide.tpAmb   := StrToTpAmb(OK, AINIRec.ReadString(sSecao, 'tpAmb', IntToStr(Ambiente)));
  Ide.finNFe   := StrToFinNFe( OK,AINIRec.ReadString( sSecao,'finNFe',AINIRec.ReadString( sSecao,'Finalidade','0')));
  Ide.indFinal := StrToConsumidorFinal(OK,AINIRec.ReadString( sSecao,'indFinal','0'));
  Ide.indPres  := StrToPresencaComprador(OK,AINIRec.ReadString( sSecao,'indPres','0'));
  Ide.indIntermed := StrToIndIntermed(OK, AINIRec.ReadString( sSecao,'indIntermed', ''));
  Ide.procEmi  := StrToProcEmi(OK,AINIRec.ReadString( sSecao,'procEmi','0'));
  Ide.verProc  := AINIRec.ReadString(  sSecao, 'verProc' ,'ACBrNFe');
  Ide.dhCont   := StringToDateTime(AINIRec.ReadString( sSecao,'dhCont'  ,'0'));
  Ide.xJust    := AINIRec.ReadString(  sSecao,'xJust' ,'' );
  Ide.cMunFG   := AINIRec.ReadInteger( sSecao,'cMunFG', 0);

  // Reforma Tributária
  Ide.cMunFGIBS := AINIRec.ReadInteger(sSecao, 'cMunFGIBS', 0);
  Ide.tpNFDebito := StrTotpNFDebito(AINIRec.ReadString(sSecao, 'tpNFDebito', ''));
  Ide.tpNFCredito := StrTotpNFCredito(AINIRec.ReadString(sSecao, 'tpNFCredito', ''));

  Ide.gCompraGov.pRedutor := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedutor', ''), 0);

  if Ide.gCompraGov.pRedutor > 0 then
    Ide.gCompraGov.tpEnteGov := StrTotpEnteGov(AINIRec.ReadString(sSecao, 'tpEnteGov', ''));

  Ide.gCompraGov.tpOperGov := StrTotpOperGov(AINIRec.ReadString(sSecao, 'tpOperGov', ''));

  Ler_gPagAntecipado(AINIRec);
end;

procedure TNFeIniReader.Ler_NFReferencia(AINIRec: TMemIniFile;
  NFref: TNFrefCollection);
var
  I: Integer;
  sSecao, sFim, sType: string;
  ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'NFref'+IntToStrZero(I,3);
    sFim     := AINIRec.ReadString(  sSecao,'Tipo'  ,'FIM');
    sType    := UpperCase(AINIRec.ReadString(  sSecao,'Tipo'  ,'NFe'));  //NF NFe NFP CTe ECF

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
    begin
      if (AINIRec.ReadString(sSecao,'refNFe','') <> '') or
         (AINIRec.ReadString(sSecao,'refNFeSig','') <> '') then
        sType := 'NFE'
      else if AINIRec.ReadString(  sSecao,'refCTe'  ,'') <> '' then
        sType := 'CTE'
      else if AINIRec.ReadString(  sSecao,'nECF'  ,'') <> '' then
        sType :=  'ECF'
      else if AINIRec.ReadString(  sSecao,'IE'    ,'') <> '' then
        sType := 'NFP'
      else if AINIRec.ReadString(  sSecao,'CNPJ'  ,'') <> '' then
        sType := 'NF'
      else
        break;
    end;

    with NFref.New do
    begin
      if (sType = 'NFE') or (sType = 'SAT') then
      begin
        refNFe :=  AINIRec.ReadString(sSecao,'refNFe','');
        refNFeSig :=  AINIRec.ReadString(sSecao,'refNFeSig','');
      end
      else if sType = 'NF' then
      begin
        RefNF.cUF    := AINIRec.ReadInteger( sSecao,'cUF'   ,0);
        RefNF.AAMM   := AINIRec.ReadString(  sSecao,'AAMM'  ,'');
        RefNF.CNPJ   := AINIRec.ReadString(  sSecao,'CNPJ'  ,'');
        RefNF.modelo := AINIRec.ReadInteger( sSecao,'mod'   ,AINIRec.ReadInteger( sSecao,'Modelo',0));
        RefNF.serie  := AINIRec.ReadInteger( sSecao,'Serie' ,0);
        RefNF.nNF    := AINIRec.ReadInteger( sSecao,'nNF'   ,0);
      end

      else if sType = 'NFP' then
      begin
        RefNFP.cUF    := AINIRec.ReadInteger( sSecao,'cUF'   ,0);
        RefNFP.AAMM   := AINIRec.ReadString(  sSecao,'AAMM'  ,'');
        RefNFP.CNPJCPF:= AINIRec.ReadString(  sSecao,'CNPJ'  ,AINIRec.ReadString(sSecao,'CPF',AINIRec.ReadString(sSecao,'CNPJCPF','')));
        RefNFP.IE     := AINIRec.ReadString(  sSecao,'IE'    ,'');
        RefNFP.Modelo := AINIRec.ReadString(  sSecao,'mod'   ,AINIRec.ReadString(  sSecao,'Modelo',''));
        RefNFP.serie  := AINIRec.ReadInteger( sSecao,'Serie' ,0);
        RefNFP.nNF    := AINIRec.ReadInteger( sSecao,'nNF'   ,0);
      end

      else if sType = 'CTE' then
        refCTe         := AINIRec.ReadString(  sSecao,'refCTe'  ,'')

      else if sType = 'ECF' then
      begin
        RefECF.modelo := StrToECFModRef(ok,AINIRec.ReadString(  sSecao,'mod'  ,AINIRec.ReadString(  sSecao,'ModECF'  ,'')));
        RefECF.nECF   := AINIRec.ReadString(  sSecao,'nECF'  ,'');
        RefECF.nCOO   := AINIRec.ReadString(  sSecao,'nCOO'  ,'');
      end;
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  sSecao: string;
  ok: Boolean;
begin
  sSecao := IfThen( AINIRec.SectionExists('Emitente'), 'Emitente', 'emit');

  Emit.CNPJCPF := AINIRec.ReadString( sSecao,'CNPJ'    ,AINIRec.ReadString( sSecao,'CNPJCPF', AINIRec.ReadString(  sSecao,'CPF','')));
  Emit.xNome   := AINIRec.ReadString( sSecao,'xNome'   ,AINIRec.ReadString( sSecao,'Razao'  , ''));
  Emit.xFant   := AINIRec.ReadString( sSecao,'xFant'   ,AINIRec.ReadString( sSecao,'Fantasia'  , ''));
  Emit.IE      := AINIRec.ReadString( sSecao,'IE'  ,'');
  Emit.IEST    := AINIRec.ReadString( sSecao,'IEST','');
  Emit.IM      := AINIRec.ReadString( sSecao,'IM'  ,'');
  Emit.CNAE    := AINIRec.ReadString( sSecao,'CNAE','');
  Emit.CRT     := StrToCRT(ok, AINIRec.ReadString( sSecao,'CRT','3'));

  Emit.EnderEmit.xLgr := AINIRec.ReadString( sSecao, 'xLgr' ,AINIRec.ReadString(  sSecao, 'Logradouro', ''));

  if (AINIRec.ReadString( sSecao,'nro', '') <> '') or (AINIRec.ReadString( sSecao, 'Numero', '') <> '') then
    Emit.EnderEmit.nro := AINIRec.ReadString( sSecao,'nro', AINIRec.ReadString( sSecao, 'Numero', ''));

  if (AINIRec.ReadString( sSecao, 'xCpl', '') <> '') or (AINIRec.ReadString( sSecao, 'Complemento', '') <> '') then
    Emit.EnderEmit.xCpl := AINIRec.ReadString( sSecao, 'xCpl', AINIRec.ReadString( sSecao, 'Complemento', ''));

  Emit.EnderEmit.xBairro := AINIRec.ReadString(  sSecao,'xBairro' ,AINIRec.ReadString(  sSecao,'Bairro',''));
  Emit.EnderEmit.cMun    := AINIRec.ReadInteger( sSecao,'cMun'    ,AINIRec.ReadInteger( sSecao,'CidadeCod'   ,0));
  Emit.EnderEmit.xMun    := AINIRec.ReadString(  sSecao,'xMun'    ,AINIRec.ReadString(  sSecao,'Cidade'   ,''));
  Emit.EnderEmit.UF      := AINIRec.ReadString(  sSecao,'UF'      ,'');
  Emit.EnderEmit.CEP     := AINIRec.ReadInteger( sSecao,'CEP'     ,0);
  Emit.EnderEmit.cPais   := AINIRec.ReadInteger( sSecao,'cPais'   ,AINIRec.ReadInteger( sSecao,'PaisCod'    ,1058));
  Emit.EnderEmit.xPais   := AINIRec.ReadString(  sSecao,'xPais'   ,AINIRec.ReadString(  sSecao,'Pais'    ,'BRASIL'));
  Emit.EnderEmit.fone    := AINIRec.ReadString(  sSecao,'Fone'    ,'');

  FNFe.Ide.cUF := AINIRec.ReadInteger( sSecao,'cUF'       ,UFparaCodigoUF(Emit.EnderEmit.UF));

  if (FNFe.Ide.cMunFG = 0) then
    FNFe.Ide.cMunFG := AINIRec.ReadInteger( sSecao,'cMunFG' ,AINIRec.ReadInteger( sSecao,'CidadeCod' ,Emit.EnderEmit.cMun));
end;

procedure TNFeIniReader.Ler_Avulsa(AINIRec: TMemIniFile; Avulsa: TAvulsa);
begin
  if AINIRec.ReadString( 'Avulsa', 'CNPJ', '') <> '' then
  begin
    Avulsa.CNPJ    := AINIRec.ReadString( 'Avulsa', 'CNPJ', '');
    Avulsa.xOrgao  := AINIRec.ReadString( 'Avulsa', 'xOrgao', '');
    Avulsa.matr    := AINIRec.ReadString( 'Avulsa', 'matr', '');
    Avulsa.xAgente := AINIRec.ReadString( 'Avulsa', 'xAgente', '');
    Avulsa.fone    := AINIRec.ReadString( 'Avulsa', 'fone', '');
    Avulsa.UF      := AINIRec.ReadString( 'Avulsa', 'UF', '');
    Avulsa.nDAR    := AINIRec.ReadString( 'Avulsa', 'nDAR', '');
    Avulsa.dEmi    := StringToDateTime(AINIRec.ReadString( 'Avulsa', 'dEmi', '0'));
    Avulsa.vDAR    := StringToFloatDef(AINIRec.ReadString( 'Avulsa', 'vDAR', ''), 0);
    Avulsa.repEmi  := AINIRec.ReadString( 'Avulsa', 'repEmi','');
    Avulsa.dPag    := StringToDateTime(AINIRec.ReadString( 'Avulsa', 'dPag', '0'));
  end;
end;

procedure TNFeIniReader.Ler_Destinatario(AINIRec: TMemIniFile; Dest: TDest);
var
  sSecao: string;
  ok: Boolean;
begin
  sSecao := IfThen( AINIRec.SectionExists('Destinatario'),'Destinatario','dest');

  Dest.idEstrangeiro     := AINIRec.ReadString(  sSecao,'idEstrangeiro','');
  Dest.CNPJCPF           := AINIRec.ReadString(  sSecao,'CNPJ'       ,AINIRec.ReadString(  sSecao,'CNPJCPF',AINIRec.ReadString(  sSecao,'CPF','')));
  Dest.xNome             := AINIRec.ReadString(  sSecao,'xNome'  ,AINIRec.ReadString(  sSecao,'NomeRazao'  ,''));
  Dest.indIEDest         := StrToindIEDest(OK,AINIRec.ReadString( sSecao,'indIEDest','1'));
  Dest.IE                := AINIRec.ReadString(  sSecao,'IE'         ,'');
  Dest.IM                := AINIRec.ReadString(  sSecao,'IM'         ,'');
  Dest.ISUF              := AINIRec.ReadString(  sSecao,'ISUF'       ,'');
  Dest.Email             := AINIRec.ReadString(  sSecao,'Email'      ,'');

  Dest.EnderDest.xLgr := AINIRec.ReadString(  sSecao, 'xLgr' ,AINIRec.ReadString( sSecao, 'Logradouro', ''));

  if (AINIRec.ReadString(sSecao, 'nro', '') <> '') or (AINIRec.ReadString(sSecao, 'Numero', '') <> '') then
    Dest.EnderDest.nro := AINIRec.ReadString(  sSecao, 'nro', AINIRec.ReadString(sSecao, 'Numero', ''));

  if (AINIRec.ReadString(sSecao, 'xCpl', '') <> '') or (AINIRec.ReadString(sSecao, 'Complemento', '') <> '') then
    Dest.EnderDest.xCpl := AINIRec.ReadString( sSecao, 'xCpl', AINIRec.ReadString(sSecao,'Complemento',''));

  Dest.EnderDest.xBairro := AINIRec.ReadString(  sSecao,'xBairro'   ,AINIRec.ReadString(  sSecao,'Bairro',''));
  Dest.EnderDest.cMun    := AINIRec.ReadInteger( sSecao,'cMun'      ,AINIRec.ReadInteger( sSecao,'CidadeCod'   ,0));
  Dest.EnderDest.xMun    := AINIRec.ReadString(  sSecao,'xMun'      ,AINIRec.ReadString(  sSecao,'Cidade'   ,''));
  Dest.EnderDest.UF      := AINIRec.ReadString(  sSecao,'UF'         ,'');
  Dest.EnderDest.CEP     := AINIRec.ReadInteger( sSecao,'CEP'       ,0);
  Dest.EnderDest.cPais   := AINIRec.ReadInteger( sSecao,'cPais'     ,AINIRec.ReadInteger(sSecao,'PaisCod',1058));
  Dest.EnderDest.xPais   := AINIRec.ReadString(  sSecao,'xPais'       ,AINIRec.ReadString( sSecao,'Pais','BRASIL'));
  Dest.EnderDest.Fone    := AINIRec.ReadString(  sSecao,'Fone'       ,'');
end;

procedure TNFeIniReader.Ler_Retirada(AINIRec: TMemIniFile; Retirada: TRetirada);
var
  sLogradouro: string;
begin
  sLogradouro := AINIRec.ReadString( 'Retirada','xLgr','');

  if sLogradouro <> '' then
  begin
    Retirada.CNPJCPF := AINIRec.ReadString( 'Retirada','CNPJ',AINIRec.ReadString('Retirada','CPF',AINIRec.ReadString('Retirada','CNPJCPF','')));
    Retirada.xNome   := AINIRec.ReadString( 'Retirada','xNome','');
    Retirada.xLgr    := sLogradouro;
    Retirada.nro     := AINIRec.ReadString( 'Retirada','nro' ,'');
    Retirada.xCpl    := AINIRec.ReadString( 'Retirada','xCpl','');
    Retirada.xBairro := AINIRec.ReadString( 'Retirada','xBairro','');
    Retirada.cMun    := AINIRec.ReadInteger('Retirada','cMun',0);
    Retirada.xMun    := AINIRec.ReadString( 'Retirada','xMun','');
    Retirada.UF      := AINIRec.ReadString( 'Retirada','UF'  ,'');
    Retirada.CEP     := AINIRec.ReadInteger('Retirada','CEP',0);
    Retirada.cPais   := AINIRec.ReadInteger('Retirada','cPais',AINIRec.ReadInteger('Retirada','PaisCod',1058));
    Retirada.xPais   := AINIRec.ReadString( 'Retirada','xPais',AINIRec.ReadString( 'Retirada','Pais','BRASIL'));
    Retirada.Fone    := AINIRec.ReadString( 'Retirada','Fone','');
    Retirada.Email   := AINIRec.ReadString( 'Retirada','Email','');
    Retirada.IE      := AINIRec.ReadString( 'Retirada','IE'  ,'');
  end;
end;

procedure TNFeIniReader.Ler_Entrega(AINIRec: TMemIniFile; Entrega: TEntrega);
var
  sLogradouro: string;
begin
  sLogradouro := AINIRec.ReadString(  'Entrega','xLgr','');

  if sLogradouro <> '' then
  begin
    Entrega.CNPJCPF := AINIRec.ReadString(  'Entrega','CNPJ',AINIRec.ReadString('Entrega','CPF',AINIRec.ReadString('Entrega','CNPJCPF','')));
    Entrega.xNome   := AINIRec.ReadString( 'Entrega','xNome','');
    Entrega.xLgr    := sLogradouro;
    Entrega.nro     := AINIRec.ReadString(  'Entrega','nro' ,'');
    Entrega.xCpl    := AINIRec.ReadString(  'Entrega','xCpl','');
    Entrega.xBairro := AINIRec.ReadString(  'Entrega','xBairro','');
    Entrega.cMun    := AINIRec.ReadInteger( 'Entrega','cMun',0);
    Entrega.xMun    := AINIRec.ReadString(  'Entrega','xMun','');
    Entrega.UF      := AINIRec.ReadString(  'Entrega','UF','');
    Entrega.CEP     := AINIRec.ReadInteger('Entrega','CEP',0);
    Entrega.cPais   := AINIRec.ReadInteger('Entrega','cPais',AINIRec.ReadInteger('Entrega','PaisCod',1058));
    Entrega.xPais   := AINIRec.ReadString( 'Entrega','xPais',AINIRec.ReadString( 'Entrega','Pais','BRASIL'));
    Entrega.Fone    := AINIRec.ReadString( 'Entrega','Fone','');
    Entrega.Email   := AINIRec.ReadString( 'Entrega','Email','');
    Entrega.IE      := AINIRec.ReadString( 'Entrega','IE'  ,'');
  end;
end;

procedure TNFeIniReader.Ler_AutorizadosXML(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'autXML'+IntToStrZero(I,3);
    sFim     := OnlyNumber(AINIRec.ReadString( sSecao ,'CNPJ',AINIRec.ReadString(  sSecao,'CPF',AINIRec.ReadString(  sSecao,'CNPJCPF','FIM'))));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
    begin
      sSecao := 'autXML'+IntToStrZero(I,2);
      sFim     := OnlyNumber(AINIRec.ReadString( sSecao ,'CNPJ',AINIRec.ReadString(  sSecao,'CPF',AINIRec.ReadString(  sSecao,'CNPJCPF','FIM'))));
    end;
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with autXML.New do
      CNPJCPF := sFim;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_Produto(AINIRec: TMemIniFile; Det: TDetCollection);
var
  I: Integer;
  sSecao, sProdID, sFim: string;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := IfThen( AINIRec.SectionExists('Produto'+IntToStrZero(I,3)), 'Produto', 'det');
    sSecao := sSecao+IntToStrZero(I,3);
    sProdID  := AINIRec.ReadString(sSecao,'Codigo',AINIRec.ReadString( sSecao,'cProd','FIM'));
    if sProdID = 'FIM' then
      break;

    with Det.New do
    begin
      Prod.nItem := I;
      infAdProd  := AINIRec.ReadString(sSecao,'infAdProd','');

      Prod.cProd := AINIRec.ReadString( sSecao,'cProd'   ,AINIRec.ReadString( sSecao,'Codigo'   ,''));
      if (Length(AINIRec.ReadString( sSecao,'cEAN','')) > 0) or (Length(AINIRec.ReadString( sSecao,'EAN','')) > 0)  then
        Prod.cEAN := AINIRec.ReadString( sSecao,'cEAN'      ,AINIRec.ReadString( sSecao,'EAN'      ,''));

      Prod.cBarra   := AINIRec.ReadString( sSecao,'cBarra', '');
      Prod.xProd    := AINIRec.ReadString( sSecao,'xProd',AINIRec.ReadString( sSecao,'Descricao',''));
      Prod.NCM      := AINIRec.ReadString( sSecao,'NCM'      ,'');
      Prod.CEST     := AINIRec.ReadString( sSecao,'CEST'     ,'');
      Prod.indEscala:= StrToIndEscala(OK, AINIRec.ReadString( sSecao,'indEscala' ,'') );
      Prod.CNPJFab  := AINIRec.ReadString( sSecao,'CNPJFab'   ,'');
      Prod.cBenef   := AINIRec.ReadString( sSecao,'cBenef'    ,'');
      Prod.EXTIPI   := AINIRec.ReadString( sSecao,'EXTIPI'      ,'');
      Prod.CFOP     := AINIRec.ReadString( sSecao,'CFOP'     ,'');
      Prod.uCom     := AINIRec.ReadString( sSecao,'uCom'  ,AINIRec.ReadString( sSecao,'Unidade'  ,''));
      Prod.qCom     := StringToFloatDef( AINIRec.ReadString(sSecao,'qCom'   ,AINIRec.ReadString(sSecao,'Quantidade'  ,'')) ,0);
      Prod.vUnCom   := StringToFloatDef( AINIRec.ReadString(sSecao,'vUnCom' ,AINIRec.ReadString(sSecao,'ValorUnitario','')) ,0);
      Prod.vProd    := StringToFloatDef( AINIRec.ReadString(sSecao,'vProd'  ,AINIRec.ReadString(sSecao,'ValorTotal' ,'')) ,0);

      if Length(AINIRec.ReadString( sSecao,'cEANTrib','')) > 0 then
        Prod.cEANTrib      := AINIRec.ReadString( sSecao,'cEANTrib'      ,'');

      Prod.cBarraTrib := AINIRec.ReadString( sSecao,'cBarraTrib', '');
      Prod.uTrib     := AINIRec.ReadString( sSecao,'uTrib'  , Prod.uCom);
      Prod.qTrib     := StringToFloatDef( AINIRec.ReadString(sSecao,'qTrib'  ,''), Prod.qCom);
      Prod.vUnTrib   := StringToFloatDef( AINIRec.ReadString(sSecao,'vUnTrib','') ,Prod.vUnCom);
      Prod.vFrete    := StringToFloatDef( AINIRec.ReadString(sSecao,'vFrete','') ,0);
      Prod.vSeg      := StringToFloatDef( AINIRec.ReadString(sSecao,'vSeg','') ,0);
      Prod.vDesc     := StringToFloatDef( AINIRec.ReadString(sSecao,'vDesc',AINIRec.ReadString(sSecao,'ValorDesconto','')) ,0);
      Prod.vOutro    := StringToFloatDef( AINIRec.ReadString(sSecao,'vOutro','') ,0);
      Prod.IndTot    := StrToindTot(OK,AINIRec.ReadString(sSecao,'indTot','1'));
      Prod.xPed      := AINIRec.ReadString( sSecao,'xPed'    ,'');
      Prod.nItemPed  := AINIRec.ReadString( sSecao,'nItemPed','');
      Prod.nFCI      := AINIRec.ReadString( sSecao,'nFCI','');  //NFe3
      Prod.nRECOPI   := AINIRec.ReadString( sSecao,'nRECOPI','');  //NFe3

      pDevol    := StringToFloatDef( AINIRec.ReadString(sSecao,'pDevol','') ,0);
      vIPIDevol := StringToFloatDef( AINIRec.ReadString(sSecao,'vIPIDevol','') ,0);

      Imposto.vTotTrib := StringToFloatDef( AINIRec.ReadString(sSecao,'vTotTrib','') ,0);

      // Reforma Tributária
      Prod.indBemMovelUsado := StrToTIndicadorEx(OK, AINIRec.ReadString(sSecao, 'indBemMovelUsado', ''));

      vItem := StringToFloatDef( AINIRec.ReadString(sSecao,'vItem','') ,0);

      Ler_ProdutogCred(AINIRec, Prod.CredPresumido, I);
      Ler_ProdutoNVE(AINIRec, Prod.NVE, I);
      Ler_ProdutoRastro(AINIRec, Prod.rastro, I);
      Ler_ProdutoDI(AINIRec, Prod.DI, I);
      Ler_ProdutoDetExport(AINIRec, Prod.detExport, I);

      sSecao := 'impostoDevol'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString( sSecao,'pDevol','FIM');
      if ((sFim <> 'FIM') and ( Length(sFim) > 0 ))  then
      begin
        pDevol    := StringToFloatDef( AINIRec.ReadString(sSecao,'pDevol','') ,0);
        vIPIDevol := StringToFloatDef( AINIRec.ReadString(sSecao,'vIPIDevol','') ,0);
      end;

      Ler_ProdutoVeiculo(AINIRec, Prod.veicProd, I);
      Ler_ProdutoMedicamento(AINIRec, Prod.med, I);
      Ler_ProdutoArma(AINIRec, Prod.arma, I);
      Ler_ProdutoCombustivel(AINIRec, Prod.comb, I);
      Ler_ProdutoImposto(AINIRec, Imposto, I);
      Ler_ProdutoObsContItem(AINIRec, obsCont, I);
      Ler_ProdutoObsFiscoItem(AINIRec, obsFisco, I);

      // Reforma Tributária
      Ler_Det_DFeReferenciado(AINIRec, DFeReferenciado, I);
    end;

    Inc( I );
  end;
end;

procedure TNFeIniReader.Ler_ProdutogCred(AINIRec: TMemIniFile;
  CredPresumido: TCredPresumidoCollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
begin
  J := 1;
  while true do
  begin
    sSecao := 'gCred' + IntToStrZero(Idx,3) + IntToStrZero(J,1);
    sFim   := AINIRec.ReadString(sSecao, 'cCredPresumido', '');
    if (sFim <> '') then
      with CredPresumido.New do
      begin
        cCredPresumido := sFim;
        pCredPresumido := StringToFloatDef(AINIRec.ReadString(sSecao, 'pCredPresumido', ''), 0);
        vCredPresumido := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPresumido', ''), 0);
      end
    else
      Break;

    Inc(J);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoNVE(AINIRec: TMemIniFile;
  NVE: TNVECollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
begin
  J := 1;
  while true do
  begin
    sSecao := 'NVE'+IntToStrZero(Idx,3)+IntToStrZero(J,3);
    sFim     := AINIRec.ReadString(sSecao,'NVE','');
    if (sFim <> '') then
      NVE.New.NVE := sFim
    else
      Break;

    Inc(J);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoRastro(AINIRec: TMemIniFile;
  rastro: TRastroCollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
begin
  J := 1;
  while true do
  begin
    sSecao  := 'rastro'+IntToStrZero(Idx,3)+IntToStrZero(J,3);
    sFim    := AINIRec.ReadString(sSecao,'nLote','');
    if (sFim <> '') then
       with rastro.New do
       begin
         nLote    := sFim;
         qLote    := StringToFloatDef( AINIRec.ReadString( sSecao,'qLote',''), 0 );
         dFab     := StringToDateTime( AINIRec.ReadString( sSecao,'dFab','0') );
         dVal     := StringToDateTime( AINIRec.ReadString( sSecao,'dVal','0') );
         cAgreg   := AINIRec.ReadString( sSecao,'cAgreg','');
       end
    else
       Break;
    Inc(J);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoDI(AINIRec: TMemIniFile; DI: TDICollection;
  Idx: Integer);
var
  J: Integer;
  sSecao, sDINumber: string;
  Ok: Boolean;
begin
  J := 1;
  while true do
  begin
    sSecao  := 'DI'+IntToStrZero(Idx,3)+IntToStrZero(J,3);
    sDINumber := AINIRec.ReadString(sSecao,'NumeroDI',AINIRec.ReadString(sSecao,'nDi',''));

    if sDINumber <> '' then
    begin
      with DI.New do
      begin
        nDi         := sDINumber;
        dDi         := StringToDateTime(AINIRec.ReadString(sSecao,'dDi'  ,AINIRec.ReadString(sSecao,'DataRegistroDI'  ,'0')));
        xLocDesemb  := AINIRec.ReadString(sSecao,'xLocDesemb',AINIRec.ReadString(sSecao,'LocalDesembaraco',''));
        UFDesemb    := AINIRec.ReadString(sSecao,'UFDesemb'   ,AINIRec.ReadString(sSecao,'UFDesembaraco'   ,''));
        dDesemb     := StringToDateTime(AINIRec.ReadString(sSecao,'dDesemb',AINIRec.ReadString(sSecao,'DataDesembaraco','0')));

        tpViaTransp  := StrToTipoViaTransp(OK,AINIRec.ReadString(sSecao,'tpViaTransp',''));
        vAFRMM       := StringToFloatDef( AINIRec.ReadString(sSecao,'vAFRMM','') ,0);
        tpIntermedio := StrToTipoIntermedio(OK,AINIRec.ReadString(sSecao,'tpIntermedio',''));
        CNPJ         := AINIRec.ReadString(sSecao,'CNPJ','');
        UFTerceiro   := AINIRec.ReadString(sSecao,'UFTerceiro','');

        cExportador := AINIRec.ReadString(sSecao,'cExportador',AINIRec.ReadString(sSecao,'CodigoExportador',''));

        Ler_ProdutoADI(AINIRec, adi, Idx, J);
      end;
    end
    else
      Break;

    Inc(J);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoADI(AINIRec: TMemIniFile;
  adi: TAdiCollection; Idx1, Idx2: Integer);
var
  K: Integer;
  sSecao, sADINumber: string;
begin
  K := 1;
  while true do
  begin
    sSecao   := IfThen( AINIRec.SectionExists('LADI'+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+IntToStrZero(K,3)), 'LADI', 'adi');
    sSecao   := sSecao+IntToStrZero(Idx1,3)+IntToStrZero(Idx2,3)+IntToStrZero(K,3);
    sADINumber := AINIRec.ReadString(sSecao,'nAdicao',AINIRec.ReadString(sSecao,'NumeroAdicao','FIM'));
    if (sADINumber = 'FIM') or (Length(sADINumber) <= 0) then
      break;

    with adi.New do
    begin
      nAdicao     := StrToInt(sADINumber);
      nSeqAdi     := AINIRec.ReadInteger( sSecao,'nSeqAdi',K);
      cFabricante := AINIRec.ReadString(  sSecao,'cFabricante',AINIRec.ReadString(  sSecao,'CodigoFabricante',''));
      vDescDI     := StringToFloatDef( AINIRec.ReadString(sSecao,'vDescDI',AINIRec.ReadString(sSecao,'DescontoADI','')) ,0);
      nDraw       := AINIRec.ReadString( sSecao,'nDraw','');
    end;

    Inc(K)
  end;
end;

procedure TNFeIniReader.Ler_ProdutoDetExport(AINIRec: TMemIniFile;
  detExport: TdetExportCollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
begin
  J := 1;
  while true do
  begin
    sSecao := 'detExport'+IntToStrZero(Idx,3)+IntToStrZero(J,3);
    sFim     := AINIRec.ReadString(sSecao,'nRE','FIM');

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      sFim :=  AINIRec.ReadString( sSecao,'chNFe','FIM');

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
    begin
      sFim     := AINIRec.ReadString(sSecao,'nDraw','FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break;
    end;

    with detExport.New do
    begin
      nDraw   := AINIRec.ReadString( sSecao,'nDraw','');
      nRE     := AINIRec.ReadString( sSecao,'nRE','');
      chNFe   := AINIRec.ReadString( sSecao,'chNFe','');
      qExport := StringToFloatDef( AINIRec.ReadString(sSecao,'qExport','') ,0);
    end;

    Inc(J);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoVeiculo(AINIRec: TMemIniFile;
  veicProd: TveicProd; Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := IfThen( AINIRec.SectionExists('Veiculo'+IntToStrZero(Idx,3)), 'Veiculo', 'veicProd');
  sSecao := sSecao+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'Chassi','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    with veicProd do
    begin
      tpOP    := StrTotpOP(OK,AINIRec.ReadString( sSecao,'tpOP','0'));
      chassi  := sFim;
      cCor    := AINIRec.ReadString( sSecao,'cCor'   ,'');
      xCor    := AINIRec.ReadString( sSecao,'xCor'   ,'');
      pot     := AINIRec.ReadString( sSecao,'pot'    ,'');
      Cilin   := AINIRec.ReadString( sSecao,'Cilin'    ,AINIRec.ReadString( sSecao,'CM3'  ,''));
      pesoL   := AINIRec.ReadString( sSecao,'pesoL'  ,'');
      pesoB   := AINIRec.ReadString( sSecao,'pesoB'  ,'');
      nSerie  := AINIRec.ReadString( sSecao,'nSerie' ,'');
      tpComb  := AINIRec.ReadString( sSecao,'tpComb' ,'');
      nMotor  := AINIRec.ReadString( sSecao,'nMotor' ,'');
      CMT     := AINIRec.ReadString( sSecao,'CMT'   ,AINIRec.ReadString( sSecao,'CMKG'    ,''));
      dist    := AINIRec.ReadString( sSecao,'dist'   ,'');
      anoMod  := AINIRec.ReadInteger(sSecao,'anoMod' ,0);
      anoFab  := AINIRec.ReadInteger(sSecao,'anoFab' ,0);
      tpPint  := AINIRec.ReadString( sSecao,'tpPint' ,'');
      tpVeic  := AINIRec.ReadInteger(sSecao,'tpVeic' ,0);
      espVeic := AINIRec.ReadInteger(sSecao,'espVeic',0);
      VIN     := AINIRec.ReadString( sSecao,'VIN'    ,'');
      condVeic := StrTocondVeic(OK,AINIRec.ReadString( sSecao,'condVeic','1'));
      cMod    := AINIRec.ReadString( sSecao,'cMod'   ,'');
      cCorDENATRAN := AINIRec.ReadString( sSecao,'cCorDENATRAN','');
      lota    := AINIRec.ReadInteger(sSecao,'lota'   ,0);
      tpRest  := AINIRec.ReadInteger(sSecao,'tpRest' ,0);
    end;
  end;
end;

procedure TNFeIniReader.Ler_ProdutoMedicamento(AINIRec: TMemIniFile;
  med: TMedCollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
begin
  J := 1;
  while true do
  begin
    sSecao := IfThen( AINIRec.SectionExists('Medicamento'+IntToStrZero(Idx,3)+IntToStrZero(J,3)), 'Medicamento', 'med');
    sSecao := sSecao+IntToStrZero(Idx,3)+IntToStrZero(J,3);
    sFim     := AINIRec.ReadString(sSecao,'cProdANVISA','FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with med.New do
    begin
      nLote := AINIRec.ReadString(sSecao,'nLote','');
      cProdANVISA:=  sFim;
      xMotivoIsencao := AINIRec.ReadString(sSecao,'xMotivoIsencao','');
      qLote := StringToFloatDef(AINIRec.ReadString( sSecao,'qLote',''),0);
      dFab  := StringToDateTime(AINIRec.ReadString( sSecao,'dFab','0'));
      dVal  := StringToDateTime(AINIRec.ReadString( sSecao,'dVal','0'));
      vPMC  := StringToFloatDef(AINIRec.ReadString( sSecao,'vPMC',''),0);
    end;

    Inc(J)
  end;
end;

procedure TNFeIniReader.Ler_ProdutoArma(AINIRec: TMemIniFile;
  arma: TArmaCollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  J := 1;
  while true do
  begin
    sSecao := 'Arma'+IntToStrZero(Idx,3)+IntToStrZero(J,3);
    sFim := AINIRec.ReadString(sSecao,'nSerie','FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with arma.New do
    begin
      tpArma := StrTotpArma(OK,AINIRec.ReadString( sSecao,'tpArma','0'));
      nSerie := sFim;
      nCano  := AINIRec.ReadString( sSecao,'nCano','');
      descr  := AINIRec.ReadString( sSecao,'descr','');
    end;

    Inc(J)
  end;
end;

procedure TNFeIniReader.Ler_ProdutoCombustivel(AINIRec: TMemIniFile;
  comb: TComb; Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := IfThen( AINIRec.SectionExists('Combustivel'+IntToStrZero(Idx,3)), 'Combustivel', 'comb');
  sSecao := sSecao+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'cProdANP','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    comb.cProdANP := AINIRec.ReadInteger( sSecao,'cProdANP',0);
    comb.pMixGN   := StringToFloatDef(AINIRec.ReadString( sSecao,'pMixGN',''),0);
    comb.descANP  := AINIRec.ReadString(  sSecao,'descANP'   ,'');
    comb.pGLP     := StringToFloatDef( AINIRec.ReadString( sSecao,'pGLP'   ,''), 0);
    comb.pGNn     := StringToFloatDef( AINIRec.ReadString( sSecao,'pGNn'   ,''), 0);
    comb.pGNi     := StringToFloatDef( AINIRec.ReadString( sSecao,'pGNi'   ,''), 0);
    comb.vPart    := StringToFloatDef( AINIRec.ReadString( sSecao,'vPart'  ,''), 0);
    comb.CODIF    := AINIRec.ReadString(  sSecao,'CODIF'   ,'');
    comb.qTemp    := StringToFloatDef(AINIRec.ReadString( sSecao,'qTemp',''),0);
    comb.UFcons   := AINIRec.ReadString( sSecao,'UFCons','');
    comb.pBio     := StringToFloatDef(AINIRec.ReadString( sSecao,'pBio',''),0);

    Ler_ProdutoCombustivelOrigemComb(AINIRec, comb.origComb, Idx);
    Ler_ProdutoCombustivelCIDE(AINIRec, comb.CIDE, Idx);
    Ler_ProdutoCombustivelEncerrante(AINIRec, comb.encerrante, Idx);
    Ler_ProdutoCombustivelICMSComb(AINIRec, comb.ICMS, Idx);
    Ler_ProdutoCombustivelICMSInter(AINIRec, comb.ICMSInter, Idx);
    Ler_ProdutoCombustivelICMSCons(AINIRec, comb.ICMSCons, Idx);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoCombustivelOrigemComb(AINIRec: TMemIniFile;
  origComb: TorigCombCollection; Idx: Integer);
var
  J: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  J := 1;
  while true do
  begin
    sSecao := 'origComb' + IntToStrZero(Idx,3) + IntToStrZero(J,2);
    sFim   := AINIRec.ReadString(sSecao,'cUFOrig','FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with origComb.New do
    begin
      indImport := StrToindImport(OK, AINIRec.ReadString( sSecao,'indImport', '0'));
      cUFOrig := StrToIntDef(sFim, 0);
      pOrig  := StringToFloatDef(AINIRec.ReadString( sSecao,'pOrig',''),0);
    end;

    Inc(J)
  end;
end;

procedure TNFeIniReader.Ler_ProdutoCombustivelCIDE(AINIRec: TMemIniFile;
  CIDE: TCIDE; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'CIDE'+IntToStrZero(Idx,3);
  CIDE.qBCprod   := StringToFloatDef(AINIRec.ReadString( sSecao,'qBCprod'  ,''),0);
  CIDE.vAliqProd := StringToFloatDef(AINIRec.ReadString( sSecao,'vAliqProd',''),0);
  CIDE.vCIDE     := StringToFloatDef(AINIRec.ReadString( sSecao,'vCIDE'    ,''),0);
end;

procedure TNFeIniReader.Ler_ProdutoCombustivelEncerrante(AINIRec: TMemIniFile;
  encerrante: Tencerrante; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'encerrante'+IntToStrZero(Idx,3);
  encerrante.nBico    := AINIRec.ReadInteger( sSecao,'nBico'  ,0);
  encerrante.nBomba   := AINIRec.ReadInteger( sSecao,'nBomba' ,0);
  encerrante.nTanque  := AINIRec.ReadInteger( sSecao,'nTanque',0);
  encerrante.vEncIni  := StringToFloatDef(AINIRec.ReadString( sSecao,'vEncIni',''),0);
  encerrante.vEncFin  := StringToFloatDef(AINIRec.ReadString( sSecao,'vEncFin',''),0);
end;

procedure TNFeIniReader.Ler_ProdutoCombustivelICMSComb(AINIRec: TMemIniFile;
  ICMS: TICMSComb; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'ICMSComb'+IntToStrZero(Idx,3);
  ICMS.vBCICMS   := StringToFloatDef(AINIRec.ReadString( sSecao,'vBCICMS'  ,''),0);
  ICMS.vICMS     := StringToFloatDef(AINIRec.ReadString( sSecao,'vICMS'    ,''),0);
  ICMS.vBCICMSST := StringToFloatDef(AINIRec.ReadString( sSecao,'vBCICMSST',''),0);
  ICMS.vICMSST   := StringToFloatDef(AINIRec.ReadString( sSecao,'vICMSST'  ,''),0);
end;

procedure TNFeIniReader.Ler_ProdutoCombustivelICMSInter(AINIRec: TMemIniFile;
  ICMSInter: TICMSInter; Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'ICMSInter'+IntToStrZero(Idx,3);
  sFim := AINIRec.ReadString( sSecao,'vBCICMSSTDest','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    ICMSInter.vBCICMSSTDest := StringToFloatDef(sFim,0);
    ICMSInter.vICMSSTDest   := StringToFloatDef(AINIRec.ReadString( sSecao,'vICMSSTDest',''),0);
  end;
end;

procedure TNFeIniReader.Ler_ProdutoCombustivelICMSCons(AINIRec: TMemIniFile;
  ICMSCons: TICMSCons; Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'ICMSCons'+IntToStrZero(Idx,3);
  sFim   := AINIRec.ReadString( sSecao,'vBCICMSSTCons','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    ICMSCons.vBCICMSSTCons := StringToFloatDef(sFim,0);
    ICMSCons.vICMSSTCons   := StringToFloatDef(AINIRec.ReadString( sSecao,'vICMSSTCons',''),0);
    ICMSCons.UFcons        := AINIRec.ReadString( sSecao,'UFCons','');
  end;
end;

procedure TNFeIniReader.Ler_ProdutoImposto(AINIRec: TMemIniFile;
  Imposto: TImposto; Idx: Integer);
begin
  Ler_ImpostoICMS(AINIRec, Imposto.ICMS, Idx);
  Ler_ImpostoICMSUFDest(AINIRec, Imposto.ICMSUFDest, Idx);
  Ler_ImpostoIPI(AINIRec, Imposto.IPI, Idx);
  Ler_ImpostoII(AINIRec, Imposto.II, Idx);
  Ler_ImpostoPIS(AINIRec, Imposto.PIS, Idx);
  Ler_ImpostoPISST(AINIRec, Imposto.PISST, Idx);
  Ler_ImpostoCOFINS(AINIRec, Imposto.COFINS, Idx);
  Ler_ImpostoCOFINSST(AINIRec, Imposto.COFINSST, Idx);
  Ler_ImpostoISSQN(AINIRec, Imposto.ISSQN, Idx);

  // Reforma Tributária
  Ler_ISel(AINIRec, Imposto.ISel, Idx);
  Ler_IBSCBS(AINIRec, Imposto.IBSCBS, Idx);
end;

procedure TNFeIniReader.Ler_ImpostoICMS(AINIRec: TMemIniFile; ICMS: TICMS;
  Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'ICMS'+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'CST','FIM');
  if (sFim = 'FIM') or ( Length(sFim) = 0 ) then
    sFim     := AINIRec.ReadString(sSecao,'CSOSN','FIM');

  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    ICMS.orig       := StrToOrig(     OK, AINIRec.ReadString(sSecao,'orig'    ,AINIRec.ReadString(sSecao,'Origem'    ,'0' ) ));
    ICMS.CST        := StrToCSTICMS(  OK, AINIRec.ReadString(sSecao,'CST'     ,'00'));
    ICMS.CSOSN      := StrToCSOSNIcms(OK, AINIRec.ReadString(sSecao,'CSOSN'   ,''  ));
    ICMS.modBC      := StrTomodBC(    OK, AINIRec.ReadString(sSecao,'modBC'   ,AINIRec.ReadString(sSecao,'Modalidade','0' ) ));
    ICMS.pRedBC     := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedBC'   ,AINIRec.ReadString(sSecao,'PercentualReducao','')) ,0);
    ICMS.cBenefRBC  := AINIRec.ReadString(sSecao, 'cBenefRBC', '');
    ICMS.vBC        := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'      ,AINIRec.ReadString(sSecao,'ValorBase'  ,'')) ,0);
    ICMS.pICMS      := StringToFloatDef( AINIRec.ReadString(sSecao,'pICMS'    ,AINIRec.ReadString(sSecao,'Aliquota','')) ,0);
    ICMS.vICMS      := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMS'    ,AINIRec.ReadString(sSecao,'Valor','')) ,0);
    ICMS.vBCFCP     := StringToFloatDef( AINIRec.ReadString( sSecao,'vBCFCP'  ,AINIRec.ReadString(sSecao,'ValorBaseFCP','')) ,0);
    ICMS.pFCP       := StringToFloatDef( AINIRec.ReadString( sSecao,'pFCP'    ,AINIRec.ReadString(sSecao,'PercentualFCP','')) ,0);
    ICMS.vFCP       := StringToFloatDef( AINIRec.ReadString( sSecao,'vFCP'    ,AINIRec.ReadString(sSecao,'ValorFCP','')) ,0);
    ICMS.modBCST    := StrTomodBCST(OK, AINIRec.ReadString(sSecao,'modBCST'   ,AINIRec.ReadString(sSecao,'ModalidadeST','0')));
    ICMS.pMVAST     := StringToFloatDef( AINIRec.ReadString(sSecao,'pMVAST'   ,AINIRec.ReadString(sSecao,'PercentualMargemST' ,'')) ,0);
    ICMS.pRedBCST   := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedBCST' ,AINIRec.ReadString(sSecao,'PercentualReducaoST','')) ,0);
    ICMS.vBCST      := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCST'    ,AINIRec.ReadString(sSecao,'ValorBaseST','')) ,0);
    ICMS.pICMSST    := StringToFloatDef( AINIRec.ReadString(sSecao,'pICMSST'  ,AINIRec.ReadString(sSecao,'AliquotaST' ,'')) ,0);
    ICMS.vICMSST    := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSST'  ,AINIRec.ReadString(sSecao,'ValorST'    ,'')) ,0);
    ICMS.vBCFCPST   := StringToFloatDef( AINIRec.ReadString( sSecao,'vBCFCPST',AINIRec.ReadString(sSecao,'ValorBaseFCPST','')) ,0);
    ICMS.pFCPST     := StringToFloatDef( AINIRec.ReadString( sSecao,'pFCPST'  ,AINIRec.ReadString(sSecao,'PercentualFCPST','')) ,0);
    ICMS.vFCPST     := StringToFloatDef( AINIRec.ReadString( sSecao,'vFCPST'  ,AINIRec.ReadString(sSecao,'ValorFCPST','')) ,0);
    ICMS.UFST       := AINIRec.ReadString(sSecao,'UFST'    ,'');
    ICMS.pBCOp      := StringToFloatDef( AINIRec.ReadString(sSecao,'pBCOp'    ,'') ,0);
    ICMS.vBCSTRet   := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCSTRet' ,'') ,0);
    ICMS.pST        := StringToFloatDef( AINIRec.ReadString(sSecao,'pST'      ,'') ,0);
    ICMS.vICMSSTRet := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSSTRet','') ,0);
    ICMS.vBCFCPSTRet:= StringToFloatDef( AINIRec.ReadString( sSecao,'vBCFCPSTRet', AINIRec.ReadString(sSecao,'ValorBaseFCPSTRes','')) ,0);
    ICMS.pFCPSTRet  := StringToFloatDef( AINIRec.ReadString( sSecao,'pFCPSTRet'  , AINIRec.ReadString(sSecao,'PercentualFCPSTRet','')) ,0);
    ICMS.vFCPSTRet  := StringToFloatDef( AINIRec.ReadString( sSecao,'vFCPSTRet'  , AINIRec.ReadString(sSecao,'ValorFCPSTRet','')) ,0);
    ICMS.motDesICMS := StrTomotDesICMS(OK, AINIRec.ReadString(sSecao,'motDesICMS','0'));
    ICMS.pCredSN    := StringToFloatDef( AINIRec.ReadString(sSecao,'pCredSN','') ,0);
    ICMS.vCredICMSSN:= StringToFloatDef( AINIRec.ReadString(sSecao,'vCredICMSSN','') ,0);
    ICMS.vBCSTDest  := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCSTDest','') ,0);
    ICMS.vICMSSTDest:= StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSSTDest','') ,0);
    ICMS.vICMSDeson := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSDeson','') ,0);
    ICMS.vICMSOp    := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSOp','') ,0);
    ICMS.pDif       := StringToFloatDef( AINIRec.ReadString(sSecao,'pDif','') ,0);
    ICMS.vICMSDif   := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSDif','') ,0);

    ICMS.pRedBCEfet := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedBCEfet','') ,0);
    ICMS.vBCEfet    := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCEfet','') ,0);
    ICMS.pICMSEfet  := StringToFloatDef( AINIRec.ReadString(sSecao,'pICMSEfet','') ,0);
    ICMS.vICMSEfet  := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSEfet','') ,0);

    ICMS.vICMSSubstituto := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSSubstituto','') ,0);

    ICMS.vICMSSTDeson := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSSTDeson','') ,0);
    ICMS.motDesICMSST := StrTomotDesICMS(ok, AINIRec.ReadString(sSecao,'motDesICMSST','0'));
    ICMS.pFCPDif      := StringToFloatDef( AINIRec.ReadString(sSecao,'pFCPDif','') ,0);
    ICMS.vFCPDif      := StringToFloatDef( AINIRec.ReadString(sSecao,'vFCPDif','') ,0);
    ICMS.vFCPEfet     := StringToFloatDef( AINIRec.ReadString(sSecao,'vFCPEfet','') ,0);

    ICMS.adRemICMS := StringToFloatDef( AINIRec.ReadString(sSecao,'adRemICMS','') ,0);
    ICMS.vICMSMono := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSMono','') ,0);
    ICMS.adRemICMSReten := StringToFloatDef( AINIRec.ReadString(sSecao,'adRemICMSReten','') ,0);
    ICMS.vICMSMonoReten := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSMonoReten','') ,0);
    ICMS.vICMSMonoDif := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSMonoDif','') ,0);
    ICMS.adRemICMSRet := StringToFloatDef( AINIRec.ReadString(sSecao,'adRemICMSRet','') ,0);
    ICMS.vICMSMonoRet := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSMonoRet','') ,0);

    ICMS.qBCMono := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCMono','') ,0);
    ICMS.qBCMonoReten := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCMonoReten','') ,0);
    ICMS.pRedAdRem := StringToFloatDef( AINIRec.ReadString(sSecao,'pRedAdRem','') ,0);
    ICMS.motRedAdRem := StrTomotRedAdRem(OK, AINIRec.ReadString(sSecao,'motRedAdRem','0'));
    ICMS.qBCMonoRet := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCMonoRet','') ,0);
    ICMS.vICMSMonoOp := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSMonoOp','') ,0);
    ICMS.indDeduzDeson := StrToTIndicadorEx(OK, AINIRec.ReadString(sSecao,'indDeduzDeson', ''));
  end;
end;

procedure TNFeIniReader.Ler_ImpostoICMSUFDest(AINIRec: TMemIniFile;
  ICMSUFDest: TICMSUFDest; Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'ICMSUFDest'+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString(sSecao,'vBCUFDest','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    with ICMSUFDest do
    begin
      vBCUFDest      := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCUFDest', ''), 0);
      vBCFCPUFDest   := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCFCPUFDest','') ,0);
      pFCPUFDest     := StringToFloatDef(AINIRec.ReadString(sSecao, 'pFCPUFDest', ''), 0);
      pICMSUFDest    := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSUFDest', ''), 0);
      pICMSInter     := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSInter', ''), 0);
      pICMSInterPart := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSInterPart', ''), 0);
      vFCPUFDest     := StringToFloatDef(AINIRec.ReadString(sSecao, 'vFCPUFDest', ''), 0);
      vICMSUFDest    := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSUFDest', ''), 0);
      vICMSUFRemet   := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSUFRemet', ''), 0);
    end;
  end;
end;

procedure TNFeIniReader.Ler_ImpostoIPI(AINIRec: TMemIniFile; IPI: TIPI;
  Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'IPI'+IntToStrZero(Idx,3);
  sFim   := AINIRec.ReadString( sSecao,'CST','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    IPI.CST := StrToCSTIPI(OK, AINIRec.ReadString( sSecao,'CST',''));
    if OK then
    begin
      IPI.clEnq    := AINIRec.ReadString(  sSecao,'clEnq'    ,AINIRec.ReadString(  sSecao,'ClasseEnquadramento',''));
      IPI.CNPJProd := AINIRec.ReadString(  sSecao,'CNPJProd' ,AINIRec.ReadString(  sSecao,'CNPJProdutor',''));
      IPI.cSelo    := AINIRec.ReadString(  sSecao,'cSelo'    ,AINIRec.ReadString(  sSecao,'CodigoSeloIPI',''));
      IPI.qSelo    := AINIRec.ReadInteger( sSecao,'qSelo'    ,AINIRec.ReadInteger( sSecao,'QuantidadeSelos',0));
      IPI.cEnq     := AINIRec.ReadString(  sSecao,'cEnq'     ,AINIRec.ReadString(  sSecao,'CodigoEnquadramento'    ,''));
      IPI.vBC      := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'   ,AINIRec.ReadString(sSecao,'ValorBase'   ,'')) ,0);
      IPI.qUnid    := StringToFloatDef( AINIRec.ReadString(sSecao,'qUnid' ,AINIRec.ReadString(sSecao,'Quantidade' ,'')) ,0);
      IPI.vUnid    := StringToFloatDef( AINIRec.ReadString(sSecao,'vUnid' ,AINIRec.ReadString(sSecao,'ValorUnidade' ,'')) ,0);
      IPI.pIPI     := StringToFloatDef( AINIRec.ReadString(sSecao,'pIPI'  ,AINIRec.ReadString(sSecao,'Aliquota'  ,'')) ,0);
      IPI.vIPI     := StringToFloatDef( AINIRec.ReadString(sSecao,'vIPI'  ,AINIRec.ReadString(sSecao,'Valor'  ,'')) ,0);
    end;
  end;
end;

procedure TNFeIniReader.Ler_ImpostoII(AINIRec: TMemIniFile; II: TII;
  Idx: Integer);
var
  sSecao, sFim: string;
begin
  sSecao := 'II'+IntToStrZero(Idx,3);
  sFim := AINIRec.ReadString( sSecao,'vBC',AINIRec.ReadString( sSecao,'ValorBase','FIM'));
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    with II do
    begin
      vBc      := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'          ,AINIRec.ReadString(sSecao,'ValorBase'     ,'')) ,0);
      vDespAdu := StringToFloatDef( AINIRec.ReadString(sSecao,'vDespAdu'     ,AINIRec.ReadString(sSecao,'ValorDespAduaneiras','')) ,0);
      vII      := StringToFloatDef( AINIRec.ReadString(sSecao,'vII'          ,AINIRec.ReadString(sSecao,'ValorII'     ,'')) ,0);
      vIOF     := StringToFloatDef( AINIRec.ReadString(sSecao,'vIOF'         ,AINIRec.ReadString(sSecao,'ValorIOF'    ,'')) ,0);
    end;
  end;
end;

procedure TNFeIniReader.Ler_ImpostoPIS(AINIRec: TMemIniFile; PIS: TPIS;
  Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'PIS'+IntToStrZero(Idx,3);
  sFim := AINIRec.ReadString( sSecao,'CST','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    PIS.CST :=  StrToCSTPIS(OK, AINIRec.ReadString( sSecao,'CST',''));
    if OK then
    begin
      PIS.vBC       := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'      ,AINIRec.ReadString(sSecao,'ValorBase'    ,'')) ,0);
      PIS.pPIS      := StringToFloatDef( AINIRec.ReadString(sSecao,'pPIS'     ,AINIRec.ReadString(sSecao,'Aliquota'     ,'')) ,0);
      PIS.qBCProd   := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCProd'  ,AINIRec.ReadString(sSecao,'Quantidade'   ,'')) ,0);
      PIS.vAliqProd := StringToFloatDef( AINIRec.ReadString(sSecao,'vAliqProd',AINIRec.ReadString(sSecao,'ValorAliquota','')) ,0);
      PIS.vPIS      := StringToFloatDef( AINIRec.ReadString(sSecao,'vPIS'     ,AINIRec.ReadString(sSecao,'Valor'        ,'')) ,0);
    end;
  end;
end;

procedure TNFeIniReader.Ler_ImpostoPISST(AINIRec: TMemIniFile; PISST: TPISST;
  Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'PISST'+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'ValorBase','F')+ AINIRec.ReadString( sSecao,'Quantidade','IM');
  if (sFim = 'FIM') then
    sFim   := AINIRec.ReadString( sSecao,'vBC','F')+ AINIRec.ReadString( sSecao,'qBCProd','IM');

  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    PISST.vBc       := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'    ,AINIRec.ReadString(sSecao,'ValorBase'    ,'')) ,0);
    PISST.pPis      := StringToFloatDef( AINIRec.ReadString(sSecao,'pPis'   ,AINIRec.ReadString(sSecao,'AliquotaPerc' ,'')) ,0);
    PISST.qBCProd   := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCProd',AINIRec.ReadString(sSecao,'Quantidade'   ,'')) ,0);
    PISST.vAliqProd := StringToFloatDef( AINIRec.ReadString(sSecao,'vAliqProd',AINIRec.ReadString(sSecao,'AliquotaValor','')) ,0);
    PISST.vPIS      := StringToFloatDef( AINIRec.ReadString(sSecao,'vPIS'   ,AINIRec.ReadString(sSecao,'ValorPISST'   ,'')) ,0);
    PISST.indSomaPISST := StrToindSomaPISST(ok, AINIRec.ReadString(sSecao,'indSomaPISST', ''));
  end;
end;

procedure TNFeIniReader.Ler_ImpostoCOFINS(AINIRec: TMemIniFile; COFINS: TCOFINS;
  Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'COFINS'+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'CST','FIM');
  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    COFINS.CST := StrToCSTCOFINS(OK, AINIRec.ReadString( sSecao,'CST',''));
    if OK then
    begin
      COFINS.vBC       := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'     ,AINIRec.ReadString(sSecao,'ValorBase'      ,'')) ,0);
      COFINS.pCOFINS   := StringToFloatDef( AINIRec.ReadString(sSecao,'pCOFINS' ,AINIRec.ReadString(sSecao,'Aliquota'  ,'')) ,0);
      COFINS.qBCProd   := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCProd' ,AINIRec.ReadString(sSecao,'Quantidade'  ,'')) ,0);
      COFINS.vAliqProd := StringToFloatDef( AINIRec.ReadString(sSecao,'vAliqProd',AINIRec.ReadString(sSecao,'ValorAliquota','')) ,0);
      COFINS.vCOFINS   := StringToFloatDef( AINIRec.ReadString(sSecao,'vCOFINS'  ,AINIRec.ReadString(sSecao,'Valor'  ,'')) ,0);
    end;
  end;
end;

procedure TNFeIniReader.Ler_ImpostoCOFINSST(AINIRec: TMemIniFile;
  COFINSST: TCOFINSST; Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'COFINSST'+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'ValorBase','F')+ AINIRec.ReadString( sSecao,'Quantidade','IM');
  if (sFim = 'FIM') then
    sFim   := AINIRec.ReadString( sSecao,'vBC','F')+ AINIRec.ReadString( sSecao,'qBCProd','IM');

  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    COFINSST.vBC       := StringToFloatDef( AINIRec.ReadString(sSecao,'vBC'    ,AINIRec.ReadString(sSecao,'ValorBase'     ,'')) ,0);
    COFINSST.pCOFINS   := StringToFloatDef( AINIRec.ReadString(sSecao,'pCOFINS',AINIRec.ReadString(sSecao,'AliquotaPerc'  ,'')) ,0);
    COFINSST.qBCProd   := StringToFloatDef( AINIRec.ReadString(sSecao,'qBCProd',AINIRec.ReadString(sSecao,'Quantidade'    ,'')) ,0);
    COFINSST.vAliqProd := StringToFloatDef( AINIRec.ReadString(sSecao,'vAliqProd',AINIRec.ReadString(sSecao,'AliquotaValor','')) ,0);
    COFINSST.vCOFINS   := StringToFloatDef( AINIRec.ReadString(sSecao,'vCOFINS',AINIRec.ReadString(sSecao,'ValorCOFINSST'  ,'')) ,0);
    COFINSST.indSomaCOFINSST := StrToindSomaCOFINSST(ok, AINIRec.ReadString(sSecao,'indSomaCOFINSST', ''));
  end;
end;

procedure TNFeIniReader.Ler_ImpostoISSQN(AINIRec: TMemIniFile; ISSQN: TISSQN;
  Idx: Integer);
var
  sSecao, sFim: string;
  Ok: Boolean;
begin
  sSecao := 'ISSQN'+IntToStrZero(Idx,3);
  sFim     := AINIRec.ReadString( sSecao,'ValorBase',AINIRec.ReadString(sSecao,'vBC'   ,'FIM'));
  if (sFim = 'FIM') then
    sFim := AINIRec.ReadString( sSecao,'vBC','FIM');

  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    if StringToFloatDef( sFim ,0) > 0 then
    begin
      ISSQN.vBC       := StringToFloatDef( sFim ,0);
      ISSQN.vAliq     := StringToFloatDef( AINIRec.ReadString(sSecao,'vAliq',AINIRec.ReadString(sSecao,'Aliquota' ,'')) ,0);
      ISSQN.vISSQN    := StringToFloatDef( AINIRec.ReadString(sSecao,'vISSQN',AINIRec.ReadString(sSecao,'ValorISSQN','')) ,0);
      ISSQN.cMunFG    := StrToInt( AINIRec.ReadString(sSecao,'cMunFG',AINIRec.ReadString(sSecao,'MunicipioFatoGerador','')));
      ISSQN.cListServ := AINIRec.ReadString(sSecao,'cListServ',AINIRec.ReadString(sSecao,'CodigoServico',''));
      ISSQN.cSitTrib  := StrToISSQNcSitTrib( OK,AINIRec.ReadString(sSecao,'cSitTrib',''));
      ISSQN.vDeducao    := StringToFloatDef( AINIRec.ReadString(sSecao,'vDeducao',AINIRec.ReadString(sSecao,'ValorDeducao'   ,'')) ,0);
      ISSQN.vOutro      := StringToFloatDef( AINIRec.ReadString(sSecao,'vOutro',AINIRec.ReadString(sSecao,'ValorOutro'   ,'')) ,0);
      ISSQN.vDescIncond := StringToFloatDef( AINIRec.ReadString(sSecao,'vDescIncond',AINIRec.ReadString(sSecao,'ValorDescontoIncondicional'   ,'')) ,0);
      ISSQN.vDescCond   := StringToFloatDef( AINIRec.ReadString(sSecao,'vDescCond',AINIRec.ReadString(sSecao,'vDescontoCondicional'   ,'')) ,0);
      ISSQN.vISSRet     := StringToFloatDef( AINIRec.ReadString(sSecao,'vISSRet',AINIRec.ReadString(sSecao,'ValorISSRetido'   ,'')) ,0);
      ISSQN.indISS      := StrToindISS( OK,AINIRec.ReadString(sSecao,'indISS',''));
      ISSQN.cServico    := AINIRec.ReadString(sSecao,'cServico','');
      ISSQN.cMun        := AINIRec.ReadInteger(sSecao,'cMun',0);
      ISSQN.cPais       := AINIRec.ReadInteger(sSecao,'cPais',1058);
      ISSQN.nProcesso   := AINIRec.ReadString(sSecao,'nProcesso','');
      ISSQN.indIncentivo := StrToindIncentivo( OK,AINIRec.ReadString(sSecao,'indIncentivo',''));
    end;
  end;
end;

procedure TNFeIniReader.Ler_ProdutoObsContItem(AINIRec: TMemIniFile;
  obsCont: TobsItem; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'obsContItem' + IntToStrZero(Idx, 3);
  obsCont.xCampo := AINIRec.ReadString(sSecao,'xCampo', '');
  obsCont.xTexto := AINIRec.ReadString(sSecao,'xTexto', '');
end;

procedure TNFeIniReader.Ler_ProdutoObsFiscoItem(AINIRec: TMemIniFile;
  obsFisco: TobsItem; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'obsFiscoItem' + IntToStrZero(Idx, 3);
  obsFisco.xCampo := AINIRec.ReadString(sSecao,'xCampo', '');
  obsFisco.xTexto := AINIRec.ReadString(sSecao,'xTexto', '');
end;

procedure TNFeIniReader.Ler_Total(AINIRec: TMemIniFile; ICMSTot: TICMSTot);
begin
  ICMSTot.vBC     := StringToFloatDef( AINIRec.ReadString('Total','vBC'       ,AINIRec.ReadString('Total','BaseICMS','')) ,0);
  ICMSTot.vICMS   := StringToFloatDef( AINIRec.ReadString('Total','vICMS'     ,AINIRec.ReadString('Total','ValorICMS','')) ,0);
  ICMSTot.vICMSDeson := StringToFloatDef( AINIRec.ReadString('Total','vICMSDeson',''),0);
  ICMSTot.vFCP       := StringToFloatDef( AINIRec.ReadString('Total','vFCP'   ,AINIRec.ReadString('Total','ValorFCP','')) ,0);
  ICMSTot.vBCST   := StringToFloatDef( AINIRec.ReadString('Total','vBCST'     ,AINIRec.ReadString('Total','BaseICMSSubstituicao','')) ,0);
  ICMSTot.vST     := StringToFloatDef( AINIRec.ReadString('Total','vST'       ,AINIRec.ReadString('Total','ValorICMSSubstituicao'  ,'')) ,0);
  ICMSTot.vFCPST  := StringToFloatDef( AINIRec.ReadString('Total','vFCPST'    ,AINIRec.ReadString('Total','ValorFCPST' ,'')) ,0);
  ICMSTot.vFCPSTRet:= StringToFloatDef( AINIRec.ReadString('Total','vFCPSTRet',AINIRec.ReadString('Total','ValorFCPSTRet' ,'')) ,0);

  ICMSTot.qBCMono := StringToFloatDef( AINIRec.ReadString('Total','qBCMono', '') , 0);
  ICMSTot.vICMSMono := StringToFloatDef( AINIRec.ReadString('Total','vICMSMono', '') , 0);
  ICMSTot.qBCMonoReten := StringToFloatDef( AINIRec.ReadString('Total','qBCMonoReten', '') , 0);
  ICMSTot.vICMSMonoReten := StringToFloatDef( AINIRec.ReadString('Total','vICMSMonoReten' ,''), 0);
  ICMSTot.qBCMonoRet := StringToFloatDef( AINIRec.ReadString('Total','qBCMonoRet', '') , 0);
  ICMSTot.vICMSMonoRet := StringToFloatDef( AINIRec.ReadString('Total','vICMSMonoRet', ''), 0);

  ICMSTot.vProd   := StringToFloatDef( AINIRec.ReadString('Total','vProd'     ,AINIRec.ReadString('Total','ValorProduto' ,'')) ,0);
  ICMSTot.vFrete  := StringToFloatDef( AINIRec.ReadString('Total','vFrete'    ,AINIRec.ReadString('Total','ValorFrete' ,'')) ,0);
  ICMSTot.vSeg    := StringToFloatDef( AINIRec.ReadString('Total','vSeg'      ,AINIRec.ReadString('Total','ValorSeguro' ,'')) ,0);
  ICMSTot.vDesc   := StringToFloatDef( AINIRec.ReadString('Total','vDesc'     ,AINIRec.ReadString('Total','ValorDesconto' ,'')) ,0);
  ICMSTot.vII     := StringToFloatDef( AINIRec.ReadString('Total','vII'       ,AINIRec.ReadString('Total','ValorII' ,'')) ,0);
  ICMSTot.vIPI    := StringToFloatDef( AINIRec.ReadString('Total','vIPI'      ,AINIRec.ReadString('Total','ValorIPI' ,'')) ,0);
  ICMSTot.vIPIDevol:= StringToFloatDef( AINIRec.ReadString('Total','vIPIDevol',AINIRec.ReadString('Total','ValorIPIDevol' ,'')) ,0);
  ICMSTot.vPIS    := StringToFloatDef( AINIRec.ReadString('Total','vPIS'      ,AINIRec.ReadString('Total','ValorPIS' ,'')) ,0);
  ICMSTot.vCOFINS := StringToFloatDef( AINIRec.ReadString('Total','vCOFINS'   ,AINIRec.ReadString('Total','ValorCOFINS','')) ,0);
  ICMSTot.vOutro  := StringToFloatDef( AINIRec.ReadString('Total','vOutro'    ,AINIRec.ReadString('Total','ValorOutrasDespesas','')) ,0);
  ICMSTot.vNF     := StringToFloatDef( AINIRec.ReadString('Total','vNF'       ,AINIRec.ReadString('Total','ValorNota' ,'')) ,0);
  ICMSTot.vTotTrib:= StringToFloatDef( AINIRec.ReadString('Total','vTotTrib'     ,''),0);
  ICMSTot.vFCPUFDest  := StringToFloatDef( AINIRec.ReadString('Total','vFCPUFDest',''),0);
  ICMSTot.vICMSUFDest := StringToFloatDef( AINIRec.ReadString('Total','vICMSUFDest',''),0);
  ICMSTot.vICMSUFRemet:= StringToFloatDef( AINIRec.ReadString('Total','vICMSUFRemet',''),0);
  FNFe.Total.vNFTot := StringToFloatDef( AINIRec.ReadString('Total','vNFTot',''),0);

  // Reforma Tributária
  Ler_ISTot(AINIRec, FNFe.Total.ISTot);
  Ler_IBSCBSTot(AINIRec, FNFe.Total.IBSCBSTot);
end;

procedure TNFeIniReader.Ler_TotalISSQNTot(AINIRec: TMemIniFile;
  ISSQNtot: TISSQNtot);
var
  Ok: Boolean;
begin
  ISSQNtot.vServ  := StringToFloatDef(  AINIRec.ReadString('ISSQNtot','vServ',AINIRec.ReadString('ISSQNtot','ValorServicos','')) ,0) ;
  ISSQNTot.vBC    := StringToFloatDef(  AINIRec.ReadString('ISSQNtot','vBC'  ,AINIRec.ReadString('ISSQNtot','ValorBaseISS'  ,'')) ,0) ;
  ISSQNTot.vISS   := StringToFloatDef(  AINIRec.ReadString('ISSQNtot','vISS' ,AINIRec.ReadString('ISSQNtot','ValorISSQN' ,'')) ,0) ;
  ISSQNTot.vPIS   := StringToFloatDef(  AINIRec.ReadString('ISSQNtot','vPIS' ,AINIRec.ReadString('ISSQNtot','ValorPISISS' ,'')) ,0) ;
  ISSQNTot.vCOFINS:= StringToFloatDef(  AINIRec.ReadString('ISSQNtot','vCOFINS',AINIRec.ReadString('ISSQNtot','ValorCONFINSISS','')) ,0) ;
  ISSQNtot.dCompet     := StringToDateTime( AINIRec.ReadString('ISSQNtot', 'dCompet', '0'));
  ISSQNtot.vDeducao    := StringToFloatDef( AINIRec.ReadString('ISSQNtot', 'vDeducao', ''), 0);
  ISSQNtot.vOutro      := StringToFloatDef( AINIRec.ReadString('ISSQNtot', 'vOutro', ''), 0);
  ISSQNtot.vDescIncond := StringToFloatDef( AINIRec.ReadString('ISSQNtot', 'vDescIncond', ''), 0);
  ISSQNtot.vDescCond   := StringToFloatDef( AINIRec.ReadString('ISSQNtot', 'vDescCond', ''), 0);
  ISSQNtot.vISSRet     := StringToFloatDef( AINIRec.ReadString('ISSQNtot', 'vISSRet', ''), 0);
  ISSQNtot.cRegTrib    := StrToRegTribISSQN( OK,AINIRec.ReadString('ISSQNtot', 'cRegTrib', '0'));
end;

procedure TNFeIniReader.Ler_TotalRetTributavel(AINIRec: TMemIniFile;
  retTrib: TretTrib);
begin
  retTrib.vRetPIS    := StringToFloatDef( AINIRec.ReadString('retTrib','vRetPIS'   ,'') ,0);
  retTrib.vRetCOFINS := StringToFloatDef( AINIRec.ReadString('retTrib','vRetCOFINS','') ,0);
  retTrib.vRetCSLL   := StringToFloatDef( AINIRec.ReadString('retTrib','vRetCSLL'  ,'') ,0);
  retTrib.vBCIRRF    := StringToFloatDef( AINIRec.ReadString('retTrib','vBCIRRF'   ,'') ,0);
  retTrib.vIRRF      := StringToFloatDef( AINIRec.ReadString('retTrib','vIRRF'     ,'') ,0);
  retTrib.vBCRetPrev := StringToFloatDef( AINIRec.ReadString('retTrib','vBCRetPrev','') ,0);
  retTrib.vRetPrev   := StringToFloatDef( AINIRec.ReadString('retTrib','vRetPrev'  ,'') ,0);
end;

procedure TNFeIniReader.Ler_Transportador(AINIRec: TMemIniFile;
  Transp: TTransp);
var
  sSecao, sFim: string;
  Ok, bVol: Boolean;
  I, J: Integer;
begin
  sSecao := IfThen( AINIRec.SectionExists('Transportador'), 'Transportador', 'transp');
  Transp.modFrete := StrTomodFrete(OK, AINIRec.ReadString(sSecao,'modFrete',AINIRec.ReadString(sSecao,'FretePorConta','0')));
  Transp.Transporta.CNPJCPF  := AINIRec.ReadString(sSecao,'CNPJCPF','');
  Transp.Transporta.xNome    := AINIRec.ReadString(sSecao,'xNome'  ,AINIRec.ReadString(sSecao,'NomeRazao',''));
  Transp.Transporta.IE       := AINIRec.ReadString(sSecao,'IE'     ,'');
  Transp.Transporta.xEnder   := AINIRec.ReadString(sSecao,'xEnder' ,AINIRec.ReadString(sSecao,'Endereco',''));
  Transp.Transporta.xMun     := AINIRec.ReadString(sSecao,'xMun'   ,AINIRec.ReadString(sSecao,'Cidade',''));
  Transp.Transporta.UF       := AINIRec.ReadString(sSecao,'UF'     ,'');

  Transp.retTransp.vServ    := StringToFloatDef( AINIRec.ReadString(sSecao,'vServ',AINIRec.ReadString(sSecao,'ValorServico'   ,'')) ,0);
  Transp.retTransp.vBCRet   := StringToFloatDef( AINIRec.ReadString(sSecao,'vBCRet'   ,AINIRec.ReadString(sSecao,'ValorBase'  ,'')) ,0);
  Transp.retTransp.pICMSRet := StringToFloatDef( AINIRec.ReadString(sSecao,'pICMSRet'    ,AINIRec.ReadString(sSecao,'Aliquota','')) ,0);
  Transp.retTransp.vICMSRet := StringToFloatDef( AINIRec.ReadString(sSecao,'vICMSRet'       ,AINIRec.ReadString(sSecao,'Valor','')) ,0);
  Transp.retTransp.CFOP     := AINIRec.ReadString(sSecao,'CFOP'     ,'');
  Transp.retTransp.cMunFG   := AINIRec.ReadInteger(sSecao,'cMunFG',AINIRec.ReadInteger(sSecao,'CidadeCod',0));

  Transp.veicTransp.placa := AINIRec.ReadString(sSecao,'Placa'  ,'');
  Transp.veicTransp.UF    := AINIRec.ReadString(sSecao,'UFPlaca','');
  Transp.veicTransp.RNTC  := AINIRec.ReadString(sSecao,'RNTC'   ,'');

  Transp.vagao := AINIRec.ReadString( sSecao,'vagao','');
  Transp.balsa := AINIRec.ReadString( sSecao,'balsa','');

  J := 1;
  while true do
  begin
    sSecao := 'Reboque'+IntToStrZero(J,3);
    sFim     := AINIRec.ReadString(sSecao,'placa','FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with Transp.Reboque.New do
    begin
      placa := sFim;
      UF    := AINIRec.ReadString( sSecao,'UF'  ,'');
      RNTC  := AINIRec.ReadString( sSecao,'RNTC','');
    end;

    Inc(J)
  end;

  I := 1;
  while true do
  begin
    sSecao := IfThen(AINIRec.SectionExists('Volume'+IntToStrZero(I,3)), 'Volume', 'vol');
    sSecao := sSecao+IntToStrZero(I,3);

    bVol := (AINIRec.ReadString(sSecao, 'qVol', AINIRec.ReadString(sSecao, 'Quantidade', '')) = '') and
            (AINIRec.ReadString(sSecao, 'esp', AINIRec.ReadString( sSecao, 'Especie', '')) = '') and
            (AINIRec.ReadString(sSecao, 'Marca', '') = '') and
            (AINIRec.ReadString(sSecao, 'nVol', AINIRec.ReadString( sSecao, 'Numeracao', '')) = '') and
            (AINIRec.ReadString(sSecao, 'pesoL', AINIRec.ReadString(sSecao, 'PesoLiquido', '')) = '') and
            (AINIRec.ReadString(sSecao, 'pesoB', AINIRec.ReadString(sSecao, 'PesoBruto', '')) = '');

    if bVol then
      break;

    with Transp.Vol.New do
    begin
      qVol  := StrToIntDef(AINIRec.ReadString(sSecao, 'qVol', AINIRec.ReadString(sSecao, 'Quantidade', '0')),0);
      esp   := AINIRec.ReadString(sSecao, 'esp', AINIRec.ReadString(sSecao, 'Especie', ''));
      marca := AINIRec.ReadString(sSecao, 'Marca', '');
      nVol  := AINIRec.ReadString(sSecao, 'nVol', AINIRec.ReadString(sSecao, 'Numeracao', ''));
      pesoL := StringToFloatDef(AINIRec.ReadString(sSecao, 'pesoL', AINIRec.ReadString(sSecao, 'PesoLiquido', '0')), 0);
      pesoB := StringToFloatDef(AINIRec.ReadString(sSecao, 'pesoB', AINIRec.ReadString(sSecao, 'PesoBruto', '0')), 0);

      J := 1;
      while true do
      begin
        sSecao := IfThen(AINIRec.SectionExists('lacres'+IntToStrZero(I,3)+IntToStrZero(J,3)), 'lacres', 'Lacre');
        sSecao := sSecao+IntToStrZero(I,3)+IntToStrZero(J,3);
        sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0)  then
          break;

        Lacres.New.nLacre := sFim;

        Inc(J);
      end;
    end;

    Inc(I);
  end;

end;

procedure TNFeIniReader.Ler_Fatura(AINIRec: TMemIniFile; Fat: TFat);
var
  sSecao: string;
begin
  sSecao := IfThen(AINIRec.SectionExists('Fatura'), 'Fatura', 'fat');
  Fat.nFat  := AINIRec.ReadString( sSecao,'nFat',AINIRec.ReadString( sSecao,'Numero',''));
  Fat.vOrig := StringToFloatDef( AINIRec.ReadString(sSecao,'vOrig',AINIRec.ReadString(sSecao,'ValorOriginal','')) ,0);
  Fat.vDesc := StringToFloatDef( AINIRec.ReadString(sSecao,'vDesc',AINIRec.ReadString(sSecao,'ValorDesconto','')) ,0);
  Fat.vLiq  := StringToFloatDef( AINIRec.ReadString(sSecao,'vLiq' ,AINIRec.ReadString(sSecao,'ValorLiquido' ,'')) ,0);
end;

procedure TNFeIniReader.Ler_gPagAntecipado(AINIRec: TMemIniFile);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao   := 'gPagAntecipado' + IntToStrZero(I,2);
    sFim := AINIRec.ReadString(sSecao, 'refNFe', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with NFe.Ide.gPagAntecipado.New do
    begin
      refNFe := sFim;
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_Duplicata(AINIRec: TMemIniFile;
  Dup: TDupCollection);
var
  I: Integer;
  sSecao, sDupNumber: string;
begin
  I := 1;
  while true do
  begin
    sSecao   := IfThen(AINIRec.SectionExists('Duplicata'+IntToStrZero(I,3)), 'Duplicata', 'dup');
    sSecao   := sSecao+IntToStrZero(I,3);
    sDupNumber := AINIRec.ReadString(sSecao,'nDup',AINIRec.ReadString(sSecao,'Numero','FIM'));
    if (sDupNumber = 'FIM') or (Length(sDupNumber) <= 0) then
      break;

    with Dup.New do
    begin
      nDup  := sDupNumber;
      dVenc := StringToDateTime(AINIRec.ReadString( sSecao,'dVenc',AINIRec.ReadString( sSecao,'DataVencimento','0')));
      vDup  := StringToFloatDef( AINIRec.ReadString(sSecao,'vDup',AINIRec.ReadString(sSecao,'Valor','')) ,0);
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_Pagamento(AINIRec: TMemIniFile;
  pag: TpagCollection);
var
  cVTroco: Double;
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'pag'+IntToStrZero(I,3);
    sFim     := AINIRec.ReadString(sSecao,'tpag','FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with pag.New do
    begin
      tPag  := StrToFormaPagamento(OK, sFim);
      xPag  := AINIRec.ReadString(sSecao,'xPag','');
      vPag  := StringToFloatDef( AINIRec.ReadString(sSecao,'vPag','') ,0);
      dPag  := StringToDateTime(AINIRec.ReadString( sSecao,'dPag','0'));

      CNPJPag := AINIRec.ReadString(sSecao,'CNPJPag','');
      UFPag   := AINIRec.ReadString(sSecao,'UFPag','');

      // Se não for informado 0=Pagamento à Vista ou 1=Pagamento à Prazo
      // a tag <indPag> não deve ser gerada.
      indPag:= StrToIndpagEX(AINIRec.ReadString(sSecao, 'indPag', ''));

      tpIntegra  := StrTotpIntegra(OK,AINIRec.ReadString(sSecao,'tpIntegra',''));
      CNPJ  := AINIRec.ReadString(sSecao,'CNPJ','');
      tBand := StrToBandeiraCartao(OK,AINIRec.ReadString(sSecao,'tBand',''));
      cAut  := AINIRec.ReadString(sSecao,'cAut','');

      CNPJReceb := AINIRec.ReadString(sSecao,'CNPJReceb','');
      idTermPag := AINIRec.ReadString(sSecao,'idTermPag','');
    end;

    cVTroco := StringToFloatDef( AINIRec.ReadString(sSecao,'vTroco','') ,0);

    if (cVTroco > 0) then
      pag.vTroco:=  cVTroco;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_InfIntermed(AINIRec: TMemIniFile;
  infIntermed: TinfIntermed);
var
  sSecao: string;
begin
  sSecao := 'infIntermed';
  infIntermed.CNPJ         := AINIRec.ReadString( sSecao,'CNPJ', '');
  infIntermed.idCadIntTran := AINIRec.ReadString( sSecao,'idCadIntTran', '');
end;

procedure TNFeIniReader.Ler_DadosAdicionais(AINIRec: TMemIniFile;
  InfAdic: TInfAdic);
var
  sSecao: string;
begin
  sSecao := IfThen(AINIRec.SectionExists('DadosAdicionais'), 'DadosAdicionais', 'infAdic');
  InfAdic.infAdFisco := AINIRec.ReadString( sSecao,'infAdFisco',AINIRec.ReadString( sSecao,'Fisco',''));
  InfAdic.infCpl     := AINIRec.ReadString( sSecao,'infCpl'    ,AINIRec.ReadString( sSecao,'Complemento',''));
end;

procedure TNFeIniReader.Ler_ObsContribuinte(AINIRec: TMemIniFile;
  obsCont: TobsContCollection);
var
  I: Integer;
  sSecao, sAdittionalField: string;
begin
  I := 1;
  while true do
  begin
    sSecao := IfThen(AINIRec.SectionExists('obsCont'+IntToStrZero(I,3)), 'obsCont', 'InfAdic');
    sSecao     := sSecao+IntToStrZero(I,3);
    sAdittionalField := AINIRec.ReadString(sSecao,'xCampo',AINIRec.ReadString(sSecao,'Campo','FIM'));
    if (sAdittionalField = 'FIM') or (Length(sAdittionalField) <= 0) then
      break;

    with obsCont.New do
    begin
      xCampo := sAdittionalField;
      xTexto := AINIRec.ReadString( sSecao,'xTexto',AINIRec.ReadString( sSecao,'Texto',''));
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_ObsFisco(AINIRec: TMemIniFile;
  obsFisco: TobsFiscoCollection);
var
  I: Integer;
  sSecao, sAdittionalField: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'obsFisco'+IntToStrZero(I,3);
    sAdittionalField := AINIRec.ReadString(sSecao,'xCampo',AINIRec.ReadString(sSecao,'Campo','FIM'));
    if (sAdittionalField = 'FIM') or (Length(sAdittionalField) <= 0) then
      break;

    with obsFisco.New do
    begin
      xCampo := sAdittionalField;
      xTexto := AINIRec.ReadString( sSecao,'xTexto',AINIRec.ReadString( sSecao,'Texto',''));
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_ProcessoReferenciado(AINIRec: TMemIniFile;
  procRef: TprocRefCollection);
var
  I: Integer;
  sSecao, sAdittionalField: string;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'procRef'+IntToStrZero(I,3);
    sAdittionalField := AINIRec.ReadString(sSecao,'nProc','FIM');
    if (sAdittionalField = 'FIM') or (Length(sAdittionalField) <= 0) then
      break;

    with procRef.New do
    begin
      nProc := sAdittionalField;
      indProc := StrToindProc(OK, AINIRec.ReadString( sSecao, 'indProc', '0'));
      tpAto := StrTotpAto(OK, AINIRec.ReadString( sSecao, 'tpAto', ''));
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_Exportacao(AINIRec: TMemIniFile; exporta: TExporta);
var
  sFim: string;
begin
  sFim := AINIRec.ReadString( 'exporta','UFembarq',AINIRec.ReadString( 'exporta','UFSaidaPais','FIM'));

  if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
  begin
    exporta.UFembarq     := AINIRec.ReadString( 'exporta','UFembarq','');;
    exporta.xLocEmbarq   := AINIRec.ReadString( 'exporta','xLocEmbarq','');
    exporta.UFSaidaPais  := AINIRec.ReadString( 'exporta','UFSaidaPais','');
    exporta.xLocExporta  := AINIRec.ReadString( 'exporta','xLocExporta','');
    exporta.xLocDespacho := AINIRec.ReadString( 'exporta','xLocDespacho','');
  end;
end;

procedure TNFeIniReader.Ler_Compra(AINIRec: TMemIniFile; compra: TCompra);
begin
  if (AINIRec.ReadString( 'compra','xNEmp','') <> '') or
     (AINIRec.ReadString( 'compra','xPed' ,'') <> '') or
     (AINIRec.ReadString( 'compra','xCont','') <> '') then
  begin
    compra.xNEmp := AINIRec.ReadString( 'compra','xNEmp','');
    compra.xPed  := AINIRec.ReadString( 'compra','xPed','');
    compra.xCont := AINIRec.ReadString( 'compra','xCont','');
  end;
end;

procedure TNFeIniReader.Ler_Cana(AINIRec: TMemIniFile; cana: Tcana);
begin
  cana.safra   := AINIRec.ReadString( 'cana','safra','');
  cana.ref     := AINIRec.ReadString( 'cana','ref'  ,'');
  cana.qTotMes := StringToFloatDef( AINIRec.ReadString('cana','qTotMes','') ,0);
  cana.qTotAnt := StringToFloatDef( AINIRec.ReadString('cana','qTotAnt','') ,0);
  cana.qTotGer := StringToFloatDef( AINIRec.ReadString('cana','qTotGer','') ,0);
  cana.vFor    := StringToFloatDef( AINIRec.ReadString('cana','vFor'   ,'') ,0);
  cana.vTotDed := StringToFloatDef( AINIRec.ReadString('cana','vTotDed','') ,0);
  cana.vLiqFor := StringToFloatDef( AINIRec.ReadString('cana','vLiqFor','') ,0);
end;

procedure TNFeIniReader.Ler_CanaForDia(AINIRec: TMemIniFile;
  fordia: TForDiaCollection);
var
  I: Integer;
  sSecao, sDay: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'forDia'+IntToStrZero(I,3);
    sDay     := AINIRec.ReadString(sSecao,'dia','FIM');
    if (sDay = 'FIM') or (Length(sDay) <= 0) then
      break;

    with fordia.New do
    begin
      dia  := StrToInt(sDay);
      qtde := StringToFloatDef(AINIRec.ReadString(sSecao,'qtde'   ,'') ,0);
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_CanaDeducao(AINIRec: TMemIniFile;
  deduc: TDeducCollection);
var
  I: Integer;
  sSecao, sDeduc: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'deduc'+IntToStrZero(I,3);
    sDeduc   := AINIRec.ReadString(sSecao,'xDed','FIM');
    if (sDeduc = 'FIM') or (Length(sDeduc) <= 0) then
      break;

    with deduc.New do
    begin
      xDed := sDeduc;
      vDed := StringToFloatDef(AINIRec.ReadString(sSecao,'vDed'   ,'') ,0);
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_InfNFeSupl(AINIRec: TMemIniFile;
  infNFeSupl: TinfNFeSupl);
var
  sSecao: string;
begin
  sSecao := 'infNFeSupl';
  if AINIRec.SectionExists(sSecao) then
  begin
    infNFeSupl.qrCode := AINIRec.ReadString(sSecao, 'qrCode','');
    infNFeSupl.urlChave := AINIRec.ReadString(sSecao, 'urlChave','');
  end;
end;

procedure TNFeIniReader.Ler_InfRespTec(AINIRec: TMemIniFile;
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

procedure TNFeIniReader.Ler_Defensivo(AINIRec: TMemIniFile;
  defensivo: TdefensivoCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    // I varia de 01 a 20
    sSecao := 'defensivo' + IntToStrZero(I, 2);
    sFim := AINIRec.ReadString(sSecao ,'nReceituario', 'FIM');

    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with defensivo.New do
    begin
      nReceituario := sFim;
      CPFRespTec := AINIRec.ReadString(sSecao, 'CPFRespTec', '');
    end;

    Inc(I);
  end;
end;

procedure TNFeIniReader.Ler_GuiaTransito(AINIRec: TMemIniFile;
  guiaTransito: TguiaTransito);
var
  sSecao: string;
begin
  sSecao := 'guiaTransito';
  if AINIRec.SectionExists(sSecao) then
  begin
    guiaTransito.UFGuia := AINIRec.ReadString(sSecao, 'UFGuia', '');
    guiaTransito.tpGuia := StrToTtpGuia(AINIRec.ReadString(sSecao, 'tpGuia', ''));
    guiaTransito.serieGuia := AINIRec.ReadString(sSecao, 'serieGuia', '');
    guiaTransito.nGuia := AINIRec.ReadString(sSecao, 'nGuia', '0');
  end;
end;

// Reforma Tributária
procedure TNFeIniReader.Ler_ISel(AINIRec: TMemIniFile; ISel: TgIS;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'IS' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    ISel.CSTIS := StrToCSTIS(AINIRec.ReadString(sSecao, 'CSTIS', ''));
    ISel.cClassTribIS := StrTocClassTribIS(AINIRec.ReadString(sSecao, 'cClassTribIS', ''));
    ISel.vBCIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCIS', ''), 0);
    ISel.pIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pIS', ''), 0);
    ISel.pISEspec := StringToFloatDef(AINIRec.ReadString(sSecao, 'pISEspec', ''), 0);
    ISel.uTrib := AINIRec.ReadString(sSecao, 'uTrib', '');
    ISel.qTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'qTrib', ''), 0);
    ISel.vIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIS', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'IBSCBS' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBS.CST := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CST', '000'));
    IBSCBS.cClassTrib := StrTocClassTrib(AINIRec.Readstring(sSecao, 'cClassTrib', '000001'));

    Ler_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS, Idx);
    Ler_IBSCBS_gIBSCBSMono(AINIRec, IBSCBS.gIBSCBSMono, Idx);
    Ler_IBSCBS_gTransfCred(AINIRec, IBSCBS.gTransfCred, Idx);
    Ler_IBSCBS_gCredPresIBSZFM(AINIRec, IBSCBS.gCredPresIBSZFM, Idx);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile;
  IBSCBS: TgIBSCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBC', ''), 0);
    IBSCBS.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBS', ''), 0);

    Ler_IBSCBS_gIBSCBS_gIBSUF(AINIRec, IBSCBS.gIBSUF, Idx);
    Ler_IBSCBS_gIBSCBS_gIBSMun(AINIRec, IBSCBS.gIBSMun, Idx);
    Ler_IBSCBS_gIBSCBS_gCBS(AINIRec, IBSCBS.gCBS, Idx);
    Ler_IBSCBS_gIBSCBS_gTribRegular(AINIRec, IBSCBS.gTribRegular, Idx);
    Ler_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, IBSCBS.gIBSCredPres, 'gIBSCredPres', Idx);
    Ler_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, IBSCBS.gCBSCredPres, 'gCBSCredPres', Idx);
    Ler_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec, IBSCBS.gTribCompraGov, Idx);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile;
  gIBSUF: TgIBSUF; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSUF.pIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'pIBSUF', ''), 0);
    gIBSUF.vIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSUF', ''), 0);

    gIBSUF.gDif.pDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDif', ''), 0);
    gIBSUF.gDif.vDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDif', ''), 0);

    gIBSUF.gDevTrib.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTrib', ''), 0);

    gIBSUF.gRed.pRedAliq := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedAliq', ''), 0);
    gIBSUF.gRed.pAliqEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqEfet', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile;
  gIBSMun: TgIBSMun; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSMun.pIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'pIBSMun', ''), 0);
    gIBSMun.vIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMun', ''), 0);

    gIBSMun.gDif.pDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDif', ''), 0);
    gIBSMun.gDif.vDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDif', ''), 0);

    gIBSMun.gDevTrib.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTrib', ''), 0);

    gIBSMun.gRed.pRedAliq := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedAliq', ''), 0);
    gIBSMun.gRed.pAliqEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqEfet', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBS; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCBS' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gCBS.pCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pCBS', ''), 0);
    gCBS.vCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBS', ''), 0);

    gCBS.gDif.pDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDif', ''), 0);
    gCBS.gDif.vDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDif', ''), 0);

    gCBS.gDevTrib.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTrib', ''), 0);

    gCBS.gRed.pRedAliq := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedAliq', ''), 0);
    gCBS.gRed.pAliqEfet := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqEfet', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS_gTribRegular(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gTribRegular.CSTReg := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CSTReg', '000'));
    gTribRegular.cClassTribReg := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTribReg', '000001'));

    gTribRegular.pAliqEfetRegIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqEfetRegIBSUF', ''), 0);
    gTribRegular.vTribRegIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTribRegIBSUF', ''), 0);
    gTribRegular.pAliqEfetRegIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqEfetRegIBSMun', ''), 0);
    gTribRegular.vTribRegIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTribRegIBSMun', ''), 0);
    gTribRegular.pAliqEfetRegCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqEfetRegCBS', ''), 0);
    gTribRegular.vTribRegCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTribRegCBS', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS_gIBSCBSCredPres(
  AINIRec: TMemIniFile; IBSCredPres: TgIBSCBSCredPres; const Grupo: string;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := Grupo + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    IBSCredPres.pCredPres := StringToFloatDef(AINIRec.ReadString(sSecao, 'pCredPres', ''), 0);
    IBSCredPres.vCredPres := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPres', ''), 0);
    IBSCredPres.vCredPresCondSus := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPresCondSus', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec: TMemIniFile;
  gTribCompraGov: TgTribCompraGov; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gTribCompraGov.pAliqIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqIBSUF', ''), 0);
    gTribCompraGov.vTribIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTribIBSUF', ''), 0);
    gTribCompraGov.pAliqIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqIBSMun', ''), 0);
    gTribCompraGov.vTribIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTribIBSMun', ''), 0);
    gTribCompraGov.pAliqCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqCBS', ''), 0);
    gTribCompraGov.vTribCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTribCBS', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gIBSCBSMono(AINIRec: TMemIniFile;
  IBSCBSMono: TgIBSCBSMono; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBSMono' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBSMono.qBCMono := StringToFloatDef(AINIRec.ReadString(sSecao, 'qBCMono', ''), 0);
    IBSCBSMono.adRemIBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'adRemIBS', ''), 0);
    IBSCBSMono.adRemCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'adRemCBS', ''), 0);
    IBSCBSMono.vIBSMono := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMono', ''), 0);
    IBSCBSMono.vCBSMono := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMono', ''), 0);
    IBSCBSMono.qBCMonoReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'qBCMonoReten', ''), 0);
    IBSCBSMono.adRemIBSReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'adRemIBSReten', ''), 0);
    IBSCBSMono.vIBSMonoReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMonoReten', ''), 0);
    IBSCBSMono.adRemCBSReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'adRemCBSReten', ''), 0);
    IBSCBSMono.vCBSMonoReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMonoReten', ''), 0);
    IBSCBSMono.qBCMonoRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'qBCMonoRet', ''), 0);
    IBSCBSMono.adRemIBSRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'adRemIBSRet', ''), 0);
    IBSCBSMono.vIBSMonoRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMonoRet', ''), 0);
    IBSCBSMono.adRemCBSRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'adRemCBSRet', ''), 0);
    IBSCBSMono.vCBSMonoRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMonoRet', ''), 0);
    IBSCBSMono.pDifIBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDifIBS', ''), 0);
    IBSCBSMono.vIBSMonoDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMonoDif', ''), 0);
    IBSCBSMono.pDifCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDifCBS', ''), 0);
    IBSCBSMono.vCBSMonoDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMonoDif', ''), 0);
    IBSCBSMono.vTotIBSMonoItem := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotIBSMonoItem', ''), 0);
    IBSCBSMono.vTotCBSMonoItem := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotCBSMonoItem', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gTransfCred(AINIRec: TMemIniFile;
  gTransfCred: TgTransfCred; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gTransfCred' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gTransfCred.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBS', ''), 0);
    gTransfCred.vCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBS', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBS_gCredPresIBSZFM(AINIRec: TMemIniFile;
  gCredPresIBSZFM: TCredPresIBSZFM; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'gCredPresIBSZFM' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    gCredPresIBSZFM.tpCredPresIBSZFM := StrToTpCredPresIBSZFM(AINIRec.ReadString(sSecao, 'tpCredPresIBSZFM', ''));
    gCredPresIBSZFM.vCredPresIBSZFM := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPresIBSZFM', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_Det_DFeReferenciado(AINIRec: TMemIniFile;
  DFeReferenciado: TDFeReferenciado; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'DFeReferenciado' + IntToStrZero(Idx, 3);

  if AINIRec.SectionExists(sSecao) then
  begin
    DFeReferenciado.chaveAcesso := AINIRec.ReadString(sSecao, 'chaveAcesso', '');
    DFeReferenciado.nItem := AINIRec.ReadInteger(sSecao, 'nItem', 0);
  end;
end;

procedure TNFeIniReader.Ler_ISTot(AINIRec: TMemIniFile; ISTot: TISTot);
var
  sSecao: string;
begin
  sSecao := 'ISTot';

  if AINIRec.SectionExists(sSecao) then
    ISTot.vIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIS', ''), 0);
end;

procedure TNFeIniReader.Ler_IBSCBSTot(AINIRec: TMemIniFile;
  IBSCBSTot: TIBSCBSTot);
var
  sSecao: string;
begin
  sSecao := 'IBSCBSTot';

  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBSTot.vBCIBSCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCIBSCBS', ''), 0);

    Ler_IBSCBSTot_gIBS(AINIRec, IBSCBSTot.gIBS);
    Ler_IBSCBSTot_gCBS(AINIRec, IBSCBSTot.gCBS);
    Ler_IBSCBSTot_gMono(AINIRec, IBSCBSTot.gMono);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBSTot_gIBS(AINIRec: TMemIniFile; IBS: TgIBSTot);
var
  sSecao: string;
begin
  sSecao := 'gIBS';

  if AINIRec.SectionExists(sSecao) then
  begin
    IBS.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBS', ''), 0);
    IBS.vCredPres := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPres', ''), 0);
    IBS.vCredPresCondSus := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPresCondSus', ''), 0);

    Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec, IBS.gIBSUFTot);
    Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec, IBS.gIBSMunTot);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBSTot_gIBS_gIBSUFTot(AINIRec: TMemIniFile;
  gIBSUFTot: TgIBSUFTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSUFTot';

  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSUFTot.vDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDif', ''), 0);
    gIBSUFTot.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTrib', ''), 0);
    gIBSUFTot.vIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSUF', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBSTot_gIBS_gIBSMunTot(AINIRec: TMemIniFile;
  gIBSMunTot: TgIBSMunTot);
var
  sSecao: string;
begin
  sSecao := 'gIBSMunTot';

  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSMunTot.vDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDif', ''), 0);
    gIBSMunTot.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTrib', ''), 0);
    gIBSMunTot.vIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMun', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBSTot_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBSTot);
var
  sSecao: string;
begin
  sSecao := 'gCBSTot';

  if AINIRec.SectionExists(sSecao) then
  begin
    gCBS.vDif := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDif', ''), 0);
    gCBS.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTrib', ''), 0);
    gCBS.vCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBS', ''), 0);
    gCBS.vCredPres := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPres', ''), 0);
    gCBS.vCredPresCondSus := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCredPresCondSus', ''), 0);
  end;
end;

procedure TNFeIniReader.Ler_IBSCBSTot_gMono(AINIRec: TMemIniFile;
  gMono: TgMono);
var
  sSecao: string;
begin
  sSecao := 'gMono';

  if AINIRec.SectionExists(sSecao) then
  begin
    gMono.vIBSMono := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMono', ''), 0);
    gMono.vCBSMono := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMono', ''), 0);
    gMono.vIBSMonoReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMonoReten', ''), 0);
    gMono.vCBSMonoReten := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMonoReten', ''), 0);
    gMono.vIBSMonoRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vIBSMonoRet', ''), 0);
    gMono.vCBSMonoRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCBSMonoRet', ''), 0);
  end;
end;

end.
