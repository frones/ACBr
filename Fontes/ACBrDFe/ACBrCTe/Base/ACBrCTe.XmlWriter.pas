{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrCTe.XmlWriter;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  ACBrXmlBase, ACBrXmlDocument, ACBrXmlWriter,
  ACBrCTe.Classes,
//  ACBrCTe.Conversao,
  pcteConversaoCTe;

type

  TCTeXmlWriterOptions = class(TACBrXmlWriterOptions)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TACBrTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;

  public
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TACBrTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
  end;

  TCTeXmlWriter = class(TACBrXmlWriter)
  private
    FCTe: TCTe;

    FVersaoDF: TVersaoCTe;
    FModeloDF: TModeloCTe;
    FtpAmb: TACBrTipoAmbiente;
    FtpEmis: TACBrTipoEmissao;
    FIdCSRT: Integer;
    FCSRT: string;
    FChaveCTe: string;

    function Gerar_InfCTe: TACBrXmlNode;
    function Gerar_ProtCTe: TACBrXmlNode;

    function Gerar_IE(const IE, UF: string): TACBrXmlNode;
    function Gerar_Endereco(const xTag: string; Endereco: TEndereco): TACBrXmlNode;

    function Gerar_Ide: TACBrXmlNode;
    function Gerar_Compl: TACBrXmlNode;
    function Gerar_Emit: TACBrXmlNode;
    function Gerar_EnderEmit: TACBrXmlNode;
    function Gerar_autXML: TACBrXmlNodeArray;
    function Gerar_infRespTec: TACBrXmlNode;

    function Gerar_Toma03: TACBrXmlNode;
    function Gerar_Toma4: TACBrXmlNode;

    function Gerar_InfPercurso: TACBrXmlNodeArray;
    function Gerar_ObsCont: TACBrXmlNodeArray;
    function Gerar_ObsFisco: TACBrXmlNodeArray;

    function Gerar_Fluxo: TACBrXmlNode;
    function Gerar_Pass: TACBrXmlNodeArray;

    function Gerar_Entrega: TACBrXmlNode;
    function Gerar_Entrega_SemData: TACBrXmlNode;
    function Gerar_Entrega_ComData: TACBrXmlNode;
    function Gerar_Entrega_NoPeriodo: TACBrXmlNode;
    function Gerar_Entrega_SemHora: TACBrXmlNode;
    function Gerar_Entrega_ComHora: TACBrXmlNode;
    function Gerar_Entrega_NoIntervalo: TACBrXmlNode;

    function Gerar_Tomador: TACBrXmlNode;

    function Gerar_vPrest: TACBrXmlNode;
    function Gerar_Comp(Comp: TCompCollection): TACBrXmlNodeArray;

    function Gerar_Imp: TACBrXmlNode;
    function Gerar_ICMS: TACBrXmlNode;
    function Gerar_CST00: TACBrXmlNode;
    function Gerar_CST20: TACBrXmlNode;
    function Gerar_CST45: TACBrXmlNode;
    function Gerar_CST60: TACBrXmlNode;
    function Gerar_CST90: TACBrXmlNode;
    function Gerar_ICMSOutraUF: TACBrXmlNode;
    function Gerar_ICMSSN: TACBrXmlNode;

    function Gerar_ICMSUFFim: TACBrXmlNode;
    function Gerar_infTribFed: TACBrXmlNode;

    function Gerar_InfCTeNorm: TACBrXmlNode;
    function Gerar_InfServico: TACBrXmlNode;
    function Gerar_InfQ: TACBrXmlNode;
    function Gerar_InfDocRef: TACBrXmlNodeArray;
    function Gerar_InfSeg: TACBrXmlNodeArray;
    function Gerar_InfDoc: TACBrXmlNode;

    function Gerar_InfNF: TACBrXmlNodeArray;
    function Gerar_InfUnidCarga(infUnidCarga: TinfUnidCargaCollection): TACBrXmlNodeArray;
    function Gerar_LacUnidCarga(LacUnidCarga: TLacUnidCargaCollection): TACBrXmlNodeArray;
    function Gerar_InfUnidTransp(infUnidTransp: TinfUnidTranspCollection): TACBrXmlNodeArray;
    function Gerar_LacUnidTransp(LacUnidTransp: TlacUnidTranspCollection): TACBrXmlNodeArray;

    function Gerar_InfNFe(infNFe: TInfNFeCollection): TACBrXmlNodeArray;
    function Gerar_InfOutros: TACBrXmlNodeArray;
    function Gerar_DocAnt: TACBrXmlNode;
    function Gerar_EmiDocAnt(emiDocAnt: TEmiDocAntCollection): TACBrXmlNodeArray;
    function Gerar_IdDocAnt(idDocAnt: TidDocAntCollection): TACBrXmlNodeArray;
    function Gerar_IdDocAntPap(idDocAntPap: TidDocAntPapCollection): TACBrXmlNodeArray;
    function Gerar_IdDocAntEle(idDocAntEle: TidDocAntEleCollection): TACBrXmlNodeArray;
    function Gerar_InfDCe(infDCe: TInfDCeCollection): TACBrXmlNodeArray;

    function Gerar_Peri(peri: TPeriCollection): TACBrXmlNodeArray;
    function Gerar_infTotAP(Idx: Integer; peri: TPeriCollection): TACBrXmlNode;

    function Gerar_VeicNovos: TACBrXmlNodeArray;
    function Gerar_InfGlobalizado: TACBrXmlNode;
    
    function Gerar_InfServVinc: TACBrXmlNode;
    function Gerar_InfCTeMultimodal: TACBrXmlNodeArray;

    function Gerar_InfGTVe: TACBrXmlNodeArray;
    function Gerar_CompGTVe(Comp: TinfGTVeCompCollection): TACBrXmlNodeArray;

    function Gerar_InfCTeComp: TACBrXmlNode;
    function Gerar_InfCTeComp10: TACBrXmlNodeArray;
    function Gerar_InfCTeAnu: TACBrXmlNode;

    function Gerar_Rem: TACBrXmlNode;
    function Gerar_Dest: TACBrXmlNode;
    function Gerar_Exped: TACBrXmlNode;
    function Gerar_Receb: TACBrXmlNode;

    function Gerar_Origem: TACBrXmlNode;
    function Gerar_Destino: TACBrXmlNode;

    function Gerar_DetGTV: TACBrXmlNode;
    function Gerar_InfEspecie: TACBrXmlNodeArray;
    function Gerar_InfVeiculo: TACBrXmlNodeArray;

    function Gerar_InfCarga(infCarga: TInfCarga): TACBrXmlNode;
    function Gerar_InfQCarga(infCarga: TInfCarga): TACBrXmlNodeArray;

    function Gerar_Det: TACBrXmlNodeArray;
    function Gerar_InfDocAnt(infDocAnt: TinfDocAntCollection): TACBrXmlNodeArray;
    function Gerar_InfNFeTranspParcial(infNFeTranspParcial: TinfNFeTranspParcialCollection): TACBrXmlNodeArray;

    function Gerar_InfModal: TACBrXmlNode;
    function Gerar_InfModalSimp: TACBrXmlNode;

    function Gerar_ModalRodo(rodo: TRodo): TACBrXmlNode;
    function Gerar_OCC(rodo: TRodo): TACBrXmlNodeArray;
    function Gerar_EmiOCC(emiOcc: TEmiOCC): TACBrXmlNode;
    function Gerar_ValePed(rodo: TRodo): TACBrXmlNodeArray;
    function Gerar_Veic(rodo: TRodo): TACBrXmlNodeArray;
    function Gerar_VeicProp(prop: TProp): TACBrXmlNode;
    function Gerar_Lacre(rodo: TRodo): TACBrXmlNodeArray;
    function Gerar_Moto(rodo: TRodo): TACBrXmlNodeArray;

    function Gerar_ModalRodoOS: TACBrXmlNode;
    function Gerar_RodoOSVeic(veic: TVeicOS): TACBrXmlNode;
    function Gerar_RodoOSVeicProp(prop: TPropOS): TACBrXmlNode;
    function Gerar_RodoOSInfFretamento(infFretamento: TinfFretamento): TACBrXmlNode;

    function Gerar_ModalAereo(aereo: TAereo): TACBrXmlNode;
    function Gerar_AereoNatCarga(natCarga: TNatCarga; NOcorr: Integer): TACBrXmlNode;
    function Gerar_AereoTarifa(tarifa: TTarifa): TACBrXmlNode;

    function Gerar_ModalAquav(aquav: TAquav): TACBrXmlNode;
    function Gerar_Balsa(balsa: TBalsaCollection): TACBrXmlNodeArray;
    function Gerar_DetCont(detCont: TdetContCollection): TACBrXmlNodeArray;
    function Gerar_AquavLacre(lacre: TLacreCollection): TACBrXmlNodeArray;
    function Gerar_AquavInfDoc(infDoc: TInfDocAquav): TACBrXmlNode;
    function Gerar_AquavInfNF(infNF: TInfNFAquavCollection): TACBrXmlNodeArray;
    function Gerar_AquavInfNFe(infNFe: TInfNFeAquavCollection): TACBrXmlNodeArray;

    function Gerar_ModalFerrov(ferrov: TFerrov): TACBrXmlNode;
    function Gerar_TrafMut(ferrov: TFerrov): TACBrXmlNode;
    function Gerar_TrafMut300(ferrov: TFerrov): TACBrXmlNode;
    function Gerar_FerroEnv(ferroEnv: TFerroEnvCollection): TACBrXmlNodeArray;
    function Gerar_FerrovEndereco(enderFerro: TEnderFerro): TACBrXmlNode;
    function Gerar_detVag(detVag: TDetVagCollection): TACBrXmlNodeArray;

    function Gerar_ModalDuto(duto: TDuto): TACBrXmlNode;

    function Gerar_ModalMultimodal: TACBrXmlNode;
    function Gerar_MultimodalSeg(multimodal: TMultimodal): TACBrXmlNode;
    function Gerar_MultimodalInfSeg(multimodal: TMultimodal): TACBrXmlNode;

    function Gerar_Cobr(cobr: TCobr): TACBrXmlNode;
    function Gerar_CobrFat(cobr: TCobr): TACBrXmlNode;
    function Gerar_CobrDup(cobr: TCobr): TACBrXmlNodeArray;

    function Gerar_InfCTeSub(infCTeSub: TInfCteSub): TACBrXmlNode;
    function Gerar_TomaICMS(tomaICMS: TTomaICMS): TACBrXmlNode;
    function Gerar_RefNF(refNF: TRefNF): TACBrXmlNode;
    function Gerar_TomaNaoICMS(tomaNaoICMS: TTomaNaoICMS): TACBrXmlNode;

    function Gerar_Total: TACBrXmlNode;

    // Reforma Tributária
    function Gerar_Ide_CompraGov(gCompraGov: TgCompraGovReduzido): TACBrXmlNode;

    function Gerar_IBSCBS(IBSCBS: TIBSCBS): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS(gIBSCBS: TgIBSCBS): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gIBSUF(gIBSUF: TgIBSUFValores): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSMun(gIBSMun: TgIBSMunValores): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gCBS(gCBS: TgCBSValores): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gIBSUF_gDif(Dif: TgDif): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSMun_gDif(Dif: TgDif): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gCBS_gDif(Dif: TgDif): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(DevTrib: TgDevTrib): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(Red: TgRed): TACBrXmlNode;

    function Gerar_IBSCBSSel_gIBSCBS_gTribRegular(gTribRegular: TgTribRegular): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(gIBSCredPres: TgIBSCBSCredPres;
      const Grupo: string): TACBrXmlNode;
    function Gerar_gTribCompraGov(gTribCompraGov: TgTribCompraGov): TACBrXmlNode;

    function GetOpcoes: TCTeXmlWriterOptions;
    procedure SetOpcoes(AValue: TCTeXmlWriterOptions);

    procedure AjustarMunicipioUF(out xUF: string; out xMun: string;
      out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
    function DefineArredondamentoQtdRat: TACBrTipoCampo;
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

  public
    constructor Create(AOwner: TCTe); reintroduce;
    destructor Destroy; override;

    function GerarXml: boolean; override;
    function ObterNomeArquivo: string; overload;

    property Opcoes: TCTeXmlWriterOptions read GetOpcoes write SetOpcoes;
    property CTe: TCTe read FCTe write FCTe;

    property VersaoDF: TVersaoCTe read FVersaoDF write FVersaoDF;
    property ModeloDF: TModeloCTe read FModeloDF write FModeloDF;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property tpEmis: TACBrTipoEmissao read FtpEmis write FtpEmis;
    property IdCSRT: Integer read FIdCSRT write FIdCSRT;
    property CSRT: string read FCSRT write FCSRT;
  end;

implementation

uses
  StrUtils,
  Math,
  ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrDFe.Conversao,
  ACBrDFeUtil,
  ACBrDFeConsts,
  ACBrCTe,
  ACBrCTe.Consts;

{ TCTeXmlWriter }

constructor TCTeXmlWriter.Create(AOwner: TCTe);
begin
  inherited Create;

  Opcoes.AjustarTagNro := True;
  Opcoes.GerarTagIPIparaNaoTributado := True;
  Opcoes.NormatizarMunicipios := False;
  Opcoes.PathArquivoMunicipios := '';
  Opcoes.GerarTagAssinatura := TACBrTagAssinatura(taSomenteSeAssinada);
  Opcoes.ValidarInscricoes := False;
  Opcoes.ValidarListaServicos := False;

  FCTe := AOwner;
end;

function TCTeXmlWriter.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TCTeXmlWriterOptions.Create();
end;

destructor TCTeXmlWriter.Destroy;
begin
  inherited Destroy;
end;

function TCTeXmlWriter.GetOpcoes: TCTeXmlWriterOptions;
begin
  Result := TCTeXmlWriterOptions(FOpcoes);
end;

function TCTeXmlWriter.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FCTe.infCTe.ID) + '-cte.xml';
end;

procedure TCTeXmlWriter.SetOpcoes(AValue: TCTeXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

procedure TCTeXmlWriter.AjustarMunicipioUF(out xUF: string; out xMun: string;
  out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
var
  PaisBrasil: boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;

  cMun := IfThen(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IfThen(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF  := IfThen(PaisBrasil, vxUF, UF_EXTERIOR);

  if Opcoes.NormatizarMunicipios then
    if ((EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR)) then
      cMun := ObterCodigoMunicipio(xMun, xUF, Opcoes.FPathArquivoMunicipios)
    else if ((EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR)) then
      xMun := ObterNomeMunicipio(cMun, xUF, Opcoes.FPathArquivoMunicipios);
end;

function TCTeXmlWriter.DefineArredondamentoQtdRat: TACBrTipoCampo;
begin
  if VersaoDF <= ve300 then
    Result := tcDe2
  else
    Result := tcDe3;
end;

function TCTeXmlWriter.Gerar_ProtCTe: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('protCTe');

  Result.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));

  xmlNode := Result.AddChild('infProt');

  xmlNode.AddChild('tpAmb').Content := TpAmbToStr(CTe.procCTe.tpAmb);

  xmlNode.AddChild('verAplic').Content := CTe.procCTe.verAplic;

  xmlNode.AddChild('chBPe').Content := CTe.procCTe.chCTe;

  xmlNode.AddChild('dhRecbto').Content :=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', CTe.procCTe.dhRecbto) +
    GetUTC(CodigoUFparaUF(CTe.Ide.cUF), CTe.procCTe.dhRecbto);

  xmlNode.AddChild('nProt').Content := CTe.procCTe.nProt;

  xmlNode.AddChild('digVal').Content := CTe.procCTe.digVal;

  xmlNode.AddChild('cStat').Content := IntToStr(CTe.procCTe.cStat);

  xmlNode.AddChild('xMotivo').Content := CTe.procCTe.xMotivo;
end;

function TCTeXmlWriter.Gerar_IE(const IE, UF: string): TACBrXmlNode;
begin
  if Trim(IE) = 'ISENTO' then
    Result := AddNode(tcStr, '#041', 'IE', 0, 14, 1, IE, DSC_IE)
  else
    Result := AddNode(tcStr, '#041', 'IE', 0, 14, 0,
                                                       OnlyNumber(IE), DSC_IE);

  if (Opcoes.ValidarInscricoes) then
    if not ValidarIE(IE, UF) then
      wAlerta('#041', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

function TCTeXmlWriter.Gerar_Endereco(const xTag: string;
  Endereco: TEndereco): TACBrXmlNode;
var
  cMun: Integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, Endereco.cPais,
                     Endereco.UF,
                     Endereco.xMun,
                     Endereco.cMun);

  Result := FDocument.CreateElement(xTag);

  Result.AppendChild(AddNode(tcStr, 'C05', 'xLgr', 2, 60, 1,
                                       Endereco.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, 'C06', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, Endereco.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, 'C07', 'xCpl', 1, 60, 0,
                                       Endereco.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, 'C08', 'xBairro', 2, 60, 1,
                                 Endereco.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, 'C09', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('C09', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'C10', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, 'C12', 'CEP', 8, 8, 1,
                                         Endereco.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, 'C11', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('C11', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcInt, 'C13', 'cPais', 4, 4, 0,
                                                    Endereco.cPais, DSC_CPAIS));

  Result.AppendChild(AddNode(tcStr, 'C15', 'xPais', 2, 60, 0,
                                     Endereco.xPais, DSC_XPAIS));
end;

function TCTeXmlWriter.Gerar_InfCTe: TACBrXmlNode;
var
  i: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('infCte');

  Result.SetAttribute('Id', CTe.infCTe.ID);
  Result.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));

  Result.AppendChild(Gerar_Ide);
  Result.AppendChild(Gerar_Compl);
  Result.AppendChild(Gerar_Emit);

  case ModeloDF of
    moCTeOS:
      begin
        Result.AppendChild(Gerar_Tomador);
        Result.AppendChild(Gerar_vPrest);
        Result.AppendChild(Gerar_Imp);

        Result.AppendChild(Gerar_InfCTeNorm);

        if VersaoDF <= ve300 then
        begin
          Result.AppendChild(Gerar_InfCTeComp);
          Result.AppendChild(Gerar_InfCTeAnu);
        end
        else
        begin
          nodeArray := Gerar_InfCTeComp10;

          for i := 0 to CTe.infCTeComp10.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;
        end;
      end;

    moGTVe:
      begin
        Result.AppendChild(Gerar_Rem);
        Result.AppendChild(Gerar_Dest);
        Result.AppendChild(Gerar_Origem);
        Result.AppendChild(Gerar_Destino);
        Result.AppendChild(Gerar_DetGTV);
      end;

    moCTeSimp:
      begin
        Result.AppendChild(Gerar_Tomador);
        Result.AppendChild(Gerar_InfCarga(CTe.InfCarga));

        nodeArray := Gerar_Det;

        for i := 0 to CTe.det.Count - 1 do
        begin
          Result.AppendChild(nodeArray[i]);
        end;

        Result.AppendChild(Gerar_InfModalSimp);
        Result.AppendChild(Gerar_Cobr(CTe.cobr));
        Result.AppendChild(Gerar_InfCTeSub(CTe.infCteSub));
        Result.AppendChild(Gerar_Imp);
        Result.AppendChild(Gerar_Total);
      end;
  else
    begin
      Result.AppendChild(Gerar_Rem);
      Result.AppendChild(Gerar_Exped);
      Result.AppendChild(Gerar_Receb);
      Result.AppendChild(Gerar_Dest);
      Result.AppendChild(Gerar_vPrest);
      Result.AppendChild(Gerar_Imp);

      Result.AppendChild(Gerar_InfCTeNorm);

      if VersaoDF <= ve300 then
      begin
        Result.AppendChild(Gerar_InfCTeComp);
        Result.AppendChild(Gerar_InfCTeAnu);
      end
      else
      begin
        nodeArray := Gerar_InfCTeComp10;

        for i := 0 to CTe.infCTeComp10.Count - 1 do
        begin
          Result.AppendChild(nodeArray[i]);
        end;
      end;
    end;
  end;

  nodeArray := Gerar_autXML;

  for i := 0 to CTe.autXML.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  if VersaoDF >= ve300 then
    Result.AppendChild(Gerar_infRespTec);
end;

function TCTeXmlWriter.Gerar_Ide: TACBrXmlNode;
var
  Obrigatorio: Integer;
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('ide');

  Result.AppendChild(AddNode(tcInt, '#005', 'cUF', 2, 2, 1,
                                                         CTe.ide.cUF, DSC_CUF));

  if not ValidarCodigoUF(CTe.ide.cUF) then
    wAlerta('#005', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcInt, '#006', 'cCT', 8, 8, 1,
                                                         CTe.Ide.cCT, DSC_CDF));

  Result.AppendChild(AddNode(tcInt, '#007', 'CFOP', 4, 4, 1,
                                                       CTe.Ide.CFOP, DSC_CFOP));

  Result.AppendChild(AddNode(tcStr, '#008', 'natOp', 1, 60, 1,
                                                     CTe.Ide.natOp, DSC_NATOP));

  if VersaoDF = ve200 then
    Result.AppendChild(AddNode(tcStr, '#009', 'forPag', 1, 1, 1,
                                    tpforPagToStr(CTe.ide.forPag), DSC_INDPAG));

  Result.AppendChild(AddNode(tcInt, '#010', 'mod', 2, 2, 1, CTe.Ide.modelo, ''));

  Result.AppendChild(AddNode(tcInt, '#011', 'serie', 1, 3, 1,
                                                     CTe.ide.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#012', 'nCT', 1, 9, 1,
                                                         CTe.ide.nCT, DSC_NDF));

  if VersaoDF >= ve300 then
    Result.AppendChild(AddNode(tcStr, '#013', 'dhEmi', 25, 25, 1,
      DateTimeTodh(CTe.ide.dhEmi) +
                  GetUTC(CodigoUFparaUF(CTe.ide.cUF), CTe.ide.dhEmi), DSC_DEMI))
  else
    Result.AppendChild(AddNode(tcDatHor, '#013', 'dhEmi', 19, 19, 1,
                                                      CTe.ide.dhEmi, DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, '#014', 'tpImp', 1, 1, 1,
                                         tpImpToStr(CTe.Ide.tpImp), DSC_TPIMP));

  Result.AppendChild(AddNode(tcStr, '#015', 'tpEmis', 1, 1, 1,
                                      tpEmisToStr(CTe.Ide.tpEmis), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcInt, '#016', 'cDV', 1, 1, 1,
                                                         CTe.Ide.cDV, DSC_CDV));

  Result.AppendChild(AddNode(tcStr, '#017', 'tpAmb', 1, 1, 1,
                                         tpAmbToStr(CTe.Ide.tpAmb), DSC_TPAMB));

  Result.AppendChild(AddNode(tcStr, '#018', 'tpCTe', 1, 1, 1,
                                      tpCTePagToStr(CTe.Ide.tpCTe), DSC_TPCTE));

  if ModeloDF <> moGTVe then
    Result.AppendChild(AddNode(tcStr, '#019', 'procEmi', 1, 1, 1,
                                   procEmiToStr(CTe.Ide.procEmi), DSC_PROCEMI));

  Result.AppendChild(AddNode(tcStr, '#020', 'verProc', 1, 20, 1,
                                                 CTe.Ide.verProc, DSC_VERPROC));

  if (VersaoDF >= ve300) and (ModeloDF = moCTe) and (CTe.ide.indGlobalizado = tiSim) then
    Result.AppendChild(AddNode(tcStr, '#020', 'indGlobalizado', 1, 1, 1,
                  TindicadorToStr(CTe.Ide.indGlobalizado), DSC_INDGLOBALIZADO));

  if (VersaoDF < ve300) then
  begin
    Result.AppendChild(AddNode(tcStr, '#021', 'refCTE', 44, 44, 0,
                                       OnlyNumber(CTe.Ide.refCTE), DSC_REFCTE));

    if OnlyNumber(CTe.Ide.refCTe) <> '' then
      if not ValidarChave(CTe.Ide.refCTe) then
        wAlerta('#021', 'refCTE', DSC_REFCTE, ERR_MSG_INVALIDO);
  end;

  Result.AppendChild(AddNode(tcInt, '#022', 'cMunEnv', 7, 7, 1,
                                                 CTe.Ide.cMunEnv, DSC_CMUNEMI));

  if not ValidarMunicipio(CTe.ide.cMunEnv) then
    wAlerta('#022', 'cMunEnv', DSC_CMUNEMI, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#023', 'xMunEnv', 2, 60, 1,
                                                    CTe.Ide.xMunEnv, DSC_XMUN));

  Result.AppendChild(AddNode(tcStr, '#024', 'UFEnv', 2, 2, 1,
                                                        CTe.Ide.UFEnv, DSC_UF));

  if not ValidarUF(CTe.ide.UFEnv) then
    wAlerta('#024', 'UFEnv', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#025', 'modal', 2, 2, 1,
                                       TpModalToStr(CTe.Ide.modal), DSC_MODAL));

  Result.AppendChild(AddNode(tcStr, '#026', 'tpServ', 1, 1, 1,
                                   TpServPagToStr(CTe.Ide.tpServ), DSC_TPSERV));

  if (VersaoDF >= ve300) and (ModeloDF = moCTeOS) then
    Result.AppendChild(AddNode(tcStr, '#027', 'indIEToma', 1, 1, 1,
                             indIEDestToStr(CTe.Ide.indIEToma), DSC_INDIETOMA));

  if (ModeloDF = moCTe) then
    Obrigatorio := 1
  else
    Obrigatorio := 0;

  if ModeloDF <> moGTVe then
  begin
    if ModeloDF = moCTeSimp then
    begin
      Result.AppendChild(AddNode(tcStr, '#028', 'UFIni', 2, 2, 1,
                                                        CTe.Ide.UFIni, DSC_UF));

      if not ValidarUF(CTe.ide.UFIni) then
        wAlerta('#028', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);

      Result.AppendChild(AddNode(tcStr, '#029', 'UFFim', 2, 2, 1,
                                                        CTe.Ide.UFFim, DSC_UF));

      if not ValidarUF(CTe.ide.UFFim) then
        wAlerta('#029', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);
    end
    else
    begin
      Result.AppendChild(AddNode(tcInt, '#028', 'cMunIni', 7, 7, Obrigatorio,
                                                 CTe.Ide.cMunIni, DSC_CMUNEMI));

      if (Obrigatorio = 1) and not ValidarMunicipio(CTe.ide.cMunIni) then
        wAlerta('#028', 'cMunIni', DSC_CMUNEMI, ERR_MSG_INVALIDO);

      Result.AppendChild(AddNode(tcStr, '#029', 'xMunIni', 2, 60, Obrigatorio,
                                                    CTe.Ide.xMunIni, DSC_XMUN));

      Result.AppendChild(AddNode(tcStr, '#030', 'UFIni', 2, 2, Obrigatorio,
                                                        CTe.Ide.UFIni, DSC_UF));

      if (Obrigatorio = 1) and not ValidarUF(CTe.ide.UFIni) then
        wAlerta('#030', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);

      Result.AppendChild(AddNode(tcInt, '#031', 'cMunFim', 7, 7, Obrigatorio,
                                                 CTe.Ide.cMunFim, DSC_CMUNEMI));

      if (Obrigatorio = 1) and not ValidarMunicipio(CTe.ide.cMunFim) then
        wAlerta('#031', 'cMunFim', DSC_CMUNEMI, ERR_MSG_INVALIDO);

      Result.AppendChild(AddNode(tcStr, '#032', 'xMunFim', 2, 60, Obrigatorio,
                                                    CTe.Ide.xMunFim, DSC_XMUN));

      Result.AppendChild(AddNode(tcStr, '#033', 'UFFim', 2, 2, Obrigatorio,
                                                        CTe.Ide.UFFim, DSC_UF));

      if (Obrigatorio = 1) and not ValidarUF(CTe.ide.UFFim) then
        wAlerta('#033', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);
    end;
  end;

  if ModeloDF in [moCTe, moCTeSimp] then
  begin
    Result.AppendChild(AddNode(tcStr, '#034', 'retira', 1, 1, 1,
                                 TpRetiraPagToStr(CTe.Ide.retira), DSC_RETIRA));

    Result.AppendChild(AddNode(tcStr, '#035', 'xDetRetira', 1, 160, 0,
                                                 CTe.Ide.xdetretira, DSC_DRET));
  end;

  if (VersaoDF >= ve300) and not (ModeloDF in [moCTeOS, moCTeSimp]) then
    Result.AppendChild(AddNode(tcStr, '#036', 'indIEToma', 1, 1, 1,
                             indIEDestToStr(CTe.Ide.indIEToma), DSC_INDIETOMA));

  if ModeloDF = moGTVe then
  begin
    Result.AppendChild(AddNode(tcStr, '#037', 'dhSaidaOrig', 25, 25, 1,
             DateTimeWithTimeZone(CTe.ide.dhSaidaOrig, CTe.ide.cUF), DSC_DEMI));

    Result.AppendChild(AddNode(tcStr, '#038', 'dhChegadaDest', 25, 25, 1,
           DateTimeWithTimeZone(CTe.ide.dhChegadaDest, CTe.ide.cUF), DSC_DEMI));
  end;

  if not (ModeloDF in [moCTeOS, moCTeSimp]) then
  begin
    Result.AppendChild(Gerar_Toma03);
    Result.AppendChild(Gerar_Toma4);
  end;

  if (ModeloDF = moCTeOS) then
  begin
    nodeArray := Gerar_InfPercurso;

    for i := 0 to CTe.Ide.infPercurso.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;

  if CTe.Ide.tpEmis = TpcnTipoEmissao(teFSDA) then
  begin
    if VersaoDF >= ve300 then
      Result.AppendChild(AddNode(tcStr, '#057', 'dhCont', 25, 25, 1,
        DateTimeTodh(CTe.ide.dhCont) +
               GetUTC(CodigoUFparaUF(CTe.ide.cUF), CTe.ide.dhCont), DSC_DHCONT))
    else
      Result.AppendChild(AddNode(tcDatHor, '#057', 'dhCont', 19, 19, 1,
                                                   CTe.ide.dhCont, DSC_DHCONT));

    Result.AppendChild(AddNode(tcStr, '#058', 'xJust', 15, 256, 1,
                                                 CTe.Ide.xJust, DSC_XJUSTCONT));
  end;

  // Reforma Tributária
  Result.AppendChild(Gerar_Ide_CompraGov(CTe.Ide.gCompraGov));
end;

function TCTeXmlWriter.Gerar_Ide_CompraGov(
  gCompraGov: TgCompraGovReduzido): TACBrXmlNode;
begin
  Result := nil;

  if gCompraGov.pRedutor > 0 then
  begin
    Result := FDocument.CreateElement('gCompraGov');

    Result.AppendChild(AddNode(tcStr, 'B32', 'tpEnteGov', 1, 1, 1,
                          tpEnteGovToStr(gCompraGov.tpEnteGov), DSC_TPENTEGOV));

    Result.AppendChild(AddNode(tcDe4, 'B33', 'pRedutor', 1, 7, 1,
                                            gCompraGov.pRedutor, DSC_PREDUTOR));
  end;
end;

function TCTeXmlWriter.Gerar_Compl: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  case ModeloDF of
    moGTVe:
      begin
        if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
           (trim(CTe.Compl.xEmi) <> '') or (trim(CTe.Compl.xObs) <> '') or
           (CTe.Compl.ObsCont.Count > 0) or (CTe.Compl.ObsFisco.Count > 0) then
        begin
          Result := FDocument.CreateElement('compl');

          Result.AppendChild(AddNode(tcStr, '#060', 'xCaracAd', 1, 15, 0,
                                             CTe.Compl.xCaracAd, DSC_XCARACAD));

          Result.AppendChild(AddNode(tcStr, '#061', 'xCaracSer', 1, 30, 0,
                                           CTe.Compl.xCaracSer, DSC_XCARACSET));

          Result.AppendChild(AddNode(tcStr, '#062', 'xEmi', 1, 20, 0,
                                                     CTe.Compl.xEmi, DSC_XEMI));

          Result.AppendChild(AddNode(tcStr, '#063', 'xObs', 1, 2000, 0,
                                                     CTe.Compl.xObs, DSC_XOBS));

          nodeArray := Gerar_ObsCont;

          for i := 0 to CTe.Compl.obsCont.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;

          nodeArray := Gerar_ObsFisco;

          for i := 0 to CTe.Compl.obsFisco.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;
        end;
      end;

    moCTeOS:
      begin
        if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
           (trim(CTe.Compl.xEmi) <> '') or (trim(CTe.Compl.fluxo.xOrig) <> '') or
           (CTe.Compl.fluxo.pass.Count > 0) or (trim(CTe.Compl.fluxo.xDest) <> '') or
           (trim(CTe.Compl.fluxo.xRota) <> '') or (trim(CTe.Compl.xObs) <> '') or
           (CTe.Compl.ObsCont.Count > 0) or (CTe.Compl.ObsFisco.Count > 0) then
        begin
          Result := FDocument.CreateElement('compl');

          Result.AppendChild(AddNode(tcStr, '#060', 'xCaracAd', 1, 15, 0,
                                             CTe.Compl.xCaracAd, DSC_XCARACAD));

          Result.AppendChild(AddNode(tcStr, '#061', 'xCaracSer', 1, 30, 0,
                                           CTe.Compl.xCaracSer, DSC_XCARACSET));

          Result.AppendChild(AddNode(tcStr, '#062', 'xEmi', 1, 20, 0,
                                                     CTe.Compl.xEmi, DSC_XEMI));

          Result.AppendChild(Gerar_Fluxo);

          Result.AppendChild(AddNode(tcStr, '#063', 'xObs', 1, 2000, 0,
                                                     CTe.Compl.xObs, DSC_XOBS));

          nodeArray := Gerar_ObsCont;

          for i := 0 to CTe.Compl.obsCont.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;

          nodeArray := Gerar_ObsFisco;

          for i := 0 to CTe.Compl.obsFisco.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;
        end;
      end;

    moCTeSimp:
      begin
        if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
           (trim(CTe.Compl.fluxo.xOrig) <> '') or (CTe.Compl.fluxo.pass.Count > 0) or
           (trim(CTe.Compl.fluxo.xDest) <> '') or (trim(CTe.Compl.fluxo.xRota) <> '') or
           (trim(CTe.Compl.xObs) <> '') or (CTe.Compl.ObsCont.Count > 0) or
           (CTe.Compl.ObsFisco.Count > 0) then
        begin
          Result := FDocument.CreateElement('compl');

          Result.AppendChild(AddNode(tcStr, '#060', 'xCaracAd', 1, 15, 0,
                                             CTe.Compl.xCaracAd, DSC_XCARACAD));

          Result.AppendChild(AddNode(tcStr, '#061', 'xCaracSer', 1, 30, 0,
                                           CTe.Compl.xCaracSer, DSC_XCARACSET));

          Result.AppendChild(Gerar_Fluxo);

          Result.AppendChild(AddNode(tcStr, '#063', 'xObs', 1, 2000, 0,
                                                     CTe.Compl.xObs, DSC_XOBS));

          nodeArray := Gerar_ObsCont;

          for i := 0 to CTe.Compl.obsCont.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;

          nodeArray := Gerar_ObsFisco;

          for i := 0 to CTe.Compl.obsFisco.Count - 1 do
          begin
            Result.AppendChild(nodeArray[i]);
          end;
        end;
      end;
  else
    begin
      if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
         (trim(CTe.Compl.xEmi) <> '') or (trim(CTe.Compl.fluxo.xOrig) <> '') or
         (CTe.Compl.fluxo.pass.Count > 0) or (trim(CTe.Compl.fluxo.xDest) <> '') or
         (trim(CTe.Compl.fluxo.xRota) <> '') or
         ((CTe.Compl.Entrega.TipoData <> tdNaoInformado) and
          (CTe.Compl.Entrega.TipoHora <> thNaoInformado)) or
         (trim(CTe.Compl.origCalc) <> '') or (trim(CTe.Compl.destCalc) <> '') or
         (trim(CTe.Compl.xObs) <> '') or
         (CTe.Compl.ObsCont.Count > 0) or (CTe.Compl.ObsFisco.Count > 0) then
      begin
        Result := FDocument.CreateElement('compl');

        Result.AppendChild(AddNode(tcStr, '#060', 'xCaracAd', 1, 15, 0,
                                             CTe.Compl.xCaracAd, DSC_XCARACAD));

        Result.AppendChild(AddNode(tcStr, '#061', 'xCaracSer', 1, 30, 0,
                                           CTe.Compl.xCaracSer, DSC_XCARACSET));

        Result.AppendChild(AddNode(tcStr, '#062', 'xEmi', 1, 20, 0,
                                                     CTe.Compl.xEmi, DSC_XEMI));

        Result.AppendChild(Gerar_Fluxo);

        if (CTe.Compl.Entrega.TipoData <> tdNaoInformado) and
           (CTe.Compl.Entrega.TipoHora <> thNaoInformado) then
          Result.AppendChild(Gerar_Entrega);

        Result.AppendChild(AddNode(tcStr, '#063', 'origCalc', 2, 40, 0,
                                             CTe.Compl.origCalc, DSC_ORIGCALC));

        Result.AppendChild(AddNode(tcStr, '#064', 'destCalc', 2, 40, 0,
                                             CTe.Compl.destCalc, DSC_DESTCALC));

        Result.AppendChild(AddNode(tcStr, '#065', 'xObs', 1, 2000, 0,
                                                     CTe.Compl.xObs, DSC_XOBS));

        nodeArray := Gerar_ObsCont;

        for i := 0 to CTe.Compl.obsCont.Count - 1 do
        begin
          Result.AppendChild(nodeArray[i]);
        end;

        nodeArray := Gerar_ObsFisco;

        for i := 0 to CTe.Compl.obsFisco.Count - 1 do
        begin
          Result.AppendChild(nodeArray[i]);
        end;
      end;
    end;
  end;
end;

function TCTeXmlWriter.Gerar_Emit: TACBrXmlNode;
var
  Obrigatorio: Integer;
begin
  Result := FDocument.CreateElement('emit');

  Result.AppendChild(AddNodeCNPJ('#098', CTe.Emit.CNPJ, CODIGO_BRASIL, True));

  Result.AppendChild(AddNode(tcStr, '#099', 'IE', 2, 14, 1,
                                              OnlyNumber(CTe.Emit.IE), DSC_IE));

  if (Opcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Emit.IE, CTe.Emit.enderEmit.UF) then
      wAlerta('#099', 'IE', DSC_IE, ERR_MSG_INVALIDO);

  if (VersaoDF >= ve300) and (ModeloDF = moCTe) then
    Result.AppendChild(AddNode(tcStr, '#100', 'IEST', 2, 14, 0,
                                          OnlyNumber(CTe.Emit.IEST), DSC_IEST));

  Result.AppendChild(AddNode(tcStr, '#101', 'xNome', 2, 60, 1,
                                                    CTe.Emit.xNome, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#102', 'xFant', 2, 60, 0,
                                                    CTe.Emit.xFant, DSC_XFANT));

  Result.AppendChild(Gerar_EnderEmit);

  if ModeloDF <> moGTVe then
  begin
    Obrigatorio := 0;

    if VersaoDF >= ve400 then
      Obrigatorio := 1;

    Result.AppendChild(AddNode(tcStr, '#103', 'CRT', 1, 1, Obrigatorio,
                                        CRTCTeToStr(CTe.Emit.CRT), DSC_CRTCTE));
  end;
end;

function TCTeXmlWriter.Gerar_EnderEmit: TACBrXmlNode;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, CTe.Emit.enderEmit.UF,
    CTe.Emit.enderEmit.xMun, CTe.Emit.EnderEmit.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, 'C05', 'xLgr', 2, 60, 1,
                                            CTe.Emit.enderEmit.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, 'C06', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, CTe.Emit.enderEmit.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, 'C07', 'xCpl', 1, 60, 0,
                                            CTe.Emit.enderEmit.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, 'C08', 'xBairro', 2, 60, 1,
                                      CTe.Emit.enderEmit.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, 'C09', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('C09', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'C10', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, 'C12', 'CEP', 8, 8, 1,
                                              CTe.Emit.enderEmit.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, 'C11', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('C11', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'C15', 'fone', 7, 12, 0,
                                OnlyNumber(CTe.Emit.enderEmit.fone), DSC_FONE));
end;

function TCTeXmlWriter.Gerar_Toma03: TACBrXmlNode;
begin
  Result := nil;

  if (trim(CTe.Ide.Toma4.xNome) = '') then
  begin
    if VersaoDF >= ve300 then
    begin
      if ModeloDF = moCTe then
        Result := FDocument.CreateElement('toma3')
      else
        Result := FDocument.CreateElement('toma');
    end
    else
      Result := FDocument.CreateElement('toma03');

    Result.AppendChild(AddNode(tcStr, '#036', 'toma', 1, 1, 1,
                                TpTomadorToStr(CTe.ide.Toma03.Toma), DSC_TOMA));
  end;
end;

function TCTeXmlWriter.Gerar_Toma4: TACBrXmlNode;
begin
  Result := nil;

  if (trim(CTe.Ide.Toma4.IE) <> '') or (trim(CTe.Ide.Toma4.xNome) <> '') then
  begin
    if ModeloDF = moCTe then
      Result := FDocument.CreateElement('toma4')
    else
      Result := FDocument.CreateElement('tomaTerceiro');

    Result.AppendChild(AddNode(tcStr, '#038', 'toma', 1, 1, 1,
                                 TpTomadorToStr(CTe.ide.Toma4.Toma), DSC_TOMA));

    if CTe.Ide.Toma4.EnderToma.cPais = 0 then
      CTe.Ide.Toma4.EnderToma.cPais := 1058;

    if CTe.Ide.Toma4.EnderToma.cPais = 1058 then
      Result.AppendChild(AddNodeCNPJCPF('#039', '#040', CTe.ide.Toma4.CNPJCPF))
    else
      Result.AppendChild(AddNodeCNPJ('#039', '00000000000000', CODIGO_BRASIL, True));

    Result.AppendChild(Gerar_IE(CTe.Ide.Toma4.IE, CTe.Ide.Toma4.EnderToma.UF));

    Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                               CTe.ide.Toma4.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, '#043', 'xFant', 2, 60, 0,
                                               CTe.ide.Toma4.xFant, DSC_XFANT));

    Result.AppendChild(AddNode(tcStr, '#044', 'fone', 7, 12, 0,
                                     OnlyNumber(CTe.Ide.Toma4.fone), DSC_FONE));

    Result.AppendChild(Gerar_Endereco('enderToma', CTe.Ide.Toma4.EnderToma));

    Result.AppendChild(AddNode(tcStr, '#056', 'email', 1, 60, 0,
                                               CTe.Ide.Toma4.email, DSC_EMAIL));
  end;
end;

function TCTeXmlWriter.Gerar_InfPercurso: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.Ide.infPercurso.Count);

  for i := 0 to CTe.Ide.infPercurso.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infPercurso');

    Result[i].AppendChild(AddNode(tcStr, '#024', 'UFPer', 2, 2, 1,
                                      CTe.Ide.infPercurso[i].UFPer, DSC_UFPER));
  end;

  if CTe.Ide.infPercurso.Count > 10 then
    wAlerta('#023', 'infPercurso', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TCTeXmlWriter.Gerar_ObsCont: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.Compl.ObsCont.Count);

  for i := 0 to CTe.Compl.ObsCont.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('ObsCont');
    Result[i].SetAttribute('xCampo', CTe.Compl.ObsCont[i].xCampo);

    if length(trim(CTe.Compl.ObsCont[i].xCampo)) > 20 then
      wAlerta('ZO5', 'xCampo', DSC_XCAMPO, ERR_MSG_MAIOR);

    if length(trim(CTe.Compl.ObsCont[i].xCampo)) = 0 then
      wAlerta('ZO5', 'xCampo', DSC_XCAMPO, ERR_MSG_VAZIO);

    Result[i].AppendChild(AddNode(tcStr, 'Z06', 'xTexto', 1, 160, 1,
                                      CTe.Compl.ObsCont[i].xTexto, DSC_XTEXTO));
  end;

  if CTe.Compl.ObsCont.Count > 10 then
    wAlerta('F01', 'ObsCont', DSC_OBSCONT, ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TCTeXmlWriter.Gerar_ObsFisco: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.Compl.ObsFisco.Count);

  for i := 0 to CTe.Compl.ObsFisco.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('ObsFisco');
    Result[i].SetAttribute('xCampo', CTe.Compl.ObsFisco[i].xCampo);

    if length(trim(CTe.Compl.ObsFisco[i].xCampo)) > 20 then
      wAlerta('ZO5', 'xCampo', DSC_XCAMPO, ERR_MSG_MAIOR);

    if length(trim(CTe.Compl.ObsFisco[i].xCampo)) = 0 then
      wAlerta('ZO5', 'xCampo', DSC_XCAMPO, ERR_MSG_VAZIO);

    Result[i].AppendChild(AddNode(tcStr, 'Z06', 'xTexto', 1, 160, 1,
                                     CTe.Compl.ObsFisco[i].xTexto, DSC_XTEXTO));
  end;

  if CTe.Compl.ObsFisco.Count > 10 then
    wAlerta('F01', 'ObsFisco', DSC_OBSFISCO, ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TCTeXmlWriter.Gerar_Fluxo: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if (trim(CTe.Compl.fluxo.xOrig) <> '') or (CTe.Compl.fluxo.pass.Count > 0) or
     (trim(CTe.Compl.fluxo.xDest) <> '') or (trim(CTe.Compl.fluxo.xRota) <> '') then
  begin
    Result := FDocument.CreateElement('fluxo');

    Result.AppendChild(AddNode(tcStr, '#064', 'xOrig', 1, 60, 0,
                                             CTe.Compl.fluxo.xOrig, DSC_XORIG));

    nodeArray := Gerar_Pass;

    for i := 0 to CTe.Compl.fluxo.pass.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    Result.AppendChild(AddNode(tcStr, '#067', 'xDest', 1, 60, 0,
                                             CTe.Compl.fluxo.xDest, DSC_XDEST));

    Result.AppendChild(AddNode(tcStr, '#068', 'xRota', 1, 10, 0,
                                             CTe.Compl.fluxo.xRota, DSC_XROTA));
  end;
end;

function TCTeXmlWriter.Gerar_Pass: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.Compl.fluxo.pass.Count);

  for i := 0 to CTe.Compl.fluxo.pass.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('pass');

    Result[i].AppendChild(AddNode(tcStr, '#066', 'xPass', 1, 15, 1,
                                     CTe.Compl.fluxo.pass[i].xPass, DSC_XPASS));
  end;

  if CTe.Compl.fluxo.pass.Count > 990 then
    wAlerta('#065', 'pass', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_Entrega: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('Entrega');

  case CTe.Compl.Entrega.TipoData of
    tdSemData:
      Result.AppendChild(Gerar_Entrega_SemData);

    tdNaData, tdAteData, tdApartirData:
      Result.AppendChild(Gerar_Entrega_ComData);

    tdNoPeriodo:
      Result.AppendChild(Gerar_Entrega_noPeriodo);
  end;

  case CTe.Compl.Entrega.TipoHora of
    thSemHorario:
      Result.AppendChild(Gerar_Entrega_SemHora);

    thNoHorario, thAteHorario, thApartirHorario:
      Result.AppendChild(Gerar_Entrega_ComHora);

    thNoIntervalo:
      Result.AppendChild(Gerar_Entrega_noIntervalo);
  end;
end;

function TCTeXmlWriter.Gerar_Entrega_SemData: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('semData');

  Result.AppendChild(AddNode(tcStr, '#071', 'tpPer', 1, 1, 1,
               TpDataPeriodoToStr(CTe.Compl.Entrega.semData.tpPer), DSC_TPPER));
end;

function TCTeXmlWriter.Gerar_Entrega_ComData: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('comData');

  Result.AppendChild(AddNode(tcStr, '#073', 'tpPer', 1, 1, 1,
               TpDataPeriodoToStr(CTe.Compl.Entrega.comData.tpPer), DSC_TPPER));

  Result.AppendChild(AddNode(tcDat, '#074', 'dProg', 10, 10, 1,
                                   CTe.Compl.Entrega.comData.dProg, DSC_DPROG));
end;

function TCTeXmlWriter.Gerar_Entrega_NoPeriodo: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('noPeriodo');

  Result.AppendChild(AddNode(tcStr, '#076', 'tpPer', 1, 1, 1,
             TpDataPeriodoToStr(CTe.Compl.Entrega.noPeriodo.tpPer), DSC_TPPER));

  Result.AppendChild(AddNode(tcDat, '#077', 'dIni', 10, 10, 1,
                                   CTe.Compl.Entrega.noPeriodo.dIni, DSC_DINI));

  Result.AppendChild(AddNode(tcDat, '#078', 'dFim', 10, 10, 1,
                                   CTe.Compl.Entrega.noPeriodo.dFim, DSC_DFIM));
end;

function TCTeXmlWriter.Gerar_Entrega_SemHora: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('semHora');

  Result.AppendChild(AddNode(tcStr, '#080', 'tpHor', 1, 1, 1,
          TpHorarioIntervaloToStr(CTe.Compl.Entrega.semHora.tpHor), DSC_TPHOR));
end;

function TCTeXmlWriter.Gerar_Entrega_ComHora: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('comHora');

  Result.AppendChild(AddNode(tcStr, '#082', 'tpHor', 1, 1, 1,
          TpHorarioIntervaloToStr(CTe.Compl.Entrega.comHora.tpHor), DSC_TPHOR));

  Result.AppendChild(AddNode(tcStr, '#083', 'hProg', 8, 8, 1,
                        TimeToStr(CTe.Compl.Entrega.comHora.hProg), DSC_HPROG));
end;

function TCTeXmlWriter.Gerar_Entrega_NoIntervalo: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('noInter');

  Result.AppendChild(AddNode(tcStr, '#085', 'tpHor', 1, 1, 1,
          TpHorarioIntervaloToStr(CTe.Compl.Entrega.noInter.tpHor), DSC_TPHOR));

  Result.AppendChild(AddNode(tcStr, '#086', 'hIni', 8, 8, 1,
                          TimeToStr(CTe.Compl.Entrega.noInter.hIni), DSC_HINI));

  Result.AppendChild(AddNode(tcStr, '#087', 'hFim', 8, 8, 1,
                          TimeToStr(CTe.Compl.Entrega.noInter.hFim), DSC_HFIM));
end;

function TCTeXmlWriter.Gerar_Tomador: TACBrXmlNode;
begin
  Result := nil;

  if (trim(CTe.toma.IE) <> '') or (trim(CTe.toma.xNome) <> '') then
  begin
    Result := FDocument.CreateElement('toma');

    if ModeloDF = moCTeSimp then
    begin
      Result.AppendChild(AddNode(tcStr, '#036', 'toma', 1, 1, 1,
                                      TpTomadorToStr(CTe.Toma.Toma), DSC_TOMA));

      Result.AppendChild(AddNode(tcStr, '#037', 'indIEToma', 1, 1, 1,
                            indIEDestToStr(CTe.Toma.indIEToma), DSC_INDIETOMA));
    end;

    if CTe.toma.EnderToma.cPais = 0 then
      CTe.toma.EnderToma.cPais := 1058;

    if CTe.Toma.EnderToma.cPais = 1058 then
      Result.AppendChild(AddNodeCNPJCPF('#064', '#065', CTe.Toma.CNPJCPF))
    else
      Result.AppendChild(AddNodeCNPJ('#064', '00000000000000', CODIGO_BRASIL, True));

    Result.AppendChild(Gerar_IE(CTe.Toma.IE, CTe.Toma.EnderToma.UF));

    if (CTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) and
       (CTe.Ide.tpAmb = TpcnTipoAmbiente(taHomologacao)) then
      Result.AppendChild(AddNode(tcStr, '#068', 'xNome', 2, 60, 1,
                                                            xRazao4, DSC_XNOME))
    else
      Result.AppendChild(AddNode(tcStr, '#068', 'xNome', 2, 60, 1,
                                                    CTe.Toma.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, '#069', 'xFant', 2, 60, 0,
                                                    CTe.Toma.xFant, DSC_XFANT));

    Result.AppendChild(AddNode(tcStr, '#070', 'fone', 7, 12, 0,
                                          OnlyNumber(CTe.Toma.fone), DSC_FONE));

    Result.AppendChild(Gerar_Endereco('enderToma', CTe.Toma.EnderToma));

    Result.AppendChild(AddNode(tcStr, '#082', 'email', 1, 60, 0,
                                                    CTe.Toma.email, DSC_EMAIL));
  end;
end;

function TCTeXmlWriter.Gerar_vPrest: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('vPrest');

  Result.AppendChild(AddNode(tcDe2, '#209', 'vTPrest', 1, 15, 1,
                                              CTe.vPrest.vTPrest, DSC_VTPREST));

  Result.AppendChild(AddNode(tcDe2, '#210', 'vRec', 1, 15, 1,
                                                    CTe.vPrest.vRec, DSC_VREC));

  nodeArray := Gerar_Comp(CTe.vPrest.comp);

  for i := 0 to CTe.vPrest.comp.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TCTeXmlWriter.Gerar_Comp(Comp: TCompCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, comp.Count);

  for i := 0 to comp.Count - 1 do
  begin
    if (trim(comp[i].xNome) <> '') and (comp[i].vComp <> 0) then
    begin
      Result[i] := FDocument.CreateElement('Comp');

      Result[i].AppendChild(AddNode(tcStr, '#212', 'xNome', 1, 15, 1,
                                                    comp[i].xNome, DSC_XNOMEC));

      Result[i].AppendChild(AddNode(tcDe2, '#213', 'vComp', 1, 15, 1,
                                                     comp[i].vComp, DSC_VCOMP));
    end;
  end;

  if comp.Count > 990 then
    wAlerta('#211', 'Comp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_Imp: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('imp');

  Result.AppendChild(Gerar_ICMS);

  Result.AppendChild(AddNode(tcDe2, '#250', 'vTotTrib', 1, 15, 0,
                                                  CTe.Imp.vTotTrib, DSC_VCOMP));

  Result.AppendChild(AddNode(tcStr, '#251', 'infAdFisco', 1, 2000, 0,
                                           CTe.Imp.InfAdFisco, DSC_INFADFISCO));

  Result.AppendChild(Gerar_ICMSUFFim);

  if (VersaoDF >= ve300) and (ModeloDF = moCTeOS) then
    Result.AppendChild(Gerar_infTribFed);

  // Reforma Tributária
  if (VersaoDF >= ve400) then
    Result.AppendChild(Gerar_IBSCBS(CTe.imp.IBSCBS));

  if (VersaoDF >= ve400) and (ModeloDF in [moCTe, moCTeOS]) then
    Result.AppendChild(AddNode(tcDe2, '#250', 'vTotDFe', 1, 15, 0,
                                                 CTe.Imp.vTotDFe, DSC_VTOTDFE));
end;

function TCTeXmlWriter.Gerar_ICMS: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS');

  case CTe.Imp.ICMS.SituTrib of
    cst00: Result.AppendChild(Gerar_CST00);
    cst20: Result.AppendChild(Gerar_CST20);
    cst40,
    cst41,
    cst51: Result.AppendChild(Gerar_CST45);
    cst60: Result.AppendChild(Gerar_CST60);
    cst90: Result.AppendChild(Gerar_CST90);
    cstICMSOutraUF: Result.AppendChild(Gerar_ICMSOutraUF);
    cstICMSSN: Result.AppendChild(Gerar_ICMSSN);
  end;
end;

function TCTeXmlWriter.Gerar_CST00: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS00');

  Result.AppendChild(AddNode(tcStr, '#217', 'CST', 2, 2, 1,
                               CSTICMSTOStr(CTe.Imp.ICMS.ICMS00.CST), DSC_CST));

  Result.AppendChild(AddNode(tcDe2, '#218', 'vBC', 1, 15, 1,
                                             CTe.Imp.ICMS.ICMS00.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#219', 'pICMS', 1, 5, 1,
                                         CTe.Imp.ICMS.ICMS00.pICMS, DSC_PICMS));

  Result.AppendChild(AddNode(tcDe2, '#220', 'vICMS', 1, 15, 1,
                                         CTe.Imp.ICMS.ICMS00.vICMS, DSC_VICMS));
end;

function TCTeXmlWriter.Gerar_CST20: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS20');

  Result.AppendChild(AddNode(tcStr, '#217', 'CST', 2, 2, 1,
                               CSTICMSTOStr(CTe.Imp.ICMS.ICMS20.CST), DSC_CST));

  Result.AppendChild(AddNode(tcDe2, '#223', 'pRedBC', 1, 5, 1,
                                       CTe.Imp.ICMS.ICMS20.pRedBC, DSC_PREDBC));

  Result.AppendChild(AddNode(tcDe2, '#224', 'vBC', 1, 15, 1,
                                             CTe.Imp.ICMS.ICMS20.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#225', 'pICMS', 1, 5, 1,
                                         CTe.Imp.ICMS.ICMS20.pICMS, DSC_PICMS));

  Result.AppendChild(AddNode(tcDe2, '#226', 'vICMS', 1, 15, 1,
                                         CTe.Imp.ICMS.ICMS20.vICMS, DSC_VICMS));

  if CTe.Imp.ICMS.ICMS20.vICMSDeson > 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#227', 'vICMSDeson', 1, 15, 1,
                               CTe.Imp.ICMS.ICMS20.vICMSDeson, DSC_VICMSDESON));

    Result.AppendChild(AddNode(tcStr, '#228', 'cBenef', 8, 10, 1,
                                       CTe.Imp.ICMS.ICMS20.cBenef, DSC_CBENEF));
  end;
end;

function TCTeXmlWriter.Gerar_CST45: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS45');

  Result.AppendChild(AddNode(tcStr, '#228', 'CST', 2, 2, 1,
                               CSTICMSTOStr(CTe.Imp.ICMS.ICMS45.CST), DSC_CST));

  if CTe.Imp.ICMS.ICMS45.vICMSDeson > 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#227', 'vICMSDeson', 1, 15, 1,
                               CTe.Imp.ICMS.ICMS45.vICMSDeson, DSC_VICMSDESON));

    Result.AppendChild(AddNode(tcStr, '#228', 'cBenef', 8, 10, 1,
                                       CTe.Imp.ICMS.ICMS45.cBenef, DSC_CBENEF));
  end;
end;

function TCTeXmlWriter.Gerar_CST60: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS60');

  Result.AppendChild(AddNode(tcStr, '#230', 'CST', 2, 2, 1,
                               CSTICMSTOStr(CTe.Imp.ICMS.ICMS60.CST), DSC_CST));

  Result.AppendChild(AddNode(tcDe2, '#231', 'vBCSTRet', 1, 15, 1,
                                        CTe.Imp.ICMS.ICMS60.vBCSTRet, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#232', 'vICMSSTRet', 1, 15, 1,
                                    CTe.Imp.ICMS.ICMS60.vICMSSTRet, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, '#233', 'pICMSSTRet', 1, 5, 1,
                                    CTe.Imp.ICMS.ICMS60.pICMSSTRet, DSC_PICMS));

  Result.AppendChild(AddNode(tcDe2, '#234', 'vCred', 1, 15, 0,
                                         CTe.Imp.ICMS.ICMS60.vCred, DSC_VCRED));

  if CTe.Imp.ICMS.ICMS60.vICMSDeson > 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#227', 'vICMSDeson', 1, 15, 1,
                               CTe.Imp.ICMS.ICMS60.vICMSDeson, DSC_VICMSDESON));

    Result.AppendChild(AddNode(tcStr, '#228', 'cBenef', 8, 10, 1,
                                       CTe.Imp.ICMS.ICMS60.cBenef, DSC_CBENEF));
  end;
end;

function TCTeXmlWriter.Gerar_CST90: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS90');

  Result.AppendChild(AddNode(tcStr, '#236', 'CST', 2, 2, 1,
                               CSTICMSTOStr(CTe.Imp.ICMS.ICMS90.CST), DSC_CST));

  Result.AppendChild(AddNode(tcDe2, '#237', 'pRedBC', 1, 5, 0,
                                       CTe.Imp.ICMS.ICMS90.pRedBC, DSC_PREDBC));

  Result.AppendChild(AddNode(tcDe2, '#238', 'vBC', 1, 15, 1,
                                             CTe.Imp.ICMS.ICMS90.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#239', 'pICMS', 1, 5, 1,
                                         CTe.Imp.ICMS.ICMS90.pICMS, DSC_PICMS));

  Result.AppendChild(AddNode(tcDe2, '#240', 'vICMS', 1, 15, 1,
                                         CTe.Imp.ICMS.ICMS90.vICMS, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, '#241', 'vCred', 1, 15, 0,
                                         CTe.Imp.ICMS.ICMS90.vCred, DSC_VCRED));

  if CTe.Imp.ICMS.ICMS90.vICMSDeson > 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#227', 'vICMSDeson', 1, 15, 1,
                               CTe.Imp.ICMS.ICMS90.vICMSDeson, DSC_VICMSDESON));

    Result.AppendChild(AddNode(tcStr, '#228', 'cBenef', 8, 10, 1,
                                       CTe.Imp.ICMS.ICMS90.cBenef, DSC_CBENEF));
  end;
end;

function TCTeXmlWriter.Gerar_ICMSOutraUF: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMSOutraUF');

  Result.AppendChild(AddNode(tcStr, '#243', 'CST', 2, 2, 1,
                          CSTICMSTOStr(CTe.Imp.ICMS.ICMSOutraUF.CST), DSC_CST));

  Result.AppendChild(AddNode(tcDe2, '#244', 'pRedBCOutraUF', 1, 5, 0,
                           CTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF, DSC_PREDBC));

  Result.AppendChild(AddNode(tcDe2, '#245', 'vBCOutraUF', 1, 15, 1,
                                 CTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#246', 'pICMSOutraUF', 1, 5, 1,
                             CTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF, DSC_PICMS));

  Result.AppendChild(AddNode(tcDe2, '#247', 'vICMSOutraUF', 1, 15, 1,
                             CTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF, DSC_VICMS));

  if CTe.Imp.ICMS.ICMSOutraUF.vICMSDeson > 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#227', 'vICMSDeson', 1, 15, 1,
                          CTe.Imp.ICMS.ICMSOutraUF.vICMSDeson, DSC_VICMSDESON));

    Result.AppendChild(AddNode(tcStr, '#228', 'cBenef', 8, 10, 1,
                                  CTe.Imp.ICMS.ICMSOutraUF.cBenef, DSC_CBENEF));
  end;
end;

function TCTeXmlWriter.Gerar_ICMSSN: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMSSN');

  if (VersaoDF >= ve300) then
    Result.AppendChild(AddNode(tcStr, '#248', 'CST', 2, 2, 1, '90', DSC_CST));

  Result.AppendChild(AddNode(tcInt, '#249', 'indSN', 1, 1, 1,
                                         CTe.Imp.ICMS.ICMSSN.indSN, DSC_INDSN));
end;

function TCTeXmlWriter.Gerar_ICMSUFFim: TACBrXmlNode;
begin
  Result := nil;

  // Grupo a ser informado nas prestações de serviços de transporte interestaduais
  // para consumidor final, não contribuinte do ICMS.

  if ((CTe.Imp.ICMSUFFim.vBCUFFim <> 0) or (CTe.Imp.ICMSUFFim.pFCPUFFim <> 0) or
     (CTe.Imp.ICMSUFFim.pICMSUFFim <> 0) or (CTe.Imp.ICMSUFFim.vFCPUFFim <> 0) or
	 (CTe.Imp.ICMSUFFim.vICMSUFFim <> 0) or (CTe.Imp.ICMSUFFim.vICMSUFIni <> 0)) or
     ((CTe.ide.UFIni <> CTe.ide.UFFim) and (CTe.ide.indIEToma = inNaoContribuinte)) then
  begin
    Result := FDocument.CreateElement('ICMSUFFim');

    Result.AppendChild(AddNode(tcDe2, '#244', 'vBCUFFim', 1, 15, 1,
                                          CTe.Imp.ICMSUFFim.vBCUFFim, DSC_VBC));

    Result.AppendChild(AddNode(tcDe2, '#245', 'pFCPUFFim', 1, 5, 1,
                                       CTe.Imp.ICMSUFFim.pFCPUFFim, DSC_PICMS));

    Result.AppendChild(AddNode(tcDe2, '#246', 'pICMSUFFim', 1, 5, 1,
                                      CTe.Imp.ICMSUFFim.pICMSUFFim, DSC_PICMS));

    Result.AppendChild(AddNode(tcDe2, '#247', 'pICMSInter', 1, 5, 1,
                                      CTe.Imp.ICMSUFFim.pICMSInter, DSC_PICMS));

    Result.AppendChild(AddNode(tcDe2, '#247', 'vFCPUFFim', 1, 15, 1,
                                       CTe.Imp.ICMSUFFim.vFCPUFFim, DSC_VICMS));

    Result.AppendChild(AddNode(tcDe2, '#247', 'vICMSUFFim', 1, 15, 1,
                                      CTe.Imp.ICMSUFFim.vICMSUFFim, DSC_VICMS));

    Result.AppendChild(AddNode(tcDe2, '#247', 'vICMSUFIni', 1, 15, 1,
                                      CTe.Imp.ICMSUFFim.vICMSUFIni, DSC_VICMS));
  end;
end;

function TCTeXmlWriter.Gerar_infTribFed: TACBrXmlNode;
  function InformarINSS: Integer;
  begin
    if ((Length(OnlyNumber(Trim(CTe.toma.CNPJCPF))) = 14) or
        (CTe.toma.EnderToma.cPais <> 1058)) and
       (CTe.Ide.tpServ in [tsTranspPessoas, tsExcessoBagagem]) then
      Result := 1
    else
      Result := 0;
  end;
begin
  Result := nil;

  if ((CTe.Imp.infTribFed.vPIS > 0) or (CTe.Imp.infTribFed.vCOFINS > 0) or
      (CTe.Imp.infTribFed.vIR > 0) or ((CTe.Imp.infTribFed.vINSS > 0) or
      (InformarINSS = 1)) or (CTe.Imp.infTribFed.vCSLL > 0)) then
  begin
    Result := FDocument.CreateElement('infTribFed');

    Result.AppendChild(AddNode(tcDe2, '#244', 'vPIS', 1, 15, 0,
                                            CTe.Imp.infTribFed.vPIS, DSC_VPIS));

    Result.AppendChild(AddNode(tcDe2, '#244', 'vCOFINS', 1, 15, 0,
                                      CTe.Imp.infTribFed.vCOFINS, DSC_VCOFINS));

    Result.AppendChild(AddNode(tcDe2, '#244', 'vIR', 1, 15, 0,
                                              CTe.Imp.infTribFed.vIR, DSC_VIR));

    Result.AppendChild(AddNode(tcDe2, '#244', 'vINSS', 1, 15, InformarINSS,
                                          CTe.Imp.infTribFed.vINSS, DSC_VINSS));

    Result.AppendChild(AddNode(tcDe2, '#244', 'vCSLL', 1, 15, 0,
                                          CTe.Imp.infTribFed.vCSLL, DSC_VCSLL));
  end;
end;

function TCTeXmlWriter.Gerar_InfCTeNorm: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if (CTe.Ide.tpCTe = tcNormal) or (CTe.Ide.tpCTe = tcSubstituto) then
  begin
    Result := FDocument.CreateElement('infCTeNorm');

    if (VersaoDF >= ve300) and (ModeloDF = moCTeOS) then
    begin
      Result.AppendChild(Gerar_InfServico);

      nodeArray := Gerar_infDocRef;

      for i := 0 to CTe.infCTeNorm.infDocRef.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;

      nodeArray := Gerar_InfSeg;

      for i := 0 to CTe.infCTeNorm.seg.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end
    else
    begin
      Result.AppendChild(Gerar_InfCarga(CTe.infCTeNorm.InfCarga));

      if (CTe.Ide.tpServ <> tsIntermediario) and (CTe.Ide.tpServ <> tsMultimodal) then
        Result.AppendChild(Gerar_InfDoc);

      if CTe.infCTeNorm.docAnt.emiDocAnt.Count > 0 then
        Result.AppendChild(Gerar_DocAnt);

      if VersaoDF = ve200 then
      begin
        nodeArray := Gerar_InfSeg;

        for i := 0 to CTe.infCTeNorm.seg.Count - 1 do
        begin
          Result.AppendChild(nodeArray[i]);
        end;
      end;
    end;

    if (ModeloDF = moCTe) or ((ModeloDF = moCTeOS) and
       (CTe.ide.modal = mdRodoviario) and (CTe.ide.tpServ <> tsTranspValores)) then
      Result.AppendChild(Gerar_InfModal);

    if (ModeloDF = moCTe) then
    begin
      if VersaoDF = ve200 then
      begin
        nodeArray := Gerar_Peri(CTe.infCTeNorm.peri);

        for i := 0 to CTe.infCTeNorm.peri.Count - 1 do
        begin
          Result.AppendChild(nodeArray[i]);
        end;
      end;

      nodeArray := Gerar_VeicNovos;

      for i := 0 to CTe.infCTeNorm.veicNovos.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;

      Result.AppendChild(Gerar_Cobr(CTe.infCTeNorm.cobr));

      Result.AppendChild(Gerar_InfCTeSub(CTe.infCTeNorm.infCTeSub));

      if (VersaoDF >= ve300) and (Trim(CTe.infCTeNorm.infGlobalizado.xObs) <> '') then
        Result.AppendChild(Gerar_InfGlobalizado);

      if VersaoDF >= ve300 then
        Result.AppendChild(Gerar_InfServVinc);
    end;

    if ModeloDF = moCTeOS then
    begin
      Result.AppendChild(Gerar_InfCTeSub(CTe.infCTeNorm.infCTeSub));

      if CTe.Ide.tpServ = tsTranspValores then
      begin
        Result.AppendChild(AddNode(tcStr, '#133', 'refCTeCanc', 44, 44, 0,
                             OnlyNumber(CTe.infCTeNorm.refCTeCanc), DSC_CHAVE));

        if OnlyNumber(CTe.infCTeNorm.refCTeCanc) <> '' then
          if not ValidarChave(CTe.infCTeNorm.refCTeCanc) then
            wAlerta('#', 'refCTeCanc', DSC_REFNFE, ERR_MSG_INVALIDO);
      end;

      Result.AppendChild(Gerar_Cobr(CTe.infCTeNorm.cobr));

      nodeArray := Gerar_InfGTVe;

      for i := 0 to CTe.infCTeNorm.infGTVe.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end;
end;

function TCTeXmlWriter.Gerar_InfServico: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infServico');

  Result.AppendChild(AddNode(tcStr, '#133', 'xDescServ', 1, 30, 1,
                           CTe.infCTeNorm.infServico.xDescServ, DSC_XDESCSERV));

  if CTe.infCTeNorm.infServico.qCarga > 0 then
    Result.AppendChild(Gerar_InfQ);
end;

function TCTeXmlWriter.Gerar_InfQ: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infQ');

  Result.AppendChild(AddNode(tcDe4, '#135', 'qCarga', 1, 15, 1,
                                 CTe.infCTeNorm.infServico.qCarga, DSC_QCARGA));
end;

function TCTeXmlWriter.Gerar_InfDocRef: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.infDocRef.Count);

  for i := 0 to CTe.infCTeNorm.infDocRef.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infDocRef');

    if CTe.infCTeNorm.infDocRef[i].chBPe = '' then
    begin
      Result[i].AppendChild(AddNode(tcStr, '#137', 'nDoc', 1, 20, 1,
                       OnlyNumber(CTe.infCTeNorm.infDocRef[i].nDoc), DSC_NDOC));

      Result[i].AppendChild(AddNode(tcStr, '#138', 'serie', 1, 3, 0,
                                 CTe.infCTeNorm.infDocRef[i].serie, DSC_SERIE));

      Result[i].AppendChild(AddNode(tcStr, '#139', 'subserie', 1, 3, 0,
                              CTe.infCTeNorm.infDocRef[i].subserie, DSC_SERIE));

      Result[i].AppendChild(AddNode(tcDat, '#140', 'dEmi', 10, 10, 1,
                                   CTe.infCTeNorm.infDocRef[i].dEmi, DSC_DEMI));

      Result[i].AppendChild(AddNode(tcDe2, '#141', 'vDoc', 1, 15, 0,
                                    CTe.infCTeNorm.infDocRef[i].vDoc, DSC_VNF));
    end
    else
      Result[i].AppendChild(AddNode(tcStr, '#137', 'chBPe', 44, 44, 1,
                     OnlyNumber(CTe.infCTeNorm.infDocRef[i].chBPe), DSC_CHBPE));

    if OnlyNumber(CTe.infCTeNorm.infDocRef[i].chBPe) <> '' then
      if not ValidarChave(CTe.infCTeNorm.infDocRef[i].chBPe) then
        wAlerta('#137', 'chBPe', DSC_CHBPE, ERR_MSG_INVALIDO);
  end;

  if CTe.infCTeNorm.infDocRef.Count > 990 then
    wAlerta('#136', 'infDocRef', DSC_INFQ, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfSeg: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.seg.Count);

  for i := 0 to CTe.infCTeNorm.seg.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('seg');

    Result[i].AppendChild(AddNode(tcStr, '#361', 'respSeg', 1, 1, 1,
                 TpRspSeguroToStr(CTe.infCTeNorm.seg[i].respSeg), DSC_RESPSEG));

    Result[i].AppendChild(AddNode(tcStr, '#362', 'xSeg', 1, 30, 0,
                                         CTe.infCTeNorm.seg[i].xSeg, DSC_XSEG));

    Result[i].AppendChild(AddNode(tcStr, '#363', 'nApol', 1, 20, 0,
                                       CTe.infCTeNorm.seg[i].nApol, DSC_NAPOL));

    if ModeloDF = moCTe then
    begin
      Result[i].AppendChild(AddNode(tcStr, '#364', 'nAver', 1, 20, 0,
                                       CTe.infCTeNorm.seg[i].nAver, DSC_NAVER));

      Result[i].AppendChild(AddNode(tcDe2, '#365', 'vCarga', 1, 15, 0,
                                      CTe.infCTeNorm.seg[i].vCarga, DSC_VMERC));
    end;
  end;

  if CTe.infCTeNorm.seg.Count > 990 then
    wAlerta('#360', 'seg', DSC_INFSEG, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfDoc: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('infDoc');

  nodeArray := Gerar_InfNF;

  for i := 0 to CTe.infCTeNorm.infDoc.infNF.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_InfNFe(CTe.infCTeNorm.infDoc.InfNFe);

  for i := 0 to CTe.infCTeNorm.infDoc.infNFe.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_InfOutros;

  for i := 0 to CTe.infCTeNorm.infDoc.infOutros.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_InfDCe(CTe.infCTeNorm.infDoc.InfDCe);

  for i := 0 to CTe.infCTeNorm.infDoc.infDCe.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TCTeXmlWriter.Gerar_InfNF: TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.infDoc.infNF.Count);

  for i := 0 to CTe.infCTeNorm.infDoc.infNF.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNF');

    Result[i].AppendChild(AddNode(tcStr, '#263', 'nRoma', 1, 20, 0,
                              CTe.infCTeNorm.infDoc.InfNF[i].nRoma, DSC_NROMA));
                                       
    Result[i].AppendChild(AddNode(tcStr, '#264', 'nPed', 1, 20, 0,
                                CTe.infCTeNorm.infDoc.InfNF[i].nPed, DSC_NPED));
                                       
    Result[i].AppendChild(AddNode(tcStr, '#265', 'mod', 2, 2, 1,
              ModeloNFToStrEX(CTe.infCTeNorm.infDoc.InfNF[i].modelo), DSC_MOD));

    Result[i].AppendChild(AddNode(tcStr, '#266', 'serie', 1, 3, 1,
                              CTe.infCTeNorm.infDoc.InfNF[i].serie, DSC_SERIE));
                              
    Result[i].AppendChild(AddNode(tcStr, '#267', 'nDoc', 1, 20, 1,
                    OnlyNumber(CTe.infCTeNorm.infDoc.InfNF[i].nDoc), DSC_NDOC));
                    
    Result[i].AppendChild(AddNode(tcDat, '#268', 'dEmi', 10, 10, 1,
                                CTe.infCTeNorm.infDoc.InfNF[i].dEmi, DSC_DEMI));
                                
    Result[i].AppendChild(AddNode(tcDe2, '#269', 'vBC', 1, 15, 1,
                                  CTe.infCTeNorm.infDoc.InfNF[i].vBC, DSC_VBC));
                                
    Result[i].AppendChild(AddNode(tcDe2, '#270', 'vICMS', 1, 15, 1,
                              CTe.infCTeNorm.infDoc.InfNF[i].vICMS, DSC_VICMS));

    Result[i].AppendChild(AddNode(tcDe2, '#271', 'vBCST', 1, 15, 1,
                              CTe.infCTeNorm.infDoc.InfNF[i].vBCST, DSC_VBCST));

    Result[i].AppendChild(AddNode(tcDe2, '#272', 'vST', 1, 15, 1,
                                  CTe.infCTeNorm.infDoc.InfNF[i].vST, DSC_VST));

    Result[i].AppendChild(AddNode(tcDe2, '#273', 'vProd', 1, 15, 1,
                              CTe.infCTeNorm.infDoc.InfNF[i].vProd, DSC_VPROD));

    Result[i].AppendChild(AddNode(tcDe2, '#274', 'vNF', 1, 15, 1,
                                  CTe.infCTeNorm.infDoc.InfNF[i].vNF, DSC_VNF));

    Result[i].AppendChild(AddNode(tcInt, '#275', 'nCFOP', 4, 4, 1,
                               CTe.infCTeNorm.infDoc.InfNF[i].nCFOP, DSC_CFOP));

    Result[i].AppendChild(AddNode(tcDe3, '#276', 'nPeso', 1, 15, 0,
                               CTe.infCTeNorm.infDoc.InfNF[i].nPeso, DSC_PESO));

    Result[i].AppendChild(AddNode(tcStr, '#277', 'PIN', 2, 9, 0,
                                 CTe.infCTeNorm.infDoc.InfNF[i].PIN, DSC_ISUF));

    if (Opcoes.ValidarInscricoes) and (trim(CTe.infCTeNorm.infDoc.InfNF[i].PIN) <> '') then
      if not ValidarISUF(CTe.infCTeNorm.infDoc.InfNF[i].PIN) then
        wAlerta('#277', 'PIN', DSC_ISUF, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcDat, '#278', 'dPrev', 10, 10, 0,
                              CTe.infCTeNorm.infDoc.InfNF[i].dPrev, DSC_DPREV));

    nodeArray := Gerar_InfUnidCarga(CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga);

    for j := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_InfUnidTransp(CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp);

    for j := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if CTe.infCTeNorm.infDoc.InfNF.Count > 990 then
    wAlerta('#262', 'infNF', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfUnidCarga(
  infUnidCarga: TinfUnidCargaCollection): TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, infUnidCarga.Count);

  for i := 0 to infUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#292', 'tpUnidCarga', 1, 1, 1,
                 UnidCargaToStr(infUnidCarga[i].tpUnidCarga), DSC_TPUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#293', 'idUnidCarga', 1, 20, 1,
                                 infUnidCarga[i].idUnidCarga, DSC_IDUNIDCARGA));

    nodeArray := Gerar_LacUnidCarga(infUnidCarga[i].lacUnidCarga);

    for j := 0 to infUnidCarga[i].lacUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(DefineArredondamentoQtdRat, '#296', 'qtdRat', 1, 5, 0,
                                           infUnidCarga[i].qtdRat, DSC_QTDRAT));
  end;
end;

function TCTeXmlWriter.Gerar_LacUnidCarga(
  LacUnidCarga: TLacUnidCargaCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, LacUnidCarga.Count);

  for i := 0 to lacUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#295', 'nLacre', 1, 20, 1,
                                           lacUnidCarga[i].nLacre, DSC_NLACRE));
  end;
end;

function TCTeXmlWriter.Gerar_InfUnidTransp(
  infUnidTransp: TinfUnidTranspCollection): TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, infUnidTransp.Count);

  for i := 0 to infUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#280', 'tpUnidTransp', 1, 1, 1,
             UnidTranspToStr(infUnidTransp[i].tpUnidTransp), DSC_TPUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#281', 'idUnidTransp', 1, 20, 1,
                              infUnidTransp[i].idUnidTransp, DSC_IDUNIDTRANSP));

    nodeArray := Gerar_LacUnidTransp(infUnidTransp[i].lacUnidTransp);

    for j := 0 to infUnidTransp[i].lacUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_InfUnidCarga(infUnidTransp[i].infUnidCarga);

    for j := 0 to infUnidTransp[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(DefineArredondamentoQtdRat, '#296', 'qtdRat', 1, 5, 0,
                                           infUnidTransp[i].qtdRat, DSC_QTDRAT));
  end;
end;

function TCTeXmlWriter.Gerar_LacUnidTransp(
  LacUnidTransp: TlacUnidTranspCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, LacUnidTransp.Count);

  for i := 0 to LacUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#283', 'nLacre', 1, 20, 1,
                                           LacUnidTransp[i].nLacre, DSC_NLACRE));
  end;
end;

function TCTeXmlWriter.Gerar_InfNFe(
  infNFe: TInfNFeCollection): TACBrXmlNodeArray;
var
  i, j: Integer;
  xTagChave: string;
  nodeArray: TACBrXmlNodeArray;
  chave: string;
begin
  Result := nil;
  SetLength(Result, infNFe.Count);

  xTagChave := 'chave';

  if ModeloDF = moCTeSimp then
    xTagChave := 'chNFe';

  for i := 0 to InfNFe.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNFe');

    chave := OnlyNumber(InfNFe[i].chave);

    Result[i].AppendChild(AddNode(tcStr, '#298', xTagChave, 44, 44, 1,
                                                            chave, DSC_REFNFE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#298', xTagChave, DSC_REFNFE, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#299', 'PIN', 02, 09, 0,
                                                      InfNFe[i].PIN, DSC_ISUF));

    if (Opcoes.ValidarInscricoes) and (trim(InfNFe[i].PIN) <> '') then
      if not ValidarISUF(InfNFe[i].PIN) then
        wAlerta('#299', 'PIN', DSC_ISUF, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcDat, '#300', 'dPrev', 10, 10, 0,
                                                   InfNFe[i].dPrev, DSC_DPREV));

    nodeArray := Gerar_InfUnidCarga(infNFe[i].infUnidCarga);

    for j := 0 to infNFe[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_InfUnidTransp(infNFe[i].infUnidTransp);

    for j := 0 to infNFe[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if InfNFe.Count > 990 then
    wAlerta('#297', 'infNFe', DSC_INFNFE, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfOutros: TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.infDoc.infOutros.Count);

  for i := 0 to CTe.infCTeNorm.infDoc.InfOutros.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infOutros');

    Result[i].AppendChild(AddNode(tcStr, '#320', 'tpDoc', 2, 2, 1,
        TpDocumentoToStr(CTe.infCTeNorm.infDoc.InfOutros[i].tpDoc), DSC_TPDOC));

    Result[i].AppendChild(AddNode(tcStr, '#321', 'descOutros', 1, 100, 0,
                    CTe.infCTeNorm.infDoc.InfOutros[i].descOutros, DSC_OUTROS));

    Result[i].AppendChild(AddNode(tcStr, '#322', 'nDoc', 1, 20, 0,
                             CTe.infCTeNorm.infDoc.InfOutros[i].nDoc, DSC_NRO));

    Result[i].AppendChild(AddNode(tcDat, '#323', 'dEmi', 10, 10, 0,
                            CTe.infCTeNorm.infDoc.InfOutros[i].dEmi, DSC_DEMI));

    Result[i].AppendChild(AddNode(tcDe2, '#324', 'vDocFisc', 1, 15, 0,
                        CTe.infCTeNorm.infDoc.InfOutros[i].vDocFisc, DSC_VDOC));

    Result[i].AppendChild(AddNode(tcDat, '#325', 'dPrev', 10, 10, 0,
                          CTe.infCTeNorm.infDoc.infOutros[i].dPrev, DSC_DPREV));

    nodeArray := Gerar_InfUnidCarga(CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga);

    for j := 0 to CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_InfUnidTransp(CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp);

    for j := 0 to CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if CTe.infCTeNorm.infDoc.InfOutros.Count > 990 then
    wAlerta('#319', 'infOutros', DSC_INFOUTRO, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfDCe(
  infDCe: TInfDCeCollection): TACBrXmlNodeArray;
var
  i: Integer;
  chave: string;
begin
  Result := nil;
  SetLength(Result, infDCe.Count);

  for i := 0 to InfDCe.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infDCe');

    chave := OnlyNumber(InfDCe[i].chave);

    Result[i].AppendChild(AddNode(tcStr, '#298', 'chave', 44, 44, 1,
                                                            chave, DSC_REFNFE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#298', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);
  end;

  if InfDCe.Count > 990 then
    wAlerta('#297', 'infDCe', DSC_INFNFE, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_DocAnt: TACBrXmlNode;
var
  i: Integer;
  nodeArray : TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('docAnt');

  nodeArray := Gerar_EmiDocAnt(CTe.infCTeNorm.docAnt.emiDocAnt);

  for i := 0 to CTe.infCTeNorm.docAnt.emiDocAnt.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TCTeXmlWriter.Gerar_EmiDocAnt(
  emiDocAnt: TEmiDocAntCollection): TACBrXmlNodeArray;
var
  i, j: Integer;  
  nodeArray : TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, emiDocAnt.Count);

  for i := 0 to emiDocAnt.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('emiDocAnt');
    
    Result[i].AppendChild(AddNodeCNPJCPF('#346', '#347', emiDocAnt[i].CNPJCPF));

    Result[i].AppendChild(Gerar_IE(emiDocAnt[i].IE, emiDocAnt[i].UF));

    Result[i].AppendChild(AddNode(tcStr, '#349', 'UF', 2, 2, 1, 
                                                      emiDocAnt[i].UF, DSC_UF));

    if not ValidarUF(CTe.infCTeNorm.docAnt.emiDocAnt[i].UF) then
      wAlerta('#349', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#350', 'xNome', 2, 60, 1, 
                                                emiDocAnt[i].xNome, DSC_XNOME));

    nodeArray := Gerar_IdDocAnt(emiDocAnt[i].idDocAnt);

    for j := 0 to emiDocAnt[i].idDocAnt.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if CTe.infCTeNorm.docAnt.emiDocAnt.Count > 990 then
    wAlerta('#345', 'emiDocAnt', DSC_EMIDOCANT, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_IdDocAnt(
  idDocAnt: TidDocAntCollection): TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray : TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, idDocAnt.Count);

  for i := 0 to idDocAnt.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('idDocAnt');

    nodeArray := Gerar_IdDocAntPap(idDocAnt[i].idDocAntPap);

    for j := 0 to idDocAnt[i].idDocAntPap.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_IdDocAntEle(idDocAnt[i].idDocAntEle);

    for j := 0 to idDocAnt[i].idDocAntEle.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if idDocAnt.Count > 2 then
    wAlerta('#351', 'idDocAnt', DSC_IDDOCANT, ERR_MSG_MAIOR_MAXIMO + '02');
end;

function TCTeXmlWriter.Gerar_IdDocAntPap(
  idDocAntPap: TidDocAntPapCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, idDocAntPap.Count);

  for i := 0 to idDocAntPap.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('idDocAntPap');

    Result[i].AppendChild(AddNode(tcStr, '#353', 'tpDoc', 2, 2, 1, 
                     TpDocumentoAnteriorToStr(idDocAntPap[i].tpDoc), DSC_TPNF));

    Result[i].AppendChild(AddNode(tcStr, '#354', 'serie', 1, 3, 1, 
                                              idDocAntPap[i].serie, DSC_SERIE));

    Result[i].AppendChild(AddNode(tcStr, '#355', 'subser', 1, 2, 0, 
                                             idDocAntPap[i].subser, DSC_SERIE));

    Result[i].AppendChild(AddNode(tcStr, '#356', 'nDoc', 1, 30, 1, 
                                                 idDocAntPap[i].nDoc, DSC_NDF));

    Result[i].AppendChild(AddNode(tcDat, '#357', 'dEmi', 10, 10, 1, 
                                                idDocAntPap[i].dEmi, DSC_DEMI));
  end;

  if idDocAntPap.Count > 990 then
    wAlerta('#352', 'idDocAntPap', DSC_IDDOCANTPAP, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_IdDocAntEle(
  idDocAntEle: TidDocAntEleCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, idDocAntEle.Count);

  for i := 0 to idDocAntEle.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('idDocAntEle');

    if (VersaoDF >= ve300) then
    begin
      Result[i].AppendChild(AddNode(tcStr, '#359', 'chCTe', 44, 44, 1, 
                                 OnlyNumber(idDocAntEle[i].chCTe), DSC_CHAVE));

      if OnlyNumber(idDocAntEle[i].chCTe) <> '' then
        if not ValidarChave(idDocAntEle[i].chCTe) then
          wAlerta('#359', 'chCTe', DSC_REFCTE, ERR_MSG_INVALIDO);
    end
    else
    begin
      Result[i].AppendChild(AddNode(tcStr, '#359', 'chave', 44, 44, 1, 
                                 OnlyNumber(idDocAntEle[i].chave), DSC_CHAVE));

      if OnlyNumber(idDocAntEle[i].chave) <> '' then
        if not ValidarChave(idDocAntEle[i].chave) then
          wAlerta('#359', 'chave', DSC_REFCTE, ERR_MSG_INVALIDO);
    end;
  end;

  if idDocAntEle.Count > 990 then
    wAlerta('#358', 'idDocAntEle', DSC_IDDOCANTELE, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_Peri(peri: TPeriCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, peri.Count);

  for i := 0 to peri.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('peri');

    if VersaoDF >= ve300 then
    begin
      Result[i].AppendChild(AddNode(tcStr, '#13', 'nONU', 1, 4, 1,
                                                       peri[i].nONU, DSC_NONU));

      Result[i].AppendChild(AddNode(tcStr, '#14', 'qTotEmb', 1, 20, 1,
                                                 peri[i].qTotEmb, DSC_QTOTEMB));

      Result[i].AppendChild(Gerar_infTotAP(i, peri));
    end
    else
    begin
      Result[i].AppendChild(AddNode(tcStr, '#370', 'nONU', 1, 4, 1,
                                                       peri[i].nONU, DSC_NONU));

      Result[i].AppendChild(AddNode(tcStr, '#371', 'xNomeAE', 1, 150, 1,
                                                 peri[i].xNomeAE, DSC_XNOMEAE));

      Result[i].AppendChild(AddNode(tcStr, '#372', 'xClaRisco', 1, 40, 1,
                                             peri[i].xClaRisco, DSC_XCLARISCO));

      Result[i].AppendChild(AddNode(tcStr, '#373', 'grEmb', 1, 6, 0,
                                                     peri[i].grEmb, DSC_GREMB));

      Result[i].AppendChild(AddNode(tcStr, '#374', 'qTotProd', 1, 20, 1,
                                               peri[i].qTotProd, DSC_QTOTPROD));

      Result[i].AppendChild(AddNode(tcStr, '#375', 'qVolTipo', 1, 60, 0,
                                               peri[i].qVolTipo, DSC_QVOLTIPO));

      Result[i].AppendChild(AddNode(tcStr, '#376', 'pontoFulgor', 1, 6, 0,
                                         peri[i].pontoFulgor, DSC_PONTOFULGOR));
    end;
  end;

  if peri.Count > 990 then
    wAlerta('#369', 'peri', DSC_PERI, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_infTotAP(Idx: Integer; peri: TPeriCollection): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infTotAP');

  Result.AppendChild(AddNode(tcStr, '#16', 'qTotProd', 1, 20, 1,
                                             peri[Idx].qTotProd, DSC_QTOTPROD));

  Result.AppendChild(AddNode(tcStr, '#17', 'uniAP', 1, 1, 1,
                                      UniMedToStr(peri[Idx].uniAP), DSC_UNIAP));
end;

function TCTeXmlWriter.Gerar_VeicNovos: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.veicNovos.Count);

  for i := 0 to CTe.infCTeNorm.veicNovos.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('veicNovos');

    Result[i].AppendChild(AddNode(tcStr, '#378', 'chassi', 17, 17, 1,
                               CTe.infCTeNorm.veicNovos[i].chassi, DSC_CHASSI));
                               
    Result[i].AppendChild(AddNode(tcStr, '#379', 'cCor', 1, 4, 1,
                                   CTe.infCTeNorm.veicNovos[i].cCor, DSC_CCOR));
                               
    Result[i].AppendChild(AddNode(tcStr, '#380', 'xCor', 1, 40, 1,
                                   CTe.infCTeNorm.veicNovos[i].xCor, DSC_XCOR));
                               
    Result[i].AppendChild(AddNode(tcStr, '#381', 'cMod', 1, 6, 1,
                                   CTe.infCTeNorm.veicNovos[i].cMod, DSC_CMOD));
                               
    Result[i].AppendChild(AddNode(tcDe2, '#382', 'vUnit', 1, 15, 1,
                                CTe.infCTeNorm.veicNovos[i].vUnit, DSC_VUNITV));
                               
    Result[i].AppendChild(AddNode(tcDe2, '#383', 'vFrete', 1, 15, 1,
                              CTe.infCTeNorm.veicNovos[i].vFrete, DSC_VFRETEV));
  end;

  if CTe.infCTeNorm.veicNovos.Count > 990 then
    wAlerta('#377', 'veicNovos', DSC_VEICNOVOS, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfGlobalizado: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infGlobalizado');

  Result.AppendChild(AddNode(tcStr, '#387', 'xObs', 15, 256, 1,
                                 CTe.infCTeNorm.infGlobalizado.xObs, DSC_XOBS));
end;

function TCTeXmlWriter.Gerar_InfServVinc: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count > 0 then
  begin
    Result := FDocument.CreateElement('infServVinc');

    nodeArray := Gerar_InfCTeMultimodal;

    for i := 0 to CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_InfCTeMultimodal: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count);

  for i := 0 to CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infCTeMultimodal');

    Result[i].AppendChild(AddNode(tcStr, '#390', 'chCTeMultimodal', 44, 44, 1,
      CTe.infCTeNorm.infServVinc.infCTeMultimodal[i].chCTeMultimodal, DSC_CHCTEMULTIMODAL));
  end;

  if CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count > 999 then
    wAlerta('#389', 'infCTeMultimodal', DSC_INFCTEMULTIMODAL, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_InfGTVe: TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeNorm.infGTVe.Count);

  for i := 0 to CTe.infCTeNorm.infGTVe.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infGTVe');

    Result[i].AppendChild(AddNode(tcStr, '#390', 'chCTe', 44, 44, 1,
                                   CTe.infCTeNorm.infGTVe[i].chCTe, DSC_CHAVE));

    nodeArray := Gerar_CompGTVe(CTe.infCTeNorm.infGTVe[i].Comp);

    for j := 0 to CTe.infCTeNorm.infGTVe[i].Comp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if CTe.infCTeNorm.infGTVe.Count > 999 then
    wAlerta('#', 'infGTVe', DSC_INFGTVE, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_CompGTVe(
  Comp: TinfGTVeCompCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, comp.Count);

  for i := 0 to comp.Count - 1 do
  begin
    if comp[i].vComp <> 0 then
    begin
      Result[i] := FDocument.CreateElement('Comp');

      Result[i].AppendChild(AddNode(tcStr, '#212', 'tpComp', 1, 1, 1,
                                      tpCompToStr(Comp[i].tpComp), DSC_TPCOMP));

      Result[i].AppendChild(AddNode(tcDe2, '#213', 'vComp', 1, 15, 1,
                                                     comp[i].vComp, DSC_VCOMP));

      Result[i].AppendChild(AddNode(tcStr, '#213', 'xComp', 1, 15, 0,
                                                     comp[i].xComp, DSC_XCOMP));
    end;
  end;

  if comp.Count > 990 then
    wAlerta('#211', 'Comp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfCTeComp: TACBrXmlNode;
var
  chave, xTag: string;
begin
  Result := nil;

  if (CTe.Ide.tpCTe = tcComplemento) then
  begin
    Result := FDocument.CreateElement('infCteComp');

    chave := OnlyNumber(CTe.infCTeComp.Chave);
    xTag := 'chCTe';

    if VersaoDF = ve200 then
      xTag := 'chave';

    Result.AppendChild(AddNode(tcStr, '#411', xTag, 44, 44, 1,
                                                             chave, DSC_CHAVE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#411', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);
  end;
end;

function TCTeXmlWriter.Gerar_InfCTeComp10: TACBrXmlNodeArray;
var
  i: Integer;
  chave: string;
begin
  Result := nil;
  SetLength(Result, CTe.infCTeComp10.Count);

  if (CTe.Ide.tpCTe = tcComplemento) then
  begin
    for i := 0 to CTe.infCTeComp10.Count - 1 do
    begin
      Result[i] := FDocument.CreateElement('infCteComp');

      chave := OnlyNumber(CTe.infCteComp10[i].chCTe);

      Result[i].AppendChild(AddNode(tcStr, '#383', 'chCTe', 44, 44, 1,
                                                             chave, DSC_CHAVE));

      if chave <> '' then
        if not ValidarChave(chave) then
          wAlerta('#383', 'chCTe', DSC_REFNFE, ERR_MSG_INVALIDO);
    end;

    if CTe.infCTeComp10.Count > 10 then
      wAlerta('#382', 'infCteComp', '', ERR_MSG_MAIOR_MAXIMO + '10');
  end;
end;

function TCTeXmlWriter.Gerar_InfCTeAnu: TACBrXmlNode;
var
  chave: string;
begin
  Result := nil;

  if (CTe.Ide.tpCTe = tcAnulacao) then
  begin
    Result := FDocument.CreateElement('infCteAnu');

    chave := OnlyNumber(CTe.InfCTeAnu.chCTe);

    Result.AppendChild(AddNode(tcStr, '#413', 'chCte', 44, 44, 1,
                                                             chave, DSC_CHAVE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#413', 'chCte', DSC_REFNFE, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcDat, '#414', 'dEmi', 10, 10, 1,
                                                 CTe.InfCTeAnu.dEmi, DSC_DEMI));
  end;
end;

function TCTeXmlWriter.Gerar_Rem: TACBrXmlNode;
var
  xNome: string;
begin
  Result := nil;

  if (trim(CTe.Rem.CNPJCPF) <> '') or (trim(CTe.Rem.xNome) <> '') then
  begin
    if VersaoDF <= ve300 then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Result := FDocument.CreateElement('rem');

    if CTe.Rem.enderReme.cPais = 0 then
      CTe.Rem.enderReme.cPais := 1058;

    if CTe.Rem.enderReme.cPais = 1058 then
      Result.AppendChild(AddNodeCNPJCPF('#039', '#040', CTe.Rem.CNPJCPF))
    else
      Result.AppendChild(AddNodeCNPJ('#039', '00000000000000', CODIGO_BRASIL, True));

    Result.AppendChild(Gerar_IE(CTe.Rem.IE, CTe.Rem.enderReme.UF));

    if CTe.Ide.tpAmb = TpcnTipoAmbiente(taHomologacao) then
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                                              xNome, DSC_XNOME))
    else
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                               CTe.Rem.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, '#117', 'xFant', 2, 60, 0,
                                                     CTe.Rem.xFant, DSC_XFANT));
                                                     
    Result.AppendChild(AddNode(tcStr, '#044', 'fone', 7, 12, 0,
                                     OnlyNumber(CTe.Rem.fone), DSC_FONE));

    Result.AppendChild(Gerar_Endereco('enderReme', CTe.Rem.enderReme));

    Result.AppendChild(AddNode(tcStr, '#056', 'email', 1, 60, 0,
                                               CTe.Rem.email, DSC_EMAIL));
  end;
end;

function TCTeXmlWriter.Gerar_Dest: TACBrXmlNode;
var
  xNome: string;
begin
  Result := nil;

  if (trim(CTe.Dest.CNPJCPF) <> '') or (trim(CTe.Dest.xNome) <> '') then
  begin
    if VersaoDF <= ve300 then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Result := FDocument.CreateElement('dest');

    if CTe.Dest.EnderDest.cPais = 0 then
      CTe.Dest.EnderDest.cPais := 1058;

    if CTe.Dest.EnderDest.cPais = 1058 then
      Result.AppendChild(AddNodeCNPJCPF('#039', '#040', CTe.Dest.CNPJCPF))
    else
      Result.AppendChild(AddNodeCNPJ('#039', '00000000000000', CODIGO_BRASIL, True));

    Result.AppendChild(Gerar_IE(CTe.Dest.IE, CTe.Dest.EnderDest.UF));

    if CTe.Ide.tpAmb = TpcnTipoAmbiente(taHomologacao) then
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                                              xNome, DSC_XNOME))
    else
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                               CTe.Dest.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, '#044', 'fone', 7, 12, 0,
                                     OnlyNumber(CTe.Dest.fone), DSC_FONE));

    Result.AppendChild(AddNode(tcStr, '#184', 'ISUF', 8, 9, 0,
                                                      CTe.Dest.ISUF, DSC_ISUF));

    if (Opcoes.ValidarInscricoes) and (trim(CTe.Dest.ISUF) <> '') then
      if not ValidarISUF(CTe.Dest.ISUF) then
        wAlerta('#184', 'ISUF', DSC_ISUF, ERR_MSG_INVALIDO);

    Result.AppendChild(Gerar_Endereco('enderDest', CTe.Dest.EnderDest));

    Result.AppendChild(AddNode(tcStr, '#056', 'email', 1, 60, 0,
                                               CTe.Dest.email, DSC_EMAIL));
  end;
end;

function TCTeXmlWriter.Gerar_Exped: TACBrXmlNode;
var
  xNome: string;
begin
  Result := nil;

  if (trim(CTe.Exped.CNPJCPF) <> '') or (trim(CTe.Exped.xNome) <> '') then
  begin
    if VersaoDF <= ve300 then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Result := FDocument.CreateElement('exped');

    if CTe.Exped.EnderExped.cPais = 0 then
      CTe.Exped.EnderExped.cPais := 1058;

    if CTe.Exped.EnderExped.cPais = 1058 then
      Result.AppendChild(AddNodeCNPJCPF('#039', '#040', CTe.Exped.CNPJCPF))
    else
      Result.AppendChild(AddNodeCNPJ('#039', '00000000000000', CODIGO_BRASIL, True));

    Result.AppendChild(Gerar_IE(CTe.Exped.IE, CTe.Exped.EnderExped.UF));

    if CTe.Ide.tpAmb = TpcnTipoAmbiente(taHomologacao) then
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                                              xNome, DSC_XNOME))
    else
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                               CTe.Exped.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, '#044', 'fone', 7, 12, 0,
                                     OnlyNumber(CTe.Exped.fone), DSC_FONE));

    Result.AppendChild(Gerar_Endereco('enderExped', CTe.Exped.EnderExped));

    Result.AppendChild(AddNode(tcStr, '#056', 'email', 1, 60, 0,
                                               CTe.Exped.email, DSC_EMAIL));
  end;
end;

function TCTeXmlWriter.Gerar_Receb: TACBrXmlNode;
var
  xNome: string;
begin
  Result := nil;

  if (trim(CTe.Receb.CNPJCPF) <> '') or (trim(CTe.Receb.xNome) <> '') then
  begin
    if VersaoDF <= ve300 then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Result := FDocument.CreateElement('receb');

    if CTe.Receb.EnderReceb.cPais = 0 then
      CTe.Receb.EnderReceb.cPais := 1058;

    if CTe.Receb.EnderReceb.cPais = 1058 then
      Result.AppendChild(AddNodeCNPJCPF('#039', '#040', CTe.Receb.CNPJCPF))
    else
      Result.AppendChild(AddNodeCNPJ('#039', '00000000000000', CODIGO_BRASIL, True));

    Result.AppendChild(Gerar_IE(CTe.Receb.IE, CTe.Receb.EnderReceb.UF));

    if CTe.Ide.tpAmb = TpcnTipoAmbiente(taHomologacao) then
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                                              xNome, DSC_XNOME))
    else
      Result.AppendChild(AddNode(tcStr, '#042', 'xNome', 2, 60, 1,
                                               CTe.Receb.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, '#044', 'fone', 7, 12, 0,
                                     OnlyNumber(CTe.Receb.fone), DSC_FONE));

    Result.AppendChild(Gerar_Endereco('enderReceb', CTe.receb.enderReceb));

    Result.AppendChild(AddNode(tcStr, '#056', 'email', 1, 60, 0,
                                               CTe.Receb.email, DSC_EMAIL));
  end;
end;

function TCTeXmlWriter.Gerar_Origem: TACBrXmlNode;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  Result := nil;

  if CTe.origem.xLgr <> '' then
  begin
    AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                       CTe.origem.UF,
                       CTe.origem.xMun,
                       CTe.origem.cMun);

    Result := FDocument.CreateElement('origem');

    Result.AppendChild(AddNode(tcStr, 'C05', 'xLgr', 2, 60, 1,
                                         CTe.origem.xLgr, DSC_XLGR));

    Result.AppendChild(AddNode(tcStr, 'C06', 'nro', 1, 60, 1,
      ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, CTe.origem.nro), DSC_NRO));

    Result.AppendChild(AddNode(tcStr, 'C07', 'xCpl', 1, 60, 0,
                                         CTe.origem.xCpl, DSC_XCPL));

    Result.AppendChild(AddNode(tcStr, 'C08', 'xBairro', 2, 60, 1,
                                   CTe.origem.xBairro, DSC_XBAIRRO));

    Result.AppendChild(AddNode(tcInt, 'C09', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

    if not ValidarMunicipio(cMun) then
      wAlerta('C09', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcStr, 'C10', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

    Result.AppendChild(AddNode(tcInt, 'C12', 'CEP', 8, 8, 1,
                                           CTe.origem.CEP, DSC_CEP));

    Result.AppendChild(AddNode(tcStr, 'C11', 'UF', 2, 2, 1, xUF, DSC_UF));

    if not ValidarUF(xUF) then
      wAlerta('C11', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcStr, '126', 'fone', 7, 12, 0,
                                        OnlyNumber(CTe.origem.fone), DSC_FONE));
  end;
end;

function TCTeXmlWriter.Gerar_Destino: TACBrXmlNode;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  Result := nil;

  if CTe.destino.xLgr <> '' then
  begin
    AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                       CTe.destino.UF,
                       CTe.destino.xMun,
                       CTe.destino.cMun);

    Result := FDocument.CreateElement('destino');

    Result.AppendChild(AddNode(tcStr, 'C05', 'xLgr', 2, 60, 1,
                                         CTe.destino.xLgr, DSC_XLGR));

    Result.AppendChild(AddNode(tcStr, 'C06', 'nro', 1, 60, 1,
      ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, CTe.destino.nro), DSC_NRO));

    Result.AppendChild(AddNode(tcStr, 'C07', 'xCpl', 1, 60, 0,
                                         CTe.destino.xCpl, DSC_XCPL));

    Result.AppendChild(AddNode(tcStr, 'C08', 'xBairro', 2, 60, 1,
                                   CTe.destino.xBairro, DSC_XBAIRRO));

    Result.AppendChild(AddNode(tcInt, 'C09', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

    if not ValidarMunicipio(cMun) then
      wAlerta('C09', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcStr, 'C10', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

    Result.AppendChild(AddNode(tcInt, 'C12', 'CEP', 8, 8, 1,
                                           CTe.destino.CEP, DSC_CEP));

    Result.AppendChild(AddNode(tcStr, 'C11', 'UF', 2, 2, 1, xUF, DSC_UF));

    if not ValidarUF(xUF) then
      wAlerta('C11', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcStr, '126', 'fone', 7, 12, 0,
                                        OnlyNumber(CTe.destino.fone), DSC_FONE));
  end;
end;

function TCTeXmlWriter.Gerar_DetGTV: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('detGTV');

  nodeArray := Gerar_InfEspecie;

  for i := 0 to CTe.detGTV.infEspecie.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(AddNode(tcDe4, '#143', 'qCarga', 1, 15, 1,
                                                  CTe.detGTV.qCarga, DSC_XLGR));

  nodeArray := Gerar_InfVeiculo;

  for i := 0 to CTe.detGTV.infVeiculo.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TCTeXmlWriter.Gerar_InfEspecie: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.detGTV.infEspecie.Count);

  for i := 0 to CTe.detGTV.infEspecie.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infEspecie');

    Result[i].AppendChild(AddNode(tcStr, '#139', 'tpEspecie', 1, 1, 1,
             TEspecieToStr(CTe.detGTV.infEspecie[i].tpEspecie), DSC_TPESPECIE));

    Result[i].AppendChild(AddNode(tcDe2, '#140', 'vEspecie', 1, 15, 1,
                              CTe.detGTV.infEspecie[i].vEspecie, DSC_VESPECIE));

    Result[i].AppendChild(AddNode(tcStr, '#141', 'tpNumerario', 1, 1, 1,
      tpNumerarioToStr(CTe.detGTV.infEspecie[i].tpNumerario), DSC_TPNUMERARIO));

    Result[i].AppendChild(AddNode(tcStr, '#142', 'xMoedaEstr', 2, 60, 0,
                          CTe.detGTV.infEspecie[i].xMoedaEstr, DSC_XMOEDAESTR));
  end;

  if CTe.detGTV.infEspecie.Count > 999 then
    wAlerta('#138', 'infEspecie', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_InfVeiculo: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, CTe.detGTV.infVeiculo.Count);

  for i := 0 to CTe.detGTV.infVeiculo.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infVeiculo');

    Result[i].AppendChild(AddNode(tcStr, '#145', 'placa', 1, 7, 1,
                                    CTe.detGTV.infVeiculo[i].placa, DSC_PLACA));

    Result[i].AppendChild(AddNode(tcStr, '#146', 'UF', 2, 2, 0,
                                          CTe.detGTV.infVeiculo[i].UF, DSC_UF));

    Result[i].AppendChild(AddNode(tcStr, '#147', 'RNTRC', 8, 8, 0,
                                    CTe.detGTV.infVeiculo[i].RNTRC, DSC_RNTRC));
  end;

  if CTe.detGTV.infVeiculo.Count > 999 then
    wAlerta('#148', 'infVeiculo', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_InfCarga(infCarga: TInfCarga): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('infCarga');

  Result.AppendChild(AddNode(tcDe2, '#254', 'vCarga', 1, 15, 1,
                                                  InfCarga.vCarga, DSC_VTMERC));

  Result.AppendChild(AddNode(tcStr, '#255', 'proPred', 1, 60, 1,
                                                   InfCarga.proPred, DSC_PRED));

  Result.AppendChild(AddNode(tcStr, '#256', 'xOutCat', 1, 30, 0,
                                                 InfCarga.xOutCat, DSC_OUTCAT));

  nodeArray := Gerar_InfQCarga(infCarga);

  for i := 0 to InfCarga.InfQ.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  if VersaoDF >= ve300 then
    Result.AppendChild(AddNode(tcDe2, '#250', 'vCargaAverb', 1, 15, 0,
                                             InfCarga.vCargaAverb, DSC_VTMERC));
end;

function TCTeXmlWriter.Gerar_InfQCarga(infCarga: TInfCarga): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, InfCarga.InfQ.Count);

  for i := 0 to InfCarga.InfQ.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infQ');

    Result[i].AppendChild(AddNode(tcStr, '#258', 'cUnid', 2, 2, 1,
                              UnidMedToStr(InfCarga.InfQ[i].cUnid), DSC_CUNID));

    Result[i].AppendChild(AddNode(tcStr, '#259', 'tpMed', 1, 20, 1,
                                            InfCarga.InfQ[i].tpMed, DSC_TPMED));

    Result[i].AppendChild(AddNode(tcDe4, '#260', 'qCarga', 1, 15, 1,
                                             InfCarga.InfQ[i].qCarga, DSC_QTD));
  end;

  if InfCarga.InfQ.Count > 990 then
    wAlerta('#257', 'infQ', DSC_INFQ, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_Det: TACBrXmlNodeArray;
var
  i, j: Integer;
  nItem: string;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, CTe.det.Count);

  for i := 0 to CTe.det.Count - 1 do
  begin
    nItem := IntToStr(i + 1);

    Result[i] := FDocument.CreateElement('det');
    Result[i].SetAttribute('nItem', nItem);

    Result[i].AppendChild(AddNode(tcInt, '#027', 'cMunIni', 7, 7, 1,
                                              CTe.det[i].cMunIni, DSC_CMUNEMI));

    if not ValidarMunicipio(CTe.det[i].cMunIni) then
      wAlerta('#027', 'cMunIni', DSC_CMUNEMI, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#028', 'xMunIni', 2, 60, 1,
                                                 CTe.det[i].xMunIni, DSC_XMUN));

    Result[i].AppendChild(AddNode(tcInt, '#030', 'cMunFim', 7, 7, 1,
                                              CTe.det[i].cMunFim, DSC_CMUNEMI));

    if not ValidarMunicipio(CTe.det[i].cMunFim) then
      wAlerta('#030', 'cMunFim', DSC_CMUNEMI, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#031', 'xMunFim', 2, 60, 1,
                                                 CTe.det[i].xMunFim, DSC_XMUN));

    Result[i].AppendChild(AddNode(tcDe2, '#209', 'vPrest', 1, 15, 1,
                                               CTe.det[i].vPrest, DSC_VTPREST));

    Result[i].AppendChild(AddNode(tcDe2, '#210', 'vRec', 1, 15, 1,
                                                    CTe.det[i].vRec, DSC_VREC));

    nodeArray := Gerar_Comp(CTe.det[i].comp);

    for j := 0 to CTe.det[i].comp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_InfNFe(CTe.det[i].InfNFe);

    for j := 0 to CTe.det[i].InfNFe.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_InfDocAnt(CTe.det[i].infdocAnt);

    for j := 0 to CTe.det[i].infdocAnt.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if CTe.det.Count > 999 then
    wAlerta('#148', 'det', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_InfDocAnt(
  infDocAnt: TinfDocAntCollection): TACBrXmlNodeArray;
var
  i, j: Integer;
  chave: string;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, infDocAnt.Count);

  for i := 0 to infDocAnt.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infDocAnt');

    chave := OnlyNumber(infDocAnt[i].chCTe);

    Result[i].AppendChild(AddNode(tcStr, '#298', 'chCTe', 44, 44, 1,
                                                            chave, DSC_REFNFE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#298', 'chCTe', DSC_REFNFE, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#349', 'tpPrest', 1, 1, 1,
                              tpPrestToStr(infDocAnt[i].tpPrest), DSC_TPPREST));

    nodeArray := Gerar_InfNFeTranspParcial(infDocAnt[i].infNFeTranspParcial);

    for j := 0 to infDocAnt[i].infNFeTranspParcial.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if CTe.infCTeNorm.docAnt.emiDocAnt.Count > 990 then
    wAlerta('#345', 'infDocAnt', DSC_EMIDOCANT, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_InfNFeTranspParcial(
  infNFeTranspParcial: TinfNFeTranspParcialCollection): TACBrXmlNodeArray;
var
  i: Integer;
  chave: string;
begin
  Result := nil;
  SetLength(Result, infNFeTranspParcial.Count);

  for i := 0 to infNFeTranspParcial.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNFeTranspParcial');

    chave := OnlyNumber(infNFeTranspParcial[i].chNFe);

    Result[i].AppendChild(AddNode(tcStr, '#298', 'chNFe', 44, 44, 1,
                                                            chave, DSC_REFNFE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#298', 'chNFe', DSC_REFNFE, ERR_MSG_INVALIDO);
  end;

  if infNFeTranspParcial.Count > 999 then
    wAlerta('#351', 'infNFeTranspParcial', DSC_IDDOCANT, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_InfModal: TACBrXmlNode;
var
  versao: string;
begin
  versao := GetVersaoModalCTe(VersaoDF, CTe.Ide.modal);

  Result := FDocument.CreateElement('infModal');
  Result.SetAttribute('versaoModal', versao);

  case StrToInt(TpModalToStr(CTe.Ide.modal)) of
    01: if ModeloDF = moCTe then
          Result.AppendChild(Gerar_ModalRodo(CTe.infCTeNorm.rodo))
        else
          Result.AppendChild(Gerar_ModalRodoOS);
    02: Result.AppendChild(Gerar_ModalAereo(CTe.infCTeNorm.aereo));
    03: Result.AppendChild(Gerar_ModalAquav(CTe.infCTeNorm.aquav));
    04: Result.AppendChild(Gerar_ModalFerrov(CTe.infCTeNorm.ferrov));
    05: Result.AppendChild(Gerar_ModalDuto(CTe.infCTeNorm.duto));
    06: Result.AppendChild(Gerar_ModalMultimodal);
  end;
end;

function TCTeXmlWriter.Gerar_InfModalSimp: TACBrXmlNode;
var
  versao: string;
begin
  versao := GetVersaoModalCTe(VersaoDF, CTe.Ide.modal);

  Result := FDocument.CreateElement('infModal');
  Result.SetAttribute('versaoModal', versao);

  case StrToInt(TpModalToStr(CTe.Ide.modal)) of
    01: Result.AppendChild(Gerar_ModalRodo(CTe.infModal.rodo));
    02: Result.AppendChild(Gerar_ModalAereo(CTe.infModal.aereo));
    03: Result.AppendChild(Gerar_ModalAquav(CTe.infModal.aquav));
  end;
end;

function TCTeXmlWriter.Gerar_ModalRodo(rodo: TRodo): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('rodo');

  if CTe.infCTeNorm.rodo.RNTRC = 'ISENTO' then
    Result.AppendChild(AddNode(tcStr, '#02', 'RNTRC', 6, 6, 1,
                                                         rodo.RNTRC, DSC_RNTRC))
  else
    Result.AppendChild(AddNode(tcStr, '#02', 'RNTRC', 8, 8, 1,
                                            OnlyNumber(rodo.RNTRC), DSC_RNTRC));

  if VersaoDF < ve300 then
  begin
    Result.AppendChild(AddNode(tcDat, '#03', 'dPrev', 10, 10, 1,
                                                        rodo.dPrev, DSC_DPREV));

    Result.AppendChild(AddNode(tcStr, '#04', 'lota', 1, 1, 1,
                                          TpLotacaoToStr(rodo.Lota), DSC_LOTA));

    Result.AppendChild(AddNode(tcStr, '#05', 'CIOT', 12, 12, 0,
                                                          rodo.CIOT, DSC_CIOT));

    nodeArray := Gerar_OCC(rodo);

    for i := 0 to rodo.occ.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    nodeArray := Gerar_ValePed(rodo);

    for i := 0 to rodo.valePed.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    if rodo.Lota = ltSim then
    begin
      nodeArray := Gerar_Veic(rodo);

      for i := 0 to rodo.veic.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;

    nodeArray := Gerar_Lacre(rodo);

    for i := 0 to rodo.lacRodo.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    if rodo.Lota = ltSim then
    begin
      nodeArray := Gerar_Moto(rodo);

      for i := 0 to rodo.moto.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end
  else
  begin
    nodeArray := Gerar_OCC(rodo);

    for i := 0 to rodo.occ.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_OCC(rodo: TRodo): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, rodo.occ.Count);

  for i := 0 to rodo.occ.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('occ');

    Result[i].AppendChild(AddNode(tcStr, '#07', 'serie', 1, 3, 0,
                                                 rodo.occ[i].serie, DSC_SERIE));

    Result[i].AppendChild(AddNode(tcInt, '#08', 'nOcc', 1, 6, 1,
                                                   rodo.occ[i].nOcc, DSC_NOCC));

    Result[i].AppendChild(AddNode(tcDat, '#09', 'dEmi', 10, 10, 1,
                                                   rodo.occ[i].dEmi, DSC_DEMI));

    Result[i].AppendChild(Gerar_EmiOCC(rodo.occ[i].emiOcc));
  end;

  if rodo.occ.Count > 10 then
    wAlerta('#06', 'occ', DSC_OCC, ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TCTeXmlWriter.Gerar_EmiOCC(emiOcc: TEmiOCC): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('emiOcc');

  Result.AppendChild(AddNodeCNPJ('#11', emiOcc.CNPJ, CODIGO_BRASIL, True));

  Result.AppendChild(AddNode(tcStr, '#12', 'cInt', 1, 10, 0,
                                                        emiOcc.cInt, DSC_CINT));

  Result.AppendChild(AddNode(tcStr, '#13', 'IE', 2, 14, 1,
                                                OnlyNumber(emiOcc.IE), DSC_IE));

  if (Opcoes.ValidarInscricoes) then
    if not ValidarIE(emiOcc.IE, emiOcc.UF) then
      wAlerta('#13', 'IE', DSC_IE, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#14', 'UF', 2, 2, 1, emiOcc.UF, DSC_CUF));

  if not ValidarUF(emiOcc.UF) then
    wAlerta('#14', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#15', 'fone', 7, 12, 0,
                                            OnlyNumber(emiOcc.fone), DSC_FONE));
end;

function TCTeXmlWriter.Gerar_ValePed(rodo: TRodo): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, rodo.valePed.Count);

  for i := 0 to rodo.valePed.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('valePed');

    Result[i].AppendChild(AddNode(tcStr, '#17', 'CNPJForn', 14, 14, 1,
                                           rodo.valePed[i].CNPJForn, DSC_CNPJ));

    Result[i].AppendChild(AddNode(tcStr, '#18', 'nCompra', 1, 20, 1,
                                         rodo.valePed[i].nCompra, DSC_NCOMPRA));

    Result[i].AppendChild(AddNode(tcStr, '#19', 'CNPJPg', 14, 14, 0,
                                             rodo.valePed[i].CNPJPg, DSC_CNPJ));

    Result[i].AppendChild(AddNode(tcDe2, '#20', 'vValePed', 1, 15, 1,
                                       rodo.valePed[i].vValePed, DSC_VVALEPED));
  end;

  if rodo.valePed.Count > 990 then
    wAlerta('#16', 'valePed', DSC_VVALEPED, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_Veic(rodo: TRodo): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, rodo.veic.Count);

  for i := 0 to rodo.veic.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('veic');

    Result[i].AppendChild(AddNode(tcStr, '#22', 'cInt', 1, 10, 0,
                                                 rodo.veic[i].cInt, DSC_CINTV));

    Result[i].AppendChild(AddNode(tcStr, '#23', 'RENAVAM', 9, 11, 1,
                                            rodo.veic[i].RENAVAM, DSC_RENAVAM));

    Result[i].AppendChild(AddNode(tcStr, '#24', 'placa', 1, 7, 1,
                                                rodo.veic[i].placa, DSC_PLACA));

    Result[i].AppendChild(AddNode(tcInt, '#25', 'tara', 1, 6, 1,
                                                  rodo.veic[i].tara, DSC_TARA));

    Result[i].AppendChild(AddNode(tcInt, '#26', 'capKG', 1, 6, 1,
                                                rodo.veic[i].capKG, DSC_CAPKG));

    Result[i].AppendChild(AddNode(tcInt, '#27', 'capM3', 1, 3, 1,
                                                rodo.veic[i].capM3, DSC_CAPM3));

    Result[i].AppendChild(AddNode(tcStr, '#28', 'tpProp', 1, 1, 1,
                          TpPropriedadeToStr(rodo.veic[i].tpProp), DSC_TPPROP));

    Result[i].AppendChild(AddNode(tcStr, '#29', 'tpVeic', 1, 1, 1,
                              TpVeiculoToStr(rodo.veic[i].tpVeic), DSC_TPVEIC));

    Result[i].AppendChild(AddNode(tcStr, '#30', 'tpRod', 2, 2, 1,
                                 TpRodadoToStr(rodo.veic[i].tpRod), DSC_TPROD));

    Result[i].AppendChild(AddNode(tcStr, '#31', 'tpCar', 2, 2, 1,
                             TpCarroceriaToStr(rodo.veic[i].tpCar), DSC_TPCAR));

    Result[i].AppendChild(AddNode(tcStr, '#32', 'UF', 2, 2, 1,
                                                     rodo.veic[i].UF, DSC_CUF));

    if not ValidarUF(rodo.veic[i].UF) then
      wAlerta('#32', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Result[i].AppendChild(Gerar_VeicProp(rodo.veic[i].prop));
  end;

  if rodo.veic.Count > 4 then
    wAlerta('#21', 'veic', DSC_VEIC, ERR_MSG_MAIOR_MAXIMO + '4');
end;

function TCTeXmlWriter.Gerar_VeicProp(prop: TProp): TACBrXmlNode;
begin
  Result := nil;

  if (trim(prop.CNPJCPF) <> '') or (trim(prop.RNTRC) <> '') or
     (trim(prop.xNome) <> '') then
  begin
    Result := FDocument.CreateElement('prop');

    Result.AppendChild(AddNodeCNPJCPF('#34', '#35', prop.CNPJCPF));

    if prop.RNTRC = 'ISENTO' then
      Result.AppendChild(AddNode(tcStr, '#36', 'RNTRC', 6, 6, 1,
                                                         prop.RNTRC, DSC_RNTRC))
    else
      Result.AppendChild(AddNode(tcStr, '#36', 'RNTRC', 8, 8, 1,
                                            OnlyNumber(prop.RNTRC), DSC_RNTRC));

    Result.AppendChild(AddNode(tcStr, '#37', 'xNome', 2, 60, 1,
                                                        prop.xNome, DSC_XNOME));

    Result.AppendChild(Gerar_IE(prop.IE, prop.UF));

    Result.AppendChild(AddNode(tcStr, '#39', 'UF', 2, 2, 1,
                                                             prop.UF, DSC_CUF));

    if not ValidarUF(prop.UF) then
      wAlerta('#39', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcStr, '#40', 'tpProp', 1, 1, 1,
                                         TpPropToStr(prop.tpProp), DSC_TPPROP));
  end;
end;

function TCTeXmlWriter.Gerar_Lacre(rodo: TRodo): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, rodo.lacRodo.Count);

  for i := 0 to rodo.lacRodo.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacRodo');

    Result[i].AppendChild(AddNode(tcStr, '#42', 'nLacre', 1, 20, 1,
                                           rodo.lacRodo[i].nLacre, DSC_NLACRE));
  end;

  if rodo.lacRodo.Count > 990 then
    wAlerta('#41', 'lacRodo', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_Moto(rodo: TRodo): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, rodo.moto.Count);

  for i := 0 to rodo.moto.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('moto');

    Result[i].AppendChild(AddNode(tcStr, '#44', 'xNome', 2, 60, 1,
                                                rodo.moto[i].xNome, DSC_XNOME));

    Result[i].AppendChild(AddNode(tcStr, '#45', 'CPF', 11, 11, 1,
                                                    rodo.moto[i].CPF, DSC_CPF));
  end;

  if rodo.moto.Count > 990 then
    wAlerta('#43', 'moto', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_ModalRodoOS: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('rodoOS');

  if CTe.infCTeNorm.rodoOS.TAF <> '' then
    Result.AppendChild(AddNode(tcStr, '#02', 'TAF', 12, 12, 1,
                                            CTe.infCTeNorm.rodoOS.TAF, DSC_TAF))
  else
    Result.AppendChild(AddNode(tcStr, '#03', 'NroRegEstadual', 25, 25, 1,
                     CTe.infCTeNorm.rodoOS.NroRegEstadual, DSC_NROREGESTADUAL));

  Result.AppendChild(Gerar_RodoOSVeic(CTe.infCTeNorm.rodoOS.veic));
  Result.AppendChild(Gerar_RodoOSInfFretamento(CTe.infCTeNorm.rodoOS.infFretamento));
end;

function TCTeXmlWriter.Gerar_RodoOSVeic(veic: TVeicOS): TACBrXmlNode;
begin
  Result := nil;

  if Trim(veic.placa) <> '' then
  begin
    Result := FDocument.CreateElement('veic');

    Result.AppendChild(AddNode(tcStr, '#05', 'placa', 1, 7, 1,
                                                        veic.placa, DSC_PLACA));

    Result.AppendChild(AddNode(tcStr, '#06', 'RENAVAM', 9, 11, 0,
                                                    veic.RENAVAM, DSC_RENAVAM));

    Result.AppendChild(Gerar_RodoOSVeicProp(veic.prop));

    Result.AppendChild(AddNode(tcStr, '#16', 'UF', 2, 2, 0, veic.UF, DSC_CUF));

    if veic.UF <> '' then
      if not ValidarUF(veic.UF) then
        wAlerta('#16', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  end;
end;

function TCTeXmlWriter.Gerar_RodoOSVeicProp(prop: TPropOS): TACBrXmlNode;
begin
  Result := nil;

  if (trim(prop.CNPJCPF) <> '') or (trim(prop.xNome) <> '') then
  begin
    Result := FDocument.CreateElement('prop');

    Result.AppendChild(AddNodeCNPJCPF('#08', '#09', prop.CNPJCPF));

    if prop.TAF <> '' then
      Result.AppendChild(AddNode(tcStr, '#10', 'TAF', 12, 12, 1,
                                                             prop.TAF, DSC_TAF))
    else
      Result.AppendChild(AddNode(tcStr, '#11', 'NroRegEstadual', 25, 25, 1,
                                      prop.NroRegEstadual, DSC_NROREGESTADUAL));

    Result.AppendChild(AddNode(tcStr, '#12', 'xNome', 2, 60, 1,
                                                        prop.xNome, DSC_XNOME));

    Result.AppendChild(Gerar_IE(prop.IE, prop.UF));

    Result.AppendChild(AddNode(tcStr, '#14', 'UF', 2, 2, 1,
                                                             prop.UF, DSC_CUF));

    if not ValidarUF(prop.UF) then
      wAlerta('#39', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Result.AppendChild(AddNode(tcStr, '#15', 'tpProp', 1, 1, 1,
                                         TpPropToStr(prop.tpProp), DSC_TPPROP));
  end;
end;

function TCTeXmlWriter.Gerar_RodoOSInfFretamento(
  infFretamento: TinfFretamento): TACBrXmlNode;
begin
  Result := nil;

  if (CTe.Ide.tpServ = tsTranspPessoas) and (infFretamento.tpFretamento <> tfNenhum) then
  begin
    Result := FDocument.CreateElement('infFretamento');

    Result.AppendChild(AddNode(tcStr, '#18', 'tpFretamento', 1, 1, 1,
              TpFretamentoToStr(infFretamento.tpFretamento), DSC_TPFRETAMENTO));

    if infFretamento.tpFretamento = tfEventual then
      Result.AppendChild(AddNode(tcStr, '#19', 'dhViagem', 25, 25, 0,
      DateTimeWithTimeZone(infFretamento.dhViagem, CTe.ide.cUF), DSC_DHVIAGEM));
  end;
end;

function TCTeXmlWriter.Gerar_ModalAereo(aereo: TAereo): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('aereo');

  Result.AppendChild(AddNode(tcInt, '#02', 'nMinu', 9, 9, 0,
                                                       aereo.nMinu, DSC_NMINU));

  Result.AppendChild(AddNode(tcStr, '#03', 'nOCA', 11, 11, 0,
                                                         aereo.nOCA, DSC_NOCA));

  Result.AppendChild(AddNode(tcDat, '#04', 'dPrevAereo', 10, 10, 0,
                                                  aereo.dPrevAereo, DSC_DPREV));

  if VersaoDF = ve200 then
  begin
    Result.AppendChild(AddNode(tcStr, '#05', 'xLAgEmi', 1, 20, 0,
                                                   aereo.xLAgEmi, DSC_XLAGEMI));

    Result.AppendChild(AddNode(tcStr, '#06', 'IdT', 1, 14, 0,
                                                           aereo.IdT, DSC_IDT));
  end;

  if VersaoDF >= ve300 then
  begin
    Result.AppendChild(Gerar_AereoNatCarga(aereo.natCarga, -1));
    Result.AppendChild(Gerar_AereoTarifa(aereo.tarifa));
  end
  else
  begin
    Result.AppendChild(Gerar_AereoTarifa(aereo.tarifa));
    Result.AppendChild(Gerar_AereoNatCarga(aereo.natCarga, 1));
  end;

  if VersaoDF >= ve300 then
  begin
    nodeArray := Gerar_Peri(aereo.peri);

    for i := 0 to aereo.peri.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_AereoNatCarga(natCarga: TNatCarga;
  NOcorr: Integer): TACBrXmlNode;
var
  i: Integer;
begin
  Result := nil;

  if (Trim(natCarga.xDime) <> '') or (natCarga.cinfManu.Count <> 0) or
     ((NOcorr = 1) and (trim(natCarga.cImp) <> '')) then
  begin
    Result := FDocument.CreateElement('natCarga');

    Result.AppendChild(AddNode(tcStr, '#12', 'xDime', 5, 14, 0,
                                                    natCarga.xDime, DSC_XDIME));

    for i := 0 to natCarga.cinfManu.Count - 1 do
      Result.AppendChild(AddNode(tcStr, '#13', 'cInfManu', 1, 2, 0,
                TpInfManuToStrV2(natCarga.cinfManu[i].nInfManu), DSC_CINFMANU));

    Result.AppendChild(AddNode(tcStr, '#14', 'cIMP', 3, 3, NOcorr,
                                                      natCarga.cIMP, DSC_CIMP));
  end;
end;

function TCTeXmlWriter.Gerar_AereoTarifa(tarifa: TTarifa): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('tarifa');

  Result.AppendChild(AddNode(tcStr, '#08', 'CL', 1, 2, 1,
                                                            tarifa.CL, DSC_CL));

  Result.AppendChild(AddNode(tcStr, '#09', 'cTar', 1, 4, 0,
                                                        tarifa.cTar, DSC_CTAR));

  Result.AppendChild(AddNode(tcDe2, '#10', 'vTar', 1, 15, 1,
                                                        tarifa.vTar, DSC_VTAR));
end;

function TCTeXmlWriter.Gerar_ModalAquav(aquav: TAquav): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('aquav');

  Result.AppendChild(AddNode(tcDe2, '#02', 'vPrest', 1, 15, 1,
                                                     aquav.vPrest, DSC_VPREST));

  Result.AppendChild(AddNode(tcDe2, '#03', 'vAFRMM', 1, 15, 1,
                                                     aquav.vAFRMM, DSC_VAFRMM));

  if (VersaoDF = ve200) then
  begin
    Result.AppendChild(AddNode(tcStr, '#04', 'nBooking', 1, 10, 0,
                                                 aquav.nBooking, DSC_NBOOKING));

    Result.AppendChild(AddNode(tcStr, '#05', 'nCtrl', 1, 10, 0,
                                                       aquav.nCtrl, DSC_NCTRL));
  end;

  Result.AppendChild(AddNode(tcStr, '#06', 'xNavio', 1, 60, 1,
                                                     aquav.xNavio, DSC_XNAVIO));

  nodeArray := Gerar_Balsa(aquav.balsa);

  for i := 0 to aquav.balsa.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(AddNode(tcStr, '#09', 'nViag', 1, 10, 0,
                                                       aquav.nViag, DSC_NVIAG));

  Result.AppendChild(AddNode(tcStr, '#10', 'direc', 1, 1, 1,
                                       TpDirecaoToStr(aquav.direc), DSC_DIREC));

  if (VersaoDF = ve200) then
  begin
    Result.AppendChild(AddNode(tcStr, '#11', 'prtEmb', 1, 60, 0,
                                                     aquav.prtEmb, DSC_PRTEMB));

    Result.AppendChild(AddNode(tcStr, '#12', 'prtTrans', 1, 60, 0,
                                                 aquav.prtTrans, DSC_PRTTRANS));

    Result.AppendChild(AddNode(tcStr, '#13', 'prtDest', 1, 60, 0,
                                                   aquav.prtDest, DSC_PRTDEST));

    Result.AppendChild(AddNode(tcStr, '#14', 'tpNav', 1, 1, 1,
                                     TpNavegacaoToStr(aquav.tpNav), DSC_TPNAV));
  end;

  Result.AppendChild(AddNode(tcStr, '#15', 'irin', 1, 10, 1,
                                                         aquav.irin, DSC_IRIN));

  nodeArray := Gerar_DetCont(aquav.detCont);

  for i := 0 to aquav.detCont.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  if VersaoDF >= ve300 then
    Result.AppendChild(AddNode(tcStr, '#28', 'tpNav', 1, 1, 0,
                                     TpNavegacaoToStr(aquav.tpNav), DSC_TPNAV));
end;

function TCTeXmlWriter.Gerar_Balsa(balsa: TBalsaCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, balsa.Count);

  for i := 0 to balsa.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('balsa');

    Result[i].AppendChild(AddNode(tcStr, '#08', 'xBalsa', 1, 60, 1,
                                                  balsa[i].xBalsa, DSC_XBALSA));
  end;

  if balsa.Count > 3 then
    wAlerta('#07', 'balsa', DSC_XBALSA, ERR_MSG_MAIOR_MAXIMO + '3');
end;

function TCTeXmlWriter.Gerar_DetCont(
  detCont: TdetContCollection): TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, detCont.Count);

  for i := 0 to detCont.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('detCont');

    Result[i].AppendChild(AddNode(tcStr, '#17', 'nCont', 1, 20, 1,
                                                  detCont[i].nCont, DSC_NCONT));

    nodeArray := Gerar_AquavLacre(detCont[i].Lacre);

    for j := 0 to detCont[i].Lacre.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(Gerar_AquavInfDoc(detCont[i].infDoc));
  end;

  if detCont.Count > 999 then
    wAlerta('#16', 'detCont', DSC_DETCONT, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_AquavLacre(
  lacre: TLacreCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, lacre.Count);

  for i := 0 to Lacre.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacre');

    Result[i].AppendChild(AddNode(tcStr, '#19', 'nLacre', 1, 20, 1,
                                                  Lacre[i].nLacre, DSC_NLACRE));
  end;

  if Lacre.Count > 3 then
    wAlerta('#18', 'lacre', DSC_NLACRE, ERR_MSG_MAIOR_MAXIMO + '3');
end;

function TCTeXmlWriter.Gerar_AquavInfDoc(infDoc: TInfDocAquav): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if (infDoc.infNF.Count > 0) or (infDoc.infNFe.Count > 0) then
  begin
    Result := FDocument.CreateElement('infDoc');

    nodeArray := Gerar_AquavInfNF(infDoc.infNF);

    for i := 0 to infDoc.infNF.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    nodeArray := Gerar_AquavInfNFe(infDoc.infNFe);

    for i := 0 to infDoc.infNFe.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_AquavInfNF(
  infNF: TInfNFAquavCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, infNF.Count);

  for i := 0 to infNF.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNF');

    Result[i].AppendChild(AddNode(tcStr, '#22', 'serie', 1, 3, 1,
                                                    infNF[i].serie, DSC_SERIE));

    Result[i].AppendChild(AddNode(tcStr, '#23', 'nDoc', 1, 20, 1,
                                          OnlyNumber(infNF[i].nDoc), DSC_NDOC));

    Result[i].AppendChild(AddNode(tcDe2, '#24', 'unidRat', 1, 5, 0,
                                                 infNF[i].unidRat, DSC_QTDRAT));
  end;

  if infNF.Count > 999 then
    wAlerta('#21', 'infNF', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_AquavInfNFe(
  infNFe: TInfNFeAquavCollection): TACBrXmlNodeArray;
var
  i: Integer;
  chave: string;
begin
  Result := nil;
  SetLength(Result, infNFe.Count);

  for i := 0 to infNFe.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNFe');

    chave := OnlyNumber(infNFe[i].chave);

    Result[i].AppendChild(AddNode(tcStr, '#26', 'chave', 44, 44, 1,
                                                             chave, DSC_SERIE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#26', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcDe2, '#27', 'unidRat', 1, 5, 0,
                                                infNFe[i].unidRat, DSC_QTDRAT));
  end;

  if infNFe.Count > 999 then
    wAlerta('#25', 'infNFe', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TCTeXmlWriter.Gerar_ModalFerrov(ferrov: TFerrov): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('ferrov');

  Result.AppendChild(AddNode(tcStr, '#02', 'tpTraf', 1, 1, 1,
                                    TpTrafegoToStr(ferrov.tpTraf), DSC_TPTRAF));

  if VersaoDF >= ve300 then
  begin
    if ferrov.tpTraf = ttMutuo then
      Result.AppendChild(Gerar_TrafMut300(ferrov));

    Result.AppendChild(AddNode(tcStr, '#22', 'fluxo', 1, 10, 1,
                                                      ferrov.fluxo, DSC_FLUXO));
  end
  else
  begin
    if ferrov.tpTraf = ttMutuo then
      Result.AppendChild(Gerar_TrafMut(ferrov));

    Result.AppendChild(AddNode(tcStr, '#22', 'fluxo', 1, 10, 1,
                                                      ferrov.fluxo, DSC_FLUXO));

    Result.AppendChild(AddNode(tcStr, '#07', 'idTrem', 1, 7, 0,
                                                    ferrov.idTrem, DSC_IDTREM));

    Result.AppendChild(AddNode(tcDe2, '#08', 'vFrete', 1, 15, 0,
                                                    ferrov.vFrete, DSC_VFRETE));

    nodeArray := Gerar_FerroEnv(ferrov.ferroEnv);

    for i := 0 to ferrov.ferroEnv.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    nodeArray := Gerar_DetVag(ferrov.detVag);

    for i := 0 to ferrov.detVag.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_TrafMut(ferrov: TFerrov): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('trafMut');

  Result.AppendChild(AddNode(tcStr, '#04', 'respFat', 1, 1, 1,
                       TrafegoMutuoToStr(ferrov.trafMut.respFat), DSC_RESPFAT));

  Result.AppendChild(AddNode(tcStr, '#05', 'ferrEmi', 1, 1, 1,
                       TrafegoMutuoToStr(ferrov.trafMut.ferrEmi), DSC_FERREMI));
end;

function TCTeXmlWriter.Gerar_TrafMut300(ferrov: TFerrov): TACBrXmlNode;
var
  chave: string;
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('trafMut');

  Result.AppendChild(AddNode(tcStr, '#04', 'respFat', 1, 1, 1,
                       TrafegoMutuoToStr(ferrov.trafMut.respFat), DSC_RESPFAT));

  Result.AppendChild(AddNode(tcStr, '#05', 'ferrEmi', 1, 1, 1,
                       TrafegoMutuoToStr(ferrov.trafMut.ferrEmi), DSC_FERREMI));

  Result.AppendChild(AddNode(tcDe2, '#06', 'vFrete', 1, 15, 0,
                                            ferrov.trafMut.vFrete, DSC_VFRETE));

  chave := OnlyNumber(ferrov.trafMut.chCTeFerroOrigem);

  Result.AppendChild(AddNode(tcStr, '#07', 'chCTeFerroOrigem', 44, 44, 0,
                                            chave, DSC_CHAVE));

  if chave <> '' then
    if not ValidarChave(chave) then
      wAlerta('#07', 'chCTeFerroOrigem', DSC_CHAVE, ERR_MSG_INVALIDO);

  nodeArray := Gerar_FerroEnv(ferrov.ferroEnv);

  for i := 0 to ferrov.ferroEnv.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TCTeXmlWriter.Gerar_FerroEnv(ferroEnv: TFerroEnvCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, ferroEnv.Count);

  for i := 0 to ferroEnv.Count - 1 do
  begin
    if (trim(ferroEnv[i].CNPJ) <> '') or (trim(ferroEnv[i].xNome) <> '') then
    begin
      Result[i] := FDocument.CreateElement('ferroEnv');

      Result[i].AppendChild(AddNodeCNPJ('#10', ferroEnv[i].CNPJ, CODIGO_BRASIL, True));

      Result[i].AppendChild(AddNode(tcStr, '#11', 'cInt', 1, 10, 0,
                                                  ferroEnv[i].cInt, DSC_CINTF));

      if trim(ferroEnv[i].IE) <> '' then
      begin
        Result[i].AppendChild(AddNode(tcStr, '#12', 'IE', 2, 14, 1,
                                                       ferroEnv[i].IE, DSC_IE));

        if (Opcoes.ValidarInscricoes) then
          if not ValidarIE(ferroEnv[i].IE, ferroEnv[i].enderFerro.UF) then
            wAlerta('#12', 'IE', DSC_IE, ERR_MSG_INVALIDO);
      end;

      Result[i].AppendChild(AddNode(tcStr, '#13', 'xNome', 2, 60, 1,
                                                 ferroEnv[i].xNome, DSC_XNOME));

      Result[i].AppendChild(Gerar_FerrovEndereco(ferroEnv[i].EnderFerro));
    end;
  end;
end;

function TCTeXmlWriter.Gerar_FerrovEndereco(
  enderFerro: TEnderFerro): TACBrXmlNode;
var
  cMun: Integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                     EnderFerro.UF,
                     EnderFerro.xMun,
                     EnderFerro.cMun);

  Result := FDocument.CreateElement('enderFerro');

  Result.AppendChild(AddNode(tcStr, 'C05', 'xLgr', 2, 60, 1,
                                       EnderFerro.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, 'C06', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, EnderFerro.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, 'C07', 'xCpl', 1, 60, 0,
                                       EnderFerro.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, 'C08', 'xBairro', 2, 60, 1,
                                 EnderFerro.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, 'C09', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('C09', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'C10', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, 'C12', 'CEP', 8, 8, 1,
                                         EnderFerro.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, 'C11', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('C11', 'UF', DSC_UF, ERR_MSG_INVALIDO);
end;

function TCTeXmlWriter.Gerar_detVag(
  detVag: TDetVagCollection): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, detVag.Count);

  for i := 0 to detVag.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('detVag');

    Result[i].AppendChild(AddNode(tcInt, '#24', 'nVag', 8, 8, 1,
                                                    detVag[i].nVag, DSC_VAGAO));

    Result[i].AppendChild(AddNode(tcDe2, '#25', 'cap', 1, 5, 0,
                                                     detVag[i].cap, DSC_CAPTO));

    Result[i].AppendChild(AddNode(tcStr, '#26', 'tpVag', 3, 3, 0,
                                                   detVag[i].tpVag, DSC_TPVAG));

    Result[i].AppendChild(AddNode(tcDe2, '#27', 'pesoR', 1, 5, 1,
                                                   detVag[i].pesoR, DSC_PESOR));

    Result[i].AppendChild(AddNode(tcDe2, '#28', 'pesoBC', 1, 5, 1,
                                                 detVag[i].pesoBC, DSC_PESOBC));
  end;

  if detVag.Count > 990 then
    wAlerta('#23', 'detVag', DSC_VAGAO, ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TCTeXmlWriter.Gerar_ModalDuto(duto: TDuto): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('duto');

  Result.AppendChild(AddNode(tcDe6, '#02', 'vTar', 1, 15, 0,
                                                          duto.vTar, DSC_VTAR));

  Result.AppendChild(AddNode(tcDat, '#03', 'dIni', 10, 10, 1,
                                                          duto.dIni, DSC_DINI));

  Result.AppendChild(AddNode(tcDat, '#04', 'dFim', 10, 10, 1,
                                                          duto.dFim, DSC_DFIM));

  Result.AppendChild(AddNode(tcStr, '#05', 'classDuto', 1, 1, 0,
                                classDutoToStr(duto.classDuto), DSC_CLASSDUTO));

  Result.AppendChild(AddNode(tcStr, '#06', 'tpContratacao', 1, 1, 0,
                    tpContratacaoToStr(duto.tpContratacao), DSC_TPCONTRATACAO));

  Result.AppendChild(AddNode(tcStr, '#07', 'codPontoEntrada', 2, 20, 0,
                                    duto.codPontoEntrada, DSC_CODPONTOENTRADA));

  Result.AppendChild(AddNode(tcStr, '#08', 'codPontoSaida', 2, 20, 0,
                                        duto.codPontoSaida, DSC_CODPONTOSAIDA));

  Result.AppendChild(AddNode(tcStr, '#09', 'nContrato', 2, 20, 0,
                                                duto.nContrato, DSC_NCONTRATO));
end;

function TCTeXmlWriter.Gerar_ModalMultimodal: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('multimodal');

  Result.AppendChild(AddNode(tcStr, '#02', 'COTM', 1, 255, 1,
                                     CTe.infCTeNorm.multimodal.COTM, DSC_COTM));

  Result.AppendChild(AddNode(tcStr, '#03', 'indNegociavel', 1, 1, 1,
      indNegociavelToStr(CTe.infCTeNorm.multimodal.indNegociavel), DSC_INDNEG));

  if (VersaoDF >= ve300) and (CTe.infCTeNorm.multimodal.xSeg <> '') then
    Result.AppendChild(Gerar_MultimodalSeg(CTe.infCTeNorm.multimodal));
end;

function TCTeXmlWriter.Gerar_MultimodalSeg(
  multimodal: TMultimodal): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('seg');

  Result.AppendChild(Gerar_MultimodalInfSeg(multimodal));

  Result.AppendChild(AddNode(tcStr, '#08', 'nApol', 1, 20, 0,
                                                  multimodal.nApol, DSC_NAPOL));

  Result.AppendChild(AddNode(tcStr, '#09', 'nAver', 1, 20, 0,
                                                  multimodal.nAver, DSC_NAVER));
end;

function TCTeXmlWriter.Gerar_MultimodalInfSeg(
  multimodal: TMultimodal): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infSeg');

  Result.AppendChild(AddNode(tcStr, '#06', 'xSeg', 1, 30, 0,
                                                    multimodal.xSeg, DSC_XSEG));

  Result.AppendChild(AddNodeCNPJ('#07', multimodal.CNPJ, CODIGO_BRASIL, True));
end;

function TCTeXmlWriter.Gerar_Cobr(cobr: TCobr): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if (Trim(cobr.fat.nFat) <> '') or (cobr.fat.vOrig > 0) or
     (cobr.fat.vDesc > 0) or (cobr.fat.vLiq > 0) or (cobr.dup.Count > 0) then
  begin
    Result := FDocument.CreateElement('cobr');

    Result.AppendChild(Gerar_CobrFat(Cobr));

    nodeArray := Gerar_CobrDup(cobr);

    for i := 0 to cobr.dup.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_CobrFat(cobr: TCobr): TACBrXmlNode;
begin
  Result := nil;

  if (Trim(cobr.fat.nFat) <> '') or (cobr.fat.vOrig > 0) or
     (cobr.fat.vDesc > 0) or (cobr.fat.vLiq > 0) then
  begin
    Result := FDocument.CreateElement('fat');

    Result.AppendChild(AddNode(tcStr, '#386', 'nFat', 1, 60, 0,
                                                      cobr.fat.nFat, DSC_NFAT));
                                                      
    Result.AppendChild(AddNode(tcDe2, '#387', 'vOrig', 1, 15, 0,
                                                    cobr.fat.vOrig, DSC_VORIG));
                                                      
    Result.AppendChild(AddNode(tcDe2, '#388', 'vDesc', 1, 15, 0,
                                                    cobr.fat.vDesc, DSC_VDESC));
                                                      
    Result.AppendChild(AddNode(tcDe2, '#389', 'vLiq', 1, 15, 0,
                                                      cobr.fat.vLiq, DSC_VLIQ));
  end;
end;

function TCTeXmlWriter.Gerar_CobrDup(cobr: TCobr): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, cobr.dup.Count);

  for i := 0 to cobr.dup.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('dup');

    Result[i].AppendChild(AddNode(tcStr, '#391', 'nDup', 1, 60, 0,
                                                   cobr.dup[i].nDup, DSC_NDUP));

    Result[i].AppendChild(AddNode(tcDat, '#392', 'dVenc', 10, 10, 0,
                                                 cobr.dup[i].dVenc, DSC_DVENC));

    Result[i].AppendChild(AddNode(tcDe2, '#393', 'vDup', 1, 15, 0,
                                                   cobr.dup[i].vDup, DSC_VDUP));
  end;
end;

function TCTeXmlWriter.Gerar_InfCTeSub(infCTeSub: TInfCteSub): TACBrXmlNode;
var
  chave: string;
begin
  Result := nil;

  if CTe.Ide.tpCTe in [tcSubstituto, tcSubstCTeSimpl] then
  begin
    Result := FDocument.CreateElement('infCteSub');

    chave :=  OnlyNumber(infCTeSub.chCte);

    Result.AppendChild(AddNode(tcStr, '#395', 'chCte', 44, 44, 1,
                                                             chave, DSC_CHAVE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#395', 'chCte', DSC_REFNFE, ERR_MSG_INVALIDO);

    if (VersaoDF <= ve300) then
    begin
      if ((VersaoDF = ve200) and (trim(infCTeSub.tomaNaoICMS.refCteAnu) = '')) or
         ((VersaoDF >= ve300) and (trim(infCTeSub.refCteAnu) = '')) then
      begin
        Result.AppendChild(Gerar_TomaICMS(infCTeSub.tomaICMS));
      end
      else
      begin
        if VersaoDF = ve200 then
        begin
          Result.AppendChild(Gerar_TomaNaoICMS(infCTeSub.tomaNaoICMS));
        end
        else
        begin
          chave :=  OnlyNumber(infCTeSub.refCteAnu);

          Result.AppendChild(AddNode(tcStr, '#372', 'refCteAnu', 44, 44, 1,
                                                             chave, DSC_CHAVE));

          if chave <> '' then
            if not ValidarChave(chave) then
              wAlerta('#372', 'refCteAnu', DSC_REFNFE, ERR_MSG_INVALIDO);
        end;
      end;
    end;

    if (VersaoDF >= ve300) and (infCteSub.indAlteraToma = tiSim) then
      Result.AppendChild(AddNode(tcStr, '#385', 'indAlteraToma', 1, 1, 0,
                  TindicadorToStr(infCteSub.indAlteraToma), DSC_INDALTERATOMA));
  end;
end;

function TCTeXmlWriter.Gerar_TomaICMS(tomaICMS: TTomaICMS): TACBrXmlNode;
var
  chave: string;
begin
  Result := FDocument.CreateElement('tomaICMS');

  if (trim(tomaICMS.refNFe) <> '') then
  begin
    chave :=  OnlyNumber(tomaICMS.refNFe);

    Result.AppendChild(AddNode(tcStr, '#397', 'refNFe', 44, 44, 1,
                                                             chave, DSC_CHAVE));

    if chave <> '' then
      if not ValidarChave(chave) then
        wAlerta('#397', 'refNFe', DSC_REFNFE, ERR_MSG_INVALIDO);
  end
  else
  begin
    if (trim(tomaICMS.refNF.CNPJCPF) <> '') then
    begin
      Result.AppendChild(Gerar_RefNF(tomaICMS.refNF));
    end
    else
    begin
      chave :=  OnlyNumber(tomaICMS.refCte);

      Result.AppendChild(AddNode(tcStr, '#407', 'refCte', 44, 44, 1,
                                                             chave, DSC_CHAVE));

      if chave <> '' then
        if not ValidarChave(chave) then
          wAlerta('#407', 'refCte', DSC_REFNFE, ERR_MSG_INVALIDO);
    end;
  end;
end;

function TCTeXmlWriter.Gerar_RefNF(refNF: TRefNF): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('refNF');

  Result.AppendChild(AddNodeCNPJCPF('#399', '#400', refNF.CNPJCPF));

  Result.AppendChild(AddNode(tcStr, '#401', 'mod', 2, 2, 1,
                                                        refNF.modelo, DSC_MOD));

  Result.AppendChild(AddNode(tcInt, '#402', 'serie', 1, 3, 1,
                                                       refNF.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#403', 'subserie', 1, 3, 0,
                                                    refNF.subserie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#404', 'nro', 1, 6, 1,
                                                           refNF.nro, DSC_NDF));

  Result.AppendChild(AddNode(tcDe2, '#405', 'valor', 1, 15, 1,
                                                        refNF.valor, DSC_VDOC));

  Result.AppendChild(AddNode(tcDat, '#406', 'dEmi', 10, 10, 1,
                                                         refNF.dEmi, DSC_DEMI));
end;

function TCTeXmlWriter.Gerar_TomaNaoICMS(
  tomaNaoICMS: TTomaNaoICMS): TACBrXmlNode;
var
  chave: string;
begin
  Result := FDocument.CreateElement('tomaNaoICMS');

  chave :=  OnlyNumber(tomaNaoICMS.refCteAnu);

  Result.AppendChild(AddNode(tcStr, '#409', 'refCteAnu', 44, 44, 1,
                                                         chave, DSC_CHAVE));

  if chave <> '' then
    if not ValidarChave(chave) then
      wAlerta('#409', 'refCteAnu', DSC_REFNFE, ERR_MSG_INVALIDO);
end;

function TCTeXmlWriter.Gerar_Total: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('total');

  Result.AppendChild(AddNode(tcDe2, '#209', 'vTPrest', 1, 15, 1,
                                               CTe.total.vTPrest, DSC_VTPREST));

  Result.AppendChild(AddNode(tcDe2, '#210', 'vTRec', 1, 15, 1,
                                                    CTe.total.vTRec, DSC_VREC));
end;

function TCTeXmlWriter.Gerar_autXML: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, CTe.autXML.Count);

  for i := 0 to CTe.autXML.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('autXML');
    Result[i].AppendChild(AddNodeCNPJCPF('F02', 'F03', CTe.autXML[i].CNPJCPF));
  end;

  if CTe.autXML.Count > 10 then
    wAlerta('F01', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TCTeXmlWriter.Gerar_infRespTec: TACBrXmlNode;
begin
  Result := nil;

  if (CTe.infRespTec.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('infRespTec');

    Result.AppendChild(AddNodeCNPJ('#82',
                                     CTe.infRespTec.CNPJ, CODIGO_BRASIL, True));

    Result.AppendChild(AddNode(tcStr, '#083', 'xContato', 2, 60, 1,
                                        CTe.infRespTec.xContato, DSC_XCONTATO));

    Result.AppendChild(AddNode(tcStr, '#084', 'email', 6, 60, 1,
                                              CTe.infRespTec.email, DSC_EMAIL));

    Result.AppendChild(AddNode(tcStr, '#085', 'fone', 7, 12, 1,
                                                CTe.infRespTec.fone, DSC_FONE));

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Result.AppendChild(AddNode(tcInt, '#086', 'idCSRT', 3, 3, 1,
                                                           idCSRT, DSC_IDCSRT));

      Result.AppendChild(AddNode(tcStr, '#087', 'hashCSRT', 28, 28, 1,
                               CalcularHashCSRT(CSRT, FChaveCTe), DSC_HASHCSRT));
    end;
  end;
end;

function TCTeXmlWriter.GerarXml: boolean;
var
  Gerar, Ok: boolean;
  xCNPJCPF: string;
  CTeNode, xmlNode: TACBrXmlNode;
begin
  Result := False;

  ListaDeAlertas.Clear;

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
  ModeloDF := StrToModeloCTe(Ok, IntToStr(CTe.ide.modelo));
  VersaoDF :=  DblToVersaoCTe(Ok, CTe.infCTe.Versao);

  if CTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
    ModeloDF := moCTeSimp;
{
  CTe.Ide.modelo := StrToInt(ModeloCTeToStr(ModeloDF));
  CTe.infCTe.Versao := VersaoCTeToDbl(VersaoDF);
  CTe.Ide.tpAmb := TpcnTipoAmbiente(tpAmb);
  CTe.ide.tpEmis := tpEmis;
}
  xCNPJCPF := CTe.emit.CNPJ;

  FChaveCTe := GerarChaveAcesso(CTe.ide.cUF, CTe.ide.dhEmi, CTe.emit.CNPJ, CTe.ide.serie,
                            CTe.ide.nCT, StrToInt(TpEmisToStr(CTe.ide.tpEmis)),
                            CTe.ide.cCT, CTe.ide.modelo);

  CTe.infCTe.ID := 'CTe' + FChaveCTe;
  CTe.ide.cDV := ExtrairDigitoChaveAcesso(CTe.infCTe.ID);
  CTe.Ide.cCT := ExtrairCodigoChaveAcesso(CTe.infCTe.ID);

  FDocument.Clear();

  case ModeloDF of
    moGTVe:
      begin
        CTeNode := FDocument.CreateElement('GTVe', ACBRCTe_NAMESPACE);
        CTeNode.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));
      end;

    moCTeOS:
      begin
        CTeNode := FDocument.CreateElement('CTeOS', ACBRCTe_NAMESPACE);
        CTeNode.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));
      end;

    moCTeSimp:
      CTeNode := FDocument.CreateElement('CTeSimp', ACBRCTe_NAMESPACE);
  else
    CTeNode := FDocument.CreateElement('CTe', ACBRCTe_NAMESPACE);
  end;

  if CTe.procCTe.nProt <> '' then
  begin
    case ModeloDF of
      moGTVe:
        begin
          xmlNode := FDocument.CreateElement('GTVeProc', ACBRCTe_NAMESPACE);
          xmlNode.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));
        end;

      moCTeOS:
        begin
          xmlNode := FDocument.CreateElement('cteOSProc', ACBRCTe_NAMESPACE);
          xmlNode.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));
        end;

      moCTeSimp:
        begin
          xmlNode := FDocument.CreateElement('cteSimpProc', ACBRCTe_NAMESPACE);
          xmlNode.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));
        end;
    else
      begin
        xmlNode := FDocument.CreateElement('cteProc', ACBRCTe_NAMESPACE);
        xmlNode.SetAttribute('versao', FloatToString(CTe.infCTe.Versao, '.', '#0.00'));
      end;
    end;

    xmlNode.AppendChild(CTeNode);
    FDocument.Root := xmlNode;
  end
  else
    FDocument.Root := CTeNode;

  xmlNode := Gerar_InfCTe;
  CTeNode.AppendChild(xmlNode);

  if CTe.infCTeSupl.qrCodCTe <> '' then
  begin
    xmlNode := CTeNode.AddChild('infCTeSupl');
    xmlNode.AppendChild(AddNode(tcStr, 'ZX02', 'qrCodCTe', 50, 1000, 1,
                       '<![CDATA[' + CTe.infCTeSupl.qrCodCTe + ']]>', DSC_INFQRCODCTE, False));
  end;

  Gerar := (Opcoes.GerarTagAssinatura = taSempre) or
    (
      (Opcoes.GerarTagAssinatura = taSomenteSeAssinada) and
        (CTe.signature.DigestValue <> '') and
        (CTe.signature.SignatureValue <> '') and
        (CTe.signature.X509Certificate <> '')
    ) or
    (
      (Opcoes.GerarTagAssinatura = taSomenteParaNaoAssinada) and
        (CTe.signature.DigestValue = '') and
        (CTe.signature.SignatureValue = '') and
        (CTe.signature.X509Certificate = '')
    );

  if Gerar then
  begin
    FCTe.signature.URI := '#CTe' + OnlyNumber(CTe.infCTe.ID);
    xmlNode := GerarSignature(FCTe.signature);
    CTeNode.AppendChild(xmlNode);
  end;

  if CTe.procCTe.nProt <> '' then
  begin
    xmlNode := Gerar_ProtCTe;
    FDocument.Root.AppendChild(xmlNode);
  end;
end;

// Reforma Tributária
function TCTeXmlWriter.Gerar_IBSCBS(IBSCBS: TIBSCBS): TACBrXmlNode;
begin
  Result := nil;

  if (IBSCBS.gIBSCBS.vBC > 0) then
  begin
    Result := FDocument.CreateElement('IBSCBS');

    Result.AppendChild(AddNode(tcStr, '#1', 'CST', 3, 3, 1,
                                          CSTIBSCBSToStr(IBSCBS.CST), DSC_CST));

    Result.AppendChild(AddNode(tcStr, '#2', 'cClassTrib', 6, 6, 1,
                           cClassTribToStr(IBSCBS.cClassTrib), DSC_CCLASSTRIB));

    if IBSCBS.gIBSCBS.vBC > 0 then
      Result.AppendChild(Gerar_IBSCBS_gIBSCBS(IBSCBS.gIBSCBS));
  end;
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS(gIBSCBS: TgIBSCBS): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gIBSCBS');


  Result.AppendChild(AddNode(tcDe2, '#4', 'vBC', 1, 15, 1,
                                                         gIBSCBS.vBC, DSC_VBC));

  Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUF(gIBSCBS.gIBSUF));
  Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSMun(gIBSCBS.gIBSMun));
  Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gCBS(gIBSCBS.gCBS));

  if gIBSCBS.gTribRegular.pAliqEfetRegIBSUF > 0 then
    Result.AppendChild(Gerar_IBSCBSSel_gIBSCBS_gTribRegular(gIBSCBS.gTribRegular));

  if gIBSCBS.gIBSCredPres.pCredPres > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(gIBSCBS.gIBSCredPres, 'gIBSCredPres'));

  if gIBSCBS.gCBSCredPres.pCredPres > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(gIBSCBS.gCBSCredPres, 'gCBSCredPres'));

  if gIBSCBS.gTribCompraGov.pAliqIBSUF > 0 then
    Result.AppendChild(Gerar_gTribCompraGov(gIBSCBS.gTribCompraGov));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSUF(
  gIBSUF: TgIBSUFValores): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gIBSUF');

  Result.AppendChild(AddNode(tcDe4, '#6', 'pIBSUF', 1, 7, 1,
                                                      gIBSUF.pIBS, DSC_PIBSUF));

  if gIBSUF.gDif.pDif > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUF_gDif(gIBSUF.gDif));

  if gIBSUF.gDevTrib.vDevTrib > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(gIBSUF.gDevTrib));

  if gIBSUF.gRed.pRedAliq > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(gIBSUF.gRed));

  Result.AppendChild(AddNode(tcDe2, '#23', 'vIBSUF', 1, 15, 1,
                                                      gIBSUF.vIBS, DSC_VIBSUF));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSUF_gDif(
  Dif: TgDif): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDif');

  Result.AppendChild(AddNode(tcDe4, '#10', 'pDif', 1, 7, 1,
                                                           Dif.pDif, DSC_PDIF));

  Result.AppendChild(AddNode(tcDe2, '#11', 'vDif', 1, 15, 1,
                                                           Dif.vDif, DSC_VDIF));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(
  DevTrib: TgDevTrib): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDevTrib');

  Result.AppendChild(AddNode(tcDe2, '#13', 'vDevTrib', 1, 15, 1,
                                               DevTrib.vDevTrib, DSC_VDEVTRIB));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(
  Red: TgRed): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gRed');

  Result.AppendChild(AddNode(tcDe4, '#15', 'pRedAliq', 1, 7, 1,
                                                   Red.pRedAliq, DSC_PREDALIQ));

  Result.AppendChild(AddNode(tcDe2, '#16', 'pAliqEfet', 1, 7, 1,
                                                 Red.pAliqEfet, DSC_PALIQEFET));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSMun(
  gIBSMun: TgIBSMunValores): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gIBSMun');

  Result.AppendChild(AddNode(tcDe4, '#6', 'pIBSMun', 1, 7, 1,
                                                    gIBSMun.pIBS, DSC_PIBSMUN));

  if gIBSMun.gDif.pDif > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSMun_gDif(gIBSMun.gDif));

  if gIBSMun.gDevTrib.vDevTrib > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(gIBSMun.gDevTrib));

  if gIBSMun.gRed.pRedAliq > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(gIBSMun.gRed));

  Result.AppendChild(AddNode(tcDe2, '#23', 'vIBSMun', 1, 15, 1,
                                                    gIBSMun.vIBS, DSC_VIBSMUN));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSMun_gDif(
  Dif: TgDif): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDif');

  Result.AppendChild(AddNode(tcDe4, '#10', 'pDif', 1, 7, 1,
                                                           Dif.pDif, DSC_PDIF));

  Result.AppendChild(AddNode(tcDe2, '#11', 'vDif', 1, 15, 1,
                                                           Dif.vDif, DSC_VDIF));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gCBS(
  gCBS: TgCBSValores): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gCBS');

  Result.AppendChild(AddNode(tcDe4, '#44', 'pCBS', 1, 7, 1,
                                                          gCBS.pCBS, DSC_PCBS));

  if gCBS.gDif.pDif > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gCBS_gDif(gCBS.gDif));

  if gCBS.gDevTrib.vDevTrib > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(gCBS.gDevTrib));

  if gCBS.gRed.pRedAliq > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(gCBS.gRed));

  Result.AppendChild(AddNode(tcDe2, '#61', 'vCBS', 1, 15, 1,
                                                          gCBS.vCBS, DSC_VCBS));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gCBS_gDif(
  Dif: TgDif): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDif');

  Result.AppendChild(AddNode(tcDe4, '#10', 'pDif', 1, 7, 1,
                                                           Dif.pDif, DSC_PDIF));

  Result.AppendChild(AddNode(tcDe2, '#11', 'vDif', 1, 15, 1,
                                                           Dif.vDif, DSC_VDIF));
end;

function TCTeXmlWriter.Gerar_IBSCBSSel_gIBSCBS_gTribRegular(
  gTribRegular: TgTribRegular): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gTribRegular');

  Result.AppendChild(AddNode(tcStr, '#56', 'CSTReg', 3, 3, 1,
                                 CSTIBSCBSToStr(gTribRegular.CSTReg), DSC_CST));

  Result.AppendChild(AddNode(tcStr, '#57', 'cClassTribReg', 6, 6, 1,
                  cClassTribToStr(gTribRegular.cClassTribReg), DSC_CCLASSTRIB));

  Result.AppendChild(AddNode(tcDe4, '#58', 'pAliqEfetRegIBSUF', 1, 7, 1,
                                    gTribRegular.pAliqEfetRegIBSUF, DSC_PALIQ));

  Result.AppendChild(AddNode(tcDe2, '#59', 'vTribRegIBSUF', 1, 15, 1,
                                     gTribRegular.vTribRegIBSUF, DSC_VTRIBREG));

  Result.AppendChild(AddNode(tcDe4, '#60', 'pAliqEfetRegIBSMun', 1, 7, 1,
                                   gTribRegular.pAliqEfetRegIBSMun, DSC_PALIQ));

  Result.AppendChild(AddNode(tcDe2, '#61', 'vTribRegIBSMun', 1, 15, 1,
                                    gTribRegular.vTribRegIBSMun, DSC_VTRIBREG));

  Result.AppendChild(AddNode(tcDe4, '#62', 'pAliqEfetRegCBS', 1, 7, 1,
                                      gTribRegular.pAliqEfetRegCBS, DSC_PALIQ));

  Result.AppendChild(AddNode(tcDe2, '#63', 'vTribRegCBS', 1, 15, 1,
                                       gTribRegular.vTribRegCBS, DSC_VTRIBREG));
end;

function TCTeXmlWriter.Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(
  gIBSCredPres: TgIBSCBSCredPres; const Grupo: string): TACBrXmlNode;
begin
  Result := FDocument.CreateElement(Grupo);

  Result.AppendChild(AddNode(tcStr, 'UB56', 'cCredPres', 2, 2, 1,
                        cCredPresToStr(gIBSCredPres.cCredPres), DSC_CCREDPRES));

  Result.AppendChild(AddNode(tcDe4, 'UB57', 'pCredPres', 1, 7, 1,
                                        gIBSCredPres.pCredPres, DSC_PCREDPRES));

  if gIBSCredPres.vCredPres > 0 then
    Result.AppendChild(AddNode(tcDe2, 'UB58', 'vCredPres', 1, 15, 1,
                                         gIBSCredPres.vCredPres, DSC_VCREDPRES))
  else
    Result.AppendChild(AddNode(tcDe2, 'UB59', 'vCredPresCondSus', 1, 15, 1,
                          gIBSCredPres.vCredPresCondSus, DSC_VCREDPRESCONDSUS));
end;

function TCTeXmlWriter.Gerar_gTribCompraGov(
  gTribCompraGov: TgTribCompraGov): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gTribCompraGov');

  Result.AppendChild(AddNode(tcDe4, '#1', 'pAliqIBSUF', 1, 7, 1,
                                    gTribCompraGov.pAliqIBSUF, DSC_PALIQIBSUF));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vTribIBSUF', 1, 15, 1,
                                    gTribCompraGov.vTribIBSUF, DSC_VTRIBIBSUF));

  Result.AppendChild(AddNode(tcDe4, '#1', 'pAliqIBSMun', 1, 7, 1,
                                  gTribCompraGov.pAliqIBSMun, DSC_PALIQIBSMUN));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vTribIBSMun', 1, 15, 1,
                                  gTribCompraGov.vTribIBSMun, DSC_VTRIBIBSMUN));

  Result.AppendChild(AddNode(tcDe4, '#1', 'pAliqCBS', 1, 7, 1,
                                        gTribCompraGov.pAliqCBS, DSC_PALIQCBS));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vTribCBS', 1, 15, 1,
                                        gTribCompraGov.vTribCBS, DSC_VTRIBCBS));
end;

end.
