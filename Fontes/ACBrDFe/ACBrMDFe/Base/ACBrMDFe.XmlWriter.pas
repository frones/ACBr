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

unit ACBrMDFe.XmlWriter;

interface

uses
  Classes, SysUtils,
  pcnConversao,
  ACBrXmlBase, ACBrXmlDocument, ACBrXmlWriter,
  pmdfeMDFe,
  pmdfeConversaoMDFe;

type
  TMDFeXmlWriterOptions = class(TACBrXmlWriterOptions)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TACBrTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
    FCamposFatObrigatorios: boolean;

  public
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TACBrTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
    property CamposFatObrigatorios: boolean read FCamposFatObrigatorios write FCamposFatObrigatorios;

  end;

  TMDFeXmlWriter = class(TACBrXmlWriter)
  private
    FMDFe: TMDFe;

    FChaveMDFe: string;

    FVersaoDF: TVersaoMDFe;
//    FModeloDF: TModeloMDFe;
    FtpAmb: TACBrTipoAmbiente;
    FtpEmis: TACBrTipoEmissao;
    FIdCSRT: integer;
    FCSRT: string;

    function Gerar_InfMDFe: TACBrXmlNode;
    function Gerar_Ide: TACBrXmlNode;
    function Gerar_InfMunCarrega: TACBrXmlNodeArray;
    function Gerar_InfPercurso: TACBrXmlNodeArray;

    function Gerar_Emit: TACBrXmlNode;
    function Gerar_EnderEmit: TACBrXmlNode;

    function Gerar_InfModal: TACBrXmlNode;

    function Gerar_ModalRodo: TACBrXmlNode;
    function Gerar_infANTT: TACBrXmlNode;
    function Gerar_InfCIOT: TACBrXmlNodeArray;
    function Gerar_ValePedagio: TACBrXmlNode;
    function Gerar_disp_v1: TACBrXmlNodeArray;
    function Gerar_disp_v3: TACBrXmlNodeArray;
    function Gerar_InfContratante: TACBrXmlNodeArray;
    function Gerar_infContrato(Idx: Integer): TACBrXmlNode;
    function Gerar_InfPag: TACBrXmlNodeArray;
    function Gerar_Comp(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infPrazo(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infBanc(Idx: Integer): TACBrXmlNode;
    function Gerar_VeiculoTracao: TACBrXmlNode;
    function Gerar_VeiculoTracao_prop: TACBrXmlNode;
    function Gerar_condutor: TACBrXmlNodeArray;
    function Gerar_VeiculoReboq: TACBrXmlNodeArray;
    function Gerar_VeiculoReboque_prop(Idx: Integer): TACBrXmlNode;
    function Gerar_LacRodo: TACBrXmlNodeArray;

    function Gerar_ModalAereo: TACBrXmlNode;

    function Gerar_ModalAquav: TACBrXmlNode;
    function Gerar_infTermCarreg: TACBrXmlNodeArray;
    function Gerar_infTermDescarreg: TACBrXmlNodeArray;
    function Gerar_infEmbComb: TACBrXmlNodeArray;
    function Gerar_infUnidCargaVazia: TACBrXmlNodeArray;
    function Gerar_infUnidTranspVazia: TACBrXmlNodeArray;

    function Gerar_ModalFerrov: TACBrXmlNode;
    function Gerar_trem: TACBrXmlNode;
    function Gerar_Vag: TACBrXmlNodeArray;

    function Gerar_InfDoc: TACBrXmlNode;
    function Gerar_infMunDescarga: TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCTe(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCTe_infUnidTransp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCTe_peri(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infEntregaParcial(Idx1, Idx2: Integer): TACBrXmlNode;
    function Gerar_infMunDescarga_infCTe_lacUnidTransp(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCTe_infUnidCarga(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCTe_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCTe_infNFePrestParcial(Idx1, Idx2: Integer): TACBrXmlNodeArray;


    function Gerar_infMunDescarga_infCT(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCT_infUnidTransp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCT_lacUnidTransp(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCT_infUnidCarga(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infCT_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;

    function Gerar_infMunDescarga_infNFe(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNFe_infUnidTransp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNFe_peri(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNFe_lacUnidTransp(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNFe_infUnidCarga(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNFe_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;

    function Gerar_infMunDescarga_infNF(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNF_infUnidTransp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNF_lacUnidTransp(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNF_infUnidCarga(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infNF_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;

    function Gerar_infMunDescarga_infMDFeTransp(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infMDFeTransp_infUnidTransp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infMDFeTransp_peri(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infMDFeTransp_lacUnidTransp(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infMDFeTransp_infUnidCarga(Idx1, Idx2, Idx3: Integer): TACBrXmlNodeArray;
    function Gerar_infMunDescarga_infMDFeTransp_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;

    {
    }

    function Gerar_Seg: TACBrXmlNodeArray;
    function Gerar_Seg_infResp(Idx: Integer): TACBrXmlNode;
    function Gerar_Seg_infSeg(Idx: Integer): TACBrXmlNode;



    function Gerar_ProdPred: TACBrXmlNode;
    function Gerar_infLotacao: TACBrXmlNode;
    function Gerar_infLocalCarrega: TACBrXmlNode;
    function Gerar_infLocalDescarrega: TACBrXmlNode;

    function Gerar_Total: TACBrXmlNode;
    function Gerar_Lacres: TACBrXmlNodeArray;
    function Gerar_autXML: TACBrXmlNodeArray;
    function Gerar_InfAdic: TACBrXmlNode;
    function Gerar_InfRespTec: TACBrXmlNode;
    function Gerar_ProtMDFe: TACBrXmlNode;

    function GetOpcoes: TMDFeXmlWriterOptions;
    procedure SetOpcoes(AValue: TMDFeXmlWriterOptions);

    procedure AjustarMunicipioUF(out xUF: string; out xMun: string;
      out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

  public
    constructor Create(AOwner: TMDFe); reintroduce;
    destructor Destroy; override;

    function GerarXml: boolean; override;
    function ObterNomeArquivo: string; overload;

    property Opcoes: TMDFeXmlWriterOptions read GetOpcoes write SetOpcoes;
    property MDFe: TMDFe read FMDFe write FMDFe;

    property VersaoDF: TVersaoMDFe read FVersaoDF write FVersaoDF;
//    property ModeloDF: TModeloMDFe read FModeloDF write FModeloDF;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property tpEmis: TACBrTipoEmissao read FtpEmis write FtpEmis;
    property IdCSRT: integer read FIdCSRT write FIdCSRT;
    property CSRT: string read FCSRT write FCSRT;
  end;

implementation

uses
  variants,
  dateutils,
  StrUtils,
  Math,
  ACBrDFeConsts,
  pmdfeConsts,
  ACBrValidador,
  ACBrDFeUtil,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

constructor TMDFeXmlWriter.Create(AOwner: TMDFe);
begin
  inherited Create;

  TMDFeXmlWriterOptions(Opcoes).AjustarTagNro := True;
  TMDFeXmlWriterOptions(Opcoes).GerarTagIPIparaNaoTributado := True;
  TMDFeXmlWriterOptions(Opcoes).NormatizarMunicipios := False;
  TMDFeXmlWriterOptions(Opcoes).PathArquivoMunicipios := '';
  TMDFeXmlWriterOptions(Opcoes).GerarTagAssinatura := taSomenteSeAssinada;
  TMDFeXmlWriterOptions(Opcoes).ValidarInscricoes := False;
  TMDFeXmlWriterOptions(Opcoes).ValidarListaServicos := False;
  TMDFeXmlWriterOptions(Opcoes).CamposFatObrigatorios := True;

  FMDFe := AOwner;
end;

destructor TMDFeXmlWriter.Destroy;
begin
  inherited Destroy;
end;

function TMDFeXmlWriter.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TMDFeXmlWriterOptions.Create();
end;

function TMDFeXmlWriter.GetOpcoes: TMDFeXmlWriterOptions;
begin
  Result := TMDFeXmlWriterOptions(FOpcoes);
end;

procedure TMDFeXmlWriter.SetOpcoes(AValue: TMDFeXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

procedure TMDFeXmlWriter.AjustarMunicipioUF(out xUF: string; out xMun: string;
  out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
var
  PaisBrasil: boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;
  cMun := IfThen(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IfThen(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF := IfThen(PaisBrasil, vxUF, UF_EXTERIOR);

  if Opcoes.NormatizarMunicipios then
    if ((EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR)) then
      cMun := ObterCodigoMunicipio(xMun, xUF, Opcoes.FPathArquivoMunicipios)
    else if ((EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR)) then
      xMun := ObterNomeMunicipio(cMun, xUF, Opcoes.FPathArquivoMunicipios);

end;

function TMDFeXmlWriter.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FMDFe.infMDFe.ID) + '-MDFe.xml';
end;

function TMDFeXmlWriter.GerarXml: boolean;
var
  Gerar, Ok: boolean;
  MDFeNode, xmlNode: TACBrXmlNode;
begin
  Result := False;

  ListaDeAlertas.Clear;

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
{
  MDFe.Ide.modelo := StrToInt(ModeloMDFeToStr(ModeloDF));
  MDFe.infMDFe.Versao := VersaoMDFeToDbl(VersaoDF);
  MDFe.Ide.tpAmb := tpAmb;
  MDFe.ide.tpEmis := tpEmis;
}
  VersaoDF :=  DblToVersaoMDFe(Ok, MDFe.infMDFe.Versao);

  FChaveMDFe := GerarChaveAcesso(MDFe.ide.cUF, MDFe.ide.dhEmi, MDFe.emit.CNPJCPF,
      MDFe.ide.serie, MDFe.ide.nMDF, StrToInt(TpEmisToStr(MDFe.ide.tpEmis)),
      MDFe.ide.cMDF, StrToInt(MDFe.ide.modelo));

  MDFe.infMDFe.ID := 'MDFe' + FChaveMDFe;
  MDFe.ide.cDV := ExtrairDigitoChaveAcesso(MDFe.infMDFe.ID);
  MDFe.Ide.cMDF := ExtrairCodigoChaveAcesso(MDFe.infMDFe.ID);

  FDocument.Clear();
  MDFeNode := FDocument.CreateElement('MDFe', 'http://www.portalfiscal.inf.br/mdfe');

  if MDFe.procMDFe.nProt <> '' then
  begin
    xmlNode := FDocument.CreateElement('MDFeProc', 'http://www.portalfiscal.inf.br/mdfe');
    xmlNode.SetAttribute('versao', FloatToString(MDFe.infMDFe.Versao, '.', '#0.00'));
    xmlNode.AppendChild(MDFeNode);
    FDocument.Root := xmlNode;
  end
  else
  begin
    FDocument.Root := MDFeNode;
  end;

  xmlNode := Gerar_InfMDFe();
  MDFeNode.AppendChild(xmlNode);

  if MDFe.infMDFeSupl.qrCodMDFe <> '' then
  begin
    xmlNode := MDFeNode.AddChild('infMDFeSupl');
    xmlNode.AppendChild(AddNode(tcStr, '#318', 'qrCodMDFe', 50, 1000, 1,
        '<![CDATA[' + MDFe.infMDFeSupl.qrCodMDFe + ']]>', DSC_QRCODDFe, False));
  end;

  if Opcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := True;
    if Opcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((MDFe.signature.DigestValue <> '') and
                (MDFe.signature.SignatureValue <> '') and
                (MDFe.signature.X509Certificate <> ''));

    if Opcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
       Gerar := ((MDFe.signature.DigestValue = '') and
                 (MDFe.signature.SignatureValue = '') and
                 (MDFe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FMDFe.signature.URI := '#MDFe' + OnlyNumber(MDFe.infMDFe.ID);
      xmlNode := GerarSignature(FMDFe.signature);
      MDFeNode.AppendChild(xmlNode);
    end;
  end;

  if MDFe.procMDFe.nProt <> '' then
  begin
    xmlNode := Gerar_ProtMDFe;
    FDocument.Root.AppendChild(xmlNode);
  end;
end;

function TMDFeXmlWriter.Gerar_InfMDFe: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infMDFe');

  Result.SetAttribute('Id', MDFe.infMDFe.ID);
  Result.SetAttribute('versao', FloatToString(MDFe.infMDFe.Versao, '.', '#0.00'));

  Result.AppendChild(Gerar_Ide);
  Result.AppendChild(Gerar_Emit);
  Result.AppendChild(Gerar_InfModal);
  Result.AppendChild(Gerar_InfDoc);

  if VersaoDF >= ve300 then
  begin
    nodeArray := Gerar_Seg;
    for i := 0 to MDFe.seg.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    if MDFe.prodPred.xProd <> '' then
      Result.AppendChild(Gerar_ProdPred);
  end;

  Result.AppendChild(Gerar_Total);

  nodeArray := Gerar_Lacres;
  for i := 0 to MDFe.lacres.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_autXML;
  for i := 0 to MDFe.autXML.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_InfAdic);

  if VersaoDF >= ve300 then
    Result.AppendChild(Gerar_InfRespTec);
end;

function TMDFeXmlWriter.Gerar_Ide: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('ide');

  Result.AppendChild(AddNode(tcInt, '#5', 'cUF', 2, 2, 1,
                                                        MDFe.ide.cUF, DSC_CUF));

  if not ValidarCodigoUF(MDFe.ide.cUF) then
    wAlerta('#5', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#6', 'tpAmb  ', 1, 1, 1,
                                        TpAmbToStr(MDFe.Ide.tpAmb), DSC_TPAMB));

  Result.AppendChild(AddNode(tcStr, '#7', 'tpEmit  ', 1, 1, 1,
                                 TpEmitenteToStr(MDFe.Ide.tpEmit), DSC_TPEMIT));

  if (VersaoDF >= ve300) and (MDFe.Ide.tpTransp <> ttNenhum) then
    Result.AppendChild(AddNode(tcStr, '#7', 'tpTransp  ', 1, 1, 1,
                         TTransportadorToStr(MDFe.Ide.tpTransp), DSC_TPTRANSP));

  Result.AppendChild(AddNode(tcInt, '#7', 'mod', 2, 2, 1,
                                                     MDFe.ide.modelo, DSC_MOD));

  Result.AppendChild(AddNode(tcInt, '#8', 'serie', 1, 3, 1,
                                                    MDFe.ide.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#9', 'nMDF', 1, 9, 1,
                                                       MDFe.ide.nMDF, DSC_NDF));

  Result.AppendChild(AddNode(tcInt, '#10', 'cMDF', 8, 8, 1,
                                                       MDFe.Ide.cMDF, DSC_CDF));

  Result.AppendChild(AddNode(tcInt, '#11', 'cDV', 1, 1, 1,
                                                        MDFe.Ide.cDV, DSC_CDV));

  Result.AppendChild(AddNode(tcStr, '#12', 'modal', 1, 1, 1,
                                        ModalToStr(MDFe.Ide.modal), DSC_MODAL));

  if VersaoDF = ve100 then
    Result.AppendChild(AddNode(tcStr, '#13', 'dhEmi', 19, 19, 1,
                                                      MDFe.ide.dhEmi, DSC_DEMI))
  else
    Result.AppendChild(AddNode(tcStr, '#13', 'dhEmi', 25, 25, 1,
    DateTimeTodh(MDFe.ide.dhEmi) + GetUTC(CodigoUFparaUF(MDFe.ide.cUF),
      MDFe.ide.dhEmi), DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, '#14', 'tpEmis', 1, 1, 1,
                                     tpEmisToStr(MDFe.Ide.tpEmis), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcStr, '#16', 'procEmi', 1, 1, 1,
                                  procEmiToStr(MDFe.Ide.procEmi), DSC_PROCEMI));

  Result.AppendChild(AddNode(tcStr, '#17', 'verProc', 1, 20, 1,
                                                MDFe.Ide.verProc, DSC_VERPROC));

  Result.AppendChild(AddNode(tcStr, '#18', 'UFIni', 2, 2, 1,
                                                       MDFe.Ide.UFIni, DSC_UF));

  if not ValidarUF(MDFe.ide.UFIni) then
    wAlerta('#018', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#19', 'UFFim', 2, 2, 1,
                                                       MDFe.Ide.UFFim, DSC_UF));

  if not ValidarUF(MDFe.ide.UFFim) then
    wAlerta('#019', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);

  nodeArray := Gerar_InfMunCarrega;
  for i := 0 to MDFe.Ide.infMunCarrega.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_InfPercurso;
  for i := 0 to MDFe.Ide.infPercurso.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  if MDFe.ide.dhIniViagem > 0 then
  begin
    if VersaoDF = ve100 then
      Result.AppendChild(AddNode(tcStr, '#024a', 'dhIniViagem', 19, 19, 1,
                                         MDFe.ide.dhIniViagem, DSC_DHINIVIAGEM))
    else
      Result.AppendChild(AddNode(tcStr, '#024a', 'dhIniViagem', 25, 25, 1,
      DateTimeTodh(MDFe.ide.dhIniViagem) + GetUTC(CodigoUFparaUF(MDFe.ide.cUF),
        MDFe.ide.dhIniViagem), DSC_DHINIVIAGEM));
  end;

  if (MDFe.infMDFe.versao >= 3) and (MDFe.ide.indCanalVerde = tiSim) then
    Result.AppendChild(AddNode(tcStr, '#27', 'indCanalVerde', 1, 1, 0,
                   TindicadorToStr(MDFe.Ide.indCanalVerde), DSC_INDCANALVERDE));

  if (MDFe.infMDFe.versao >= 3) and (MDFe.ide.indCarregaPosterior = tiSim) then
    Result.AppendChild(AddNode(tcStr, '#27', 'indCarregaPosterior', 1, 1, 0,
       TindicadorToStr(MDFe.Ide.indCarregaPosterior), DSC_INDCARREGAPOSTERIOR));
end;

function TMDFeXmlWriter.Gerar_InfMunCarrega: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.Ide.infMunCarrega.Count);

  for i := 0 to MDFe.Ide.infMunCarrega.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infMunCarrega');

    Result[i].AppendChild(AddNode(tcInt, '#21', 'cMunCarrega', 7, 7, 1,
                       MDFe.Ide.infMunCarrega[i].cMunCarrega, DSC_CMUNCARREGA));

    Result[i].AppendChild(AddNode(tcStr, '#22', 'xMunCarrega', 2, 60, 1,
                       MDFe.Ide.infMunCarrega[i].xMunCarrega, DSC_XMUNCARREGA));
  end;

  if MDFe.Ide.infMunCarrega.Count > 50 then
    wAlerta('#20', 'infMunCarrega', '', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TMDFeXmlWriter.Gerar_InfPercurso: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.Ide.infPercurso.Count);

  for i := 0 to MDFe.Ide.infPercurso.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infPercurso');

    Result[i].AppendChild(AddNode(tcStr, '#24', 'UFPer', 2, 2, 1,
                                     MDFe.Ide.infPercurso[i].UFPer, DSC_UFPER));
  end;

  if MDFe.Ide.infPercurso.Count > 25 then
    wAlerta('#24', 'infPercurso', '', ERR_MSG_MAIOR_MAXIMO + '25');
end;

function TMDFeXmlWriter.Gerar_Emit: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('emit');

  Result.AppendChild(AddNodeCNPJCPF('#26', '#26a', MDFe.Emit.CNPJCPF));

  Result.AppendChild(AddNode(tcStr, '#26', 'IE', 2, 14, 1,
                                             OnlyNumber(MDFe.Emit.IE), DSC_IE));

  if (Opcoes.ValidarInscricoes) then
  begin
    if Length(MDFe.Emit.IE) = 0 then
      wAlerta('#26', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not ValidarIE(MDFe.Emit.IE, CodigoUFparaUF(MDFe.Ide.cUF)) then
        wAlerta('#26', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, '#22', 'xNome', 2, 60, 1,
                                                   MDFe.Emit.xNome, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#23', 'xFant', 1, 60, 0,
                                                   MDFe.Emit.xFant, DSC_XFANT));

  Result.AppendChild(Gerar_EnderEmit);
end;

function TMDFeXmlWriter.Gerar_EnderEmit: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, MDFe.Emit.enderEmit.UF,
    MDFe.Emit.enderEmit.xMun, MDFe.Emit.EnderEmit.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, '#34', 'xLgr', 2, 60, 1,
                                           MDFe.Emit.enderEmit.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#35', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, MDFe.Emit.enderEmit.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#36', 'xCpl', 1, 60, 0,
                                           MDFe.Emit.enderEmit.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#37', 'xBairro', 2, 60, 1,
                                     MDFe.Emit.enderEmit.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#38', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#29', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#39', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#40', 'CEP', 8, 8, 1,
                                             MDFe.Emit.enderEmit.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#41', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#32', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#42', 'fone', 7, 12, 0,
                               OnlyNumber(MDFe.Emit.enderEmit.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#43', 'email', 1, 60, 0,
                                         MDFe.Emit.enderEmit.email, DSC_EMAIL));
end;

function TMDFeXmlWriter.Gerar_InfModal: TACBrXmlNode;
var
 versao: string;
begin
  versao := GetVersaoModalMDFe(VersaoDF, MDFe.Ide.modal);

  Result := FDocument.CreateElement('infModal');

  Result.SetAttribute('versaoModal', versao);

  case StrToInt(ModalToStr(MDFe.Ide.modal)) of
   1: Result.AppendChild(Gerar_ModalRodo);
   2: Result.AppendChild(Gerar_ModalAereo);
   3: Result.AppendChild(Gerar_ModalAquav);
   4: Result.AppendChild(Gerar_ModalFerrov);
  end;
end;

function TMDFeXmlWriter.Gerar_ModalRodo: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('rodo');

  if VersaoDF = ve100 then
  begin
    Result.AppendChild(AddNode(tcStr, '#02', 'RNTRC', 8, 8, 0,
                                       OnlyNumber(MDFe.Rodo.RNTRC), DSC_RNTRC));

    Result.AppendChild(AddNode(tcStr, '#03', 'CIOT', 12, 12, 0,
                                                     MDFe.Rodo.CIOT, DSC_CIOT));
  end
  else
  begin
    if (MDFe.Rodo.infANTT.RNTRC <> '') or
       (MDFe.Rodo.infANTT.infCIOT.Count > 0) or
       (MDFe.Rodo.infANTT.valePed.disp.Count > 0) or
       (MDFe.rodo.infANTT.infContratante.Count > 0) or
       (MDFe.rodo.infANTT.infPag.Count > 0) then
    begin
      Result.AppendChild(Gerar_infANTT);
    end;
  end;

  Result.AppendChild(Gerar_VeiculoTracao);

  nodeArray := Gerar_VeiculoReboq;
  for i := 0 to MDFe.rodo.veicReboque.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  if (VersaoDF = ve100) then
    Result.AppendChild(Gerar_ValePedagio);

  Result.AppendChild(AddNode(tcStr, '#45', 'codAgPorto', 1, 16, 0,
                                         MDFe.Rodo.codAgPorto, DSC_CODAGPORTO));

  if (VersaoDF >= ve300) then
  begin
    nodeArray := Gerar_LacRodo;
    for i := 0 to MDFe.rodo.lacRodo.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TMDFeXmlWriter.Gerar_infANTT: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infANTT');

  Result.AppendChild(AddNode(tcStr, '#02', 'RNTRC', 8, 8, 0,
                               OnlyNumber(MDFe.Rodo.infANTT.RNTRC), DSC_RNTRC));

  nodeArray := Gerar_InfCIOT;
  for i := 0 to MDFe.rodo.infANTT.infCIOT.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_ValePedagio);

  nodeArray := Gerar_InfContratante;
  for i := 0 to MDFe.rodo.infANTT.infContratante.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_InfPag;
  for i := 0 to MDFe.rodo.infANTT.infPag.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TMDFeXmlWriter.Gerar_InfCIOT: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.infANTT.infCIOT.Count);

  for i := 0 to MDFe.rodo.infANTT.infCIOT.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infCIOT');

    Result[i].AppendChild(AddNode(tcStr, '#05', 'CIOT', 12, 12, 0,
                                  MDFe.Rodo.infANTT.infCIOT[i].CIOT, DSC_CIOT));

    Result[i].AppendChild(AddNodeCNPJCPF('#06', '#07',
                                         MDFe.rodo.infANTT.infCIOT[i].CNPJCPF));
  end;

  if MDFe.rodo.infANTT.infCIOT.Count > 990 then
    wAlerta('#04', 'infCIOT', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_ValePedagio: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('valePed');

  if VersaoDF = ve100 then
  begin
    nodeArray := Gerar_disp_v1;
    for i := 0 to MDFe.rodo.valePed.disp.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end
  else
  begin
    nodeArray := Gerar_disp_v3;
    for i := 0 to MDFe.rodo.infANTT.valePed.disp.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    Result.AppendChild(AddNode(tcStr, '#02', 'categCombVeic', 1, 2, 0,
      categCombVeicToStr(MDFe.Rodo.infANTT.valePed.categCombVeic), DSC_CATEGCOMBVEIC));
  end;
end;

function TMDFeXmlWriter.Gerar_disp_v1: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.valePed.disp.Count);

  for i := 0 to MDFe.rodo.valePed.disp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('disp');

    Result[i].AppendChild(AddNode(tcStr, '#25', 'CNPJForn', 14, 14, 1,
                             MDFe.Rodo.valePed.disp[i].CNPJForn, DSC_CNPJFORN));

    Result[i].AppendChild(AddNode(tcStr, '#26', 'CNPJPg', 14, 14, 0,
                                 MDFe.Rodo.valePed.disp[i].CNPJPg, DSC_CNPJPG));

    Result[i].AppendChild(AddNode(tcStr, '#27', 'nCompra', 1, 20, 0,
                               MDFe.Rodo.valePed.disp[i].nCompra, DSC_NCOMPRA));
  end;

  if MDFe.rodo.valePed.disp.Count > 990 then
    wAlerta('#24', 'disp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_disp_v3: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.infANTT.valePed.disp.Count);

  for i := 0 to MDFe.rodo.infANTT.valePed.disp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('disp');

    Result[i].AppendChild(AddNode(tcStr, '#25', 'CNPJForn', 14, 14, 1,
                     MDFe.Rodo.infANTT.valePed.disp[i].CNPJForn, DSC_CNPJFORN));

    if Length(MDFe.Rodo.infANTT.valePed.disp[i].CNPJPg) = 14 then
      Result[i].AppendChild(AddNode(tcStr, '#26', 'CNPJPg', 14, 14, 0,
                          MDFe.Rodo.infANTT.valePed.disp[i].CNPJPg, DSC_CNPJPG))
    else
      Result[i].AppendChild(AddNode(tcStr, '#26', 'CPFPg', 11, 11, 0,
                         MDFe.Rodo.infANTT.valePed.disp[i].CNPJPg, DSC_CNPJPG));

    Result[i].AppendChild(AddNode(tcStr, '#27', 'nCompra', 1, 20, 0,
                       MDFe.Rodo.infANTT.valePed.disp[i].nCompra, DSC_NCOMPRA));

    Result[i].AppendChild(AddNode(tcDe2, '#28', 'vValePed', 1, 15, 1,
                     MDFe.Rodo.infANTT.valePed.disp[i].vValePed, DSC_VVALEPED));

    Result[i].AppendChild(AddNode(tcStr, '#27', 'tpValePed', 1, 2, 0,
      tpValePedToStr(MDFe.Rodo.infANTT.valePed.disp[i].tpValePed), DSC_TPVALEPED));
  end;

  if MDFe.rodo.infANTT.valePed.disp.Count > 990 then
    wAlerta('#24', 'disp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_InfContratante: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.infANTT.infContratante.Count);

  for i := 0 to MDFe.rodo.infANTT.infContratante.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infContratante');

    Result[i].AppendChild(AddNode(tcStr, '#25', 'xNome', 2, 60, 0,
                         MDFe.rodo.infANTT.infContratante[i].xNome, DSC_XNOME));

    if MDFe.rodo.infANTT.infContratante[i].idEstrangeiro <> '' then
      Result[i].AppendChild(AddNode(tcStr, '#26', 'idEstrangeiro', 2, 20, 0,
          MDFe.rodo.infANTT.infContratante[i].idEstrangeiro, DSC_IDESTRANGEIRO))
    else
      Result[i].AppendChild(AddNodeCNPJCPF('#26', '#26a',
                                  MDFe.rodo.infANTT.infContratante[i].CNPJCPF));

    if (MDFe.rodo.infANTT.infContratante[i].infContrato.NroContrato <> '') and
       (MDFe.rodo.infANTT.infContratante[i].infContrato.vContratoGlobal <> 0) then
    begin
      Result[i].AppendChild(Gerar_infContrato(i));
    end;
  end;

  if MDFe.rodo.infANTT.infContratante.Count > 990 then
    wAlerta('#15', 'infContratante', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infContrato(Idx: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infContrato');

  Result.AppendChild(AddNode(tcStr, '#23', 'NroContrato', 1, 20, 1,
    MDFe.rodo.infANTT.infContratante[Idx].infContrato.NroContrato, DSC_NROCONTRATO));

  Result.AppendChild(AddNode(tcDe2, '#24', 'vContratoGlobal', 1, 15, 1,
    MDFe.rodo.infANTT.infContratante[Idx].infContrato.vContratoGlobal, DSC_VCONTRATOGLOBAL));
end;

function TMDFeXmlWriter.Gerar_InfPag: TACBrXmlNodeArray;
var
  i, j: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.infANTT.infPag.Count);

  for i := 0 to MDFe.rodo.infANTT.infPag.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infPag');

    Result[i].AppendChild(AddNode(tcStr, '#25', 'xNome', 2, 60, 0,
                                 MDFe.rodo.infANTT.infPag[i].xNome, DSC_XNOME));

    if MDFe.rodo.infANTT.infPag[i].idEstrangeiro <> '' then
      Result[i].AppendChild(AddNode(tcStr, '#26', 'idEstrangeiro', 2, 20, 0,
                  MDFe.rodo.infANTT.infPag[i].idEstrangeiro, DSC_IDESTRANGEIRO))
    else
      Result[i].AppendChild(AddNodeCNPJCPF('#26', '#26a',
                                          MDFe.rodo.infANTT.infPag[i].CNPJCPF));

    nodeArray := Gerar_Comp(i);
    for j := 0 to MDFe.rodo.infANTT.infPag[i].Comp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#25', 'vContrato', 1, 15, 1,
                         MDFe.rodo.infANTT.infPag[i].vContrato, DSC_VCONTRATO));

    Result[i].AppendChild(AddNode(tcStr, '#25', 'indAltoDesemp', 1, 1, 0,
            indAltoDesempToStr(MDFe.rodo.infANTT.infPag[i].indAltoDesemp), ''));

    Result[i].AppendChild(AddNode(tcStr, '#25', 'indPag', 1, 1, 1,
                 TIndPagToStr(MDFe.rodo.infANTT.infPag[i].indPag), DSC_INDPAG));

    Result[i].AppendChild(AddNode(tcDe2, '#25', 'vAdiant', 1, 15, 0,
                             MDFe.rodo.infANTT.infPag[i].vAdiant, DSC_VADIANT));

    if MDFe.rodo.infANTT.infPag[i].indAntecipaAdiant = tiSim then
      Result[i].AppendChild(AddNode(tcStr, '#25', 'indAntecipaAdiant', 1, 1, 1,
                                                                          '1'));

    if MDFe.rodo.infANTT.infPag[i].indPag = ipPrazo then
    begin
      nodeArray := Gerar_infPrazo(i);
      for j := 0 to MDFe.rodo.infANTT.infPag[i].infPrazo.Count - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;

    Result[i].AppendChild(AddNode(tcStr, '#25', 'tpAntecip', 1, 1, 0,
         tpAntecipToStr(MDFe.rodo.infANTT.infPag[i].tpAntecip), DSC_TPANTECIP));

    Result[i].AppendChild(Gerar_infBanc(i));
  end;

  if MDFe.rodo.infANTT.infPag.Count > 990 then
    wAlerta('#15', 'infPag', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_Comp(Idx: Integer): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.infANTT.infPag[Idx].Comp.Count);

  for i := 0 to MDFe.rodo.infANTT.infPag[Idx].Comp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('Comp');

    Result[i].AppendChild(AddNode(tcStr, '#25', 'tpComp', 2, 2, 1,
         TCompToStr(MDFe.rodo.infANTT.infPag[Idx].Comp[i].tpComp), DSC_TPCOMP));

    Result[i].AppendChild(AddNode(tcDe2, '#25', 'vComp', 1, 15, 1,
                       MDFe.rodo.infANTT.infPag[Idx].Comp[i].vComp, DSC_VCOMP));

    Result[i].AppendChild(AddNode(tcStr, '#25', 'xComp', 2, 60, 0,
                       MDFe.rodo.infANTT.infPag[Idx].Comp[i].xComp, DSC_XCOMP));
  end;

  if MDFe.rodo.infANTT.infPag[Idx].Comp.Count > 990 then
    wAlerta('#15', 'Comp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infPrazo(Idx: Integer): TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.infANTT.infPag[Idx].infPrazo.Count);

  for i := 0 to MDFe.rodo.infANTT.infPag[Idx].infPrazo.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infPrazo');

    Result[i].AppendChild(AddNode(tcStr, '#25', 'nParcela', 3, 3, 1,
      FormatFloat('000', MDFe.rodo.infANTT.infPag[Idx].infPrazo[i].nParcela), DSC_NPARCELA));

    Result[i].AppendChild(AddNode(tcDat, '#25', 'dVenc', 10, 10, 1,
                   MDFe.rodo.infANTT.infPag[Idx].infPrazo[i].dVenc, DSC_DVENC));

    Result[i].AppendChild(AddNode(tcDe2, '#25', 'vParcela', 1, 15, 1,
             MDFe.rodo.infANTT.infPag[Idx].infPrazo[i].vParcela, DSC_VPARCELA));
  end;

  if MDFe.rodo.infANTT.infPag[Idx].infPrazo.Count > 990 then
    wAlerta('#15', 'infPrazo', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infBanc(Idx: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infBanc');

  if MDFe.rodo.infANTT.infPag[Idx].infBanc.PIX <> '' then
    Result.AppendChild(AddNode(tcStr, '#', 'PIX', 2, 60, 1,
                            MDFe.rodo.infANTT.infPag[Idx].infBanc.PIX, DSC_PIX))
  else
  begin
    if MDFe.rodo.infANTT.infPag[Idx].infBanc.CNPJIPEF <> '' then
      Result.AppendChild(AddNode(tcStr, '#', 'CNPJIPEF', 14, 14, 1,
                  MDFe.rodo.infANTT.infPag[Idx].infBanc.CNPJIPEF, DSC_CNPJIPEF))
    else
    begin
      Result.AppendChild(AddNode(tcStr, '#', 'codBanco', 3, 5, 1,
                 MDFe.rodo.infANTT.infPag[Idx].infBanc.codBanco, DSC_CODBANCO));

      Result.AppendChild(AddNode(tcStr, '#', 'codAgencia', 1, 10, 1,
             MDFe.rodo.infANTT.infPag[Idx].infBanc.codAgencia, DSC_CODAGENCIA));
    end;
  end;
end;

function TMDFeXmlWriter.Gerar_VeiculoTracao: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('veicTracao');

  Result.AppendChild(AddNode(tcStr, '#05', 'cInt', 1, 10, 0,
                                         MDFe.Rodo.veicTracao.cInt, DSC_CINTV));

  Result.AppendChild(AddNode(tcStr, '#06', 'placa', 1, 7, 1,
                                        MDFe.Rodo.veicTracao.placa, DSC_PLACA));

  Result.AppendChild(AddNode(tcStr, '#06a', 'RENAVAM', 9, 11, 0,
                                    MDFe.Rodo.veicTracao.RENAVAM, DSC_RENAVAM));

  Result.AppendChild(AddNode(tcInt, '#07', 'tara', 1, 6, 1,
                                          MDFe.Rodo.veicTracao.tara, DSC_TARA));

  Result.AppendChild(AddNode(tcInt, '#08', 'capKG', 1, 6, 0,
                                        MDFe.Rodo.veicTracao.capKG, DSC_CAPKG));

  Result.AppendChild(AddNode(tcInt, '#09', 'capM3', 1, 6, 0,
                                        MDFe.Rodo.veicTracao.capM3, DSC_CAPM3));

  if (MDFe.Rodo.veicTracao.Prop.CNPJCPF <> '') or
     (MDFe.Rodo.veicTracao.Prop.RNTRC <> '') or
     (MDFe.Rodo.veicTracao.Prop.xNome <> '') then
  begin
    Result.AppendChild(Gerar_VeiculoTracao_prop);
  end;

  nodeArray := Gerar_condutor;
  for i := 0 to MDFe.rodo.veicTracao.condutor.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(AddNode(tcStr, '#21', 'tpRod', 2, 2, 1,
                         TpRodadoToStr(MDFe.Rodo.veicTracao.tpRod), DSC_TPROD));

  Result.AppendChild(AddNode(tcStr, '#22', 'tpCar', 2, 2, 1,
                     TpCarroceriaToStr(MDFe.Rodo.veicTracao.tpCar), DSC_TPCAR));

  Result.AppendChild(AddNode(tcStr, '#23', 'UF', 2, 2, 0,
                                             MDFe.Rodo.veicTracao.UF, DSC_CUF));

  if MDFe.Rodo.veicTracao.UF <> '' then
    if not ValidarUF(MDFe.Rodo.veicTracao.UF) then
      wAlerta('#23', 'UF', DSC_UF, ERR_MSG_INVALIDO);
end;

function TMDFeXmlWriter.Gerar_VeiculoTracao_prop: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('prop');

  Result.AppendChild(AddNodeCNPJCPF('#11', '#12',
                                            MDFe.Rodo.veicTracao.Prop.CNPJCPF));

  Result.AppendChild(AddNode(tcStr, '#13', 'RNTRC', 8, 8, 1,
                       OnlyNumber(MDFe.Rodo.veicTracao.Prop.RNTRC), DSC_RNTRC));

  Result.AppendChild(AddNode(tcStr, '#14', 'xNome', 2, 60, 1,
                                   MDFe.Rodo.veicTracao.Prop.xNome, DSC_XNOME));

  if MDFe.Rodo.veicTracao.Prop.IE <> '' then
  begin
    if MDFe.Rodo.veicTracao.Prop.IE = 'ISENTO' then
      Result.AppendChild(AddNode(tcStr, '#15', 'IE', 0, 14, 1,
                                          MDFe.Rodo.veicTracao.Prop.IE, DSC_IE))

    else
      Result.AppendChild(AddNode(tcStr, '#15', 'IE', 2, 14, 1,
                             OnlyNumber(MDFe.Rodo.veicTracao.Prop.IE), DSC_IE));

    if (Opcoes.ValidarInscricoes) then
      if not ValidarIE(MDFe.Rodo.veicTracao.Prop.IE, MDFe.Rodo.veicTracao.Prop.UF) then
        wAlerta('#15', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  end
  else
    Result.AppendChild(AddNode(tcStr, '#15', 'IE', 0, 14, 1, ''));

  Result.AppendChild(AddNode(tcStr, '#16', 'UF', 2, 2, 1,
                                        MDFe.Rodo.veicTracao.Prop.UF, DSC_CUF));

  if not ValidarUF(MDFe.Rodo.veicTracao.Prop.UF) then
    wAlerta('#16', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#17', 'tpProp', 1, 1, 1,
                    TpPropToStr(MDFe.Rodo.veicTracao.Prop.tpProp), DSC_TPPROP));
end;

function TMDFeXmlWriter.Gerar_condutor: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.veicTracao.condutor.Count);

  for i := 0 to MDFe.rodo.veicTracao.condutor.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('condutor');

    Result[i].AppendChild(AddNode(tcStr, '#19', 'xNome', 2, 60, 1,
                            MDFe.rodo.veicTracao.condutor[i].xNome, DSC_XNOME));

    Result[i].AppendChild(AddNode(tcStr, '#20', 'CPF', 11, 11, 1,
                                MDFe.rodo.veicTracao.condutor[i].CPF, DSC_CPF));
  end;

  if MDFe.rodo.veicTracao.condutor.Count > 10 then
    wAlerta('#18', 'condutor', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TMDFeXmlWriter.Gerar_VeiculoReboq: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.veicReboque.Count);

  for i := 0 to MDFe.rodo.veicReboque.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('veicReboque');

    Result[i].AppendChild(AddNode(tcStr, '#05', 'cInt', 1, 10, 0,
                                     MDFe.Rodo.veicReboque[i].cInt, DSC_CINTV));

    Result[i].AppendChild(AddNode(tcStr, '#06', 'placa', 1, 7, 1,
                                    MDFe.Rodo.veicReboque[i].placa, DSC_PLACA));

    Result[i].AppendChild(AddNode(tcStr, '#06a', 'RENAVAM', 9, 11, 0,
                                MDFe.Rodo.veicReboque[i].RENAVAM, DSC_RENAVAM));

    Result[i].AppendChild(AddNode(tcInt, '#07', 'tara', 1, 6, 1,
                                      MDFe.Rodo.veicReboque[i].tara, DSC_TARA));

    Result[i].AppendChild(AddNode(tcInt, '#08', 'capKG', 1, 6, 0,
                                    MDFe.Rodo.veicReboque[i].capKG, DSC_CAPKG));

    Result[i].AppendChild(AddNode(tcInt, '#09', 'capM3', 1, 6, 0,
                                    MDFe.Rodo.veicReboque[i].capM3, DSC_CAPM3));

    if (MDFe.Rodo.veicReboque[i].Prop.CNPJCPF <> '') or
       (MDFe.Rodo.veicReboque[i].Prop.RNTRC <> '') or
       (MDFe.Rodo.veicReboque[i].Prop.xNome <> '') then
    begin
      Result[i].AppendChild(Gerar_VeiculoReboque_prop(i));
    end;

    Result[i].AppendChild(AddNode(tcStr, '#22', 'tpCar', 2, 2, 1,
                 TpCarroceriaToStr(MDFe.Rodo.veicReboque[i].tpCar), DSC_TPCAR));

    Result[i].AppendChild(AddNode(tcStr, '#23', 'UF', 2, 2, 0,
                                         MDFe.Rodo.veicReboque[i].UF, DSC_CUF));

    if MDFe.Rodo.veicReboque[i].UF <> '' then
      if not ValidarUF(MDFe.Rodo.veicReboque[i].UF) then
        wAlerta('#23', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  end;

  if MDFe.rodo.veicReboque.Count > 3 then
    wAlerta('#15', 'veicReboque', '', ERR_MSG_MAIOR_MAXIMO + '3');
end;

function TMDFeXmlWriter.Gerar_VeiculoReboque_prop(Idx: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('prop');

  Result.AppendChild(AddNodeCNPJCPF('#11', '#12',
                                      MDFe.Rodo.veicReboque[Idx].Prop.CNPJCPF));

  Result.AppendChild(AddNode(tcStr, '#13', 'RNTRC', 8, 8, 1,
                 OnlyNumber(MDFe.Rodo.veicReboque[Idx].Prop.RNTRC), DSC_RNTRC));

  Result.AppendChild(AddNode(tcStr, '#14', 'xNome', 2, 60, 1,
                             MDFe.Rodo.veicReboque[Idx].Prop.xNome, DSC_XNOME));

  if MDFe.Rodo.veicReboque[Idx].Prop.IE <> '' then
  begin
    if MDFe.Rodo.veicReboque[Idx].Prop.IE = 'ISENTO' then
      Result.AppendChild(AddNode(tcStr, '#15', 'IE', 0, 14, 1,
                                    MDFe.Rodo.veicReboque[Idx].Prop.IE, DSC_IE))

    else
      Result.AppendChild(AddNode(tcStr, '#15', 'IE', 2, 14, 1,
                       OnlyNumber(MDFe.Rodo.veicReboque[Idx].Prop.IE), DSC_IE));

    if (Opcoes.ValidarInscricoes) then
      if not ValidarIE(MDFe.Rodo.veicReboque[Idx].Prop.IE, MDFe.Rodo.veicReboque[Idx].Prop.UF) then
        wAlerta('#15', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  end
  else
    Result.AppendChild(AddNode(tcStr, '#15', 'IE', 0, 14, 1, ''));

  Result.AppendChild(AddNode(tcStr, '#16', 'UF', 2, 2, 1,
                                  MDFe.Rodo.veicReboque[Idx].Prop.UF, DSC_CUF));

  if not ValidarUF(MDFe.Rodo.veicReboque[Idx].Prop.UF) then
    wAlerta('#16', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#17', 'tpProp', 1, 1, 1,
              TpPropToStr(MDFe.Rodo.veicReboque[Idx].Prop.tpProp), DSC_TPPROP));
end;

function TMDFeXmlWriter.Gerar_LacRodo: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.rodo.lacRodo.Count);

  for i := 0 to MDFe.rodo.lacRodo.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacRodo');

    Result[i].AppendChild(AddNode(tcStr, '#58', 'nLacre', 1, 20, 1,
                                      MDFe.rodo.lacRodo[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.rodo.lacRodo.Count > 990 then
    wAlerta('#57', 'lacRodo', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_ModalAereo: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('aereo');

  Result.AppendChild(AddNode(tcStr, '#02', 'nac', 1, 4, 1,
                                                      MDFe.Aereo.nac, DSC_NAC));

  Result.AppendChild(AddNode(tcStr, '#03', 'matr', 1, 6, 1,
                                                    MDFe.Aereo.matr, DSC_MATR));

  Result.AppendChild(AddNode(tcStr, '#04', 'nVoo', 5, 9, 1,
                                                    MDFe.Aereo.nVoo, DSC_NVOO));

  Result.AppendChild(AddNode(tcStr, '#05', 'cAerEmb', 3, 4, 1,
                                              MDFe.Aereo.cAerEmb, DSC_CAEREMB));

  Result.AppendChild(AddNode(tcStr, '#06', 'cAerDes', 3, 4, 1,
                                              MDFe.Aereo.cAerDes, DSC_CAERDES));

  Result.AppendChild(AddNode(tcDat, '#07', 'dVoo', 10, 10, 0,
                                                    MDFe.Aereo.dVoo, DSC_DVOO));
end;

function TMDFeXmlWriter.Gerar_ModalAquav: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('aquav');

  if VersaoDF = ve100 then
    Result.AppendChild(AddNode(tcStr, '#02', 'CNPJAgeNav', 14, 14, 1,
                                        MDFe.aquav.CNPJAgeNav, DSC_CNPJAGENAV));

  if VersaoDF >= ve300 then
    Result.AppendChild(AddNode(tcStr, '#03', 'irin', 1, 10, 1,
                                                    MDFe.aquav.irin, DSC_IRIN));

  Result.AppendChild(AddNode(tcStr, '#04', 'tpEmb', 2, 2, 1,
                                                  MDFe.aquav.tpEmb, DSC_TPEMB));

  Result.AppendChild(AddNode(tcStr, '#05', 'cEmbar', 1, 10, 1,
                                                MDFe.aquav.cEmbar, DSC_CEMBAR));

  Result.AppendChild(AddNode(tcStr, '#06', 'xEmbar', 1, 60, 1,
                                                MDFe.aquav.xEmbar, DSC_XEMBAR));

  Result.AppendChild(AddNode(tcStr, '#07', 'nViag', 1, 10, 1,
                                                MDFe.aquav.nViagem, DSC_NVIAG));

  Result.AppendChild(AddNode(tcStr, '#08', 'cPrtEmb', 1, 5, 1,
                                              MDFe.aquav.cPrtEmb, DSC_CPRTEMB));

  Result.AppendChild(AddNode(tcStr, '#09', 'cPrtDest', 1, 5, 1,
                                            MDFe.aquav.cPrtDest, DSC_CPRTDEST));

  if VersaoDF >= ve300 then
  begin
    Result.AppendChild(AddNode(tcStr, '#10', 'prtTrans', 1, 60, 0,
                                            MDFe.aquav.prtTrans, DSC_PRTTRANS));

    Result.AppendChild(AddNode(tcStr, '#11', 'tpNav', 1, 1, 0,
                                TpNavegacaoToStr(MDFe.aquav.tpNav), DSC_TPNAV));
  end;

  nodeArray := Gerar_infTermCarreg;
  for i := 0 to MDFe.aquav.infTermCarreg.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_infTermDescarreg;
  for i := 0 to MDFe.aquav.infTermDescarreg.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_infEmbComb;
  for i := 0 to MDFe.aquav.infEmbComb.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_infUnidCargaVazia;
  for i := 0 to MDFe.aquav.infUnidCargaVazia.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  if VersaoDF >= ve300 then
  begin
    nodeArray := Gerar_infUnidTranspVazia;
    for i := 0 to MDFe.aquav.infUnidTranspVazia.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TMDFeXmlWriter.Gerar_infTermCarreg: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.aquav.infTermCarreg.Count);

  for i := 0 to MDFe.aquav.infTermCarreg.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infTermCarreg');

    Result[i].AppendChild(AddNode(tcStr, '#13', 'cTermCarreg', 1, 8, 1,
                     MDFe.aquav.infTermCarreg[i].cTermCarreg, DSC_CTERMCARREG));

    Result[i].AppendChild(AddNode(tcStr, '#14', 'xTermCarreg', 1, 60, 1,
                     MDFe.aquav.infTermCarreg[i].xTermCarreg, DSC_XTERMCARREG));
  end;

  if MDFe.aquav.infTermCarreg.Count > 5 then
    wAlerta('#12', 'infTermCarreg', '', ERR_MSG_MAIOR_MAXIMO + '5');
end;

function TMDFeXmlWriter.Gerar_infTermDescarreg: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.aquav.infTermDescarreg.Count);

  for i := 0 to MDFe.aquav.infTermDescarreg.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infTermDescarreg');

    Result[i].AppendChild(AddNode(tcStr, '#16', 'cTermDescarreg', 1, 8, 1,
               MDFe.aquav.infTermDescarreg[i].cTermDescarreg, DSC_CTERMDESCAR));

    Result[i].AppendChild(AddNode(tcStr, '#17', 'xTermDescarreg', 1, 60, 1,
               MDFe.aquav.infTermDescarreg[i].xTermDescarreg, DSC_XTERMDESCAR));
  end;

  if MDFe.aquav.infTermDescarreg.Count > 5 then
    wAlerta('#15', 'infTermDescarreg', '', ERR_MSG_MAIOR_MAXIMO + '5');
end;

function TMDFeXmlWriter.Gerar_infEmbComb: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.aquav.infEmbComb.Count);

  for i := 0 to MDFe.aquav.infEmbComb.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infEmbComb');

    Result[i].AppendChild(AddNode(tcStr, '#18', 'cEmbComb', 1, 10, 1,
                              MDFe.aquav.infEmbComb[i].cEmbComb, DSC_CEMBCOMB));

    if VersaoDF >= ve300 then
      Result[i].AppendChild(AddNode(tcStr, '#19', 'xBalsa', 1, 60, 1,
                                  MDFe.aquav.infEmbComb[i].xBalsa, DSC_XBALSA));
  end;

  if MDFe.aquav.infEmbComb.Count > 30 then
    wAlerta('#18', 'infEmbComb', '', ERR_MSG_MAIOR_MAXIMO + '30');
end;

function TMDFeXmlWriter.Gerar_infUnidCargaVazia: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.aquav.infUnidCargaVazia.Count);

  for i := 0 to MDFe.aquav.infUnidCargaVazia.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCargaVazia');

    Result[i].AppendChild(AddNode(tcStr, '#21', 'idUnidCargaVazia', 1, 20, 1,
            MDFe.aquav.infUnidCargaVazia[i].idUnidCargaVazia, DSC_IDUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#22', 'tpUnidCargaVazia', 1, 1, 1,
      UnidCargaToStr(MDFe.aquav.infUnidCargaVazia[i].tpUnidCargaVazia), DSC_TPUNIDCARGA));
  end;

  if MDFe.aquav.infUnidCargaVazia.Count > 990 then
    wAlerta('#20', 'infUnidCargaVazia', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infUnidTranspVazia: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.aquav.infUnidTranspVazia.Count);

  for i := 0 to MDFe.aquav.infUnidTranspVazia.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTranspVazia');

    Result[i].AppendChild(AddNode(tcStr, '#24', 'idUnidTranspVazia', 1, 20, 1,
         MDFe.aquav.infUnidTranspVazia[i].idUnidTranspVazia, DSC_IDUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#25', 'tpUnidTranspVazia', 1, 1, 1,
      UnidTranspToStr(MDFe.aquav.infUnidTranspVazia[i].tpUnidTranspVazia), DSC_TPUNIDTRANSP));
  end;

  if MDFe.aquav.infUnidTranspVazia.Count > 990 then
    wAlerta('#23', 'infUnidTranspVazia', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_ModalFerrov: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('ferrov');

  Result.AppendChild(Gerar_trem);

  nodeArray := Gerar_Vag;
  for i := 0 to MDFe.ferrov.vag.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TMDFeXmlWriter.Gerar_trem: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('trem');

  Result.AppendChild(AddNode(tcStr, '#03', 'xPref', 14, 14, 1,
                                                 MDFe.ferrov.xPref, DSC_XPREF));

  if VersaoDF = ve100 then
    Result.AppendChild(AddNode(tcDatHor, '#04', 'dhTrem', 19, 19, 0,
                                                MDFe.ferrov.dhTrem, DSC_DHTREM))
  else
    Result.AppendChild(AddNode(tcStr, '#04', 'dhTrem', 25, 25, 0,
           DateTimeWithTimeZone(MDFe.ferrov.dhTrem, MDFe.ide.cUF), DSC_DHTREM));

  Result.AppendChild(AddNode(tcStr, '#05', 'xOri', 1, 3, 1,
                                                   MDFe.ferrov.xOri, DSC_XORI));

  Result.AppendChild(AddNode(tcStr, '#06', 'xDest', 1, 3, 1,
                                                 MDFe.ferrov.xDest, DSC_XDEST));

  Result.AppendChild(AddNode(tcInt, '#07', 'qVag', 1, 3, 1,
                                                   MDFe.ferrov.qVag, DSC_QVAG));
end;

function TMDFeXmlWriter.Gerar_Vag: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.ferrov.vag.Count);

  for i := 0 to MDFe.ferrov.vag.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('vag');

    if MDFe.infMDFe.versao >= 3 then
    begin
      Result[i].AppendChild(AddNode(tcDe3, '#09', 'pesoBC', 1, 7, 1,
                                            MDFe.ferrov.vag[i].pesoBC, '****'));

      Result[i].AppendChild(AddNode(tcDe3, '#10', 'pesoR', 1, 7, 1,
                                             MDFe.ferrov.vag[i].pesoR, '****'));

      Result[i].AppendChild(AddNode(tcStr, '#11', 'tpVag', 1, 3, 0,
                                             MDFe.ferrov.vag[i].tpVag, '****'));
    end;

    Result[i].AppendChild(AddNode(tcStr, '#12', 'serie', 3, 3, 1,
                                         MDFe.ferrov.vag[i].serie, DSC_NSERIE));

    Result[i].AppendChild(AddNode(tcInt, '#13', 'nVag', 1, 8, 1,
                                            MDFe.ferrov.vag[i].nVag, DSC_NVAG));

    Result[i].AppendChild(AddNode(tcInt, '#14', 'nSeq', 1, 3, 0,
                                            MDFe.ferrov.vag[i].nSeq, DSC_NSEQ));

    Result[i].AppendChild(AddNode(tcDe3, '#15', 'TU', 1, 7, 1,
                                                MDFe.ferrov.vag[i].TU, DSC_TU));
  end;

  if MDFe.ferrov.vag.Count > 990 then
    wAlerta('#08', 'vag', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_InfDoc: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('infDoc');

  nodeArray := Gerar_infMunDescarga;
  for i := 0 to MDFe.infDoc.infMunDescarga.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TMDFeXmlWriter.Gerar_infMunDescarga: TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infMunDescarga');

    Result[i].AppendChild(AddNode(tcInt, '#046', 'cMunDescarga', 7, 7, 1,
                         MDFe.infDoc.infMunDescarga[i].cMunDescarga, DSC_CMUN));

    if not ValidarMunicipio(MDFe.infDoc.infMunDescarga[i].cMunDescarga) then
      wAlerta('#045', 'cMunDescarga', DSC_CMUN, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#046', 'xMunDescarga', 2, 60, 1,
                         MDFe.infDoc.infMunDescarga[i].xMunDescarga, DSC_XMUN));


    case MDFe.Ide.tpEmit of
     // Se Tipo de Emitente for Prestador de Serviço de Transporte
     // só pode relacionar os grupos de documentos CT-e e CT
     teTransportadora:
       begin
         nodeArray := Gerar_infMunDescarga_infCTe(i);
         for j := 0 to MDFe.infDoc.infMunDescarga[i].infCTe.Count - 1 do
         begin
           Result[i].AppendChild(nodeArray[j]);
         end;

         nodeArray := Gerar_infMunDescarga_infCT(i);
         for j := 0 to MDFe.infDoc.infMunDescarga[i].infCT.Count - 1 do
         begin
           Result[i].AppendChild(nodeArray[j]);
         end;
       end;

     // Se Tipo de Emitente for Transporte de Carga Própria
     // só pode relacionar os grupos de documentos NF-e e NT
     // Obs: É considerado Emitente de Transporte de Carga Própria os
     //      Emitentes de NF-e e transportadoras quando estiverem fazendo
     //      transporte de carga própria.
     teTranspCargaPropria, teTranspCTeGlobalizado:
       begin
         nodeArray := Gerar_infMunDescarga_infNFe(i);
         for j := 0 to MDFe.infDoc.infMunDescarga[i].infNFe.Count - 1 do
         begin
           Result[i].AppendChild(nodeArray[j]);
         end;

         nodeArray := Gerar_infMunDescarga_infNF(i);
         for j := 0 to MDFe.infDoc.infMunDescarga[i].infNF.Count - 1 do
         begin
           Result[i].AppendChild(nodeArray[j]);
         end;
       end;
    end;

    if MDFe.Ide.modal = moAquaviario then
    begin
      nodeArray := Gerar_infMunDescarga_infMDFeTransp(i);
      for j := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp.Count - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;
  end;

  if MDFe.infDoc.infMunDescarga.Count > 1000 then
    wAlerta('#045', 'infMunDescarga', '', ERR_MSG_MAIOR_MAXIMO + '1000');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe(
  Idx: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx].infCTe.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx].infCTe.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infCTe');

    Result[i].AppendChild(AddNode(tcEsp, '#049', 'chCTe', 44, 44, 1,
        OnlyNumber(MDFe.infDoc.infMunDescarga[Idx].infCTe[i].chCTe), DSC_REFCTE));

    if OnlyNumber(MDFe.infDoc.infMunDescarga[Idx].infCTe[i].chCTe) <> '' then
      if not ValidarChave(MDFe.infDoc.infMunDescarga[Idx].infCTe[i].chCTe) then
        wAlerta('#049', 'chCTe', DSC_REFCTE, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#050', 'SegCodBarra', 36, 36, 0,
         MDFe.infDoc.infMunDescarga[Idx].infCTe[i].SegCodBarra, DSC_SEGCODBARRA));

    if VersaoDF >= ve300 then
      Result[i].AppendChild(AddNode(tcStr, '#050', 'indReentrega', 1, 1, 0,
       MDFe.infDoc.infMunDescarga[Idx].infCTe[i].indReentrega, DSC_INDREENTREGA));

    nodeArray := Gerar_infMunDescarga_infCTe_infUnidTransp(Idx, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infCTe[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    if VersaoDF >= ve300 then
    begin
      nodeArray := Gerar_infMunDescarga_infCTe_peri(Idx, i);
      for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infCTe[i].peri.Count - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;

      if (MDFe.Ide.modal = moAereo) and
         ((MDFe.infDoc.infMunDescarga[Idx].infCTe[i].infEntregaParcial.qtdTotal <> 0) or
         (MDFe.infDoc.infMunDescarga[Idx].infCTe[i].infEntregaParcial.qtdParcial <> 0)) then
      begin
        Result[i].AppendChild(Gerar_infEntregaParcial(Idx, i));
      end;

      if MDFe.infDoc.infMunDescarga[Idx].infCTe[i].indPrestacaoParcial = tieSim then
        Result[i].AppendChild(AddNode(tcStr, '#', 'indPrestacaoParcial', 1, 1, 0,
                                                                          '1'));

      nodeArray := Gerar_infMunDescarga_infCTe_infNFePrestParcial(Idx, i);
      for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infCTe[i].infNFePrestParcial.Count - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;
  end;

  if MDFe.infDoc.infMunDescarga[Idx].infCTe.Count > 20000 then
    wAlerta('#048', 'infCTe', '', ERR_MSG_MAIOR_MAXIMO + '20000');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe_infUnidTransp(
  Idx1, Idx2: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#052', 'tpUnidTransp', 1, 1, 1,
     UnidTranspToStr(MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[i].tpUnidTransp), DSC_TPUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#053', 'idUnidTransp', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[i].idUnidTransp, DSC_IDUNIDTRANSP));

    nodeArray := Gerar_infMunDescarga_infCTe_lacUnidTransp(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[i].lacUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_infMunDescarga_infCTe_infUnidCarga(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#062', 'qtdRat', 1, 5, 0,
     MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp.Count > 990 then
    wAlerta('#89', 'infUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe_peri(
  Idx1, Idx2: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('peri');

    Result[i].AppendChild(AddNode(tcStr, '#90', 'nONU', 1, 15, 1,
       MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri[i].nONU, DSC_NONU));

    Result[i].AppendChild(AddNode(tcStr, '#91', 'xNomeAE', 1, 150, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri[i].xNomeAE, DSC_XNOMEAE));

    Result[i].AppendChild(AddNode(tcStr, '#92', 'xClaRisco', 1, 40, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri[i].xClaRisco, DSC_XCLARISCO));

    Result[i].AppendChild(AddNode(tcStr, '#93', 'grEmb', 1, 6, 0,
       MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri[i].grEmb, DSC_GREMB));

    Result[i].AppendChild(AddNode(tcStr, '#94', 'qTotProd', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri[i].qTotProd, DSC_QTOTPROD));

    Result[i].AppendChild(AddNode(tcStr, '#95', 'qVolTipo', 1, 60, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri[i].qVolTipo, DSC_QVOLTIPO));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].peri.Count > 990 then
    wAlerta('#89', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe_lacUnidTransp(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#055', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].lacUnidTransp[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count > 990 then
    wAlerta('#054', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe_infUnidCarga(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#057', 'tpUnidCarga', 1, 1, 1,
      UnidCargaToStr(MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].tpUnidCarga), DSC_TPUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#058', 'idUnidCarga', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].idUnidCarga, DSC_IDUNIDCARGA));

    nodeArray := Gerar_infMunDescarga_infCTe_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].lacUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#061', 'qtdRat', 1, 5, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga.Count > 990 then
    wAlerta('#056', 'infUnidCarga', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe_infUnidCarga_lacUnidCarga(Idx1, Idx2,
  Idx3, Idx4: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count > 990 then
    wAlerta('#059', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCTe_infNFePrestParcial(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infNFePrestParcial.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infNFePrestParcial.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNFePrestParcial');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'chNFe', 44, 44, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infNFePrestParcial[i].chNFe, DSC_CHAVE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infNFePrestParcial.Count > 990 then
    wAlerta('#059', 'infNFePrestParcial', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infEntregaParcial(Idx1,
  Idx2: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infEntregaParcial');

  Result.AppendChild(AddNode(tcDe4, '#97', 'qtdTotal', 1, 15, 1,
    MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infEntregaParcial.qtdTotal, DSC_QTDTOTAL));

  Result.AppendChild(AddNode(tcDe4, '#98', 'qtdParcial', 1, 15, 1,
    MDFe.infDoc.infMunDescarga[Idx1].infCTe[Idx2].infEntregaParcial.qtdParcial, DSC_QTDPARCIAL));
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCT(
  Idx: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx].infCT.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx].infCT.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infCT');

    Result[i].AppendChild(AddNode(tcStr, '#052', 'nCT', 1, 20, 1,
                        MDFe.infDoc.infMunDescarga[Idx].infCT[i].nCT, DSC_NCT));

    Result[i].AppendChild(AddNode(tcInt, '#053', 'serie', 1, 3, 1,
                    MDFe.infDoc.infMunDescarga[Idx].infCT[i].serie, DSC_SERIE));

    Result[i].AppendChild(AddNode(tcInt, '#054', 'subser', 1, 2, 1,
                MDFe.infDoc.infMunDescarga[Idx].infCT[i].subser, DSC_SUBSERIE));

    Result[i].AppendChild(AddNode(tcDat, '#055', 'dEmi', 10, 10, 1,
                      MDFe.infDoc.infMunDescarga[Idx].infCT[i].dEmi, DSC_DEMI));

    Result[i].AppendChild(AddNode(tcDe2, '#056', 'vCarga', 1, 15, 1,
                    MDFe.infDoc.infMunDescarga[Idx].infCT[i].vCarga, DSC_VDOC));

    nodeArray := Gerar_infMunDescarga_infCT_infUnidTransp(Idx, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infCT[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if MDFe.infDoc.infMunDescarga[Idx].infCT.Count > 10000 then
    wAlerta('#048', 'infCT', '', ERR_MSG_MAIOR_MAXIMO + '10000');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCT_infUnidTransp(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#052', 'tpUnidTransp', 1, 1, 1,
     UnidTranspToStr(MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[i].tpUnidTransp), DSC_TPUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#053', 'idUnidTransp', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[i].idUnidTransp, DSC_IDUNIDTRANSP));

    nodeArray := Gerar_infMunDescarga_infCT_lacUnidTransp(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[i].lacUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_infMunDescarga_infCT_infUnidCarga(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#062', 'qtdRat', 1, 5, 0,
     MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp.Count > 990 then
    wAlerta('#89', 'infUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCT_lacUnidTransp(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#055', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].lacUnidTransp[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count > 990 then
    wAlerta('#054', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCT_infUnidCarga(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#057', 'tpUnidCarga', 1, 1, 1,
      UnidCargaToStr(MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[i].tpUnidCarga), DSC_TPUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#058', 'idUnidCarga', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[i].idUnidCarga, DSC_IDUNIDCARGA));

    nodeArray := Gerar_infMunDescarga_infCT_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[i].lacUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#061', 'qtdRat', 1, 5, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga.Count > 990 then
    wAlerta('#056', 'infUnidCarga', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infCT_infUnidCarga_lacUnidCarga(
  Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infCT[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count > 990 then
    wAlerta('#059', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNFe(
  Idx: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx].infNFe.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx].infNFe.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNFe');

    Result[i].AppendChild(AddNode(tcEsp, '#049', 'chNFe', 44, 44, 1,
        OnlyNumber(MDFe.infDoc.infMunDescarga[Idx].infNFe[i].chNFe), DSC_REFNFE));

    if OnlyNumber(MDFe.infDoc.infMunDescarga[Idx].infNFe[i].chNFe) <> '' then
      if not ValidarChave(MDFe.infDoc.infMunDescarga[Idx].infNFe[i].chNFe) then
        wAlerta('#049', 'chNFe', DSC_REFNFE, ERR_MSG_INVALIDO);

    Result[i].AppendChild(AddNode(tcStr, '#050', 'SegCodBarra', 36, 36, 0,
         MDFe.infDoc.infMunDescarga[Idx].infNFe[i].SegCodBarra, DSC_SEGCODBARRA));

    if VersaoDF >= ve300 then
      Result[i].AppendChild(AddNode(tcStr, '#050', 'indReentrega', 1, 1, 0,
       MDFe.infDoc.infMunDescarga[Idx].infNFe[i].indReentrega, DSC_INDREENTREGA));

    nodeArray := Gerar_infMunDescarga_infNFe_infUnidTransp(Idx, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infNFe[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    if VersaoDF >= ve300 then
    begin
      nodeArray := Gerar_infMunDescarga_infNFe_peri(Idx, i);
      for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infNFe[i].peri.Count - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;
  end;

  if MDFe.infDoc.infMunDescarga[Idx].infNFe.Count > 20000 then
    wAlerta('#048', 'infNFe', '', ERR_MSG_MAIOR_MAXIMO + '20000');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNFe_infUnidTransp(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#052', 'tpUnidTransp', 1, 1, 1,
     UnidTranspToStr(MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[i].tpUnidTransp), DSC_TPUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#053', 'idUnidTransp', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[i].idUnidTransp, DSC_IDUNIDTRANSP));

    nodeArray := Gerar_infMunDescarga_infNFe_lacUnidTransp(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[i].lacUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_infMunDescarga_infNFe_infUnidCarga(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#062', 'qtdRat', 1, 5, 0,
     MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp.Count > 990 then
    wAlerta('#89', 'infUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNFe_peri(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('peri');

    Result[i].AppendChild(AddNode(tcStr, '#90', 'nONU', 1, 15, 1,
       MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri[i].nONU, DSC_NONU));

    Result[i].AppendChild(AddNode(tcStr, '#91', 'xNomeAE', 1, 150, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri[i].xNomeAE, DSC_XNOMEAE));

    Result[i].AppendChild(AddNode(tcStr, '#92', 'xClaRisco', 1, 40, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri[i].xClaRisco, DSC_XCLARISCO));

    Result[i].AppendChild(AddNode(tcStr, '#93', 'grEmb', 1, 6, 0,
       MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri[i].grEmb, DSC_GREMB));

    Result[i].AppendChild(AddNode(tcStr, '#94', 'qTotProd', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri[i].qTotProd, DSC_QTOTPROD));

    Result[i].AppendChild(AddNode(tcStr, '#95', 'qVolTipo', 1, 60, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri[i].qVolTipo, DSC_QVOLTIPO));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].peri.Count > 990 then
    wAlerta('#89', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNFe_lacUnidTransp(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#055', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].lacUnidTransp[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count > 990 then
    wAlerta('#054', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNFe_infUnidCarga(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#057', 'tpUnidCarga', 1, 1, 1,
      UnidCargaToStr(MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].tpUnidCarga), DSC_TPUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#058', 'idUnidCarga', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].idUnidCarga, DSC_IDUNIDCARGA));

    nodeArray := Gerar_infMunDescarga_infNFe_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].lacUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#061', 'qtdRat', 1, 5, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga.Count > 990 then
    wAlerta('#056', 'infUnidCarga', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNFe_infUnidCarga_lacUnidCarga(
  Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNFe[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count > 990 then
    wAlerta('#059', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNF(
  Idx: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx].infNF.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx].infNF.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infNF');

    Result[i].AppendChild(AddNode(tcStr, '#061', 'CNPJ', 14, 14, 1,
                      MDFe.infDoc.infMunDescarga[Idx].infNF[i].CNPJ, DSC_CNPJ));

    Result[i].AppendChild(AddNode(tcStr, '#062', 'UF', 2, 2, 1,
                          MDFe.infDoc.infMunDescarga[Idx].infNF[i].UF, DSC_UF));

    Result[i].AppendChild(AddNode(tcStr, '#063', 'nNF', 1, 20, 1,
                        MDFe.infDoc.infMunDescarga[Idx].infNF[i].nNF, DSC_NDF));

    Result[i].AppendChild(AddNode(tcInt, '#064', 'serie', 1, 3, 1,
                    MDFe.infDoc.infMunDescarga[Idx].infNF[i].serie, DSC_SERIE));

    Result[i].AppendChild(AddNode(tcDat, '#065', 'dEmi', 10, 10, 1,
                      MDFe.infDoc.infMunDescarga[Idx].infNF[i].dEmi, DSC_DEMI));

    Result[i].AppendChild(AddNode(tcDe2, '#066', 'vNF', 1, 15, 1,
                       MDFe.infDoc.infMunDescarga[Idx].infNF[i].vNF, DSC_VDOC));

    Result[i].AppendChild(AddNode(tcInt, '#067', 'PIN', 2, 9, 0,
                        MDFe.infDoc.infMunDescarga[Idx].infNF[i].PIN, DSC_PIN));

    nodeArray := Gerar_infMunDescarga_infNF_infUnidTransp(Idx, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infNF[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if MDFe.infDoc.infMunDescarga[Idx].infNF.Count > 10000 then
    wAlerta('#048', 'infNF', '', ERR_MSG_MAIOR_MAXIMO + '10000');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNF_infUnidTransp(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#052', 'tpUnidTransp', 1, 1, 1,
     UnidTranspToStr(MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[i].tpUnidTransp), DSC_TPUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#053', 'idUnidTransp', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[i].idUnidTransp, DSC_IDUNIDTRANSP));

    nodeArray := Gerar_infMunDescarga_infNF_lacUnidTransp(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[i].lacUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_infMunDescarga_infNF_infUnidCarga(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#062', 'qtdRat', 1, 5, 0,
     MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp.Count > 990 then
    wAlerta('#89', 'infUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNF_lacUnidTransp(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#055', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].lacUnidTransp[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count > 990 then
    wAlerta('#054', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNF_infUnidCarga(Idx1, Idx2,
  Idx3: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#057', 'tpUnidCarga', 1, 1, 1,
      UnidCargaToStr(MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[i].tpUnidCarga), DSC_TPUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#058', 'idUnidCarga', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[i].idUnidCarga, DSC_IDUNIDCARGA));

    nodeArray := Gerar_infMunDescarga_infNF_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[i].lacUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#061', 'qtdRat', 1, 5, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga.Count > 990 then
    wAlerta('#056', 'infUnidCarga', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infNF_infUnidCarga_lacUnidCarga(
  Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infNF[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count > 990 then
    wAlerta('#059', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infMDFeTransp(
  Idx: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infMDFeTransp');

    Result[i].AppendChild(AddNode(tcEsp, '#049', 'chMDFe', 44, 44, 1,
        OnlyNumber(MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp[i].chMDFe), DSC_REFNFE));

    if OnlyNumber(MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp[i].chMDFe) <> '' then
      if not ValidarChave(MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp[i].chMDFe) then
        wAlerta('#049', 'chMDFe', DSC_REFNFE, ERR_MSG_INVALIDO);

    if VersaoDF >= ve300 then
      Result[i].AppendChild(AddNode(tcStr, '#050', 'indReentrega', 1, 1, 0,
       MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp[i].indReentrega, DSC_INDREENTREGA));

    nodeArray := Gerar_infMunDescarga_infMDFeTransp_infUnidTransp(Idx, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp[i].infUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    if VersaoDF >= ve300 then
    begin
      nodeArray := Gerar_infMunDescarga_infMDFeTransp_peri(Idx, i);
      for j := 0 to MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp[i].peri.Count - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;
  end;

  if MDFe.infDoc.infMunDescarga[Idx].infMDFeTransp.Count > 20000 then
    wAlerta('#048', 'infMDFeTransp', '', ERR_MSG_MAIOR_MAXIMO + '20000');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infMDFeTransp_infUnidTransp(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#052', 'tpUnidTransp', 1, 1, 1,
     UnidTranspToStr(MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[i].tpUnidTransp), DSC_TPUNIDTRANSP));

    Result[i].AppendChild(AddNode(tcStr, '#053', 'idUnidTransp', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[i].idUnidTransp, DSC_IDUNIDTRANSP));

    nodeArray := Gerar_infMunDescarga_infMDFeTransp_lacUnidTransp(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[i].lacUnidTransp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    nodeArray := Gerar_infMunDescarga_infMDFeTransp_infUnidCarga(Idx1, Idx2, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[i].infUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#062', 'qtdRat', 1, 5, 0,
     MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp.Count > 990 then
    wAlerta('#89', 'infUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infMDFeTransp_peri(Idx1,
  Idx2: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('peri');

    Result[i].AppendChild(AddNode(tcStr, '#90', 'nONU', 1, 15, 1,
       MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri[i].nONU, DSC_NONU));

    Result[i].AppendChild(AddNode(tcStr, '#91', 'xNomeAE', 1, 150, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri[i].xNomeAE, DSC_XNOMEAE));

    Result[i].AppendChild(AddNode(tcStr, '#92', 'xClaRisco', 1, 40, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri[i].xClaRisco, DSC_XCLARISCO));

    Result[i].AppendChild(AddNode(tcStr, '#93', 'grEmb', 1, 6, 0,
       MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri[i].grEmb, DSC_GREMB));

    Result[i].AppendChild(AddNode(tcStr, '#94', 'qTotProd', 1, 20, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri[i].qTotProd, DSC_QTOTPROD));

    Result[i].AppendChild(AddNode(tcStr, '#95', 'qVolTipo', 1, 60, 1,
     MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri[i].qVolTipo, DSC_QVOLTIPO));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].peri.Count > 990 then
    wAlerta('#89', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infMDFeTransp_lacUnidTransp(Idx1,
  Idx2, Idx3: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidTransp');

    Result[i].AppendChild(AddNode(tcStr, '#055', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].lacUnidTransp[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].lacUnidTransp.Count > 990 then
    wAlerta('#054', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infMDFeTransp_infUnidCarga(Idx1,
  Idx2, Idx3: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#057', 'tpUnidCarga', 1, 1, 1,
      UnidCargaToStr(MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[i].tpUnidCarga), DSC_TPUNIDCARGA));

    Result[i].AppendChild(AddNode(tcStr, '#058', 'idUnidCarga', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[i].idUnidCarga, DSC_IDUNIDCARGA));

    nodeArray := Gerar_infMunDescarga_infMDFeTransp_infUnidCarga_lacUnidCarga(Idx1, Idx2, Idx3, i);
    for j := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[i].lacUnidCarga.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;

    Result[i].AppendChild(AddNode(tcDe2, '#061', 'qtdRat', 1, 5, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[i].qtdRat, DSC_QTDRAT));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga.Count > 990 then
    wAlerta('#056', 'infUnidCarga', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_infMunDescarga_infMDFeTransp_infUnidCarga_lacUnidCarga(
  Idx1, Idx2, Idx3, Idx4: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count);

  for i := 0 to MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacUnidCarga');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'nLacre', 1, 20, 1,
      MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.infDoc.infMunDescarga[Idx1].infMDFeTransp[Idx2].infUnidTransp[Idx3].infUnidCarga[Idx4].lacUnidCarga.Count > 990 then
    wAlerta('#059', 'lacUnidTransp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_Seg: TACBrXmlNodeArray;
var
  i, j: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.seg.Count);

  for i := 0 to MDFe.seg.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('seg');

    Result[i].AppendChild(Gerar_Seg_infResp(i));

    if (MDFe.Ide.tpEmit in [teTransportadora, teTranspCTeGlobalizado]) or
       ((MDFe.seg[i].xSeg <> '') and (MDFe.seg[i].CNPJ <> '')) then
    begin
      Result[i].AppendChild(Gerar_Seg_infSeg(i));
    end;

    Result[i].AppendChild(AddNode(tcStr, '#126', 'nApol', 1, 20, 0,
                                                 MDFe.seg[i].nApol, DSC_NAPOL));

    for j := 0 to MDFe.seg[i].aver.Count - 1 do
      Result[i].AppendChild(AddNode(tcStr, '#127', 'nAver', 1, 40, 0,
                                         MDFe.seg[i].aver[j].nAver, DSC_NAVER));

    if MDFe.seg[i].aver.Count > 990 then
      wAlerta('#', 'nAver', '', ERR_MSG_MAIOR_MAXIMO + '990');
  end;

  if MDFe.seg.Count > 990 then
    wAlerta('#118', 'seg', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_Seg_infResp(Idx: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infResp');

  Result.AppendChild(AddNode(tcStr, '#120', 'respSeg', 1, 1, 1,
                       RspSeguroMDFeToStr(MDFe.seg[Idx].respSeg), DSC_RESPSEG));

  if MDFe.seg[Idx].respSeg = rsTomadorServico then
    Result.AppendChild(AddNodeCNPJCPF('#121', '#122',
                                                 MDFe.seg[Idx].CNPJCPF, False));
end;

function TMDFeXmlWriter.Gerar_Seg_infSeg(Idx: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infSeg');

  Result.AppendChild(AddNode(tcStr, '#130', 'xSeg', 1, 30, 1,
                                                 MDFe.seg[Idx].xSeg, DSC_XSEG));

  Result.AppendChild(AddNodeCNPJCPF('#131', '#132',
                                                     MDFe.seg[Idx].CNPJ, True));
end;

function TMDFeXmlWriter.Gerar_ProdPred: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('prodPred');

  Result.AppendChild(AddNode(tcStr, '#136', 'tpCarga', 2, 2, 1,
                              TCargaToStr(MDFe.prodPred.tpCarga), DSC_TPCARGA));

  Result.AppendChild(AddNode(tcStr, '#137', 'xProd', 1, 120, 1,
                                               MDFe.prodPred.xProd, DSC_XPROD));

  Result.AppendChild(AddNode(tcStr, '#138', 'cEAN', 0, 14, 0,
                                                 MDFe.prodPred.cEAN, DSC_CEAN));

  Result.AppendChild(AddNode(tcStr, '#139', 'NCM', 2, 8, 0,
                                                   MDFe.prodPred.NCM, DSC_NCM));

  if (MDFe.prodPred.infLocalCarrega.CEP > 0) or
     (MDFe.prodPred.infLocalCarrega.latitude <> 0) or
     (MDFe.prodPred.infLocalCarrega.Longitude <> 0) or
     (MDFe.prodPred.infLocalDescarrega.CEP > 0) or
     (MDFe.prodPred.infLocalDescarrega.latitude <> 0) or
     (MDFe.prodPred.infLocalDescarrega.Longitude <> 0) then
  begin
    Result.AppendChild(Gerar_infLotacao);
  end;
end;

function TMDFeXmlWriter.Gerar_infLotacao: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infLotacao');

  Result.AppendChild(Gerar_infLocalCarrega);
  Result.AppendChild(Gerar_infLocalDescarrega);
end;

function TMDFeXmlWriter.Gerar_infLocalCarrega: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infLocalCarrega');

  if (MDFe.prodPred.infLocalCarrega.CEP > 0) then
    Result.AppendChild(AddNode(tcInt, '#141', 'CEP', 8, 8, 1,
                                    MDFe.prodPred.infLocalCarrega.CEP, DSC_CEP))
  else
    if (MDFe.prodPred.infLocalCarrega.latitude <> 0) or
       (MDFe.prodPred.infLocalCarrega.Longitude <> 0) then
    begin
      Result.AppendChild(AddNode(tcDe6, '#142', 'latitude', 1, 10, 1,
                         MDFe.prodPred.infLocalCarrega.latitude, DSC_LATITUDE));

      Result.AppendChild(AddNode(tcDe6, '#143', 'longitude', 1, 11, 1,
                       MDFe.prodPred.infLocalCarrega.Longitude, DSC_LONGITUDE));
    end;
end;

function TMDFeXmlWriter.Gerar_infLocalDescarrega: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infLocalDescarrega');

  if (MDFe.prodPred.infLocalDescarrega.CEP > 0) then
    Result.AppendChild(AddNode(tcInt, '#141', 'CEP', 8, 8, 1,
                                 MDFe.prodPred.infLocalDescarrega.CEP, DSC_CEP))
  else
    if (MDFe.prodPred.infLocalDescarrega.latitude <> 0) or
       (MDFe.prodPred.infLocalDescarrega.Longitude <> 0) then
    begin
      Result.AppendChild(AddNode(tcDe6, '#142', 'latitude', 1, 10, 1,
                      MDFe.prodPred.infLocalDescarrega.latitude, DSC_LATITUDE));

      Result.AppendChild(AddNode(tcDe6, '#143', 'longitude', 1, 11, 1,
                    MDFe.prodPred.infLocalDescarrega.Longitude, DSC_LONGITUDE));
    end;
end;

function TMDFeXmlWriter.Gerar_Total: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('tot');

  Result.AppendChild(AddNode(tcInt, '#069', 'qCTe', 1, 4, 0,
                                                      MDFe.tot.qCTe, DSC_QCTE));

  Result.AppendChild(AddNode(tcInt, '#070', 'qCT', 1, 4, 0,
                                                        MDFe.tot.qCT, DSC_QCT));

  Result.AppendChild(AddNode(tcInt, '#071', 'qNFe', 1, 4, 0,
                                                      MDFe.tot.qNFe, DSC_QNFE));

  Result.AppendChild(AddNode(tcInt, '#072', 'qNF', 1, 4, 0,
                                                        MDFe.tot.qNF, DSC_QNF));

  Result.AppendChild(AddNode(tcInt, '#073', 'qMDFe', 1, 4, 0,
                                                      MDFe.tot.qMDFe, DSC_QNF));

  Result.AppendChild(AddNode(tcDe2, '#074', 'vCarga', 1, 15, 1,
                                                    MDFe.tot.vCarga, DSC_VDOC));

  Result.AppendChild(AddNode(tcStr, '#075', 'cUnid', 2, 2, 1,
                                      UnidMedToStr(MDFe.tot.cUnid), DSC_CUNID));

  Result.AppendChild(AddNode(tcDe4, '#076', 'qCarga', 1, 15, 1,
                                                  MDFe.tot.qCarga, DSC_QCARGA));
end;

function TMDFeXmlWriter.Gerar_Lacres: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.lacres.Count);

  for i := 0 to MDFe.lacres.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('lacres');

    Result[i].AppendChild(AddNode(tcStr, '#060', 'nLacre', 1, 20, 1,
      MDFe.lacres[i].nLacre, DSC_NLACRE));
  end;

  if MDFe.lacres.Count > 990 then
    wAlerta('#076', 'lacres', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TMDFeXmlWriter.Gerar_autXML: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, MDFe.autXML.Count);

  for i := 0 to MDFe.autXML.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('autXML');

    Result[i].AppendChild(AddNodeCNPJCPF('#305', '#306', MDFe.autXML[i].CNPJCPF));
  end;

  if MDFe.autXML.Count > 10 then
    wAlerta('#304', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TMDFeXmlWriter.Gerar_InfAdic: TACBrXmlNode;
begin
  Result := nil;

  if (trim(MDFe.InfAdic.infAdFisco) <> '') or (trim(MDFe.InfAdic.infCpl) <> '') then
  begin
    Result := FDocument.CreateElement('infAdic');

    Result.AppendChild(AddNode(tcStr, '#308', 'infAdFisco', 1, 2000, 0,
                                      MDFe.InfAdic.infAdFisco, DSC_INFADFISCO));

    Result.AppendChild(AddNode(tcStr, '#309', 'infCpl', 1, 5000, 0,
                                              MDFe.InfAdic.infCpl, DSC_INFCPL));
  end;
end;

function TMDFeXmlWriter.Gerar_InfRespTec: TACBrXmlNode;
begin
  Result := nil;

  if (MDFe.infRespTec.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('infRespTec');

    Result.AppendChild(AddNodeCNPJ('#311', MDFe.infRespTec.CNPJ, CODIGO_BRASIL, True));

    Result.AppendChild(AddNode(tcStr, '#312', 'xContato', 2, 60, 1,
                                       MDFe.infRespTec.xContato, DSC_XCONTATO));

    Result.AppendChild(AddNode(tcStr, '#313', 'email', 6, 60, 1,
                                             MDFe.infRespTec.email, DSC_EMAIL));

    Result.AppendChild(AddNode(tcStr, '#314', 'fone', 7, 12, 1,
                                               MDFe.infRespTec.fone, DSC_FONE));

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Result.AppendChild(AddNode(tcInt, '#315', 'idCSRT', 2, 2, 1,
                                                           idCSRT, DSC_IDCSRT));

      Result.AppendChild(AddNode(tcStr, '#316', 'hashCSRT', 28, 28, 1,
                             CalcularHashCSRT(CSRT, FChaveMDFe), DSC_HASHCSRT));
    end;
  end;
end;

function TMDFeXmlWriter.Gerar_ProtMDFe: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('protMDFe');

  Result.SetAttribute('versao', FloatToString(MDFe.infMDFe.Versao, '.', '#0.00'));

  xmlNode := Result.AddChild('infProt');

  xmlNode.AddChild('tpAmb').Content := TpAmbToStr(MDFe.procMDFe.tpAmb);

  xmlNode.AddChild('verAplic').Content := MDFe.procMDFe.verAplic;

  xmlNode.AddChild('chMDFe').Content := MDFe.procMDFe.chMDFe;

  xmlNode.AddChild('dhRecbto').Content :=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', MDFe.procMDFe.dhRecbto) +
    GetUTC(CodigoUFparaUF(FMDFe.Ide.cUF), MDFe.procMDFe.dhRecbto);

  xmlNode.AddChild('nProt').Content := MDFe.procMDFe.nProt;

  xmlNode.AddChild('digVal').Content := MDFe.procMDFe.digVal;

  xmlNode.AddChild('cStat').Content := IntToStr(MDFe.procMDFe.cStat);

  xmlNode.AddChild('xMotivo').Content := MDFe.procMDFe.xMotivo;
end;

end.
