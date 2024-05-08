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

unit ACBrNFComXmlWriter;

interface

uses
  Classes, SysUtils,
  ACBrXmlBase, ACBrXmlDocument, ACBrXmlWriter,
  ACBrNFComClass,
  ACBrNFComConversao;

type
  TNFComXmlWriterOptions = class(TACBrXmlWriterOptions)
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

  TNFComXmlWriter = class(TACBrXmlWriter)
  private
    FNFCom: TNFCom;

    FChaveNFCom: string;

    FVersaoDF: TVersaoNFCom;
    FModeloDF: Integer;
    FtpAmb: TACBrTipoAmbiente;
    FtpEmis: TACBrTipoEmissao;
    FIdCSRT: integer;
    FCSRT: string;

    function Gerar_InfNFCom: TACBrXmlNode;
    function Gerar_Ide: TACBrXmlNode;

    function Gerar_Emit: TACBrXmlNode;
    function Gerar_EmitEnderEmit: TACBrXmlNode;

    function Gerar_Dest: TACBrXmlNode;
    function Gerar_DestEnderDest(var UF: string): TACBrXmlNode;

    function Gerar_Assinante: TACBrXmlNode;
    function Gerar_gNF: TACBrXmlNode;
    function Gerar_gSub: TACBrXmlNode;
    function Gerar_gCofat: TACBrXmlNode;

    function Gerar_det: TACBrXmlNodeArray;

    function Gerar_det_prod(adet: Integer): TACBrXmlNode;

    function Gerar_det_imposto(adet: Integer): TACBrXmlNode;
    function Gerar_det_imposto_ICMS(aDet: Integer): TACBrXmlNode;
    function Gerar_det_imposto_ICMSUFDest(aDet: Integer): TACBrXmlNodeArray;
    function Gerar_det_imposto_PIS(aDet: Integer): TACBrXmlNode;
    function Gerar_det_imposto_COFINS(aDet: Integer): TACBrXmlNode;
    function Gerar_det_imposto_FUST(aDet: Integer): TACBrXmlNode;
    function Gerar_det_imposto_FUNTTEL(aDet: Integer): TACBrXmlNode;
    function Gerar_det_imposto_retTrib(aDet: Integer): TACBrXmlNode;

    function Gerar_det_gProcRef(aDet: Integer): TACBrXmlNode;
    function Gerar_det_gProcRef_gProc(aDet: Integer): TACBrXmlNodeArray;

    function Gerar_det_gRessarc(aDet: Integer): TACBrXmlNode;

    function Gerar_Total: TACBrXmlNode;
    function Gerar_Total_ICMSTotal: TACBrXmlNode;
    function Gerar_Total_vRetTribTot: TACBrXmlNode;

    function Gerar_gFidelidade: TACBrXmlNode;

    function Gerar_gFat: TACBrXmlNode;
    function Gerar_gFat_enderCorresp: TACBrXmlNode;
    function Gerar_gFat_gPix: TACBrXmlNode;

    function Gerar_gFatCentral: TACBrXmlNode;

    function Gerar_autXML: TACBrXmlNodeArray;

    function Gerar_InfAdic: TACBrXmlNode;
    function Gerar_gRespTec: TACBrXmlNode;

    function Gerar_ProtNFCom: TACBrXmlNode;

    function GetOpcoes: TNFComXmlWriterOptions;
    procedure SetOpcoes(AValue: TNFComXmlWriterOptions);

    procedure AjustarMunicipioUF(out xUF: string; out xMun: string;
      out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

  public
    constructor Create(AOwner: TNFCom); reintroduce;
    destructor Destroy; override;

    function GerarXml: boolean; override;
    function ObterNomeArquivo: string; override;

    property Opcoes: TNFComXmlWriterOptions read GetOpcoes write SetOpcoes;
    property NFCom: TNFCom read FNFCom write FNFCom;

    property VersaoDF: TVersaoNFCom read FVersaoDF write FVersaoDF;
    property ModeloDF: Integer read FModeloDF write FModeloDF;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property tpEmis: TACBrTipoEmissao read FtpEmis write FtpEmis;
    property IdCSRT: integer read FIdCSRT write FIdCSRT;
    property CSRT: string read FCSRT write FCSRT;
  end;

implementation

uses
  StrUtils,
  Math,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
//  ACBrValidador,
  ACBrNFComConsts;

constructor TNFComXmlWriter.Create(AOwner: TNFCom);
begin
  inherited Create;

  TNFComXmlWriterOptions(Opcoes).AjustarTagNro := True;
  TNFComXmlWriterOptions(Opcoes).GerarTagIPIparaNaoTributado := True;
  TNFComXmlWriterOptions(Opcoes).NormatizarMunicipios := False;
  TNFComXmlWriterOptions(Opcoes).PathArquivoMunicipios := '';
  TNFComXmlWriterOptions(Opcoes).GerarTagAssinatura := taSomenteSeAssinada;
  TNFComXmlWriterOptions(Opcoes).ValidarInscricoes := False;
  TNFComXmlWriterOptions(Opcoes).ValidarListaServicos := False;
  TNFComXmlWriterOptions(Opcoes).CamposFatObrigatorios := True;

  FNFCom := AOwner;
end;

destructor TNFComXmlWriter.Destroy;
begin
  inherited Destroy;
end;

function TNFComXmlWriter.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TNFComXmlWriterOptions.Create();
end;

function TNFComXmlWriter.GetOpcoes: TNFComXmlWriterOptions;
begin
  Result := TNFComXmlWriterOptions(FOpcoes);
end;

procedure TNFComXmlWriter.SetOpcoes(AValue: TNFComXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

procedure TNFComXmlWriter.AjustarMunicipioUF(out xUF: string; out xMun: string;
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

function TNFComXmlWriter.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FNFCom.infNFCom.ID) + '-NFCom.xml';
end;

function TNFComXmlWriter.GerarXml: boolean;
var
  Gerar: boolean;
  NFComNode, xmlNode: TACBrXmlNode;
begin
  Result := False;

  ListaDeAlertas.Clear;

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
{
  NFCom.infNFCom.Versao := VersaoNFComToDbl(VersaoDF);
  NFCom.Ide.modelo := ModeloDF;
  NFCom.Ide.tpAmb := tpAmb;
  NFCom.ide.tpEmis := tpEmis;
}
  FChaveNFCom := GerarChaveAcesso(NFCom.ide.cUF, NFCom.ide.dhEmi, NFCom.emit.CNPJ,
    NFCom.ide.serie, NFCom.ide.nNF, StrToInt(TipoEmissaoToStr(NFCom.ide.tpEmis)),
    NFCom.ide.cNF, NFCom.ide.modelo,
    StrToInt(SiteAutorizadorToStr(NFCom.Ide.nSiteAutoriz)));

  NFCom.infNFCom.ID := 'NFCom' + FChaveNFCom;
  NFCom.ide.cDV := ExtrairDigitoChaveAcesso(NFCom.infNFCom.ID);
  NFCom.Ide.cNF := ExtrairCodigoChaveAcesso(NFCom.infNFCom.ID);

  FDocument.Clear();
  NFComNode := FDocument.CreateElement('NFCom', 'http://www.portalfiscal.inf.br/nfcom');

  if NFCom.procNFCom.nProt <> '' then
  begin
    xmlNode := FDocument.CreateElement('NFComProc', 'http://www.portalfiscal.inf.br/nfCom');

    xmlNode.SetAttribute('versao', FloatToString(NFCom.infNFCom.Versao, '.', '#0.00'));

    xmlNode.AppendChild(NFComNode);

    FDocument.Root := xmlNode;
  end
  else
  begin
    FDocument.Root := NFComNode;
  end;

  xmlNode := Gerar_InfNFCom;
  NFComNode.AppendChild(xmlNode);

  if NFCom.infNFComSupl.qrCodNFCom <> '' then
  begin
    xmlNode := NFComNode.AddChild('infNFComSupl');
    xmlNode.AppendChild(AddNode(tcStr, '#318', 'qrCodNFCom', 50, 1000, 1,
     NFCom.infNFComSupl.qrCodNFCom, DSC_QRCODNFCOM));
//     '<![CDATA[' + NFCom.infNFComSupl.qrCodNFCom + ']]>', DSC_QRCODNFCOM, False));
  end;

  if Opcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := True;

    if Opcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NFCom.signature.DigestValue <> '') and
                (NFCom.signature.SignatureValue <> '') and
                (NFCom.signature.X509Certificate <> ''));

    if Opcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((NFCom.signature.DigestValue = '') and
                (NFCom.signature.SignatureValue = '') and
                (NFCom.signature.X509Certificate = ''));

    if Gerar then
    begin
      FNFCom.signature.URI := '#NFCom' + OnlyNumber(NFCom.infNFCom.ID);
      xmlNode := GerarSignature(FNFCom.signature);
      NFComNode.AppendChild(xmlNode);
    end;
  end;

  if NFCom.procNFCom.nProt <> '' then
  begin
    xmlNode := Gerar_ProtNFCom;
    FDocument.Root.AppendChild(xmlNode);
  end;
end;

function TNFComXmlWriter.Gerar_InfNFCom: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infNFCom');
  Result.SetAttribute('Id', 'NFCom' + NFCom.infNFCom.ID);
  Result.SetAttribute('versao', FloatToString(NFCom.infNFCom.Versao, '.', '#0.00'));

  Result.AppendChild(Gerar_Ide);
  Result.AppendChild(Gerar_Emit);
  Result.AppendChild(Gerar_Dest);
  Result.AppendChild(Gerar_Assinante);
  Result.AppendChild(Gerar_gSub);
  Result.AppendChild(Gerar_gCofat);

  nodeArray := Gerar_Det;
  for i := 0 to NFCom.Det.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_Total);
  Result.AppendChild(Gerar_gFidelidade);
  Result.AppendChild(Gerar_gFat);
  Result.AppendChild(Gerar_gFatCentral);

  nodeArray := Gerar_autXML;
  for i := 0 to NFCom.autXML.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_InfAdic);
  Result.AppendChild(Gerar_gRespTec);
end;

function TNFComXmlWriter.Gerar_Ide: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ide');

  Result.AppendChild(AddNode(tcInt, '#5', 'cUF', 2, 2, 1,
                                                       NFCom.ide.cUF, DSC_CUF));

  if not ValidarCodigoUF(NFCom.ide.cUF) then
    wAlerta('#5', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#6', 'tpAmb', 1, 1, 1,
                                TipoAmbienteToStr(NFCom.Ide.tpAmb), DSC_TPAMB));

  Result.AppendChild(AddNode(tcInt, '#7', 'mod', 2, 2, 1,
                                                    NFCom.ide.modelo, DSC_MOD));

  Result.AppendChild(AddNode(tcInt, '#8', 'serie', 1, 3, 1,
                                                   NFCom.ide.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#9', 'nNF', 1, 9, 1,
                                                       NFCom.ide.nNF, DSC_NDF));

  Result.AppendChild(AddNode(tcInt, '#10', 'cNF', 7, 7, 1,
                                                       NFCom.ide.cNF, DSC_CDF));

  Result.AppendChild(AddNode(tcInt, '#11', 'cDV', 1, 1, 1,
                                                       NFCom.Ide.cDV, DSC_CDV));

  Result.AppendChild(AddNode(tcStr, '#12', 'dhEmi', 25, 25, 1,
    DateTimeTodh(NFCom.ide.dhEmi) + GetUTC(CodigoUFparaUF(NFCom.ide.cUF),
    NFCom.ide.dhEmi), DSC_DHEMI));

  Result.AppendChild(AddNode(tcStr, '#13', 'tpEmis', 1, 1, 1,
                               TipoEmissaoToStr(NFCom.Ide.tpEmis), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcInt, '#14', 'nSiteAutoriz', 1, 1, 1,
                                     NFCom.Ide.nSiteAutoriz, DSC_NSITEAUTORIZ));

  Result.AppendChild(AddNode(tcInt, '#15', 'cMunFG', 7, 7, 1,
                                                 NFCom.ide.cMunFG, DSC_CMUNFG));

  if not ValidarMunicipio(NFCom.ide.cMunFG) then
    wAlerta('#14', 'cMunFG', DSC_CMUNFG, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#16', 'finNFCom', 1, 1, 1,
                              finNFComToStr(NFCom.Ide.finNFCom), DSC_FINNFCom));

  Result.AppendChild(AddNode(tcStr, '#17', 'tpFat', 1, 1, 1,
                             TipoFaturamentoToStr(NFCom.Ide.tpFat), DSC_TPFAT));

  Result.AppendChild(AddNode(tcStr, '#18', 'verProc', 1, 20, 1,
                                               NFCom.Ide.verProc, DSC_VERPROC));

  if NFCom.Ide.indPrePago = tiSim  then
    Result.AppendChild(AddNode(tcStr, '#19', 'indPrePago', 1, 1, 1, '1', ''));

  if NFCom.Ide.indCessaoMeiosRede = tiSim  then
    Result.AppendChild(AddNode(tcStr, '#19', 'indCessaoMeiosRede', 1, 1, 1,
                                                                      '1', ''));

  if NFCom.Ide.indNotaEntrada= tiSim  then
    Result.AppendChild(AddNode(tcStr, '#19', 'indNotaEntrada', 1, 1, 1,
                                                                      '1', ''));

  if (NFCom.Ide.dhCont > 0) or (NFCom.Ide.xJust <> '') then
  begin
    Result.AppendChild(AddNode(tcStr, '#21', 'dhCont', 25, 25, 1,
      DateTimeTodh(NFCom.ide.dhCont) + GetUTC(CodigoUFparaUF(NFCom.ide.cUF),
      NFCom.ide.dhCont), DSC_DHCONT));

    Result.AppendChild(AddNode(tcStr, '#22', 'xJust', 15, 256, 1,
                                               NFCom.ide.xJust, DSC_XJUSTCONT));
  end;
end;

function TNFComXmlWriter.Gerar_Emit: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('emit');

  Result.AppendChild(AddNode(tcStr, '#24', 'CNPJ', 14, 14, 1,
                                                    NFCom.Emit.CNPJ, DSC_CNPJ));

  if NFCom.Emit.IE = 'ISENTO' then
    Result.AppendChild(AddNode(tcStr, '#25', 'IE', 2, 14, 1,
                                                         NFCom.Emit.IE, DSC_IE))
  else
    Result.AppendChild(AddNode(tcStr, '#25', 'IE', 2, 14, 1,
                                            OnlyNumber(NFCom.Emit.IE), DSC_IE));

  if (Opcoes.ValidarInscricoes) then
  begin
    if Length(NFCom.Emit.IE) = 0 then
      wAlerta('#25', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not ValidarIE(NFCom.Emit.IE, CodigoUFparaUF(NFCom.Ide.cUF)) then
        wAlerta('#25', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, '#26', 'IEUFDest', 2, 14, 0,
                                            NFCom.Emit.IEUFDest, DSC_IEUFDEST));

  Result.AppendChild(AddNode(tcStr, '#27', 'CRT', 1, 1, 1,
                                            CRTToStr(NFCom.Emit.CRT), DSC_CRT));

  Result.AppendChild(AddNode(tcStr, '#28', 'xNome', 2, 60, 1,
                                                  NFCom.Emit.xNome, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#29', 'xFant', 1, 60, 0,
                                                  NFCom.Emit.xFant, DSC_XFANT));

  Result.AppendChild(Gerar_EmitEnderEmit);
end;

function TNFComXmlWriter.Gerar_EmitEnderEmit: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NFCom.Emit.enderEmit.UF,
    NFCom.Emit.enderEmit.xMun, NFCom.Emit.EnderEmit.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, '#31', 'xLgr', 2, 60, 1,
                                          NFCom.Emit.enderEmit.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#32', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, NFCom.Emit.enderEmit.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#33', 'xCpl', 1, 60, 0,
                                          NFCom.Emit.enderEmit.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#34', 'xBairro', 2, 60, 1,
                                    NFCom.Emit.enderEmit.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#35', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#35', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#36', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#37', 'CEP', 8, 8, 1,
                                            NFCom.Emit.enderEmit.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#38', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#38', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#39', 'fone', 7, 12, 0,
                              OnlyNumber(NFCom.Emit.enderEmit.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#40', 'email', 1, 60, 0,
                                        NFCom.Emit.enderEmit.email, DSC_EMAIL));
end;

function TNFComXmlWriter.Gerar_Dest: TACBrXmlNode;
var
  UF, xNome, nIE: string;
const
  HOM_NOME_DEST = 'NFCOM EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
begin
  UF := '';
  Result := FDocument.CreateElement('dest');

  if NFCom.Ide.tpAmb = TACBrTipoAmbiente(taProducao) then
    xNome := NFCom.Dest.xNome
  else
    xNome := HOM_NOME_DEST;

  Result.AppendChild(AddNode(tcStr, '#42', 'xNome', 2, 60, 1,
                                                             xNome, DSC_XNOME));

  if NFCom.Dest.idOutros <> '' then
    Result.AppendChild(AddNode(tcStr, '#45', 'idOutros', 2, 20, 1,
                                               NFCom.Dest.idOutros, DSC_IDESTR))
  else
    Result.AppendChild(AddNodeCNPJCPF('#43', '#44', NFCom.Dest.CNPJCPF));

  Result.AppendChild(AddNode(tcStr, '#46', 'indIEDest', 1, 1, 1,
                          indIEDestToStr(NFCom.Dest.indIEDest), DSC_INDIEDEST));

  if NFCom.Dest.indIEDest <> inIsento then
  begin
    nIE := NFCom.Dest.IE;

    if nIE <> '' then
    begin
      if nIE <> 'ISENTO' then
        nIE := OnlyNumber(NFCom.Dest.IE);

      Result.AppendChild(AddNode(tcStr, '#47', 'IE', 0, 14, 1, nIE, DSC_IE));

      if (Opcoes.ValidarInscricoes) and (nIE <> 'ISENTO') then
        if not ValidarIE(nIE, UF) then
          wAlerta('#47', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, '#48', 'IM', 1, 15, 0,
                                                        NFCom.Dest.IM, DSC_IM));

  Result.AppendChild(Gerar_DestEnderDest(UF));
end;

function TNFComXmlWriter.Gerar_DestEnderDest(var UF: string): TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NFCom.Dest.enderDest.UF,
    NFCom.Dest.enderDest.xMun, NFCom.Dest.enderDest.cMun);

  UF := xUF;
  Result := FDocument.CreateElement('enderDest');

  Result.AppendChild(AddNode(tcStr, '#50', 'xLgr', 2, 60, 1,
                                          NFCom.Dest.enderDest.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#51', 'nro', 01, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, NFCom.Dest.enderDest.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#52', 'xCpl', 1, 60, 0,
                                          NFCom.Dest.enderDest.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#53', 'xBairro', 1, 60, 1,
                                    NFCom.Dest.enderDest.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#54', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#54', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#55', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#56', 'CEP', 8, 8, 1,
                                            NFCom.Dest.enderDest.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#57', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#57', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#58', 'fone', 7, 12, 0,
                              OnlyNumber(NFCom.Dest.enderDest.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#59', 'email', 01, 60, 0,
                                        NFCom.Dest.EnderDest.Email, DSC_EMAIL));
end;

function TNFComXmlWriter.Gerar_Assinante: TACBrXmlNode;
var
  i: integer;
begin
  Result := FDocument.CreateElement('assinante');

  Result.AppendChild(AddNode(tcStr, '#61', 'iCodAssinante', 1, 15, 1,
                             NFCom.assinante.iCodAssinante, DSC_ICODASSINANTE));

  Result.AppendChild(AddNode(tcStr, '#62', 'tpAssinante', 2, 2, 1,
               tpAssinanteToStr(NFCom.assinante.tpAssinante), DSC_TPASSINANTE));

  Result.AppendChild(AddNode(tcStr, '#63', 'tpServUtil', 1, 1, 1,
                  tpServUtilToStr(NFCom.assinante.tpServUtil), DSC_TPSERVUTIL));

  Result.AppendChild(AddNode(tcStr, '#64', 'nContrato', 1, 20, 1,
                                     NFCom.assinante.nContrato, DSC_NCONTRATO));

  Result.AppendChild(AddNode(tcDat, '#65', 'dContratoIni', 10, 10, 1,
                               NFCom.assinante.dContratoIni, DSC_DCONTRATOINI));

  Result.AppendChild(AddNode(tcDat, '#66', 'dContratoFim', 10, 10, 0,
                               NFCom.assinante.dContratoFim, DSC_DCONTRATOFIM));

  if NFCom.assinante.NroTermPrinc <> '' then
  begin
    Result.AppendChild(AddNode(tcStr, '#68', 'NroTermPrinc', 7, 12, 1,
                               NFCom.assinante.NroTermPrinc, DSC_NROTERMPRINC));

    Result.AppendChild(AddNode(tcInt, '#69', 'cUFPrinc', 2, 2, 1,
                                            NFCom.assinante.cUFPrinc, DSC_CUF));
  end;

  for i := 0 to NFCom.assinante.TermAdic.Count - 1 do
  begin
    Result.AppendChild(AddNode(tcStr, '#71', 'NroTermAdic', 7, 12, 1,
                     NFCom.assinante.TermAdic[i].NroTermAdic, DSC_NROTERMADIC));

    Result.AppendChild(AddNode(tcInt, '#72', 'cUFAdic', 2, 2, 1,
                                 NFCom.assinante.TermAdic[i].cUFAdic, DSC_CUF));
  end;
end;

function TNFComXmlWriter.Gerar_gSub: TACBrXmlNode;
begin
  Result := nil;

  if (NFCom.gSub.chNFCom <> '') or (NFCom.gSub.gNF.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('gSub');

    if NFCom.gSub.chNFCom <> '' then
    begin
      Result.AppendChild(AddNode(tcStr, '#75', 'chNFCom', 44, 44, 1,
        NFCom.gSub.chNFCom, DSC_CHNFCom));

      if not ValidarChave(NFCom.gSub.chNFCom) then
        wAlerta('#75', 'chNFCom', DSC_CHNFCom, ERR_MSG_INVALIDO);
    end
    else
      Result.AppendChild(Gerar_gNF);

    Result.AppendChild(AddNode(tcStr, '#83', 'motSub', 2, 2, 1,
                                   motSubToStr(NFCom.gSub.motSub), DSC_MOTSUB));
  end;
end;

function TNFComXmlWriter.Gerar_gNF: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gNF');

  Result.AppendChild(AddNode(tcStr, '#77', 'CNPJ', 14, 14, 1,
                                                NFCom.gSub.gNF.CNPJ, DSC_CNPJ));

  Result.AppendChild(AddNode(tcInt, '#78', 'mod', 2, 2, 1,
                                            NFCom.gSub.gNF.Modelo, DSC_MOD));

  Result.AppendChild(AddNode(tcStr, '#79', 'serie', 1, 3, 1,
                                              NFCom.gSub.gNF.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcStr, '#80', 'nNF', 1, 9, 1,
                                                  NFCom.gSub.gNF.nNF, DSC_NDF));

  Result.AppendChild(AddNode(tcStr, '#81', 'CompetEmis', 6, 6, 1,
          FormatDateTime('yyyymm', NFCom.gSub.gNF.CompetEmis), DSC_COMPETEMIS));

  Result.AppendChild(AddNode(tcStr, '#82', 'hash115', 32, 32, 0,
                                          NFCom.gSub.gNF.hash115, DSC_HASH115));
end;

function TNFComXmlWriter.Gerar_gCofat: TACBrXmlNode;
begin
  Result := nil;

  if (NFCom.gCofat.chNFComLocal <> '') then
  begin
    Result := FDocument.CreateElement('gCofat');

    Result.AppendChild(AddNode(tcStr, '#85', 'chNFComLocal', 44, 44, 1,
                                       NFCom.gCofat.chNFComLocal, DSC_CHNFCOM));

    if not ValidarChave(NFCom.gCofat.chNFComLocal) then
      wAlerta('#085', 'chNFComLocal', DSC_CHNFCOM, ERR_MSG_INVALIDO);
  end;
end;

function TNFComXmlWriter.Gerar_det: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;

  SetLength(Result, NFCom.Det.Count);

  for i := 0 to NFCom.Det.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('det');

    Result[i].SetAttribute('nItem', FormatFloat('###0', NFCom.Det[i].nItem));

    if NFCom.Det[i].chNFComAnt <> '' then
      Result[i].SetAttribute('chNFComAnt', NFCom.Det[i].chNFComAnt);

    if NFCom.Det[i].nItemAnt > 0 then
      Result[i].SetAttribute('nItemAnt', FormatFloat('000', NFCom.Det[i].nItemAnt));

    Result[i].AppendChild(Gerar_det_prod(i));
    Result[i].AppendChild(Gerar_det_imposto(i));
    Result[i].AppendChild(Gerar_det_gProcRef(i));
    Result[i].AppendChild(Gerar_det_gRessarc(i));

    Result[i].AppendChild(AddNode(tcStr, '#92', 'infAdProd', 1, 500, 0,
                                        NFCom.Det[i].infAdProd, DSC_INFADPROD));
  end;

  if NFCom.Det.Count > 9990 then
    wAlerta('#87', 'det', '', ERR_MSG_MAIOR_MAXIMO + '9990');
end;

function TNFComXmlWriter.Gerar_det_prod(adet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('prod');

  Result.AppendChild(AddNode(tcStr, '#92', 'cProd', 1, 60, 1,
                                        NFCom.Det[adet].Prod.cProd, DSC_CPROD));

  Result.AppendChild(AddNode(tcStr, '#93', 'xProd', 1, 120, 1,
                                        NFCom.Det[aDet].Prod.xProd, DSC_XPROD));

  Result.AppendChild(AddNode(tcStr, '#94', 'cClass', 7, 7, 1,
                                      NFCom.Det[aDet].Prod.cClass, DSC_CCLASS));

  Result.AppendChild(AddNode(tcInt, '#95', 'CFOP', 4, 4, 0,
                                          NFCom.Det[aDet].Prod.CFOP, DSC_CFOP));

  Result.AppendChild(AddNode(tcStr, '#96', 'CNPJLD', 14, 14, 0,
                                        NFCom.Det[aDet].Prod.CNPJLD, DSC_CNPJ));

  Result.AppendChild(AddNode(tcStr, '#166', 'uMed', 1, 1, 1,
                               uMedToStr(NFCom.Det[aDet].Prod.uMed), DSC_UMED));

  if Frac(NFCom.Det[aDet].Prod.qFaturada) > 0 then
    Result.AppendChild(AddNode(tcDe4, '#167', 'qFaturada', 1, 15, 1,
                                 NFCom.Det[aDet].Prod.qFaturada, DSC_QFATURADA))
  else
    Result.AppendChild(AddNode(tcInt, '#167', 'qFaturada', 1, 15, 1,
                                NFCom.Det[aDet].Prod.qFaturada, DSC_QFATURADA));

  // pode ter 2 ou 8 casas decimais
  Result.AppendChild(AddNode(tcDe2, '#168', 'vItem', 1, 15, 1,
                                        NFCom.Det[aDet].Prod.vItem, DSC_VITEM));

  Result.AppendChild(AddNode(tcDe2, '#168', 'vDesc', 1, 15, 0,
                                        NFCom.Det[aDet].Prod.vDesc, DSC_VDESC));

  Result.AppendChild(AddNode(tcDe2, '#168', 'vOutro', 1, 15, 0,
                                      NFCom.Det[aDet].Prod.vOutro, DSC_VOUTRO));

  // pode ter 2 ou 8 casas decimais
  Result.AppendChild(AddNode(tcDe2, '#169', 'vProd', 1, 15, 1,
                                        NFCom.Det[aDet].Prod.vProd, DSC_VPROD));

  Result.AppendChild(AddNode(tcDat, '#169', 'dExpiracao', 10, 10, 0,
                              NFCom.Det[aDet].Prod.dExpiracao, DSC_DEXPIRACAO));

  if NFCom.Det[aDet].Prod.indDevolucao = tiSim then
    Result.AppendChild(AddNode(tcStr, '#170', 'indDevolucao', 1, 1, 1, '1', ''));
end;

function TNFComXmlWriter.Gerar_det_imposto(adet: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('imposto');

  if NFCom.Det[aDet].Imposto.indSemCST = tiSim  then
    Result.AppendChild(AddNode(tcStr, '#153', 'indSemCST', 1, 1, 1, '1'))
  else
  begin
    Result.AppendChild(Gerar_det_imposto_ICMS(aDet));

    nodeArray := Gerar_det_imposto_ICMSUFDest(aDet);
    for i := 0 to NFCom.Det[aDet].Imposto.ICMSUFDest.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;

  Result.AppendChild(Gerar_det_imposto_PIS(aDet));
  Result.AppendChild(Gerar_det_imposto_COFINS(aDet));
  Result.AppendChild(Gerar_det_imposto_FUST(aDet));
  Result.AppendChild(Gerar_det_imposto_FUNTTEL(aDet));
  Result.AppendChild(Gerar_det_imposto_retTrib(aDet));
end;

function TNFComXmlWriter.Gerar_det_imposto_ICMS(aDet: Integer): TACBrXmlNode;

  function BuscaTag(const t: TCSTIcms): String;
  begin
    case t of
      cst00: result := '00';
      cst20: result := '20';
      cst40,
      cst41: result := '40';
      cst51: result := '51';
      cst90: result := '90';
    else
      Result := 'SN';
    end;
  end;

var
  sTagTemp: String;

begin
  with NFCom.Det[aDet].Imposto.ICMS do
  begin
    sTagTemp := BuscaTag(CST);

    Result := FDocument.CreateElement('ICMS' + sTagTemp);

    Result.AppendChild(AddNode(tcStr, '#174', 'CST', 2, 2, 1,
                                                   CSTICMSTOStr(CST), DSC_CST));

    case CST of
      cst00:
        begin
          Result.AppendChild(AddNode(tcDe2, '#175', 'vBC', 1, 15, 1,
                                                                 vBC, DSC_VBC));

          Result.AppendChild(AddNode(tcDe2, '#176', 'pICMS', 1, 5, 1,
                                                             pICMS, DSC_PICMS));

          Result.AppendChild(AddNode(tcDe2, '#177', 'vICMS', 1, 15, 1,
                                                             vICMS, DSC_VICMS));

          if (pFCP > 0) or (vFCP > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '#178', 'pFCP', 1, 5, 1,
                                                               pFCP, DSC_PFCP));

            Result.AppendChild(AddNode(tcDe2, '#179', 'vFCP', 1, 15, 1,
                                                               vFCP, DSC_VFCP));
          end;
        end;

      cst20:
        begin
          Result.AppendChild(AddNode(tcDe2, '#189', 'pRedBC', 1, 5, 1,
                                                           pRedBC, DSC_PREDBC));

          Result.AppendChild(AddNode(tcDe2, '#190', 'vBC', 1, 15, 1,
                                                                 vBC, DSC_VBC));

          Result.AppendChild(AddNode(tcDe2, '#191', 'pICMS', 1, 5, 1,
                                                             pICMS, DSC_PICMS));

          Result.AppendChild(AddNode(tcDe2, '#192', 'vICMS', 1, 15, 1,
                                                             vICMS, DSC_VICMS));

          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#193', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#194', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;

          if (pFCP > 0) or (vFCP > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '#195', 'pFCP', 1, 5, 1,
                                                               pFCP, DSC_PFCP));

            Result.AppendChild(AddNode(tcDe2, '#196', 'vFCP', 1, 15, 1,
                                                               vFCP, DSC_VFCP));
          end;
        end;

      cst40,
      cst41:
        begin
          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#199', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#200', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;
        end;

      cst51:
        begin
          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#203', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#204', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;
        end;

      cst90:
        begin
          Result.AppendChild(AddNode(tcDe2, '#207', 'vBC', 1, 15, 1,
                                                                 vBC, DSC_VBC));

          Result.AppendChild(AddNode(tcDe2, '#208', 'pICMS', 1, 5, 1,
                                                             pICMS, DSC_PICMS));

          Result.AppendChild(AddNode(tcDe2, '#209', 'vICMS', 1, 15, 1,
                                                             vICMS, DSC_VICMS));
        end;

    else
      begin
        // cstICMSSN
        if indSN = tiSim then
          Result.AppendChild(AddNode(tcStr, '#209', 'indSN', 1, 1, 1, '1'));
      end;
    end;
  end;
end;

function TNFComXmlWriter.Gerar_det_imposto_ICMSUFDest(
  aDet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  result := nil;

  SetLength(Result, NFCom.Det[aDet].Imposto.ICMSUFDest.Count);

  for i := 0 to NFCom.Det[aDet].Imposto.ICMSUFDest.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('ICMSUFDest');

    Result[i].SetAttribute('cUFDest',
              FormatFloat('00', NFCom.Det[aDet].Imposto.ICMSUFDest[i].cUFDest));

    Result[i].AppendChild(AddNode(tcDe2, '#147', 'vBCUFDest', 1, 15, 1,
               NFCom.Det[aDet].Imposto.ICMSUFDest[i].vBCUFDest, DSC_VBCUFDEST));

    Result[i].AppendChild(AddNode(tcDe2, '#148', 'pFCPUFDest', 1, 5, 1,
             NFCom.Det[aDet].Imposto.ICMSUFDest[i].pFCPUFDest, DSC_PFCPUFDEST));

    Result[i].AppendChild(AddNode(tcDe2, '#149', 'pICMSUFDest', 1, 5, 1,
           NFCom.Det[aDet].Imposto.ICMSUFDest[i].pICMSUFDest, DSC_PICMSUFDEST));

    Result[i].AppendChild(AddNode(tcDe2, '#150', 'vFCPUFDest', 1, 15, 1,
             NFCom.Det[aDet].Imposto.ICMSUFDest[i].vFCPUFDest, DSC_VFCPUFDEST));

    Result[i].AppendChild(AddNode(tcDe2, '#151', 'vICMSUFDest', 1, 15, 1,
           NFCom.Det[aDet].Imposto.ICMSUFDest[i].vICMSUFDest, DSC_VICMSUFDEST));

    Result[i].AppendChild(AddNode(tcDe2, '#152', 'vICMSUFEmi', 1, 15, 1,
             NFCom.Det[aDet].Imposto.ICMSUFDest[i].vICMSUFEmi, DSC_VICMSUFEMI));

    Result[i].AppendChild(AddNode(tcStr, '#152', 'cBenefUFDest', 10, 10, 0,
               NFCom.Det[aDet].Imposto.ICMSUFDest[i].cBenefUFDest, DSC_CBENEF));
  end;

  if NFCom.Det[aDet].Imposto.ICMSUFDest.Count > 999 then
    wAlerta('#105', 'ICMSUFDest', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TNFComXmlWriter.Gerar_det_imposto_PIS(aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NFCom.Det[aDet].Imposto.PIS do
  begin
    if (vBC > 0) or (pPIS > 0) or (vPIS > 0) then
    begin
      Result := FDocument.CreateElement('PIS');

      Result.AppendChild(AddNode(tcStr, '#211', 'CST', 2, 2, 1,
                                                    CSTPISToStr(CST), DSC_CST));

      Result.AppendChild(AddNode(tcDe2, '#212', 'vBC', 1, 15, 1, vBC, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#213', 'pPIS', 1, 5, 1,
                                                               pPIS, DSC_PPIS));

      Result.AppendChild(AddNode(tcDe2, '#214', 'vPIS', 1, 15, 1,
                                                               vPIS, DSC_VPIS));
    end;
  end;
end;

function TNFComXmlWriter.Gerar_det_imposto_COFINS(aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NFCom.Det[aDet].Imposto.COFINS do
  begin
    if (vBC > 0) or (pCOFINS > 0) or (vCOFINS > 0) then
    begin
      Result := FDocument.CreateElement('COFINS');

      Result.AppendChild(AddNode(tcStr, '#220', 'CST', 2, 2, 1,
                                                 CSTCOFINSToStr(CST), DSC_CST));

      Result.AppendChild(AddNode(tcDe2, '#221', 'vBC', 1, 15, 1, vBC, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#222', 'pCOFINS', 1, 5, 1,
                                                         pCOFINS, DSC_PCOFINS));

      Result.AppendChild(AddNode(tcDe2, '#223', 'vCOFINS', 1, 15, 1,
                                                         vCOFINS, DSC_VCOFINS));
    end;
  end;
end;

function TNFComXmlWriter.Gerar_det_imposto_FUST(aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NFCom.Det[aDet].Imposto.FUST do
  begin
    if (vBC > 0) or (pFUST > 0) or (vFUST > 0) then
    begin
      Result := FDocument.CreateElement('FUST');

      Result.AppendChild(AddNode(tcDe2, '#221', 'vBC', 1, 15, 1, vBC, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#222', 'pFUST', 1, 5, 1,
                                                         pFUST, DSC_PFUST));

      Result.AppendChild(AddNode(tcDe2, '#223', 'vFUST', 1, 15, 1,
                                                         vFUST, DSC_VFUST));
    end;
  end;
end;

function TNFComXmlWriter.Gerar_det_imposto_FUNTTEL(aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NFCom.Det[aDet].Imposto.FUNTTEL do
  begin
    if (vBC > 0) or (pFUNTTEL > 0) or (vFUNTTEL > 0) then
    begin
      Result := FDocument.CreateElement('FUNTTEL');

      Result.AppendChild(AddNode(tcDe2, '#221', 'vBC', 1, 15, 1, vBC, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#222', 'pFUNTTEL', 1, 5, 1,
                                                       pFUNTTEL, DSC_PFUNTTEL));

      Result.AppendChild(AddNode(tcDe2, '#223', 'vFUNTTEL', 1, 15, 1,
                                                       vFUNTTEL, DSC_VFUNTTEL));
    end;
  end;
end;

function TNFComXmlWriter.Gerar_det_imposto_retTrib(aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('retTrib');

  Result.AppendChild(AddNode(tcDe2, '#123c', 'vRetPIS', 1, 15, 1,
                         NFCom.Det[aDet].Imposto.retTrib.vRetPIS, DSC_VRETPIS));

  Result.AppendChild(AddNode(tcDe2, '#123d', 'vRetCofins', 1, 15, 1,
                   NFCom.Det[aDet].Imposto.retTrib.vRetCOFINS, DSC_VRETCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#12Com', 'vRetCSLL', 1, 15, 1,
                       NFCom.Det[aDet].Imposto.retTrib.vRetCSLL, DSC_VRETCSLL));

  Result.AppendChild(AddNode(tcDe2, '#123f', 'vBCIRRF', 1, 15, 1,
                         NFCom.Det[aDet].Imposto.retTrib.vBCIRRF, DSC_VBCIRRF));

  Result.AppendChild(AddNode(tcDe2, '#123g', 'vIRRF', 1, 15, 1,
                             NFCom.Det[aDet].Imposto.retTrib.vIRRF, DSC_VIRRF));
end;

function TNFComXmlWriter.Gerar_det_gProcRef(aDet: Integer): TACBrXmlNode;
var
  i: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if NFCom.Det[aDet].gProcRef.vItem > 0 then
  begin
    Result := FDocument.CreateElement('gProcRef');

    // pode ter 2 ou 6 casas decimais
    Result.AppendChild(AddNode(tcDe2, '#235', 'vItem', 1, 15, 1,
                                    NFCom.Det[aDet].gProcRef.vItem, DSC_VITEM));

    if Frac(NFCom.Det[aDet].gProcRef.qFaturada) > 0 then
      Result.AppendChild(AddNode(tcDe4, '#236', 'qFaturada', 1, 15, 1,
                             NFCom.Det[aDet].gProcRef.qFaturada, DSC_QFATURADA))
    else
      Result.AppendChild(AddNode(tcInt, '#236', 'qFaturada', 1, 15, 1,
                            NFCom.Det[aDet].gProcRef.qFaturada, DSC_QFATURADA));

    // pode ter 2 ou 6 casas decimais
    Result.AppendChild(AddNode(tcDe2, '#237', 'vProd', 1, 15, 1,
                                    NFCom.Det[aDet].gProcRef.vProd, DSC_VPROD));

    Result.AppendChild(AddNode(tcDe2, '#237', 'vDesc', 1, 15, 0,
                                    NFCom.Det[aDet].gProcRef.vDesc, DSC_VDESC));

    Result.AppendChild(AddNode(tcDe2, '#237', 'vOutro', 1, 15, 0,
                                  NFCom.Det[aDet].gProcRef.vOutro, DSC_VOUTRO));

    if NFCom.Det[aDet].gProcRef.indDevolucao = tiSim then
      Result.AppendChild(AddNode(tcStr, '#238', 'indDevolucao', 1, 1, 1, '1', ''));

    Result.AppendChild(AddNode(tcDe2, '#239', 'vBC', 1, 15, 0,
                                        NFCom.Det[aDet].gProcRef.vBC, DSC_VBC));

    Result.AppendChild(AddNode(tcDe2, '#240', 'pICMS', 1, 5, 0,
                                    NFCom.Det[aDet].gProcRef.pICMS, DSC_PICMS));

    Result.AppendChild(AddNode(tcDe2, '#241', 'vICMS', 1, 15, 0,
                                    NFCom.Det[aDet].gProcRef.vICMS, DSC_VICMS));

    Result.AppendChild(AddNode(tcDe2, '#242', 'vPIS', 1, 15, 0,
                                      NFCom.Det[aDet].gProcRef.vPIS, DSC_VPIS));

    Result.AppendChild(AddNode(tcDe2, '#243', 'vCOFINS', 1, 15, 0,
                                NFCom.Det[aDet].gProcRef.vCOFINS, DSC_VCOFINS));

    nodeArray := Gerar_det_gProcRef_gProc(aDet);
    for i := 0 to NFCom.Det[aDet].gProcRef.gProc.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFComXmlWriter.Gerar_det_gProcRef_gProc(aDet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;

  SetLength(Result, NFCom.Det[aDet].gProcRef.gProc.Count);

  for i := 0 to NFCom.Det[aDet].gProcRef.gProc.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gProc');

    Result[i].AppendChild(AddNode(tcStr, '#245', 'tpProc', 1, 1, 1,
            tpProcToStr(NFCom.Det[aDet].gProcRef.gProc[i].tpProc), DSC_TPPROC));

    Result[i].AppendChild(AddNode(tcStr, '#246', 'nProcesso', 1, 60, 1,
                   NFCom.Det[aDet].gProcRef.gProc[i].nProcesso, DSC_NPROCESSO));
  end;

  if NFCom.Det[aDet].gProcRef.gProc.Count > 10 then
    wAlerta('#244', 'gProc', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFComXmlWriter.Gerar_det_gRessarc(aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  if NFCom.Det[aDet].gRessarc.dRef > 0 then
  begin
    Result := FDocument.CreateElement('gRessarc');

    Result.AppendChild(AddNode(tcStr, '#235', 'tpRessarc', 1, 2, 1,
            tpRessarcToStr(NFCom.Det[aDet].gRessarc.tpRessarc), DSC_TPRESSARC));

    Result.AppendChild(AddNode(tcDat, '#235', 'dRef', 10, 10, 1,
                                      NFCom.Det[aDet].gRessarc.dRef, DSC_DREF));

    Result.AppendChild(AddNode(tcStr, '#237', 'nProcesso', 1, 60, 0,
                            NFCom.Det[aDet].gRessarc.nProcesso, DSC_NPROCESSO));

    Result.AppendChild(AddNode(tcStr, '#237', 'nProtReclama', 1, 60, 0,
                      NFCom.Det[aDet].gRessarc.nProtReclama, DSC_NPROTRECLAMA));

    Result.AppendChild(AddNode(tcStr, '#237', 'xObs', 15, 100, 0,
                                      NFCom.Det[aDet].gRessarc.xObs, DSC_XOBS));
  end;
end;

function TNFComXmlWriter.Gerar_Total: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('total');

  Result.AppendChild(AddNode(tcDe2, '#254', 'vProd', 1, 15, 1,
                                                 NFCom.Total.vProd, DSC_VPROD));

  Result.AppendChild(Gerar_Total_ICMSTotal);

  Result.AppendChild(AddNode(tcDe2, '#268', 'vCOFINS', 1, 15, 1,
                                             NFCom.Total.vCOFINS, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#270', 'vPIS', 1, 15, 1,
                                                   NFCom.Total.vPIS, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#269', 'vFUNTTEL', 1, 15, 1,
                                           NFCom.Total.vFUNTTEL, DSC_VFUNTTEL));

  Result.AppendChild(AddNode(tcDe2, '#271', 'vFUST', 1, 15, 1,
                                                 NFCom.Total.vFUST, DSC_VFUST));

  Result.AppendChild(Gerar_Total_vRetTribTot);

  Result.AppendChild(AddNode(tcDe2, '#271', 'vDesc', 1, 15, 1,
                                                 NFCom.Total.vDesc, DSC_VDESC));

  Result.AppendChild(AddNode(tcDe2, '#271', 'vOutro', 1, 15, 1,
                                               NFCom.Total.vOutro, DSC_VOUTRO));

  Result.AppendChild(AddNode(tcDe2, '#271', 'vNF', 1, 15, 1,
                                                     NFCom.Total.vNF, DSC_VNF));
end;

function TNFComXmlWriter.Gerar_Total_ICMSTotal: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMSTot');

  Result.AppendChild(AddNode(tcDe2, '#256', 'vBC', 1, 15, 1,
                                                     NFCom.Total.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#257', 'vICMS', 1, 15, 1,
                                                 NFCom.Total.vICMS, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, '#258', 'vICMSDeson', 1, 15, 1,
                                       NFCom.Total.vICMSDeson, DSC_VICMSDESON));

  Result.AppendChild(AddNode(tcDe2, '#259', 'vFCP', 1, 15, 1,
                                                   NFCom.Total.vFCP, DSC_VFCP));
end;

function TNFComXmlWriter.Gerar_Total_vRetTribTot: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('vRetTribTot');

  Result.AppendChild(AddNode(tcDe2, '#264', 'vRetPIS', 1, 15, 1,
                                             NFCom.Total.vRetPIS, DSC_VRETPIS));

  Result.AppendChild(AddNode(tcDe2, '#265', 'vRetCofins', 1, 15, 1,
                                       NFCom.Total.vRetCofins, DSC_VRETCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#266', 'vRetCSLL', 1, 15, 1,
                                           NFCom.Total.vRetCSLL, DSC_VRETCSLL));

  Result.AppendChild(AddNode(tcDe2, '#267', 'vIRRF', 1, 15, 1,
                                                 NFCom.Total.vIRRF, DSC_VIRRF));
end;

function TNFComXmlWriter.Gerar_gFidelidade: TACBrXmlNode;
begin
  Result := nil;

  if NFCom.gFidelidade.qtdSaldoPts <> '' then
  begin
    Result := FDocument.CreateElement('gFidelidade');

    Result.AppendChild(AddNode(tcStr, '#274', 'qtdSaldoPts', 1, 20, 1,
                               NFCom.gFidelidade.qtdSaldoPts, DSC_QTDSALDOPTS));

    Result.AppendChild(AddNode(tcDat, '#275', 'dRefSaldoPts', 10, 10, 1,
                             NFCom.gFidelidade.dRefSaldoPts, DSC_DREFSALDOPTS));

    Result.AppendChild(AddNode(tcStr, '#274', 'qtdPtsResg', 1, 20, 1,
                                 NFCom.gFidelidade.qtdPtsResg, DSC_QTDPTSRESG));

    Result.AppendChild(AddNode(tcDat, '#275', 'dRefResgPts', 10, 10, 1,
                               NFCom.gFidelidade.dRefResgPts, DSC_DREFRESGPTS));
  end;
end;

function TNFComXmlWriter.Gerar_gFat: TACBrXmlNode;
begin
  Result := nil;

  if NFCom.gFat.dVencFat > 0 then
  begin
    Result := FDocument.CreateElement('gFat');

    Result.AppendChild(AddNode(tcStr, '#274', 'CompetFat', 6, 6, 1,
                FormatDateTime('yyyymm', NFCom.gFat.CompetFat), DSC_COMPETFAT));

    Result.AppendChild(AddNode(tcDat, '#275', 'dVencFat', 10, 10, 1,
                                               NFCom.gFat.dVencFat, DSC_DVENC));

    if (NFCom.gFat.dPerUsoIni > 0) or (NFCom.gFat.dPerUsoFim > 0) then
    begin
      Result.AppendChild(AddNode(tcDat, '#276', 'dPerUsoIni', 10, 10, 1,
                                        NFCom.gFat.dPerUsoIni, DSC_DPERUSOINI));

      Result.AppendChild(AddNode(tcDat, '#277', 'dPerUsoFim', 10, 10, 1,
                                        NFCom.gFat.dPerUsoFim, DSC_DPERUSOFIM));
    end;

    Result.AppendChild(AddNode(tcStr, '#279', 'codBarras', 1, 48, 1,
                                          NFCom.gFat.codBarras, DSC_CODBARRAS));

    if NFCom.gFat.codDebAuto <> '' then
      Result.AppendChild(AddNode(tcStr, '#280', 'codDebAuto', 1, 20, 1,
                                         NFCom.gFat.codDebAuto, DSC_CODDEBAUTO))
    else
    begin
      Result.AppendChild(AddNode(tcStr, '#281', 'codBanco', 3, 5, 1,
                                            NFCom.gFat.codBanco, DSC_CODBANCO));

      Result.AppendChild(AddNode(tcStr, '#282', 'codAgencia', 1, 10, 1,
                                        NFCom.gFat.codAgencia, DSC_CODAGENCIA));
    end;

    if NFCom.gFat.enderCorresp.xLgr <> '' then
      Result.AppendChild(Gerar_gFat_enderCorresp);

    if NFCom.gFat.gPIX.urlQRCodePIX <> '' then
      Result.AppendChild(Gerar_gFat_gPix);
  end;
end;

function TNFComXmlWriter.Gerar_gFat_enderCorresp: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NFCom.gFat.enderCorresp.UF,
    NFCom.gFat.enderCorresp.xMun, NFCom.gFat.enderCorresp.cMun);

  Result := FDocument.CreateElement('enderCorresp');

  Result.AppendChild(AddNode(tcStr, '#284', 'xLgr', 2, 60, 1,
                                       NFCom.gFat.enderCorresp.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#285', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, NFCom.gFat.enderCorresp.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#286', 'xCpl', 1, 60, 0,
                                       NFCom.gFat.enderCorresp.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#287', 'xBairro', 2, 60, 1,
                                 NFCom.gFat.enderCorresp.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#288', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#288', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#289', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#290', 'CEP', 8, 8, 1,
                                         NFCom.gFat.enderCorresp.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#291', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#291', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#292', 'fone', 7, 12, 0,
                           OnlyNumber(NFCom.gFat.enderCorresp.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#293', 'email', 1, 60, 0,
                                     NFCom.gFat.enderCorresp.email, DSC_EMAIL));
end;

function TNFComXmlWriter.Gerar_gFat_gPix: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gPIX');

  Result.AppendChild(AddNode(tcStr, '#295', 'urlQRCodePIX', 2, 2000, 1,
                               NFCom.gFat.gPIX.urlQRCodePIX, DSC_URLQRCODEPIX));
end;

function TNFComXmlWriter.Gerar_gFatCentral: TACBrXmlNode;
begin
  Result := nil;

  if NFCom.gFatCentral.CNPJ <> '' then
  begin
    Result := FDocument.CreateElement('gFatCentral');

    Result.AppendChild(AddNode(tcStr, '#274', 'CNPJ', 14, 14, 1,
                                             NFCom.gFatCentral.CNPJ, DSC_CNPJ));

    Result.AppendChild(AddNode(tcInt, '#291', 'cUF', 2, 2, 1,
                                               NFCom.gFatCentral.cUF, DSC_CUF));
  end;
end;

function TNFComXmlWriter.Gerar_autXML: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;

  SetLength(Result, NFCom.autXML.Count);

  for i := 0 to NFCom.autXML.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('autXML');

    Result[i].AppendChild(AddNodeCNPJCPF('#305', '#306', NFCom.autXML[i].CNPJCPF));
  end;

  if NFCom.autXML.Count > 10 then
    wAlerta('#304', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFComXmlWriter.Gerar_InfAdic: TACBrXmlNode;
begin
  Result := nil;

  if (trim(NFCom.InfAdic.infAdFisco) <> '') or (trim(NFCom.InfAdic.infCpl) <> '') then
  begin
    Result := FDocument.CreateElement('infAdic');

    Result.AppendChild(AddNode(tcStr, '#308', 'infAdFisco', 1, 2000, 0,
                                     NFCom.InfAdic.infAdFisco, DSC_INFADFISCO));

    Result.AppendChild(AddNode(tcStr, '#309', 'infCpl', 1, 5000, 0,
                                             NFCom.InfAdic.infCpl, DSC_INFCPL));
  end;
end;

function TNFComXmlWriter.Gerar_gRespTec: TACBrXmlNode;
begin
  Result := nil;

  if (NFCom.infRespTec.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('gRespTec');

    Result.AppendChild(AddNodeCNPJ('#311', NFCom.infRespTec.CNPJ,
                                                          CODIGO_BRASIL, True));

    Result.AppendChild(AddNode(tcStr, '#312', 'xContato', 2, 60, 1,
                                      NFCom.infRespTec.xContato, DSC_XCONTATO));

    Result.AppendChild(AddNode(tcStr, '#313', 'email', 6, 60, 1,
                                            NFCom.infRespTec.email, DSC_EMAIL));

    Result.AppendChild(AddNode(tcStr, '#314', 'fone', 7, 12, 1,
                                              NFCom.infRespTec.fone, DSC_FONE));

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Result.AppendChild(AddNode(tcInt, '#315', 'idCSRT', 2, 2, 1,
                                                           idCSRT, DSC_IDCSRT));

      Result.AppendChild(AddNode(tcStr, '#316', 'hashCSRT', 28, 28, 1,
                             CalcularHashCSRT(CSRT, FChaveNFCom), DSC_HASHCSRT));
    end;
  end;
end;

function TNFComXmlWriter.Gerar_ProtNFCom: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('protNFCom');

  Result.SetAttribute('versao', FloatToString(NFCom.infNFCom.Versao, '.', '#0.00'));

  xmlNode := Result.AddChild('infProt');

  xmlNode.AddChild('tpAmb').Content := TipoAmbienteToStr(NFCom.procNFCom.tpAmb);

  xmlNode.AddChild('verAplic').Content := NFCom.procNFCom.verAplic;

  xmlNode.AddChild('chNFCom').Content := NFCom.procNFCom.chNFCom;

  xmlNode.AddChild('dhRecbto').Content :=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', NFCom.procNFCom.dhRecbto) +
    GetUTC(CodigoUFparaUF(FNFCom.Ide.cUF), NFCom.procNFCom.dhRecbto);

  xmlNode.AddChild('nProt').Content := NFCom.procNFCom.nProt;

  xmlNode.AddChild('digVal').Content := NFCom.procNFCom.digVal;

  xmlNode.AddChild('cStat').Content := IntToStr(NFCom.procNFCom.cStat);

  xmlNode.AddChild('xMotivo').Content := NFCom.procNFCom.xMotivo;
end;

end.
