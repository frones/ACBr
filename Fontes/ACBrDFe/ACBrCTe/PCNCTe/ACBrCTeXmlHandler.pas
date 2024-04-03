{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit ACBrCTeXmlHandler;

interface

uses
  Classes, SysUtils, ACBrXmlDocument, ACBrXmlReader, pcteCTe, pcnConversao, pcteProcCTe, pcnSignature;

type
  TprotCTeHandler = class
  public
    procedure LerProtCTe(const ANode: TACBrXmlNode; const procCTe: TProcCTe);
  end;

  TInfPercursoHandler = class
  public
    procedure LerInfPercurso(const ANode: TACBrXmlNode; const infPercurso: TinfPercursoCollection);
  end;

  TToma03Handler = class
  public
    procedure LerToma03(const ANode: TACBrXmlNode; const toma03: TToma03);
  end;

  TEnderHandler = class
  public
    procedure LerEnder(const ANode: TACBrXmlNode; const Ender: TEndereco); overload;
    procedure LerEnder(const ANode: TACBrXmlNode; const Ender: TEnderEmit); overload;
  end;

  TToma4Handler = class
  private
   FEnderHandler: TEnderHandler;
   FPossuiToma4: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerToma4(const ANode: TACBrXmlNode; const toma4: TToma4);
    property PossuiToma4: Boolean read FPossuiToma4;
  end;

  TIdeHandler = class
  private
    FInfPercursoHandler: TInfPercursoHandler;
    FToma03Handler: TToma03Handler;
    FToma4Handler: TToma4Handler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIde(const ANode: TACBrXmlNode; const Ide: TIde);
  end;

  TFluxoHandler = class
  public
    procedure LerFluxo(const ANode: TACBrXmlNode; const Fluxo: TFluxo);
  end;

  TEntregaHandler = class
  public
    procedure LerEntrega(const ANode: TACBrXMLNode; const Entrega: TEntrega);
  end;

  TObsHandler = class
  public
    procedure LerObsCont(const ANode: TACBrXMLNode; const ObsCont: TObsContCollection);
    procedure LerObsFisco(const ANode: TACBrXMLNode; const ObsFisco: TObsFiscoCollection);
  end;

  TComplHandler = class
  private
    FFluxoHandler: TFluxoHandler;
    FEntregaHandler: TEntregaHandler;
    FObsHandler: TObsHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerCompl(const ANode: TACBrXmlNode; const Compl: TCompl);
  end;

  TEmitHandler = class
  private
    FEnderHandler: TEnderHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerEmit(const ANode: TACBrXmlNode; const Emit: TEmit);
  end;

  TTomaHandler = class
  private
    FEnderHandler: TEnderHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerToma(const ANode: TACBrXMLNode; const Toma: TToma);
  end;

  TRemHandler = class
  private
    FEnderHandler: TEnderHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerRem(const ANode: TACBrXMLNode; const Rem: TRem);
  end;

  TExpedHandler = class
  private
    FEnderHandler: TEnderHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerExped(const ANode: TACBrXMLNode; const Exped: TExped);
  end;

  TRecebHandler = class
  private
    FEnderHandler: TEnderHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerReceb(const ANode: TACBrXMLNode; const Receb: TReceb);
  end;

  TDestHandler = class
  private
    FEnderHandler: TEnderHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerDest(const ANode: TACBrXmlNode; const Dest: TDest);
  end;

  TInfEspecieHandler = class
  public
    procedure LerInfEspecie(const ANode: TACBrXmlNode; const infEspecie: TinfEspecieCollection);
  end;

  TInfVeiculoHandler = class
  public
    procedure LerInfVeiculo(const ANode: TACBrXmlNode; const InfVeiculo: TinfVeiculoCollection);
  end;

  TDetGTVeHandler = class
  private
    FInfEspecieHandler: TInfEspecieHandler;
    FInfVeiculoHandler: TInfVeiculoHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerDetGTVe(const ANode: TACBrXmlNode; const detGTV: TdetGTV);
  end;

  TCompHandler = class
  public
    procedure LerComp(const ANode: TACBrXmlNode; const comp: TCompCollection);
  end;

  TVPrestHandler = class
  private
    FCompHandler: TCompHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerVPrest(const ANode: TACBrXMLNode; const vPrest: TVPrest);
  end;

  TICMS00Handler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMS00(const ANode: TACBrXMLNode; const ICMS00: TCST00);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMS20Handler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMS20(const ANode: TACBrXMLNode; const ICMS20: TCST20);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMS45Handler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMS45(const ANode: TACBrXMLNode; const ICMS45: TCST45);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMS60Handler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMS60(const ANode: TACBrXMLNode; const ICMS60: TCST60);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMS90Handler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMS90(const ANode: TACBrXMLNode; const ICMS90: TCST90);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMSOutraUFHandler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMSOutraUF(const ANode: TACBrXMLNode; const ICMSOutraUF: TICMSOutraUF);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMSSNHandler = class
  private
    FLiAhInformacao: Boolean;
    FSituTrib: TpcnCSTIcms;
  public
    procedure LerICMSSN(const ANode: TACBrXMLNode; const ICMSSN: TICMSSN);
  published
    property LiAhInformacao: Boolean read FLiAhInformacao;
    property SituTrib: TpcnCSTIcms read FSituTrib;
  end;

  TICMSHandler = class
  private
    FICMS00Handler: TICMS00Handler;
    FICMS20Handler: TICMS20Handler;
    FICMS45Handler: TICMS45Handler;
    FICMS60Handler: TICMS60Handler;
    FICMS90Handler: TICMS90Handler;
    FICMSOutraUFHandler: TICMSOutraUFHandler;
    FICMSSNHandler: TICMSSNHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerICMS(const ANode: TACBrXMLNode; const ICMS: TICMS);
  end;

  TICMSUFFimHandler = class
  public
    procedure LerICMSUFFim(const ANode: TACBrXmlNode; const ICMSUfFim: TICMSUFFim);
  end;

  TInfTribFedHandler = class
  public
    procedure LerInfTribFed(const ANode: TACBrXMLNode; const InfTribFed: TInfTribFed);
  end;

  TImpHandler = class
  private
    FICMSHandler: TICMSHandler;
    FICMSUFFimHandler: TICMSUFFimHandler;
    FInfTribFedHandler: TInfTribFedHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerImp(const ANode: TACBrXMLNode; const Imp: TImp);
  end;

  TInfServicoHandler = class
  public
    procedure LerInfServico(const ANode: TACBrXmlNode; const infServico: TinfServico);
  end;

  TInfDocRefHandler = class
  public
    procedure LerInfDocRef(const ANode: TACBrXmlNode; const infDocRef: TinfDocRefCollection);
  end;

  TInfQHandler = class
  public
    procedure LerInfQ(const ANode: TACBrXmlNode; const infQ: TInfQCollection);
  end;

  TInfCargaHandler = class
  private
    FInfQHandler: TInfQHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfCarga(const ANode: TACBrXmlNode; const infCarga: TInfCarga);
  end;

  TlacUnidTranspHandler = class
  public
    procedure LerlacUnidTransp(const ANode: TACBrXmlNode; const lacUnidTransp: TlacUnidTranspCollection);
  end;

  TlacUnidCargaHandler = class
  public
    procedure LerlacUnidCarga(const ANode: TACBrXmlNode; const lacUnidCarga: TlacUnidCargaCollection);
  end;

  TinfUnidCargaHandler = class
  private
    FLacUnidCargaHandler: TlacUnidCargaHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerinfUnidCarga(const ANode: TACBrXmlNode; const infUnidCarga: TinfUnidCargaCollection);
  end;

  TInfUnidTranspHandler = class
  private
    FlacUnidTranspHandler: TlacUnidTranspHandler;
    FInfUnidCargaHandler: TInfUnidcargaHandler;
    procedure LerInfUnidTranspItem(const ANode: TACBrXmlNode; const infUnidTranspItem: TinfUnidTranspCollectionItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfUnidTransp(const ANode: TACBrXmlNode; const InfUnidTransp: TinfUnidTranspNFCollection); overload;
    procedure LerInfUnidTransp(const ANode: TACBrXmlNode; const InfUnidTransp: TinfUnidTranspOutrosCollection); overload;
  end;

  TInfNFHandler = class
  private
    FInfUnidTranspHandler: TInfUnidTranspHandler;
    FInfUnidCargahandler: TInfUnidCargaHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerinfNF(const ANode: TACBrXmlNode; const infNF: TInfNFCollection);
  end;

  TInfNFeHandler = class
  private
    FInfUnidTranspHandler: TInfUnidTranspHandler;
    FInfUnidCargaHandler: TinfUnidCargaHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerinfNFe(const ANode: TACBrXmlNode; const infNFe: TInfNFeCollection);
  end;

  TInfOutrosHandler = class
  private
    FInfUnidTranspHandler: TInfUnidTranspHandler;
    FInfUnidCargaHandler: TinfUnidCargaHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfOutros(const ANode: TACBrXmlNode; const infOutros: TInfOutrosCollection);
  end;

  TInfDocHandler = class
  private
    FInfNfHandler: TInfNFHandler;
    FInfNFeHandler: TInfNFeHandler;
    FInfOutrosHandler: TInfOutrosHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfDoc(const ANode: TACBrXmlNode; const InfDoc: TInfDoc);
  end;

  TIdDocAntPapHandler = class
  public
    procedure LerIdDocAntPap(const ANode: TACBrXmlNode; const IdDocAntPap: TIdDocAntPapCollection);
  end;

  TIdDocAntEleHandler = class
  public
    procedure LerIdDocAntEle(const ANode: TACBrXMLNode; const IdDocAntEle: TIdDocAntEleCollection);
  end;

  TIdDocAntHandler = class
  private
    FIdDocAntPapHandler: TIdDocAntPapHandler;
    FIdDocAntEleHandler: TIdDocAntEleHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIdDocAnt(const ANode: TACBrXmlNode; const IdDocAnt: TIdDocAntCollection);
  end;

  TEmiDocAntHandler = class
  private
    FIdDocAntHandler: TIdDocAntHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerEmiDocAnt(const ANode: TACBrXmlNode; const emiDocAnt: TEmiDocAntCollection);
  end;

  TInfDocAntHandler = class
  private
    FEmiDocAntHandler: TEmiDocAntHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerDocAnt(const ANode: TACBrXmlNode; const DocAnt: TDocAnt);
  end;

  TSegHandler = class
  public
    procedure LerSeg(const ANode: TACBrXmlNode; const Seg: TSegCollection);
  end;

  TEmiOccHandler = class
  public
    procedure LerEmiOcc(const ANode: TACBrXmlNode; const EmiOcc: TEmiOCC);
  end;

  TOccHandler = class
  private
    FEmiOccHandler: TEmiOccHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerOcc(const ANode: TACBrXmlNode; const Occ: TOccCollection);
  end;

  TValePedHandler = class
  public
    procedure LerValePed(const ANode: TACBrXmlNode; const ValePed: TValePedCollection);
  end;

  TpropHandler = class
  public
    procedure LerProp(const ANode: TACBrXMLNode; const prop: TProp);
  end;

  TVeicHandler = class
  private
    FPropHandler: TPropHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerVeic(const ANode: TACBrXmlNode; const Veic: TVeicCollection);
  end;

  TlacRodoHandler = class
  public
    procedure LerlacRodo(const ANode: TACBrXmlNode; const lacRodo: TLacRodoCollection);
  end;

  TMotoHandler = class
  public
    procedure LerMoto(const ANode: TACBrXmlNode; const Moto: TMotoCollection);
  end;

  TRodoHandler = class
  private
    FOccHandler: TOccHandler;
    FValePedHandler: TValePedHandler;
    FVeicHandler: TVeicHandler;
    FLacRodoHandler: TLacRodoHandler;
    FMotoHandler: TMotoHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function LerRodo(const ANode: TACBrXMLNode; const Rodo: TRodo): Boolean;
  end;

  TPropOSHandler = class
  public
    procedure LerPropOS(const ANode: TACBrXmlNode; const propOS: TPropOS);
  end;

  TVeicOSHandler = class
  private
    FPropOSHandler: TPropOSHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerVeicOS(const ANode: TACBrXmlNode; const veicOS: TVeicOS);
  end;

  TInfFretamentoHandler = class
  public
    procedure LerInfFretamento(const ANode: TACBrXMLNode; const infFretamentoOS: TinfFretamento);
  end;

  TRodoOSHandler = class
  private
    FVeicOSHandler: TVeicOSHandler;
    FinfFretamentoOS: TInfFretamentoHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function LerRodoOS(const ANode: TACBrXmlNode; const RodoOS: TRodoOS): Boolean;
  end;

  TTarifaHandler = class
  public
    procedure LerTarifa(const ANode: TACBrXmlNode; const tarifa: TTarifa);
  end;

  TcInfManuHandler = class
  public
    procedure LercInfManu(const ANode: TACBrXmlNode; const cInfManu: TpInfManuCollection);
  end;

  TNatCargaHandler = class
  private
    FcInfManuHandler: TcInfManuHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerNatCarga(const ANode: TACBrXmlNode; const natCarga: TNatCarga);
  end;

  TPeriHandler = class
  public
    procedure LerPeri(const ANode: TACBrXmlNode; const peri: TPeriCollection);
  end;

  TAereoHandler = class
  private
    FTarifaHandler: TTarifaHandler;
    FNatCargaHandler: TNatCargaHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function LerAereo(const ANode: TACBrXmlNode; const Aereo: TAereo): Boolean;
  end;

  TbalsaHandler = class
  public
    procedure LerBalsa(const ANode: TACBrXMLNode; const Balsa: TBalsaCollection);
  end;

  TLacreHandler = class
  public
    procedure LerLacre(const ANode: TACBrXmlNode; const Lacre: TLacreCollection);
  end;

  TInfNFAquavHandler = class
  public
    procedure LerinfNF(const ANode: TACBrXmlNode; const infNF: TInfNFAquavCollection);
  end;

  TInfNFeAquavHandler = class
  public
    procedure LerInfNFe(const ANode: TACBrXmlNode; const infNFe: TInfNFeAquavCollection);
  end;

  TInfDocAquavHandler = class
  private
    FInfNfAquavHandler: TInfNFAquavHandler;
    FInfNFeAquavHandler: TInfNFeAquavHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfDoc(const ANode: TACBrXmlNode; const InfDoc: TInfDocAquav);
  end;

  TDetContHandler = class
  private
    FLacreHandler: TLacreHandler;
    FInfDocAquavHandler: TInfDocAquavHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerDetCont(const ANode: TACBrXmlNode; const DetCont: TdetContCollection);
  end;

  TAquavHandler = class
  private
    FBalsaHandler: TBalsaHandler;
    FDetContHandler: TDetContHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function LerAquav(const ANode: TACBrXMLNode; const Aquav: TAquav): Boolean;
  end;

  TTrafMutHandler = class
  public
    procedure LerTrafMut(const ANode: TACBrXmlNode; const TrafMut: TTrafMut);
  end;

  TenderFerroHandler = class
  public
    procedure LerEnderFerro(const ANode: TACBrXmlNode; const enderFerro: TEnderFerro);
  end;

  TFerroEnvHandler = class
  private
    FEnderFerroHandler: TenderFerroHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerFerroEnv(const ANode: TACBrXmlNode; const FerroEnv: TFerroEnvCollection);
  end;

  TFerrovHandler = class
  private
    FTrafMutHandler: TTrafMutHandler;
    FFerroEnvHandler: TFerroEnvHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function LerFerrov(const ANode: TACBrXMLNode; const Ferrov: TFerrov): Boolean;
  end;

  TDutoHandler = class
  public
    function LerDuto(const ANode: TACBrXmlNode; const Duto: TDuto): Boolean;
  end;

  TMultiModalHandler = class
  public
    function LerMultiModal(const ANode: TACBrXmlNode; const Multimodal: TMultimodal): Boolean;
  end;

  TVeicNovosHandler = class
  public
    procedure LerVeicNovos(const ANode: TACBrXMLNode; const VeicNovos: TVeicNovosCollection);
  end;

  TFatHandler = class
  public
    procedure LerFat(const ANode: TACBrXmlNode; const Fat: TFat);
  end;

  TDupHandler = class
  public
    procedure LerDup(const ANode: TACBrXmlNode; const Dup: TDupCollection);
  end;

  TCobrHandler = class
  private
    FFatHandler: TFatHandler;
    FDupHandler: TDupHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerCobr(const ANode: TACBrXmlNode; const Cobr: TCobr);
  end;

  TInfGTVeCompHandler = class
  public
    procedure LerComp(const ANode: TACBrXmlNode; const comp: TinfGTVeCompCollection);
  end;

  TInfGTVeHandler = class
  private
    FInfGTVeCompHandler: TInfGTVeCompHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfGTVe(const ANode: TACBrXmlNode; const infGTVe: TinfGTVeCollection);
  end;

  TRefNFHandler = class
  public
    procedure LerRefNF(const ANode: TACBrXmlNode; const refNF: TRefNF);
  end;

  TTomaICMSHandler = class
  private
    FRefNFHandler: TRefNFHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerTomaICMS(const ANode: TACBrXmlNode; const tomaICMS: TTomaICMS);
  end;

  TTomaNaoICMSHandler = class
  public
    procedure LerNaoTomaICMS(const ANode: TACBrXmlNode; const tomaNaoICMS: TTomaNaoICMS);
  end;

  TInfCTeSubHandler = class
  private
    FTomaICMSHandler: TTomaICMSHandler;
    FTomaNaoICMSHandler: TTomaNaoICMSHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfCTeSub(const ANode: TACBrXmlNode; const infCTeSub: TInfCteSub);
  end;

  TInfGlobalizadoHandler = class
  public
    procedure LerInfGlobalizado(const ANode: TACBrXmlNode; const infGlobalizado: TinfGlobalizado);
  end;

  TInfCTeMultiModalHandler = class
  public
    procedure LerInfCTeMultiModal(const ANode: TACBrXmlNode; const InfCteMultimodal: TinfCTeMultimodalCollection);
  end;

  TinfServVincHandler = class
  private
    FInfCTeMultiModalHandler: TInfCTeMultiModalHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfServVinc(const ANode: TACBrXmlNode; const InfServVinc: TinfServVinc);
  end;

  TInfCTeNormHandler = class
  private
    FInfServicoHandler: TInfServicoHandler;
    FInfDocRefHandler: TInfDocRefHandler;
    FInfCargaHandler: TInfCargaHandler;
    FInfDocHandler: TInfDocHandler;
    FInfDocAntHandler: TInfDocAntHandler;
    FSegHandler: TSegHandler;
    FRodoHandler: TRodoHandler;
    FRodoOSHandler: TRodoOSHandler;
    FAereoHandler: TAereoHandler;
    FPeriHandler: TPeriHandler;
    FAquavHandler: TAquavHandler;
    FFerrovHandler: TFerrovHandler;
    FDutoHandler: TDutoHandler;
    FMultiModalHandler: TMultiModalHandler;
    FVeicNovosHandler: TVeicNovosHandler;
    FCobrHandler: TCobrHandler;
    FInfGTVeHandler: TInfGTVeHandler;
    FInfCTeSubHandler: TInfCTeSubHandler;
    FInfGlobalizadoHandler: TInfGlobalizadoHandler;
    FInfServVincHandler: TInfServVincHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerinfCTeNorm(const ANode: TACBrXmlNode; const infCTeNorm: TInfCTeNorm);
  end;

  TInfCTeCompHandler = class
  public
    procedure LerInfCteComp(const ANode: TACBrXmlNode; const infCTeComp: TInfCteComp); overload;
    procedure LerInfCTeComp(const ANode: TACBrXmlNode; const infCTeComp: TInfCteCompCollection); overload;
  end;

  TInfCTeAnuHandler = class
  public
    procedure LerInfCTeAnu(const ANode: TACBrXmlNode; const infCteAnu: TInfCteAnu);
  end;

  TAutXMLHandler = class
  public
    procedure LerAutXML(const ANode: TACBrXMLNode; const autXML: TautXMLCollection);
  end;

  TinfRespTecHandler = class
  public
    procedure LerInfRespTec(const ANode: TACBrXMLNode; const infRespTec: TinfRespTec);
  end;

  TInfCTeHandler = class
  private
    FIdeHandler: TIdeHandler;
    FComplHandler: TComplHandler;
    FEmitHandler: TEmitHandler;
    FTomaHandler: TTomaHandler;
    FRemHandler: TRemHandler;
    FExpedHandler: TExpedHandler;
    FRecebHandler: TRecebHandler;
    FDestHandler: TDestHandler;
    FOrigemHandler: TEnderHandler;
    FDestinoHandler: TEnderHandler;
    FDetGTVHandler: TDetGTVeHandler;
    FVPrestHandler: TVPrestHandler;
    FImpHandler: TImpHandler;
    FInfCTeNormHandler: TInfCTeNormHandler;
    FInfCteCompHandler: TInfCTeCompHandler;
    FInfCteAnuHandler: TInfCTeAnuHandler;
    FAutXMLHandler: TAutXMLHandler;
    FInfRespTecHandler: TinfRespTecHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerInfCTe(const ANode: TACBrXmlNode; const CTe: TCTe);
  end;

  TinfCTeSuplHandler = class
  public
    procedure LerInfCTeSupl(const ANode: TACBrXmlNode; const infCTeSupl: TinfCTeSupl);
  end;

  TSignatureHandler = class
  public
    procedure LerSignature(const ANode: TACBrXmlNode; const Signature: Tsignature);
  end;

  TCTeXmlReader = class(TACBrXmlReader)
  private
    FCTe: TCTe;
    FprotCTeHandler: TprotCTeHandler;
    FInfCTeHandler: TInfCTeHandler;
    FInfCTeSuplHandler: TinfCTeSuplHandler;
    FSignatureHandler: TSignatureHandler;
    procedure ValidarXML(out infCTeNode: TACBrXmlNode; out CTeNode: TACBrXmlNode);
  public
    constructor Create(AOwner: TCTe); reintroduce;
    destructor Destroy; override;
    function LerXML: Boolean; override;
    property CTe: TCTe read FCTe;
  end;

implementation

uses
  ACBrUtil.Base, ACBrXmlBase, pcteConversaoCTe;

{ TCTeXmlReader }

constructor TCTeXmlReader.Create(AOwner: TCTe);
begin
  inherited Create;

  FCTe := AOwner;
  FprotCTeHandler := TprotCTeHandler.Create;
  FInfCTeHandler := TInfCTeHandler.Create;
  FInfCTeSuplHandler := TInfCTeSuplHandler.Create;
  FSignatureHandler := TSignatureHandler.Create;
end;

destructor TCTeXmlReader.Destroy;
begin
  FprotCTeHandler.Free;
  FInfCTeHandler.Free;
  FInfCTeSuplHandler.Free;
  FSignatureHandler.Free;
  inherited;
end;

function TCTeXmlReader.LerXML: Boolean;
var
  CTeNode, infCteNode: TACBrXMLNode;
begin
  Result := False;
  ValidarXML(infCTeNode, CTeNode);
  FInfCTeHandler.LerInfCTe(infCTeNode, FCTe);
  FInfCTeSuplHandler.LerInfCTeSupl(CTeNode.Childrens.FindAnyNs('infCTeSupl'), FCTe.infCTeSupl);
  FSignatureHandler.LerSignature(CTeNode.Childrens.FindAnyNs('Signature'), FCTe.signature);
  Result := True;
end;

procedure TCTeXmlReader.ValidarXML(out infCTeNode: TACBrXmlNode; out CTeNode: TACBrXmlNode);
var
  att: TACBrXmlAttribute;
begin
  if not Assigned(FCTe) or (FCTe = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TCTe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo Xml do CTe não carregado.');

  infCTeNode := nil;
  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'cteProc' then
  begin
    FprotCTeHandler.LerProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'), FCTe.procCTe);
    CTeNode := Document.Root.Childrens.FindAnyNs('CTe');
  end else
  begin
    CTeNode := Document.Root;
  end;

  if Assigned(CTeNode) then
    infCTeNode := CTeNode.Childrens.FindAnyNs('infCte');

  if not Assigned(infCTeNode) then
    raise Exception.Create('Arquivo Xml incorreto.');

  att := infCTeNode.Attributes.Items['Id'];
  if not Assigned(att) then
    raise Exception.Create('Não encontrei o atributo: Id.');

  FCTe.infCTe.Id := att.Content;

  att := infCTeNode.Attributes.Items['versao'];
  if not Assigned(att) then
    raise Exception.Create('Não encontrei o atributo: versão.');

  FCTe.infCTe.versao := StringToFloat(att.Content);
end;

{ TprotCTeHandle }

procedure TprotCTeHandler.LerProtCTe(const ANode: TACBrXmlNode; const procCTe: TProcCTe);
var
  Ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  AuxNode := ANode.Childrens.FindAnyNs('infProt');
  if Assigned(AuxNode) then
  begin
    procCTe.tpAmb := StrToTpAmb(Ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpAmb'), tcStr));
    procCTe.verAplic := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
    procCTe.chCTe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('chCTe'), tcStr);
    procCTe.dhRecbto := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
    procCTe.nProt := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nProt'), tcStr);
    procCTe.digVal := ObterConteudoTag(AuxNode.Childrens.FindAnyNS('digVal'), tcStr);
    procCTe.cStat := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cStat'), tcInt);
    procCTe.xMotivo := ObterConteudoTag(AuxNode.Childrens.FindAnyNS('xMotivo'), tcStr);
    procCTe.cMSg := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cMsg'), tcInt);
    procCTe.xMsg := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xMsg'), tcStr);
  end;
end;

{ TIdeHandler }

constructor TIdeHandler.Create;
begin
  inherited;
  FInfPercursoHandler := TInfPercursoHandler.Create;
  FToma03Handler := TToma03Handler.Create;
  FToma4Handler := TToma4Handler.Create;
end;

destructor TIdeHandler.Destroy;
begin
  FInfPercursoHandler.Free;
  FToma03Handler.Free;
  FToma4Handler.Free;
  inherited;
end;

procedure TIdeHandler.LerIde(const ANode: TACBrXmlNode; const Ide: TIde);
var
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  (*B02*)Ide.cUF := ObterConteudoTag(ANode.Childrens.FindAnyNs('cUF'), tcInt);
  (*B03*)Ide.cCT := ObterConteudoTag(ANode.Childrens.FindAnyNs('cCT'), tcInt);
  (*B04*)Ide.CFOP := ObterConteudoTag(ANode.Childrens.FindAnyNs('CFOP'), tcStr);
  (*B05*)Ide.natOp := ObterConteudoTag(ANode.Childrens.FindAnyNs('natOp'), tcStr);

  (*B07*)Ide.modelo := ObterConteudoTag(ANode.Childrens.FindAnyNs('mod'), tcStr);
  (*B08*)Ide.serie := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie'), tcStr);
  (*B09*)Ide.nCT := ObterConteudoTag(ANode.Childrens.FindAnyNs('nCT'), tcInt);
  (*B10*)Ide.dhEmi := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
  (*B11*)Ide.tpImp := StrToTpImp(Ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpImp'), tcStr));
  (*B12*)Ide.tpEmis := StrToTpEmis(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEmis'), tcStr));
  (*B13*)Ide.cDV := ObterConteudoTag(ANode.Childrens.FindAnyNs('cDV'), tcInt);
  (*B14*)Ide.tpAmb := StrToTpAmb(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  (*B15*)Ide.tpCTe := StrTotpCTe(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpCTe'), tcStr));
  (*B15a*)Ide.procEmi := StrToprocEmi(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('procEmi'), tcStr));
  (*B15b*)Ide.verProc := ObterConteudoTag(ANode.Childrens.FindAnyNs('verProc'), tcStr);

  if ObterConteudoTag(ANode.Childrens.FindAnyNs('indGlobalizado'), tcStr) = '1' then
    Ide.indGlobalizado := tiSim
  else
    Ide.IndGlobalizado := tiNao;

  (*B15c*)Ide.refCTE := ObterConteudoTag(ANode.Childrens.FindAnyNs('refCTE'), tcStr);
  (*B16*)Ide.cMunEnv := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMunEnv'), tcInt);
  (*B17*)Ide.xMunEnv := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMunEnv'), tcStr);
  (*B18*)Ide.UFEnv := ObterConteudoTag(ANode.Childrens.FindAnyNs('UFEnv'), tcStr);
  (*B19*)Ide.modal := StrToTpModal(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('modal'), tcStr));
  (*B20*)Ide.tpServ := StrtoTpServ(Ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpServ'), tcStr));
  (*B21*)Ide.cMunIni := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMunIni'), tcInt);
  (*B22*)Ide.xMunIni := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMunIni'), tcStr);
  (*B23*)Ide.UFIni := ObterConteudoTag(ANode.Childrens.FindAnyNs('UFIni'), tcStr);
  (*B24*)Ide.cMunFim := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMunFim'), tcInt);
  (*B25*)Ide.xMunFim := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMunFim'), tcStr);
  (*B26*)Ide.UFFim := ObterConteudoTag(ANode.Childrens.FindAnyNs('UFFim'), tcStr);
  if Ide.modelo = 57 then
    (*B27*)Ide.retira := StrToTpRetira(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('retira'), tcStr));
  (*B27a*)Ide.xdetretira := ObterConteudoTag(ANode.Childrens.FindAnyNs('xDetRetira'), tcStr);
  (*#57*)Ide.dhCont := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhCont'), tcDatHor);
  (*#58*)Ide.xJust := ObterConteudoTag(ANode.Childrens.FindAnyNs('xJust'), tcStr);

  Ide.IndIEToma := StrToindIEDest(OK, ObterConteudoTag(ANode.Childrens.FindAnyNs('indIEToma'), tcStr));

  Ide.dhSaidaOrig := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhSaidaOrig'), tcDatHor);
  Ide.dhChegadaDest := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhChegadaDest'), tcDatHor);

  FInfPercursoHandler.LerInfPercurso(ANode, Ide.infPercurso);
  FToma03Handler.LerToma03(ANode, Ide.toma03);
  FToma4Handler.LerToma4(ANode, Ide.toma4);
  if FToma4Handler.PossuiToma4 then
    Ide.toma03.Toma := Ide.toma4.Toma;
end;

{ TToma03Handler }

procedure TToma03Handler.LerToma03(const ANode: TACBrXmlNode; const toma03: TToma03);
var
  toma: string;
  tomaNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  tomaNode := ANode.Childrens.FindAnyNs('toma03');

  if not Assigned(tomaNode) then
    tomaNode := ANode.Childrens.FindAnyNs('toma3');

  if not Assigned(tomaNode) then
    tomaNode := ANode.Childrens.FindAnyNs('toma');

  if not Assigned(tomaNode) then exit;

  (*B29*)toma03.Toma := StrToTpTomador(Ok, ObterConteudoTag(tomaNode.Childrens.FindAnyNs('toma'), tcStr));
end;

{ TInfPercursoHandler }

procedure TInfPercursoHandler.LerInfPercurso(const ANode: TACBrXmlNode; const infPercurso: TinfPercursoCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infPercurso');
  infPercurso.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infPercurso.New;
    InfPercurso[i].UFPer := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('UFPer'), tcStr);
  end;
end;

{ TToma4Handler }

constructor TToma4Handler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TToma4Handler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TToma4Handler.LerToma4(const ANode: TACBrXmlNode; const toma4: TToma4);
var
  tomaNode: TACBrXmlNode;
  OK: Boolean;
begin
  FPossuiToma4 := False;

  if not Assigned(ANode) then exit;

  tomaNode := ANode.Childrens.FindAnyNs('toma4');

  if not Assigned(tomaNode) then
    tomaNode := ANode.Childrens.FindAnyNs('tomaTerceiro');

  if not Assigned(tomaNode) then exit;

  (*B29*)toma4.toma := StrToTpTomador(OK, ObterConteudoTag(tomaNode.Childrens.FindAnyNs('toma'), tcStr));

  (*B31*)toma4.CNPJCPF := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(toma4.CNPJCPF) = EmptyStr then
    (*B31*)toma4.CNPJCPF := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('CPF'), tcStr);
  (*B33*)toma4.IE := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('IE'), tcStr);
  (*B34*)toma4.xNome := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('xNome'), tcStr);
  (*B35*)toma4.xFant := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('xFant'), tcStr);
  (*#44*)toma4.fone := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('fone'), tcStr);
  (*#56*)toma4.email := ObterConteudoTag(tomaNode.Childrens.FindAnyNs('email'), tcStr);

  FEnderHandler.LerEnder(tomaNode.Childrens.FindAnyNs('enderToma'), toma4.enderToma);

  FPossuiToma4 := True;
end;

{ TComplHandler }

constructor TComplHandler.Create;
begin
  inherited;
  FFluxoHandler := TFluxoHandler.Create;
  FEntregaHandler := TEntregaHandler.Create;
  FObsHandler := TObsHandler.Create;
end;

destructor TComplHandler.Destroy;
begin
  FFluxoHandler.Free;
  FEntregaHandler.Free;
  FObsHandler.Free;
  inherited;
end;

procedure TComplHandler.LerCompl(const ANode: TACBrXmlNode; const Compl: TCompl);
begin
  if not Assigned(ANode) then exit;

  Compl.xCaracAd  := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCaracAd'), tcStr);
  Compl.xCaracSer := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCaracSer'), tcStr);
  Compl.xEmi      := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEmi'), tcStr);
  Compl.origCalc  := ObterConteudoTag(ANode.Childrens.FindAnyNs('origCalc'), tcStr);
  Compl.destCalc  := ObterConteudoTag(ANode.Childrens.FindAnyNs('destCalc'), tcStr);
  Compl.xObs      := ObterConteudoTag(ANode.Childrens.FindAnyNs('xObs'), tcStr);

  FFluxoHandler.LerFluxo(ANode.Childrens.FindAnyNs('fluxo'), Compl.fluxo);
  FEntregaHandler.LerEntrega(ANode.Childrens.FindAnyNs('Entrega'), Compl.Entrega);
  FObsHandler.LerObsCont(ANode, Compl.ObsCont);
  FObsHandler.LerObsFisco(ANode, Compl.ObsFisco)
end;

{ TFluxoHandler }

procedure TFluxoHandler.LerFluxo(const ANode: TACBrXmlNode; const Fluxo: TFluxo);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  fluxo.xOrig := ObterConteudoTag(ANode.Childrens.FindAnyNs('xOrig'), tcStr);
  fluxo.xDest := ObterConteudoTag(ANode.Childrens.FindAnyNs('xDest'), tcStr);
  fluxo.xRota := ObterConteudoTag(ANode.Childrens.FindAnyNs('xRota'), tcStr);

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('pass');

  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    fluxo.pass.New;
    fluxo.pass[i].xPass := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xPass'), tcStr);
  end;
end;

{ TEntregaHandler }

procedure TEntregaHandler.LerEntrega(const ANode: TACBrXMLNode; const Entrega: TEntrega);
var
  OK: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  Entrega.TipoData := tdNaoInformado;
  Entrega.TipoHora := thNaoInformado;

  if Assigned(ANode.Childrens.FindAnyNs('semData')) then
  begin
    AuxNode := ANode.Childrens.FindAnyNs('semData');
    Entrega.TipoData := tdSemData;
    Entrega.semData.tpPer := StrTotpDataPeriodo(Ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNS('tpPer'), tcStr));
  end else if Assigned(ANode.Childrens.FindAnyNs('comData'))  then
  begin
    AuxNode := ANode.Childrens.FindAnyNs('comData');
    Entrega.TipoDAta := tdNaData;
    Entrega.comData.tpPer := StrToTpDataPeriodo(Ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpPer'), tcStr));
    Entrega.comData.dProg := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dProg'), tcDat);
  end else if Assigned(ANode.Childrens.FindAnyNs('noPeriodo')) then
  begin
    AuxNode := ANode.Childrens.FindAnyNs('noPeriodo');
    Entrega.TipoData        := tdNoPeriodo;
    Entrega.noPeriodo.tpPer := StrToTpDataPeriodo(ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpPer'), tcStr));
    Entrega.noPeriodo.dIni  := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dIni'), tcDat);
    Entrega.noPeriodo.dFim  := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dFim'), tcDat);
  end else if Assigned(ANode.Childrens.FindAnyNs('semHora')) then
  begin
    AuxNode := ANode.Childrens.FindAnyNs('semHora');
    Entrega.TipoHora      := thSemHorario;
    Entrega.semHora.tpHor := StrToTpHorarioIntervalo(ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpHor'), tcStr));
  end else if Assigned(ANode.Childrens.FindAnyNs('comHora')) then
  begin
    AuxNode := ANode.Childrens.FindAnyNs('comHora');
    Entrega.TipoHora      := thNoHorario;
    Entrega.comHora.tpHor := StrToTpHorarioIntervalo(ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpHor'), tcStr));
    Entrega.comHora.hProg := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hProg'), tcHor);
  end else if Assigned(ANode.Childrens.FindAnyNs('noInter')) then
  begin
    AuxNode := ANode.Childrens.FindAnyNs('noInter');
    Entrega.TipoHora      := thNoIntervalo;
    Entrega.noInter.tpHor := StrToTpHorarioIntervalo(ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpHor'), tcStr));
    Entrega.noInter.hIni  := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hIni'), tcHor);
    Entrega.noInter.hFim  := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hFim'), tcHor);
  end;
end;

{ TObsHandler }

procedure TObsHandler.LerObsCont(const ANode: TACBrXMLNode; const ObsCont: TObsContCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  ObsCont.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('ObsCont');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    ObsCont.New;
    ObsCont[i].xCampo := AuxNodeArray[i].Attributes.Items['xCampo'].Content;
    ObsCont[i].xTexto := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xTexto'), tcStr);
  end;
end;

procedure TObsHandler.LerObsFisco(const ANode: TACBrXMLNode; const ObsFisco: TObsFiscoCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  ObsFisco.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('ObsFisco');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    ObsFisco.New;
    ObsFisco[i].xCampo := AuxNodeArray[i].Attributes.Items['xCampo'].Content;
    ObsFisco[i].xTexto := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xTexto'), tcStr);
  end;
end;

{ TEmitHandler }

constructor TEmitHandler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TEmitHandler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TEmitHandler.LerEmit(const ANode: TACBrXmlNode; const Emit: TEmit);
var
  OK: Boolean;
  LAux: String;
begin
  if not Assigned(ANode) then exit;

  Emit.CNPJ  := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(Emit.CNPJ) = EmptyStr then
    Emit.CNPJ := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);
  Emit.IE    := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  Emit.IEST  := ObterConteudoTag(ANode.Childrens.FindAnyNs('IEST'), tcStr);
  Emit.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  Emit.xFant := ObterConteudoTag(ANode.Childrens.FindAnyNs('xFant'), tcStr);
  LAux := ObterConteudoTag(ANode.Childrens.FindAnyNs('CRT'), tcStr);
  if LAux <> '' then
    Emit.CRT   := StrToCRTCTe(ok, LAux);

  FEnderHandler.LerEnder(ANode.Childrens.FindAnyNs('enderEmit'), Emit.enderEmit);
end;

{ TTomaHandler }

constructor TTomaHandler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TTomaHandler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TTomaHandler.LerToma(const ANode: TACBrXMLNode; const Toma: TToma);
begin
  if not Assigned(ANode) then exit;

  toma.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(toma.CNPJCPF) = EmptyStr then
    toma.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);

  toma.IE      := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  toma.xNome   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  toma.xFant   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xFant'), tcStr);
  toma.fone    := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
  toma.email   := ObterConteudoTag(ANode.Childrens.FindAnyNs('email'), tcStr);

  FEnderHandler.LerEnder(ANode.Childrens.FindAnyNs('enderToma'), toma.enderToma);
end;

{ TRemHandler }

constructor TRemHandler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TRemHandler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TRemHandler.LerRem(const ANode: TACBrXMLNode; const Rem: TRem);
begin
  if not Assigned(ANode) then exit;

  Rem.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(Rem.CNPJCPF) = EmptyStr then
    Rem.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

  Rem.IE      := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  Rem.xNome   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  Rem.xFant   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xFant'), tcStr);
  Rem.fone    := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
  Rem.email   := ObterConteudoTag(ANode.Childrens.FindAnyNs('email'), tcStr);

  FEnderHandler.LerEnder(ANode.Childrens.FindAnyNs('enderReme'), Rem.enderReme);
end;

{ TExpedHandler }

constructor TExpedHandler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TExpedHandler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TExpedHandler.LerExped(const ANode: TACBrXMLNode; const Exped: TExped);
begin
  if not Assigned(ANode) then exit;

  Exped.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(Exped.CNPJCPF) = EmptyStr then
    Exped.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

  Exped.IE      := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  Exped.xNome   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  Exped.fone    := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
  Exped.email   := ObterConteudoTag(ANode.Childrens.FindAnyNs('email'), tcStr);

  FEnderHandler.LerEnder(ANode.Childrens.FindAnyNs('enderExped'), Exped.enderExped);
end;

{ TRecebHandler }

constructor TRecebHandler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TRecebHandler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TRecebHandler.LerReceb(const ANode: TACBrXMLNode; const Receb: TReceb);
begin
  if not Assigned(ANode) then exit;

  receb.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(receb.CNPJCPF) = EmptyStr then
    receb.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);
  receb.IE      := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  receb.xNome   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  receb.fone    := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
  receb.email   := ObterConteudoTag(ANode.Childrens.FindAnyNs('email'), tcStr);

  FEnderHandler.LerEnder(ANode.Childrens.FindAnyNs('enderReceb'), receb.enderReceb);
end;

{ TDestHandler }

constructor TDestHandler.Create;
begin
  inherited;
  FEnderHandler := TEnderHandler.Create;
end;

destructor TDestHandler.Destroy;
begin
  FEnderHandler.Free;
  inherited;
end;

procedure TDestHandler.LerDest(const ANode: TACBrXmlNode; const Dest: TDest);
begin
  if not Assigned(ANode) then exit;

  Dest.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(Dest.CNPJCPF) = EmptyStr then
    Dest.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);
  Dest.IE      := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  Dest.xNome   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  Dest.fone    := ObterConteudoTag(ANode.Childrens.FindANyNs('fone'), tcStr);
  Dest.ISUF    := ObterConteudoTag(ANode.Childrens.FindAnyNs('ISUF'), tcStr);
  Dest.email   := ObterConteudoTag(ANode.Childrens.FindAnyNs('email'), tcStr);

  FEnderHandler.LerEnder(ANode.Childrens.FindAnyNs('enderDest'), Dest.enderDest);
end;

{ TDetGTVeHandler }

constructor TDetGTVeHandler.Create;
begin
  inherited;
  FInfEspecieHandler:= TInfEspecieHandler.Create;
  FInfVeiculoHandler:= TInfVeiculoHandler.Create;
end;

destructor TDetGTVeHandler.Destroy;
begin
  FInfEspecieHandler.Free;
  FInfVeiculoHandler.Free;
  inherited;
end;

procedure TDetGTVeHandler.LerDetGTVe(const ANode: TACBrXmlNode; const detGTV: TdetGTV);
begin
  if not Assigned(ANode) then exit;

  detGTV.qCarga := ObterConteudoTag(ANode.Childrens.FindAnyNs('qCarga'), tcDe4);

  FInfEspecieHandler.LerInfEspecie(ANode, detGTV.infEspecie);
  FInfVeiculoHandler.LerInfVeiculo(ANode, detGTV.infVeiculo);
end;

{ TEnderHandler }

procedure TEnderHandler.LerEnder(const ANode: TACBrXmlNode; const Ender: TEndereco);
begin
  if not Assigned(ANode) then exit;

  Ender.xLgr    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  Ender.nro     := ObterConteudoTag(ANode.Childrens.FindAnyNs('nro'), tcStr);
  Ender.xCpl    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  Ender.xBairro := ObterConteudoTag(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  Ender.cMun    := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  Ender.xMun    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  Ender.CEP     := ObterConteudoTag(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  Ender.UF      := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  Ender.cPais   := ObterConteudoTag(ANode.Childrens.FindAnyNs('cPais'), tcInt);
  Ender.xPais   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xPais'), tcStr);
end;

procedure TEnderHandler.LerEnder(const ANode: TACBrXmlNode; const Ender: TEnderEmit);
begin
  if not Assigned(ANode) then exit;

  Ender.xLgr    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  Ender.Nro     := ObterConteudoTag(ANode.Childrens.FindAnyNs('nro'), tcStr);
  Ender.xCpl    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  Ender.xBairro := ObterConteudoTag(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  Ender.cMun    := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  Ender.xMun    := ObterConteudoTag(ANode.Childrens.FindANyNs('xMun'), tcStr);
  Ender.CEP     := ObterConteudoTag(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  Ender.UF      := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  Ender.fone    := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
end;

{ TInfEspecieHandler }

procedure TInfEspecieHandler.LerInfEspecie(const ANode: TACBrXmlNode; const infEspecie: TinfEspecieCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infEspecie');
  infEspecie.Clear;
  for i:= 0 to Length(AuxNodeArray)-1 do
  begin
    infEspecie.New;
    infEspecie[i].tpEspecie   := StrToTEspecie(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpEspecie'), tcStr));
    infEspecie[i].vEspecie    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vEspecie'), tcDe2);
    infEspecie[i].tpNumerario := StrTotpNumerario(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpNumerario'), tcStr));
    infEspecie[i].xMoedaEstr  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xMoedaEstr'), tcStr);
  end;
end;

{ TInfVeiculoHandler }

procedure TInfVeiculoHandler.LerInfVeiculo(const ANode: TACBrXmlNode; const InfVeiculo: TinfVeiculoCollection);
var
  Ok: Boolean;
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  infVeiculo.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infVeiculo');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infVeiculo.New;
    infVeiculo[i].placa := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('placa'), tcStr);
    infVeiculo[i].UF    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('UF'), tcStr);
    infVeiculo[i].RNTRC := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('RNTRC'), tcStr);
  end;
end;

{ TVPrestHandler }

constructor TVPrestHandler.Create;
begin
  inherited;
  FCompHandler := TCompHandler.Create;
end;

destructor TVPrestHandler.Destroy;
begin
  FCompHandler.Free;
  inherited;
end;

procedure TVPrestHandler.LerVPrest(const ANode: TACBrXMLNode; const vPrest: TVPrest);
begin
  if not Assigned(ANode) then exit;

  vPrest.vTPrest := ObterConteudoTag(ANode.Childrens.FindAnyNs('vTPrest'), tcDe2);
  vPrest.vRec    := ObterConteudoTag(ANode.Childrens.FindAnyNs('vRec'), tcDe2);

  FCompHandler.LerComp(ANode, vPrest.Comp);
end;

{ TCompHandler }

procedure TCompHandler.LerComp(const ANode: TACBrXmlNode; const comp: TCompCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('Comp');
  Comp.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    Comp.New;
    Comp[i].xNome := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xNome'), tcStr);
    Comp[i].vComp := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vComp'), tcDe2);
  end;
end;

{ TImpHandler }

constructor TImpHandler.Create;
begin
  inherited;
  FICMSHandler := TICMSHandler.Create;
  FICMSUFFimHandler := TICMSUFFimHandler.Create;
  FInfTribFedHandler := TInfTribFedHandler.Create;
end;

destructor TImpHandler.Destroy;
begin
  FICMSHandler.Free;
  FICMSUFFimHandler.Free;
  FInfTribFedHandler.Free;
  inherited;
end;

procedure TImpHandler.LerImp(const ANode: TACBrXMLNode; const Imp: TImp);
begin
  if not Assigned(ANode) then exit;

  Imp.vTotTrib   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vTotTrib'), tcDe2);
  Imp.infAdFisco := ObterConteudoTag(ANode.Childrens.FindAnyNs('infAdFisco'), tcStr);

  FICMSHandler.LerICMS(ANode.Childrens.FindAnyNs('ICMS'), Imp.ICMS);
  FICMSUFFimHandler.LerICMSUFFim(ANode.Childrens.FindAnyNs('ICMSUFFim'), imp.ICMSUFFim);
  FInfTribFedHandler.LerInfTribFed(ANode.Childrens.FindAnyNs('infTribFed'), imp.infTribFed);
end;

{ TICMSHandler }

constructor TICMSHandler.Create;
begin
  inherited;
  FICMS00Handler := TICMS00Handler.Create;
  FICMS20Handler := TICMS20Handler.Create;
  FICMS45Handler := TICMS45Handler.Create;
  FICMS60Handler := TICMS60Handler.Create;
  FICMS90Handler := TICMS90Handler.Create;
  FICMSOutraUFHandler := TICMSOutraUFHandler.Create;
  FICMSSNHandler := TICMSSNHandler.Create;
end;

destructor TICMSHandler.Destroy;
begin
  FICMS00Handler.Free;
  FICMS20Handler.Free;
  FICMS45Handler.Free;
  FICMS60Handler.Free;
  FICMS90Handler.Free;
  FICMSOutraUFHandler.Free;
  FICMSSNHandler.Free;
  inherited;
end;

procedure TICMSHandler.LerICMS(const ANode: TACBrXMLNode; const ICMS: TICMS);
var
  JaLiICMS: Boolean;
begin

  if not Assigned(ANode) then exit;

  JaLiICMS := False;

  FICMS00Handler.LerICMS00(ANode.Childrens.FindAnyNs('ICMS00'), ICMS.ICMS00);
  ICMS.SituTrib := FICMS00Handler.SituTrib;
  JaLiICMS := FICMS00Handler.LiAhInformacao;

  if not JaLiICMS then
  begin
    FICMS20Handler.LerICMS20(ANode.Childrens.FindAnyNs('ICMS20'), ICMS.ICMS20);
    ICMS.SituTrib := FICMS20Handler.SituTrib;
    JaLiICMS := FICMS20Handler.LiAhInformacao;
  end;

  if not JaLiICMS then
  begin
    FICMS45Handler.LerICMS45(ANode.Childrens.FindAnyNs('ICMS45'), ICMS.ICMS45);
    ICMS.SituTrib := FICMS45Handler.SituTrib;
    JaLiICMS := FICMS45Handler.LiAhInformacao;
  end;

  if not JaLiICMS then
  begin
    FICMS60Handler.LerICMS60(ANode.Childrens.FindAnyNs('ICMS60'), ICMS.ICMS60);
    ICMS.SituTrib := FICMS60Handler.SituTrib;
    JaLiICMS := FICMS60Handler.LiAhInformacao;
  end;

  if not JaLiICMS then
  begin
    FICMS90Handler.LerICMS90(ANode.Childrens.FindAnyNs('ICMS90'), ICMS.ICMS90);
    ICMS.SituTrib := FICMS90Handler.SituTrib;
    JaLiICMS := FICMS90Handler.LiAhInformacao;
  end;

  if not JaLiICMS then
  begin
    FICMSOutraUFHandler.LerICMSOutraUF(ANode.Childrens.FindAnyNs('ICMSOutraUF'), ICMS.ICMSOutraUF);
    ICMS.SituTrib := FICMSOutraUFHandler.SituTrib;
    JaLiICMS := FICMSOutraUFHandler.LiAhInformacao;
  end;

  if not JaLiICMS then
  begin
    FICMSSNHandler.LerICMSSN(ANode.Childrens.FindAnyNs('ICMSSN'), ICMS.ICMSSN);
    ICMS.SituTrib := FICMSSNHandler.SituTrib;
    JaLiICMS := FICMSSNHandler.LiAhInformacao;
  end;

end;

{ TICMS00Handler }

procedure TICMS00Handler.LerICMS00(const ANode: TACBrXMLNode; const ICMS00: TCST00);
var
  Ok: Boolean;
begin
  FLiAhInformacao := False;
  if not Assigned(ANode) then exit;

  FSituTrib     := cst00;
  ICMS00.CST   := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));
  ICMS00.vBC   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
  ICMS00.pICMS := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
  ICMS00.vICMS := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);

  FLiAhInformacao := True;
end;

{ TICMS20Handler }

procedure TICMS20Handler.LerICMS20(const ANode: TACBrXMLNode; const ICMS20: TCST20);
var
  Ok: Boolean;
begin
  FLiAhInformacao := False;

  if not Assigned(ANode) then exit;

  FSituTrib      := cst20;
  ICMS20.CST    := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));
  ICMS20.pRedBC := ObterConteudoTag(ANode.Childrens.FindAnyNs('pRedBC'), tcDe2);
  ICMS20.vBC    := ObterConteudoTag(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
  ICMS20.pICMS  := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
  ICMS20.vICMS  := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);

  FLiAhInformacao := True;
end;

{ TICMS45Handler }

procedure TICMS45Handler.LerICMS45(const ANode: TACBrXMLNode; const ICMS45: TCST45);
var
  Ok: Boolean;
begin
  FLiAhInformacao := False;
  if not Assigned(ANode) then exit;

  ICMS45.CST := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));

  case ICMS45.CST of
    cst40: FSituTrib := cst40;
    cst41: FSituTrib := cst41;
    cst51: FSituTrib := cst51;
  end;

  FLiAhInformacao := True;
end;

{ TICMS60Handler }

procedure TICMS60Handler.LerICMS60(const ANode: TACBrXMLNode; const ICMS60: TCST60);
var
  OK: Boolean;
begin
  FLiAhInformacao := False;

  if not Assigned(ANode) then exit;

  FSituTrib         := cst60;
  ICMS60.CST        := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));
  ICMS60.vBCSTRet   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vBCSTRet'), tcDe2);
  ICMS60.vICMSSTRet := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMSSTRet'), tcDe2);
  ICMS60.pICMSSTRet := ObterConteudoTag(ANode.Childrens.FindANyNs('pICMSSTRet'), tcDe2);
  ICMS60.vCred      := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCred'), tcDe2);

  FLiAhInformacao := True;
end;

{ TICMS90Handler }

procedure TICMS90Handler.LerICMS90(const ANode: TACBrXMLNode; const ICMS90: TCST90);
var
  Ok: Boolean;
begin
  FLiAhInformacao := False;

  if not Assigned(ANode) then exit;

  FSituTrib     := cst90;
  ICMS90.CST    := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));
  ICMS90.pRedBC := ObterConteudoTag(ANode.Childrens.FindAnyNs('pRedBC'), tcDe2);
  ICMS90.vBC    := ObterConteudoTag(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
  ICMS90.pICMS  := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
  ICMS90.vICMS  := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  ICMS90.vCred  := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCred'), tcDe2);

  FLiAhInformacao := True;
end;

{ TICMSOutrasUFHandler }

procedure TICMSOutraUFHandler.LerICMSOutraUF(const ANode: TACBrXMLNode; const ICMSOutraUF: TICMSOutraUF);
var
  OK: Boolean;
begin
  FLiAhInformacao := False;

  if not Assigned(ANode) then exit;

  // ICMS devido à UF de origem da prestação, quando diferente da UF do emitente
  FSituTrib                 := cstICMSOutraUF;
  ICMSOutraUF.CST           := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));
  ICMSOutraUF.pRedBCOutraUF := ObterConteudoTag(ANode.Childrens.FindAnyNs('pRedBCOutraUF'), tcDe2);
  ICMSOutraUF.vBCOutraUF    := ObterConteudoTag(ANode.Childrens.FindAnyNs('vBCOutraUF'), tcDe2);
  ICMSOutraUF.pICMSOutraUF  := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMSOutraUF'), tcDe2);
  ICMSOutraUF.vICMSOutraUF  := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMSOutraUF'), tcDe2);

  FLiAhInformacao := True;
end;

{ TICMSSNHandler }

procedure TICMSSNHandler.LerICMSSN(const ANode: TACBrXMLNode; const ICMSSN: TICMSSN);
var
  Ok: Boolean;
begin
  FLiAhInformacao := False;
  if not Assigned(ANode) then exit;

  // ICMS Simples Nacional
  FSituTrib := cstICMSSN;
  ICMSSN.CST := StrToCSTICMS(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('CST'), tcStr));
  ICMSSN.indSN := ObterConteudoTag(ANode.Childrens.FindAnyNs('indSN'), tcInt);

  FLiAhInformacao := True;
end;

{ TICMSUFFimHandler }

procedure TICMSUFFimHandler.LerICMSUFFim(const ANode: TACBrXmlNode; const ICMSUfFim: TICMSUFFim);
begin
  if not Assigned(ANode) then exit;

  ICMSUFFim.vBCUFFim       := ObterConteudoTag(ANode.Childrens.FindAnyNs('vBCUFFim'), tcDe2);
  ICMSUFFim.pFCPUFFim      := ObterConteudoTag(ANode.Childrens.FindAnyNs('pFCPUFFim'), tcDe2);
  ICMSUFFim.pICMSUFFim     := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMSUFFim'), tcDe2);
  ICMSUFFim.pICMSInter     := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMSInter'), tcDe2);
  ICMSUFFim.pICMSInterPart := ObterConteudoTag(ANode.Childrens.FindAnyNs('pICMSInterPart'), tcDe2);
  ICMSUFFim.vFCPUFFim      := ObterConteudoTag(ANode.Childrens.FindAnyNs('vFCPUFFim'), tcDe2);
  ICMSUFFim.vICMSUFFim     := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMSUFFim'), tcDe2);
  ICMSUFFim.vICMSUFIni     := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMSUFIni'), tcDe2);
end;

{ TInfTribFed }

procedure TInfTribFedHandler.LerInfTribFed(const ANode: TACBrXMLNode; const InfTribFed: TInfTribFed);
begin
  if not Assigned(ANode) then exit;

  infTribFed.vPIS    := ObterConteudoTag(ANode.Childrens.FindAnyNs('vPIS'), tcDe2);
  infTribFed.vCOFINS := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCOFINS'), tcDe2);
  infTribFed.vIR     := ObterConteudoTag(ANode.Childrens.FindAnyNs('vIR'), tcDe2);
  infTribFed.vINSS   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vINSS'), tcDe2);
  infTribFed.vCSLL   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCSLL'), tcDe2);
end;

{ TInfCTeNormHandler }

constructor TInfCTeNormHandler.Create;
begin
  inherited;
  FInfServicoHandler := TInfServicoHandler.Create;
  FInfDocRefHandler := TInfDocRefHandler.Create;
  FInfCargaHandler := TInfCargaHandler.Create;
  FInfDocHandler := TInfDocHandler.Create;
  FInfDocAntHandler := TInfDocAntHandler.Create;
  FSegHandler := TSegHandler.Create;
  FRodoHandler := TRodoHandler.Create;
  FRodoOSHandler := TRodoOSHandler.Create;
  FAereoHandler := TAereoHandler.Create;
  FPeriHandler := TPeriHandler.Create;
  FAquavHandler := TAquavHandler.Create;
  FFerrovHandler := TFerrovHandler.Create;
  FDutoHandler := TDutoHandler.Create;
  FMultiModalHandler := TMultiModalHandler.Create;
  FVeicNovosHandler := TVeicNovosHandler.Create;
  FCobrHandler := TCobrHandler.Create;
  FInfGTVeHandler := TInfGTVeHandler.Create;
  FInfCTeSubHandler := TInfCTeSubHandler.Create;
  FInfGlobalizadoHandler := TInfGlobalizadoHandler.Create;
  FInfServVincHandler := TinfServVincHandler.Create;
end;

destructor TInfCTeNormHandler.Destroy;
begin
  FInfServicoHandler.Free;
  FInfDocRefHandler.Free;
  FInfCargaHandler.Free;
  FInfDocHandler.Free;
  FInfDocAntHandler.Free;
  FSegHandler.Free;
  FRodoHandler.Free;
  FRodoOSHandler.Free;
  FAereoHandler.Free;
  FPeriHandler.Free;
  FAquavHandler.Free;
  FFerrovHandler.Free;
  FDutoHandler.Free;
  FMultiModalHandler.Free;
  FVeicNovosHandler.Free;
  FCobrHandler.Free;
  FInfGTVeHandler.Free;
  FInfCTeSubHandler.Free;
  FInfGlobalizadoHandler.Free;
  FInfServVincHandler.Free;
  inherited;
end;

procedure TInfCTeNormHandler.LerinfCTeNorm(const ANode: TACBrXmlNode; const infCTeNorm: TInfCTeNorm);
var
  AuxNode: TACBrXmlNode;
  JaLiOhModal: Boolean;
begin
  if not Assigned(ANode) then exit;

  infCTeNorm.refCTeCanc := ObterConteudoTag(ANode.Childrens.FindAnyNs('refCTeCanc'), tcStr);

  FInfServicoHandler.LerInfServico(ANode.Childrens.FindAnyNs('infServico'), infCteNorm.infServico);
  FInfDocRefHandler.LerInfDocRef(ANode, InfCteNorm.infDocRef);
  FInfCargaHandler.LerInfCarga(ANode.Childrens.FindAnyNs('infCarga'), infCTeNorm.infCarga);
  FInfDocHandler.LerInfDoc(ANode.Childrens.FindAnyNs('infDoc'), infCteNorm.InfDoc);
  FInfDocAntHandler.LerDocAnt(ANode.Childrens.FindAnyNs('docAnt'), infCteNorm.docAnt);
  FSegHandler.LerSeg(ANode, infCTeNorm.seg);

  AuxNode := ANode.Childrens.FindAnyNs('infModal');

  if not Assigned(AuxNode) then exit;

  JaLiOhModal := False;
  JaLiOhModal := FRodoHandler.LerRodo(AuxNode.Childrens.FindAnyNs('rodo'), infCTeNorm.rodo);
  if not JaLiOhModal then
    JaLiOhModal := FRodoOSHandler.LerRodoOS(AuxNode.Childrens.FindAnyNs('rodoOS'), infCTeNorm.rodoOS);
  if not JaLiOhModal then
  begin
    JaLiOhModal := FAereoHandler.LerAereo(AuxNode.Childrens.FindAnyNs('aereo'), infCTeNorm.aereo);
    FPeriHandler.LerPeri(AuxNode.Childrens.FindAnyNs('aereo'), infCtenorm.peri);
  end;
  if not JaLiOhModal then
    JaLiOhModal := FAquavHandler.LerAquav(AuxNode.Childrens.FindAnyNs('aquav'), infCTeNorm.aquav);
  if not JaLiOhModal then
    FFerrovHandler.LerFerrov(AuxNode.Childrens.FindAnyNs('ferrov'), infCteNorm.ferrov);
  if not JaLiOhModal then
    FDutoHandler.LerDuto(AuxNode.Childrens.FindAnyNs('duto'), infCTeNorm.duto);
  if not JaLiOhModal then
    FMultiModalHandler.LerMultiModal(AuxNode.Childrens.FindAnyNs('multimodal'), infCTeNorm.multimodal);

  FVeicNovosHandler.LerVeicNovos(ANode, infCTeNorm.veicNovos);
  FCobrHandler.LerCobr(ANode.Childrens.FindAnyNs('cobr'), infCTeNorm.cobr);
  FInfGTVeHandler.LerInfGTVe(ANode, InfCTeNorm.infGTVe);
  FInfCTeSubHandler.LerInfCTeSub(ANode.Childrens.FindAnyNs('infCteSub'), infCTeNorm.infCteSub);
  FInfGlobalizadoHandler.LerInfGlobalizado(ANode.Childrens.FindAnyNs('infGlobalizado'), infCTeNorm.infGlobalizado);
  FInfServVincHandler.LerInfServVinc(ANode.Childrens.FindAnyNs('infServVinc'), infCTeNorm.infServVinc);
end;

{ TInfServicoHandler }

procedure TInfServicoHandler.LerInfServico(const ANode: TACBrXmlNode; const infServico: TinfServico);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  infServico.xDescServ := ObterConteudoTag(ANode.Childrens.FindAnyNs('xDescServ'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('infQ');
  if Assigned(AuxNode) then
    infServico.qCarga := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('qCarga'), tcDe4);
end;

{ TInfDocRefHandler }

procedure TInfDocRefHandler.LerInfDocRef(const ANode: TACBrXmlNode; const infDocRef: TinfDocRefCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infDocRef');
  infDocRef.Clear;
  for i:= 0 to Length(AuxNodeArray)-1 do
  begin
    infDocRef.New;
    infDocRef[i].nDoc     := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nDoc'), tcEsp);
    infDocRef[i].serie    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('serie'), tcStr);
    infDocRef[i].subserie := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('subserie'), tcStr);
    infDocRef[i].dEmi     := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dEmi'), tcDat);
    infDocRef[i].vDoc     := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vDoc'), tcDe2);

    infDocRef[i].chBPe := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chBPe'), tcStr);
  end;
end;

{ TInfCargaHandler }

constructor TInfCargaHandler.Create;
begin
  inherited;
  FInfQHandler := TInfQHandler.Create;
end;

destructor TInfCargaHandler.Destroy;
begin
  FInfQHandler.Free;
  inherited;
end;

procedure TInfCargaHandler.LerInfCarga(const ANode: TACBrXmlNode; const infCarga: TInfCarga);
begin
  if not Assigned(ANode) then exit;

  infCarga.vCarga      := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCarga'), tcDe2);
  InfCarga.proPred     := ObterConteudoTag(ANode.Childrens.FindAnyNs('proPred'), tcStr);
  InfCarga.xOutCat     := ObterConteudoTag(ANode.Childrens.FindAnyNs('xOutCat'), tcStr);
  infCarga.vCargaAverb := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCargaAverb'), tcDe2);

  FInfQHandler.LerInfQ(ANode, infCarga.InfQ);
end;

{ TInfQHandler }

procedure TInfQHandler.LerInfQ(const ANode: TACBrXmlNode; const infQ: TInfQCollection);
var
  AuxNodeArray: TACBrXMLNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infQ');

  infQ.Clear;
  for i:= 0 to Length(AuxNodeArray)-1 do
  begin
    infQ.New;
    infQ[i].cUnid  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('cUnid'), tcStr);
    infQ[i].tpMed  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpMed'), tcStr);
    infQ[i].qCarga := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('qCarga'), tcDe4);
  end;
end;

{ TInfDocHandler }

constructor TInfDocHandler.Create;
begin
  inherited;
  FInfNfHandler := TInfNFHandler.Create;
  FInfNFeHandler := TInfNFeHandler.Create;
  FInfOutrosHandler := TInfOutrosHandler.Create;
end;

destructor TInfDocHandler.Destroy;
begin
  FInfNfHandler.Free;
  FInfNFeHandler.Free;
  FInfOutrosHandler.Free;
  inherited;
end;

procedure TInfDocHandler.LerInfDoc(const ANode: TACBrXmlNode; const InfDoc: TInfDoc);
begin
  if not Assigned(ANode) then exit;

  FInfNfHandler.LerinfNF(ANode, infDoc.infNF);
  FInfNFeHandler.LerinfNFe(ANode, infDoc.infNFe);
  FInfOutrosHandler.LerInfOutros(ANode, infDoc.infOutros);
end;

{ TInfNFHandler }

constructor TInfNFHandler.Create;
begin
  inherited;
  FInfUnidTranspHandler := TInfUnidTranspHandler.Create;
  FInfUnidCargaHandler := TInfUnidCargaHandler.Create;
end;

destructor TInfNFHandler.Destroy;
begin
  FInfUnidTranspHandler.Free;
  FInfUnidCargaHandler.Free;
  inherited;
end;

procedure TInfNFHandler.LerinfNF(const ANode: TACBrXmlNode; const infNF: TInfNFCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: boolean;
begin
  if not Assigned(ANode) then exit;

  infNF.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infNF');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infNF.New;
    InfNF[i].nRoma := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nRoma'), tcStr);
    InfNF[i].nPed  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nPed'), tcStr);
    InfNF[i].Modelo := StrToModeloNF(Ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('mod'), tcStr));
    InfNF[i].serie := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('serie'), tcStr);
    InfNF[i].nDoc  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nDoc'), tcEsp);
    InfNF[i].dEmi  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dEmi'), tcDat);
    InfNF[i].vBC   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vBC'), tcDe2);
    InfNF[i].vICMS := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vICMS'), tcDe2);
    InfNF[i].vBCST := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vBCST'), tcDe2);
    InfNF[i].vST   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vST'), tcDe2);
    InfNF[i].vProd := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vProd'), tcDe2);
    InfNF[i].vNF   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vNF'), tcDe2);
    InfNF[i].nCFOP := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nCFOP'), tcInt);
    InfNF[i].nPeso := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nPeso'), tcDe3);
    InfNF[i].PIN   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('PIN'), tcStr);
    InfNF[i].dPrev := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dPrev'), tcDat);

    FInfUnidTranspHandler.LerInfUnidTransp(AuxNodeArray[i], infNF[i].infUnidTransp);
    FInfUnidCargaHandler.LerinfUnidCarga(AuxNodeArray[i], infNF[i].infUnidCarga);
  end;
end;

{ TInfUnidTranspHandler }

constructor TInfUnidTranspHandler.Create;
begin
  inherited;
  FlacUnidTranspHandler := TlacUnidTranspHandler.Create;
  FInfUnidCargaHandler := TInfUnidCargaHandler.Create;
end;

destructor TInfUnidTranspHandler.Destroy;
begin
  FlacUnidTranspHandler.Free;
  FInfUnidCargaHandler.Free;
  inherited;
end;

procedure TInfUnidTranspHandler.LerInfUnidTransp(const ANode: TACBrXmlNode; const InfUnidTransp: TinfUnidTranspOutrosCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  infUnidTransp.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infUnidTransp');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infUnidTransp.New;
    LerInfUnidTranspItem(AuxNodeArray[i], infUnidTransp[i]);
  end;
end;

procedure TInfUnidTranspHandler.LerInfUnidTransp(const ANode: TACBrXmlNode; const InfUnidTransp: TinfUnidTranspNFCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  infUnidTransp.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infUnidTransp');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infUnidTransp.New;
    LerInfUnidTranspItem(AuxNodeArray[i], infUnidTransp[i]);
  end;
end;

procedure TInfUnidTranspHandler.LerInfUnidTranspItem(const ANode: TACBrXmlNode; const infUnidTranspItem: TinfUnidTranspCollectionItem);
var
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  infUnidTranspItem.tpUnidTransp := StrToUnidTransp(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpUnidTransp'), tcStr));
  infUnidTranspItem.idUnidTransp := ObterConteudoTag(ANode.Childrens.FindAnyNs('idUnidTransp'), tcStr);
  infUnidTranspItem.qtdRat := ObterConteudoTag(ANode.Childrens.FindAnyNs('qtdRat'), tcDe2);

  FlacUnidTranspHandler.LerlacUnidTransp(ANode, InfUnidTranspItem.lacUnidTransp);
  FInfUnidCargaHandler.LerinfUnidCarga(ANode, infUnidTranspItem.infUnidCarga);
end;

{ TlacUnidTranspHandler }

procedure TlacUnidTranspHandler.LerlacUnidTransp(const ANode: TACBrXmlNode; const lacUnidTransp: TlacUnidTranspCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('lacUnidTransp');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    lacUnidTransp.New;
    lacUnidTransp[i].nLacre := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nLacre'), tcStr);
  end;
end;

{ TlacUnidCargaHandler }

procedure TlacUnidCargaHandler.LerlacUnidCarga(const ANode: TACBrXmlNode; const lacUnidCarga: TlacUnidCargaCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('lacUnidCarga');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    lacUnidCarga.New;
    lacUnidCarga[i].nLacre := ObterConteudoTag(AuxNodeArray[i].Childrens.FindANyNs('nLacre'), tcStr);
  end;
end;

{ TinfUnidCargaHandler }

constructor TinfUnidCargaHandler.Create;
begin
  inherited;
  FLacUnidCargaHandler := TlacUnidCargaHandler.Create;
end;

destructor TinfUnidCargaHandler.Destroy;
begin
  FLacUnidCargaHandler.Free;
  inherited;
end;

procedure TinfUnidCargaHandler.LerinfUnidCarga(const ANode: TACBrXmlNode; const infUnidCarga: TinfUnidCargaCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infUnidCarga');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infUnidCarga.New;
    infUnidCarga[i].tpUnidCarga := StrToUnidCarga(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpUnidCarga'), tcStr));
    infUnidCarga[i].idUnidCarga := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('idUnidCarga'), tcStr);
    infUnidCarga[i].qtdRat      := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('qtdRat'), tcDe2);

    FLacUnidCargaHandler.LerlacUnidCarga(AuxNodeArray[i], infUnidCarga[i].lacUnidCarga);
  end;
end;

{ TInfNFeHandler }

constructor TInfNFeHandler.Create;
begin
  inherited;
  FInfUnidTranspHandler := TInfUnidTranspHandler.Create;
  FInfUnidCargaHandler := TinfUnidCargaHandler.Create;
end;

destructor TInfNFeHandler.Destroy;
begin
  FInfUnidTranspHandler.Free;
  FInfUnidCargaHandler.Free;
  inherited;
end;

procedure TInfNFeHandler.LerinfNFe(const ANode: TACBrXmlNode; const infNFe: TInfNFeCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infNFe');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    InfNFE.New;
    InfNFE[i].chave := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chave'), tcStr);
    InfNFE[i].PIN   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('PIN'), tcStr);
    InfNFE[i].dPrev := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dPrev'), tcDat);

    FInfUnidTranspHandler.LerInfUnidTransp(AuxNodeArray[i], infNFe[i].infUnidTransp);
    FInfUnidCargaHandler.LerinfUnidCarga(AuxNodeArray[i], InfNFe[i].infUnidCarga);
  end;
end;

{ TInfOutrosHandler }

constructor TInfOutrosHandler.Create;
begin
  inherited;
  FInfUnidTranspHandler := TInfUnidTranspHandler.Create;
  FInfUnidCargaHandler := TinfUnidCargaHandler.Create;
end;

destructor TInfOutrosHandler.Destroy;
begin
  FInfUnidTranspHandler.Free;
  FInfUnidCargaHandler.Free;
  inherited;
end;

procedure TInfOutrosHandler.LerInfOutros(const ANode: TACBrXmlNode; const infOutros: TInfOutrosCollection);
var
  AuxNodeArray: TACBrXMLNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  InfOutros.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infOutros');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    InfOutros.New;
    InfOutros[i].tpDoc      := StrToTpDocumento(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpDoc'), tcStr));
    InfOutros[i].descOutros := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('descOutros'), tcStr);
    InfOutros[i].nDoc       := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nDoc'), tcStr);
    InfOutros[i].dEmi       := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dEmi'), tcDat);
    InfOutros[i].vDocFisc   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vDocFisc'), tcDe2);
    InfOutros[i].dPrev      := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dPrev'), tcDat);

    FInfUnidTranspHandler.LerInfUnidTransp(AuxNodeArray[i], infOutros[i].infUnidTransp);
    FInfUnidCargaHandler.LerinfUnidCarga(AuxNodeArray[i], infOutros[i].infUnidCarga);
  end;
end;

{ TDocAntHandler }

constructor TInfDocAntHandler.Create;
begin
  inherited;
  FEmiDocAntHandler := TEmiDocAntHandler.Create;
end;

destructor TInfDocAntHandler.Destroy;
begin
  FEmiDocAntHandler.Free;
  inherited;
end;

procedure TInfDocAntHandler.LerDocAnt(const ANode: TACBrXmlNode; const DocAnt: TDocAnt);
var
  AuxNodeArray: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  FEmiDocAntHandler.LerEmiDocAnt(ANode, docAnt.emiDocAnt);
end;

{ TEmiDocAntHandler }

constructor TEmiDocAntHandler.Create;
begin
  inherited;
  FIdDocAntHandler := TIdDocAntHandler.Create;
end;

destructor TEmiDocAntHandler.Destroy;
begin
  FIdDocAntHandler.Free;
  inherited;
end;

procedure TEmiDocAntHandler.LerEmiDocAnt(const ANode: TACBrXmlNode; const emiDocAnt: TEmiDocAntCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;
  emiDocAnt.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('emiDocAnt');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    emiDocAnt.New;
    emiDocAnt[i].CNPJCPF := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CNPJ'), tcStr);
    if Trim(emiDocAnt[i].CNPJCPF) = EmptyStr then
      emiDocAnt[i].CNPJCPF := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CPF'), tcStr);
    emiDocAnt[i].IE      := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('IE'), tcStr);
    emiDocAnt[i].UF      := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('UF'), tcStr);
    emiDocAnt[i].xNome   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xNome'), tcStr);

    FIdDocAntHandler.LerIdDocAnt(AuxNodeArray[i], emiDocAnt[i].idDocAnt);
  end;
end;

{ TIdDocAntHandler }

constructor TIdDocAntHandler.Create;
begin
  inherited;
  FIdDocAntPapHandler := TIdDocAntPapHandler.Create;
  FIdDocAntEleHandler := TIdDocAntEleHandler.Create;
end;

destructor TIdDocAntHandler.Destroy;
begin
  FIdDocAntPapHandler.Free;
  FIdDocAntEleHandler.Free;
  inherited;
end;

procedure TIdDocAntHandler.LerIdDocAnt(const ANode: TACBrXmlNode; const IdDocAnt: TIdDocAntCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('idDocAnt');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    idDocAnt.New;
    FIdDocAntPapHandler.LerIdDocAntPap(AuxNodeArray[i], IdDocAnt[i].idDocAntPap);
    FIdDocAntEleHandler.LerIdDocAntEle(AuxNodeArray[i], IdDocAnt[i].idDocAntEle);
  end;
end;

{ TIdDocAntPapHandler }

procedure TIdDocAntPapHandler.LerIdDocAntPap(const ANode: TACBrXmlNode; const IdDocAntPap: TIdDocAntPapCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('idDocAntPap');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    idDocAntPap.New;
    idDocAntPap[i].tpDoc  := StrToTpDocumentoAnterior(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpDoc'), tcStr));
    idDocAntPap[i].serie  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('serie'), tcStr);
    idDocAntPap[i].subser := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('subser'), tcStr);
    idDocAntPap[i].nDoc   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nDoc'), tcStr);
    idDocAntPap[i].dEmi   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('dEmi'), tcDat);
  end;
end;

{ TIdDocAntEleHandler }

procedure TIdDocAntEleHandler.LerIdDocAntEle(const ANode: TACBrXMLNode; const IdDocAntEle: TIdDocAntEleCollection);
var
  AuxNodeArray: TACBrXMLNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('idDocAntEle');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    idDocAntEle.New;
    idDocAntEle[i].chCTe := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chCTe'), tcStr);
  end;
end;

{ TSegHandler }

procedure TSegHandler.LerSeg(const ANode: TACBrXmlNode; const Seg: TSegCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXMLNodeArray;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('seg');
  for i:=0 to Length(AuxNodeArray) -1 do
  begin
    seg.New;
    seg[i].respSeg := StrToTpRspSeguro(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('respSeg'), tcStr));
    seg[i].xSeg    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xSeg'), tcStr);
    seg[i].nApol   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nApol'), tcStr);
    seg[i].nAver   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nAver'), tcStr);
    seg[i].vCarga  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vCarga'), tcDe2);
  end;
end;

{ TRodoHandler }

constructor TRodoHandler.Create;
begin
  inherited;
  FOccHandler := TOccHandler.Create;
  FValePedHandler := TValePedHandler.Create;
  FVeicHandler := TVeicHandler.Create;
  FLacRodoHandler := TLacRodoHandler.Create;
  FMotoHandler := TMotoHandler.Create;
end;

destructor TRodoHandler.Destroy;
begin
  FOccHandler.Free;
  FValePedHandler.Free;
  FVeicHandler.Free;
  FLacRodoHandler.Free;
  FMotoHandler.Free;
  inherited;
end;

function TRodoHandler.LerRodo(const ANode: TACBrXMLNode; const Rodo: TRodo): Boolean;
var
  OK: Boolean;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  rodo.RNTRC := ObterConteudoTag(ANode.Childrens.FindAnyNs('RNTRC'), tcStr);
  rodo.dPrev := ObterConteudoTag(ANode.Childrens.FindAnyNs('dPrev'), tcDat);
  rodo.lota  := StrToTpLotacao(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('lota'), tcDat));
  rodo.CIOT  := ObterConteudoTag(ANode.Childrens.FindAnyNs('CIOT'), tcStr);

  FOccHandler.LerOcc(ANode, rodo.occ);
  FValePedHandler.LerValePed(ANode, rodo.valePed);
  FVeicHandler.LerVeic(ANode, rodo.veic);
  FLacRodoHandler.LerlacRodo(ANode, rodo.lacRodo);
  FMotoHandler.LerMoto(ANode, rodo.moto);
  Result := True;
end;

{ TOccHandler }

constructor TOccHandler.Create;
begin
  inherited;
  FEmiOCCHandler := TEmiOccHandler.Create;
end;

destructor TOccHandler.Destroy;
begin
  FEmiOccHandler.Free;
  inherited;
end;

procedure TOccHandler.LerOcc(const ANode: TACBrXmlNode; const Occ: TOccCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('occ');
  occ.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    occ.New;
    occ[i].serie := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('serie'), tcStr);
    occ[i].nOcc  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nOcc'), tcInt);
    occ[i].dEmi  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindaNyNs('dEmi'), tcDat);

    FEmiOccHandler.LerEmiOcc(AuxNodeArray[i].Childrens.FindAnyNs('emiOcc'), occ[i].emiOcc);
  end;
end;

{ TEmiOccHandler }

procedure TEmiOccHandler.LerEmiOcc(const ANode: TACBrXmlNode; const EmiOcc: TEmiOCC);
begin
  if not Assigned(ANode) then exit;

  emiOcc.CNPJ := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  emiOcc.cInt := ObterConteudoTag(ANode.Childrens.FindAnyNs('cInt'), tcStr);
  emiOcc.IE   := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  emiOcc.UF   := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  emiOcc.fone := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
end;

{ TValePedHandler }

procedure TValePedHandler.LerValePed(const ANode: TACBrXmlNode; const ValePed: TValePedCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('valePed');
  valePed.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    valePed.New;
    valePed[i].CNPJForn := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CNPJForn'), tcStr);
    valePed[i].nCompra  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nCompra'), tcStr);
    valePed[i].CNPJPg   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CNPJPg'), tcStr);
    valePed[i].vValePed := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vValePed'), tcDe2);
  end;
end;

{ TVeicHandler }

constructor TVeicHandler.Create;
begin
  inherited;
  FPropHandler := TPropHandler.Create;
end;

destructor TVeicHandler.Destroy;
begin
  FPropHandler.Free;
  inherited;
end;

procedure TVeicHandler.LerVeic(const ANode: TACBrXmlNode; const Veic: TVeicCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('veic');
  veic.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    veic.New;
    veic[i].cInt    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('cInt'), tcInt);
    veic[i].RENAVAM := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('RENAVAM'), tcStr);
    veic[i].placa   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('placa'), tcStr);
    veic[i].tara    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tara'), tcInt);
    veic[i].capKG   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('capKG'), tcInt);
    veic[i].capM3   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('capM3'), tcInt);
    veic[i].tpProp  := StrToTpPropriedade(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpProp'), tcStr));
    veic[i].tpVeic  := StrToTpVeiculo(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpVeic'), tcStr));
    veic[i].tpRod   := StrToTpRodado(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpRod'), tcStr));
    veic[i].tpCar   := StrToTpCarroceria(ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpCar'), tcStr));
    veic[i].UF      := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('UF'), tcStr);

    FPropHandler.LerProp(AuxNodeArray[i].Childrens.FindAnyNs('prop'), veic[i].Prop);
  end;
end;

{ TpropHandler }

procedure TpropHandler.LerProp(const ANode: TACBrXMLNode; const prop: TProp);
var
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  prop.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(prop.CNPJCPF) = EmptyStr then
    prop.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

  prop.RNTRC   := ObterConteudoTag(ANode.Childrens.FindAnyNs('RNTRC'), tcStr);
  prop.xNome   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  prop.IE      := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  prop.UF      := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  prop.tpProp  := StrToTpProp(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpProp'), tcStr));
end;

{ TlacRodoHandler }

procedure TlacRodoHandler.LerlacRodo(const ANode: TACBrXmlNode; const lacRodo: TLacRodoCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('lacRodo');
  lacRodo.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    lacRodo.New;
    lacRodo[i].nLacre := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nLacre'), tcStr);
  end;
end;

{ TMotoHandler }

procedure TMotoHandler.LerMoto(const ANode: TACBrXmlNode; const Moto: TMotoCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('moto');
  moto.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    moto.New;
    moto[i].xNome := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xNome'), tcStr);
    moto[i].CPF   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CPF'), tcStr);
  end;
end;

{ TVeicNovosHandler }

procedure TVeicNovosHandler.LerVeicNovos(const ANode: TACBrXMLNode; const VeicNovos: TVeicNovosCollection);
var
  AuxNodeArray: TACBrXMLNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('veicNovos');
  veicNovos.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    veicNovos.New;
    veicNovos[i].chassi := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chassi'), tcStr);
    veicNovos[i].cCor   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('cCor'), tcStr);
    veicNovos[i].xCor   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xCor'), tcStr);
    veicNovos[i].cMod   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('cMod'), tcStr);
    veicNovos[i].vUnit  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vUnit'), tcDe2);
    veicNovos[i].vFrete := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vFrete'), tcDe2);
  end;
end;

{ TCobrHandler }

constructor TCobrHandler.Create;
begin
  inherited;
  FFatHandler := TFatHandler.Create;
  FDupHandler := TDupHandler.Create;
end;

destructor TCobrHandler.Destroy;
begin
  FFatHandler.Free;
  FDupHandler.Free;
  inherited;
end;

procedure TCobrHandler.LerCobr(const ANode: TACBrXmlNode; const Cobr: TCobr);
begin
  if not Assigned(ANode) then exit;

  FFatHandler.LerFat(ANode.Childrens.FindAnyNs('fat'), Cobr.fat);
  FDupHandler.LerDup(ANode, Cobr.dup);
end;

{ TFatHandler }

procedure TFatHandler.LerFat(const ANode: TACBrXmlNode; const Fat: TFat);
begin
  if not Assigned(ANode)then exit;

  fat.nFat  := ObterConteudoTag(ANode.Childrens.FindAnyNs('nFat'), tcStr);
  fat.vOrig := ObterConteudoTag(ANode.Childrens.FindAnyNs('vOrig'), tcDe2);
  fat.vDesc := ObterConteudoTag(ANode.Childrens.FindAnyNs('vDesc'), tcDe2);
  fat.vLiq  := ObterConteudoTag(ANode.Childrens.FindAnyNs('vLiq'), tcDe2);
end;

{ TDupHandler }

procedure TDupHandler.LerDup(const ANode: TACBrXmlNode; const Dup: TDupCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('dup');
  dup.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    dup.New;
    dup[i].nDup  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nDup'), tcStr);
    dup[i].dVenc := ObterConteudoTag(AuxNodeArray[i].Childrens.FindANyNs('dVenc'), tcDat);
    dup[i].vDup  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vDup'), tcDe2);
  end;

end;

{ TInfGTVeHandler }

constructor TInfGTVeHandler.Create;
begin
  inherited;
  FInfGTVeCompHandler := TInfGTVeCompHandler.Create;
end;

destructor TInfGTVeHandler.Destroy;
begin
  FInfGTVeCompHandler.Free;
  inherited;
end;

procedure TInfGTVeHandler.LerInfGTVe(const ANode: TACBrXmlNode; const infGTVe: TinfGTVeCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXMLNodeArray;
begin
  if not Assigned(ANode) then exit;

  infGTVe.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infGTVe');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infGTVe.New;
    infGTVe[i].chCTe  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chCTe'), tcStr);

    FInfGTveCompHandler.LerComp(AuxNodeArray[i], infGTVe[i].Comp);
  end;
end;

{ TInfGTVeCompHandler }

procedure TInfGTVeCompHandler.LerComp(const ANode: TACBrXmlNode; const comp: TinfGTVeCompCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
  OK: Boolean;
begin
  if not Assigned(ANode) then exit;

  Comp.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('Comp');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    Comp.New;
    Comp[i].tpComp := StrTotpComp(Ok, ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('tpComp'), tcStr));
    Comp[i].vComp  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('vComp'), tcDe2);
    Comp[i].xComp  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xComp'), tcStr);
  end;
end;

{ TInfCTeSubHandler }

constructor TInfCTeSubHandler.Create;
begin
  inherited;
  FTomaICMSHandler := TTomaICMSHandler.Create;
  FTomaNaoICMSHandler := TTomaNaoICMSHandler.Create;
end;

destructor TInfCTeSubHandler.Destroy;
begin
  FTomaICMSHandler.Free;
  FTomaNaoICMSHandler.Free;
  inherited;
end;

procedure TInfCTeSubHandler.LerInfCTeSub(const ANode: TACBrXmlNode; const infCTeSub: TInfCteSub);
var
  indAlteraToma: String;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  infCTeSub.chCte := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCte'), tcStr);
  infCTeSub.refCteAnu := ObterConteudoTag(ANode.Childrens.FindAnyNs('refCteAnu'), tcStr);

  indAlteratoma := ObterConteudoTag(ANode.Childrens.FindAnyNs('indAlteraToma'), tcStr);
  if indAlteratoma <> '' then
    infCTeSub.indAlteraToma := StrToTIndicador(Ok, indAlteraToma);

  FTomaICMSHandler.LerTomaICMS(ANode.Childrens.FindAnyNs('tomaICMS'), infCTeSub.tomaICMS);
  FTomaNaoICMSHandler.LerNaoTomaICMS(ANode.Childrens.FindAnyNs('tomaNaoICMS'), infCTeSub.tomaNaoICMS);
end;

{ TTomaICMSHandler }

constructor TTomaICMSHandler.Create;
begin
  inherited;
  FRefNFHandler := TRefNFHandler.Create;
end;

destructor TTomaICMSHandler.Destroy;
begin
  FRefNFHandler.Free;
  inherited;
end;

procedure TTomaICMSHandler.LerTomaICMS(const ANode: TACBrXmlNode; const tomaICMS: TTomaICMS);
begin
  if not Assigned(ANode) then exit;

  tomaICMS.refNFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('refNFe'), tcStr);
  tomaICMS.refCte := ObterConteudoTag(ANode.Childrens.FindAnyNs('refCte'), tcStr);

  FRefNFHandler.LerRefNF(ANode.Childrens.FindAnyNs('refNF'), tomaICMS.refNF);
end;

{ TRefNFHandler }

procedure TRefNFHandler.LerRefNF(const ANode: TACBrXmlNode; const refNF: TRefNF);
begin
  if not Assigned(ANode) then exit;

  refNF.CNPJCPF  := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(refNF.CNPJCPF) = EmptyStr then
    refNF.CNPJCPF  := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

  refNF.modelo   := ObterConteudoTag(ANode.Childrens.FindAnyNs('mod'), tcStr);
  refNF.serie    := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie'), tcInt);
  refNF.subserie := ObterConteudoTag(ANode.Childrens.FindAnyNs('subserie'), tcInt);
  refNF.nro      := ObterConteudoTag(ANode.Childrens.FindAnyNs('nro'), tcInt);
  refNF.valor    := ObterConteudoTag(ANode.Childrens.FindAnyNs('valor'), tcDe2);
  refNF.dEmi     := ObterConteudoTag(ANode.Childrens.FindAnyNs('dEmi'), tcDat);
end;

{ TTomaNaoICMSHandler }

procedure TTomaNaoICMSHandler.LerNaoTomaICMS(const ANode: TACBrXmlNode; const tomaNaoICMS: TTomaNaoICMS);
begin
  if not Assigned(ANode) then exit;

  tomaNaoICMS.refCteAnu := ObterConteudoTag(ANode.Childrens.FindAnyNs('refCteAnu'), tcStr);
end;

{ TInfGlobalizadoHandler }

procedure TInfGlobalizadoHandler.LerInfGlobalizado(const ANode: TACBrXmlNode; const infGlobalizado: TinfGlobalizado);
begin
  if not Assigned(ANode) then exit;

  infGlobalizado.xObs := ObterConteudoTag(ANode.Childrens.FindAnyNs('xObs'), tcStr);
end;

{ TinfServVincHandler }

constructor TinfServVincHandler.Create;
begin
  inherited;
  FInfCTeMultiModalHandler := TInfCTeMultiModalHandler.Create;
end;

destructor TinfServVincHandler.Destroy;
begin
  FInfCTeMultiModalHandler.Free;
  inherited;
end;

procedure TinfServVincHandler.LerInfServVinc(const ANode: TACBrXmlNode; const InfServVinc: TinfServVinc);
begin
  if not Assigned(ANode) then exit;

  FInfCTeMultiModalHandler.LerInfCTeMultiModal(ANode, InfServVinc.infCTeMultimodal);
end;

{ TInfCTeMultiModalHandler }

procedure TInfCTeMultiModalHandler.LerInfCTeMultiModal(const ANode: TACBrXmlNode; const InfCteMultimodal: TinfCTeMultimodalCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  infCTeMultimodal.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infCTeMultimodal');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infCTeMultimodal.New;
    infCTeMultimodal[i].chCTeMultimodal := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chCTeMultimodal'), tcStr);
  end;
end;

{ TInfCTeCompHandler }

procedure TInfCTeCompHandler.LerInfCteComp(const ANode: TACBrXmlNode;
  const infCTeComp: TInfCteCompCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infCteComp');

  InfCTeComp.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    InfCTeComp.New;
    InfCTeComp[i].chCTe := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chCTe'), tcStr);
  end;
end;

procedure TInfCTeCompHandler.LerInfCteComp(const ANode: TACBrXmlNode;
  const infCTeComp: TInfCteComp);
begin
  if not Assigned(ANode) then exit;
  InfCTeComp.Chave := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCTe'), tcStr);
end;

{ TInfCTeAnuHandler }

procedure TInfCTeAnuHandler.LerInfCTeAnu(const ANode: TACBrXmlNode;
  const infCteAnu: TInfCteAnu);
begin
  if not Assigned(ANode) then exit;

  InfCTeAnu.chCTe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCte'), tcStr);
  InfCTeAnu.dEmi  := ObterConteudoTag(ANode.Childrens.FindAnyNs('dEmi'), tcDat);
end;

{ TAutXMLHandler }

procedure TAutXMLHandler.LerAutXML(const ANode: TACBrXMLNode;
  const autXML: TautXMLCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('autXML');

  autXML.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    autXML.New;
    autXML[i].CNPJCPF := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CNPJ'), tcStr);
    if Trim(autXML[i].CNPJCPF) = EmptyStr then
      autXML[i].CNPJCPF := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CPF'), tcStr);
  end;
end;

{ TinfRespTecHandler }

procedure TinfRespTecHandler.LerInfRespTec(const ANode: TACBrXMLNode;
  const infRespTec: TinfRespTec);
begin
  if not Assigned(ANode) then exit;

  infRespTec.CNPJ     := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  infRespTec.xContato := ObterConteudoTag(ANode.Childrens.FindAnyNs('xContato'), tcStr);
  infRespTec.email    := ObterConteudoTag(ANode.Childrens.FindAnyNs('email'), tcStr);
  infRespTec.fone     := ObterConteudoTag(ANode.Childrens.FindAnyNs('fone'), tcStr);
  infRespTec.idCSRT   := ObterConteudoTag(ANode.Childrens.FindAnyNs('idCSRT'), tcInt);
  infRespTec.hashCSRT := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashCSRT'), tcStr);
end;

{ TInfCTeHandler }

constructor TInfCTeHandler.Create;
begin
  inherited;
  FIdeHandler := TIdeHandler.Create;
  FComplHandler := TComplHandler.Create;
  FEmitHandler := TEmitHandler.Create;
  FTomaHandler := TTomaHandler.Create;
  FRemHandler := TRemHandler.Create;
  FExpedHandler := TExpedHandler.Create;
  FRecebHandler := TRecebHandler.Create;
  FDestHandler := TDestHandler.Create;
  FOrigemHandler := TEnderHandler.Create;
  FDestinoHandler := TEnderHandler.Create;
  FDetGTVHandler := TDetGTVeHandler.Create;
  FVPrestHandler := TVPrestHandler.Create;
  FImpHandler := TImpHandler.Create;
  FInfCTeNormHandler := TInfCTeNormHandler.Create;
  FInfCTeCompHandler := TInfCTeCompHandler.Create;
  FInfCTeAnuHandler := TInfCTeAnuHandler.Create;
  FAutXMLHandler := TAutXMLHandler.Create;
  FInfRespTecHandler := TinfRespTecHandler.Create;
end;

destructor TInfCTeHandler.Destroy;
begin
  FIdeHandler.Free;
  FComplHandler.Free;
  FEmitHandler.Free;
  FTomaHandler.Free;
  FRemHandler.Free;
  FExpedHandler.Free;
  FRecebHandler.Free;
  FDestHandler.Free;
  FOrigemHandler.Free;
  FDestinoHandler.Free;
  FDetGTVHandler.Free;
  FVPrestHandler.Free;
  FImpHandler.Free;
  FInfCTeNormHandler.Free;
  FInfCTeCompHandler.Free;
  FInfCTeAnuHandler.Free;
  FAutXMLHandler.Free;
  FInfRespTecHandler.Free;
  inherited;
end;

procedure TInfCTeHandler.LerInfCTe(const ANode: TACBrXmlNode; const CTe: TCTe);
begin
  FIdeHandler.LerIde(ANode.Childrens.FindAnyNs('ide'),  CTe.ide);
  FComplHandler.LerCompl(ANode.Childrens.FindAnyNs('compl'), CTe.compl);
  FEmitHandler.LerEmit(ANode.Childrens.FindAnyNs('emit'), CTe.emit);
  FTomaHandler.LerToma(ANode.Childrens.FindAnyNs('toma'), CTe.toma);
  FRemHandler.LerRem(ANode.Childrens.FindAnyNs('rem'), CTe.rem);
  FExpedHandler.LerExped(ANode.Childrens.FindAnyNs('exped'), CTe.exped);
  FRecebHandler.LerReceb(ANode.Childrens.FindAnyNs('receb'), CTe.receb);
  FDestHandler.LerDest(ANode.Childrens.FindAnyNs('dest'), CTe.dest);
  //Ambas classes são TEnderEmit
  FOrigemHandler.LerEnder(ANode.Childrens.FindAnyNs('origem'), CTe.origem);
  FDestinoHandler.LerEnder(ANode.Childrens.FindAnyNs('destino'), CTe.destino);

  FDetGTVHandler.LerDetGTVe(ANode.Childrens.FindAnyNs('detGTV'), CTe.detGTV);
  FVPrestHandler.LerVPrest(ANode.Childrens.FindAnyNs('vPrest'), CTe.vPrest);
  FImpHandler.LerImp(ANode.Childrens.FindAnyNs('imp'), CTe.imp);
  FInfCTeNormHandler.LerinfCTeNorm(ANode.Childrens.FindAnyNs('infCTeNorm'), CTe.infCTeNorm);
  if CTe.infCTe.versao <= 3 then
    FInfCteCompHandler.LerInfCteComp(ANode.Childrens.FindAnyNs('infCteComp'), CTe.infCteComp)
  else
    FInfCteCompHandler.LerInfCteComp(ANode, CTe.infCteComp10);

  FInfCteAnuHandler.LerInfCTeAnu(ANode.Childrens.FindAnyNs('infCteAnu'), CTe.infCteAnu);
  FAutXMLHandler.LerAutXML(ANode, CTe.autXML);
  FInfRespTecHandler.LerInfRespTec(ANode.Childrens.FindAnyNs('infRespTec'), CTe.infRespTec);
end;

{ TinfCTeSuplHandler }

procedure TinfCTeSuplHandler.LerInfCTeSupl(const ANode: TACBrXmlNode; const infCTeSupl: TinfCTeSupl);
begin
  if not Assigned(ANode) then exit;

  infCTeSupl.qrCodCTe := ObterConteudoTag(ANode.Childrens.Find('qrCodCTe'), tcStr);
  infCTeSupl.qrCodCTe := StringReplace(infCTeSupl.qrCodCTe, '<![CDATA[', '', []);
  infCTeSupl.qrCodCTe := StringReplace(infCTeSupl.qrCodCTe, ']]>', '', []);
end;

{ TSignatureHandler }

procedure TSignatureHandler.LerSignature(const ANode: TACBrXmlNode; const Signature: Tsignature);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  AuxNode := ANode.Childrens.FindAnyNs('SignedInfo');
  AuxNode := AuxNode.Childrens.FindAnyNs('Reference');
  if Assigned(AuxNode) then
  begin
    signature.URI := AuxNode.Attributes.Items['URI'].Content;
    signature.DigestValue := ObterConteudoTag(AuxNode.Childrens.Find('DigestValue'), tcStr);
  end;

  signature.SignatureValue := ObterConteudoTag(ANode.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('KeyInfo');
  AuxNode := AuxNode.Childrens.FindAnyNs('X509Data');
  if Assigned(AuxNode) then
    signature.X509Certificate := ObterConteudoTag(AuxNode.Childrens.Find('X509Certificate'), tcStr);
end;

{ TRodoOSHandler }

constructor TRodoOSHandler.Create;
begin
  inherited;
  FVeicOSHandler := TVeicOSHandler.Create;
  FinfFretamentoOS := TInfFretamentoHandler.Create;
end;

destructor TRodoOSHandler.Destroy;
begin
  FVeicOSHandler.Free;
  FinfFretamentoOS.Free;
  inherited;
end;

function TRodoOSHandler.LerRodoOS(const ANode: TACBrXmlNode; const RodoOS: TRodoOS): Boolean;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  rodoOS.TAF            := ObterConteudoTag(ANode.Childrens.FindAnyNs('TAF'), tcStr);
  rodoOS.NroRegEstadual := ObterConteudoTag(ANode.Childrens.FindAnyNs('NroRegEstadual'), tcStr);

  FVeicOSHandler.LerVeicOS(ANode.Childrens.FindAnyNs('veic'), RodoOS.veic);
  FinfFretamentoOS.LerInfFretamento(ANode.Childrens.FindAnyNs('infFretamento'), RodoOS.infFretamento);
  Result := True;
end;

{ TVeicOSHandler }

constructor TVeicOSHandler.Create;
begin
  inherited;
  FPropOSHandler := TPropOSHandler.Create;
end;

destructor TVeicOSHandler.Destroy;
begin
  FPropOSHandler.Free;
  inherited;
end;

procedure TVeicOSHandler.LerVeicOS(const ANode: TACBrXmlNode; const veicOS: TVeicOS);
begin
  if not Assigned(ANode) then exit;

  veicOS.placa   := ObterConteudoTag(ANode.Childrens.FindAnyNs('placa'), tcStr);
  veicOS.RENAVAM := ObterConteudoTag(ANode.Childrens.FindAnyNs('RENAVAM'), tcStr);
  veicOS.UF      := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);

  FPropOSHandler.LerPropOS(ANode.Childrens.FindAnyNs('prop'), veicOS.prop);
end;

{ TPropOSHandler }

procedure TPropOSHandler.LerPropOS(const ANode: TACBrXmlNode; const propOS: TPropOS);
var
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  propOS.CNPJCPF        := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  if Trim(propOS.CNPJCPF) = EmptyStr then
    propOS.CNPJCPF        := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);
  propOS.TAF            := ObterConteudoTag(ANode.Childrens.FindAnyNs('TAF'), tcStr);
  propOS.NroRegEstadual := ObterConteudoTag(ANode.Childrens.FindAnyNs('NroRegEstadual'), tcStr);
  propOS.xNome          := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  propOS.IE             := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  propOS.UF             := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  propOS.tpProp         := StrToTpProp(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpProp'), tcStr));
end;

{ TInfFretamentoHandler }

procedure TInfFretamentoHandler.LerInfFretamento(const ANode: TACBrXMLNode; const infFretamentoOS: TinfFretamento);
var
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  infFretamentoOS.tpFretamento := StrToTpFretamento(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpFretamento'), tcStr));
  infFretamentoOS.dhViagem     := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhViagem'), tcDatHor);
end;

{ TAereoHandler }

constructor TAereoHandler.Create;
begin
  inherited;
  FTarifaHandler := TTarifaHandler.Create;
  FNatCargaHandler := TNatCargaHandler.Create;
end;

destructor TAereoHandler.Destroy;
begin
  FTarifaHandler.Free;
  FNatCargaHandler.Free;
  inherited;
end;

function TAereoHandler.LerAereo(const ANode: TACBrXmlNode; const Aereo: TAereo): Boolean;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  aereo.nMinu      := ObterConteudoTag(ANode.Childrens.FindAnyNs('nMinu'), tcInt);
  aereo.nOCA       := ObterConteudoTag(ANode.Childrens.FindAnyNs('nOCA'), tcStr);
  aereo.dPrevAereo := ObterConteudoTag(ANode.Childrens.FindAnyNs('dPrevAereo'), tcDat);
  aereo.xLAgEmi    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xLAgEmi'), tcStr);
  aereo.IdT        := ObterConteudoTag(ANode.Childrens.FindAnyNs('IdT'), tcStr);

  FTarifaHandler.LerTarifa(ANode.Childrens.FindAnyNs('tarifa'), aereo.tarifa);
  FNatCargaHandler.LerNatCarga(ANode.Childrens.FindAnyNs('natCarga'), aereo.natCarga);
  Result := True;
end;

{ TTarifaHandler }

procedure TTarifaHandler.LerTarifa(const ANode: TACBrXmlNode; const tarifa: TTarifa);
begin
  if not Assigned(ANode) then exit;

  tarifa.CL     := ObterConteudoTag(ANode.Childrens.FindAnyNs('CL'), tcStr);
  tarifa.cTar   := ObterConteudoTag(ANode.Childrens.FindAnyNs('cTar'), tcStr);
  tarifa.vTar   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vTar'), tcDe2);
end;

{ TNatCargaHandler }

constructor TNatCargaHandler.Create;
begin
  inherited;
  FcInfManuHandler := TcInfManuHandler.Create;
end;

destructor TNatCargaHandler.Destroy;
begin
  FcInfManuHandler.Free;
  inherited;
end;

procedure TNatCargaHandler.LerNatCarga(const ANode: TACBrXmlNode; const natCarga: TNatCarga);
begin
  if not Assigned(ANode) then exit;

  natCarga.xDime     := ObterConteudoTag(ANode.Childrens.FindAnyNs('xDime'), tcStr);
  natCarga.cIMP      := ObterConteudoTag(ANode.Childrens.FindAnyNs('cIMP'), tcStr);

  FcInfManuHandler.LercInfManu(ANode, natCarga.cInfManu);
end;

{ TcInfManuHandler }

procedure TcInfManuHandler.LercInfManu(const ANode: TACBrXmlNode;
  const cInfManu: TpInfManuCollection);
var
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  cinfManu.Clear;
  for i:=0 to ANode.Childrens.Count-1 do
  begin
    if ANode.Childrens[i].Name = 'cInfManu' then
    begin
      with cInfManu.New do
        nInfManu := StrToTpInfManu(ok, ObterConteudoTag(ANode.Childrens[i], tcStr));
    end;
  end;
end;

{ TPeriHandler }

procedure TPeriHandler.LerPeri(const ANode: TACBrXmlNode; const peri: TPeriCollection);
var
  AuxNodeArray: TACBrXMLNodeArray;
  AuxNode: TACBrXmlNode;
  i: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('peri');
  peri.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    peri.New;
    peri[i].nONU := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nONU'), tcStr);
    peri[i].qTotEmb := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('qTotEmb'), tcStr);

    AuxNode := AuxNodeArray[i].Childrens.FindAnyNs('infTotAP');
    if Assigned(AuxNode) then
    begin
      peri[i].qTotProd := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('qTotProd'), tcStr);
      peri[i].uniAP := StrToUniMed(Ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('uniAP'), tcStr));
    end;
  end;
end;

{ TAquavHandler }

constructor TAquavHandler.Create;
begin
  inherited;
  FBalsaHandler := TBalsaHandler.Create;
  FDetContHandler := TDetContHandler.Create;
end;

destructor TAquavHandler.Destroy;
begin
  FBalsaHandler.Free;
  FDetContHandler.Free;
  inherited;
end;

function TAquavHandler.LerAquav(const ANode: TACBrXMLNode; const Aquav: TAquav): Boolean;
var
  Ok: Boolean;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  aquav.vPrest   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vPrest'), tcDe2);
  aquav.vAFRMM   := ObterConteudoTag(ANode.Childrens.FindAnyNs('vAFRMM'), tcDe2);
  aquav.nBooking := ObterConteudoTag(ANode.Childrens.FindAnyNs('nBooking'), tcStr);
  aquav.nCtrl    := ObterConteudoTag(ANode.Childrens.FindAnyNs('nCtrl'), tcStr);
  aquav.xNavio   := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNavio'), tcStr);
  aquav.nViag    := ObterConteudoTag(ANode.Childrens.FindAnyNs('nViag'), tcStr);
  aquav.direc    := StrToTpDirecao(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs( 'direc'), tcStr));
  aquav.prtEmb   := ObterConteudoTag(ANode.Childrens.FindAnyNs('prtEmb'), tcStr);
  aquav.prtTrans := ObterConteudoTag(ANode.Childrens.FindAnyNs('prtTrans'), tcStr);
  aquav.prtDest  := ObterConteudoTag(ANode.Childrens.FindAnyNs('prtDest'), tcStr);
  aquav.tpNav    := StrToTpNavegacao(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs( 'tpNav'), tcStr));
  aquav.irin     := ObterConteudoTag(ANode.Childrens.FindAnyNs('irin'), tcStr);

  FBalsaHandler.LerBalsa(ANode, aquav.balsa);
  FDetContHandler.LerDetCont(ANode, aquav.detCont);
  Result := True;
end;

{ TbalsaHandler }

procedure TbalsaHandler.LerBalsa(const ANode: TACBrXMLNode;
  const Balsa: TBalsaCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;
  balsa.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('balsa');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    balsa.New;
    balsa[i].xBalsa := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xBalsa'), tcStr);
  end;
end;

{ TDetContHandler }

constructor TDetContHandler.Create;
begin
  inherited;
  FLacreHandler := TLacreHandler.Create;
  FInfDocAquavHandler := TInfDocAquavHandler.Create;
end;

destructor TDetContHandler.Destroy;
begin
  FLacreHandler.Free;
  FInfDocAquavHandler.Free;
  inherited;
end;

procedure TDetContHandler.LerDetCont(const ANode: TACBrXmlNode;
  const DetCont: TdetContCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  if not Assigned(ANode)then exit;

  detCont.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('detCont');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    detCont.New;
    detCont[i].nCont := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nCont'), tcStr);

    FLacreHandler.LerLacre(AuxNodeArray[i], detCont[i].Lacre);
    FInfDocAquavHandler.LerInfDoc(AuxNodeArray[i].Childrens.FindAnyNs('infDoc'), detCont[i].infDoc);
  end;
end;

{ TLacreHandler }

procedure TLacreHandler.LerLacre(const ANode: TACBrXmlNode;
  const Lacre: TLacreCollection);
var
  i: Integer;
  AuxNodeArray: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  Lacre.Clear;
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('lacre');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    Lacre.New;
    Lacre[i].nLacre := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nLacre'), tcStr);
  end;
end;

{ TInfNFAquavHandler }

procedure TInfNFAquavHandler.LerinfNF(const ANode: TACBrXmlNode; const infNF: TInfNFAquavCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infNF');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infNF.New;
    infNF[i].serie   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('serie'), tcStr);
    infNF[i].nDoc    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('nDoc'), tcStr);
    infNF[i].unidRat := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('unidRat'), tcDe2);
  end;

end;

{ TInfNFeAquavHandler }

procedure TInfNFeAquavHandler.LerInfNFe(const ANode: TACBrXmlNode; const infNFe: TInfNFeAquavCollection);
var
  AuxNodeArray: TACBrXMLNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  AuxNodeArray := ANode.Childrens.FindAllAnyNs('infNFe');
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    infNFe.New;
    infNFe[i].chave   := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('chave'), tcStr);
    infNFe[i].unidRat := ObterConteudoTag(AuxNodeArray[i].Childrens.FIndAnyNs('unidRat'), tcDe2);
  end;
end;

{ TInfDocAquavHandler }

constructor TInfDocAquavHandler.Create;
begin
  inherited;
  FInfNfAquavHandler := TInfNFAquavHandler.Create;
  FInfNFeAquavHandler := TInfNFeAquavHandler.Create;
end;

destructor TInfDocAquavHandler.Destroy;
begin
  FInfNfAquavHandler.Free;
  FInfNFeAquavHandler.Free;
  inherited;
end;

procedure TInfDocAquavHandler.LerInfDoc(const ANode: TACBrXmlNode; const InfDoc: TInfDocAquav);
begin
  if not Assigned(ANode) then exit;

  FInfNfAquavHandler.LerinfNF(ANode, infDoc.infNF);
  FInfNFeAquavHandler.LerinfNFe(ANode, infDoc.infNFe);
end;

{ TFerrovHandler }

constructor TFerrovHandler.Create;
begin
  inherited;
  FTrafMutHandler := TTrafMutHandler.Create;
  FFerroEnvHandler := TFerroEnvHandler.Create;
end;

destructor TFerrovHandler.Destroy;
begin
  FTrafMutHandler.Free;
  FFerroEnvHandler.Free;
  inherited;
end;

function TFerrovHandler.LerFerrov(const ANode: TACBrXMLNode; const Ferrov: TFerrov): Boolean;
var
  Ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  ferrov.tpTraf := StrToTpTrafego(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpTraf'), tcStr));
  ferrov.fluxo  := ObterConteudoTag(ANode.Childrens.FindAnyNs('fluxo'), tcStr);
  ferrov.idTrem := ObterConteudoTag(ANode.Childrens.FindAnyNs('idTrem'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('trafMut');
  if Assigned(AuxNode) then
  begin
    ferrov.vFrete := ObterConteudoTag(AuxNode.Childrens.FindANyNs('vFrete'), tcDe2);
    FTrafMutHandler.LerTrafMut(AuxNode, ferrov.trafMut);
    FFerroEnvHandler.LerFerroEnv(AuxNode, Ferrov.ferroEnv);
  end;
  Result := True;
end;

{ TTrafMutHandler }

procedure TTrafMutHandler.LerTrafMut(const ANode: TACBrXmlNode; const TrafMut: TTrafMut);
var
  Ok: Boolean;
begin
  trafMut.respFat := StrToTrafegoMutuo(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('respFat'), tcStr));
  trafMut.ferrEmi := StrToTrafegoMutuo(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('ferrEmi'), tcStr));
  trafMut.chCTeFerroOrigem := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCTeFerroOrigem'), tcStr);
end;

{ TFerroEnvHandler }

constructor TFerroEnvHandler.Create;
begin
  inherited;
  FEnderFerroHandler := TenderFerroHandler.Create;
end;

destructor TFerroEnvHandler.Destroy;
begin
  FEnderFerroHandler.Free;
  inherited;
end;

procedure TFerroEnvHandler.LerFerroEnv(const ANode: TACBrXmlNode; const FerroEnv: TFerroEnvCollection);
var
  AuxNodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  AuxNodeArray := ANode.Childrens.FindAllAnyNs('ferroEnv');
  ferroEnv.Clear;
  for i:=0 to Length(AuxNodeArray)-1 do
  begin
    ferroEnv.New;
    ferroEnv[i].CNPJ  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('CNPJ'), tcStr);
    ferroEnv[i].cInt  := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('cInt'), tcStr);
    ferroEnv[i].IE    := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('IE'), tcStr);
    ferroEnv[i].xNome := ObterConteudoTag(AuxNodeArray[i].Childrens.FindAnyNs('xNome'), tcStr);

    FEnderFerroHandler.LerEnderFerro(AuxNodeArray[i].Childrens.FindAnyNs('enderFerro'), ferroEnv[i].enderFerro);
  end;
end;

{ TenderFerroHandler }

procedure TenderFerroHandler.LerEnderFerro(const ANode: TACBrXmlNode; const enderFerro: TEnderFerro);
begin
  if not Assigned(ANode) then exit;

  EnderFerro.xLgr    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  EnderFerro.nro     := ObterConteudoTag(ANode.Childrens.FindAnyNs('nro'), tcStr);
  EnderFerro.xCpl    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  EnderFerro.xBairro := ObterConteudoTag(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  EnderFerro.cMun    := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  EnderFerro.xMun    := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  EnderFerro.CEP     := ObterConteudoTag(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  EnderFerro.UF      := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
end;

{ TDutoHandler }

function TDutoHandler.LerDuto(const ANode: TACBrXmlNode; const Duto: TDuto): Boolean;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  duto.vTar := ObterConteudoTag(ANode.Childrens.FindAnyNs('vTar'), tcDe6);
  duto.dIni := ObterConteudoTag(ANode.Childrens.FindAnyNs('dIni'), tcDat);
  duto.dFim := ObterConteudoTag(ANode.Childrens.FindAnyNs('dFim'), tcDat);
  Result := True;
end;

{ TMultiModalHandler }

function TMultiModalHandler.LerMultiModal(const ANode: TACBrXmlNode; const Multimodal: TMultimodal): Boolean;
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  Result := False;
  if not Assigned(ANode) then exit;

  multimodal.COTM          := ObterConteudoTag(ANode.Childrens.FindAnyNs('COTM'), tcStr);
  multimodal.indNegociavel := StrToindNegociavel(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('indNegociavel'), tcStr));

  // dados sobre o seguro informados somente na versão 3.00
  AuxNode := ANode.Childrens.FindAnyNs('seg');
  multimodal.nApol         := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nApol'), tcStr);
  multimodal.nAver         := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nAver'), tcStr);

  AuxNode := AuxNode.Childrens.FindAnyNs('infSeg');
  multimodal.xSeg          := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xSeg'), tcStr);
  multimodal.CNPJ          := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
  Result := True;
end;

end.
