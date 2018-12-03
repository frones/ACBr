{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }
{******************************************************************************}


{$I ACBr.inc}

unit ACBrLibSATConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrLibConfig, ACBrSAT, ACBrSATClass, ACBrSATExtratoClass,
  ACBrIntegradorConfig, ACBrDFeSSL, ACBrSATExtratoESCPOS,
  pcnRede, pcnConversao;

type
  TTipoExtrato = (teFortes, teEscPos);

  { TExtratoConfig }
  TExtratoConfig = class
  private
    FTipoExtrato: TTipoExtrato;
    FMask_qCom: String;
    FMask_vUnCom: String;
    FImprimeQRCode: Boolean;
    FImprimeMsgOlhoNoImposto: Boolean;
    FImprimeCPFNaoInformado: Boolean;
    FPictureLogo: String;
    FMostrarPreview: Boolean;
    FMostrarSetup: Boolean;
    FNumCopias: Integer;
    FNomeArquivo: String;
    FFiltro: TACBrSATExtratoFiltro;
    FMsgAppQRCode: String;
    FImprimeEmUmaLinha: Boolean;
    FImprimeDescAcrescItem: Boolean;
    FUsaCodigoEanImpressao: Boolean;
    FLarguraBobina: Integer;
    FMargensTopo: Integer;
    FMargensEsquerda: Integer;
    FMargensFundo: Integer;
    FMargensDireita: Integer;
    FEspacoFinal: Integer;
    FLogoWidth: Integer;
    FLogoHeigth: Integer;
    FLogoStretch: Boolean;
    FLogoAutoSize: Boolean;
    FLogoCenter: Boolean;
    FLogoVisible: Boolean;
    FPrinterName: String;
    FImprimeChaveEmUmaLinha: TAutoSimNao;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property TipoExtrato: TTipoExtrato read FTipoExtrato write FTipoExtrato;
    property MaskqCom: String   read FMask_qCom      write FMask_qCom;
    property MaskvUnCom: String   read FMask_vUnCom    write FMask_vUnCom;
    property ImprimeQRCode: Boolean  read FImprimeQRCode  write FImprimeQRCode;
    property ImprimeMsgOlhoNoImposto: Boolean read FImprimeMsgOlhoNoImposto write FImprimeMsgOlhoNoImposto;
    property ImprimeCPFNaoInformado: Boolean read FImprimeCPFNaoInformado write FImprimeCPFNaoInformado;
    property PictureLogo: String read FPictureLogo write FPictureLogo;
    property MostraPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostraSetup: Boolean read FMostrarSetup write FMostrarSetup;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property NomeDocumento: String read FNomeArquivo  write FNomeArquivo ;
    property Filtro: TACBrSATExtratoFiltro read FFiltro write FFiltro;
    property MsgAppQRCode: String read FMsgAppQRCode write FMsgAppQRCode;
    property ImprimeEmUmaLinha: Boolean read FImprimeEmUmaLinha write FImprimeEmUmaLinha;
    property ImprimeDescAcrescItem: Boolean read FImprimeDescAcrescItem write FImprimeDescAcrescItem;
    property ImprimeCodigoEan: Boolean read FUsaCodigoEanImpressao write FUsaCodigoEanImpressao;
    property LarguraBobina: Integer read FLarguraBobina  write FLarguraBobina;
    property MargensTopo: Integer read FMargensTopo write FMargensTopo;
    property MargensEsquerda: Integer read FMargensEsquerda write FMargensEsquerda;
    property MargensFundo: Integer read FMargensFundo write FMargensFundo;
    property MargensDireita: Integer read FMargensDireita write FMargensDireita;
    property EspacoFinal: Integer read FEspacoFinal write FEspacoFinal;
    property LogoWidth: Integer read FLogoWidth write FLogoWidth;
    property LogoHeigth: Integer read FLogoHeigth write FLogoHeigth;
    property LogoStretch: Boolean read FLogoStretch write FLogoStretch;
    property LogoAutoSize: Boolean read FLogoAutoSize write FLogoAutoSize;
    property LogoCenter: Boolean read FLogoCenter write FLogoCenter;
    property LogoVisible: Boolean read FLogoVisible write FLogoVisible;
    property PrinterName: String read FPrinterName write FPrinterName;
    property ImprimeChaveEmUmaLinha: TAutoSimNao read FImprimeChaveEmUmaLinha
      write FImprimeChaveEmUmaLinha default rAuto;

  end;

  { TSATConfig }
  TSATConfig = class
  private
    Femit_CNPJ: string;
    Femit_cRegTrib: TpcnRegTrib;
    Femit_cRegTribISSQN: TpcnRegTribISSQN;
    Femit_IE: string;
    Femit_IM: string;
    Femit_indRatISSQN: TpcnindRatISSQN;
    Fide_CNPJ: string;
    Fide_numeroCaixa: integer;
    FinfCFe_versaoDadosEnt: real;
    Fide_tpAmb: TpcnTipoAmbiente;
    FPaginaDeCodigo: word;
    FArqSchema: string;
    FXmlSignLib: TSSLXmlSignLib;

    function GetEhUTF8: boolean;
    procedure SetEhUTF8(AValue: boolean);

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property infCFe_versaoDadosEnt: real read FinfCFe_versaoDadosEnt
      write FinfCFe_versaoDadosEnt;
    property ide_CNPJ: string read Fide_CNPJ write Fide_CNPJ;
    property ide_numeroCaixa: integer read Fide_numeroCaixa write Fide_numeroCaixa;
    property ide_tpAmb: TpcnTipoAmbiente read Fide_tpAmb write Fide_tpAmb;
    property emit_CNPJ: string read Femit_CNPJ write Femit_CNPJ;
    property emit_IE: string read Femit_IE write Femit_IE;
    property emit_IM: string read Femit_IM write Femit_IM;
    property emit_cRegTrib: TpcnRegTrib read Femit_cRegTrib write Femit_cRegTrib;
    property emit_cRegTribISSQN: TpcnRegTribISSQN read Femit_cRegTribISSQN write Femit_cRegTribISSQN;
    property emit_indRatISSQN: TpcnindRatISSQN read Femit_indRatISSQN write Femit_indRatISSQN;
    property EhUTF8: boolean read GetEhUTF8 write SetEhUTF8;
    property PaginaDeCodigo: word read FPaginaDeCodigo write FPaginaDeCodigo;
    property ArqSchema: string read FArqSchema write FArqSchema;
    property XmlSignLib: TSSLXmlSignLib read FXmlSignLib write FXmlSignLib;
  end;

  { TSATCertificado }
  TSATCertificado = class
  private
    FCryptLib: TSSLCryptLib;
    FArquivoPFX: String;
    FNumeroSerie: String;
    FSenha: String;
    FChaveCrypt: AnsiString;

    function GetSenha: String;

  public
    constructor Create(AChaveCrypt: AnsiString = '');

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property SSLCryptLib: TSSLCryptLib read FCryptLib write FCryptLib;
    property ArquivoPFX: String read FArquivoPFX write FArquivoPFX;
    property NumeroSerie: String read FNumeroSerie write FNumeroSerie;
    property Senha: String read GetSenha write FSenha;
  end;

  { TSATConfigArquivo }
  TSATConfigArquivo = class
  private
    FPrefixoArqCFe: string;
    FPrefixoArqCFeCanc: string;
    FSalvarCFe: boolean;
    FPastaCFeCancelamento: string;
    FPastaCFeVenda: string;
    FSalvarCFeCanc: boolean;
    FPastaEnvio: string;
    FSalvarEnvio: boolean;
    FSepararPorCNPJ: boolean;
    FSepararPorAno: boolean;
    FSepararPorMes: boolean;
    FSepararPorDia: boolean;
    FSepararPorModelo: boolean;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property SalvarCFe: boolean read FSalvarCFe write FSalvarCFe;
    property SalvarCFeCanc: boolean read FSalvarCFeCanc write FSalvarCFeCanc;
    property SalvarEnvio: boolean read FSalvarEnvio write FSalvarEnvio;
    property SepararPorCNPJ: boolean read FSepararPorCNPJ write FSepararPorCNPJ;
    property SepararPorModelo: boolean read FSepararPorModelo write FSepararPorModelo;
    property SepararPorAno: boolean read FSepararPorAno write FSepararPorAno;
    property SepararPorMes: boolean read FSepararPorMes write FSepararPorMes;
    property SepararPorDia: boolean read FSepararPorDia write FSepararPorDia;
    property PastaCFeVenda: string read FPastaCFeVenda write FPastaCFeVenda;
    property PastaCFeCancelamento: string read FPastaCFeCancelamento
      write FPastaCFeCancelamento;
    property PastaEnvio: string read FPastaEnvio write FPastaEnvio;
    property PrefixoArqCFe: string read FPrefixoArqCFe write FPrefixoArqCFe;
    property PrefixoArqCFeCanc: string read FPrefixoArqCFeCanc write FPrefixoArqCFeCanc;

  end;

  { TLibSATConfig }
  TLibSATConfig = class(TLibConfig)
  private
    FModelo: TACBrSATModelo;
    FNomeDLL: string;
    FCodigoDeAtivacao: string;
    FSignAC: string;
    FValidarNumeroSessaoResposta: boolean;
    FNumeroTentativasValidarSessao: integer;
    FArqLOG: string;
    FConfig: TSATConfig;
    FConfigArquivos: TSATConfigArquivo;
    FSATCertificado: TSATCertificado;
    FRede: TRede;
    FExtrato: TExtratoConfig;
    FIntegrador: TIntegradorConfig;
    FChaveCrypt: String;

    function GetIsMFe: Boolean;

  protected
    function AtualizarArquivoConfiguracao: boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: string = '';
      AChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Modelo: TACBrSATModelo read FModelo write FModelo;
    property NomeDLL: string read FNomeDLL write FNomeDLL;
    property CodigoDeAtivacao: string read FCodigoDeAtivacao write FCodigoDeAtivacao;
    property SignAC: string read FSignAC write FSignAC;
    property ValidarNumeroSessaoResposta: boolean read FValidarNumeroSessaoResposta write FValidarNumeroSessaoResposta;
    property NumeroTentativasValidarSessao: integer read FNumeroTentativasValidarSessao write FNumeroTentativasValidarSessao;
    property ArqLOG: string read FArqLOG write FArqLOG;
    property IsMFe: boolean read GetIsMFe;
    property Config: TSATConfig read FConfig;
    property Arquivos: TSATConfigArquivo read FConfigArquivos;
    property Certificado: TSATCertificado read FSATCertificado;
    property Rede: TRede read FRede;
    property Extrato: TExtratoConfig read FExtrato;
    property Integrador: TIntegradorConfig read FIntegrador;

  end;

implementation

uses
  ACBrLibSATClass, ACBrLibSATConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil, ACBrConsts;

{ TExtratoConfig }
constructor TExtratoConfig.Create;
begin
  FTipoExtrato := teFortes;
  FMask_qCom := ',0.0000';
  FMask_vUnCom := ',0.000';
  FImprimeQRCode := True;
  FImprimeMsgOlhoNoImposto := True;
  FImprimeCPFNaoInformado := True;
  FPictureLogo := '';
  FMostrarPreview := False;
  FMostrarSetup := False;
  FNumCopias := 1;
  FNomeArquivo := '';
  FFiltro := fiNenhum;
  FMsgAppQRCode := ACBrStr(cMsgAppQRCode);
  FImprimeEmUmaLinha := True;
  FImprimeDescAcrescItem := True;
  FUsaCodigoEanImpressao := False;
  FLarguraBobina := 302;
  FMargensTopo := 2;
  FMargensEsquerda := 2;
  FMargensFundo := 4;
  FMargensDireita := 2;
  FEspacoFinal := 0;
  FLogoWidth := 77;
  FLogoHeigth := 50;
  FLogoStretch := False;
  FLogoAutoSize := True;
  FLogoCenter := True;
  FLogoVisible := True;
  FPrinterName := '';
  FImprimeChaveEmUmaLinha := rAuto;
end;

procedure TExtratoConfig.LerIni(const AIni: TCustomIniFile);
begin
  FTipoExtrato := TTipoExtrato(AIni.ReadInteger(CSessaoExtrato, CChaveTipo, Integer(FTipoExtrato)));
  FMask_qCom := AIni.ReadString(CSessaoExtrato, CChaveMaskqCom, FMask_qCom);
  FMask_vUnCom := AIni.ReadString(CSessaoExtrato, CChaveMaskvUnCom, FMask_vUnCom);
  FImprimeQRCode := AIni.ReadBool(CSessaoExtrato, CChaveImprimeQRCode, FImprimeQRCode);
  FImprimeMsgOlhoNoImposto := AIni.ReadBool(CSessaoExtrato, CChaveImprimeMsgOlhoNoImposto, FImprimeMsgOlhoNoImposto);
  FImprimeCPFNaoInformado := AIni.ReadBool(CSessaoExtrato, CChaveImprimeCPFNaoInformado, FImprimeCPFNaoInformado);
  FPictureLogo := AIni.ReadString(CSessaoExtrato, CChavePictureLogo, FPictureLogo);
  FMostrarPreview := AIni.ReadBool(CSessaoExtrato, CChaveMostraPreview, FMostrarPreview);
  FMostrarSetup := AIni.ReadBool(CSessaoExtrato, CChaveMostraSetup, FMostrarSetup);
  FNumCopias := AIni.ReadInteger(CSessaoExtrato, CChaveNumCopias, FNumCopias);
  FNomeArquivo := AIni.ReadString(CSessaoExtrato, CChaveNomeDocumento, FNomeArquivo);
  FFiltro := TACBrSATExtratoFiltro(AIni.ReadInteger(CSessaoExtrato, CChaveFiltro, Integer(FFiltro)));
  FMsgAppQRCode := AIni.ReadString(CSessaoExtrato, CChaveMsgAppQRCode, FMsgAppQRCode);
  FImprimeEmUmaLinha := AIni.ReadBool(CSessaoExtrato, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  FImprimeDescAcrescItem := AIni.ReadBool(CSessaoExtrato, CChaveImprimeDescAcrescItem, FImprimeDescAcrescItem);
  FUsaCodigoEanImpressao := AIni.ReadBool(CSessaoExtrato, CChaveImprimeCodigoEan, FUsaCodigoEanImpressao);
  FLarguraBobina := AIni.ReadInteger(CSessaoExtrato, CChaveLarguraBobina, FLarguraBobina);
  FMargensTopo := AIni.ReadInteger(CSessaoExtrato, CChaveMargensTopo, FMargensTopo);
  FMargensEsquerda := AIni.ReadInteger(CSessaoExtrato, CChaveMargensEsquerda, FMargensEsquerda);
  FMargensFundo := AIni.ReadInteger(CSessaoExtrato, CChaveMargensFundo, FMargensFundo);
  FMargensDireita := AIni.ReadInteger(CSessaoExtrato, CChaveMargensDireita, FMargensDireita);
  FEspacoFinal := AIni.ReadInteger(CSessaoExtrato, CChaveEspacoFinal, FEspacoFinal);
  FLogoWidth := AIni.ReadInteger(CSessaoExtrato, CChaveLogoWidth, FLogoWidth);
  FLogoHeigth := AIni.ReadInteger(CSessaoExtrato, CChaveLogoHeigth, FLogoHeigth);
  FLogoStretch := AIni.ReadBool(CSessaoExtrato, CChaveLogoStretch, FLogoStretch);
  FLogoAutoSize := AIni.ReadBool(CSessaoExtrato, CChaveLogoAutoSize, FLogoAutoSize);
  FLogoCenter := AIni.ReadBool(CSessaoExtrato, CChaveLogoCenter, FLogoCenter);
  FLogoVisible := AIni.ReadBool(CSessaoExtrato, CChaveLogoVisible, FLogoVisible);
  FPrinterName := AIni.ReadString(CSessaoExtrato, CChavePrinterName, FPrinterName);
  FImprimeChaveEmUmaLinha := TAutoSimNao(AIni.ReadInteger(CSessaoExtrato, CChaveImprimeChaveEmUmaLinha,
    Integer(FImprimeChaveEmUmaLinha)));
end;

procedure TExtratoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoExtrato, CChaveTipo, Integer(FTipoExtrato));
  AIni.WriteString(CSessaoExtrato, CChaveMaskqCom, FMask_qCom);
  AIni.WriteString(CSessaoExtrato, CChaveMaskvUnCom, FMask_vUnCom);
  AIni.WriteBool(CSessaoExtrato, CChaveImprimeQRCode, FImprimeQRCode);
  AIni.WriteBool(CSessaoExtrato, CChaveImprimeMsgOlhoNoImposto, FImprimeMsgOlhoNoImposto);
  AIni.WriteBool(CSessaoExtrato, CChaveImprimeCPFNaoInformado, FImprimeCPFNaoInformado);
  AIni.WriteString(CSessaoExtrato, CChavePictureLogo, FPictureLogo);
  AIni.WriteBool(CSessaoExtrato, CChaveMostraPreview, FMostrarPreview);
  AIni.WriteBool(CSessaoExtrato, CChaveMostraSetup, FMostrarSetup);
  AIni.WriteInteger(CSessaoExtrato, CChaveNumCopias, FNumCopias);
  AIni.WriteString(CSessaoExtrato, CChaveNomeDocumento, FNomeArquivo);
  AIni.WriteInteger(CSessaoExtrato, CChaveFiltro, Integer(FFiltro));
  AIni.WriteString(CSessaoExtrato, CChaveMsgAppQRCode, FMsgAppQRCode);
  AIni.WriteBool(CSessaoExtrato, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  AIni.WriteBool(CSessaoExtrato, CChaveImprimeDescAcrescItem, FImprimeDescAcrescItem);
  AIni.WriteBool(CSessaoExtrato, CChaveImprimeCodigoEan, FUsaCodigoEanImpressao);
  AIni.WriteInteger(CSessaoExtrato, CChaveLarguraBobina, FLarguraBobina);
  AIni.WriteInteger(CSessaoExtrato, CChaveMargensTopo, FMargensTopo);
  AIni.WriteInteger(CSessaoExtrato, CChaveMargensEsquerda, FMargensEsquerda);
  AIni.WriteInteger(CSessaoExtrato, CChaveMargensFundo, FMargensFundo);
  AIni.WriteInteger(CSessaoExtrato, CChaveMargensDireita, FMargensDireita);
  AIni.WriteInteger(CSessaoExtrato, CChaveEspacoFinal, FEspacoFinal);
  AIni.WriteInteger(CSessaoExtrato, CChaveLogoWidth, FLogoWidth);
  AIni.WriteInteger(CSessaoExtrato, CChaveLogoHeigth, FLogoHeigth);
  AIni.WriteBool(CSessaoExtrato, CChaveLogoStretch, FLogoStretch);
  AIni.WriteBool(CSessaoExtrato, CChaveLogoAutoSize, FLogoAutoSize);
  AIni.WriteBool(CSessaoExtrato, CChaveLogoCenter, FLogoCenter);
  AIni.WriteBool(CSessaoExtrato, CChaveLogoVisible, FLogoVisible);
  AIni.WriteString(CSessaoExtrato, CChavePrinterName, FPrinterName);
  AIni.WriteInteger(CSessaoExtrato, CChaveImprimeChaveEmUmaLinha, Integer(FImprimeChaveEmUmaLinha));
end;

{ TSATConfig }
constructor TSATConfig.Create;
begin
  Femit_CNPJ := '';
  Femit_cRegTrib := RTSimplesNacional;
  Femit_cRegTribISSQN := RTISSMicroempresaMunicipal;
  Femit_IE := '';
  Femit_IM := '';
  Femit_indRatISSQN := irSim;
  Fide_CNPJ := '';
  Fide_numeroCaixa := 0;
  Fide_tpAmb := taHomologacao;
  FinfCFe_versaoDadosEnt := cversaoDadosEnt;
  FArqSchema := '';
  FXmlSignLib := xsNone;
end;

procedure TSATConfig.LerIni(const AIni: TCustomIniFile);
begin
  infCFe_versaoDadosEnt := AIni.ReadFloat(CSessaoSATConfig,
    CChaveVersaoDadosEnt, infCFe_versaoDadosEnt);
  ide_CNPJ := AIni.ReadString(CSessaoSATConfig, CChaveIdeCNPJ, ide_CNPJ);
  ide_numeroCaixa := AIni.ReadInteger(CSessaoSATConfig, CChaveIdeNumeroCaixa,
    ide_numeroCaixa);
  ide_tpAmb := TpcnTipoAmbiente(AIni.ReadInteger(CSessaoSATConfig,
    CChaveIdeTpAmb, integer(ide_tpAmb)));
  emit_CNPJ := AIni.ReadString(CSessaoSATConfig, CChaveEmitCNPJ, emit_CNPJ);
  emit_IE := AIni.ReadString(CSessaoSATConfig, CChaveEmitIE, emit_IE);
  emit_IM := AIni.ReadString(CSessaoSATConfig, CChaveEmitIM, emit_IM);
  emit_cRegTrib := TpcnRegTrib(AIni.ReadInteger(CSessaoSATConfig,
    CChaveEmitcRegTrib, integer(emit_cRegTrib)));
  emit_cRegTribISSQN := TpcnRegTribISSQN(AIni.ReadInteger(CSessaoSATConfig,
    CChaveEmitcRegTribISSQN, integer(emit_cRegTribISSQN)));
  emit_indRatISSQN := TpcnindRatISSQN(AIni.ReadInteger(CSessaoSATConfig,
    CChaveEmitIndRatISSQN, integer(emit_indRatISSQN)));
  EhUTF8 := AIni.ReadBool(CSessaoSATConfig, CChaveEhUTF8, EhUTF8);
  PaginaDeCodigo := AIni.ReadInteger(CSessaoSATConfig, CChavePaginaDeCodigo,
    PaginaDeCodigo);
  ArqSchema := AIni.ReadString(CSessaoSATConfig, CChaveArqSchema, ArqSchema);
  XmlSignLib := TSSLXmlSignLib(AIni.ReadInteger(CSessaoSATConfig,
    CChaveXmlSignLib, integer(XmlSignLib)));
end;

procedure TSATConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteFloat(CSessaoSATConfig, CChaveVersaoDadosEnt, infCFe_versaoDadosEnt);
  AIni.WriteString(CSessaoSATConfig, CChaveIdeCNPJ, ide_CNPJ);
  AIni.WriteInteger(CSessaoSATConfig, CChaveIdeNumeroCaixa, ide_numeroCaixa);
  AIni.WriteInteger(CSessaoSATConfig, CChaveIdeTpAmb, integer(ide_tpAmb));
  AIni.WriteString(CSessaoSATConfig, CChaveEmitCNPJ, emit_CNPJ);
  AIni.WriteString(CSessaoSATConfig, CChaveEmitIE, emit_IE);
  AIni.WriteString(CSessaoSATConfig, CChaveEmitIM, emit_IM);
  AIni.WriteInteger(CSessaoSATConfig, CChaveEmitcRegTrib, integer(emit_cRegTrib));
  AIni.WriteInteger(CSessaoSATConfig, CChaveEmitcRegTribISSQN,
    integer(emit_cRegTribISSQN));
  AIni.WriteInteger(CSessaoSATConfig, CChaveEmitIndRatISSQN, integer(emit_indRatISSQN));
  AIni.WriteBool(CSessaoSATConfig, CChaveEhUTF8, EhUTF8);
  AIni.WriteInteger(CSessaoSATConfig, CChavePaginaDeCodigo, PaginaDeCodigo);
  AIni.WriteString(CSessaoSATConfig, CChaveArqSchema, ArqSchema);
  AIni.WriteInteger(CSessaoSATConfig, CChaveXmlSignLib, integer(XmlSignLib));
end;

function TSATConfig.GetEhUTF8: boolean;
begin
  Result := (FPaginaDeCodigo = CUTF8CodPage);
end;

procedure TSATConfig.SetEhUTF8(AValue: boolean);
begin
  if AValue then
    FPaginaDeCodigo := CUTF8CodPage
  else
  begin
    if FPaginaDeCodigo = CUTF8CodPage then
      FPaginaDeCodigo := 0;
  end;
end;

{ TSATCertificado }
constructor TSATCertificado.Create(AChaveCrypt: AnsiString = '');
begin
  FChaveCrypt := AChaveCrypt;
  FCryptLib := cryNone;
  FArquivoPFX := '';
  FNumeroSerie := '';
  FSenha := '';
end;

function TSATCertificado.GetSenha: String;
begin
  Result := B64CryptToString(FSenha, FChaveCrypt);
end;

procedure TSATCertificado.LerIni(const AIni: TCustomIniFile);
begin
  FCryptLib := TSSLCryptLib(AIni.ReadInteger(CSessaoDFe, CChaveSSLCryptLib, Integer(FCryptLib)));
  FArquivoPFX := AIni.ReadString(CSessaoDFe, CChaveArquivoPFX, FArquivoPFX);
  FNumeroSerie := AIni.ReadString(CSessaoDFe, CChaveNumeroSerie, FNumeroSerie);
  FSenha := AIni.ReadString(CSessaoDFe, CChaveSenha, FSenha);
end;

procedure TSATCertificado.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoDFe, CChaveSSLCryptLib, Integer(FCryptLib));
  AIni.WriteString(CSessaoDFe, CChaveArquivoPFX, FArquivoPFX);
  AIni.WriteString(CSessaoDFe, CChaveNumeroSerie, FNumeroSerie);
  AIni.WriteString(CSessaoDFe, CChaveSenha, FSenha);
end;

{ TSATConfigArquivo }
constructor TSATConfigArquivo.Create;
begin
  FPastaCFeCancelamento := '';
  FPastaCFeVenda := '';
  FSalvarCFe := False;
  FSalvarCFeCanc := False;
  FSalvarEnvio := False;
  FSepararPorCNPJ := False;
  FSepararPorModelo := False;
  FSepararPorMes := False;
  FSepararPorDia := False;
  FPrefixoArqCFe := CPREFIXO_ArqCFe;
  FPrefixoArqCFeCanc := CPREFIXO_ArqCFeCanc;
end;

procedure TSATConfigArquivo.LerIni(const AIni: TCustomIniFile);
begin
  SalvarCFe := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSalvarCFe, SalvarCFe);
  SalvarCFeCanc := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSalvarCFeCanc,
    SalvarCFeCanc);
  SalvarEnvio := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSalvarEnvio, SalvarEnvio);
  SepararPorCNPJ := AIni.ReadBool(CSessaoSATConfigArquivos,
    CChaveSepararPorCNPJ, SepararPorCNPJ);
  SepararPorModelo := AIni.ReadBool(CSessaoSATConfigArquivos,
    CChaveSepararPorModelo, SepararPorModelo);
  SepararPorAno := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorAno,
    SepararPorAno);
  SepararPorMes := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorMes,
    SepararPorMes);
  SepararPorDia := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorDia,
    SepararPorDia);
  PastaCFeVenda := AIni.ReadString(CSessaoSATConfigArquivos,
    CChavePastaCFeVenda, PastaCFeVenda);
  PastaCFeCancelamento := AIni.ReadString(CSessaoSATConfigArquivos,
    CChavePastaCFeCancelamento, PastaCFeCancelamento);
  PastaEnvio := AIni.ReadString(CSessaoSATConfigArquivos, CChavePastaEnvio, PastaEnvio);
  PrefixoArqCFe := AIni.ReadString(CSessaoSATConfigArquivos,
    CChavePrefixoArqCFe, PrefixoArqCFe);
  PrefixoArqCFeCanc := AIni.ReadString(CSessaoSATConfigArquivos,
    CChavePrefixoArqCFeCanc, PrefixoArqCFeCanc);
end;

procedure TSATConfigArquivo.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSalvarCFe, SalvarCFe);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSalvarCFeCanc, SalvarCFeCanc);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSalvarEnvio, SalvarEnvio);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorCNPJ, SepararPorCNPJ);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorModelo, SepararPorModelo);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorAno, SepararPorAno);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorMes, SepararPorMes);
  AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorDia, SepararPorDia);
  AIni.WriteString(CSessaoSATConfigArquivos, CChavePastaCFeVenda, PastaCFeVenda);
  AIni.WriteString(CSessaoSATConfigArquivos, CChavePastaCFeCancelamento,
    PastaCFeCancelamento);
  AIni.WriteString(CSessaoSATConfigArquivos, CChavePastaEnvio, PastaEnvio);
  AIni.WriteString(CSessaoSATConfigArquivos, CChavePrefixoArqCFe, PrefixoArqCFe);
  AIni.WriteString(CSessaoSATConfigArquivos, CChavePrefixoArqCFeCanc, PrefixoArqCFeCanc);
end;

{ TLibSATConfig }

constructor TLibSATConfig.Create(AOwner: TObject; ANomeArquivo: string;
  AChaveCrypt: ansistring);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FModelo := satNenhum;
  FNomeDLL := '';
  FArqLOG := '';
  FCodigoDeAtivacao := '';
  FSignAC := '';
  FValidarNumeroSessaoResposta := False;
  FNumeroTentativasValidarSessao := CMAX_ERROS_SESSAO;

  FConfig := TSATConfig.Create;
  FConfigArquivos := TSATConfigArquivo.Create;
  FSATCertificado := TSATCertificado.Create(AChaveCrypt);
  FRede := TRede.Create;
  FExtrato := TExtratoConfig.Create;
  FIntegrador := TIntegradorConfig.Create;
  FChaveCrypt := AChaveCrypt;
end;

destructor TLibSATConfig.Destroy;
begin
  FConfig.Free;
  FConfigArquivos.Free;
  FSATCertificado.Free;
  FRede.Free;
  FExtrato.Free;

  inherited Destroy;
end;

function TLibSATConfig.GetIsMFe: Boolean;
begin
  Result := FModelo = mfe_Integrador_XML;
end;

procedure TLibSATConfig.LerIni(const AIni: TCustomIniFile);
begin
  FModelo := TACBrSATModelo(AIni.ReadInteger(CSessaoSAT, CChaveModelo,
    integer(FModelo)));
  FNomeDLL := AIni.ReadString(CSessaoSAT, CChaveNomeDLL, FNomeDLL);
  FArqLOG := AIni.ReadString(CSessaoSAT, CChaveArqLog, FArqLOG);
  FCodigoDeAtivacao := AIni.ReadString(CSessaoSAT, CChaveCodigoDeAtivacao, FCodigoDeAtivacao);
  FSignAC := AIni.ReadString(CSessaoSAT, CChaveSignAC, FSignAC);
  FValidarNumeroSessaoResposta :=
    AIni.ReadBool(CSessaoSAT, CChaveValidarNumero, FValidarNumeroSessaoResposta);
  FNumeroTentativasValidarSessao :=
    AIni.ReadInteger(CSessaoSAT, CChaveNumeroTentativas, FNumeroTentativasValidarSessao);

  with FRede do
  begin
    tipoInter := TTipoInterface(AIni.ReadInteger(CSessaoSATRede,
      CChaveTipoInter, integer(tipoInter)));
    SSID := AIni.ReadString(CSessaoSATRede, CChaveSSID, SSID);
    seg := TSegSemFio(AIni.ReadInteger(CSessaoSATRede, CChaveSeg, integer(seg)));
    codigo := AIni.ReadString(CSessaoSATRede, CChaveCodigo, codigo);
    tipoLan := TTipoLan(AIni.ReadInteger(CSessaoSATRede, CChaveTipoLan,
      integer(tipoLan)));
    lanIP := AIni.ReadString(CSessaoSATRede, CChaveLanIP, lanIP);
    lanMask := AIni.ReadString(CSessaoSATRede, CChaveLanMask, lanMask);
    lanGW := AIni.ReadString(CSessaoSATRede, CChaveLanGW, lanGW);
    lanDNS1 := AIni.ReadString(CSessaoSATRede, CChaveLanDNS1, lanDNS1);
    lanDNS2 := AIni.ReadString(CSessaoSATRede, CChaveLanDNS2, lanDNS2);
    usuario := AIni.ReadString(CSessaoSATRede, CChaveUsuario, usuario);
    senha := AIni.ReadString(CSessaoSATRede, CChaveSenha, '');
    proxy := AIni.ReadInteger(CSessaoSATRede, CChaveProxy, proxy);
    proxy_ip := AIni.ReadString(CSessaoSATRede, CChaveProxyIp, proxy_ip);
    proxy_porta := AIni.ReadInteger(CSessaoSATRede, CChaveProxyPorta, proxy_porta);
    proxy_user := AIni.ReadString(CSessaoSATRede, CChaveProxyUser, proxy_user);
    proxy_senha := AIni.ReadString(CSessaoSATRede, CChaveProxySenha, '');
  end;

  FConfig.LerIni(AIni);
  FConfigArquivos.LerIni(AIni);
  FExtrato.LerIni(AIni);
  FIntegrador.LerIni(AIni);
end;

procedure TLibSATConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoSAT, CChaveModelo, integer(FModelo));
  AIni.WriteString(CSessaoSAT, CChaveNomeDLL, FNomeDLL);
  AIni.WriteString(CSessaoSAT, CChaveArqLog, FArqLOG);
  AIni.WriteString(CSessaoSAT, CChaveCodigoDeAtivacao, FCodigoDeAtivacao);
  AIni.WriteString(CSessaoSAT, CChaveSignAC, FSignAC);
  AIni.WriteBool(CSessaoSAT, CChaveValidarNumero, FValidarNumeroSessaoResposta);
  AIni.WriteInteger(CSessaoSAT, CChaveNumeroTentativas, FNumeroTentativasValidarSessao);

  with FRede do
  begin
    AIni.WriteInteger(CSessaoSATRede, CChaveTipoInter, integer(tipoInter));
    AIni.WriteString(CSessaoSATRede, CChaveSSID, SSID);
    AIni.WriteInteger(CSessaoSATRede, CChaveSeg, integer(seg));
    AIni.WriteString(CSessaoSATRede, CChaveCodigo, codigo);
    AIni.WriteInteger(CSessaoSATRede, CChaveTipoLan, integer(tipoLan));
    AIni.WriteString(CSessaoSATRede, CChaveLanIP, lanIP);
    AIni.WriteString(CSessaoSATRede, CChaveLanMask, lanMask);
    AIni.WriteString(CSessaoSATRede, CChaveLanGW, lanGW);
    AIni.WriteString(CSessaoSATRede, CChaveLanDNS1, lanDNS1);
    AIni.WriteString(CSessaoSATRede, CChaveLanDNS2, lanDNS2);
    AIni.WriteString(CSessaoSATRede, CChaveUsuario, usuario);
    AIni.WriteString(CSessaoSATRede, CChaveSenha, StringToB64Crypt(senha, FChaveCrypt));
    AIni.WriteInteger(CSessaoSATRede, CChaveProxy, proxy);
    AIni.WriteString(CSessaoSATRede, CChaveProxyIp, proxy_ip);
    AIni.WriteInteger(CSessaoSATRede, CChaveProxyPorta, proxy_porta);
    AIni.WriteString(CSessaoSATRede, CChaveProxyUser, proxy_user);
    AIni.WriteString(CSessaoSATRede, CChaveProxySenha, StringToB64Crypt(proxy_senha, FChaveCrypt));
  end;

  FConfig.GravarIni(AIni);
  FConfigArquivos.GravarIni(AIni);
  FExtrato.GravarIni(AIni);
  FIntegrador.GravarIni(AIni);
end;

function TLibSATConfig.AtualizarArquivoConfiguracao: boolean;
var
  Versao: string;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibSATNome, '0');
  Result := (CompareVersions(CLibSATVersao, Versao) > 0) or
    (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibSATConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  LerIni(Ini);
end;

procedure TLibSATConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibSATNome, CLibSATVersao);

  GravarIni(Ini);
end;

procedure TLibSATConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibSAT(Owner).SatDM.AplicarConfiguracoes;
end;

procedure TLibSATConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibSAT(Owner) do
      SatDM.Travar;
  end;
end;

procedure TLibSATConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibSAT(Owner) do
      SatDM.Destravar;
  end;
end;

end.
