{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibNFeConfig;

interface

uses
  Classes, Graphics, SysUtils, IniFiles,
  pcnConversao, pcnConversaoNFe, ACBrLibComum,
  ACBrNFeConfiguracoes, ACBrDFeReport, ACBrDFeDANFeReport,
  ACBrNFeDANFEClass, ACBrNFeDANFeRLClass, ACBrLibConfig,
  DFeReportConfig;

type
  TTipoRelatorioBobina = (tpFortes, tpEscPos, tpFortesA4);
  TTipoRelatorioEvento = (evA4, evBobina);

  { TDANFeNFeConfig }
  TDANFeNFeConfig = class
  private
    FImprimeDescPorPercentual: Boolean;
    FFormularioContinuo: Boolean;
    FImprimeValor: TImprimirUnidQtdeValor;
    FImprimeDetalhamentoEspecifico: Boolean;
    FPosCanhoto: TPosRecibo;
    FPosCanhotoLayout: TPosReciboLayout;
    FExibeResumoCanhoto: Boolean;
    FTextoResumoCanhoto: String;
    FExibeCampoFatura: Boolean;
    FExibeDadosISSQN: Boolean;
    FExibeDadosDocReferenciados: Boolean;
    FDetVeiculos: TDetVeiculos;
    FDetMedicamentos: TDetMedicamentos;
    FDetArmamentos: TDetArmamentos;
    FDetCombustiveis: TDetCombustiveis;
    FTributosPercentual: TpcnPercentualTributos;
    FTributosPercentualPersonalizado: Double;
    FMarcadagua: String;
    FLarguraCodProd: Integer;
    FFonte: TFonte;
    FExibeEAN: Boolean;
    FAltLinhaComun: Integer;
    FEspacoEntreProdutos: Integer;
    FAlternaCoresProdutos: Boolean;
    FCorDestaqueProdutos: TColor;
    FTamanhoLogoHeight: Integer;
    FTamanhoLogoWidth: Integer;
    FRecuoEndereco: Integer;
    FRecuoEmpresa: Integer;
    FLogoEmCima: Boolean;
    FRecuoLogo: Integer;
    FExpandirDadosAdicionaisAuto: boolean;
    FImprimeContDadosAdPrimeiraPagina: Boolean;
    FExibeCampoDePagamento: TpcnInformacoesDePagamento;
    FImprimeInscSuframa: Boolean;
    FImprimeXPedNitemPed: Boolean;
    FImprimeDescAcrescItemNFe: TpcnImprimeDescAcrescItem;

  public
    constructor Create;
    destructor Destroy; override;

    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);
    procedure Apply(const DFeReport: TACBrNFeDANFeRL);

  published
    property FormularioContinuo: Boolean read FFormularioContinuo write FFormularioContinuo;
    property ImprimeValor: TImprimirUnidQtdeValor read FImprimeValor write FImprimeValor;
    property ImprimeDescPorPercentual: Boolean read FImprimeDescPorPercentual write FImprimeDescPorPercentual;
    property ImprimeDetalhamentoEspecifico: Boolean read FImprimeDetalhamentoEspecifico write FImprimeDetalhamentoEspecifico;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property PosCanhotoLayout: TPosReciboLayout read FPosCanhotoLayout write FPosCanhotoLayout;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property TextoResumoCanhoto: String read FTextoResumoCanhoto write FTextoResumoCanhoto;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura;
    property ExibeDadosISSQN: Boolean read FExibeDadosISSQN write FExibeDadosISSQN;
    property ExibeDadosDocReferenciados: Boolean read FExibeDadosDocReferenciados write FExibeDadosDocReferenciados;
    property DetVeiculos: TDetVeiculos read FDetVeiculos write FDetVeiculos;
    property DetMedicamentos: TDetMedicamentos read FDetMedicamentos write FDetMedicamentos;
    property DetArmamentos: TDetArmamentos read FDetArmamentos write FDetArmamentos;
    property DetCombustiveis: TDetCombustiveis read FDetCombustiveis write FDetCombustiveis;
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write FTributosPercentual;
    property TributosPercentualPersonalizado: Double read FTributosPercentualPersonalizado write FTributosPercentualPersonalizado;
    property MarcadAgua: String read FMarcadagua write FMarcadagua;
    property LarguraCodProd: Integer read FLarguraCodProd write FLarguraCodProd;
    property Fonte: TFonte read FFonte;
    property ExibeEAN: Boolean read FExibeEAN write FExibeEAN;
    property AltLinhaComun: Integer read FAltLinhaComun write FAltLinhaComun;
    property EspacoEntreProdutos: Integer read FEspacoEntreProdutos write FEspacoEntreProdutos;
    property AlternaCoresProdutos: Boolean read FAlternaCoresProdutos write FAlternaCoresProdutos;
    property CorDestaqueProdutos: TColor read FCorDestaqueProdutos write FCorDestaqueProdutos;
    property TamanhoLogoHeight: Integer read FTamanhoLogoHeight write FTamanhoLogoHeight;
    property TamanhoLogoWidth: Integer read FTamanhoLogoWidth write FTamanhoLogoWidth;
    property RecuoEndereco: Integer read FRecuoEndereco write FRecuoEndereco;
    property RecuoEmpresa: Integer read FRecuoEmpresa write FRecuoEmpresa;
    property LogoemCima: Boolean read FLogoEmCima write FLogoEmCima;
    property RecuoLogo: Integer read FRecuoLogo write FRecuoLogo;
    property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto;
    property ImprimeContDadosAdPrimeiraPagina: Boolean read FImprimeContDadosAdPrimeiraPagina write FImprimeContDadosAdPrimeiraPagina;
    property ExibeCampoDePagamento: TpcnInformacoesDePagamento read FExibeCampoDePagamento write FExibeCampoDePagamento;
    property ImprimeInscSuframa: Boolean read FImprimeInscSuframa write FImprimeInscSuframa;
    property ImprimeXPedNitemPed: Boolean read FImprimeXPedNitemPed write FImprimeXPedNitemPed;
    property ImprimeDescAcrescItemNFe: TpcnImprimeDescAcrescItem read FImprimeDescAcrescItemNFe write FImprimeDescAcrescItemNFe;

  end;

  { TDANFeNFCeConfig }
  TDANFeNFCeConfig = class
  private
    FTipoRelatorioBobina: TTipoRelatorioBobina;
    FTipoRelatorioEvento: TTipoRelatorioEvento;
    FEspacoFinal: Integer;
    FLarguraBobina: Integer;
    FImprimeDescAcrescItem: Boolean;
    FImprimeItens: Boolean;
    FViaConsumidor: Boolean;
    FvTroco: currency;
    FImprimeQRCodeLateral: Boolean;
    FImprimeLogoLateral: Boolean;
    FTamanhoLogoHeight: Integer;
    FTamanhoLogoWidth: Integer;
    FDescricaoPagamentos: TDescricaoPagamentos;
    FImprimeEmUmaLinha: Boolean;
    FImprimeEmDuasLinhas: Boolean;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FFonteLinhaItem: TFont;

    procedure setImprimeEmUmaLinha(const Value: Boolean);
    procedure setImprimeEmDuasLinhas(const Value: Boolean);

  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);
    procedure Apply(const DFeReport: TACBrNFeDANFCEClass);

  published
    property TipoRelatorioBobina: TTipoRelatorioBobina read FTipoRelatorioBobina write FTipoRelatorioBobina;
    property TipoRelatorioEvento: TTipoRelatorioEvento read FTipoRelatorioEvento write FTipoRelatorioEvento;
    property LarguraBobina: Integer read FLarguraBobina write FLarguraBobina;
    property ImprimeDescAcrescItem: Boolean read FImprimeDescAcrescItem write FImprimeDescAcrescItem;
    property ImprimeItens: Boolean read FImprimeItens write FImprimeItens;
    property ImprimeQRCodeLateral: Boolean read FImprimeQRCodeLateral write FImprimeQRCodeLateral;
    property ImprimeLogoLateral: Boolean read FImprimeLogoLateral write FImprimeLogoLateral;
    property EspacoFinal: Integer read FEspacoFinal write FEspacoFinal;
    property vTroco: currency read FvTroco write FvTroco;
    property ViaConsumidor: Boolean read FViaConsumidor write FViaConsumidor;
    property TamanhoLogoHeight: Integer read FTamanhoLogoHeight write FTamanhoLogoHeight;
    property TamanhoLogoWidth: Integer read FTamanhoLogoWidth write FTamanhoLogoWidth;
    property DescricaoPagamentos: TDescricaoPagamentos read FDescricaoPagamentos write FDescricaoPagamentos;
    property ImprimeEmUmaLinha: Boolean read FImprimeEmUmaLinha write setImprimeEmUmaLinha;
    property ImprimeEmDuasLinhas: Boolean read FImprimeEmDuasLinhas write setImprimeEmDuasLinhas;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property FonteLinhaItem: TFont read FFonteLinhaItem write FFonteLinhaItem;

  end;

  { TDANFeReportConfig }
  TDANFeReportConfig = class(TDFeReportConfig<TACBrDFeDANFeReport>)
  private
    FProtocolo: String;
    FCancelada: Boolean;
    FvTribFed: currency;
    FvTribEst: currency;
    FvTribMun: currency;
    FFonteTributos: String;
    FChaveTributos: String;
    FTipoDANFE: TpcnTipoImpressao;
    FImprimeTotalLiquido: Boolean;
    FImprimeCodigoEan: Boolean;
    FImprimeTributos: TpcnTributos;
    FQuebraLinhaEmDetalhamentos: Boolean;
    FExibeTotalTributosItem: Boolean;
    FExibeInforAdicProduto: TinfAdcProd;
    FImprimeNomeFantasia: Boolean;
    FNFeConfig: TDANFeNFeConfig;
    FNFCeConfig: TDANFeNFCeConfig;

  protected
    procedure LerIniChild(const AIni: TCustomIniFile); override;
    procedure GravarIniChild(const AIni: TCustomIniFile); override;
    procedure ApplyChild(const DFeReport: TACBrDFeDANFeReport; const Lib: TACBrLib); override;
    procedure DefinirValoresPadroesChild; override;

  public
    constructor Create;
    destructor Destroy; override;

    property Protocolo: String read FProtocolo write FProtocolo;
    property Cancelada: Boolean read FCancelada write FCancelada;
    property vTribFed: currency read FvTribFed write FvTribFed;
    property vTribEst: currency read FvTribEst write FvTribEst;
    property vTribMun: currency read FvTribMun write FvTribMun;
    property FonteTributos: String read FFonteTributos write FFonteTributos;
    property ChaveTributos: String read FChaveTributos write FChaveTributos;
    property TipoDANFE: TpcnTipoImpressao read FTipoDANFE write FTipoDANFE;
    property QuebraLinhaEmDetalhamentos: Boolean read FQuebraLinhaEmDetalhamentos write FQuebraLinhaEmDetalhamentos;
    property ImprimeTotalLiquido: Boolean read FImprimeTotalLiquido write FImprimeTotalLiquido;
    property ImprimeTributos: TpcnTributos read FImprimeTributos write FImprimeTributos;
    property ExibeTotalTributosItem: Boolean read FExibeTotalTributosItem write FExibeTotalTributosItem;
    property ExibeInforAdicProduto: TinfAdcProd read FExibeInforAdicProduto write FExibeInforAdicProduto;
    property ImprimeCodigoEan: Boolean read FImprimeCodigoEan write FImprimeCodigoEan;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property NFe: TDANFeNFeConfig read FNFeConfig;
    property NFCe: TDANFeNFCeConfig read FNFCeConfig;

  end;

  { TLibNFeConfig }
  TLibNFeConfig = class(TLibConfig)
  private
    FDANFeConfig: TDANFeReportConfig;
    FNFeConfig: TConfiguracoesNFe;

  protected
    procedure Travar; override;
    procedure Destravar; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property NFe: TConfiguracoesNFe read FNFeConfig;
    property DANFe: TDANFeReportConfig read FDANFeConfig;

  end;

implementation

uses
  typinfo, strutils, synacode, blcksock, pcnAuxiliar,
  ACBrLibNFeBase, ACBrLibNFeConsts, ACBrLibConsts,
  ACBrDANFCeFortesFr, ACBrNFeDANFeESCPOS, ACBrDFeConfiguracoes,
  ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TDANFeNFeConfig }

constructor TDANFeNFeConfig.Create;
begin
  DefinirValoresPadroes;
end;

destructor TDANFeNFeConfig.Destroy;
begin
  if Assigned(FFonte) then FFonte.Destroy;

  inherited Destroy;
end;

procedure TDANFeNFeConfig.DefinirValoresPadroes;
begin
  FFormularioContinuo := False;
  FImprimeValor := iuComercial;
  FImprimeDetalhamentoEspecifico := True;
  FPosCanhoto := prCabecalho;
  FPosCanhotoLayout := prlPadrao;
  FExibeResumoCanhoto := True;
  FTextoResumoCanhoto := '';
  FImprimeDescPorPercentual := False;
  FExibeCampoFatura := True;
  FExibeDadosISSQN := False;
  FExibeDadosDocReferenciados := True;
  FDetVeiculos := [dv_chassi, dv_xCor, dv_nSerie, dv_tpComb, dv_nMotor, dv_anoMod, dv_anoFab];
  FDetMedicamentos := [dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC];
  FDetArmamentos := [da_tpArma, da_nSerie, da_nCano, da_descr];
  FDetCombustiveis := [dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE, dc_qBCProd, dc_vAliqProd, dc_vCIDE];
  FTributosPercentual := ptValorProdutos;
  FTributosPercentualPersonalizado := 0;
  FLarguraCodProd := 54;
  FExibeEAN := False;
  FAltLinhaComun := 30;
  FEspacoEntreProdutos := 7;
  FAlternaCoresProdutos := False;
  FCorDestaqueProdutos := clWhite;
  FTamanhoLogoHeight := 0;
  FTamanhoLogoWidth := 0;
  FRecuoEndereco := 0;
  FRecuoEmpresa := 0;
  FLogoEmCima := False;
  FRecuoLogo := 0;
  FExpandirDadosAdicionaisAuto := False;
  FImprimeContDadosAdPrimeiraPagina := False;
  FExibeCampoDePagamento := eipNunca;
  FImprimeInscSuframa:= True;
  FImprimeXPedNitemPed:= False;
  FImprimeDescAcrescItemNFe:= idaiSempre;

  if Assigned(FFonte) then FFonte.Free;
  FFonte := TFonte.Create(nil);

end;

procedure TDANFeNFeConfig.LerIni(const AIni: TCustomIniFile);
begin
  FormularioContinuo := AIni.ReadBool(CSessaoDANFENFE, CChaveFormularioContinuo, FormularioContinuo);
  ImprimeValor := TImprimirUnidQtdeValor(AIni.ReadInteger(CSessaoDANFENFE, CChaveImprimeValor, Integer(ImprimeValor)));
  ImprimeDescPorPercentual := AIni.ReadBool(CSessaoDANFENFE, CChaveImprimeDescPorPercentual, ImprimeDescPorPercentual);
  ImprimeDetalhamentoEspecifico := AIni.ReadBool(CSessaoDANFENFE, CChaveImprimeDetalhamentoEspecifico, ImprimeDetalhamentoEspecifico);
  PosCanhoto := TPosRecibo(AIni.ReadInteger(CSessaoDANFENFE, CChavePosCanhoto, Integer(PosCanhoto)));
  PosCanhotoLayout := TPosReciboLayout(AIni.ReadInteger(CSessaoDANFENFE, CChavePosCanhoto, Integer(PosCanhotoLayout)));
  ExibeResumoCanhoto := AIni.ReadBool(CSessaoDANFENFE, CChaveExibeResumoCanhoto, ExibeResumoCanhoto);
  TextoResumoCanhoto := AIni.ReadString(CSessaoDANFENFE, CChaveTextoResumoCanhoto, TextoResumoCanhoto);
  ExibeCampoFatura := AIni.ReadBool(CSessaoDANFENFE, CChaveExibeCampoFatura, ExibeCampoFatura);
  ExibeDadosISSQN := AIni.ReadBool(CSessaoDANFENFE, CChaveExibeDadosISSQN, ExibeDadosISSQN);
  ExibeDadosDocReferenciados := AIni.ReadBool(CSessaoDANFENFE, CChaveExibeDadosDocReferenciados, ExibeDadosDocReferenciados);

  // Usando RTTI para trabalhar com Sets
  SetSetProp(self, 'DetVeiculos', AIni.ReadString(CSessaoDANFENFE, CChaveDetVeiculos, GetSetProp(self, 'DetVeiculos', True)));
  SetSetProp(self, 'DetMedicamentos', AIni.ReadString(CSessaoDANFENFE, CChaveDetMedicamentos, GetSetProp(self, 'DetMedicamentos', True)));
  SetSetProp(self, 'DetArmamentos', AIni.ReadString(CSessaoDANFENFE, CChaveDetArmamentos, GetSetProp(self, 'DetArmamentos', True)));
  SetSetProp(self, 'DetCombustiveis', AIni.ReadString(CSessaoDANFENFE, CChaveDetCombustiveis, GetSetProp(self, 'DetCombustiveis', True)));

  TributosPercentual := TpcnPercentualTributos(AIni.ReadInteger(CSessaoDANFENFE, CChaveTributosPercentual, Integer(TributosPercentual)));
  TributosPercentualPersonalizado := AIni.ReadFloat(CSessaoDANFENFE, CChaveTributosPercentualPersonalizado, TributosPercentualPersonalizado);
  MarcadAgua := AIni.ReadString(CSessaoDANFENFE, CChaveMarcadAgua, MarcadAgua);
  LarguraCodProd := AIni.ReadInteger(CSessaoDANFENFE, CChaveLarguraCodProd, LarguraCodProd);
  ExibeEAN := AIni.ReadBool(CSessaoDANFENFE, CChaveExibeEAN, ExibeEAN);
  AltLinhaComun := AIni.ReadInteger(CSessaoDANFENFE, CChaveAltLinhaComun, AltLinhaComun);
  EspacoEntreProdutos := AIni.ReadInteger(CSessaoDANFENFE, CChaveEspacoEntreProdutos, EspacoEntreProdutos);
  AlternaCoresProdutos := AIni.ReadBool(CSessaoDANFENFE, CChaveAlternaCoresProdutos, AlternaCoresProdutos);
  CorDestaqueProdutos := StringToColorDef(AIni.ReadString(CSessaoDANFENFE, CChaveCorDestaqueProdutos, ''), CorDestaqueProdutos);
  TamanhoLogoHeight := AIni.ReadInteger(CSessaoDANFENFE, CChaveTamanhoLogoHeight, TamanhoLogoHeight);
  TamanhoLogoWidth := AIni.ReadInteger(CSessaoDANFENFE, CChaveTamanhoLogoWidth, TamanhoLogoWidth);
  RecuoEndereco := AIni.ReadInteger(CSessaoDANFENFE, CChaveRecuoEndereco, RecuoEndereco);
  RecuoEmpresa := AIni.ReadInteger(CSessaoDANFENFE, CChaveRecuoEmpresa, RecuoEmpresa);
  LogoemCima := AIni.ReadBool(CSessaoDANFENFE, CChaveLogoemCima, LogoemCima);
  RecuoLogo := AIni.ReadInteger(CSessaoDANFENFE, CChaveRecuoLogo, RecuoLogo);
  ExpandirDadosAdicionaisAuto := AIni.ReadBool(CSessaoDANFENFE, CChaveExpandirDadosAdicionaisAuto, ExpandirDadosAdicionaisAuto);
  ImprimeContDadosAdPrimeiraPagina := AIni.ReadBool(CSessaoDANFENFE, CChaveImprimeContDadosAdPrimeiraPagina, ImprimeContDadosAdPrimeiraPagina);
  ExibeCampoDePagamento := TpcnInformacoesDePagamento(AIni.ReadInteger(CSessaoDANFENFE, CChaveExibeCampoDePagamento, Integer(ExibeCampoDePagamento)));
  ImprimeInscSuframa:= AIni.ReadBool(CSessaoDANFENFE, CChaveImprimeInscSuframa, ImprimeInscSuframa);
  ImprimeXPedNitemPed:= AIni.ReadBool(CSessaoDANFENFE, CChaveImprimeXPedNitemPed, ImprimeXPedNitemPed);
  ImprimeDescAcrescItemNFe:= TpcnImprimeDescAcrescItem(AIni.ReadInteger(CSessaoDANFENFE, CChaveImprimeDescAcrescItemNFe, Integer(ImprimeDescAcrescItemNFe)));

  with Fonte do
  begin
    Nome := TNomeFonte(AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteNome, Integer(Nome)));
    Negrito := AIni.ReadBool(CSessaoDANFENFE, CChaveFonteNegrito, Negrito);
    TamanhoFonteRazaoSocial := AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteRazaoSocial, TamanhoFonteRazaoSocial);
    TamanhoFonteEndereco := AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteEndereco, TamanhoFonteEndereco);
    TamanhoFonteInformacoesComplementares := AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteInformacoesComplementares, TamanhoFonteInformacoesComplementares);
    TamanhoFonteDemaisCampos := AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteDemaisCampos, TamanhoFonteDemaisCampos);
  end;
end;

procedure TDANFeNFeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteBool(CSessaoDANFENFE, CChaveFormularioContinuo, FormularioContinuo);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveImprimeValor, Integer(ImprimeValor));
  AIni.WriteBool(CSessaoDANFENFE, CChaveImprimeDescPorPercentual, ImprimeDescPorPercentual);
  AIni.WriteBool(CSessaoDANFENFE, CChaveImprimeDetalhamentoEspecifico, ImprimeDetalhamentoEspecifico);
  AIni.WriteInteger(CSessaoDANFENFE, CChavePosCanhoto, Integer(PosCanhoto));
  AIni.WriteInteger(CSessaoDANFENFE, CChavePosCanhotoLayout, Integer(PosCanhotoLayout));
  AIni.WriteBool(CSessaoDANFENFE, CChaveExibeResumoCanhoto, ExibeResumoCanhoto);
  AIni.WriteString(CSessaoDANFENFE, CChaveTextoResumoCanhoto, TextoResumoCanhoto);
  AIni.WriteBool(CSessaoDANFENFE, CChaveExibeCampoFatura, ExibeCampoFatura);
  AIni.WriteBool(CSessaoDANFENFE, CChaveExibeDadosISSQN, ExibeDadosISSQN);
  AIni.WriteBool(CSessaoDANFENFE, CChaveExibeDadosDocReferenciados, ExibeDadosDocReferenciados);

  // Usando RTTI para trabalhar com Sets
  AIni.WriteString(CSessaoDANFENFE, CChaveDetVeiculos, GetSetProp(self, 'DetVeiculos', True));
  AIni.WriteString(CSessaoDANFENFE, CChaveDetMedicamentos, GetSetProp(self, 'DetMedicamentos', True));
  AIni.WriteString(CSessaoDANFENFE, CChaveDetArmamentos, GetSetProp(self, 'DetArmamentos', True));
  AIni.WriteString(CSessaoDANFENFE, CChaveDetCombustiveis, GetSetProp(self, 'DetCombustiveis', True));

  AIni.WriteInteger(CSessaoDANFENFE, CChaveTributosPercentual, Integer(TributosPercentual));
  AIni.WriteFloat(CSessaoDANFENFE, CChaveTributosPercentualPersonalizado, TributosPercentualPersonalizado);
  AIni.WriteString(CSessaoDANFENFE, CChaveMarcadAgua, MarcadAgua);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveLarguraCodProd, LarguraCodProd);
  AIni.WriteBool(CSessaoDANFENFE, CChaveExibeEAN, ExibeEAN);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveAltLinhaComun, AltLinhaComun);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveEspacoEntreProdutos, EspacoEntreProdutos);
  AIni.WriteBool(CSessaoDANFENFE, CChaveAlternaCoresProdutos, AlternaCoresProdutos);
  AIni.WriteString(CSessaoDANFENFE, CChaveCorDestaqueProdutos, ColorToString(CorDestaqueProdutos));
  AIni.WriteInteger(CSessaoDANFENFE, CChaveTamanhoLogoHeight, TamanhoLogoHeight);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveTamanhoLogoWidth, TamanhoLogoWidth);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveRecuoEndereco, RecuoEndereco);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveRecuoEmpresa, RecuoEmpresa);
  AIni.WriteBool(CSessaoDANFENFE, CChaveLogoemCima, LogoemCima);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveRecuoLogo, RecuoLogo);
  AIni.WriteBool(CSessaoDANFENFE, CChaveExpandirDadosAdicionaisAuto, ExpandirDadosAdicionaisAuto);
  AIni.WriteBool(CSessaoDANFENFE, CChaveImprimeContDadosAdPrimeiraPagina, ImprimeContDadosAdPrimeiraPagina);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveExibeCampoDePagamento, Integer(ExibeCampoDePagamento));
  AIni.WriteBool(CSessaoDANFENFE, CChaveImprimeInscSuframa, ImprimeInscSuframa);
  AIni.WriteBool(CSessaoDANFENFE, CChaveImprimeXPedNitemPed, ImprimeXPedNitemPed);
  AIni.WriteInteger(CSessaoDANFENFE, CChaveImprimeDescAcrescItemNFe, Integer(ImprimeDescAcrescItemNFe));

  with Fonte do
  begin
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteNome, Integer(Nome));
    AIni.WriteBool(CSessaoDANFENFE, CChaveFonteNegrito, Negrito);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteRazaoSocial, TamanhoFonteRazaoSocial);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteEndereco, TamanhoFonteEndereco);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteInformacoesComplementares, TamanhoFonteInformacoesComplementares);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteDemaisCampos, TamanhoFonteDemaisCampos);
  end;
end;

procedure TDANFeNFeConfig.Apply(const DFeReport: TACBrNFeDANFeRL);
begin

  if not Assigned(DFeReport) then Exit;

  with DFeReport do
  begin
    FormularioContinuo := FFormularioContinuo;
    ImprimeValor := FImprimeValor;
    ImprimeDescPorPercentual := FImprimeDescPorPercentual;
    ImprimeDetalhamentoEspecifico := FImprimeDetalhamentoEspecifico;
    PosCanhoto := FPosCanhoto;
    PosCanhotoLayout := FPosCanhotoLayout;
    ExibeResumoCanhoto := FExibeResumoCanhoto;
    TextoResumoCanhoto := FTextoResumoCanhoto;
    ExibeCampoFatura := FExibeCampoFatura;
    ExibeDadosISSQN := FExibeDadosISSQN;
    ExibeDadosDocReferenciados := FExibeDadosDocReferenciados;
    DetVeiculos := FDetVeiculos;
    DetMedicamentos := FDetMedicamentos;
    DetArmamentos := FDetArmamentos;
    DetCombustiveis := FDetCombustiveis;
    TributosPercentual := FTributosPercentual;
    TributosPercentualPersonalizado := FTributosPercentualPersonalizado;
    MarcadAgua := FMarcadAgua;
    LarguraCodProd := FLarguraCodProd;
    ExibeEAN := FExibeEAN;
    AltLinhaComun := FAltLinhaComun;
    EspacoEntreProdutos := FEspacoEntreProdutos;
    AlternaCoresProdutos := FAlternaCoresProdutos;
    CorDestaqueProdutos := FCorDestaqueProdutos;
    TamanhoLogoHeight := FTamanhoLogoHeight;
    TamanhoLogoWidth := FTamanhoLogoWidth;
    RecuoEndereco := FRecuoEndereco;
    RecuoEmpresa := FRecuoEmpresa;
    LogoemCima := FLogoemCima;
    RecuoLogo := FRecuoLogo;
    ExpandirDadosAdicionaisAuto := FExpandirDadosAdicionaisAuto;
    ImprimeContinuacaoDadosAdicionaisPrimeiraPagina := FImprimeContDadosAdPrimeiraPagina;
    ExibeCampoDePagamento := FExibeCampoDePagamento;
    ImprimeInscSuframa:= FImprimeInscSuframa;
    ImprimeXPedNItemPed:= FImprimeXPedNitemPed;
    ImprimeDescAcrescItemNFe:= FImprimeDescAcrescItemNFe;

    with Fonte do
    begin
      Nome := FFonte.Nome;
      Negrito := FFonte.Negrito;
      TamanhoFonteRazaoSocial := FFonte.TamanhoFonteRazaoSocial;
      TamanhoFonteEndereco := FFonte.TamanhoFonteEndereco;
      TamanhoFonteInformacoesComplementares := FFonte.TamanhoFonteInformacoesComplementares;
      TamanhoFonteDemaisCampos := FFonte.TamanhoFonteDemaisCampos;
    end;
  end;
end;

{ TDANFeNFCeConfig }

constructor TDANFeNFCeConfig.Create;
begin
  DefinirValoresPadroes;
end;

procedure TDANFeNFCeConfig.setImprimeEmDuasLinhas(const Value: Boolean);
begin
  if Value = FImprimeEmDuasLinhas then Exit;

  FImprimeEmDuasLinhas := Value;
  if Value then
  begin
    FImprimeEmUmaLinha := False;
  end;
end;

procedure TDANFeNFCeConfig.setImprimeEmUmaLinha(const Value: Boolean);
begin
  if Value = FImprimeEmUmaLinha then Exit;

  FImprimeEmUmaLinha := Value;
  if Value then
  begin
    FImprimeEmDuasLinhas := False;
  end;
end;

procedure TDANFeNFCeConfig.DefinirValoresPadroes;
begin
  FTipoRelatorioBobina := tpFortes;
  FTipoRelatorioEvento := evA4;
  FLarguraBobina := 302;
  FImprimeDescAcrescItem := True;
  FImprimeItens := True;
  FViaConsumidor := False;
  FvTroco := 0;
  FImprimeQRCodeLateral := False;
  FImprimeLogoLateral := False;
  FEspacoFinal := 38;
  FTamanhoLogoHeight := 50;
  FTamanhoLogoWidth := 77;
  FDescricaoPagamentos := [icaTipo, icaBandeira];
  FImprimeEmUmaLinha := False;
  FImprimeEmDuasLinhas := False;
  FMargemInferior := 0;
  FMargemSuperior := 0;
  FMargemEsquerda := 0;
  FMargemDireita := 0;
  FFonteLinhaItem := TFont.Create;
  FFonteLinhaItem.Name := 'Lucida Console';
  FFonteLinhaItem.Size := 7;
end;

procedure TDANFeNFCeConfig.LerIni(const AIni: TCustomIniFile);
begin
  TipoRelatorioBobina := TTipoRelatorioBobina(AIni.ReadInteger(CSessaoDANFENFCE, CChaveTipoRelatorioBobina, Integer(TipoRelatorioBobina)));
  TipoRelatorioEvento := TTipoRelatorioEvento(AIni.ReadInteger(CSessaoDANFENFCE, CChaveTipoRelatorioEvento, Integer(TipoRelatorioEvento)));
  LarguraBobina := AIni.ReadInteger(CSessaoDANFENFCE, CChaveLarguraBobina, LarguraBobina);
  ImprimeDescAcrescItem := AIni.ReadBool(CSessaoDANFENFCE, CChaveImprimeDescAcrescItem, ImprimeDescAcrescItem);
  ImprimeItens := AIni.ReadBool(CSessaoDANFENFCE, CChaveImprimeItens, ImprimeItens);
  ViaConsumidor := AIni.ReadBool(CSessaoDANFENFCE, CChaveViaConsumidor, ViaConsumidor);
  vTroco := AIni.ReadFloat(CSessaoDANFENFCE, CChavevTroco, vTroco);
  ImprimeQRCodeLateral := AIni.ReadBool(CSessaoDANFENFCE, CChaveImprimeQRCodeLateral, ImprimeQRCodeLateral);
  ImprimeLogoLateral := AIni.ReadBool(CSessaoDANFENFCE, CChaveImprimeLogoLateral, ImprimeLogoLateral);
  EspacoFinal := AIni.ReadInteger(CSessaoDANFENFCE, CChaveEspacoFinal, EspacoFinal);
  TamanhoLogoHeight := AIni.ReadInteger(CSessaoDANFENFCE, CChaveTamanhoLogoHeight, TamanhoLogoHeight);
  TamanhoLogoWidth := AIni.ReadInteger(CSessaoDANFENFCE, CChaveTamanhoLogoWidth, TamanhoLogoWidth);
  SetSetProp(Self, 'DescricaoPagamentos', AIni.ReadString(CSessaoDANFENFCE, CChaveDescricaoPagamentos, GetSetProp(Self, 'DescricaoPagamentos', True)));
  ImprimeEmUmaLinha := AIni.ReadBool(CSessaoDANFENFCE, CChaveImprimeEmUmaLinha, ImprimeEmUmaLinha);
  ImprimeEmDuasLinhas := AIni.ReadBool(CSessaoDANFENFCE, CChaveImprimeEmDuasLinhas, ImprimeEmDuasLinhas);
  MargemInferior := AIni.ReadFloat(CSessaoDANFENFCE, CChaveMargemInferior, MargemInferior);
  MargemSuperior := AIni.ReadFloat(CSessaoDANFENFCE, CChaveMargemSuperior, MargemSuperior);
  MargemEsquerda := AIni.ReadFloat(CSessaoDANFENFCE, CChaveMargemEsquerda, MargemEsquerda);
  MargemDireita := AIni.ReadFloat(CSessaoDANFENFCE, CChaveMargemDireita, MargemDireita);
  FonteLinhaItem.Name    :=  AIni.ReadString( CSessaoDANFENFCE, CChaveFonteLinhaItemName, FonteLinhaItem.Name );
  FonteLinhaItem.Color   :=  TColor(AIni.ReadInteger( CSessaoDANFENFCE, CChaveFonteLinhaItemColor, FonteLinhaItem.Color ));
  FonteLinhaItem.Size    :=  AIni.ReadInteger( CSessaoDANFENFCE, CChaveFonteLinhaItemSize, FonteLinhaItem.Size );
  FonteLinhaItem.Style := [];
  if AIni.ReadBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemBold , False ) then
    FonteLinhaItem.Style := FonteLinhaItem.Style + [fsBold];
  if AIni.ReadBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemItalic , False ) then
    FonteLinhaItem.Style := FonteLinhaItem.Style + [fsItalic];
  if AIni.ReadBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemUnderline , False ) then
    FonteLinhaItem.Style := FonteLinhaItem.Style + [fsUnderline];
  if AIni.ReadBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemStrikeOut , False ) then
    FonteLinhaItem.Style := FonteLinhaItem.Style + [fsStrikeOut];
end;

procedure TDANFeNFCeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoDANFENFCE, CChaveTipoRelatorioBobina, Integer(TipoRelatorioBobina));
  AIni.WriteInteger(CSessaoDANFENFCE, CChaveTipoRelatorioEvento, Integer(TipoRelatorioEvento));
  AIni.WriteInteger(CSessaoDANFENFCE, CChaveLarguraBobina, LarguraBobina);
  AIni.WriteBool(CSessaoDANFENFCE, CChaveImprimeDescAcrescItem, ImprimeDescAcrescItem);
  AIni.WriteBool(CSessaoDANFENFCE, CChaveImprimeItens, ImprimeItens);
  AIni.WriteBool(CSessaoDANFENFCE, CChaveViaConsumidor, ViaConsumidor);
  AIni.WriteFloat(CSessaoDANFENFCE, CChavevTroco, vTroco);
  AIni.WriteBool(CSessaoDANFENFCE, CChaveImprimeQRCodeLateral, ImprimeQRCodeLateral);
  AIni.WriteBool(CSessaoDANFENFCE, CChaveImprimeLogoLateral, ImprimeLogoLateral);
  AIni.WriteInteger(CSessaoDANFENFCE, CChaveEspacoFinal, EspacoFinal);
  AIni.WriteInteger(CSessaoDANFENFCE, CChaveTamanhoLogoHeight, TamanhoLogoHeight);
  AIni.WriteInteger(CSessaoDANFENFCE, CChaveTamanhoLogoWidth, TamanhoLogoWidth);
  AIni.WriteString(CSessaoDANFENFCE, CChaveDescricaoPagamentos, GetSetProp(self, 'DescricaoPagamentos', True));
  AIni.WriteBool(CSessaoDANFENFCE, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  AIni.WriteBool(CSessaoDANFENFCE, CChaveImprimeEmDuasLinhas, FImprimeEmDuasLinhas);
  AIni.WriteFloat(CSessaoDANFENFCE, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDANFENFCE, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDANFENFCE, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDANFENFCE, CChaveMargemDireita, FMargemDireita);
  AIni.WriteString( CSessaoDANFENFCE,   CChaveFonteLinhaItemName   , FonteLinhaItem.Name );
  AIni.WriteInteger( CSessaoDANFENFCE,  CChaveFonteLinhaItemColor , FonteLinhaItem.Color );
  AIni.WriteInteger( CSessaoDANFENFCE,  CChaveFonteLinhaItemSize  , FonteLinhaItem.Size );
  AIni.WriteBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemBold , fsBold in FonteLinhaItem.Style );
  AIni.WriteBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemItalic , fsItalic in FonteLinhaItem.Style );
  AIni.WriteBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemUnderline , fsUnderline in FonteLinhaItem.Style );
  AIni.WriteBool( CSessaoDANFENFCE,   CChaveFonteLinhaItemStrikeOut , fsStrikeOut in FonteLinhaItem.Style );
end;

procedure TDANFeNFCeConfig.Apply(const DFeReport: TACBrNFeDANFCEClass);
Var
  DANFeFortes: TACBrNFeDANFCeFortes;
begin
  if not Assigned(DFeReport) then Exit;

  with DFeReport do
  begin
    LarguraBobina := FLarguraBobina;
    ImprimeDescAcrescItem := FImprimeDescAcrescItem;
    ImprimeItens := FImprimeItens;
    ViaConsumidor := FViaConsumidor;
    vTroco := FvTroco;
    ImprimeQRCodeLateral := FImprimeQRCodeLateral;
    ImprimeLogoLateral := FImprimeLogoLateral;
    EspacoFinal := FEspacoFinal;

    DescricaoPagamentos := FDescricaoPagamentos;
    ImprimeEmUmaLinha := FImprimeEmUmaLinha;
    ImprimeEmDuasLinhas := FImprimeEmDuasLinhas;
    MargemInferior := FMargemInferior;
    MargemSuperior := FMargemSuperior;
    MargemEsquerda := FMargemEsquerda;
    MargemDireita := FMargemDireita;
  end;

  if DFeReport is TACBrNFeDANFCeFortes then
  begin
    DANFeFortes := TACBrNFeDANFCeFortes(DFeReport);
    DANFeFortes.TamanhoLogoHeight := FTamanhoLogoHeight;
    DANFeFortes.TamanhoLogoWidth := FTamanhoLogoWidth;
    DANFeFortes.FonteLinhaItem.Name := FFonteLinhaItem.Name;
    DANFeFortes.FonteLinhaItem.Color := FFonteLinhaItem.Color;
    DANFeFortes.FonteLinhaItem.Size := FFonteLinhaItem.Size;
    DANFeFortes.FonteLinhaItem.Style := FFonteLinhaItem.Style;
    DANFeFortes.FormularioContinuo := True;
  end;
end;

{ TDANFeReportConfig }

constructor TDANFeReportConfig.Create;
begin
  inherited Create(CSessaoDANFE);
end;

destructor TDANFeReportConfig.Destroy;
begin
  FNFeConfig.Destroy;
  FNFCeConfig.Destroy;

  inherited Destroy;
end;

procedure TDANFeReportConfig.DefinirValoresPadroesChild;
begin
  FProtocolo := '';
  FCancelada := False;
  FvTribFed := 0.0;
  FvTribEst := 0.0;
  FvTribMun := 0.0;
  FFonteTributos := '';
  FChaveTributos := '';
  FTipoDANFE := tiRetrato;
  FImprimeTotalLiquido := True;
  FImprimeTributos := trbNormal;
  FExibeTotalTributosItem := False;
  FImprimeCodigoEan := False;
  FImprimeNomeFantasia := False;
  FExibeInforAdicProduto := infDescricao;
  FQuebraLinhaEmDetalhamentos := True;

  if not Assigned(FNFeConfig) then
    FNFeConfig := TDANFeNFeConfig.Create
  else
    FNFeConfig.DefinirValoresPadroes;

  if not Assigned(FNFCeConfig) then
    FNFCeConfig := TDANFeNFCeConfig.Create
  else
    FNFCeConfig.DefinirValoresPadroes;

end;

procedure TDANFeReportConfig.LerIniChild(const AIni: TCustomIniFile);
begin
  FProtocolo := AIni.ReadString(CSessaoDANFE, CChaveProtocolo, FProtocolo);
  FCancelada := AIni.ReadBool(CSessaoDANFE, CChaveCancelada, FCancelada);
  FTipoDANFE := TpcnTipoImpressao(AIni.ReadInteger(CSessaoDANFE, CChaveTipoDANFE, Integer(FTipoDANFE)));
  FImprimeTotalLiquido := AIni.ReadBool(CSessaoDANFE, CChaveImprimeTotalLiquido, FImprimeTotalLiquido);
  FvTribFed := AIni.ReadFloat(CSessaoDANFE, CChavevTribFed, FvTribFed);
  FvTribEst := AIni.ReadFloat(CSessaoDANFE, CChavevTribEst, FvTribEst);
  FvTribMun := AIni.ReadFloat(CSessaoDANFE, CChavevTribMun, FvTribMun);
  FFonteTributos := AIni.ReadString(CSessaoDANFE, CChaveFonteTributos, FFonteTributos);
  FChaveTributos := AIni.ReadString(CSessaoDANFE, CChaveChaveTributos, FChaveTributos);
  FImprimeTributos := TpcnTributos(AIni.ReadInteger(CSessaoDANFE, CChaveImprimeTributos, Integer(FImprimeTributos)));
  FExibeTotalTributosItem := AIni.ReadBool(CSessaoDANFE, CChaveExibeTotalTributosItem, FExibeTotalTributosItem);
  FImprimeCodigoEan := AIni.ReadBool(CSessaoDANFE, CChaveImprimeCodigoEan, FImprimeCodigoEan);
  FImprimeNomeFantasia := AIni.ReadBool(CSessaoDANFE, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  FExibeInforAdicProduto := TinfAdcProd(AIni.ReadInteger(CSessaoDANFE, CChaveExibeInforAdicProduto, Integer(FExibeInforAdicProduto)));
  FQuebraLinhaEmDetalhamentos := AIni.ReadBool(CSessaoDANFE, CChaveQuebraLinhaEmDetalhamentos, FQuebraLinhaEmDetalhamentos);

  FNFeConfig.LerIni(AIni);
  FNFCeConfig.LerIni(AIni);
end;

procedure TDANFeReportConfig.GravarIniChild(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDANFE, CChaveProtocolo, FProtocolo);
  AIni.WriteBool(CSessaoDANFE, CChaveCancelada, FCancelada);
  AIni.WriteInteger(CSessaoDANFE, CChaveTipoDANFE, Integer(FTipoDANFE));
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeTotalLiquido, FImprimeTotalLiquido);
  AIni.WriteFloat(CSessaoDANFE, CChavevTribFed, FvTribFed);
  AIni.WriteFloat(CSessaoDANFE, CChavevTribEst, FvTribEst);
  AIni.WriteFloat(CSessaoDANFE, CChavevTribMun, FvTribMun);
  AIni.WriteString(CSessaoDANFE, CChaveFonteTributos, FFonteTributos);
  AIni.WriteString(CSessaoDANFE, CChaveChaveTributos, FChaveTributos);
  AIni.WriteInteger(CSessaoDANFE, CChaveImprimeTributos, Integer(FImprimeTributos));
  AIni.WriteBool(CSessaoDANFE, CChaveExibeTotalTributosItem, FExibeTotalTributosItem);
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeCodigoEan, FImprimeCodigoEan);
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  AIni.WriteInteger(CSessaoDANFE, CChaveExibeInforAdicProduto, Integer(FExibeInforAdicProduto));
  AIni.WriteBool(CSessaoDANFE, CChaveQuebraLinhaEmDetalhamentos, FQuebraLinhaEmDetalhamentos);

  FNFeConfig.GravarIni(AIni);
  FNFCeConfig.GravarIni(AIni);
end;

procedure TDANFeReportConfig.ApplyChild(const DFeReport: TACBrDFeDANFeReport; const Lib: TACBrLib);
begin
  with DFeReport do
  begin
    Protocolo := FProtocolo;
    Cancelada := FCancelada;
    TipoDANFE := FTipoDANFE;
    ImprimeTotalLiquido := FImprimeTotalLiquido;
    vTribFed := FvTribFed;
    vTribEst := FvTribEst;
    vTribMun := FvTribMun;
    FonteTributos := FFonteTributos;
    ChaveTributos := FChaveTributos;
    ImprimeTributos := FImprimeTributos;
    ExibeTotalTributosItem := FExibeTotalTributosItem;
    ImprimeCodigoEan := FImprimeCodigoEan;
    ImprimeNomeFantasia := FImprimeNomeFantasia;
    ExibeInforAdicProduto := FExibeInforAdicProduto;
    QuebraLinhaEmDetalhamentos := FQuebraLinhaEmDetalhamentos;
  end;

  if DFeReport is TACBrNFeDANFeRL then
    NFe.Apply(TACBrNFeDANFeRL(DFeReport))
  else if DFeReport is TACBrNFeDANFCEClass then
    NFCe.Apply(TACBrNFeDANFCEClass(DFeReport));
end;

{ TLibNFeConfig }

constructor TLibNFeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FNFeConfig := TConfiguracoesNFe.Create(nil);
  FNFeConfig.ChaveCryptINI := AChaveCrypt;

  FDANFeConfig := TDANFeReportConfig.Create;
end;

destructor TLibNFeConfig.Destroy;
begin
  FNFeConfig.Free;
  FDANFeConfig.Free;

  inherited Destroy;
end;

procedure TLibNFeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FNFeConfig.ChaveCryptINI := ChaveCrypt;
  FNFeConfig.LerIni(Ini);
  FDANFeConfig.LerIni(Ini);
end;

procedure TLibNFeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FNFeConfig.ChaveCryptINI := ChaveCrypt;
  FNFeConfig.GravarIni(Ini);
  FDANFeConfig.GravarIni(Ini);
end;

procedure TLibNFeConfig.ClasseParaComponentes;
begin
  FNFeConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibNFe(Owner).NFeDM.AplicarConfiguracoes;
end;

procedure TLibNFeConfig.Travar;
begin
  if Assigned(Owner) then
    TACBrLibNFe(Owner).NFeDM.Travar;
end;

procedure TLibNFeConfig.Destravar;
begin
  if Assigned(Owner) then
    TACBrLibNFe(Owner).NFeDM.Destravar;
end;

end.

