{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeConfig;

interface

uses
  Classes, Graphics, SysUtils, IniFiles,
  pcnConversao,
  ACBrNFeConfiguracoes, ACBrDFeReport, ACBrDFeDANFeReport,
  ACBrNFeDANFEClass, ACBrNFeDANFeRLClass, ACBrLibConfig,
  ACBrDeviceConfig, ACBrIntegradorConfig, DFeReportConfig;

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
    FImprimeContinuacaoDadosAdicionaisPrimeiraPagina: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);
    procedure Assign(const DFeReport: TACBrNFeDANFeRL);

  published
    property FormularioContinuo: Boolean read FFormularioContinuo write FFormularioContinuo;
    property ImprimeValor: TImprimirUnidQtdeValor read FImprimeValor write FImprimeValor;
    property ImprimeDescPorPercentual: Boolean read FImprimeDescPorPercentual write FImprimeDescPorPercentual;
    property ImprimeDetalhamentoEspecifico: Boolean read FImprimeDetalhamentoEspecifico write FImprimeDetalhamentoEspecifico;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
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
    property ImprimeContinuacaoDadosAdicionaisPrimeiraPagina: Boolean read FImprimeContinuacaoDadosAdicionaisPrimeiraPagina write FImprimeContinuacaoDadosAdicionaisPrimeiraPagina;

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

  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);
    procedure Assign(const DFeReport: TACBrNFeDANFCEClass);

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

  end;

  { TDANFeReportConfig }
  TDANFeReportConfig = class(TDFeReportConfig<TACBrDFeDANFeReport>)
  private
    FTipoDANFE: TpcnTipoImpressao;
    FImprimeTotalLiquido: Boolean;
    FImprimeCodigoEan: Boolean;
    FvTribFed: currency;
    FvTribEst: currency;
    FvTribMun: currency;
    FFonteTributos: String;
    FChaveTributos: String;
    FImprimeTributos: TpcnTributos;
    FQuebraLinhaEmDetalhamentos: Boolean;
    FExibeTotalTributosItem: Boolean;
    FExibeInforAdicProduto: TinfAdcProd;
    FImprimeNomeFantasia: Boolean;
    FImprimeEmUmaLinha: Boolean;
    FNFeConfig: TDANFeNFeConfig;
    FNFCeConfig: TDANFeNFCeConfig;

  protected
    procedure LerIniChild(const AIni: TCustomIniFile); override;
    procedure GravarIniChild(const AIni: TCustomIniFile); override;
    procedure ApplyChild(const DFeReport: TACBrDFeDANFeReport); override;
    procedure DefinirValoresPadroesChild; override;

  public
    constructor Create;
    destructor Destroy; override;

    property TipoDANFE: TpcnTipoImpressao read FTipoDANFE write FTipoDANFE;
    property QuebraLinhaEmDetalhamentos: Boolean read FQuebraLinhaEmDetalhamentos write FQuebraLinhaEmDetalhamentos;
    property ImprimeTotalLiquido: Boolean read FImprimeTotalLiquido write FImprimeTotalLiquido;
    property ImprimeTributos: TpcnTributos read FImprimeTributos write FImprimeTributos;
    property ExibeTotalTributosItem: Boolean read FExibeTotalTributosItem write FExibeTotalTributosItem;
    property ExibeInforAdicProduto: TinfAdcProd read FExibeInforAdicProduto write FExibeInforAdicProduto;
    property ImprimeCodigoEan: Boolean read FImprimeCodigoEan write FImprimeCodigoEan;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property ImprimeEmUmaLinha: Boolean read FImprimeEmUmaLinha write FImprimeEmUmaLinha;
    property NFe: TDANFeNFeConfig read FNFeConfig;
    property NFCe: TDANFeNFCeConfig read FNFCeConfig;

  end;

  { TLibNFeConfig }
  TLibNFeConfig = class(TLibConfig)
  private
    FDANFeConfig: TDANFeReportConfig;
    FNFeConfig: TConfiguracoesNFe;
    FIntegradorConfig: TIntegradorConfig;
    FDeviceConfig: TDeviceConfig;

  protected
    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;
    procedure ImportarIni(FIni: TIniFile); override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    function AjustarValor(Tipo: TTipoFuncao; ASessao, AChave, AValor: Ansistring): Ansistring; override;

    property NFe: TConfiguracoesNFe read FNFeConfig;
    property DANFe: TDANFeReportConfig read FDANFeConfig;
    property Integrador: TIntegradorConfig read FIntegradorConfig;
    property PosDevice: TDeviceConfig read FDeviceConfig write FDeviceConfig;

  end;

implementation

uses
  typinfo, strutils, synacode,
  ACBrLibNFeClass, ACBrLibNFeConsts, ACBrLibConsts, ACBrLibComum,
  ACBrDANFCeFortesFr, ACBrNFeDANFeESCPOS,
  ACBrUtil, ACBrDFeConfiguracoes;

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
  FImprimeContinuacaoDadosAdicionaisPrimeiraPagina := False;

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
  ExpandirDadosAdicionaisAuto := AIni.ReadBool(CSessaoDANFENFE, CChaveExpandirDadosAdicionaisAuto, FExpandirDadosAdicionaisAuto);
  ImprimeContinuacaoDadosAdicionaisPrimeiraPagina := AIni.ReadBool(CSessaoDANFENFE, CChaveImprimeContinuacaoDadosAdicionaisPrimeiraPagina, FImprimeContinuacaoDadosAdicionaisPrimeiraPagina);

  with Fonte do
  begin
    Nome := TNomeFonte(AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteNome, Integer(Nome)));
    Negrito := AIni.ReadBool(CSessaoDANFENFE, CChaveFonteNegrito, Negrito);
    TamanhoFonteRazaoSocial := AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteRazaoSocial, TamanhoFonteRazaoSocial);
    TamanhoFonteEndereco := AIni.ReadInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteEndereco, TamanhoFonteEndereco);
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
  AIni.WriteBool(CSessaoDANFENFE, CChaveImprimeContinuacaoDadosAdicionaisPrimeiraPagina, ImprimeContinuacaoDadosAdicionaisPrimeiraPagina);


  with Fonte do
  begin
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteNome, Integer(Nome));
    AIni.WriteBool(CSessaoDANFENFE, CChaveFonteNegrito, Negrito);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteRazaoSocial, TamanhoFonteRazaoSocial);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteEndereco, TamanhoFonteEndereco);
    AIni.WriteInteger(CSessaoDANFENFE, CChaveFonteTamanhoFonteDemaisCampos, TamanhoFonteDemaisCampos);
  end;
end;

procedure TDANFeNFeConfig.Assign(const DFeReport: TACBrNFeDANFeRL);
begin

  if not Assigned(DFeReport) then Exit;

  with DFeReport do
  begin
    FormularioContinuo := FFormularioContinuo;
    ImprimeValor := FImprimeValor;
    ImprimeDescPorPercentual := FImprimeDescPorPercentual;
    ImprimeDetalhamentoEspecifico := FImprimeDetalhamentoEspecifico;
    PosCanhoto := FPosCanhoto;
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
    ImprimeContinuacaoDadosAdicionaisPrimeiraPagina := FImprimeContinuacaoDadosAdicionaisPrimeiraPagina;

    with Fonte do
    begin
      Nome := FFonte.Nome;
      Negrito := FFonte.Negrito;
      TamanhoFonteRazaoSocial := FFonte.TamanhoFonteRazaoSocial;
      TamanhoFonteEndereco := FFonte.TamanhoFonteEndereco;
      TamanhoFonteDemaisCampos := FFonte.TamanhoFonteDemaisCampos;
    end;
  end;
end;

{ TDANFeNFCeConfig }

constructor TDANFeNFCeConfig.Create;
begin
  DefinirValoresPadroes;
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
end;

procedure TDANFeNFCeConfig.Assign(const DFeReport: TACBrNFeDANFCEClass);
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
  end;

  if DFeReport is TACBrNFeDANFCeFortes then
  begin
    TACBrNFeDANFCeFortes(DFeReport).TamanhoLogoHeight := FTamanhoLogoHeight;
    TACBrNFeDANFCeFortes(DFeReport).TamanhoLogoWidth := FTamanhoLogoWidth;
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
  FTipoDANFE := tiRetrato;
  FImprimeTotalLiquido := True;
  FvTribFed := 0.0;
  FvTribEst := 0.0;
  FvTribMun := 0.0;
  FFonteTributos := '';
  FChaveTributos := '';
  FImprimeTributos := trbNormal;
  FExibeTotalTributosItem := False;
  FImprimeCodigoEan := False;
  FImprimeNomeFantasia := False;
  FImprimeEmUmaLinha := False;
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
  FImprimeNomeFantasia := AIni.ReadBool(CSessaoDANFE, CChaveImprimeCodigoEan, FImprimeNomeFantasia);
  FImprimeEmUmaLinha := AIni.ReadBool(CSessaoDANFE, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  FExibeInforAdicProduto := TinfAdcProd(AIni.ReadInteger(CSessaoDANFE, CChaveExibeInforAdicProduto, Integer(FExibeInforAdicProduto)));
  FQuebraLinhaEmDetalhamentos := AIni.ReadBool(CSessaoDANFE, CChaveQuebraLinhaEmDetalhamentos, FQuebraLinhaEmDetalhamentos);

  FNFeConfig.LerIni(AIni);
  FNFCeConfig.LerIni(AIni);

end;

procedure TDANFeReportConfig.GravarIniChild(const AIni: TCustomIniFile);
begin
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
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeCodigoEan, FImprimeNomeFantasia);
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  AIni.WriteInteger(CSessaoDANFE, CChaveExibeInforAdicProduto, Integer(FExibeInforAdicProduto));
  AIni.WriteBool(CSessaoDANFE, CChaveQuebraLinhaEmDetalhamentos, FQuebraLinhaEmDetalhamentos);

  FNFeConfig.GravarIni(AIni);
  FNFCeConfig.GravarIni(AIni);

end;

procedure TDANFeReportConfig.ApplyChild(const DFeReport: TACBrDFeDANFeReport);
var
  pLibConfig: TLibNFeConfig;
begin
  pLibConfig := TLibNFeConfig(pLib.Config);

  with DFeReport do
  begin
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
    ImprimeEmUmaLinha := FImprimeEmUmaLinha;
    ExibeInforAdicProduto := FExibeInforAdicProduto;
    QuebraLinhaEmDetalhamentos := FQuebraLinhaEmDetalhamentos;
  end;

  if DFeReport is TACBrNFeDANFeRL then
  begin
    pLibConfig.DANFe.NFe.Assign(TACBrNFeDANFeRL(DFeReport));
  end
  else if DFeReport is TACBrNFeDANFCEClass then
  begin
    pLibConfig.DANFe.NFCe.Assign(TACBrNFeDANFCEClass(DFeReport));
  end;
end;

{ TLibNFeConfig }

constructor TLibNFeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FNFeConfig := TConfiguracoesNFe.Create(nil);
  FNFeConfig.ChaveCryptINI := AChaveCrypt;

  FDANFeConfig := TDANFeReportConfig.Create;
  FIntegradorConfig := TIntegradorConfig.Create;
end;

destructor TLibNFeConfig.Destroy;
begin
  FNFeConfig.Free;
  FDANFeConfig.Free;
  FIntegradorConfig.Free;
  if FDeviceConfig <> nil then FDeviceConfig.Free;

  inherited Destroy;
end;

procedure TLibNFeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FNFeConfig.ChaveCryptINI := ChaveCrypt;
  FNFeConfig.LerIni(Ini);
  FDANFeConfig.LerIni(Ini);
  FIntegradorConfig.LerIni(Ini);
  if FDeviceConfig <> nil then FDeviceConfig.LerIni(Ini);
end;

procedure TLibNFeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FNFeConfig.ChaveCryptINI := ChaveCrypt;
  FNFeConfig.GravarIni(Ini);
  FDANFeConfig.GravarIni(Ini);
  FIntegradorConfig.GravarIni(Ini);
  if FDeviceConfig <> nil then FDeviceConfig.GravarIni(Ini);
end;

procedure TLibNFeConfig.ClasseParaComponentes;
begin
  FNFeConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibNFe(Owner).NFeDM.AplicarConfiguracoes;
end;

procedure TLibNFeConfig.ImportarIni(FIni: TIniFile);
begin

end;

procedure TLibNFeConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibNFe(Owner) do
      NFeDM.Travar;
  end;
end;

procedure TLibNFeConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibNFe(Owner) do
      NFeDM.Destravar;
  end;
end;

function TLibNFeConfig.AjustarValor(Tipo: TTipoFuncao; ASessao, AChave, AValor: Ansistring): Ansistring;
begin
  Result := '';

  if (ASessao = CSessaoDFe) and (AChave = CChaveDadosPFX) then
  begin
    TACBrLib(Owner).GravarLog(ClassName + '.AjustarValor(' + GetEnumName(TypeInfo(TTipoFuncao), Integer(Tipo)) + ','
                                                          + ASessao + ',' + AChave + ',' +
                                                          IfThen(PrecisaCriptografar(ASessao, AChave),
                                                          StringOfChar('*', Length(AValor)), AValor) +')', logParanoico);
    case Tipo of
      tfGravar: Result := StringToB64Crypt(DecodeBase64(AValor), pLib.Config.ChaveCrypt);
      tfLer: Result := EncodeBase64(B64CryptToString(AValor, pLib.Config.ChaveCrypt));
    end;

    TACBrLib(Owner).GravarLog(ClassName + '.AjustarValor - Feito Result: ' + Result, logParanoico);
  end
  else
    Result := inherited AjustarValor(Tipo, ASessao, AChave, AValor);
end;

end.

