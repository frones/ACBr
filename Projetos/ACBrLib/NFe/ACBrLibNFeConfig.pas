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

unit ACBrLibNFeConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrNFeConfiguracoes, ACBrNFeDANFeRLClass,
  pcnConversao,
  ACBrLibConfig;

type

  { TDANFeConfig }

  TDANFeConfig = class
  private
    FPathLogo: String;
    FUsaCodigoEanImpressao: Boolean;
    FPathPDF: String;
    FImpressora: String;
    FImprimeNomeFantasia: Boolean;
    FImprimeTotalLiquido: Boolean;
    FMostraPreview: Boolean;
    FMostraStatus: Boolean;
    FTipoDANFE: TpcnTipoImpressao;
    FNumCopias: Integer;
    FExpandeLogoMarca: Boolean;
    FImprimeDescPorc: Boolean;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FqCom: Integer;
    FvUnCom: Integer;
    FExibeResumoCanhoto: Boolean;
    FFormularioContinuo: Boolean;
    FTamanhoFonteDemaisCampos: Integer;
    FProdutosPorPagina: Integer;
    FImprimeDetalhamentoEspecifico: Boolean;
    FTamanhoFonteEndereco: Integer;
    FPosCanhoto: TPosRecibo;
    FNomeFonte: TNomeFonte;
    FLarguraCodProd: Integer;
    FExibeEAN: Boolean;
    FExibeCampoFatura: Boolean;
    FQuebraLinhaEmDetalhamentoEspecifico: Boolean;
    FTamanhoFonteRazaoSocial: Integer;
    FAlturaLinhaComun: Integer;
    FTipoUnQtVlComercial: TImprimirUnidQtdeValor;
  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property PathLogo: String read FPathLogo write FPathLogo;
    property PathPDF: String read FPathPDF write FPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property MostraPreview: Boolean read FMostraPreview write FMostraPreview;
    property MostraStatus: Boolean read FMostraStatus write FMostraStatus;
    property TipoDANFE: TpcnTipoImpressao read FTipoDANFE write FTipoDANFE;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property ImprimeDescPorc: Boolean read FImprimeDescPorc write FImprimeDescPorc;
    property ImprimeTotalLiquido: Boolean read FImprimeTotalLiquido write FImprimeTotalLiquido;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property QCom: Integer read FQCom write FQCom;
    property VUnCom: Integer read FvUnCom write FvUnCom;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property FormularioContinuo: Boolean read FFormularioContinuo write FFormularioContinuo;
    property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca;
    property TamanhoFonteDemaisCampos: Integer read FTamanhoFonteDemaisCampos write FTamanhoFonteDemaisCampos;
    property ProdutosPorPagina: Integer read FProdutosPorPagina write FProdutosPorPagina;
    property ImprimeDetalhamentoEspecifico: Boolean read FImprimeDetalhamentoEspecifico write FImprimeDetalhamentoEspecifico;
    property TamanhoFonteEndereco: Integer read FTamanhoFonteEndereco write FTamanhoFonteEndereco;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property UsaCodigoEanImpressao: Boolean read FUsaCodigoEanImpressao write FUsaCodigoEanImpressao;
    property NomeFonte: TNomeFonte read FNomeFonte write FNomeFonte;
    property LarguraCodProd: Integer read FLarguraCodProd write FLarguraCodProd;
    property ExibeEAN: Boolean read FExibeEAN write FExibeEAN;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura;
    property QuebraLinhaEmDetalhamentoEspecifico: Boolean read FQuebraLinhaEmDetalhamentoEspecifico write FQuebraLinhaEmDetalhamentoEspecifico;
    property TamanhoFonteRazaoSocial: Integer read FTamanhoFonteRazaoSocial write FTamanhoFonteRazaoSocial;
    property AlturaLinhaComun: Integer read FAlturaLinhaComun write FAlturaLinhaComun;
    property TipoUnQtVlComercial: TImprimirUnidQtdeValor read FTipoUnQtVlComercial write FTipoUnQtVlComercial;
  end;

  TEventoNFCeRelatorio = (evA4, evBobina);

  { TDANFECeConfig }

  TDANFECeConfig = class
  private
    FPathLogo: String;
    FPathPDF: String;
    FImpressora: String;
    FTipoRelatorio: TTipoRelatorioBobina;
    FTipoRelatorioEvento: TEventoNFCeRelatorio;
    FImprimeNomeFantasia: Boolean;
    FMostraPreview: Boolean;
    FMostraStatus: Boolean;
    FNumCopias: Integer;
    FLarguraBobina: Integer;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FqCom: Integer;
    FvUnCom: Integer;
    FImprimeEmUmaLinha: Boolean;
    FUsaCodigoEanImpressao: Boolean;
    FImprimeDescAcrescItem: Boolean;
    FQRCodeLateral: Boolean;
  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property PathLogo: String read FPathLogo write FPathLogo;
    property PathPDF: String read FPathPDF write FPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property TipoRelatorio: TTipoRelatorioBobina read FTipoRelatorio write FTipoRelatorio;
    property TipoRelatorioEvento: TEventoNFCeRelatorio read FTipoRelatorioEvento write FTipoRelatorioEvento;
    property MostraPreview: Boolean read FMostraPreview write FMostraPreview;
    property MostraStatus: Boolean read FMostraStatus write FMostraStatus;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property LarguraBobina: Integer read FLarguraBobina write FLarguraBobina;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property QCom: Integer read FQCom write FQCom;
    property VUnCom: Integer read FvUnCom write FvUnCom;
    property ImprimeEmUmaLinha: Boolean read FImprimeEmUmaLinha write FImprimeEmUmaLinha;
    property ImprimeDescAcrescItem: Boolean read FImprimeDescAcrescItem write FImprimeDescAcrescItem;
    property UsaCodigoEanImpressao: Boolean read FUsaCodigoEanImpressao write FUsaCodigoEanImpressao;
    property QRCodeLateral: Boolean read FQRCodeLateral write FQRCodeLateral;
  end;


  { TLibNFeConfig }

  TLibNFeConfig = class(TLibConfig)
  private
    FDANFECeConfig: TDANFECeConfig;
    FDANFeConfig: TDANFeConfig;
    FNFeConfig: TConfiguracoesNFe;
  protected
    function AtualizarArquivoConfiguracao: Boolean; override;
    procedure AplicarConfiguracoes; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property NFeConfig: TConfiguracoesNFe read FNFeConfig;
    property DANFeConfig: TDANFeConfig read FDANFeConfig;
    property DANFECeConfig: TDANFECeConfig read FDANFECeConfig;
  end;

implementation

uses
  ACBrLibNFeClass, ACBrLibNFeConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TDANFeConfig }

constructor TDANFeConfig.Create;
begin
  inherited Create;
  DefinirValoresPadroes;
end;

procedure TDANFeConfig.DefinirValoresPadroes;
begin
  FPathLogo := '';
  FPathPDF := '';
  FImpressora := '';
  FTipoDANFE := tiRetrato;
  FImprimeTotalLiquido := False;
  FMostraPreview := True;
  FMostraStatus := True;
  FNumCopias := 1;
  FImprimeDescPorc := False;
  FExpandeLogoMarca := False;
  FMargemInferior := 0.7;
  FMargemSuperior := 0.7;
  FMargemEsquerda := 0.7;
  FMargemDireita := 0.7;
  FFormularioContinuo := False;
  FTamanhoFonteEndereco := 0;
  FTamanhoFonteDemaisCampos := 8;
  FProdutosPorPagina := 0;
  FImprimeNomeFantasia := False;
  FImprimeDetalhamentoEspecifico := True;
  FQCom := 2;
  FvUnCom := 2;
  FExibeResumoCanhoto := False;
  FPosCanhoto := prCabecalho;
  FUsaCodigoEanImpressao := False;
  FNomeFonte := nfTimesNewRoman;
  FLarguraCodProd := 54;
  FExibeEAN := False;
  FExibeCampoFatura := True;
  FQuebraLinhaEmDetalhamentoEspecifico := True;
  FTamanhoFonteRazaoSocial := 8;
  FAlturaLinhaComun := 30;
  FTipoUnQtVlComercial := iuComercial;
end;

procedure TDANFeConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPathLogo := AIni.ReadString(CSessaoDANFE, CChavePathLogo, FPathLogo);
  FPathPDF := AIni.ReadString(CSessaoDANFE, CChavePathPDF, FPathPDF);
  FImpressora := AIni.ReadString(CSessaoDANFE, CChaveImpressora, FImpressora);
  FTipoDANFE := TpcnTipoImpressao(AIni.ReadInteger(CSessaoDANFE, CChaveTipoDANFE, Integer(FTipoDANFE)));
  FImprimeTotalLiquido := AIni.ReadBool(CSessaoDANFE, CChaveImprimeTotalLiquido, FImprimeTotalLiquido);
  FMostraPreview := AIni.ReadBool(CSessaoDANFE, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(CSessaoDANFE, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(CSessaoDANFE, CChaveCopias, FNumCopias);
  FImprimeDescPorc := AIni.ReadBool(CSessaoDANFE, CChaveImprimeDescPorc, FImprimeDescPorc);
  FExpandeLogoMarca := AIni.ReadBool(CSessaoDANFE, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  FMargemInferior := AIni.ReadFloat(CSessaoDANFE, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoDANFE, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoDANFE, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoDANFE, CChaveMargemDireita, FMargemDireita);
  FFormularioContinuo := AIni.ReadBool(CSessaoDANFE, CChaveFormularioContinuo, FFormularioContinuo);
  FTamanhoFonteEndereco := AIni.ReadInteger(CSessaoDANFE, CChaveTamanhoFonteEndereco, FTamanhoFonteEndereco);
  FTamanhoFonteDemaisCampos := AIni.ReadInteger(CSessaoDANFE, CChaveTamanhoFonteDemaisCampos, FTamanhoFonteDemaisCampos);
  FTamanhoFonteRazaoSocial := AIni.ReadInteger(CSessaoDANFE, CChaveTamanhoFonteRazaoSocial, FTamanhoFonteRazaoSocial);
  FProdutosPorPagina := AIni.ReadInteger(CSessaoDANFE, CChaveProdutosPorPagina, FProdutosPorPagina);
  FImprimeNomeFantasia := AIni.ReadBool(CSessaoDANFE, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  FImprimeDetalhamentoEspecifico := AIni.ReadBool(CSessaoDANFE, CChaveImprimeDetalhamentoEspecifico, FImprimeDetalhamentoEspecifico);
  FqCom := AIni.ReadInteger(CSessaoDANFE, CChaveDecimaisQtd, FqCom);
  FvUnCom := AIni.ReadInteger(CSessaoDANFE, CChaveDecimaisValUnit, FvUnCom);
  FExibeResumoCanhoto := AIni.ReadBool(CSessaoDANFE, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  FPosCanhoto := TPosRecibo(AIni.ReadInteger(CSessaoDANFE, CChavePosCanhoto, Integer(FPosCanhoto)));
  FUsaCodigoEanImpressao := AIni.ReadBool(CSessaoDANFE, CChaveImprimeCodigoEAN, FUsaCodigoEanImpressao);
  FNomeFonte := TNomeFonte(AIni.ReadInteger(CSessaoDANFE, CChaveNomeFonte, Integer(FNomeFonte)));
  FLarguraCodProd := AIni.ReadInteger(CSessaoDANFE, CChaveLarguraCodProd, FLarguraCodProd);
  FExibeEAN := AIni.ReadBool(CSessaoDANFE, CChaveExibeEAN, FExibeEAN);
  FExibeCampoFatura := AIni.ReadBool(CSessaoDANFE, CChaveExibeCampoFatura, FExibeCampoFatura);
  FQuebraLinhaEmDetalhamentoEspecifico := AIni.ReadBool(CSessaoDANFE, CChaveQuebraLinhaEmDetalhamentoEspecifico, FQuebraLinhaEmDetalhamentoEspecifico);
  FAlturaLinhaComun := AIni.ReadInteger(CSessaoDANFE, CChaveAlturaLinhaComun, FAlturaLinhaComun);
  FTipoUnQtVlComercial := TImprimirUnidQtdeValor(AIni.ReadInteger(CSessaoDANFE, CChaveTipoUnQtVlComercial, Integer(FTipoUnQtVlComercial)));
end;

procedure TDANFeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDANFE, CChavePathLogo, FPathLogo);
  AIni.WriteString(CSessaoDANFE, CChavePathPDF, FPathPDF);
  AIni.WriteString(CSessaoDANFE, CChaveImpressora, FImpressora);
  AIni.WriteInteger(CSessaoDANFE, CChaveTipoDANFE, Integer(FTipoDANFE));
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeTotalLiquido, FImprimeTotalLiquido);
  AIni.WriteBool(CSessaoDANFE, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(CSessaoDANFE, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(CSessaoDANFE, CChaveCopias, FNumCopias);
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeDescPorc, FImprimeDescPorc);
  AIni.WriteBool(CSessaoDANFE, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  AIni.WriteFloat(CSessaoDANFE, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDANFE, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDANFE, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDANFE, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(CSessaoDANFE, CChaveFormularioContinuo, FFormularioContinuo);
  AIni.WriteInteger(CSessaoDANFE, CChaveTamanhoFonteEndereco, FTamanhoFonteEndereco);
  AIni.WriteInteger(CSessaoDANFE, CChaveTamanhoFonteDemaisCampos, FTamanhoFonteDemaisCampos);
  AIni.WriteInteger(CSessaoDANFE, CChaveTamanhoFonteRazaoSocial, FTamanhoFonteRazaoSocial);
  AIni.WriteInteger(CSessaoDANFE, CChaveProdutosPorPagina, FProdutosPorPagina);
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeDetalhamentoEspecifico, FImprimeDetalhamentoEspecifico);
  AIni.WriteInteger(CSessaoDANFE, CChaveDecimaisQtd, FqCom);
  AIni.WriteInteger(CSessaoDANFE, CChaveDecimaisValUnit, FvUnCom);
  AIni.WriteBool(CSessaoDANFE, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  AIni.WriteInteger(CSessaoDANFE, CChavePosCanhoto, Integer(FPosCanhoto));
  AIni.WriteBool(CSessaoDANFE, CChaveImprimeCodigoEAN, FUsaCodigoEanImpressao);
  AIni.WriteInteger(CSessaoDANFE, CChaveNomeFonte, Integer(FNomeFonte));
  AIni.WriteInteger(CSessaoDANFE, CChaveLarguraCodProd, FLarguraCodProd);
  AIni.WriteBool(CSessaoDANFE, CChaveExibeEAN, FExibeEAN);
  AIni.WriteBool(CSessaoDANFE, CChaveExibeCampoFatura, FExibeCampoFatura);
  AIni.WriteBool(CSessaoDANFE, CChaveQuebraLinhaEmDetalhamentoEspecifico, FQuebraLinhaEmDetalhamentoEspecifico);
  AIni.WriteInteger(CSessaoDANFE, CChaveAlturaLinhaComun, FAlturaLinhaComun);
  AIni.WriteInteger(CSessaoDANFE, CChaveTipoUnQtVlComercial, Integer(FTipoUnQtVlComercial));
end;

{ TDANFECeConfig }

constructor TDANFECeConfig.Create;
begin
  inherited Create;
  DefinirValoresPadroes;
end;

procedure TDANFECeConfig.DefinirValoresPadroes;
begin
  FPathLogo := '';
  FPathPDF := '';
  FImpressora := '';
  FTipoRelatorio := tpEscPos;
  FTipoRelatorioEvento := evBobina;
  FImprimeNomeFantasia := True;
  FMostraPreview := False;
  FMostraStatus := False;
  FNumCopias := 1;
  FLarguraBobina := 302;
  FMargemInferior := 0.8;
  FMargemSuperior := 0.8;
  FMargemEsquerda := 0.6;
  FMargemDireita := 0.51;
  FqCom := 2;
  FvUnCom := 2;
  FImprimeEmUmaLinha := False;
  FUsaCodigoEanImpressao := True;
  FImprimeDescAcrescItem := True;
  FQRCodeLateral := True;
end;

procedure TDANFECeConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPathLogo := AIni.ReadString(CSessaoDANFECe, CChavePathLogo, FPathLogo);
  FPathPDF := AIni.ReadString(CSessaoDANFECe, CChavePathPDF, FPathPDF);
  FImpressora := AIni.ReadString(CSessaoDANFECe, CChaveImpressora, FImpressora);
  FTipoRelatorio := TTipoRelatorioBobina(AIni.ReadInteger(CSessaoDANFECe, CChaveTipoRelatorioBobina, Integer(FTipoRelatorio)));
  FTipoRelatorioEvento := TEventoNFCeRelatorio(AIni.ReadInteger(CSessaoDANFECe, CChaveTipoRelatorioEvento, Integer(FTipoRelatorioEvento)));
  FImprimeNomeFantasia := AIni.ReadBool(CSessaoDANFECe, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  FMostraPreview := AIni.ReadBool(CSessaoDANFECe, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(CSessaoDANFECe, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(CSessaoDANFECe, CChaveCopias, FNumCopias);
  FLarguraBobina := AIni.ReadInteger(CSessaoDANFECe, CChaveLarguraBobina, FLarguraBobina);
  FMargemInferior := AIni.ReadFloat(CSessaoDANFECe, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoDANFECe, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoDANFECe, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoDANFECe, CChaveMargemDireita, FMargemDireita);
  FqCom := AIni.ReadInteger(CSessaoDANFECe, CChaveDecimaisQtd, FqCom);
  FvUnCom := AIni.ReadInteger(CSessaoDANFECe, CChaveDecimaisValUnit, FvUnCom);
  FImprimeEmUmaLinha := AIni.ReadBool(CSessaoDANFECe, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  FUsaCodigoEanImpressao := AIni.ReadBool(CSessaoDANFECe, CChaveImprimeCodigoEAN, FUsaCodigoEanImpressao);
  FImprimeDescAcrescItem := AIni.ReadBool(CSessaoDANFECe, CChaveImprimeDescAcrescItem, FImprimeDescAcrescItem);
  FQRCodeLateral := AIni.ReadBool(CSessaoDANFECe, CChaveQRCodeLateral, FQRCodeLateral);
end;

procedure TDANFECeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDANFECe, CChavePathLogo, FPathLogo);
  AIni.WriteString(CSessaoDANFECe, CChavePathPDF, FPathPDF);
  AIni.WriteString(CSessaoDANFECe, CChaveImpressora, FImpressora);
  AIni.WriteInteger(CSessaoDANFECe, CChaveTipoRelatorioBobina, Integer(FTipoRelatorio));
  AIni.WriteInteger(CSessaoDANFECe, CChaveTipoRelatorioEvento, Integer(FTipoRelatorioEvento));
  AIni.WriteBool(CSessaoDANFECe, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  AIni.WriteBool(CSessaoDANFECe, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(CSessaoDANFECe, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(CSessaoDANFECe, CChaveCopias, FNumCopias);
  AIni.WriteInteger(CSessaoDANFECe, CChaveLarguraBobina, FLarguraBobina);
  AIni.WriteFloat(CSessaoDANFECe, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDANFECe, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDANFECe, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDANFECe, CChaveMargemDireita, FMargemDireita);
  AIni.WriteInteger(CSessaoDANFECe, CChaveDecimaisQtd, FqCom);
  AIni.WriteInteger(CSessaoDANFECe, CChaveDecimaisValUnit, FvUnCom);
  AIni.WriteBool(CSessaoDANFECe, CChaveImprimeEmUmaLinha, FImprimeEmUmaLinha);
  AIni.WriteBool(CSessaoDANFECe, CChaveImprimeCodigoEAN, FUsaCodigoEanImpressao);
  AIni.WriteBool(CSessaoDANFECe, CChaveImprimeDescAcrescItem, FImprimeDescAcrescItem);
  AIni.WriteBool(CSessaoDANFECe, CChaveQRCodeLateral, FQRCodeLateral);
end;

{ TLibNFeConfig }

constructor TLibNFeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FNFeConfig := TConfiguracoesNFe.Create(nil);
  FDANFeConfig := TDANFeConfig.Create;
  FDANFECeConfig := TDANFECeConfig.Create;
end;

destructor TLibNFeConfig.Destroy;
begin
  FNFeConfig.Destroy;
  FDANFeConfig.Free;
  FDANFECeConfig.Free;

  inherited Destroy;
end;

function TLibNFeConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibNFeNome, '0');
  Result := (CompareVersions(CLibNFeVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibNFeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FNFeConfig.LerIni(Ini);
  FDANFeConfig.LerIni(Ini);
  FDANFECeConfig.LerIni(Ini);
end;

procedure TLibNFeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibNFeNome, CLibNFeVersao);

  FNFeConfig.GravarIni(Ini);
  FDANFeConfig.GravarIni(Ini);
  FDANFECeConfig.GravarIni(Ini);
end;

procedure TLibNFeConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibNFe(Owner).NFeDM.AplicarConfiguracoes;
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

end.

