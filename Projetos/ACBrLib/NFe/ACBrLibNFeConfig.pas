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
    FImprimirTotalLiquido: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FTipoDANFE: TpcnTipoImpressao;
    FNumCopias: Integer;
    FExpandirLogoMarca: Boolean;
    FImprimeDescPorc: Boolean;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FqCom: Integer;
    FvUnCom: Integer;
    FExibeResumoCanhoto: Boolean;
    FFormularioContinuo: Boolean;
    FTamanhoFonte_DemaisCampos: Integer;
    FProdutosPorPagina: Integer;
    FImprimirDetalhamentoEspecifico: Boolean;
    FTamanhoFonteEndereco: Integer;
    FPosCanhoto: TPosRecibo;
    FNomeFonte: TNomeFonte;
    FLarguraCodProd: Integer;
    FExibirEAN: Boolean;
    FExibeCampoFatura: Boolean;
    FQuebraLinhaEmDetalhamentoEspecifico: Boolean;
    FTamanhoFonte_RazaoSocial: Integer;
    FAltLinhaComun: Integer;
    FImprimirUnQtVlComercial: TImprimirUnidQtdeValor;
  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni( const AIni: TCustomIniFile );
    procedure GravarIni( const AIni: TCustomIniFile );

    property PathLogo: String read FPathLogo write FPathLogo;
    property PathPDF: String read FPathPDF write FPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostrarStatus: Boolean read FMostrarStatus write FMostrarStatus;
    property TipoDANFE: TpcnTipoImpressao read FTipoDANFE write FTipoDANFE;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property ImprimirDescPorc: Boolean read FImprimeDescPorc write FImprimeDescPorc;
    property ImprimirTotalLiquido: Boolean read FImprimirTotalLiquido write FImprimirTotalLiquido;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property QCom: Integer read FQCom write FQCom;
    property VUnCom: Integer read FvUnCom write FvUnCom;
    property ExibirResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property FormularioContinuo: Boolean read FFormularioContinuo write FFormularioContinuo;
    property ExpandirLogoMarca: Boolean read FExpandirLogoMarca write FExpandirLogoMarca;
    property TamanhoFonte_DemaisCampos: Integer read FTamanhoFonte_DemaisCampos write FTamanhoFonte_DemaisCampos;
    property ProdutosPorPagina: Integer read FProdutosPorPagina write FProdutosPorPagina;
    property ImprimirDetalhamentoEspecifico: Boolean read FImprimirDetalhamentoEspecifico write FImprimirDetalhamentoEspecifico;
    property TamanhoFonteEndereco: Integer read FTamanhoFonteEndereco write FTamanhoFonteEndereco;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property UsaCodigoEanImpressao: Boolean read FUsaCodigoEanImpressao write FUsaCodigoEanImpressao;
    property NomeFonte: TNomeFonte read FNomeFonte write FNomeFonte;
    property LarguraCodProd: Integer read FLarguraCodProd write FLarguraCodProd;
    property ExibirEAN: Boolean read FExibirEAN write FExibirEAN;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura;
    property QuebraLinhaEmDetalhamentoEspecifico: Boolean read FQuebraLinhaEmDetalhamentoEspecifico write FQuebraLinhaEmDetalhamentoEspecifico;
    property TamanhoFonte_RazaoSocial: Integer read FTamanhoFonte_RazaoSocial write FTamanhoFonte_RazaoSocial;
    property AltLinhaComun: Integer read FAltLinhaComun write FAltLinhaComun;
    property ImprimirUnQtVlComercial: TImprimirUnidQtdeValor read FImprimirUnQtVlComercial write FImprimirUnQtVlComercial;
  end;

  TACBrNFCeTipoRelatorio = (tpFortes, tpEscPos);

  TACBrEventoNFCeRelatorio = (evA4, evBobina);

  { TDANFeConfig }

  { TDANFECeConfig }

  TDANFECeConfig = class
  private
    FPathLogo: String;
    FPathPDF: String;
    FImpressora: String;
    FTipoRelatorio: TACBrNFCeTipoRelatorio;
    FTipoRelatorioEvento: TACBrEventoNFCeRelatorio;
    FImprimeNomeFantasia: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
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
    procedure LerIni( const AIni: TCustomIniFile );
    procedure GravarIni( const AIni: TCustomIniFile );

    property PathLogo: String read FPathLogo write FPathLogo;
    property PathPDF: String read FPathPDF write FPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property TipoRelatorio: TACBrNFCeTipoRelatorio read FTipoRelatorio write FTipoRelatorio;
    property TipoRelatorioEvento: TACBrEventoNFCeRelatorio read FTipoRelatorioEvento write FTipoRelatorioEvento;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostrarStatus: Boolean read FMostrarStatus write FMostrarStatus;
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
    FNFeConfig: TConfiguracoesNFe;
  public
    constructor Create(ANomeArquivo: String = ''); override;
    destructor Destroy; override;

    procedure Ler; override;
    procedure Gravar; override;

    property NFeConfig: TConfiguracoesNFe read FNFeConfig;
  end;

implementation

uses
  ACBrLibNFeClass;

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
  FMostrarPreview := False;
  FMostrarStatus := False;
  FNumCopias := 1;
  FLarguraBobina   := 302;
  FMargemInferior  := 0.8;
  FMargemSuperior  := 0.8;
  FMargemEsquerda  := 0.6;
  FMargemDireita   := 0.51;
  FqCom         := 2;
  FvUnCom       := 2;
  FImprimeEmUmaLinha := False;
  FUsaCodigoEanImpressao := True;
  FImprimeDescAcrescItem := True;
  FQRCodeLateral := True;
end;

procedure TDANFECeConfig.LerIni(const AIni: TCustomIniFile);
begin

end;

procedure TDANFECeConfig.GravarIni(const AIni: TCustomIniFile);
begin

end;

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
  FImprimirTotalLiquido := False;
  FMostrarPreview := True;
  FMostrarStatus := True;
  FNumCopias := 1;
  FImprimeDescPorc := False;
  FExpandirLogoMarca := False;
  FMargemInferior := 0.7;
  FMargemSuperior := 0.7;
  FMargemEsquerda := 0.7;
  FMargemDireita := 0.7;
  FFormularioContinuo := False;
  FTamanhoFonteEndereco := 0;
  FTamanhoFonte_DemaisCampos := 8;
  FProdutosPorPagina := 0;
  FImprimeNomeFantasia := False;
  FImprimirDetalhamentoEspecifico := True;
  FQCom := 2;
  FvUnCom := 2;
  FExibeResumoCanhoto := False;
  FPosCanhoto := prCabecalho;
  FUsaCodigoEanImpressao := False;
  FNomeFonte := nfTimesNewRoman;
  FLarguraCodProd := 54;
  FExibirEAN := False;
  FExibeCampoFatura := True;
  FQuebraLinhaEmDetalhamentoEspecifico := True;
  FTamanhoFonte_RazaoSocial := 8;
  FAltLinhaComun := 30;
  FImprimirUnQtVlComercial := iuComercial;
end;

procedure TDANFeConfig.LerIni(const AIni: TCustomIniFile);
begin

end;

procedure TDANFeConfig.GravarIni(const AIni: TCustomIniFile);
begin

end;

{ TLibNFeConfig }

constructor TLibNFeConfig.Create(ANomeArquivo: String);
begin
  inherited Create(ANomeArquivo);

  FNFeConfig := TConfiguracoesNFe.Create(nil);
end;

destructor TLibNFeConfig.Destroy;
begin
  FNFeConfig.Destroy;

  inherited Destroy;
end;

procedure TLibNFeConfig.Ler;
begin
  if Assigned(pLibNFeDM) then
    pLibNFeDM.Lock.Acquire;

  try
    inherited Ler;

    FNFeConfig.LerIni(Ini);
  finally
    // Ajustes pos leitura das configurações //
    if Assigned(pLibNFeDM) then
    begin
      pLibNFeDM.AplicarConfiguracoes;
      pLibNFeDM.Lock.Release;
    end;
  end;
end;

procedure TLibNFeConfig.Gravar;
begin
  if Assigned(pLibNFeDM) then
    pLibNFeDM.Lock.Acquire;

  try
    inherited Gravar;

    FNFeConfig.GravarIni(Ini);
  finally
    if Assigned(pLibNFeDM) then
      pLibNFeDM.Lock.Release;
  end;
end;

end.























































