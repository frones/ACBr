{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Rafael Dias                                    }
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

{******************************************************************************
|* Historico
|*
|* 19/01/2018: Rafael Dias/DSA
|*  - Criação do componente
******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeReport;

interface

uses
  Classes, SysUtils, ACBrBase, pcnConversao;

type

  { TCasasDecimais }
  {@class TCasasDecimais - Propriedades para configurar a formatação das casas decimais.
   Determina como será a formatação das casas decimais existentes no relátorio.
   @links TACBrDFeReport.CasasDecimais }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TCasasDecimais = class(TComponent)
  private
    fFormato: TDetFormato;
    FqCom: Integer;
    FvUnCom: Integer;
    FMaskqCom: String;
    FMaskvUnCom: String;
    FAliquota: Integer;
    FMaskAliquota: String;

    procedure SetqCom(AValue: Integer);
    procedure SetvUnCom(AValue: Integer);
    procedure SetAliquota(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    {@prop Formato - Qual formato será utlizado.
     @links TCasasDecimais.Formato :/}
    property Formato: TDetFormato read fFormato write fFormato;
    {@prop _qCom - Número de Casas Decimais da Quantidade. Usado apenas se Formato = tdetInteger
     @links TCasasDecimais.qCom :/}
    property qCom: Integer read FQCom write SetqCom;
    {@prop _vUnCom - Número de Casas Decimais do Valor Unitário. Usado apenas se Formato = tdetInteger
     @links TCasasDecimais.vUnCom :/}
    property vUnCom: Integer read FvUnCom write SetvUnCom;
    {@prop _Mask_qCom - Mascara de Decimais da Quantidade. Usado apenas se Formato = tdetMascara
     @links TCasasDecimais.Mask_qCom :/}
    property MaskqCom: String read FMaskqCom write FMaskqCom;
    {@prop _Mask_vUnCom - Mascara de Decimais do Valor Unitário. Usado apenas se Formato = tdetMascara
     @links TCasasDecimais.Mask_vUnCom :/}
    property MaskvUnCom: String read FMaskvUnCom write FMaskvUnCom;
    {@prop _Aliquota - Número de Casas Decimais da Aliquota. Usado apenas se Formato = tdetInteger
     @links TCasasDecimais.Aliquota :/}
    property Aliquota: Integer read FAliquota write SetAliquota;
    {@prop _Mask_Aliquota - Mascara de Decimais da Aliquota. Usado apenas se Formato = tdetMascara
     @links TCasasDecimais.Mask_Aliquota :/}
    property MaskAliquota: String read FMaskAliquota write FMaskAliquota;
  end;

  { TExpandeLogoMarcaConfig }
  {@class TExpandeLogoMarcaConfig - Propriedades para configurar a logomarca se ExpandeLogoMarca = True
   @links TACBrDFeReport.ExpandeLogoMarcaConfig }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TExpandeLogoMarcaConfig = class(TComponent)
  private
    FAltura: Integer;
    FEsquerda: Integer;
    FTopo: Integer;
    FLargura: Integer;
    FDimensionar: Boolean;
    FEsticar: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    {@prop Altura - Define a altura da logomarca expandida
     @links TExpandeLogoMarcaConfig.Altura :/}
    property Altura: Integer read FAltura write FAltura;
    {@prop Esquerda - Define a posição a esquerda da logomarca expandida
     @links TExpandeLogoMarcaConfig.Esquerda :/}
    property Esquerda: Integer read FEsquerda write FEsquerda;
    {@prop Topo - Define a posição ao topo da logomarca expandida
     @links TExpandeLogoMarcaConfig.Topo :/}
    property Topo: Integer read FTopo write FTopo;
    {@prop Largura - Define a largura da logomarca expandida
     @links TExpandeLogoMarcaConfig.Largura :/}
    property Largura: Integer read FLargura write FLargura;
    {@prop Dimensionar - Define a logomarca expandida deve esticar no tamanho total ou não
     @links TExpandeLogoMarcaConfig.Dimensionar :/}
    property Dimensionar: Boolean read FDimensionar write FDimensionar;
    {@prop Esticar - Define a logomarca expandida deve esticar no tamanho total ou não
     @links TExpandeLogoMarcaConfig.Esticar :/}
    property Esticar: Boolean read FEsticar write FEsticar;
  end;

  { TACBrDFeReport }
  {@class TACBrDFeReport - Classe base para os componentes de impressão dos documentos DFe.
   @links TACBrDFeReport }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrDFeReport = class(TACBrComponent)
  private
    FFormularioContinuo: Boolean;
    FPathPDF: String;
    FUsaSeparadorPathPDF: Boolean;
    FImpressora: String;
    FNumCopias: Integer;
    FMostraSetup: Boolean;
    FMostraPreview: Boolean;
    FMostraStatus: Boolean;
    FLogo: String;
    FSistema: String;
    FUsuario: String;
    FSite: String;
    FEmail: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimais: TCasasDecimais;
    FExpandeLogoMarca: Boolean;
    FExpandeLogoMarcaConfig: TExpandeLogoMarcaConfig;
    FNomeDocumento: String;
    FAlterarEscalaPadrao: Boolean;
    FNovaEscala: Integer;

    procedure SetNumCopias(const AValue: Integer);
    procedure SetPathPDF(const AValue: String);
    function GetPathPDF: String;
    procedure SetNomeDocumento(const AValue: String);
    procedure SetMargemInferior(const AValue: Double);
    procedure SetMargemSuperior(const AValue: Double);
    procedure SetMargemEsquerda(const AValue: Double);
    procedure SetMargemDireita(const AValue: Double);

  protected
    FPArquivoPDF: String;
    function GetSeparadorPathPDF(const aInitialPath: String): String; virtual;

    function AplicarAtributoTexto(const ATexto, ABloco, ATag: String): String;

  public
    FIndexImpressaoIndividual : Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FormatarQuantidade(dValor: Double; dForcarDecimais: Boolean = True): String; virtual;
    function FormatarValorUnitario(dValor: Double): String; virtual;
    function FormatarAliquota(dValor: Double): String; virtual;

  protected
    {@prop FormularioContinuo - Se True, não quebra a página durante a impressão, útil para Impressão em Bobinas
     @links TACBrDFeReport.FormularioContinuo :/}
    property FormularioContinuo: Boolean read FFormularioContinuo write FFormularioContinuo default False;

  published
    {@prop Impressora - Define/retorna o nome da impressora onde será impresso o documento.
     @links TACBrDFeReport.Impressora :/}
    property Impressora: String read FImpressora write FImpressora;
    {@prop NomeDocumento - Define/retorna o nome do documento para exportação PDF.
     @links TACBrDFeReport.NomeDocumento :/}
    property NomeDocumento: String read FNomeDocumento write SetNomeDocumento;
    {@prop NumCopias - Define/retorna a quantidade de copias para Imprimir.
     @links TACBrDFeReport.NumCopias :/}
    property NumCopias: Integer read FNumCopias write SetNumCopias default 1;
    {@prop MostraPreview - Define/retorna se exibi a pre-visualização da impressão.
     @links TACBrDFeReport.MostraPreview :/}
    property MostraPreview: Boolean read FMostraPreview write FMostraPreview default True;
    {@prop MostraSetup - Define/retorna se exibi a tela de seleção de impressoras ao imprimir.
     @links TACBrDFeReport.MostraSetup :/}
    property MostraSetup: Boolean read FMostraSetup write FMostraSetup default False;
    {@prop MostraStatus - Define/retorna se exibi a situação da impressão.
     @links TACBrDFeReport.MostraStatus :/}
    property MostraStatus: Boolean read FMostraStatus write FMostraStatus default True;
    {@prop PathPDF - Define/retorna o caminho onde será salvo o PDF.
     @links TACBrDFeReport.PathPDF :/}
    property PathPDF: String read GetPathPDF write SetPathPDF;
    {@prop ArquivoPDF - Retorna o nome do arquivo PDF que foi gerado.
     @links TACBrDFeReport.ArquivoPDF :/}
    property ArquivoPDF: String read FPArquivoPDF;
    {@prop UsaSeparadorPathPDF - Define/retorna se usa os separadores no camindo do PDF.
     @links TACBrDFeReport.UsaSeparadorPathPDF :/}
    property UsaSeparadorPathPDF: Boolean read FUsaSeparadorPathPDF write FUsaSeparadorPathPDF default False;
    {@prop Logo - Define/retorna o caminho do arquivo de logo para impressão.
     @links TACBrDFeReport.Logo :/}
    property Logo: String read FLogo write FLogo;
    {@prop Sistema - Define/retorna o nome do sistema para impressão.
     @links TACBrDFeReport.Sistema :/}
    property Sistema: String read FSistema write FSistema;
    {@prop Usuario - Define/retorna o nome do usuário para impressão.
     @links TACBrDFeReport.Usuario :/}
    property Usuario: String read FUsuario write FUsuario;
    {@prop Site - Define/retorna o endereço do site para impressão.
     @links TACBrDFeReport.Site :/}
    property Site: String read FSite write FSite;
    {@prop Email - Define/retorna o endereço de e-mail para impressão.
     @links TACBrDFeReport.Email :/}
    property Email: String read FEmail write FEmail;
    {@prop MargemInferior - Define/retorna a margem inferior.
     @links TACBrDFeReport.MargemInferior :/}
    property MargemInferior: Double read FMargemInferior write SetMargemInferior;
    {@prop MargemSuperior - Define/retorna a margem superior.
     @links TACBrDFeReport.MargemSuperior :/}
    property MargemSuperior: Double read FMargemSuperior write SetMargemSuperior;
    {@prop MargemEsquerda - Define/retorna a margem esquerda.
     @links TACBrDFeReport.MargemEsquerda :/}
    property MargemEsquerda: Double read FMargemEsquerda write SetMargemEsquerda;
    {@prop MargemDireita - Define/retorna a margem direita.
     @links TACBrDFeReport.MargemDireita :/}
    property MargemDireita: Double read FMargemDireita write SetMargemDireita;
    {@prop ExpandirLogoMarca - Define/retorna se de expandir a logomarca na impressão.
     @links TACBrDFeReport.ExpandeLogoMarca :/}
    property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca default False;
    {@prop ExpandeLogoMarcaConfig - Configurações da logomarda expandida na impressão.
     @links TACBrDFeReport.ExpandeLogoMarcaConfig :/}
    property ExpandeLogoMarcaConfig: TExpandeLogoMarcaConfig read FExpandeLogoMarcaConfig;
    {@prop CasasDecimais - Configurações de impresão de números decimais.
     @links TACBrDFeReport.CasasDecimais :/}
    property CasasDecimais: TCasasDecimais read FCasasDecimais;
    {@prop AlterarEscalaPadrao - Configuração para permitir alterar escala da impressão.
     @links TACBrDFeReport.AlterarEscalaPadrao :/}
    property AlterarEscalaPadrao: Boolean read FAlterarEscalaPadrao write FAlterarEscalaPadrao default False;
    {@prop NovaEscala - Configuração para alterar escala da impressão.
     @links TACBrDFeReport.NovaEscala :/}
    property NovaEscala: Integer read FNovaEscala write FNovaEscala default 96;

  end;
  
implementation

uses
  Math,
  ACBrUtil.Base, ACBrUtil.FilesIO;

{ TCasasDecimais }

constructor TCasasDecimais.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMaskqCom := ',0.00';
  FMaskvUnCom := ',0.00';
  FMaskAliquota := ',0.00';
  FQCom := 2;
  FvUnCom := 2;
  FAliquota := 2;
end;

procedure TCasasDecimais.SetAliquota(AValue: Integer);
begin
  FAliquota := max(min(AValue, 4), 0);
end;

procedure TCasasDecimais.SetqCom(AValue: Integer);
begin
  FqCom := max(min(AValue, 4), 0);
end;

procedure TCasasDecimais.SetvUnCom(AValue: Integer);
begin
  FvUnCom := max(min(AValue, 10), 0);
end;

{ TACBrDFeReport }
constructor TACBrDFeReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPathPDF := '';
  FPArquivoPDF := '';
  FUsaSeparadorPathPDF := False;
  FImpressora := '';
  FNomeDocumento := '';
  FMostraSetup := False;
  FMostraPreview := True;
  FMostraStatus := True;
  FNumCopias := 1;
  FLogo := '';
  FSistema := 'Projeto ACBr - www.projetoacbr.com.br';
  FUsuario := '';
  FSite := '';
  FEmail := '';
  FMargemInferior := 8;
  FMargemSuperior := 8;
  FMargemEsquerda := 6;
  FMargemDireita  := 5.1;
  FExpandeLogoMarca := False;
  FAlterarEscalaPadrao := False;
  FNovaEscala := 96;
  FFormularioContinuo := False;

  FCasasDecimais := TCasasDecimais.Create(self);
  FCasasDecimais.Name := 'CasasDecimais';

  FExpandeLogoMarcaConfig := TExpandeLogoMarcaConfig.Create(self);
  FExpandeLogoMarcaConfig.Name := 'ExpandeLogoMarcaConfig';

  {$IFDEF COMPILER6_UP}
  FCasasDecimais.SetSubComponent(True);{ para gravar no DFM/XFM }
  FExpandeLogoMarcaConfig.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}
end;

destructor TACBrDFeReport.Destroy;
begin
  FCasasDecimais.Free;
  FExpandeLogoMarcaConfig.Free;

  inherited Destroy;
end;

procedure TACBrDFeReport.SetPathPDF(const AValue: String);
var
  APath, AFile, AExt: String;
begin
  if FPathPDF = AValue then
    Exit;

  APath := Trim(AValue);
  AExt  := ExtractFileExt(APath);
  AFile := '';
  if (AExt <> '') then
  begin
    AFile := ExtractFileName(APath);
    APath := ExtractFilePath(APath);
  end;

  if (APath <> '') then
    FPathPDF := PathWithDelim( APath );

  if (AFile <> '') then
    SetNomeDocumento( AFile );
end;

function TACBrDFeReport.GetPathPDF: String;
begin
  if (csDesigning in ComponentState) then  // Não tenta mudar o Path, em tempo de Design
  begin
    Result := FPathPDF;
    Exit;
  end;

  Result := Trim(FPathPDF);

  if EstaVazio(Result) then  // Se não pode definir o Path, use o Path da Aplicaçao
    Result := ApplicationPath + 'pdf' + PathDelim;

  if FUsaSeparadorPathPDF then
    Result := PathWithDelim(GetSeparadorPathPDF(Result));
end;

procedure TACBrDFeReport.SetMargemDireita(const AValue: Double);
begin
  if (AValue = 0) and (csDesigning in ComponentState) then
    Exit;
  FMargemDireita := AValue;
end;

procedure TACBrDFeReport.SetMargemEsquerda(const AValue: Double);
begin
  if (AValue = 0) and (csDesigning in ComponentState) then
    Exit;
  FMargemEsquerda := AValue;
end;

procedure TACBrDFeReport.SetMargemInferior(const AValue: Double);
begin
  if (AValue = 0) and (csDesigning in ComponentState) then
    Exit;
  FMargemInferior := AValue;
end;

procedure TACBrDFeReport.SetMargemSuperior(const AValue: Double);
begin
  if (AValue = 0) and (csDesigning in ComponentState) then
    Exit;
  FMargemSuperior := AValue;
end;

procedure TACBrDFeReport.SetNomeDocumento(const AValue: String);
var
  AFile, APath: String;
begin
  if FNomeDocumento = AValue then
    Exit;

  AFile := Trim(AValue);
  APath := ExtractFilePath(AFile);
  if (APath <> '') then
  begin
    AFile := ExtractFileName(AFile);
    FPathPDF := PathWithDelim( APath );
  end;

  if (AFile <> '') then
  begin
    if ExtractFileExt(AFile) = '' then
      AFile := AFile + '.pdf';

    FNomeDocumento := AFile;
  end;
end;

function TACBrDFeReport.GetSeparadorPathPDF(const aInitialPath: String): String;
begin
  // Esse método deve ser sobreposto pelas Classes Filhas //
  Result := aInitialPath;
end;

function TACBrDFeReport.AplicarAtributoTexto(const ATexto, ABloco, ATag: String
  ): String;
var
  TagClose: String;
begin
  TagClose := StringReplace(ATag, '<', '</', [rfReplaceAll]);
  Result := StringReplace(ATexto, ABloco, ATag + ABloco + TagClose, [rfReplaceAll]);
end;

procedure TACBrDFeReport.SetNumCopias(const AValue: Integer);
begin
  // O valor de cópias zero é utilizado por aplicações ISAPI no momento.
  // É utilizado por causa de problemas encontrados ao usar o Fortes Report.
  // Para mais informações, veja:
  // https://www.projetoacbr.com.br/forum/topic/52337-gerar-pdf-nfcenfe-danfe-aplica%C3%A7%C3%A3o-isapi-com-fortes-report/?tab=comments#comment-344431
  if (AValue < 0) then
    Exit;

  FNumCopias := AValue;
end;

function TACBrDFeReport.FormatarAliquota(dValor: Double): String;
begin
  // formatar conforme configurado
  case CasasDecimais.Formato of
    tdetMascara: Result := FormatFloatBr(dValor, CasasDecimais.MaskAliquota);
  else
    Result := FormatFloatBr(dValor, FloatMask(CasasDecimais.Aliquota));
  end;
end;

function TACBrDFeReport.FormatarQuantidade(dValor: Double; dForcarDecimais: Boolean): String;
begin
  if (Frac(dValor) > 0) or (dForcarDecimais) then
  begin
    case CasasDecimais.Formato of
      tdetMascara: Result := FormatFloatBr(dValor, CasasDecimais.MaskqCom);
    else
      Result := FormatFloatBr(dValor, FloatMask(CasasDecimais.qCom));
    end;
  end
  else
    // caso contrário mostrar somente o número inteiro
    Result := FloatToStr(dValor);
end;

function TACBrDFeReport.FormatarValorUnitario(dValor: Double): String;
begin
  // formatar conforme configurado
  case CasasDecimais.Formato of
    tdetMascara: Result := FormatFloatBr(dValor, CasasDecimais.MaskvUnCom);
  else
    Result := FormatFloatBr(dValor, FloatMask(CasasDecimais.vUnCom));
  end;
end;

{ TExpandeLogoMarcaConfig }

constructor TExpandeLogoMarcaConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAltura := 0;
  FEsquerda := 0;
  FTopo := 0;
  FLargura := 0;
  FDimensionar := False;
  FEsticar := True;
end;

end.
