{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{ Colaboradores nesse arquivo:                                                 }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
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
  Classes, SysUtils,
  ACBrBase,
  pcnConversao;

type

  { TCasasDecimais }
  {@class TCasasDecimais - Propriedades para configurar a formatação das casas decimais.
   Determina como será a formatação das casas decimais existentes no relátorio.
   @links TACBrDFeReport.CasasDecimais }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TCasasDecimais = class(TComponent)
  private
    fFormato: TDetFormato;
    FqCom: Integer;
    FvUnCom: Integer;
    FMaskqCom: String;
    FMaskvUnCom: String;

    procedure SetqCom(AValue: Integer);
    procedure SetvUnCom(AValue: Integer);
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
  end;

  { TACBrDFeReport }
  {@class TACBrDFeReport - Classe base para os componentes de impressão dos documentos DFe.
   @links TACBrDFeReport }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrDFeReport = class(TACBrComponent)
  private
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
    FFax: String;
    FSite: String;
    FEmail: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimais: TCasasDecimais;
    FExpandeLogoMarca: Boolean;
    FNomeDocumento: String;

    procedure SetNumCopias(const Value: Integer);
    procedure SetPathPDF(const Value: String);
    function GetPathPDF: String;
  protected
    FPArquivoPDF: String;
    function GetSeparadorPathPDF(const aInitialPath: String): String; virtual;

    function AplicarAtributoTexto(const ATexto, ABloco, ATag: String): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FormatarQuantidade(dValor: Double; dForcarDecimais: Boolean = True): String; virtual;
    function FormatarValorUnitario(dValor: Double): String; virtual;

  published
    {@prop Impressora - Define/retorna o nome da impressora onde será impresso o documento.
     @links TACBrDFeReport.Impressora :/}
    property Impressora: String read FImpressora write FImpressora;
    {@prop NomeDocumento - Define/retorna o nome do documento para exportação PDF.
     @links TACBrDFeReport.NomeDocumento :/}
    property NomeDocumento: String read FNomeDocumento write FNomeDocumento;
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
    {@prop Fax - Define/retorna número de fax para impressão.
     @links TACBrDFeReport.Fax :/}
    property Fax: String read FFax write FFax;
    {@prop Site - Define/retorna o endereço do site para impressão.
     @links TACBrDFeReport.Site :/}
    property Site: String read FSite write FSite;
    {@prop Email - Define/retorna o endereço de e-mail para impressão.
     @links TACBrDFeReport.Email :/}
    property Email: String read FEmail write FEmail;
    {@prop MargemInferior - Define/retorna a margem inferior.
     @links TACBrDFeReport.MargemInferior :/}
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    {@prop MargemSuperior - Define/retorna a margem superior.
     @links TACBrDFeReport.MargemSuperior :/}
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    {@prop MargemEsquerda - Define/retorna a margem esquerda.
     @links TACBrDFeReport.MargemEsquerda :/}
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    {@prop MargemDireita - Define/retorna a margem direita.
     @links TACBrDFeReport.MargemDireita :/}
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    {@prop ExpandirLogoMarca - Define/retorna se de expandir a logomarca na impressão.
     @links TACBrDFeReport.ExpandeLogoMarca :/}
    property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca default False;
    {@prop CasasDecimais - Configurações de impresão de números decimais.
     @links TACBrDFeReport.CasasDecimais :/}
    property CasasDecimais: TCasasDecimais read FCasasDecimais;

  end;

implementation

uses
  Math,
  ACBrUtil;

{ TCasasDecimais }
constructor TCasasDecimais.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMaskqCom := ',0.00';
  FMaskvUnCom := ',0.00';
  FQCom := 2;
  FvUnCom := 2;
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
  FFax := '';
  FSite := '';
  FEmail := '';
  FMargemInferior := 0.8;
  FMargemSuperior := 0.8;
  FMargemEsquerda := 0.6;
  FMargemDireita := 0.51;
  ExpandeLogoMarca := False;

  FCasasDecimais := TCasasDecimais.Create(self);
  FCasasDecimais.Name := 'CasasDecimais';

  {$IFDEF COMPILER6_UP}
  FCasasDecimais.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}
end;

destructor TACBrDFeReport.Destroy;
begin
  FCasasDecimais.Destroy;

  inherited Destroy;
end;

procedure TACBrDFeReport.SetPathPDF(const Value: String);
begin
  FPathPDF := PathWithDelim(Trim(Value));
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

procedure TACBrDFeReport.SetNumCopias(const Value: Integer);
begin
  if (Value < 1) then
    Exit;

  FNumCopias := Value;
end;

function TACBrDFeReport.FormatarQuantidade(dValor: Double; dForcarDecimais: Boolean): String;
begin
  if (Frac(dValor) > 0) or (dForcarDecimais) then
  begin
    case CasasDecimais.Formato of
      tdetInteger: Result := FormatFloatBr(dValor, FloatMask(CasasDecimais.qCom));
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
    tdetInteger: Result := FormatFloatBr(dValor, FloatMask(CasasDecimais.vUnCom));
    tdetMascara: Result := FormatFloatBr(dValor, CasasDecimais.MaskvUnCom);
    else
      Result := FormatFloatBr(dValor, FloatMask(CasasDecimais.vUnCom));
  end;
end;

end.
