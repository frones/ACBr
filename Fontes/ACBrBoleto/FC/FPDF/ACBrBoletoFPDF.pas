  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
  {                                                                              }
  { Colaboradores nesse arquivo:                                                 }
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
//incluido 24/04/2023

{$I ACBr.inc}
unit ACBrBoletoFPDF;

interface

uses
  SysUtils,
  Classes,
  ACBrBase,
  ACBrBoleto,
  StrUtils,
  ACBrBoletoConversao,
  ACBr_fpdf_ext,
  ACBrBoletoFPDFConst;

type
  EACBrBoletoFPDF = class(Exception);

    { TACBrBoletoFPDF }
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(piacbrAllPlatforms) ]
{$ENDIF RTL230_UP}

  TACBrBoletoFPDF = class(TACBrBoletoFCClass)
  private
    FPDF               : TFPDFExt;
    FLinhaDigitavel    : String;
    FBanco             : String;
    FCodigoBarras      : String;
    FBeneficiarioCodigo: String;
    FBeneficiarioNome  : String;
    FNomeArquivo       : String;
      { Private declarations }
    procedure GeraBoleto(const AACBrTitulo: TACBrTitulo);
    procedure GeraFichaPagamento(const AACBrTitulo: TACBrTitulo);
    procedure GeraDados(const AACBrTitulo: TACBrTitulo);
    procedure ImpressaoUnificada;
    Procedure ImpressaoIndividual;
    procedure InicializarArquivo;
    procedure FinalizarArquivo;

  public
      { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Imprimir; override;
    procedure Imprimir(AStream: TStream); override;

  published
      { Published declarations }
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings;

  { TACBrBoletoFPDF }

constructor TACBrBoletoFPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  {$IFDEF HAS_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := ',';
end;

destructor TACBrBoletoFPDF.Destroy;
begin

  inherited;
end;

procedure TACBrBoletoFPDF.FinalizarArquivo;
var LSenhaPDF : String;
begin
  LSenhaPDF := Trim(Self.PdfSenha);

  if LSenhaPDF <> '' then
    FPDF.SetProtection([canPrint, canCopy],LSenhaPDF,'');

  if Self.CalcularNomeArquivoPDFIndividual then
    FPDF.SaveToFile(Self.NomeArquivo + FNomeArquivo + '.pdf')
  else
    FPDF.SaveToFile(Self.NomeArquivo + '.pdf');
  FPDF.Free;
end;

procedure TACBrBoletoFPDF.GeraBoleto(const AACBrTitulo: TACBrTitulo);
begin
  GeraDados(AACBrTitulo);
  FPDF.SetUTF8({$IfDef USE_UTF8}True{$Else}False{$EndIf});
  FPDF.SetCompression(True);
  FPDF.AddPage();
  FPDF.SetFont('arial', '', 8);
  GeraFichaPagamento(AACBrTitulo);
end;

procedure TACBrBoletoFPDF.GeraDados(const AACBrTitulo: TACBrTitulo);
begin
  FCodigoBarras       := ACBrBoleto.Banco.MontarCodigoBarras(AACBrTitulo);
  FLinhaDigitavel     := ACBrBoleto.Banco.MontarLinhaDigitavel(FCodigoBarras, AACBrTitulo);
  FBeneficiarioCodigo := AACBrTitulo.ACBrBoleto.Cedente.Agencia + ' / ' + AACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;

  FBeneficiarioNome := Copy(AACBrTitulo.ACBrBoleto.Cedente.Nome + ' ' + AACBrTitulo.ACBrBoleto.Cedente.CNPJCPF + ' ' + AACBrTitulo.ACBrBoleto.Cedente.Logradouro + ' ' +
      AACBrTitulo.ACBrBoleto.Cedente.Cidade + ' ' + AACBrTitulo.ACBrBoleto.Cedente.UF, 1, 86);

  FBanco := FormatFloat('000', AACBrTitulo.ACBrBoleto.Banco.Numero) + '-' + IfThen(AACBrTitulo.ACBrBoleto.Banco.Digito >= 10, 'X',
    IntToStrZero(AACBrTitulo.ACBrBoleto.Banco.Digito, 1));

end;

procedure TACBrBoletoFPDF.GeraFichaPagamento(const AACBrTitulo: TACBrTitulo);
var LArquivoLogo : String;
begin
  LArquivoLogo := ChangeFileExt(ArquivoLogo,'.png');
  FPDF.SetFont('arial', '', 8);
  FPDF.Ln(35);
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 2, 'Recibo do Pagador', '', 1, 'R');
  FPDF.SetFont('arial', '', 9);
  FPDF.Cell(50, 10, '', 'B', 0, 'L');
  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, 10, 45, 40, 10);

  FPDF.SetFont('arial', 'B', 14);
  FPDF.Cell(20, 10, FBanco, 'LBR', 0, 'C');
  FPDF.SetFont('arial', 'B', 10);
  FPDF.Cell(120, 10, FLinhaDigitavel, 'B', 1, 'R');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_PAGADOR, 'LR', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 3, Copy(AACBrTitulo.ACBrBoleto.Cedente.Nome, 1, 86), 'LR', 1, 'L');
  FPDF.Cell(190, 3, Copy(AACBrTitulo.ACBrBoleto.Cedente.CNPJCPF, 1, 86), 'LR', 1, 'L');
  FPDF.Cell(190, 3, Copy(AACBrTitulo.ACBrBoleto.Cedente.Logradouro, 1, 86), 'LR', 1, 'L');
  FPDF.Cell(190, 3, Copy(AACBrTitulo.ACBrBoleto.Cedente.Cidade + ' ' + AACBrTitulo.ACBrBoleto.Cedente.UF + ' ' + AACBrTitulo.ACBrBoleto.Cedente.CEP, 1, 86), 'LRB', 1, 'L');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(38, 3, NOSSO_NUMERO, 'LR', 0, 'L');
  FPDF.Cell(38, 3, NR_DOCUMENTO, 'R', 0, 'L');
  FPDF.Cell(38, 3, DATA_VENCIMENTO, 'R', 0, 'L');
  FPDF.Cell(38, 3, VALOR_DOCUMENTO, 'L', 0, 'L');
  FPDF.Cell(38, 3, VALOR_PAGO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(38, 5, AACBrTitulo.NossoNumero, 'BLR', 0, 'C');
  FPDF.Cell(38, 5, AACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(38, 5, DateToStr(AACBrTitulo.Vencimento), 'BR', 0, 'C');
  FPDF.Cell(38, 5, FormatFloatBr(AACBrTitulo.ValorDocumento), 'BL', 0, 'C');
  FPDF.Cell(38, 5, '', 'BLR', 1, 'C');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_PAGADOR, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190, 5, AACBrTitulo.Sacado.NomeSacado + ' ' + AACBrTitulo.Sacado.CNPJCPF, 'LR', 1, 'L');
  FPDF.Cell(190, 5, AACBrTitulo.Sacado.Logradouro + ' ' + AACBrTitulo.Sacado.Complemento, 'LR', 1, 'L');
  FPDF.Cell(190, 5, AACBrTitulo.Sacado.Bairro + ' ' + AACBrTitulo.Sacado.Cidade + ' ' + AACBrTitulo.Sacado.UF + ' ' + AACBrTitulo.Sacado.CEP, 'LR', 1, 'L');
  FPDF.Cell(190, 5, BENFICIARIO_FINAL + ': ' + AACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 'BLR', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Text(FPDF.GetX + 30, FPDF.GetY + 2, AGENCIA_CODIGO_BENEFICIARIO);
  FPDF.Text(FPDF.GetX + 30, FPDF.GetY + 5, 'AAA' + FBeneficiarioCodigo);
  FPDF.Text(FPDF.GetX + 140, FPDF.GetY + 2, AUTENTICACAO_MECANICA);
  FPDF.Cell(110, 15, ' ', 'LRB', 0);
  FPDF.Cell(80, 15, ' ', 'LRB', 1);

  FPDF.Ln(35);
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 2, 'Corte na linha pontilhada', '', 1, 'R');
  FPDF.SetFont('arial', '', 12);
  FPDF.Cell(190, 2, '--------------------------------------------------------------------------------------------------------------------', '', 0, 'C');

  FPDF.Ln(20);

  FPDF.Cell(50, 10, '', 'B', 0, 'L');

  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, 10, 172, 40, 10);

  FPDF.SetFont('arial', 'B', 14);
  FPDF.Cell(20, 10, FBanco, 'LBR', 0, 'C');

  FPDF.SetFont('arial', 'B', 9);
  FPDF.Cell(120, 10, FLinhaDigitavel, 'B', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130, 3, LOCAL_PAGAMENTO, 'LR', 0, 'L');
  FPDF.Cell(60, 3, DATA_VENCIMENTO, 'R', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 5, AACBrTitulo.LocalPagamento, 'BLR', 0, 'L');
  FPDF.Cell(60, 5, DateToStr(AACBrTitulo.Vencimento), 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130, 3, NOME_BENEFICIARIO, 'LR', 0, 'L');
  FPDF.Cell(60, 3, AGENCIA_CODIGO_BENEFICIARIO, 'R', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 5, FBeneficiarioNome, 'BLR', 0, 'L');
  FPDF.Cell(60, 5, FBeneficiarioCodigo, 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(28, 3, DATA_DOCUMENTO, 'LR', 0, 'L');
  FPDF.Cell(40, 3, NR_DOCUMENTO, 'R', 0, 'L');
  FPDF.Cell(20, 3, ESPECIE_DOC, 'R', 0, 'L');
  FPDF.Cell(15, 3, ACEITE, 'R', 0, 'L');
  FPDF.Cell(27, 3, DATA_PROCESSAMENTO, '', 0, 'L');
  FPDF.Cell(60, 3, NOSSO_NUMERO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(28, 5, DateToStr(AACBrTitulo.DataDocumento), 'BLR', 0, 'C');
  FPDF.Cell(40, 5, AACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(20, 5, AACBrTitulo.EspecieDoc, 'BR', 0, 'C');
  FPDF.Cell(15, 5, IfThen(AACBrTitulo.ACEITE = atSim, 'Sim', 'Não'), 'BR', 0, 'C');
  FPDF.Cell(27, 5, DateToStr(AACBrTitulo.DataProcessamento), 'BR', 0, 'C');
  FPDF.Cell(60, 5, AACBrTitulo.NossoNumero, 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(28, 3, USO_BANCO, 'LR', 0, 'L');
  FPDF.Cell(25, 3, CARTEIRA, 'R', 0, 'L');
  FPDF.Cell(15, 3, ESPECIE, 'R', 0, 'L');
  FPDF.Cell(35, 3, QUANTIDADE, 'R', 0, 'L');
  FPDF.Cell(27, 3, XVALOR, '', 0, 'L');
  FPDF.Cell(60, 3, VALOR_DOCUMENTO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(28, 5, AACBrTitulo.UsoBanco, 'BLR', 0, 'C');
  FPDF.Cell(25, 5, AACBrTitulo.CARTEIRA, 'BR', 0, 'C');
  FPDF.Cell(15, 5, AACBrTitulo.EspecieMod, 'BR', 0, 'C');
  FPDF.Cell(35, 5, '', 'BR', 0, 'C');
  FPDF.Cell(27, 5, '', 'BR', 0, 'C');
  FPDF.Cell(60, 5, FormatFloatBr(AACBrTitulo.ValorDocumento), 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130, 3, INSTRUCOES_PAGAMENTO, 'L', 0, 'L');
  FPDF.Cell(60, 3, DESCONTO_ABATIMENTO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 5, Copy(AACBrTitulo.Mensagem.Text, 1, 86), 'L', 0, 'L');
  FPDF.Cell(60, 5, '', 'LBR', 1, 'R');

  FPDF.Cell(130, 3, Copy(AACBrTitulo.Mensagem.Text, 87, 86), 'LR', 0, 'L');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(60, 3, JUROS_MULTA, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 5, Copy(AACBrTitulo.Mensagem.Text, 174, 86), 'LR', 0, 'L');
  FPDF.Cell(60, 5, '', 'LBR', 1, 'R');
  FPDF.Cell(130, 3, Copy(AACBrTitulo.Mensagem.Text, 261, 86), 'LR', 0, 'L');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(60, 3, VALOR_PAGO, 'LR', 1, 'L');
  FPDF.SetFont('arial', '', 7);
  FPDF.Cell(130, 5, '', 'LBR', 0, 'L');
  FPDF.Cell(60, 5, '', 'LBR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_PAGADOR, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190, 5, AACBrTitulo.Sacado.NomeSacado + ' ' + AACBrTitulo.Sacado.CNPJCPF, 'LR', 1, 'L');
  FPDF.Cell(190, 5, AACBrTitulo.Sacado.Logradouro + ' ' + AACBrTitulo.Sacado.Complemento, 'LR', 1, 'L');
  FPDF.Cell(190, 5, AACBrTitulo.Sacado.Bairro + ' ' + AACBrTitulo.Sacado.Cidade + ' ' + AACBrTitulo.Sacado.UF + ' ' + AACBrTitulo.Sacado.CEP, 'LR', 1, 'L');
  FPDF.Cell(190, 5, BENFICIARIO_FINAL + ': ' + AACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 'BLR', 1, 'L');

  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 3, AUTENTICACAO_MECANICA + ' - ' + FICHA_COMPENSACAO, '', 1, 'R');
  FPDF.CodeI25(FCodigoBarras, FPDF.GetX, FPDF.GetY + 5, 15, 1);

end;

procedure TACBrBoletoFPDF.Imprimir;

begin
  ACBrBoleto.ChecarDadosObrigatorios;
  Inherited;
  if Self.CalcularNomeArquivoPDFIndividual then
    ImpressaoIndividual
  else
    ImpressaoUnificada;
end;

procedure TACBrBoletoFPDF.ImpressaoIndividual;
var
  I: Integer;
begin
  for I := 0 to Pred(ACBrBoleto.ListadeBoletos.Count) do
  begin
    InicializarArquivo;
    try
      if EstaVazio(FNomeArquivo) or (ExtractFileName(Self.NomeArquivo) = 'boleto') then
        FNomeArquivo := OnlyAlphaNum(ACBrBoleto.ListadeBoletos[ I ].NossoNumero);
      GeraBoleto(ACBrBoleto.ListadeBoletos[ I ]);
    finally
      FinalizarArquivo;
    end;
  end;
end;

procedure TACBrBoletoFPDF.ImpressaoUnificada;
var
  I: Integer;
begin
  InicializarArquivo;
  try
    for I := 0 to Pred(ACBrBoleto.ListadeBoletos.Count) do
      GeraBoleto(ACBrBoleto.ListadeBoletos[ I ]);
  finally
    FinalizarArquivo;
  end;
end;

procedure TACBrBoletoFPDF.Imprimir(AStream: TStream);
begin

end;

procedure TACBrBoletoFPDF.InicializarArquivo;
begin
  FPDF := TFPDFExt.Create();
end;

end.
