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
  ACBr_fpdf,
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
    FACBrTitulo        : TACBrTitulo;
    FNumeroPassadas    : Cardinal;
    
      { Private declarations }
    procedure GeraDados(const AACBrTitulo: TACBrTitulo);

    procedure ImpressaoUnificada;
    Procedure ImpressaoIndividual;
    procedure InicializarArquivo(const AOrientation: TFPDFOrientation = poPortrait; APageUnit: TFPDFUnit = puMM; APageFormat: TFPDFPageFormat = pfA4);
    procedure FinalizarArquivo;
    procedure ModeloImpressao(const AInicializarArquivo : Boolean = False);
    procedure ModeloEstruturaReciboPagador(const AEspacoAntes: Double = 0; AEspacoDepois : Double = 0);
    procedure ModeloEstruturaReciboPagadorPIX(const AEspacoAntes: Double = 0; AEspacoDepois : Double = 0);
    procedure ModeloEstruturaReciboEntrega(const AEspacoAntes: Double = 0;  AEspacoDepois : Double = 0; ALinhaDigitavel :Boolean = True);
    procedure ModeloEstruturaFichaPagamento(const AEspacoAntes: Double = 0;  AEspacoDepois : Double = 0);
    procedure ModeloEstruturaLinhaPontinhada(const AEspacoAntes: Double = 35; AEspacoDepois : Double = 20);
    procedure ModeloEstruturaFatura(const AEspacoAntes: Double = 0; AEspacoDepois : Double = 0);
    procedure ModeloEstruturaSoftwareHouse;

    {MODELOS DE FICHAS DE PAGAMENTO}
    procedure ModeloBoletoCarne(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoCarneA5(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoFatura(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoFaturaDetal(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoPadrao(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoEntrega(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoEntrega2(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoPIX(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoPrestaServicos(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoReciboTopo(const AInicializarArquivo : Boolean = False);
    procedure ModeloBoletoTermica80mm(const AInicializarArquivo : Boolean = False);
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
    LPath : String;
    LFile : String;
begin
  LSenhaPDF := Trim(Self.PdfSenha);

  if LSenhaPDF <> '' then
    FPDF.SetProtection([canPrint, canCopy],LSenhaPDF,'');

  LPath := IncludeTrailingPathDelimiter(ExtractFilePath(Self.NomeArquivo));
  LFile := ChangeFileExt(ExtractFileName(Self.NomeArquivo),'');

  if LFile <> '' then
    LFile := LFile+'_';

  if Self.CalcularNomeArquivoPDFIndividual then
    FPDF.SaveToFile( ChangeFileExt(LPath + LFile+FNomeArquivo, '.pdf'))
  else
    FPDF.SaveToFile(ChangeFileExt(Self.NomeArquivo, '.pdf'));
  FPDF.Free;
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

  FACBrTitulo := AACBrTitulo;
  ModeloImpressao;
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
    ModeloImpressao(True);
    try
      if EstaVazio(FNomeArquivo) or (ExtractFileName(Self.NomeArquivo) = 'boleto') then
        FNomeArquivo := OnlyAlphaNum(ACBrBoleto.ListadeBoletos[ I ].NossoNumero);
      GeraDados(ACBrBoleto.ListadeBoletos[ I ]);
    finally
      FinalizarArquivo;
    end;
  end;
end;

procedure TACBrBoletoFPDF.ImpressaoUnificada;
var
  I: Integer;
begin
  ModeloImpressao(true);
  try
    for I := 0 to Pred(ACBrBoleto.ListadeBoletos.Count) do
      GeraDados(ACBrBoleto.ListadeBoletos[ I ]);
  finally
    FinalizarArquivo;
  end;
end;

procedure TACBrBoletoFPDF.Imprimir(AStream: TStream);
begin

end;

procedure TACBrBoletoFPDF.ModeloEstruturaFatura(const AEspacoAntes: Double;
  AEspacoDepois: Double);
var LArquivoLogo : String;
    LDetalhamento : TStringList;
    I,IndexDetalhamento: Integer;
begin
  LArquivoLogo := ChangeFileExt(FACBrTitulo.ArquivoLogoEmp,'.png');
  
  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);
  
  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, FPDF.GetX, FPDF.GetY-2, 50, 10);
  FPDF.Cell(50, 5, '', '', 0, 'L');
  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(140, 5, Copy(FACBrTitulo.ACBrBoleto.Cedente.Nome, 1, 86)
                    + ' - '
                    + CNPJ
                    + ' '
                    +Copy(FACBrTitulo.ACBrBoleto.Cedente.CNPJCPF, 1, 86), 'LR', 1, 'L');
  FPDF.SetFont('arial', '', 7);
  FPDF.Cell(50, 5, '', 'B', 0, 'L');
  FPDF.Cell(140, 5, Copy(FACBrTitulo.ACBrBoleto.Cedente.Cidade
                    + ' '
                    + FACBrTitulo.ACBrBoleto.Cedente.UF
                    + ' '
                    + FACBrTitulo.ACBrBoleto.Cedente.CEP, 1, 86), 'LRB', 1, 'L');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(31.67, 3, NOSSO_NUMERO, 'LR', 0, 'C');
  FPDF.Cell(31.67, 3, NR_DOCUMENTO, 'R', 0, 'C');
  FPDF.Cell(31.67, 3, DATA_DOCUMENTO, 'R', 0, 'C');
  FPDF.Cell(31.67, 3, COMPETENCIA, 'R', 0, 'C');
  FPDF.Cell(31.66, 3, VALOR_DOCUMENTO, 'R', 0, 'C');
  FPDF.Cell(31.66, 3, DATA_VENCIMENTO, 'R', 1, 'C');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(31.67, 3, FACBrTitulo.NossoNumero, 'BLR', 0, 'C');
  FPDF.Cell(31.67, 3, FACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(31.67, 3, DateToStr(FACBrTitulo.DataDocumento), 'BR', 0, 'C');
  FPDF.Cell(31.67, 3, FACBrTitulo.Competencia, 'BR', 0, 'C');
  FPDF.Cell(31.66, 3, FormatFloatBr(FACBrTitulo.ValorDocumento), 'BR', 0, 'C');
  FPDF.Cell(31.66, 3, DateToStr(FACBrTitulo.Vencimento), 'BR', 1, 'C');
  FPDF.SetFont('arial', 'B', 5);
  FPDF.Cell(190, 3, DETALHAMENTO_FATURA, '', 1, 'L');
  FPDF.SetFont('arial', '', 5);
  LDetalhamento := TStringList.Create;
  try
    IndexDetalhamento := FACBrTitulo.Detalhamento.Count;
    for I := 0 to 39 do
     if IndexDetalhamento > I then
       LDetalhamento.AddStrings(FACBrTitulo.Detalhamento.Strings[I])
     else
       LDetalhamento.Add(''); 

    FPDF.MultiCell(190,2,LDetalhamento.Text,'','J');
  finally
    LDetalhamento.Free;
  end;
 
end;

procedure TACBrBoletoFPDF.ModeloEstruturaFichaPagamento(const AEspacoAntes: Double;
  AEspacoDepois     : Double);
var LArquivoLogo : String;
begin
  LArquivoLogo := ChangeFileExt(ArquivoLogo,'.png');

  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);
  
  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, FPDF.GetX, FPDF.GetY-2, 40, 10);

  FPDF.Cell(50, 8, '', 'B', 0, 'L');
  FPDF.SetFont('arial', 'B', 14);
  FPDF.Cell(20, 8, FBanco, 'LBR', 0, 'C');

  FPDF.SetFont('arial', 'B', 9);
  FPDF.Cell(120, 8, FLinhaDigitavel, 'B', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130, 3, LOCAL_PAGAMENTO, 'LR', 0, 'L');
  FPDF.Cell(60, 3, DATA_VENCIMENTO, 'R', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 3, FACBrTitulo.LocalPagamento, 'BLR', 0, 'L');
  FPDF.Cell(60, 3, DateToStr(FACBrTitulo.Vencimento), 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130, 3, NOME_BENEFICIARIO, 'LR', 0, 'L');
  FPDF.Cell(60, 3, AGENCIA_CODIGO_BENEFICIARIO, 'R', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 3, FBeneficiarioNome, 'BLR', 0, 'L');
  FPDF.Cell(60, 3, FBeneficiarioCodigo, 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(28, 3, DATA_DOCUMENTO, 'LR', 0, 'L');
  FPDF.Cell(40, 3, NR_DOCUMENTO, 'R', 0, 'L');
  FPDF.Cell(20, 3, ESPECIE_DOC, 'R', 0, 'L');
  FPDF.Cell(15, 3, ACEITE, 'R', 0, 'L');
  FPDF.Cell(27, 3, DATA_PROCESSAMENTO, '', 0, 'L');
  FPDF.Cell(60, 3, NOSSO_NUMERO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(28, 3, DateToStr(FACBrTitulo.DataDocumento), 'BLR', 0, 'C');
  FPDF.Cell(40, 3, FACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(20, 3, FACBrTitulo.EspecieDoc, 'BR', 0, 'C');
  FPDF.Cell(15, 3, IfThen(FACBrTitulo.ACEITE = atSim, 'Sim', 'Não'), 'BR', 0, 'C');
  FPDF.Cell(27, 3, DateToStr(FACBrTitulo.DataProcessamento), 'BR', 0, 'C');
  FPDF.Cell(60, 3, FACBrTitulo.NossoNumero, 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(28, 3, USO_BANCO, 'LR', 0, 'L');
  FPDF.Cell(25, 3, CARTEIRA, 'R', 0, 'L');
  FPDF.Cell(15, 3, ESPECIE, 'R', 0, 'L');
  FPDF.Cell(35, 3, QUANTIDADE, 'R', 0, 'L');
  FPDF.Cell(27, 3, XVALOR, '', 0, 'L');
  FPDF.Cell(60, 3, VALOR_DOCUMENTO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(28, 3, FACBrTitulo.UsoBanco, 'BLR', 0, 'C');
  FPDF.Cell(25, 3, FACBrTitulo.CARTEIRA, 'BR', 0, 'C');
  FPDF.Cell(15, 3, FACBrTitulo.EspecieMod, 'BR', 0, 'C');
  FPDF.Cell(35, 3, '', 'BR', 0, 'C');
  FPDF.Cell(27, 3, '', 'BR', 0, 'C');
  FPDF.Cell(60, 3, FormatFloatBr(FACBrTitulo.ValorDocumento), 'BR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130, 3, INSTRUCOES_PAGAMENTO, 'L', 0, 'L');
  FPDF.Cell(60, 3, DESCONTO_ABATIMENTO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 3, Copy(FACBrTitulo.Mensagem.Text, 1, 86), 'L', 0, 'L');
  FPDF.Cell(60, 3, '', 'LBR', 1, 'R');

  FPDF.Cell(130, 3, Copy(FACBrTitulo.Mensagem.Text, 87, 86), 'LR', 0, 'L');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(60, 3, JUROS_MULTA, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130, 3, Copy(FACBrTitulo.Mensagem.Text, 174, 86), 'LR', 0, 'L');
  FPDF.Cell(60, 3, '', 'LBR', 1, 'R');
  FPDF.Cell(130, 3, Copy(FACBrTitulo.Mensagem.Text, 261, 86), 'LR', 0, 'L');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(60, 3, VALOR_PAGO, 'LR', 1, 'L');
  FPDF.SetFont('arial', '', 7);
  FPDF.Cell(130, 3, '', 'LBR', 0, 'L');
  FPDF.Cell(60, 3, '', 'LBR', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_PAGADOR, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.NomeSacado + ' ' + FACBrTitulo.Sacado.CNPJCPF, 'LR', 1, 'L');
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Complemento, 'LR', 1, 'L');
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Bairro + ' ' + FACBrTitulo.Sacado.Cidade + ' ' + FACBrTitulo.Sacado.UF + ' ' + FACBrTitulo.Sacado.CEP, 'LR', 1, 'L');
  FPDF.Cell(190, 3, BENFICIARIO_FINAL + ': ' + FACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 'BLR', 1, 'L');

  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 3, AUTENTICACAO_MECANICA + ' - ' + FICHA_COMPENSACAO, '', 1, 'R');

  FPDF.CodeI25(FCodigoBarras, FPDF.GetX, FPDF.GetY, 15, 1);

  if (Trim(SoftwareHouse) <> '') then
    ModeloEstruturaSoftwareHouse;

  if AEspacoDepois > 0 then
    FPDF.Ln(AEspacoDepois);
end;

procedure TACBrBoletoFPDF.ModeloEstruturaLinhaPontinhada(const AEspacoAntes: Double;
  AEspacoDepois: Double);
begin
  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);

  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 2, CORTE_NA_LINHA, '', 1, 'R');
  FPDF.SetFont('arial', '', 12);
  FPDF.Cell(190, 2, '--------------------------------------------------------------------------------------------------------------------', '', 1, 'C');

  if AEspacoDepois > 0 then
    FPDF.Ln(AEspacoDepois);
end;

procedure TACBrBoletoFPDF.ModeloEstruturaReciboEntrega(const AEspacoAntes: Double;  AEspacoDepois: Double; ALinhaDigitavel :Boolean);
var LArquivoLogo : String;
begin
  LArquivoLogo := ChangeFileExt(ArquivoLogo,'.png');
  
  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);

  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, FPDF.GetX, FPDF.GetY - 5, 40, 10);
    
  FPDF.SetFont('arial', 'B', 6);

  if ALinhaDigitavel then
    FPDF.Cell(190, 2, COMPROVANTE_ENTREGA, '', 1, 'R');

  FPDF.SetFont('arial', '', 9);
  FPDF.Cell(50, 3, '', 'B', 0, 'L');
  

  FPDF.SetFont('arial', 'B', 14);
  FPDF.Cell(20, 3, FBanco, 'LBR', 0, 'C');
  FPDF.SetFont('arial', 'B', 10);

  if ALinhaDigitavel then
   FPDF.Cell(120, 3, FLinhaDigitavel, 'B', 1, 'R')
  else
   FPDF.Cell(120, 3, COMPROVANTE_ENTREGA, 'B', 1, 'R');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(149, 3, NOME_BENEFICIARIO, 'LR', 0, 'L');


  FPDF.SetFont('arial', 'B', 5);
  FPDF.Cell(41, 3, MOTIVO_NAO_ENTREGA, 'R', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(149, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.Nome, 1, 86), 'LR', 0, 'L');
  FPDF.SetFont('arial', 'B', 5);
  FPDF.Cell(16.25, 3, MOTIVO_MUDOU, '', 0, 'L');
  FPDF.Cell(24.75, 3, MOTIVO_FALECIDO, 'R', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(149, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.CNPJCPF, 1, 86), 'LR', 0, 'L');
  FPDF.SetFont('arial', 'B', 5);
  FPDF.Cell(16.25, 3, MOTIVO_RECUSADO, '', 0, 'L');
  FPDF.Cell(24.75, 3, MOTIVO_NUM_NAO_EXISTE, 'R', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(109, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.Logradouro, 1, 86), 'LR', 0, 'L');
  FPDF.Cell(40, 3, AGENCIA_CODIGO_BENEFICIARIO, 'LRT', 0, 'C');
  FPDF.SetFont('arial', 'B', 5);
  FPDF.Cell(16.25, 3, MOTIVO_DESCONHECIDO, '', 0, 'L');
  FPDF.Cell(24.75, 3, MOTIVO_END_INSUFICIENTE, 'R', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(109, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.Cidade
                         + ' '
                         + FACBrTitulo.ACBrBoleto.Cedente.UF
                         + ' '
                         + FACBrTitulo.ACBrBoleto.Cedente.CEP, 1, 86), 'LRB', 0, 'L');
  FPDF.SetFont('arial', 'B', 6);

  FPDF.Cell(40, 3, FBeneficiarioCodigo, 'LRB', 0, 'C');

  FPDF.SetFont('arial', 'B', 5);
  FPDF.Cell(16.25, 3, MOTIVO_AUSENTE, '', 0, 'L');
  FPDF.Cell(24.75, 3, MOTIVO_OUTROS, 'R', 1, 'L');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(23.75, 3, NOSSO_NUMERO, 'LR', 0, 'C');
  FPDF.Cell(23.75, 3, NR_DOCUMENTO, 'R', 0, 'C');
  FPDF.Cell(23.75, 3, DATA_VENCIMENTO, 'R', 0, 'C');
  FPDF.Cell(23.75, 3, VALOR_DOCUMENTO, 'L', 0, 'C');
  FPDF.Cell(25.75, 3, DATA_RECEBIMENTO, 'LR', 0, 'C');
  FPDF.Cell(28.25, 3, ASSINATURA, 'LR', 0, 'C');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(20.50, 3, DATA_RECUSA, 'LRT', 0, 'C');
  FPDF.Cell(20.50, 3, ASSINATURA, 'LRT', 1, 'C');
  //FPDF.SetFont('arial', 'B', 5);
  //FPDF.Cell(41, 3, MOTIVO_NAO_PROCURADO, 'R', 1, 'L');


  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(23.75, 3, FACBrTitulo.NossoNumero, 'BLR', 0, 'C');
  FPDF.Cell(23.75, 3, FACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(23.75, 3, DateToStr(FACBrTitulo.Vencimento), 'BR', 0, 'C');
  FPDF.Cell(23.75, 3, FormatFloatBr(FACBrTitulo.ValorDocumento), 'BL', 0, 'C');
  FPDF.Cell(25.75, 3, DATA_PREENCHER, 'BLR', 0, 'C');
  FPDF.Cell(28.25, 3, '', 'BLR', 0, 'C');

  FPDF.Cell(20.50, 3, DATA_PREENCHER, 'BLR', 0, 'C');
  FPDF.Cell(20.50, 3, '', 'BLR', 1, 'C');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_PAGADOR, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.NomeSacado + ' ' + FACBrTitulo.Sacado.CNPJCPF, 'LR', 1, 'L');
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Complemento, 'LR', 1, 'L');
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Bairro + ' ' + FACBrTitulo.Sacado.Cidade + ' ' + FACBrTitulo.Sacado.UF + ' ' + FACBrTitulo.Sacado.CEP, 'LR', 1, 'L');
  FPDF.Cell(190, 3, BENFICIARIO_FINAL + ': ' + FACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 'BLR', 1, 'L');


  if AEspacoDepois > 0 then
    FPDF.Ln(AEspacoDepois);
end;

procedure TACBrBoletoFPDF.ModeloEstruturaReciboPagador(const AEspacoAntes: Double;
  AEspacoDepois: Double);
var LArquivoLogo : String;
begin
  LArquivoLogo := ChangeFileExt(ArquivoLogo,'.png');

  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);

  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, FPDF.GetX, FPDF.GetY, 40, 10);
      
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 2, RECIBO_PAGADOR, '', 1, 'R');
  FPDF.SetFont('arial', '', 9);
  FPDF.Cell(50, 10, '', 'B', 0, 'L');

  FPDF.SetFont('arial', 'B', 14);
  FPDF.Cell(20, 10, FBanco, 'LBR', 0, 'C');
  FPDF.SetFont('arial', 'B', 10);
  FPDF.Cell(120, 10, FLinhaDigitavel, 'B', 1, 'R');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_BENEFICIARIO, 'LR', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Cell(190, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.Nome, 1, 86), 'LR', 1, 'L');
  FPDF.Cell(190, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.CNPJCPF, 1, 86), 'LR', 1, 'L');
  FPDF.Cell(190, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.Logradouro, 1, 86), 'LR', 1, 'L');
  FPDF.Cell(190, 3, Copy(FACBrTitulo.ACBrBoleto.Cedente.Cidade
                         + ' '
                         + FACBrTitulo.ACBrBoleto.Cedente.UF
                         + ' '
                         + FACBrTitulo.ACBrBoleto.Cedente.CEP, 1, 86), 'LRB', 1, 'L');
  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(38, 3, NOSSO_NUMERO, 'LR', 0, 'L'); 
  FPDF.Cell(38, 3, NR_DOCUMENTO, 'R', 0, 'L');
  FPDF.Cell(38, 3, DATA_VENCIMENTO, 'R', 0, 'L');
  FPDF.Cell(38, 3, VALOR_DOCUMENTO, 'L', 0, 'L');
  FPDF.Cell(38, 3, VALOR_PAGO, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(38, 5, FACBrTitulo.NossoNumero, 'BLR', 0, 'C');
  FPDF.Cell(38, 5, FACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(38, 5, DateToStr(FACBrTitulo.Vencimento), 'BR', 0, 'C');
  FPDF.Cell(38, 5, FormatFloatBr(FACBrTitulo.ValorDocumento), 'BL', 0, 'C');
  FPDF.Cell(38, 5, '', 'BLR', 1, 'C');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190, 3, NOME_PAGADOR, 'LR', 1, 'L');

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.NomeSacado + ' ' + FACBrTitulo.Sacado.CNPJCPF, 'LR', 1, 'L');
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Complemento, 'LR', 1, 'L');
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Bairro + ' ' + FACBrTitulo.Sacado.Cidade + ' ' + FACBrTitulo.Sacado.UF + ' ' + FACBrTitulo.Sacado.CEP, 'LR', 1, 'L');
  FPDF.Cell(190, 3, BENFICIARIO_FINAL + ': ' + FACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 'BLR', 1, 'L');
  FPDF.SetFont('arial', 'B', 6);
  FPDF.Text(FPDF.GetX + 30, FPDF.GetY + 2, AGENCIA_CODIGO_BENEFICIARIO);
  FPDF.Text(FPDF.GetX + 30, FPDF.GetY + 5, FBeneficiarioCodigo);
  FPDF.Text(FPDF.GetX + 140, FPDF.GetY + 2, AUTENTICACAO_MECANICA);
  FPDF.Cell(110, 15, ' ', 'LRB', 0);
  FPDF.Cell(80, 15, ' ', 'LRB', 1);
  if AEspacoDepois > 0 then
    FPDF.Ln(AEspacoDepois);
end;

procedure TACBrBoletoFPDF.ModeloEstruturaReciboPagadorPIX(
  const AEspacoAntes: Double; AEspacoDepois: Double);
var LArquivoLogo : String;
begin
  LArquivoLogo := ChangeFileExt(FACBrTitulo.ArquivoLogoEmp,'.png');
  
  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);
  
  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, FPDF.GetX, FPDF.GetY-2, 50, 10);
  FPDF.Cell(50, 5, '', '', 0, 'L');
  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(140, 5, Copy(FACBrTitulo.ACBrBoleto.Cedente.Nome, 1, 86)
                    + ' - '
                    + CNPJ
                    + ' '
                    +Copy(FACBrTitulo.ACBrBoleto.Cedente.CNPJCPF, 1, 86), 'LR', 1, 'L');
  FPDF.SetFont('arial', '', 7);
  FPDF.Cell(50, 5, '', 'B', 0, 'L');
  FPDF.Cell(140, 5, Copy(FACBrTitulo.ACBrBoleto.Cedente.Cidade
                    + ' '
                    + FACBrTitulo.ACBrBoleto.Cedente.UF
                    + ' '
                    + FACBrTitulo.ACBrBoleto.Cedente.CEP, 1, 86), 'LRB', 1, 'L');
  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190, 5, PAGUE_COM_PIX,'',1,'L');
  FPDF.QRCode(FPDF.GetX + 20, FPDF.GetY,FACBrTitulo.QrCode.emv);
  FPDF.SetX(FPDF.GetX + 50);
  FPDF.SetFont('arial', 'B', 12);
  FPDF.Cell(60, 10, VALOR_DOCUMENTO_LITERAL,'LRT',1,'C');
  FPDF.SetX(FPDF.GetX + 50);
  FPDF.Cell(60, 10, FACBrTitulo.EspecieMod + ' ' + FormatFloatBr(FACBrTitulo.ValorDocumento),'LRB',1,'C');
  FPDF.SetFont('arial', 'B', 7);
  FPDF.Ln(7);
  FPDF.Cell(190, 5, PAGUE_COM_COPIA_COLA,'',1,'L');
  FPDF.SetFont('arial', '', 5);
  FPDF.Cell(190, 3, FACBrTitulo.QrCode.emv,'',1,'L');
  if AEspacoDepois > 0 then
    FPDF.Ln(AEspacoDepois);
end;

procedure TACBrBoletoFPDF.ModeloEstruturaSoftwareHouse;
begin
  FPDF.SetXY(6, FPDF.GetY);
  FPDF.Rotate(90);
  try
    FPDF.SetFont('arial', '', 4);
    FPDF.Cell(100, 5, SoftwareHouse,'',1);
  finally
    FPDF.Rotate(0);
  end;
end;

procedure TACBrBoletoFPDF.ModeloImpressao(const AInicializarArquivo : Boolean);
begin
  case ACBrBoleto.ACBrBoletoFC.LayOut of
    lPadrao         : ModeloBoletoPadrao(AInicializarArquivo);
    lCarne          : ModeloBoletoCarne(AInicializarArquivo);
    lFatura,
    lFaturaDetal,
    lPrestaServicos : ModeloBoletoFatura(AInicializarArquivo);
    //lFaturaDetal    : ModeloBoletoFaturaDetal(AInicializarArquivo);
    //lPrestaServicos : ModeloBoletoPrestaServicos;
    lPadraoEntrega  : ModeloBoletoEntrega(AInicializarArquivo);
    lReciboTopo     : ModeloBoletoReciboTopo(AInicializarArquivo);
    lPadraoEntrega2 : ModeloBoletoEntrega2(AInicializarArquivo);
    lTermica80mm    : ModeloBoletoTermica80mm(AInicializarArquivo);
    lPadraoPIX      : ModeloBoletoPIX(AInicializarArquivo);
    lCarneA5        : ModeloBoletoCarneA5(AInicializarArquivo);
    else
      ModeloBoletoPadrao(AInicializarArquivo);
  end;
  Inc(FNumeroPassadas);
end;

procedure TACBrBoletoFPDF.InicializarArquivo(const AOrientation: TFPDFOrientation; APageUnit: TFPDFUnit; APageFormat: TFPDFPageFormat);
begin
  FPDF := TFPDFExt.Create(AOrientation, APageUnit, APageFormat);
  FPDF.SetUTF8({$IfDef USE_UTF8}True{$Else}False{$EndIf});
  FPDF.SetCompression(True);
  FNumeroPassadas := 0;
end;

procedure TACBrBoletoFPDF.ModeloBoletoCarne(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboPagador();
    ModeloEstruturaLinhaPontinhada();
    ModeloEstruturaFichaPagamento();
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoCarneA5(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA5)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboPagador();//<<<canhoto precisa sair na lateral da ficha de pagamento
    ModeloEstruturaLinhaPontinhada();//<<< precisa ser na vertical
    ModeloEstruturaFichaPagamento();
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoEntrega(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboEntrega(0,0,False);
    ModeloEstruturaLinhaPontinhada(15,15);
    ModeloEstruturaReciboPagador(0,0);
    ModeloEstruturaLinhaPontinhada(15,15);
    ModeloEstruturaFichaPagamento(0,2);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoEntrega2(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboEntrega(0,0,True);
    ModeloEstruturaLinhaPontinhada(13,13);
    ModeloEstruturaReciboPagador(0,0);
    ModeloEstruturaLinhaPontinhada(13,13);
    ModeloEstruturaFichaPagamento(0,2);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoFatura(const AInicializarArquivo : Boolean);
begin
  //modelo não é implementado no fortes...
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4) //no FR é igual o o Fatural Detail do RL não no RL não existe ele faz o Padrão
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaFatura();
    ModeloEstruturaReciboPagador();
    ModeloEstruturaLinhaPontinhada(5,5);
    ModeloEstruturaFichaPagamento();
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoFaturaDetal(const AInicializarArquivo : Boolean);
begin
  //não implementado... usando o Modelo Fatura... no fortes não existia e no fr o modelo fatura é o esse
end;

procedure TACBrBoletoFPDF.ModeloBoletoPadrao(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboPagador(10,0);
    ModeloEstruturaLinhaPontinhada(50,50);
    ModeloEstruturaFichaPagamento(0,2);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoPIX(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboPagadorPIX(5,10);
    ModeloEstruturaReciboPagador(0,0);
    ModeloEstruturaLinhaPontinhada(29,29);
    ModeloEstruturaFichaPagamento(0,0);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoPrestaServicos(const AInicializarArquivo : Boolean);
begin
  //não implementado... usando o Modelo Fatura... no fortes similar a Fatura Detail
  {if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboPagador(); //precisa desenhar um recibo do pagador similar a fatura
    ModeloEstruturaLinhaPontinhada();
    ModeloEstruturaFichaPagamento();
  end;}
end;

procedure TACBrBoletoFPDF.ModeloBoletoReciboTopo(const AInicializarArquivo : Boolean);
var LPassadas : Cardinal;
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    LPassadas := (FNumeroPassadas mod 2);
    if (FPDF.PageNo = 0) or ((FPDF.PageNo > 0) and (LPassadas = 1)) then
      FPDF.AddPage();
    ModeloEstruturaReciboEntrega(0,0);
    ModeloEstruturaLinhaPontinhada(2,3);
    //ModeloEstruturaReciboPagador(0,0);
    //ModeloEstruturaLinhaPontinhada(5,5);
    ModeloEstruturaFichaPagamento(0,5);
    if LPassadas = 1 then
      ModeloEstruturaLinhaPontinhada(5,5);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoTermica80mm(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA5) ///<<< 80mm ??????
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboPagador(); ///usaremos o carnê A5 ou tentaremos
    ModeloEstruturaLinhaPontinhada();
    ModeloEstruturaFichaPagamento();
  end;
end;

end.
