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
  ACBrBoletoFPDFConst,
  ACBrUtil.FPDF;

type
  EACBrBoletoFPDF = class(Exception);

    { TACBrBoletoFPDF }
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(piacbrAllPlatforms) ]
{$ENDIF RTL230_UP}

  TACBrBoletoFPDF = class(TACBrBoletoFCClass)
  private
    FPDF               : TACBrFPDFExt;
    FLinhaDigitavel    : String;
    FBanco             : String;
    FCodigoBarras      : String;
    FBeneficiarioCodigo: String;
    FBeneficiarioNome  : String;
    FNomeArquivo       : String;
    FACBrTitulo        : TACBrTitulo;
    FNumeroPassadas    : Cardinal;
    FNossoNumero       : String;
    FCarteira          : String;
    FMensagem          : TStringList;
    FBoletoIndex       : Boolean;
    FStream            : TStream;
      { Private declarations }
    procedure GeraDados(const AACBrTitulo: TACBrTitulo);
    procedure ImpressaoUnificada;
    Procedure ImpressaoIndividual;
    procedure InicializarArquivo(const AOrientation: TFPDFOrientation = poPortrait; APageUnit: TFPDFUnit = puMM; APageFormat: TFPDFPageFormat = pfA4; APageWidthCustom : Double = 0; APageHeightCustom : Double = 0);
    procedure FinalizarArquivo;
    procedure ModeloImpressao(const AInicializarArquivo : Boolean = False);
    procedure ModeloEstruturaReciboPagador(const AEspacoAntes: Double = 0; AEspacoDepois : Double = 0);
    procedure ModeloEstruturaReciboPagadorPIX(const AEspacoAntes: Double = 0; AEspacoDepois : Double = 0);
    procedure ModeloEstruturaReciboEntrega(const AEspacoAntes: Double = 0;  AEspacoDepois : Double = 0; ALinhaDigitavel :Boolean = True);
    procedure ModeloEstruturaFichaPagamento(const AEspacoAntes: Double = 0;  AEspacoDepois : Double = 0; ACanhoto : Boolean = False; ABobina : Boolean = False);
    procedure ModeloEstruturaLinhaPontinhada(const AEspacoAntes: Double = 35; AEspacoDepois : Double = 20);
    procedure ModeloEstruturaFatura(const AEspacoAntes: Double = 0; AEspacoDepois : Double = 0);
    procedure ModeloEstruturaSoftwareHouse;
    function  MensagemInstrucaoPagamento(const AMensagem :String; const ASize : Byte = 103 ) : String;

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
    function GerarPDF(AIndex: Integer) : string; override;
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
    FPDF.SetProtection( [canPrint, canCopy], LSenhaPDF, LSenhaPDF);

  LPath := IncludeTrailingPathDelimiter(ExtractFilePath(Self.NomeArquivo));
  LFile := ChangeFileExt(ExtractFileName(Self.NomeArquivo),'');

  if LFile <> '' then
    LFile := LFile+'_';

  if Assigned(FStream) then
    FPDF.SaveToStream(FStream)
  else
  if Self.CalcularNomeArquivoPDFIndividual then
    FPDF.SaveToFile( Self.NomeArquivo )
  else
    FPDF.SaveToFile(ChangeFileExt(Self.NomeArquivo, '.pdf'));

  FPDF.Free;
end;

procedure TACBrBoletoFPDF.GeraDados(const AACBrTitulo: TACBrTitulo);
begin
  FMensagem           := TStringList.Create;
  try
    FCodigoBarras       := ACBrBoleto.Banco.MontarCodigoBarras(AACBrTitulo);
    FLinhaDigitavel     := ACBrBoleto.Banco.MontarLinhaDigitavel(FCodigoBarras, AACBrTitulo);
    //FBeneficiarioCodigo := AACBrTitulo.ACBrBoleto.Cedente.Agencia + ' / ' + AACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
    FBeneficiarioCodigo := ACBrBoleto.Banco.MontarCampoCodigoCedente(AACBrTitulo);
    FNossoNumero        := ACBrBoleto.Banco.MontarCampoNossoNumero(AACBrTitulo);
    FCarteira           := ACBrBoleto.Banco.MontarCampoCarteira(AACBrTitulo);

    FBeneficiarioNome := Copy(AACBrTitulo.ACBrBoleto.Cedente.Nome + ' ' + AACBrTitulo.ACBrBoleto.Cedente.CNPJCPF + ' ' + AACBrTitulo.ACBrBoleto.Cedente.Logradouro + ' ' +
        AACBrTitulo.ACBrBoleto.Cedente.Cidade + ' ' + AACBrTitulo.ACBrBoleto.Cedente.UF, 1, 86);

    FBanco := FormatFloat('000', AACBrTitulo.ACBrBoleto.Banco.Numero) + '-' + IfThen(AACBrTitulo.ACBrBoleto.Banco.Digito >= 10, 'X',
      IntToStrZero(AACBrTitulo.ACBrBoleto.Banco.Digito, 1));

    FACBrTitulo := AACBrTitulo;
    FMensagem.Text := FACBrTitulo.Mensagem.Text;
    ACBrBoleto.AdicionarMensagensPadroes(FACBrTitulo,FMensagem);
    ModeloImpressao;
  finally
    FMensagem.Free;
  end;
end;

function TACBrBoletoFPDF.GerarPDF(AIndex: Integer) : string;
begin
  FBoletoIndex := True;
  Result := inherited GerarPDF(AIndex);
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
  I, LIndex: Integer;
begin

  for I := 0 to Pred(ACBrBoleto.ListadeBoletos.Count) do
  begin
    ModeloImpressao(True);
    try
      if Self.IndiceImprimirIndividual >= 0 then
        LIndex := Self.IndiceImprimirIndividual
      else
        LIndex := I;

      GeraDados(ACBrBoleto.ListadeBoletos[ LIndex ]);
      if EstaVazio(FNomeArquivo) or (ExtractFileName(Self.NomeArquivo) = 'boleto') then
        FNomeArquivo := OnlyAlphaNum(FNossoNumero);
      if FBoletoIndex then
        Break;
    finally
      FinalizarArquivo;
      FNomeArquivo := '';
    end;
  end;
end;

procedure TACBrBoletoFPDF.ImpressaoUnificada;
var
  I: Integer;
begin
  ModeloImpressao(true);
  try
    if Self.IndiceImprimirIndividual >= 0 then
      GeraDados(ACBrBoleto.ListadeBoletos[ Self.IndiceImprimirIndividual ])
    else
    for I := 0 to Pred(ACBrBoleto.ListadeBoletos.Count) do
      GeraDados(ACBrBoleto.ListadeBoletos[ I ]);
  finally
    FinalizarArquivo;
    FNomeArquivo := '';
  end;
end;

procedure TACBrBoletoFPDF.Imprimir(AStream: TStream);
begin
  FStream := AStream;
  Imprimir;
end;

procedure TACBrBoletoFPDF.ModeloEstruturaFatura(const AEspacoAntes: Double;
  AEspacoDepois: Double);
var LArquivoLogo : String;
    LDetalhamento : TStringList;
    I,IndexDetalhamento: Integer;
begin
  LArquivoLogo := IfThen( LowerCase(ExtractFileExt(FACBrTitulo.ArquivoLogoEmp)) = 'jpg',
    FACBrTitulo.ArquivoLogoEmp,
    ChangeFileExt(FACBrTitulo.ArquivoLogoEmp,'.png')
  );

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
  FPDF.Cell(31.67, 3, FNossoNumero, 'BLR', 0, 'C');
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
       //LDetalhamento.AddStrings(FACBrTitulo.Detalhamento.Strings[I])
       LDetalhamento.Add(FACBrTitulo.Detalhamento.Strings[I])
     else
       LDetalhamento.Add('');

    FPDF.MultiCell(190,2,LDetalhamento.Text,'','J');
  finally
    LDetalhamento.Free;
  end;
 
end;

procedure TACBrBoletoFPDF.ModeloEstruturaFichaPagamento(const AEspacoAntes: Double;
  AEspacoDepois     : Double; ACanhoto : Boolean; ABobina : Boolean);
var LArquivoLogo  : String;
  LReducaoCanhoto,
  LReducaoEMV  : Cardinal;
  LAlturaPadraoBaixo,
  LAlturaPadraoAlto : Double;
  LMensagem : String;
begin
  LReducaoEMV := 0;
  if (FACBrTitulo.QrCode.emv <> '') and (FACBrTitulo.ACBrBoleto.ACBrBoletoFC.LayOut <> lPadraoPIX) then
    LReducaoEMV := 15;

  LReducaoCanhoto := 0;
  if ACanhoto then
    LReducaoCanhoto := 25;

  LArquivoLogo := ChangeFileExt(ArquivoLogo,'.png');
  if AEspacoAntes > 0 then
    FPDF.Ln(AEspacoAntes);

  LAlturaPadraoBaixo := 3;
  LAlturaPadraoAlto  := 8;
  if ABobina then
  begin
    LAlturaPadraoBaixo := LAlturaPadraoBaixo - 0.6;
    LAlturaPadraoAlto  := LAlturaPadraoAlto - 0.12;
  end;

  FPDF.SetFont('arial', '', 5);
  if ACanhoto then
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), 8, RECIBO_PAGADOR, 'B', 0, 'L');

  if FileExists(LArquivoLogo) then
    FPDF.Image(LArquivoLogo, FPDF.GetX, FPDF.GetY-2, 40, 10);

  FPDF.Cell(50, LAlturaPadraoAlto, '', 'B', 0, 'L');

  FPDF.SetFont('arial', 'B', 14);
  FPDF.Cell(20 - (LReducaoCanhoto/4), LAlturaPadraoAlto, FBanco, 'LBR', 0, 'C');

  FPDF.SetFont('arial', 'B', StrToFloat(IfThen(ACanhoto,'8','9')));
  FPDF.Cell(120 - LReducaoCanhoto - (LReducaoCanhoto/4), LAlturaPadraoAlto, FLinhaDigitavel, 'B', 1, 'R');


  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, PARCELA, 'R', 0, 'C');
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, DATA_VENCIMENTO, '', 0, 'C');
  end;

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, LOCAL_PAGAMENTO, 'LR', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, DATA_VENCIMENTO, 'R', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, FormatFloat('000',FACBrTitulo.Parcela) + ' / ' +FormatFloat('000',FACBrTitulo.TotalParcelas), 'BR', 0, 'C');
    FPDF.SetFont('arial', 'B', 5);
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo,  DateToStr(FACBrTitulo.Vencimento), 'B', 0, 'C');
  end;

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, FACBrTitulo.LocalPagamento, 'BLR', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, DateToStr(FACBrTitulo.Vencimento), 'BR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, AGENCIA_CODIGO_BENEFICIARIO, '', 0, 'C');

  end;

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, NOME_BENEFICIARIO, 'LR', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, AGENCIA_CODIGO_BENEFICIARIO, 'R', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, FBeneficiarioCodigo, 'B', 0, 'C');
  end;

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, FBeneficiarioNome, 'BLR', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, FBeneficiarioCodigo, 'BR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, ESPECIE, 'BR', 0, 'C');
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, QUANTIDADE, 'B', 0, 'C');
  end;

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(28, LAlturaPadraoBaixo, DATA_DOCUMENTO, 'LR', 0, 'L');
  FPDF.Cell(40 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, NR_DOCUMENTO, 'R', 0, 'L');
  FPDF.Cell(20, LAlturaPadraoBaixo, ESPECIE_DOC, 'R', 0, 'L');
  FPDF.Cell(15, LAlturaPadraoBaixo, ACEITE, 'R', 0, 'L');
  FPDF.Cell(27, LAlturaPadraoBaixo, DATA_PROCESSAMENTO, '', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, NOSSO_NUMERO, 'LR', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, FACBrTitulo.EspecieMod, 'BR', 0, 'C');
    FPDF.Cell((LReducaoCanhoto + (LReducaoCanhoto / 2))/2, LAlturaPadraoBaixo, IntToStr(FACBrTitulo.TotalParcelas), 'B', 0, 'C');
  end;

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(28, LAlturaPadraoBaixo, DateToStr(FACBrTitulo.DataDocumento), 'BLR', 0, 'C');
  FPDF.Cell(40 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, FACBrTitulo.NumeroDocumento, 'BR', 0, 'C');
  FPDF.Cell(20, LAlturaPadraoBaixo, FACBrTitulo.EspecieDoc, 'BR', 0, 'C');
  FPDF.Cell(15, LAlturaPadraoBaixo, IfThen(FACBrTitulo.ACEITE = atSim, 'Sim', 'Não'), 'BR', 0, 'C');
  FPDF.Cell(27, LAlturaPadraoBaixo, DateToStr(FACBrTitulo.DataProcessamento), 'BR', 0, 'C');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, FNossoNumero, 'BR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, VALOR_DOCUMENTO, '', 0, 'L');
  end;

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(28, LAlturaPadraoBaixo, USO_BANCO, 'LR', 0, 'L');
  FPDF.Cell(25 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, CARTEIRA, 'R', 0, 'L');
  FPDF.Cell(15, LAlturaPadraoBaixo, ESPECIE, 'R', 0, 'L');
  FPDF.Cell(35, LAlturaPadraoBaixo, QUANTIDADE, 'R', 0, 'L');
  FPDF.Cell(27, LAlturaPadraoBaixo, XVALOR, '', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, VALOR_DOCUMENTO, 'LR', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', 'B', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, FACBrTitulo.EspecieMod + ' ' + FormatFloatBr(FACBrTitulo.ValorDocumento), 'B', 0, 'R');
  end;

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(28, LAlturaPadraoBaixo, FACBrTitulo.UsoBanco, 'BLR', 0, 'C');
  FPDF.Cell(25- (LReducaoCanhoto/2), LAlturaPadraoBaixo, FCarteira, 'BR', 0, 'C');
  FPDF.Cell(15, LAlturaPadraoBaixo, FACBrTitulo.EspecieMod, 'BR', 0, 'C');
  FPDF.Cell(35, LAlturaPadraoBaixo, '', 'BR', 0, 'C');
  FPDF.Cell(27, LAlturaPadraoBaixo, '', 'BR', 0, 'C');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, FormatFloatBr(FACBrTitulo.ValorDocumento), 'BR', 1, 'R');
  if (FACBrTitulo.ACBrBoleto.ACBrBoletoFC.LayOut <> lPadraoPIX) and (ABobina = false)  then
    FPDF.QRCode(FPDF.GetX + 111 + LReducaoCanhoto, FPDF.GetY,FACBrTitulo.QrCode.emv, 0.35);

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, DESCONTO_ABATIMENTO, 'B', 0, 'L');
  end;

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, INSTRUCOES_PAGAMENTO, 'L', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, DESCONTO_ABATIMENTO, 'LR', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, JUROS_MULTA, 'B', 0, 'L');
  end;

  FPDF.SetFont('arial', 'B', 7);

  if FMensagem.Count >= 1 then
    LMensagem := MensagemInstrucaoPagamento(FMensagem[0], 103 - LReducaoEMV)
  else
    LMensagem := '';

  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, LMensagem, 'L', 0, 'L');
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, '', 'LBR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, VALOR_PAGO, 'B', 0, 'L');
  end;
  FPDF.SetFont('arial', 'B', 7);

  if FMensagem.Count >= 2 then
    LMensagem := MensagemInstrucaoPagamento(FMensagem[1], 103 - LReducaoEMV)
  else
    LMensagem := '';


  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, LMensagem, 'LR', 0, 'L');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, JUROS_MULTA, 'LR', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, NOME_BENEFICIARIO, '', 0, 'L');
  end;

  FPDF.SetFont('arial', 'B', 7);

  if FMensagem.Count >= 3 then
    LMensagem := MensagemInstrucaoPagamento(FMensagem[2], 103 - LReducaoEMV)
  else
    LMensagem := '';

  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, LMensagem, 'LR', 0, 'L');

  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, '', 'LBR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(FBeneficiarioNome,1,36), '', 0, 'L');
  end;
  FPDF.SetFont('arial', 'B', 7);

  if FMensagem.Count >= 4 then
    LMensagem := MensagemInstrucaoPagamento(FMensagem[3], 103 - LReducaoEMV)
  else
    LMensagem := '';

  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo,LMensagem, 'LR', 0, 'L');

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, VALOR_PAGO, 'LR', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(FBeneficiarioNome,37,36), '', 0, 'L');
  end;

  FPDF.SetFont('arial', 'B', 7);

  if FMensagem.Count >= 5 then
    LMensagem := MensagemInstrucaoPagamento(FMensagem[4], 103 - LReducaoEMV)
  else
    LMensagem := '';

  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, LMensagem, 'L', 0, 'L');

  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, '', 'LR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(FBeneficiarioNome,73,36), '', 0, 'L');
  end;

  FPDF.SetFont('arial', 'B', 7);

  if FMensagem.Count >= 6 then
    LMensagem := MensagemInstrucaoPagamento(FMensagem[5], 103 - LReducaoEMV)
  else
    LMensagem := '';

  FPDF.Cell(130 - (LReducaoCanhoto/2), LAlturaPadraoBaixo, LMensagem, 'LBR', 0, 'L');

  FPDF.Cell(60 - LReducaoCanhoto, LAlturaPadraoBaixo, '', 'LBR', 1, 'R');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(NOME_PAGADOR,1,37), 'T', 0, 'L');
  end;

  FPDF.SetFont('arial', '', 6);
  FPDF.Cell(190 - LReducaoCanhoto - (LReducaoCanhoto/2), LAlturaPadraoBaixo, NOME_PAGADOR, 'LR', 1, 'L');

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(FACBrTitulo.Sacado.NomeSacado + ' ' + FACBrTitulo.Sacado.CNPJCPF,1,37), '', 0, 'L');
  end;

  FPDF.SetFont('arial', 'B', 7);
  FPDF.Cell(190 - LReducaoCanhoto - (LReducaoCanhoto/2), LAlturaPadraoBaixo, FACBrTitulo.Sacado.NomeSacado + ' ' + FACBrTitulo.Sacado.CNPJCPF, 'LR', 1, 'L');
  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 5);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoAlto, Copy(FACBrTitulo.Sacado.NomeSacado + ' ' + FACBrTitulo.Sacado.CNPJCPF,37,37), 'B', 0, 'L');

  end;

  FPDF.Cell(190 - LReducaoCanhoto - (LReducaoCanhoto/2), LAlturaPadraoBaixo, FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Numero+ ' ' + FACBrTitulo.Sacado.Complemento, 'LR', 1, 'L');

  if ACanhoto then
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Numero+ ' ' + FACBrTitulo.Sacado.Complemento,1,37), '', 0, 'L');

  FPDF.Cell(190 - LReducaoCanhoto - (LReducaoCanhoto/2), LAlturaPadraoBaixo, FACBrTitulo.Sacado.Bairro + ' ' + FACBrTitulo.Sacado.Cidade + ' ' + FACBrTitulo.Sacado.UF + ' ' + FACBrTitulo.Sacado.CEP, 'LR', 1, 'L');

  if ACanhoto then
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, Copy(FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Numero+ ' ' + FACBrTitulo.Sacado.Complemento,37,37), '', 0, 'L');

  FPDF.Cell(190 - LReducaoCanhoto - (LReducaoCanhoto/2), LAlturaPadraoBaixo, BENFICIARIO_FINAL + ': ' + FACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 'BLR', 1, 'L');

  FPDF.SetFont('arial', 'B', StrToFloat(IfThen(ACanhoto,'4','6')));

  if ACanhoto then
  begin
    FPDF.SetFont('arial', '', 4);
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, AUTENTICACAO_MECANICA, '', 0, 'L');
  end;

  FPDF.Cell(190 - LReducaoCanhoto - (LReducaoCanhoto/2), LAlturaPadraoBaixo, AUTENTICACAO_MECANICA + ' - ' + FICHA_COMPENSACAO, '', 1, 'R');

  if ACanhoto then
    FPDF.Cell(LReducaoCanhoto + (LReducaoCanhoto / 2), LAlturaPadraoBaixo, '', '', 0, 'L');


  FPDF.CodeI25(FCodigoBarras, FPDF.GetX, FPDF.GetY, 13, StrToFloat(IfThen(ACanhoto,IfThen(ABobina,'1,1','0,8'),'1')));

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
  FPDF.Cell(23.75, 3, FNossoNumero, 'BLR', 0, 'C');
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
  FPDF.Cell(190, 3, FACBrTitulo.Sacado.Logradouro + ' ' + FACBrTitulo.Sacado.Complemento + ' ' +
                    FACBrTitulo.Sacado.Bairro + ' ' + FACBrTitulo.Sacado.Cidade + ' ' + FACBrTitulo.Sacado.UF + ' ' + FACBrTitulo.Sacado.CEP, 'LR', 1, 'L');
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
  FPDF.Cell(38, 5, FNossoNumero, 'BLR', 0, 'C');
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
  LArquivoLogo := IfThen( LowerCase(ExtractFileExt(FACBrTitulo.ArquivoLogoEmp)) = 'jpg',
    FACBrTitulo.ArquivoLogoEmp,
    ChangeFileExt(FACBrTitulo.ArquivoLogoEmp,'.png')
  );
  
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

procedure TACBrBoletoFPDF.InicializarArquivo(const AOrientation: TFPDFOrientation; APageUnit: TFPDFUnit; APageFormat: TFPDFPageFormat; APageWidthCustom : Double; APageHeightCustom : Double);
var LPage : TFPDFPageSize;
begin
  if (APageWidthCustom = 0) and (APageHeightCustom = 0) then
    FPDF := TACBrFPDFExt.Create(AOrientation, APageUnit, APageFormat)
  else
  begin
    LPage.w := APageWidthCustom;
    LPage.h := APageHeightCustom;
    FPDF := TACBrFPDFExt.Create(AOrientation, APageUnit, LPage);
    FPDF.SetMargins(0,0,0);
  end;
  FPDF.SetUTF8(False);
  FPDF.SetCompression(True);
  FNumeroPassadas := 0;
end;

function TACBrBoletoFPDF.MensagemInstrucaoPagamento(const AMensagem: String;
  const ASize: Byte): String;
begin
  result := Copy(AMensagem,0,ASize);
end;

procedure TACBrBoletoFPDF.ModeloBoletoCarne(const AInicializarArquivo : Boolean);
var
  LPassadas : Cardinal;
begin
  if AInicializarArquivo then
    InicializarArquivo(poPortrait, puMM, pfA4)
  else
  begin
    LPassadas := (FNumeroPassadas mod 3);
    if (FPDF.PageNo = 0) or ((FPDF.PageNo > 0) and (LPassadas = 1)) then
      FPDF.AddPage();
    ModeloEstruturaFichaPagamento(0,5,True);
    if not (LPassadas = 0) then
      ModeloEstruturaLinhaPontinhada(6,5);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoCarneA5(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poLandscape, puMM, pfA5)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaReciboEntrega(0,0);
    ModeloEstruturaLinhaPontinhada(0,2);
    ModeloEstruturaFichaPagamento(0,0,True);
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
    ModeloEstruturaFichaPagamento(0,5);
    if LPassadas = 1 then
      ModeloEstruturaLinhaPontinhada(10,5);
  end;
end;

procedure TACBrBoletoFPDF.ModeloBoletoTermica80mm(const AInicializarArquivo : Boolean);
begin
  if AInicializarArquivo then
    InicializarArquivo(poLandscape, puMM, pfA5, 80, 200)
  else
  begin
    FPDF.AddPage();
    ModeloEstruturaFichaPagamento(1,0,True,True);
  end;
end;

end.
