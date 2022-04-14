{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrBoletoFCFR;

interface

uses
  SysUtils, Classes, DB, DBClient, ACBrBase, ACBrBoleto, StrUtils, ACBrBoletoConversao,
  frxClass, frxDBSet, frxBarcode, frxExportHTML, frxExportPDF, frxExportImage;

type
  EACBrBoletoFCFR = class(Exception);

  { TACBrBoletoFCFR }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBoletoFCFR = class(TACBrBoletoFCClass)
  private
    MensagemPadrao: TStringList;
    fFastReportFile: String;
    FImpressora: String;
    fIndice: Integer;
    FModoThread: Boolean;
    FIncorporarFontesPdf: Boolean;
    FIncorporarBackgroundPdf: Boolean;
    FCustomPreview: TfrxCustomPreview;
    FfrxReport: TfrxReport;
    FfrxHTMLExport: TfrxHTMLExport;
    FfrxJPEGExport: TfrxJPEGExport;
    FfrxPDFExport: TfrxPDFExport;
    FfrxBarCodeObject: TfrxBarCodeObject;
    FcdsBanco: TClientDataSet;
    FfrxBanco: TfrxDBDataset;
    FcdsCedente: TClientDataSet;
    FfrxCedente: TfrxDBDataset;
    FcdsTitulo: TClientDataSet;
    FfrxTitulo: TfrxDBDataset;
    FUsarConfiguracoesACBr : Boolean;
    function PrepareBoletos: Boolean;
    function PreparaRelatorio: Boolean;
    function GetACBrTitulo: TACBrTitulo;
    procedure SetCustomPreview(const Value: TfrxCustomPreview);
    procedure ImprimeLogoMarca(const sfrxPicture: string);
    procedure SetDataSetsToFrxReport;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
    procedure frxReportProgressStart(Sender: TfrxReport;
      ProgressType: TfrxProgressType; Progress: Integer);

  public
    { Public declarations }
    property Report     : TfrxReport     read FfrxReport;
    property HTMLExport : TfrxHTMLExport read FfrxHTMLExport;
    property JPEGExport : TfrxJPEGExport read FfrxJPEGExport;
    property PDFExport  : TfrxPDFExport  read FfrxPDFExport;


    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Imprimir; override;
    procedure Imprimir(AStream: TStream); override;
    function CarregaFastReportFile: Boolean;
    function PreparedReport: TfrxReport;
    property Titulo: TACBrTitulo read GetACBrTitulo;

  published
    property FastReportFile: String read fFastReportFile write fFastReportFile;
    property Impressora: String read FImpressora write FImpressora;
    property ModoThread: Boolean read FModoThread write FModoThread;
    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
    property IncorporarFontesPdf: Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;
    property CustomPreview: TfrxCustomPreview read FCustomPreview write SetCustomPreview;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrImage, ACBrBancoBanestes, ACBrDelphiZXingQRCode;

{ TdmACBrBoletoFCFR }

procedure TACBrBoletoFCFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var emvQrCode: String;
begin
  emvQrCode := Trim(FcdsTitulo.FieldByName('EMV').AsString);

  if Assigned(Sender) and (Sender.Name = 'ImgEmvQrcode') then
  begin
    TfrxPictureView(Sender).Visible := not (emvQrCode = '');
    if (emvQrCode <> '') then
      PintarQRCode(emvQrCode, TfrxPictureView(Sender).Picture.Bitmap, qrAuto);
  end;
end;

procedure TACBrBoletoFCFR.frxReportProgressStart(Sender: TfrxReport;
  ProgressType: TfrxProgressType; Progress: Integer);
begin
  ImprimeLogoMarca('Logo_1');
  ImprimeLogoMarca('Logo_2');
  ImprimeLogoMarca('Logo_3');
end;

function TACBrBoletoFCFR.GetACBrTitulo: TACBrTitulo;
begin
  Result := fACBrBoleto.ListadeBoletos[fIndice];
end;

procedure TACBrBoletoFCFR.ImprimeLogoMarca(const sfrxPicture: string);
var
  frxPict: TfrxPictureView; // Componente para inserção de imagem na impressão.
begin
  frxPict := TfrxPictureView(FfrxReport.FindObject(sfrxPicture));
  if Assigned(frxPict) then
    CarregaLogo(frxPict.Picture, ACBrBoleto.Banco.Numero);
end;

procedure TACBrBoletoFCFR.Imprimir(AStream: TStream);
var FiltroAtual : TACBrBoletoFCFiltro;
begin
  inherited;
  FiltroAtual := filtro;
  try
    Filtro := fiPDF;
    if Filtro <> fiPDF then
      raise Exception.Create(ACBrStr('Impressão por Stream apenas para o Filtro PDF.'));

    if (AStream <> nil) then
      FfrxPDFExport.Stream := AStream;

    Imprimir;
  finally
    Filtro := FiltroAtual;
  end;

end;

procedure TACBrBoletoFCFR.SetDataSetsToFrxReport;
begin
  FfrxReport.EnabledDataSets.Clear;
  FfrxReport.EnabledDataSets.Add(FfrxBanco);
  FfrxReport.EnabledDataSets.Add(FfrxTitulo);
  FfrxReport.EnabledDataSets.Add(FfrxCedente);
end;

{ TACBrBoletoFCFR }
constructor TACBrBoletoFCFR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFastReportFile := '';
  FImpressora := '';
  fIndice := 0;
  FModoThread := False;
  FIncorporarBackgroundPdf := False;
  FIncorporarFontesPdf := False;
  MensagemPadrao  := TStringList.Create;
  FCustomPreview := Nil;

  FfrxReport := TfrxReport.Create(Self);
  FfrxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, {$IFNDEF FMX} pbFind, {$ENDIF}
    pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick];
  FfrxReport.EngineOptions.UseGlobalDataSetList := False;
  FfrxReport.StoreInDFM := False;
  FfrxReport.OnBeforePrint := frxReportBeforePrint;
  FfrxReport.OnProgressStart := frxReportProgressStart;

  FfrxHTMLExport := TfrxHTMLExport.Create(Self);
  FfrxHTMLExport.ShowProgress := False;

  FfrxJPEGExport := TfrxJPEGExport.Create(Self);
  FfrxJPEGExport.ShowProgress := False;

  FfrxPDFExport := TfrxPDFExport.Create(Self);
  FfrxPDFExport.ShowProgress := False;

  RttiSetProp(FfrxPDFExport, 'Transparency', 'False');

  FfrxBarCodeObject := TfrxBarCodeObject.Create(Self);

  // Banco
  FcdsBanco := TClientDataSet.Create(Self);
  FcdsBanco.FieldDefs.Clear;
  FcdsBanco.FieldDefs.Add('Numero', ftString, 20);
  FcdsBanco.FieldDefs.Add('Digito', ftString, 1);
  FcdsBanco.FieldDefs.Add('Nome', ftString, 100);
  FcdsBanco.FieldDefs.Add('DirLogo', ftString, 254);
  FcdsBanco.FieldDefs.Add('OrientacoesBanco', ftString, 254);
  FcdsBanco.FieldDefs.Add('CIP', ftString, 3);
  FcdsBanco.CreateDataSet;

  FfrxBanco := TfrxDBDataset.Create(Self);
  FfrxBanco.DataSet := FcdsBanco;
  FfrxBanco.OpenDataSource := False;
  FfrxBanco.UserName := 'Banco';

  // Cedente
  FcdsCedente := TClientDataSet.Create(Self);
  FcdsCedente.FieldDefs.Clear;
  FcdsCedente.FieldDefs.Add('Nome', ftString, 100);
  FcdsCedente.FieldDefs.Add('CodigoCedente', ftString, 20);
  FcdsCedente.FieldDefs.Add('CodigoTransmissao', ftString, 20);
  FcdsCedente.FieldDefs.Add('Agencia', ftString, 5);
  FcdsCedente.FieldDefs.Add('AgenciaDigito', ftString, 2);
  FcdsCedente.FieldDefs.Add('Conta', ftString, 20);
  FcdsCedente.FieldDefs.Add('ContaDigito', ftString, 2);
  FcdsCedente.FieldDefs.Add('Modalidade', ftString, 20);
  FcdsCedente.FieldDefs.Add('Convenio', ftString, 20);
  FcdsCedente.FieldDefs.Add('ResponEmissao', ftInteger);
  FcdsCedente.FieldDefs.Add('CNPJCPF', ftString, 18);
  FcdsCedente.FieldDefs.Add('TipoInscricao', ftInteger);
  FcdsCedente.FieldDefs.Add('Logradouro', ftString, 100);
  FcdsCedente.FieldDefs.Add('NumeroRes', ftString, 10);
  FcdsCedente.FieldDefs.Add('Complemento', ftString, 100);
  FcdsCedente.FieldDefs.Add('Bairro', ftString, 100);
  FcdsCedente.FieldDefs.Add('Cidade', ftString, 100);
  FcdsCedente.FieldDefs.Add('UF', ftString, 2);
  FcdsCedente.FieldDefs.Add('CEP', ftString, 9);
  FcdsCedente.FieldDefs.Add('Telefone', ftString, 15);
  FcdsCedente.CreateDataSet;

  FfrxCedente := TfrxDBDataset.Create(Self);
  FfrxCedente.DataSet := FcdsCedente;
  FfrxCedente.OpenDataSource := False;
  FfrxCedente.UserName := 'Cedente';

  // Titulo
  FcdsTitulo := TClientDataSet.Create(Self);
  FcdsTitulo.FieldDefs.Clear;
  FcdsTitulo.FieldDefs.Add('NossoNum', ftString, 100);
  FcdsTitulo.FieldDefs.Add('CodCedente', ftString, 100);
  FcdsTitulo.FieldDefs.Add('CodBarras', ftString, 100);
  FcdsTitulo.FieldDefs.Add('LinhaDigitavel', ftString, 100);
  FcdsTitulo.FieldDefs.Add('TipoDoc', ftString, 10);
  FcdsTitulo.FieldDefs.Add('Vencimento', ftDateTime);
  FcdsTitulo.FieldDefs.Add('DataDocumento', ftDateTime);
  FcdsTitulo.FieldDefs.Add('NumeroDocumento', ftString, 20);
  FcdsTitulo.FieldDefs.Add('TotalParcelas', ftInteger);
  FcdsTitulo.FieldDefs.Add('Parcela', ftInteger);
  FcdsTitulo.FieldDefs.Add('EspecieDoc', ftString, 10);
  FcdsTitulo.FieldDefs.Add('EspecieMod', ftString, 10);
  FcdsTitulo.FieldDefs.Add('UsoBanco', ftString, 4);
  FcdsTitulo.FieldDefs.Add('Aceite', ftString,3);
  FcdsTitulo.FieldDefs.Add('DataProcessamento', ftDateTime);
  FcdsTitulo.FieldDefs.Add('NossoNumero', ftString, 20);
  FcdsTitulo.FieldDefs.Add('Carteira', ftString, 20);
  FcdsTitulo.FieldDefs.Add('ValorDocumento', ftBCD, 18);
  FcdsTitulo.FieldDefs.Add('LocalPagamento', ftString, 100);
  FcdsTitulo.FieldDefs.Add('ValorMoraJuros', ftBCD, 18);
  FcdsTitulo.FieldDefs.Add('ValorDesconto', ftBCD, 18);
  FcdsTitulo.FieldDefs.Add('ValorAbatimento', ftBCD, 18);
  FcdsTitulo.FieldDefs.Add('DataMoraJuros', ftDateTime);
  FcdsTitulo.FieldDefs.Add('DataDesconto', ftDateTime);
  FcdsTitulo.FieldDefs.Add('DataAbatimento', ftDateTime);
  FcdsTitulo.FieldDefs.Add('DataProtesto', ftDateTime);
  FcdsTitulo.FieldDefs.Add('PercentualMulta', ftFloat);
  FcdsTitulo.FieldDefs.Add('Mensagem', ftString, 600);
  FcdsTitulo.FieldDefs.Add('OcorrenciaOriginal', ftInteger);
  FcdsTitulo.FieldDefs.Add('Instrucao1', ftString, 300);
  FcdsTitulo.FieldDefs.Add('Instrucao2', ftString, 300);
  FcdsTitulo.FieldDefs.Add('TextoLivre', ftMemo, 2000);
  FcdsTitulo.FieldDefs.Add('Asbace', ftString, 40);
  FcdsTitulo.FieldDefs.Add('EMV', ftString, 500);
  FcdsTitulo.FieldDefs.Add('ArquivoLogoEmp', ftString,300);
    // Sacado
  FcdsTitulo.FieldDefs.Add('Sacado_NomeSacado', ftString, 100);
  FcdsTitulo.FieldDefs.Add('Sacado_CNPJCPF', ftString, 18);
  FcdsTitulo.FieldDefs.Add('Sacado_Logradouro', ftString, 100);
  FcdsTitulo.FieldDefs.Add('Sacado_Complemento', ftString, 100);
  FcdsTitulo.FieldDefs.Add('Sacado_Numero', ftString, 10);
  FcdsTitulo.FieldDefs.Add('Sacado_Bairro', ftString, 100);
  FcdsTitulo.FieldDefs.Add('Sacado_Cidade', ftString, 100);
  FcdsTitulo.FieldDefs.Add('Sacado_UF', ftString, 2);
  FcdsTitulo.FieldDefs.Add('Sacado_CEP', ftString, 9);
  FcdsTitulo.FieldDefs.Add('Sacado_Avalista', ftString, 100);
  FcdsTitulo.FieldDefs.Add('Sacado_Avalista_CNPJCPF', ftString, 18);
  FcdsTitulo.FieldDefs.Add('Sacado_Fone', ftString, 100);
  FcdsTitulo.CreateDataSet;

  FfrxTitulo := TfrxDBDataset.Create(Self);
  FfrxTitulo.DataSet := FcdsTitulo;
  FfrxTitulo.OpenDataSource := False;
  FfrxTitulo.UserName := 'Titulo';

end;

destructor TACBrBoletoFCFR.Destroy;
begin
  MensagemPadrao.Free;
  FfrxReport.Free;
  FfrxPDFExport.Free;
  FfrxHTMLExport.Free;
  FfrxJPEGExport.Free;
  FfrxBarCodeObject.Free;
  FcdsBanco.Free;
  FfrxBanco.Free;
  FcdsCedente.Free;
  FfrxCedente.Free;
  FcdsTitulo.Free;
  FfrxTitulo.Free;
  inherited;
end;

function TACBrBoletoFCFR.PreparedReport: TfrxReport;
begin
  ACBrBoleto.ChecarDadosObrigatorios;
  inherited Imprimir; // Verifica se a lista de boletos está vazia
  FcdsBanco.EmptyDataSet;
  FcdsCedente.EmptyDataSet;
  FcdsTitulo.EmptyDataSet;

  if PreparaRelatorio then
    Result := FfrxReport
  else
    Result := nil;
end;

procedure TACBrBoletoFCFR.SetCustomPreview(const Value: TfrxCustomPreview);
begin
  FCustomPreview := Value;
end;

procedure TACBrBoletoFCFR.Imprimir;
begin
  inherited Imprimir; // Verifica se a lista de boletos está vazia
  FcdsBanco.EmptyDataSet;
  FcdsCedente.EmptyDataSet;
  FcdsTitulo.EmptyDataSet;

  if PreparaRelatorio then
  begin
    FfrxReport.Preview := CustomPreview;
    FfrxReport.PrintOptions.ShowDialog := (MostrarSetup) and (not FModoThread);
    FfrxReport.PrintOptions.Copies := NumCopias;
    if TituloPreview <> '' then
      FfrxReport.ReportOptions.Name := TituloPreview;

    if Length(Impressora) > 0 then
      FfrxReport.PrintOptions.Printer := Impressora;

    case Filtro of
      fiPDF, fiNenhum:
        begin
          if FModoThread then
          begin
            FfrxPDFExport.ShowDialog   := False;
            FfrxPDFExport.ShowProgress := False;
          end
          else
          begin
            FfrxPDFExport.ShowDialog   := MostrarSetup;
            FfrxPDFExport.ShowProgress := MostrarProgresso;
          end;

          FfrxPDFExport.FileName      := NomeArquivo;
          FfrxPDFExport.Author        := SoftwareHouse;
          FfrxPDFExport.Creator       := SoftwareHouse;
          FfrxPDFExport.Producer      := SoftwareHouse;
          FfrxPDFExport.Title         := 'Boleto';
          FfrxPDFExport.Subject       := FfrxPDFExport.Title;
          FfrxPDFExport.Keywords      := FfrxPDFExport.Title;
          FfrxPDFExport.Background    := IncorporarBackgroundPdf;//False diminui 70% do tamanho do pdf
          FfrxPDFExport.EmbeddedFonts := IncorporarFontesPdf;

          

          if NaoEstaVazio(PdfSenha) then
          begin
            FfrxPDFExport.UserPassword    := PdfSenha;
            FfrxPDFExport.ProtectionFlags := [ePrint];
          end else
          begin
            FfrxPDFExport.UserPassword    := '';
            FfrxPDFExport.ProtectionFlags := [];
          end;

          FfrxPDFExport.OwnerPassword   := FfrxPDFExport.UserPassword;

          if Filtro = fiNenhum then
          begin
            if (MostrarPreview) and (not FModoThread) then
              FfrxReport.ShowReport(false)
            else
              FfrxReport.Print;
          end else
            FfrxReport.Export(FfrxPDFExport);

          if FfrxPDFExport.FileName <> NomeArquivo then
            NomeArquivo := FfrxPDFExport.FileName;

        end;
      fiHTML:
        begin
          if EstaVazio(FfrxHTMLExport.FileName)then
            FfrxHTMLExport.FileName     := NomeArquivo;

          FfrxHTMLExport.ShowDialog   := MostrarSetup;
          FfrxHTMLExport.ShowProgress := MostrarSetup;

          FfrxReport.Export(FfrxHTMLExport);
          if FfrxHTMLExport.FileName <> NomeArquivo then
            NomeArquivo := FfrxHTMLExport.FileName;
        end;
      fiJPG:
        begin
          if EstaVazio(FfrxJPEGExport.FileName) then
            FfrxJPEGExport.FileName      := NomeArquivo;

          FfrxJPEGExport.ShowDialog    := False;
          FfrxJPEGExport.ShowProgress  := True;
          FfrxJPEGExport.Monochrome    := True;
          FfrxJPEGExport.SeparateFiles := True;
          FfrxJPEGExport.JPEGQuality   := 200;
          FfrxJPEGExport.Resolution    := 160;
          FfrxReport.Export(FfrxJPEGExport);
          if FfrxJPEGExport.FileName <> NomeArquivo then
            NomeArquivo := FfrxJPEGExport.FileName;
        end;
    else
      exit;
    end;
  end;
end;

function TACBrBoletoFCFR.CarregaFastReportFile: Boolean;
var
	BoletoStreamFR3: TStringStream;
begin
  Result := False;
  if Trim(fFastReportFile) <> '' then
  begin
  	if Pos('.FR3',UpperCase(fFastReportFile)) = 0 then
  	begin
			BoletoStreamFR3:=TStringStream.Create(fFastReportFile);
      FfrxReport.FileName := '';
      FfrxReport.LoadFromStream(BoletoStreamFR3);
      BoletoStreamFR3.Free;
   	end
   	else
   	begin
      if FileExists(fFastReportFile) then
         FfrxReport.LoadFromFile(fFastReportFile)
      else
        raise EACBrBoletoFCFR.CreateFmt('Caminho do arquivo de impressão do boleto "%s" inválido.', [fFastReportFile]);
      Result := True;
    end;
  end
  else
    raise EACBrBoletoFCFR.Create('Caminho ou o arquivo de impressão do boleto não assinalado.');
end;

function TACBrBoletoFCFR.PreparaRelatorio: Boolean;
begin
  Result := False;
  SetDataSetsToFrxReport;
  if FModoThread then
  begin
    //*****************
    //* Em modo thread não pode ficar carregando o arquivo a cada execução
    //* pois começa a gerar exception e memory leak
    //* Caso tenha mudança de arquivo pode ser chamando o CarregaFastReportFile que está public
    //*****************
    if Trim(FfrxReport.FileName) = '' then
    begin
      CarregaFastReportFile;

      FfrxReport.PrintOptions.ShowDialog := False;
      FfrxReport.ShowProgress := False;

      FfrxReport.EngineOptions.SilentMode := True;
      FfrxReport.EngineOptions.EnableThreadSafe := True;
      FfrxReport.EngineOptions.DestroyForms := False;
      FfrxReport.PreviewOptions.AllowEdit := False;
    end;
  end
  else
  begin
    FfrxReport.PrintOptions.ShowDialog := MostrarSetup;
    FfrxReport.ShowProgress := MostrarProgresso;

    FfrxReport.EngineOptions.SilentMode := False;
    FfrxReport.EngineOptions.EnableThreadSafe := False;
    FfrxReport.EngineOptions.DestroyForms := True;
    FfrxReport.PreviewOptions.AllowEdit := True;
    CarregaFastReportFile;
  end;

  if PrepareBoletos then
  begin
    Result := FfrxReport.PrepareReport;
  end;
end;

function TACBrBoletoFCFR.PrepareBoletos: Boolean;
var
  iFor: Integer;
  sTipoDoc: String;

  // Titulos
  Field_NossNum: TField;
  Field_CodCendente: TField;
  Field_CodBarras: TField;
  Field_LinhaDigitaval: TField;
  Field_TipoDoc: TField;
  Field_Vencimento: TField;
  Field_DataDocumento: TField;
  Field_NumeroDocumento: TField;
  Field_TotalParcelas: TField;
  Field_Parcela: TField;
  Field_EspecieMod: TField;
  Field_EspecieDoc: TField;
  Field_UsoBanco: TField;
  Field_Aceite: TField;
  Field_DataProcessamento: TField;
  Field_NossoNumero: TField;
  Field_Carteira: TField;
  Field_ValorDocumento: TField;
  Field_LocalPagamento: TField;
  Field_ValorMoraJuros: TField;
  Field_ValorDesconto: TField;
  Field_ValorAbatimento: TField;
  Field_DataMoraJuros: TField;
  Field_DataDesconto: TField;
  Field_DataABatimento: TField;
  Field_DataProtesto: TField;
  Field_PercentualMulta: TField;
  Field_Mensagem: TField;
  Field_OcorrenciaOriginal: TField;
  Field_Instrucao1: TField;
  Field_Instrucao2: TField;
  Field_TextoLivre: TField;
  Field_Asbace: TField;
  Field_EMV: TField;
  Field_ArquivoLogoEmp: TField;

  // Sacado
  Field_Sacado_NomeSacado: TField;
  Field_Sacado_CNPJCPF: TField;
  Field_Sacado_Logradouro: TField;
  Field_Sacado_Complemento: TField;
  Field_Sacado_Numero: TField;
  Field_Sacado_Bairro: TField;
  Field_Sacado_Cidade: TField;
  Field_Sacado_UF: TField;
  Field_Sacado_CEP: TField;
  Field_Sacado_Avalista: TField;
  Field_Sacado_Avalista_CNPJCPF : TField;
  Field_Sacado_Fone: TField;

  procedure InserirTitulo(Indice: Integer);
  begin
    with ACBrBoleto do
    begin
      case ACBrBoleto.Cedente.TipoInscricao of
        pFisica:
          sTipoDoc := 'CPF: ';
        pJuridica:
          sTipoDoc := 'CNPJ: ';
      else
        sTipoDoc := 'DOC.: ';
      end;
      // Monta mensagens de multa e juros
      MensagemPadrao.Clear;
      MensagemPadrao.Text := ListadeBoletos[Indice].Mensagem.Text;
      AdicionarMensagensPadroes(ListadeBoletos[Indice], MensagemPadrao);

      with FcdsTitulo do
      begin
        Append;
        Field_NossNum.AsString := Banco.MontarCampoNossoNumero(ListadeBoletos[Indice]);
        Field_CodCendente.AsString := Banco.MontarCampoCodigoCedente(ListadeBoletos[Indice]);
        Field_CodBarras.AsString := Banco.MontarCodigoBarras(ListadeBoletos[Indice]);
        Field_LinhaDigitaval.AsString := Banco.MontarLinhaDigitavel(Field_CodBarras.AsString, ListadeBoletos[Indice]);
        Field_TipoDoc.AsString := sTipoDoc;
        Field_Vencimento.AsDateTime := ListadeBoletos[Indice].Vencimento;
        Field_DataDocumento.AsDateTime := ListadeBoletos[Indice].DataDocumento;
        Field_NumeroDocumento.AsString := ListadeBoletos[Indice].NumeroDocumento;
        Field_TotalParcelas.AsInteger := ListadeBoletos[Indice].TotalParcelas;
        Field_Parcela.AsInteger := ListadeBoletos[Indice].Parcela;
        Field_EspecieMod.AsString := ListadeBoletos[Indice].EspecieMod;
        Field_EspecieDoc.AsString := ListadeBoletos[Indice].EspecieDoc;
        Field_UsoBanco.AsString := ListadeBoletos[Indice].UsoBanco;
        Field_Aceite.AsString := DefineAceiteImpressao(ListadeBoletos[Indice]);
        Field_DataProcessamento.AsDateTime := ListadeBoletos[Indice].DataProcessamento;
        Field_NossoNumero.AsString := ListadeBoletos[Indice].NossoNumero;
        Field_Carteira.AsString := Banco.MontarCampoCarteira(ListadeBoletos[Indice]);
        Field_ValorDocumento.AsCurrency := ListadeBoletos[Indice].ValorDocumento;
        Field_LocalPagamento.AsString := ListadeBoletos[Indice].LocalPagamento;
        Field_ValorMoraJuros.AsCurrency := ListadeBoletos[Indice].ValorMoraJuros;
        Field_ValorDesconto.AsCurrency := ListadeBoletos[Indice].ValorDesconto;
        Field_ValorAbatimento.AsCurrency := ListadeBoletos[Indice].ValorAbatimento;
        Field_DataMoraJuros.AsDateTime := ListadeBoletos[Indice].DataMoraJuros;
        Field_DataDesconto.AsDateTime := ListadeBoletos[Indice].DataDesconto;
        Field_DataABatimento.AsDateTime := ListadeBoletos[Indice].DataAbatimento;
        Field_DataProtesto.AsDateTime := ListadeBoletos[Indice].DataProtesto;
        Field_PercentualMulta.AsFloat := ListadeBoletos[Indice].PercentualMulta;
        Field_Mensagem.AsString := MensagemPadrao.Text;
        Field_OcorrenciaOriginal.AsInteger := Integer(ListadeBoletos[Indice].OcorrenciaOriginal);
        Field_Instrucao1.AsString := ListadeBoletos[Indice].Instrucao1;
        Field_Instrucao2.AsString := ListadeBoletos[Indice].Instrucao2;
        Field_TextoLivre.AsString := ListadeBoletos[Indice].TextoLivre;
        if ACBrBoleto.Banco.Numero = 21 then
          Field_Asbace.AsString := TACBrBancoBanestes(Banco).CalcularCampoASBACE(ListadeBoletos[Indice]);
        Field_EMV.AsString := ListadeBoletos[Indice].QrCode.emv;
        Field_ArquivoLogoEmp.AsString := ListadeBoletos[Indice].ArquivoLogoEmp;

        // Sacado
        Field_Sacado_NomeSacado.AsString := ListadeBoletos[Indice].Sacado.NomeSacado;
        Field_Sacado_CNPJCPF.AsString := ListadeBoletos[Indice].Sacado.CNPJCPF;
        Field_Sacado_Logradouro.AsString := ListadeBoletos[Indice].Sacado.Logradouro;
        Field_Sacado_Complemento.AsString := ListadeBoletos[Indice].Sacado.Complemento;
        Field_Sacado_Numero.AsString := ListadeBoletos[Indice].Sacado.Numero;
        Field_Sacado_Bairro.AsString := ListadeBoletos[Indice].Sacado.Bairro;
        Field_Sacado_Cidade.AsString := ListadeBoletos[Indice].Sacado.Cidade;
        Field_Sacado_UF.AsString := ListadeBoletos[Indice].Sacado.UF;
        Field_Sacado_CEP.AsString := ListadeBoletos[Indice].Sacado.CEP;
        Field_Sacado_Avalista.AsString := ListadeBoletos[Indice].Sacado.Avalista;
        Field_Sacado_Avalista_CNPJCPF.asString := ListadeBoletos[Indice].Sacado.SacadoAvalista.CNPJCPF;
        Field_Sacado_Fone.AsString := ListadeBoletos[Indice].Sacado.Fone;

        Post;
      end;
    end;
  end;
begin
  with ACBrBoleto do
  begin
    // Banco
    with FcdsBanco do
    begin
      Append;
      FieldByName('Numero').AsString           := FormatFloat('000', Banco.Numero);
      FieldByName('Digito').AsString           := IfThen(Banco.Digito >= 10, 'X', IntToStrZero(Banco.Digito, 1));
      FieldByName('Nome').AsString             := Banco.Nome;
      FieldByName('DirLogo').AsString          := DirLogo;
      FieldByName('OrientacoesBanco').AsString := Banco.OrientacoesBanco.Text;
      FieldByName('CIP').AsString              := Banco.CIP;
      Post;
    end;
    // Cedente
    with FcdsCedente do
    begin
      Append;
      FieldByName('Nome').AsString := Cedente.Nome;
      FieldByName('CodigoCedente').AsString := Banco.MontarCampoCodigoCedente(Titulo); // Cedente.CodigoCedente;
      FieldByName('CodigoTransmissao').AsString := Cedente.CodigoTransmissao;
      FieldByName('Agencia').AsString := Cedente.Agencia;
      FieldByName('AgenciaDigito').AsString := Cedente.AgenciaDigito;
      FieldByName('Conta').AsString := Cedente.Conta;
      FieldByName('ContaDigito').AsString := Cedente.ContaDigito;
      FieldByName('Modalidade').AsString := Cedente.Modalidade;
      FieldByName('Convenio').AsString := Cedente.Convenio;
      FieldByName('ResponEmissao').AsInteger := Integer(Cedente.ResponEmissao);
      FieldByName('CNPJCPF').AsString := Cedente.CNPJCPF;
      FieldByName('TipoInscricao').AsInteger := Integer(Cedente.TipoInscricao);
      FieldByName('Logradouro').AsString := Cedente.Logradouro;
      FieldByName('NumeroRes').AsString := Cedente.NumeroRes;
      FieldByName('Complemento').AsString := Cedente.Complemento;
      FieldByName('Bairro').AsString := Cedente.Bairro;
      FieldByName('Cidade').AsString := Cedente.Cidade;
      FieldByName('UF').AsString := Cedente.UF;
      FieldByName('CEP').AsString := Cedente.CEP;
      FieldByName('Telefone').AsString := Cedente.Telefone;
      Post;
    end;
    // Titulos

    with FcdsTitulo do
    begin
      Field_NossNum := FieldByName('NossoNum');
      Field_CodCendente := FieldByName('CodCedente');
      Field_CodBarras := FieldByName('CodBarras');
      Field_LinhaDigitaval := FieldByName('LinhaDigitavel');
      Field_TipoDoc := FieldByName('TipoDoc');
      Field_Vencimento := FieldByName('Vencimento');
      Field_DataDocumento := FieldByName('DataDocumento');
      Field_NumeroDocumento := FieldByName('NumeroDocumento');
      Field_TotalParcelas := FieldByName('TotalParcelas');
      Field_Parcela := FieldByName('Parcela');
      Field_EspecieMod := FieldByName('EspecieMod');
      Field_EspecieDoc := FieldByName('EspecieDoc');
      Field_UsoBanco := FieldByName('UsoBanco');
      Field_Aceite := FieldByName('Aceite');
      Field_DataProcessamento := FieldByName('DataProcessamento');
      Field_NossoNumero := FieldByName('NossoNumero');
      Field_Carteira := FieldByName('Carteira');
      Field_ValorDocumento := FieldByName('ValorDocumento');
      Field_LocalPagamento := FieldByName('LocalPagamento');
      Field_ValorMoraJuros := FieldByName('ValorMoraJuros');
      Field_ValorDesconto := FieldByName('ValorDesconto');
      Field_ValorAbatimento := FieldByName('ValorAbatimento');
      Field_DataMoraJuros := FieldByName('DataMoraJuros');
      Field_DataDesconto := FieldByName('DataDesconto');
      Field_DataABatimento := FieldByName('DataAbatimento');
      Field_DataProtesto := FieldByName('DataProtesto');
      Field_PercentualMulta := FieldByName('PercentualMulta');
      Field_Mensagem := FieldByName('Mensagem');
      Field_OcorrenciaOriginal := FieldByName('OcorrenciaOriginal');
      Field_Instrucao1 := FieldByName('Instrucao1');
      Field_Instrucao2 := FieldByName('Instrucao2');
      Field_TextoLivre := FieldByName('TextoLivre');
      Field_Asbace := FieldByName('Asbace');
      Field_EMV := FieldByName('EMV');
      Field_ArquivoLogoEmp := FieldByName('ArquivoLogoEmp');

      // Sacado
      Field_Sacado_NomeSacado := FieldByName('Sacado_NomeSacado');
      Field_Sacado_CNPJCPF := FieldByName('Sacado_CNPJCPF');
      Field_Sacado_Logradouro := FieldByName('Sacado_Logradouro');
      Field_Sacado_Complemento := FieldByName('Sacado_Complemento');
      Field_Sacado_Numero := FieldByName('Sacado_Numero');
      Field_Sacado_Bairro := FieldByName('Sacado_Bairro');
      Field_Sacado_Cidade := FieldByName('Sacado_Cidade');
      Field_Sacado_UF := FieldByName('Sacado_UF');
      Field_Sacado_CEP := FieldByName('Sacado_CEP');
      Field_Sacado_Avalista := FieldByName('Sacado_Avalista');
      Field_Sacado_Avalista_CNPJCPF := FieldByName('Sacado_Avalista_CNPJCPF');
      Field_Sacado_Fone := FieldByName('Sacado_Fone');
    end;

    if IndiceImprimirIndividual > -1 then
      InserirTitulo(IndiceImprimirIndividual)
    else
      for iFor := 0 to ListadeBoletos.Count - 1 do
        InserirTitulo(iFor);
  end;

  Result := True;
end;

end.

