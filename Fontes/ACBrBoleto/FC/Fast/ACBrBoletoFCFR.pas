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

  TdmACBrBoletoFCFR = class;

  { TACBrBoletoFCFR }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBoletoFCFR = class(TACBrBoletoFCClass)
  private
    MensagemPadrao: TStringList;
    { Private declarations }
    fFastReportFile: String;
    FImpressora: String;
    fIndice: Integer;
    FdmBoleto: TdmACBrBoletoFCFR;
    FModoThread: Boolean;
    FIncorporarFontesPdf: Boolean;
    FIncorporarBackgroundPdf: Boolean;
    FCustomPreview: TfrxCustomPreview;
    function PrepareBoletos: Boolean;
    function PreparaRelatorio: Boolean;
    function GetACBrTitulo: TACBrTitulo;
    procedure SetCustomPreview(const Value: TfrxCustomPreview);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Imprimir; override;
    function CarregaFastReportFile: Boolean;
    function PreparedReport: TfrxReport;
    property Titulo: TACBrTitulo read GetACBrTitulo;
  published
    property FastReportFile: String read fFastReportFile write fFastReportFile;
    property Impressora: String read FImpressora write FImpressora;
    property ModoThread: Boolean read FModoThread write FModoThread;
    property dmBoleto: TdmACBrBoletoFCFR read FdmBoleto write FdmBoleto;
    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
    property IncorporarFontesPdf: Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;
    property CustomPreview: TfrxCustomPreview read FCustomPreview write SetCustomPreview;
  end;

  { TdmACbrBoletoFCFR }
  TdmACBrBoletoFCFR = class(TDataModule)
    frxPDFExport: TfrxPDFExport;
    cdsTitulo: TClientDataSet;
    frxTitulo: TfrxDBDataset;
    frxBarCodeObject: TfrxBarCodeObject;
    frxReport: TfrxReport;
    frxHTMLExport: TfrxHTMLExport;
    cdsCedente: TClientDataSet;
    frxCedente: TfrxDBDataset;
    cdsBanco: TClientDataSet;
    frxBanco: TfrxDBDataset;
    frxJPEGExport: TfrxJPEGExport;
    procedure DataModuleCreate(Sender: TObject);
    procedure frxReportProgressStart(Sender: TfrxReport;
      ProgressType: TfrxProgressType; Progress: Integer);
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
  private
    { Private declarations }
    FACBrBoletoReport: TACBrBoletoFCFR;
    procedure SetDataSetsToFrxReport;
    procedure ImprimeLogoMarca(const sfrxPicture: string);
  public
    { Public declarations }
    constructor Create( AOwner : TComponent); override;
  end;

implementation

{$R *.dfm}

uses ACBrUtil, ACBrBancoBanestes, ACBrDFeReport, ACBrDelphiZXingQRCode;

{ TdmACBrBoletoFCFR }

procedure TdmACBrBoletoFCFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  emvQrCode: String;
begin
  emvQrCode := Trim(FACBrBoletoReport.Titulo.QrCode.emv);
  if Assigned(Sender) and (Trim(emvQrCode) = '') and (Sender.Name = 'ImgEmvQrcode') then
    TfrxPictureView(Sender).Visible := False
  else
  if Assigned(Sender) and (Sender.Name = 'ImgEmvQrcode') then
     PintarQRCode(emvQrCode, TfrxPictureView(Sender).Picture.Bitmap, qrAuto);
end;

procedure TdmACBrBoletoFCFR.frxReportProgressStart(Sender: TfrxReport;
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

procedure TdmACBrBoletoFCFR.ImprimeLogoMarca(const sfrxPicture: string);
var
  frxPict: TfrxPictureView; // Componente para inserção de imagem na impressão.
begin
  frxPict := TfrxPictureView(Self.frxReport.FindObject(sfrxPicture));
  if Assigned(frxPict) then
  Begin
    FACBrBoletoReport.CarregaLogo(frxPict.Picture, FACBrBoletoReport.ACBrBoleto.Banco.Numero);
  End;
end;

procedure TdmACBrBoletoFCFR.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(frxBanco);
  frxReport.EnabledDataSets.Add(frxTitulo);
  frxReport.EnabledDataSets.Add(frxCedente);
end;

constructor TdmACBrBoletoFCFR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FACBrBoletoReport := TACBrBoletoFCFR(AOwner);
end;

procedure TdmACBrBoletoFCFR.DataModuleCreate(Sender: TObject);
begin
  frxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick];
  frxReport.EngineOptions.UseGlobalDataSetList := False;

  // Banco
  with cdsBanco do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('Numero', ftString, 20);
    FieldDefs.Add('Digito', ftString, 1);
    FieldDefs.Add('Nome', ftString, 100);
    FieldDefs.Add('DirLogo', ftString, 254);
    FieldDefs.Add('OrientacoesBanco', ftString, 254);
    FieldDefs.Add('CIP', ftString, 3);
    CreateDataSet;
  end;
  // Cedente
  with cdsCedente do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('Nome', ftString, 100);
    FieldDefs.Add('CodigoCedente', ftString, 20);
    FieldDefs.Add('CodigoTransmissao', ftString, 20);
    FieldDefs.Add('Agencia', ftString, 5);
    FieldDefs.Add('AgenciaDigito', ftString, 2);
    FieldDefs.Add('Conta', ftString, 20);
    FieldDefs.Add('ContaDigito', ftString, 2);
    FieldDefs.Add('Modalidade', ftString, 20);
    FieldDefs.Add('Convenio', ftString, 20);
    FieldDefs.Add('ResponEmissao', ftInteger);
    FieldDefs.Add('CNPJCPF', ftString, 18);
    FieldDefs.Add('TipoInscricao', ftInteger);
    FieldDefs.Add('Logradouro', ftString, 100);
    FieldDefs.Add('NumeroRes', ftString, 10);
    FieldDefs.Add('Complemento', ftString, 100);
    FieldDefs.Add('Bairro', ftString, 100);
    FieldDefs.Add('Cidade', ftString, 100);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('Telefone', ftString, 15);
    CreateDataSet;
  end;
  // Titulo
  with cdsTitulo do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('NossoNum', ftString, 100);
    FieldDefs.Add('CodCedente', ftString, 100);
    FieldDefs.Add('CodBarras', ftString, 100);
    FieldDefs.Add('LinhaDigitavel', ftString, 100);
    FieldDefs.Add('TipoDoc', ftString, 10);
    FieldDefs.Add('Vencimento', ftDateTime);
    FieldDefs.Add('DataDocumento', ftDateTime);
    FieldDefs.Add('NumeroDocumento', ftString, 20);
    FieldDefs.Add('TotalParcelas', ftInteger);
    FieldDefs.Add('Parcela', ftInteger);
    FieldDefs.Add('EspecieDoc', ftString, 10);
    FieldDefs.Add('EspecieMod', ftString, 10);
    FieldDefs.Add('UsoBanco', ftString, 4);
    FieldDefs.Add('Aceite', ftInteger);
    FieldDefs.Add('DataProcessamento', ftDateTime);
    FieldDefs.Add('NossoNumero', ftString, 20);
    FieldDefs.Add('Carteira', ftString, 20);
    FieldDefs.Add('ValorDocumento', ftBCD, 18);
    FieldDefs.Add('LocalPagamento', ftString, 100);
    FieldDefs.Add('ValorMoraJuros', ftBCD, 18);
    FieldDefs.Add('ValorDesconto', ftBCD, 18);
    FieldDefs.Add('ValorAbatimento', ftBCD, 18);
    FieldDefs.Add('DataMoraJuros', ftDateTime);
    FieldDefs.Add('DataDesconto', ftDateTime);
    FieldDefs.Add('DataAbatimento', ftDateTime);
    FieldDefs.Add('DataProtesto', ftDateTime);
    FieldDefs.Add('PercentualMulta', ftFloat);
    FieldDefs.Add('Mensagem', ftString, 600);
    FieldDefs.Add('OcorrenciaOriginal', ftInteger);
    FieldDefs.Add('Instrucao1', ftString, 300);
    FieldDefs.Add('Instrucao2', ftString, 300);
    FieldDefs.Add('TextoLivre', ftMemo, 2000);
    FieldDefs.Add('Asbace', ftString, 40);
    FieldDefs.Add('EMV', ftString, 500);
    // Sacado
    FieldDefs.Add('Sacado_NomeSacado', ftString, 100);
    FieldDefs.Add('Sacado_CNPJCPF', ftString, 18);
    FieldDefs.Add('Sacado_Logradouro', ftString, 100);
    FieldDefs.Add('Sacado_Complemento', ftString, 100);
    FieldDefs.Add('Sacado_Numero', ftString, 10);
    FieldDefs.Add('Sacado_Bairro', ftString, 100);
    FieldDefs.Add('Sacado_Cidade', ftString, 100);
    FieldDefs.Add('Sacado_UF', ftString, 2);
    FieldDefs.Add('Sacado_CEP', ftString, 9);
    FieldDefs.Add('Sacado_Avalista', ftString, 100);
    FieldDefs.Add('Sacado_Avalista_CNPJCPF', ftString, 18);
    FieldDefs.Add('Sacado_Fone', ftString, 100);
    CreateDataSet;
  end;
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
  FdmBoleto := TdmACBrBoletoFCFR.Create(Self);
  MensagemPadrao  := TStringList.Create;
  FCustomPreview := Nil;
end;

destructor TACBrBoletoFCFR.Destroy;
begin
  MensagemPadrao.Free;
  FdmBoleto.Free;
  inherited;
end;

function TACBrBoletoFCFR.PreparedReport: TfrxReport;
begin
  ACBrBoleto.ChecarDadosObrigatorios;
  inherited Imprimir; // Verifica se a lista de boletos está vazia
    with FdmBoleto do
    begin
      cdsBanco.EmptyDataSet;
      cdsCedente.EmptyDataSet;
      cdsTitulo.EmptyDataSet;

      if PreparaRelatorio then
        Result := frxReport
      else
        Result := nil;
    end;
end;

procedure TACBrBoletoFCFR.SetCustomPreview(const Value: TfrxCustomPreview);
begin
  FCustomPreview := Value;
end;

procedure TACBrBoletoFCFR.Imprimir;
begin
  inherited Imprimir; // Verifica se a lista de boletos está vazia
  with FdmBoleto do
	begin
      cdsBanco.EmptyDataSet;
      cdsCedente.EmptyDataSet;
      cdsTitulo.EmptyDataSet;

      if PreparaRelatorio then
      begin
        frxReport.Preview := CustomPreview;
        frxReport.PrintOptions.ShowDialog := (MostrarSetup) and (not FModoThread);
        frxReport.PrintOptions.Copies := NumCopias;
        if TituloPreview <> '' then
          frxReport.ReportOptions.Name := TituloPreview;

        if Length(Impressora) > 0 then
          frxReport.PrintOptions.Printer := Impressora;

        case Filtro of
          fiNenhum:
            begin
              if (MostrarPreview) and (not FModoThread) then
              begin
                frxPDFExport.Keywords      := frxPDFExport.Title;
                frxPDFExport.Background    := IncorporarBackgroundPdf; // False diminui 70% do tamanho do pdf
                frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;
                frxPDFExport.Author        := SoftwareHouse;
                frxPDFExport.Creator       := SoftwareHouse;
                frxPDFExport.Producer      := SoftwareHouse;
                frxPDFExport.Title         := 'Boleto';
                frxPDFExport.Subject       := frxPDFExport.Title;
                frxPDFExport.Keywords      := frxPDFExport.Title;
                frxPDFExport.Background    := IncorporarBackgroundPdf; // False diminui 70% do tamanho do pdf
                frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;

                frxReport.Engine.Report.FileName := NomeArquivo; // Nome do arquivo a ser exportado
                frxReport.ShowReport(false)
              end else
                frxReport.Print;
            end;
          fiPDF:
            begin
              if FModoThread then
              begin
                frxPDFExport.ShowDialog := False;
                frxPDFExport.ShowProgress := False;
              end
              else
              begin
                frxPDFExport.ShowDialog := MostrarSetup;
                frxPDFExport.ShowProgress := MostrarProgresso;
              end;
              frxPDFExport.FileName := NomeArquivo;
              frxPDFExport.Author := SoftwareHouse;
              frxPDFExport.Creator := SoftwareHouse;
              frxPDFExport.Producer := SoftwareHouse;
              frxPDFExport.Title := 'Boleto';
              frxPDFExport.Subject := frxPDFExport.Title;
              frxPDFExport.Keywords := frxPDFExport.Title;
              frxPDFExport.Background := IncorporarBackgroundPdf;//False diminui 70% do tamanho do pdf
              frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;
              frxPDFExport.UserPassword := PdfSenha;
              if NaoEstaVazio(frxPDFExport.UserPassword) then
                frxPDFExport.ProtectionFlags := [ePrint];
              frxReport.Export(FdmBoleto.frxPDFExport);
              if frxPDFExport.FileName <> NomeArquivo then
                NomeArquivo := frxPDFExport.FileName;
            end;
          fiHTML:
            begin
              frxHTMLExport.FileName := NomeArquivo;
              frxHTMLExport.ShowDialog := MostrarSetup;
              frxHTMLExport.ShowProgress := MostrarSetup;
              frxReport.Export(FdmBoleto.frxHTMLExport);
              if frxHTMLExport.FileName <> NomeArquivo then
                NomeArquivo := frxHTMLExport.FileName;
            end;
          fiJPG:
            begin
              frxJPEGExport.FileName      := NomeArquivo;
              frxJPEGExport.ShowDialog    := False;
              frxJPEGExport.ShowProgress  := True;
              frxJPEGExport.Monochrome    := True;
              frxJPEGExport.SeparateFiles := True;
              frxJPEGExport.JPEGQuality   := 200;
              frxJPEGExport.Resolution    := 160;
              frxReport.Export(FdmBoleto.frxJPEGExport);
              if frxJPEGExport.FileName <> NomeArquivo then
                NomeArquivo := frxJPEGExport.FileName;
            end;
        else
          exit;
        end;
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
      FdmBoleto.frxReport.FileName := '';
      FdmBoleto.frxReport.LoadFromStream(BoletoStreamFR3);
      BoletoStreamFR3.Free;
   	end
   	else
   	begin
      if FileExists(fFastReportFile) then
         FdmBoleto.frxReport.LoadFromFile(fFastReportFile)
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
  with FdmBoleto do
  begin
    SetDataSetsToFrxReport;
    if FModoThread then
    begin
      //*****************
      //* Em modo thread não pode ficar carregando o arquivo a cada execução
      //* pois começa a gerar exception e memory leak
      //* Caso tenha mudança de arquivo pode ser chamando o CarregaFastReportFile que está public
      //*****************
      if Trim(frxReport.FileName) = '' then
      begin
        CarregaFastReportFile;

        frxReport.PrintOptions.ShowDialog := False;
        frxReport.ShowProgress := False;

        frxReport.EngineOptions.SilentMode := True;
        frxReport.EngineOptions.EnableThreadSafe := True;
        frxReport.EngineOptions.DestroyForms := False;
        frxReport.PreviewOptions.AllowEdit := False;
      end;
    end
    else
    begin
      frxReport.PrintOptions.ShowDialog := MostrarSetup;
      frxReport.ShowProgress := MostrarProgresso;

      frxReport.EngineOptions.SilentMode := False;
      frxReport.EngineOptions.EnableThreadSafe := False;
      frxReport.EngineOptions.DestroyForms := True;
      frxReport.PreviewOptions.AllowEdit := True;
      CarregaFastReportFile;
    end;

    if PrepareBoletos then
    begin
      Result := FdmBoleto.frxReport.PrepareReport;
    end;
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

      with FdmBoleto.cdsTitulo do
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
        Field_Aceite.AsInteger := Integer(ListadeBoletos[Indice].Aceite);
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
    with FdmBoleto.cdsBanco do
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
    with FdmBoleto.cdsCedente do
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

    with FdmBoleto.cdsTitulo do
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
