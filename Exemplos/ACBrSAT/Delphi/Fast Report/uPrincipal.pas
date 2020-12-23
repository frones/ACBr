unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, frxClass, Vcl.Menus,
  System.Actions, Vcl.ActnList, frxExportPDF, frxDBSet;

type
  TfPrincipal = class(TForm)
    Button1: TButton;
    OpenDialog: TOpenDialog;
    btnSalvarPDF: TButton;
    frxSAT: TfrxReport;
    procedure Button1Click(Sender: TObject);
    procedure btnSalvarPDFClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStream: TMemoryStream;
    FSistema: string;
    function CarregarXML: ansistring;
    function GerarImpressao(const Report: TfrxReport): Boolean;
    function ConverterFastReportParaString(Relatorio: TfrxReport): string;
  end;

var
  fPrincipal: TfPrincipal;

implementation

uses
  ACBrSAT, Xml.XMLDoc, Xml.XMLIntf, ACBrSATExtratoFR;

{$R *.dfm}

function TfPrincipal.CarregarXML: ansistring;
var
   XMLDocument: IXMLDocument;
   ArquivoXML: TStringList;
   i: Integer;
begin
   OpenDialog.Options := [ofAllowMultiSelect];

   if OpenDialog.Execute then
   begin
      for i := 0 to OpenDialog.Files.Count -1  do
      begin
         ArquivoXML := TStringList.Create;

         ArquivoXML.LoadFromFile(OpenDialog.FileName, TEncoding.UTF8);

         Result := ArquivoXML.Text
//         XMLDocument := TXMLDocument.Create(nil);
//         XMLDocument.LoadFromFile(OpenDialog.Files[i], TEncoding.UTF8TEncoding.UTF8);
//
//         Result := XMLDocument.XML.Text;
      end;
   end;
end;

procedure TfPrincipal.FormCreate(Sender: TObject);
begin
   FStream := TMemoryStream.Create;
   FSistema := 'https://www.iaush.com.br';
end;

procedure TfPrincipal.FormDestroy(Sender: TObject);
begin
   FStream.Free;
   inherited;
end;

procedure TfPrincipal.btnSalvarPDFClick(Sender: TObject);
begin
   FStream.SaveToFile('SAT.pdf');
end;

function TfPrincipal.ConverterFastReportParaString(Relatorio: TfrxReport): string;
var
   StringStream: TStringStream;
begin
   StringStream := TStringStream.Create('');
   Relatorio.SaveToStream(StringStream);
   Result := StringStream.DataString;
   StringStream.Free;
end;

procedure TfPrincipal.Button1Click(Sender: TObject);
var
   ACBrDFe: TACBrSAT;
begin
   ACBrDFe := TACBrSAT.Create(Self);
   ACBrDFe.CFe.SetXMLString(AnsiString(CarregarXML));

   ACBrDFe.Extrato := TACBrSATExtratoFast.Create(ACBrDFe);

   with ACBrDFe.Extrato as TACBrSATExtratoFast do
   try
//      Logo      := FLogotipo;
      FastFile := ConverterFastReportParaString(frxSAT);
      Sistema   := FSistema;

//      MargemEsquerda := 0.1;
//      MargemSuperior := 1.5;
//      MargemDireita  := 0.1;
//      MargemInferior := 1.0;

//         ImprimirExtrato


      GerarImpressao(PreparedReport);

      MostraPreview := True;
     // ImprimirDAMDFe(MDFe);

//      FStream.SaveToFile('SAT.pdf');
   finally
      FreeAndNil(ACBrDFe);
   end;
end;

function TfPrincipal.GerarImpressao(const Report: TfrxReport): Boolean;
var
   PDFExport: TfrxPDFExport;
   i :integer; s: string;
begin
   FStream.Clear;
   PDFExport := TfrxPDFExport.Create(Self);

   with PDFExport do
   begin
      ShowDialog    := False;
      ShowProgress  := False;
      Stream        := FStream;
      FileName      := 'Sat.PDF';

      EmbeddedFonts := True;
      Title         := 'SAT Extrato FR';
   end;

   Result := Report.Export(PDFExport);
   Report.ShowReport;
  // Report.SaveToFile('Sat.PDF');
end;

end.
