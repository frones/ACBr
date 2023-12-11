program ProjDANFSeXFPDF;

uses
  Vcl.Forms,
  UForm in 'UForm.pas' {Form1},
  ACBr.DANFSe.FPDF.A4Retrato in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\ACBr.DANFSe.FPDF.A4Retrato.pas',
  ACBrNFSeXDANFSeFPDFClass in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\ACBrNFSeXDANFSeFPDFClass.pas',
  ACBrNFSeXDANFSeConsts in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\ACBrNFSeXDANFSeConsts.pas',
  ACBr.DANFSeX.Classes in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\ACBr.DANFSeX.Classes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
