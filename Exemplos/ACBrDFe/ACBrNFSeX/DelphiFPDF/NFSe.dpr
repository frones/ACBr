program NFSe;

uses
  Vcl.Forms,
  Form.NFSe in 'Form.NFSe.pas' {FNFSe},
  ACBrNFSeXDANFSeFPDFClass in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\ACBrNFSeXDANFSeFPDFClass.pas',
  NFSeDANFSeFPDF in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\NFSeDANFSeFPDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFNFSe, FNFSe);
  Application.Run;
end.
