program NFSe;

uses
  Vcl.Forms,
  Form.NFSe in 'Form.NFSe.pas' {FNFSe},
  ACBrNFSeXDANFSeFPDFClass in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\ACBrNFSeXDANFSeFPDFClass.pas',
  ACBr.DANFSeX.FPDFA4Retrato in '..\..\..\..\Fontes\ACBrDFe\ACBrNFSeX\DANFSE\FPDF\ACBr.DANFSeX.FPDFA4Retrato.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFNFSe, FNFSe);
  Application.Run;
end.
