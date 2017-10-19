program ACBrNFe_demo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ufrmStatus in 'ufrmStatus.pas' {frmStatus},
  Unit4 in 'Unit4.pas' {Form4},
  Unit5 in 'Unit5.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
