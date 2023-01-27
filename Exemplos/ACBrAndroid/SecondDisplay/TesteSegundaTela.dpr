program TesteSegundaTela;
uses
  System.StartUpCopy,
  FMX.Forms,
  uPrincipal in 'uPrincipal.pas' {Form2},
  uFrameVendaProduto in 'uFrameVendaProduto.pas' {FrameVendaProduto: TFrame},
  uSecondDisplay in 'uSecondDisplay.pas' {FormSecondDisplay};

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TFormSecondDisplay, FormSecondDisplay);
  Application.Run;
end.
