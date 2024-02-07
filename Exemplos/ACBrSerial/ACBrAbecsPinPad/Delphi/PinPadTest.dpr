program PinPadTest;



uses
  Forms,
  configuraserial in 'configuraserial.pas',
  formprincipal in 'formprincipal.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Aqua Light Slate');
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
