program ACBrInstall;

uses
  Forms,
  SVN_Class in 'SVN_Class.pas',
  uPrincipal in 'uPrincipal.pas' {frmPrincipal},
  uFrameLista in 'uFrameLista.pas' {framePacotes: TFrame},
  JclIDEUtils in 'D:\Axial Componentes\Jedi\Jcl\Jcl\source\common\JclIDEUtils.pas',
  JclFileUtils in 'D:\Axial Componentes\Jedi\Jcl\Jcl\source\common\JclFileUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Instalação do Projeto ACBr';
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
