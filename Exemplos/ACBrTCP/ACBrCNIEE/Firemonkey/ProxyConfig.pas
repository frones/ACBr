unit ProxyConfig;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR



uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, FMX.Controls.Presentation;

//**   Original VCL Uses section : 


//**   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
//**   StdCtrls, ACBrEnterTab, ACBrBase;


type
  TfrProxyConfig = class(TForm)
    bCancelar: TButton;
    bOk: TButton;
    cbAutenticacao: TCheckBox;
    edPorta: TEdit;
    edUser: TEdit;
    edSenha: TEdit;
    edServidor: TEdit;
    lSenha1: TLabel;
    lUser: TLabel;
    lSenha: TLabel;
    lUser1: TLabel;
    Panel2: TPanel;
    procedure cbAutenticacaoChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.FMX}

procedure TfrProxyConfig.FormShow(Sender: TObject);
begin
  cbAutenticacao.IsChecked  := (edUser.Text <> '');
end;

procedure TfrProxyConfig.cbAutenticacaoChange(Sender: TObject);
begin
  edUser.Enabled := cbAutenticacao.IsChecked;
  edSenha.Enabled := cbAutenticacao.IsChecked;
end;

end.
