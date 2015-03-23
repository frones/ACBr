unit ProxyConfig;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ACBrEnterTab, ACBrBase;

type
  TfrProxyConfig = class(TForm)
    bCancelar: TBitBtn;
    bOk: TBitBtn;
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

{$R *.dfm}

procedure TfrProxyConfig.FormShow(Sender: TObject);
begin
  cbAutenticacao.Checked := (edUser.Text <> '');
end;

procedure TfrProxyConfig.cbAutenticacaoChange(Sender: TObject);
begin
  edUser.Enabled := cbAutenticacao.Checked;
  edSenha.Enabled := cbAutenticacao.Checked;
end;

end.
