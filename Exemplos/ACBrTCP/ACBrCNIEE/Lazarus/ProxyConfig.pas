unit ProxyConfig ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ACBrEnterTab ;

type


  { TfrProxyConfig }

  TfrProxyConfig = class(TForm)
    ACBrEnterTab1 : TACBrEnterTab ;
    bCancelar : TBitBtn ;
    bOk : TBitBtn ;
    cbAutenticacao : TCheckBox ;
    edPorta : TEdit ;
    edUser : TEdit ;
    edSenha : TEdit ;
    edServidor : TEdit ;
    Image1 : TImage ;
    lSenha1 : TLabel ;
    lUser : TLabel ;
    lSenha : TLabel ;
    lUser1 : TLabel ;
    Panel1 : TPanel ;
    Panel2 : TPanel ;
    procedure cbAutenticacaoChange(Sender : TObject) ;
    procedure FormShow(Sender : TObject) ;
  private
    { private declarations }
  public
    { public declarations }
  end ; 

implementation

{$R *.lfm}

procedure TfrProxyConfig.FormShow(Sender : TObject) ;
begin
   cbAutenticacao.Checked := (edUser.Text <> '');
end;

procedure TfrProxyConfig.cbAutenticacaoChange(Sender : TObject) ;
begin
  edUser.Enabled  := cbAutenticacao.Checked;
  edSenha.Enabled := cbAutenticacao.Checked;
end;

end.

