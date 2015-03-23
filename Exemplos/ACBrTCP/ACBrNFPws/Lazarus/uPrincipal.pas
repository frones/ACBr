unit uPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, EditBtn, ACBrNFPws;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrNFPws1: TACBrNFPws;
    btConsultar: TButton;
    btEnviar: TButton;
    btEnviar1: TButton;
    cbxTipoContr: TComboBox;
    edObservacoes: TEdit;
    edProxyHost: TEdit;
    edProxyPass: TEdit;
    edSenha: TEdit;
    edCNPJ: TEdit;
    edProxyPort: TEdit;
    edProxyUser: TEdit;
    edUsuario: TEdit;
    edProtocolo: TEdit;
    edFileNameEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    rgModo: TRadioGroup;
    rgDocto: TRadioGroup;
    tsConf: TTabSheet;
    tsEnvio: TTabSheet;
    procedure btConsultarClick(Sender: TObject);
    procedure btEnviar1Click(Sender: TObject);
    procedure btEnviarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AjustaProxy;
    procedure AjustaUsuario;

    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.btConsultarClick(Sender: TObject);
begin
  AjustaProxy;
  AjustaUsuario;

  Memo1.Lines.Add( ACBrNFPws1.Consultar( edProtocolo.Text ) );
end;

procedure TForm1.btEnviar1Click(Sender: TObject);
Var
  Resp, Protocolo : String ;
  SL : TStringList;
begin
  AjustaProxy;
  AjustaUsuario;

  Protocolo := '';
  Resp      := ACBrNFPws1.Enviar( edFileNameEdit.Text, edObservacoes.Text ) ;

  SL := TStringList.Create;
  try
    SL.Text := StringReplace( Resp, '|', sLineBreak, [rfReplaceAll] );
    Memo1.Lines.Add( 'Resposta Envio:' );
    Memo1.Lines.Add( Resp );
    Memo1.Lines.Add( '------------------' );
    Memo1.lines.AddStrings( SL );
    Memo1.Lines.Add( '------------------' );

    if SL.Count > 0 then
      Protocolo := SL[1];

    if Protocolo <> '' then
    begin
      Resp := ACBrNFPws1.Consultar( Protocolo );

      SL.Text := StringReplace( Resp, '|', sLineBreak, [rfReplaceAll] );
      Memo1.Lines.Add( 'Resposta Consulta:' );
      Memo1.Lines.Add( Resp );
      Memo1.Lines.Add( '------------------' );
      Memo1.lines.AddStrings( SL );
      Memo1.Lines.Add( '------------------' );
    end;
  finally
    SL.Free;
  end;

end;

procedure TForm1.btEnviarClick(Sender: TObject);
begin
  AjustaProxy;
  AjustaUsuario;

  Memo1.Lines.Add( ACBrNFPws1.Enviar( edFileNameEdit.Text, edObservacoes.Text ) );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.AjustaProxy ;
begin
  ACBrNFPws1.ProxyHost := edProxyHost.Text ;
  ACBrNFPws1.ProxyPort := edProxyPort.Text ;
  ACBrNFPws1.ProxyUser := edProxyUser.Text ;
  ACBrNFPws1.ProxyPass := edProxyPass.Text ;
end ;

procedure TForm1.AjustaUsuario;
begin
  ACBrNFPws1.Usuario := edUsuario.Text;
  ACBrNFPws1.Senha   := edSenha.Text;
  ACBrNFPws1.CNPJ    := edCNPJ.Text;
  ACBrNFPws1.CategoriaUsuario := TACBrNFPwsCategoriaUsuario( cbxTipoContr.ItemIndex );
  ACBrNFPws1.ModoTeste        := (rgModo.ItemIndex = 0) ;
  ACBrNFPws1.TipoDocto        := TACBrNFPwsTipoDocto( rgDocto.ItemIndex ) ;
end;

end.

