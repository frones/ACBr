{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

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

