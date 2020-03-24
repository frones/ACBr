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

unit CMC7Teste1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ACBrLCB, ACBrCMC7, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
     ACBrCMC7_1: TACBrCMC7;
    ACBrLCB1: TACBrLCB;
    edtcmc7: TEdit;
    btnAtivar: TButton;
    ACBrCMC71: TACBrCMC7;
    Memo1: TMemo;
    edtcomp: TEdit;
    edtbanco: TEdit;
    edtagencia: TEdit;
    edtconta: TEdit;
    edtnumcheque: TEdit;
    edttipificacao: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Label5: TLabel;
    Label6: TLabel;
    procedure ACBrLCB1LeCodigo(Sender: TObject);
    procedure btnAtivarClick(Sender: TObject);
    procedure edtcmc7KeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    procedure MostraCMC7(Codigo : String);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

procedure TForm1.MostraCMC7(Codigo : String);
begin
  ACBrCMC71.CMC7 := Codigo;
// Memo1.Lines.Clear;
  Memo1.Lines.Add('=======================================');
  Memo1.Lines.Add('CMC7:'+ACBrCMC71.CMC7);
  Memo1.Lines.Add('Comp:'+ACBrCMC71.Comp);
  Memo1.Lines.Add('Banco:'+ACBrCMC71.Banco);
  Memo1.Lines.Add('Agencia:'+ACBrCMC71.Agencia);
  Memo1.Lines.Add('Conta:'+ACBrCMC71.Conta);
  Memo1.Lines.Add('Numero:'+ACBrCMC71.Numero);
end;

procedure TForm1.ACBrLCB1LeCodigo(Sender: TObject);
begin
  edtcmc7.Text := ACBrLCB1.UltimoCodigo;
  MostraCMC7(edtcmc7.Text);
end;

procedure TForm1.btnAtivarClick(Sender: TObject);
begin
 if ACBrLCB1.Ativo then
  begin
    btnAtivar.Caption := 'Ativar Leitor Serial';
    ACBrLCB1.Desativar ;
  end
 else
  begin
    btnAtivar.Caption := 'Desativar Leitor Serial';
    ACBrLCB1.Ativar ;
  end;
end;

procedure TForm1.edtcmc7KeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
     MostraCMC7(edtcmc7.Text) ;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ACBrCMC71.MontaCMC7(edtbanco.Text,edtagencia.Text,edtconta.Text,edtnumcheque.Text,edtcomp.Text,edttipificacao.Text);
  edtcmc7.Text := ACBrCMC71.CMC7;
end;

initialization
  {$I cmc7teste1.lrs}

end.

