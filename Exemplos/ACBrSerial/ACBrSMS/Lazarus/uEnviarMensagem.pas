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

unit uEnviarMensagem;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ACBrSMSClass;

type
  TfrmEnviarMensagem = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblContador: TLabel;
    edtTelefone: TEdit;
    btnEnviar: TButton;
    memMensagem: TMemo;
    btnCancelar: TButton;
    ckbQuebrarMensagem: TCheckBox;
    rdgBandeja: TRadioGroup;
    procedure btnCancelarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure memMensagemChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEnviarMensagem: TfrmEnviarMensagem;

implementation

uses
  uPrincipal{, ACBrSMSClass};

{$R *.lfm}

procedure TfrmEnviarMensagem.FormCreate(Sender: TObject);
begin
  edtTelefone.Clear;
  memMensagem.Clear;

  rdgBandeja.Enabled := frmPrincipal.ACBrSMS1.BandejasSimCard > 1;
  if rdgBandeja.Enabled then
    rdgBandeja.ItemIndex := Integer(frmPrincipal.ACBrSMS1.SimCard);
end;

procedure TfrmEnviarMensagem.memMensagemChange(Sender: TObject);
begin
  lblContador.Caption := Format('%d caracter(es)', [Length(memMensagem.Text)]);
end;

procedure TfrmEnviarMensagem.btnCancelarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmEnviarMensagem.btnEnviarClick(Sender: TObject);
var
  IndiceMsgEnviada: String;
begin
  if Trim(edtTelefone.Text) = EmptyStr then
    raise Exception.Create('Informe o número do telefone.');

  if Trim(memMensagem.Text) = EmptyStr then
    raise Exception.Create('Informe a mensagem a ser enviada.');

  // definir a bandeja de chip quando possuir mais de uma
  if frmPrincipal.ACBrSMS1.BandejasSimCard > 1 then
  begin
    if rdgBandeja.ItemIndex = 0 then
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard1)
    else
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard2);
  end;

  // quebra de mensagens maiores que 160 caracteres
  frmPrincipal.ACBrSMS1.QuebraMensagens := ckbQuebrarMensagem.Checked;

  // enviar o sms
  frmPrincipal.ACBrSMS1.EnviarSMS(
    edtTelefone.Text,
    memMensagem.Text,
    IndiceMsgEnviada
  );

  ShowMessage(
    'Mensagem envida com sucesso. Indice: ' + IndiceMsgEnviada +
    sLineBreak +
    sLineBreak +
    'Ultima resposta: ' +
    sLineBreak +
    frmPrincipal.ACBrSMS1.UltimaResposta
  );
end;

end.
