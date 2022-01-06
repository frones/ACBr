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

unit uTestes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ACBrMail;

type

  { TForm2 }

  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure ACBrMailTesteMailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    ACBrMailTeste: TACBrMail;
    MensagemErro: string;
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  ACBrMailTeste := TACBrMail.Create(nil);
  ACBrMailTeste.OnMailProcess := @ACBrMailTesteMailProcess;
  MensagemErro := '';
end;

procedure TForm2.ACBrMailTesteMailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
begin
  case aStatus of
    pmsStartProcess:
      Label2.Caption := 'Teste: Iniciando processo de envio.';
    pmsConfigHeaders:
      Label2.Caption := 'Teste: Configurando o cabeçalho do e-mail.';
    pmsLoginSMTP:
      Label2.Caption := 'Teste: Logando no servidor de e-mail.';
    pmsStartSends:
      Label2.Caption := 'Teste: Iniciando os envios.';
    pmsSendTo:
      Label2.Caption := 'Teste: Processando lista de destinatários.';
    pmsSendData:
      Label2.Caption := 'Teste: Enviando dados.';
    pmsLogoutSMTP:
      Label2.Caption := 'Teste: Fazendo Logout no servidor de e-mail.';
  end;
  Application.ProcessMessages;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  try
    try
      Screen.Cursor := crHourGlass;
      Application.ProcessMessages;
      ACBrMailTeste.Send;
    finally
      ACBrMailTeste.Free;
    end;
    ModalResult := mrOK;
  except
    on e: Exception do
    begin
      MensagemErro := e.Message;
      ModalResult := mrCancel;
    end;
  end;
end;

end.

