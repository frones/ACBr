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

unit FormConsultaCNPJ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls;

type

  { TfrConsultaCNPJ }

  TfrConsultaCNPJ = class(TForm)
    btNovoCaptcha: TBitBtn;
    btOK: TBitBtn;
    btOK1: TBitBtn;
    edtCaptcha: TEdit;
    imgCaptcha: TImage;
    lInformeCaptcha: TLabel;
    pBotoes: TPanel;
    pCaptcha: TPanel;
    procedure btNovoCaptchaClick(Sender: TObject);
    procedure edtCaptchaChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    procedure AtualizarCaptcha;

  public

  end;

implementation

uses
  FormTelaPrincipal;

{$R *.lfm}

{ TfrConsultaCNPJ }

procedure TfrConsultaCNPJ.FormShow(Sender: TObject);
begin
  AtualizarCaptcha;
end;

procedure TfrConsultaCNPJ.btNovoCaptchaClick(Sender: TObject);
begin
  AtualizarCaptcha;
end;

procedure TfrConsultaCNPJ.edtCaptchaChange(Sender: TObject);
begin
  lInformeCaptcha.Visible := False;
end;

procedure TfrConsultaCNPJ.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if (ModalResult = mrOK) then
  begin
    if (Trim(edtCaptcha.Text) = '') then
    begin
      lInformeCaptcha.Visible := True;
      CanClose := False;
    end;
  end;
end;

procedure TfrConsultaCNPJ.AtualizarCaptcha;
var
  Stream: TMemoryStream;
  Png: TPortableNetworkGraphic;
begin
  Stream:= TMemoryStream.Create;
  Png:= TPortableNetworkGraphic.Create;
  try
    frPOSTEFServer.ACBrConsultaCNPJ1.Captcha(Stream);
    Png.LoadFromStream(Stream);
    imgCaptcha.Picture.Assign(Png);

    edtCaptcha.Clear;
    edtCaptcha.SetFocus;
  finally
    Stream.Free;
    Png.Free;
  end;
end;

end.

